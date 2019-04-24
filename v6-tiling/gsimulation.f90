!==============================================================
!
! SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
! http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
!
! Copyright 2018 Intel Corporation
!
! THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT
! NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
! PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
!
! =============================================================

module gsimulation_mod

    use iso_fortran_env
    use mt19937_64
    use cputime_mod
    use types_mod
    use particle_mod

    implicit none

    private
    public gsimulation

    type gsimulation

        type(particle), allocatable :: particles       !array of particles

        !Simulation parameters
        integer(int32) :: nparts      = 16000          !number of particles
        integer(int32) :: nsteps      = 10             !number of integration steps
        real(real_t)   :: tstep       = 0.1            !time step of the simulation

        integer(int32) :: nthreads    = 0              !number of threads

        !Simulation result variables
        real(real_t)   :: kenergy     = 0.0            !kinetic energy

        real(real64)   :: tot_time    = 0.0            !total time of the simulation
        real(real64)   :: tot_flops   = 0.0            !total gflops

        real(real64)   :: gflops_avg  = 0.0            !gflops average
        real(real64)   :: gflops_dev  = 0.0            !glofps deviation

        contains

            !A function for simulation
            procedure, public  :: start

            !Functions for memory operations
            procedure, private :: allocate_particles
            procedure, private :: deallocate_particles

            !Functions for initializing particles fields with random values
            generic,   private :: init_particles => init_particles_mtlib    !MT impl. is default
            procedure, private :: init_particles_stock                      !standard implementation
            procedure, private :: init_particles_mtlib                      !Mersenne Twister impl.

            !Functions for printing report header and footer
            procedure, private :: print_header
            procedure, private :: print_footer

    end type

contains

    !-----------------------------------!
    !- Functions for memory operations -!
    !-----------------------------------!

    subroutine allocate_particles(self)

        implicit none

        class(gsimulation), intent(inout) :: self

        if (.not. allocated(self%particles)) allocate(self%particles)
        call self%particles%alloc(self%nparts)

    end subroutine

    subroutine deallocate_particles(self)

        implicit none

        class(gsimulation), intent(inout) :: self

        call self%particles%dealloc()
        if (allocated(self%particles)) deallocate(self%particles)

    end subroutine

    !------------------------------------------------------------------!
    !- Functions for initializing particles fields with random values -!
    !------------------------------------------------------------------!

    subroutine init_particles_stock(self, seed)

        implicit none

        class(gsimulation), intent(inout) :: self
        integer(int32),     intent(inout) :: seed

        integer :: i

        if (allocated(self%particles)) then

            !Initializes particles position
            call random_seed(seed)    !intent(inout) is required by the function
            do i = 1, self%nparts
                call random_number(self%particles%pos_x(i))
                call random_number(self%particles%pos_y(i))
                call random_number(self%particles%pos_z(i))
            end do

            !Initializes particles velocity
            call random_seed(seed)    !intent(inout) is required by the function
            do i = 1, self%nparts
                call random_number(self%particles%vel_x(i))
                call random_number(self%particles%vel_y(i))
                call random_number(self%particles%vel_z(i))
                self%particles%vel_x(i) = (self%particles%vel_x(i)*2 - 1) * 1.0e-3
                self%particles%vel_y(i) = (self%particles%vel_y(i)*2 - 1) * 1.0e-3
                self%particles%vel_z(i) = (self%particles%vel_z(i)*2 - 1) * 1.0e-3
            end do

            !Initializes particles acceleration
            do i = 1, self%nparts
                 self%particles%acc_x(i) = 0.
                 self%particles%acc_y(i) = 0.
                 self%particles%acc_z(i) = 0.
            end do

            !Initializes particles mass
            call random_seed(seed)    !intent(inout) is required by the function
            do i = 1, self%nparts
                call random_number(self%particles%mass(i))
                self%particles%mass(i) = self%particles%mass(i) * self%nparts
            end do

        end if

    end subroutine

    subroutine init_particles_mtlib(self, seed)

        implicit none

        class(gsimulation), intent(inout) :: self
        integer(int64),     intent(in)    :: seed

        integer :: i

        if (allocated(self%particles)) then

            !Initializes particles position
            call init_genrand64(seed)
            do i = 1, self%nparts
                self%particles%pos_x(i) = real(genrand64_real2(), real_t)
                self%particles%pos_y(i) = real(genrand64_real2(), real_t)
                self%particles%pos_z(i) = real(genrand64_real2(), real_t)
            end do

            !Initializes particles velocity
            call init_genrand64(seed)
            do i = 1, self%nparts
                self%particles%vel_x(i) = (real(genrand64_real2(), real_t) * 2 - 1) * 1.0e-3
                self%particles%vel_y(i) = (real(genrand64_real2(), real_t) * 2 - 1) * 1.0e-3
                self%particles%vel_z(i) = (real(genrand64_real2(), real_t) * 2 - 1) * 1.0e-3
            end do

            !Initializes particles acceleration
            do i = 1, self%nparts
                self%particles%acc_x(i) = 0.
                self%particles%acc_y(i) = 0.
                self%particles%acc_z(i) = 0.
            end do

            !Initializes particles mass
            call init_genrand64(seed)
            do i = 1, self%nparts
                self%particles%mass(i) = real(genrand64_real2(), real_t) * self%nparts * 0.9911
            end do

        end if

    end subroutine

    !---------------------------------------------------!
    !- Functions for printing report header and footer -!
    !---------------------------------------------------!

    subroutine print_header(self)

        implicit none

        class(gsimulation), intent(in) :: self

        write(*,*) '==================================================='
        write(*,*) '# Gravity Simulation'
        write(*,'(A12,I9)')   ' # nPart  : ', self%nparts
        write(*,'(A12,I9)')   ' # nSteps : ', self%nsteps
        write(*,'(A12,F9.2)') ' # dt     : ', self%tstep
        write(*,*)
        write(*,*) '---------------------------------------------------'
        write(*,'(A6,A7,A14,A12,A12)') 'step', 'dt', 'kenergy', 'time(s)', 'gflops'
        write(*,*) '---------------------------------------------------'

    end subroutine

    subroutine print_footer(self)

        implicit none

        class(gsimulation), intent(in) :: self

        write(*,*) '---------------------------------------------------'
        write(*,*)
        write(*,'(A25,I8)')           ' # Number of threads   : ', self%nthreads
        write(*,'(A25,F8.5)')         ' # Total time          : ', self%tot_time
        write(*,'(A25,F8.5,A3,F8.5)') ' # Average performance : ', self%gflops_avg, ' +-', self%gflops_dev
        write(*,*) '==================================================='

    end subroutine

    !---------------------------!
    !- Function for simulation -!
    !---------------------------!

    subroutine start(self)

        implicit none

        class(gsimulation), intent(inout) :: self

        !-------------------------!
        !- Variables declaration -!
        !-------------------------!

        !Simulation independent constants
        real(real_t), parameter :: G = 6.67259e-11         !gravitational constant
        real(real_t), parameter :: softeningSquared = 1e-3 !prevents explosion if the particles
                                                           !are close to each other
        !Simulation parameters
        integer(int32) :: nparts                           !number of particles
        integer(int32) :: nsteps                           !number of integration steps
        real(real_t)   :: dt                               !time step of the simulation

        integer(int64) :: seed = 42                        !a seed for particles initialization

        !Simulation result variables
        real(real_t)   :: step_kenergy                     !kinetic energy in each step
        real(real64)   :: step_time_start                  !step starting time
        real(real64)   :: step_time_duration               !step duration
        real(real64)   :: total_time_start                 !simulation starting time
        real(real64)   :: total_time_duration              !simulation duration

        real(real64)   :: nflop                            !number of gflop in each step
        real(real64)   :: gflops_avg                       !gflops average
        real(real64)   :: gflops_dev                       !gflops deviation

        !Temporary variables
        integer(int32) :: step, i, j, itile                !vars used for iteration
        real(real_t)   :: dx, dy, dz                       !xyz distance
        real(real_t)   :: distanceSquared                  !squared distance
        real(real_t)   :: distanceInv                      !1/distance

        integer(int32), parameter :: tile_size = 8         !tile size
        real(real_t)              :: acc_x(tile_size)      !tile for x-axis acceleration
        real(real_t)              :: acc_y(tile_size)      !tile for y-axis acceleration
        real(real_t)              :: acc_z(tile_size)      !tile for z-axis acceleration

        !----------------------------!
        !- Variables initialization -!
        !----------------------------!

        nparts = self%nparts
        nsteps = self%nsteps
        dt     = self%tstep

        !1e-9          : flop -> gflop
        !(11. + 18.)   : number of flop for each iteration  (the first loop)
        !npart * npart : number of iterations               (the first loop)
        !(19.)         : number of flop for each iteration (the second loop)
        !npart         : number of iterations              (the second loop)
        nflop       = 1e-9 * ((11. + 18.) * nparts * nparts + nparts * 19.);
        gflops_avg  = 0.0
        gflops_dev  = 0.0

        !-------------------------------------------!
        !- Particles allocation and initialization -!
        !-------------------------------------------!

        call self%allocate_particles()
        call self%init_particles(seed)

        !--------------------!
        !- Simulation start -!
        !--------------------!

        call self%print_header()

        total_time_start = get_time_in_seconds()

        !In each step
        do step = 1, nsteps

            step_time_start = get_time_in_seconds()

            !Iterates over all particles
            !dir$ vector aligned
            do itile = 1, nparts, tile_size

                !Resets acceleration
                !dir$ vector aligned
                do i = itile, itile+tile_size-1
                    acc_x(i-itile+1) = 0.
                    acc_y(i-itile+1) = 0.
                    acc_z(i-itile+1) = 0.
                end do

                !For given particle
                !computes the distance to other particles
                !and updates acceleration
                !using Newton's law of gravitation
                !dir$ vector aligned
                do j = 1, nparts

                    !dir$ vector aligned
                    do i = itile, itile+tile_size-1

                        !Computes the distance
                        distanceSquared = (self%particles%pos_x(j) - self%particles%pos_x(i))**2 + &
                                          (self%particles%pos_y(j) - self%particles%pos_y(i))**2 + &
                                          (self%particles%pos_z(j) - self%particles%pos_z(i))**2 + &
                                          softeningSquared                                             !9flops
                        distanceInv     = 1.0 / sqrt(distanceSquared)                                  !1div+1sqrt

                        !Updates acceleration
                        acc_x(i-itile+1) = acc_x(i-itile+1) +                                    &
                                           G * self%particles%mass(j) *                          &
                                           (self%particles%pos_x(j) - self%particles%pos_x(i)) * &
                                           distanceInv * distanceInv * distanceInv                     !6flops
                        acc_y(i-itile+1) = acc_y(i-itile+1) +                                    &
                                           G * self%particles%mass(j) *                          &
                                           (self%particles%pos_y(j) - self%particles%pos_y(i)) * &
                                           distanceInv * distanceInv * distanceInv                     !6flops
                        acc_z(i-itile+1) = acc_z(i-itile+1) +                                    &
                                           G * self%particles%mass(j) *                          &
                                           (self%particles%pos_z(j) - self%particles%pos_z(i)) * &
                                           distanceInv * distanceInv * distanceInv                     !6flops
                    end do
                end do

                !dir$ vector aligned
                do i = itile, itile+tile_size-1
                    self%particles%acc_x(i) = acc_x(i-itile+1)
                    self%particles%acc_y(i) = acc_y(i-itile+1)
                    self%particles%acc_z(i) = acc_z(i-itile+1)
                end do

            end do

            !Resets kinetic energy for given iteration step
            step_kenergy = 0.

            !Iterates over all particles
            !dir$ vector aligned
            do i = 1, nparts

                !Updates velocity for given particle
                self%particles%vel_x(i) = self%particles%vel_x(i) + self%particles%acc_x(i) * dt       !2flops
                self%particles%vel_y(i) = self%particles%vel_y(i) + self%particles%acc_y(i) * dt       !2flops
                self%particles%vel_z(i) = self%particles%vel_z(i) + self%particles%acc_z(i) * dt       !2flops

                !Updates position for given particle
                self%particles%pos_x(i) = self%particles%pos_x(i) + self%particles%vel_x(i) * dt       !2flops
                self%particles%pos_y(i) = self%particles%pos_y(i) + self%particles%vel_y(i) * dt       !2flops
                self%particles%pos_z(i) = self%particles%pos_z(i) + self%particles%vel_z(i) * dt       !2flops

                !Adds particle kinetic energy to step kinetic energy
                step_kenergy = step_kenergy + &                                                        !7flops
                               self%particles%mass(i) * (self%particles%vel_x(i) * self%particles%vel_x(i) + &
                                                         self%particles%vel_y(i) * self%particles%vel_y(i) + &
                                                         self%particles%vel_z(i) * self%particles%vel_z(i))
            end do

            !Kinetic energy at the current step
            self%kenergy = 0.5 * step_kenergy

            !Step duration in seconds for the current step
            step_time_duration = get_time_in_seconds() - step_time_start

            !Computes the average and deviation
            gflops_avg = gflops_avg + nflop / step_time_duration
            gflops_dev = gflops_dev + nflop * nflop / (step_time_duration * step_time_duration)

            !Prints the results line
            write(*,'(I5,F8.2,F14.4,2F12.4)') step, step*dt, self%kenergy, &
                                              step_time_duration, nflop / step_time_duration

        end do

        !Total duration in seconds for the simulation
        total_time_duration = get_time_in_seconds() - total_time_start

        !Total time and gflops
        self%tot_time   = total_time_duration
        self%tot_flops  = nflop * nsteps

        self%nthreads   = 1

        self%gflops_avg =      gflops_avg / real(nsteps, real64)
        self%gflops_dev = sqrt(gflops_dev / real(nsteps, real64) - self%gflops_avg**2)

        !Prints the footer
        call self%print_footer()

        !--------------------------!
        !- Particles deallocation -!
        !--------------------------!

        call self%deallocate_particles()

    end subroutine

end module
