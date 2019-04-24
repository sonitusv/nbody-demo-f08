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

module particle_mod

    use iso_fortran_env
    use types_mod

    implicit none

    private
    public particle

    type particle
        real(real_t), allocatable :: pos_x(:)    !position
        real(real_t), allocatable :: pos_y(:)
        real(real_t), allocatable :: pos_z(:)

        real(real_t), allocatable :: vel_x(:)    !velocity
        real(real_t), allocatable :: vel_y(:)
        real(real_t), allocatable :: vel_z(:)

        real(real_t), allocatable :: acc_x(:)    !acceleration
        real(real_t), allocatable :: acc_y(:)
        real(real_t), allocatable :: acc_z(:)

        real(real_t), allocatable :: mass(:)     !mass

        !dir$ attributes align:64 :: pos_x
        !dir$ attributes align:64 :: pos_y
        !dir$ attributes align:64 :: pos_z
        !dir$ attributes align:64 :: vel_x
        !dir$ attributes align:64 :: vel_y
        !dir$ attributes align:64 :: vel_z
        !dir$ attributes align:64 :: acc_x
        !dir$ attributes align:64 :: acc_y
        !dir$ attributes align:64 :: acc_z
        !dir$ attributes align:64 :: mass

        contains
            procedure, public :: alloc
            procedure, public :: dealloc
    end type

contains

    subroutine alloc(self, nparts)

        implicit none

        class(particle), intent(inout) :: self
        integer(int32),  intent(in)    :: nparts

        call self%dealloc()

        allocate(self%pos_x(nparts))
        allocate(self%pos_y(nparts))
        allocate(self%pos_z(nparts))

        allocate(self%vel_x(nparts))
        allocate(self%vel_y(nparts))
        allocate(self%vel_z(nparts))

        allocate(self%acc_x(nparts))
        allocate(self%acc_y(nparts))
        allocate(self%acc_z(nparts))

        allocate(self%mass(nparts))

    end subroutine

    subroutine dealloc(self)

        implicit none

        class(particle), intent(inout) :: self

        if (allocated(self%pos_x)) deallocate(self%pos_x)
        if (allocated(self%pos_y)) deallocate(self%pos_y)
        if (allocated(self%pos_z)) deallocate(self%pos_z)

        if (allocated(self%vel_x)) deallocate(self%vel_x)
        if (allocated(self%vel_y)) deallocate(self%vel_y)
        if (allocated(self%vel_z)) deallocate(self%vel_z)

        if (allocated(self%acc_x)) deallocate(self%acc_x)
        if (allocated(self%acc_y)) deallocate(self%acc_y)
        if (allocated(self%acc_z)) deallocate(self%acc_z)

        if (allocated(self%mass)) deallocate(self%mass)

    end subroutine

end module
