!---------------------------------------------------------------------------!
!                                                                           !
!   This file is part of N-Body Simulation Demo.                            !
!                                                                           !
!   Copyright (C) 2018, Aleksei Abliazov <aleksei.abliazov@gmail.com>       !
!                                                                           !
!   This program is free software: you can redistribute it and/or modify    !
!   it under the terms of the GNU General Public License as published by    !
!   the Free Software Foundation, either version 3 of the License, or       !
!   (at your option) any later version.                                     !
!                                                                           !
!   This program is distributed in the hope that it will be useful,         !
!   but WITHOUT ANY WARRANTY; without even the implied warranty of          !
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the            !
!   GNU General Public License for more details.                            !
!                                                                           !
!   You should have received a copy of the GNU General Public License       !
!   along with this program. If not, see <https://www.gnu.org/licenses/>.   !
!                                                                           !
!---------------------------------------------------------------------------!

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
