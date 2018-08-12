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
        real(real_t) :: pos(3) = 0.0    !position
        real(real_t) :: vel(3) = 0.0    !velocity
        real(real_t) :: acc(3) = 0.0    !acceleration
        real(real_t) :: mass   = 0.0    !mass
    end type

end module
