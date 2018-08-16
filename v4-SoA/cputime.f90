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

module cputime_mod

    use iso_fortran_env
    use omp_lib

    implicit none

    private
    public :: get_time_in_seconds

    !Implementation being used by default
    interface get_time_in_seconds
        module procedure omp_get_time_in_seconds
    end interface

contains

    !Implementation using OpenMP
    real(real64) function omp_get_time_in_seconds()

        implicit none

        omp_get_time_in_seconds = omp_get_wtime()

    end function

    !Implementation using standard library
    real(real64) function sys_get_time_in_seconds()

        implicit none

        integer(int32) :: count, count_rate

        call system_clock(count, count_rate)
        sys_get_time_in_seconds = real(count, real64) / count_rate

    end function

end module
