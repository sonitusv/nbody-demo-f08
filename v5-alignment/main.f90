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

program nbody_demo

    use iso_fortran_env
    use gsimulation_mod

    implicit none

    character(len=256) :: nparts_str
    character(len=256) :: nsteps_str
    integer(int32)     :: nparts
    integer(int32)     :: nsteps

    integer(int32)     :: status
    type(gsimulation)  :: gsim


    !Checks CLI arguments
    call get_command_argument(1, nparts_str)
    if (nparts_str .ne. '') then

        !Sets number of particles if the first argument is castable to int32
        read(nparts_str, *, iostat=status) nparts
        if (status .ne. 0) stop 'ERROR: invalid value for number of particles'

        gsim%nparts = nparts

    end if

    call get_command_argument(2, nsteps_str)
    if (nsteps_str .ne. '') then

        !Sets number of steps if the second argument is castable to int32
        read(nsteps_str, *, iostat=status) nsteps
        if (status .ne. 0) stop 'ERROR: invalid value for number of steps'

        gsim%nsteps = nsteps

    end if

    !Starts simulation
    call gsim%start()

end program
