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
