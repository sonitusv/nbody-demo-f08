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
