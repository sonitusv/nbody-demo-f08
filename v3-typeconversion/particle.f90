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
        real(real_t) :: pos(3) = 0.0    !position
        real(real_t) :: vel(3) = 0.0    !velocity
        real(real_t) :: acc(3) = 0.0    !acceleration
        real(real_t) :: mass   = 0.0    !mass
    end type

end module
