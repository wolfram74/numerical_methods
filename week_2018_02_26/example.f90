!from http://malayamaarutham.blogspot.com/2006/02/passing-function-names-as-arguments-in.html
! Author : Kamaraju S Kusumanchi
! Email  : kamaraju@gmail.com
! Last edited : Sun Feb  5 2006
!
! Sample program demonstrating the use of external attribute.  This program
! shows how to pass function names as arguments in Fortran 90 programs.
!
! Compilation and execution steps
! $gfortran passing_functions.f90 -o passing_functions
! $./passing_functions
!  beta =    5.500000
!  beta =    1.500000
!
! I would appreciate any comments, feedback, criticism, mistakes, errors etc.,
!   (however minor they are)
!
module dummy
  implicit none
contains
!------------------------------------------------------------------------------
function func1(a)
  implicit none
  real :: a
  real :: func1

  func1 = a+5
end function func1
!------------------------------------------------------------------------------
function func2(b)
  implicit none
  real :: b
  real :: func2

  func2 = b*3.0
end function func2
!------------------------------------------------------------------------------
function func3(dyn_func, c)
  implicit none
  real :: c
  real, external :: dyn_func
  real :: func3

  func3 = dyn_func(c)
end function func3
end module dummy
!------------------------------------------------------------------------------
program passing_functions
  use dummy
  implicit none

  real :: alpha=0.5, beta

  beta = func3(func1, alpha)
  write(*,*) 'beta = ', beta
  beta = func3(func2, alpha)
  write(*,*) 'beta = ', beta
end program passing_functions
