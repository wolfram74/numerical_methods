program golden_mean

!Example for Recursion Formula of Golden Mean

implicit none
integer, parameter :: dp =selected_real_kind(15,307)
real(kind=dp)    :: phi_0, phi_1
real(kind=dp)    :: phi_n, phi_n_minus1, phi_n_plus1
real(kind=dp)    :: correct_answer
integer :: n
real(kind=dp) :: deviation

! Initialize phi_0 and phi_1 (These are needed to start the recursion)
! (To calculate the square root of 5.0 use ‘sqrt(5.0) )
phi_0 = 1.0_dp
! phi_1 = (sqrt(5.0)-1.0_dp)/2.0_dp
phi_1 = (1.0_dp)/1.3_dp

! These are used initially on the RHS of the recursion formula
phi_n_minus1 = phi_0
phi_n        = phi_1

do n = 2, 100
   phi_n_plus1    = phi_n_minus1 - phi_n      ! Use Recursion Formula
   correct_answer = (phi_1)**(n)  ! Correct answer
   deviation = (phi_n_plus1-correct_answer)/correct_answer
   ! write(*,*)  log10(abs(deviation))
   ! write(*,*)  n, phi_n_plus1, correct_answer,  log10(abs(deviation))
   ! write(*,*)  n, phi_n/phi_n_minus1, phi_n/phi_n_minus1-phi_1
   ! write(*,*)  n, phi_n/phi_n_minus1, (phi_n/phi_n_minus1)/phi_1
   write(*,*)  n, phi_n/phi_n_minus1
   phi_n_minus1   = phi_n                     ! Prepare for the next recursion
   phi_n          = phi_n_plus1
end do

end program golden_mean
