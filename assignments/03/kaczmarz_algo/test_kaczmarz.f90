program test_kaczmarz
  use kaczmarz
  implicit none
  ! integer, parameter :: dp =selected_real_kind(15,307)
  real(kind=dp) :: test1
  test1 = dot([2._dp, 2._dp, 2._dp], [1._dp, 2._dp, 3._dp])
  print *, (test1-12_dp)<(10_dp**(-8._dp))
end program test_kaczmarz
