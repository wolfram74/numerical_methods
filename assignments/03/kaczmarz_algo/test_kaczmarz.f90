program test_kaczmarz
  use kaczmarz
  implicit none
  ! integer, parameter :: dp =selected_real_kind(15,307)
  real(kind=dp) :: epsi ,test1, test2, test3
  epsi =(10_dp**(-8._dp))
  test1 = dot([2._dp, 2._dp, 2._dp], [1._dp, 2._dp, 3._dp])
  print *, abs(test1-12_dp) < epsi
  test2 = magnitude([3._dp, 4._dp])
  print *,abs(test2-5._dp) < epsi
  test3 = delta_relative_magnitude(&
    [1000._dp, 1000._dp, 1000._dp], &
    [1001._dp, 1000._dp, 1000._dp] )
  print *, abs(test3) < .01

end program test_kaczmarz
