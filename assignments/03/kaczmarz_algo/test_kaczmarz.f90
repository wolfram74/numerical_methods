program test_kaczmarz
  use kaczmarz
  implicit none
  ! integer, parameter :: dp =selected_real_kind(15,307)
  real(kind=dp) :: epsi ,test1, test2, test3
  real(kind=dp) :: test4(3)
  real(kind=dp) :: eq_mat4(2,3)
  real(kind=dp) :: constraints4(2)
  epsi =(10_dp**(-8._dp))
  test1 = dot([2._dp, 2._dp, 2._dp], [1._dp, 2._dp, 3._dp])
  print *, abs(test1-12_dp) < epsi
  test2 = magnitude([3._dp, 4._dp])
  print *,abs(test2-5._dp) < epsi
  test3 = delta_relative_magnitude(&
    [1000._dp, 1000._dp, 1000._dp], &
    [1001._dp, 1000._dp, 1000._dp] )
  print *, abs(test3) < .01
  ! eq_mat4 = [&
  !   [1.0_dp,1.0_dp,0.0_dp],&
  !   [0.0_dp,1.0_dp,1.0_dp] ] ! how it ought to be
  eq_mat4 = reshape(&
    [&
    1.0_dp,0.0_dp,&
    1.0_dp,1.0_dp,&
    0.0_dp,1.0_dp &
    ],&
    [2,3]) ! appropriate adjective unbecoming of an academic setting
  constraints4 = [2.0_dp, 37.0_dp]
  test4 = kaczmarz_algo(eq_mat4, constraints4)
  print *, (delta_relative_magnitude(matmul(eq_mat4, test4), constraints4)) < 0.0001
end program test_kaczmarz
