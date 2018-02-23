program test_neville
  use neville
  implicit none
  ! integer, parameter :: dp =selected_real_kind(15,307)
  real(kind=dp) :: epsi
  real(kind=dp) :: testx(3), testy(3), test1(2)
  epsi =(10_dp**(-3._dp))
  print *, 'running test cases'
  ! testx = [0.1_dp, 0.2_dp, 0.3_dp]
  ! testy = [-1.6228_dp, -0.8218_dp, -0.3027_dp]
  ! test1 = neville_algo(testx, testy, 0.15_dp)
  testx = [-0.44_dp, 2.04_dp, 5.0_dp]
  testy = [0.2819_dp, 2.8115_dp, 1.0_dp]
  test1 = neville_algo(testx, testy, 3.76_dp)
  print *, abs(test1(1)-2.3987_dp)<epsi
  print *, test1(2)
end program test_neville
