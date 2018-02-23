program test_neville
  use neville
  implicit none
  ! integer, parameter :: dp =selected_real_kind(15,307)
  real(kind=dp) :: epsi
  real(kind=dp) :: testx(3), testy(3), test1(2)
  epsi =(10_dp**(-3._dp))
  print *, 'running test cases'
  testx = [-0.44_dp, 2.04_dp, 5.0_dp]
  testy = [0.2819_dp, 2.8115_dp, 1.0_dp]
  test1 = neville_algo(testx, testy, 3.76_dp)
  print *, abs(test1(1)-2.3987_dp)<epsi
end program test_neville
