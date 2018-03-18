program test_romberg
  use romberg
  implicit none
  real(kind=dp) :: epsi, test1, test2
  real(kind=dp) :: pi
  print *, 'running tests'
  epsi = (10_dp**(-8._dp))
  pi = 3.141592653589793_dp
  test1 = romberg_algo(test_cos, 0.0_dp, 2.0_dp*pi)
  print *, abs(test1) < epsi
  test2 = romberg_algo(test_cubic, -7.0_dp, 7.0_dp)
  print *, abs(test2) < epsi

  contains
  function test_cos(arg) result(scalar)
    real(kind=dp), intent(in) :: arg
    real(kind=dp) :: scalar
    scalar = dcos(arg)
  end function

  function test_cubic(arg) result(scalar)
    real(kind=dp), intent(in) :: arg
    real(kind=dp) :: scalar
    scalar = 5.0_dp*arg**3-3.0_dp*arg
  end function

end program test_romberg
