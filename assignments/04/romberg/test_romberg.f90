program test_romberg
  use romberg
  implicit none
  real(kind=dp) :: epsi, test1, test2, test3, result3
  real(kind=dp) :: ttest1, ttest2
  real(kind=dp) :: pi, e
  print *, 'running tests'
  epsi = (10_dp**(-8._dp))
  pi = 3.141592653589793_dp
  e = 2.718281828459045_dp
  result3 = e*(5.0_dp*e**2.0_dp-1.0_dp)
  test1 = romberg_algo(test_cos, 0.0_dp, 2.0_dp*pi)
  print *, abs(test1) < epsi
  test2 = romberg_algo(test_cubic, -7.0_dp, 7.0_dp)
  print *, abs(test2) < epsi
  test3 = romberg_algo(test_wonky, 1.0_dp, 3.0_dp)
  print *, result3
  print *, abs( &
    test3-result3&
    ) < epsi
  ttest1 = trapezoid_algo(test_wonky, 1.0_dp, 3.0_dp, 3)
  ttest2 = trapezoid_algo(test_wonky, 1.0_dp, 3.0_dp, 10)
  print *, abs(ttest2-result3) < abs(ttest1-result3)
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

  function test_wonky(arg) result(scalar)
    real(kind=dp), intent(in) :: arg
    real(kind=dp) :: scalar
    scalar = (arg**2)*(e**(arg))
  end function

end program test_romberg
