module romberg
  implicit none
  integer, parameter :: dp =selected_real_kind(15,307)

  contains
  function romberg_algo(func, lower, upper) result(scalar)
    real(kind=dp) :: func
    real(kind=dp), intent(in) :: lower, upper
    real(kind=dp) :: scalar
    scalar = func(lower)+ func(upper)+4
  end function
end module romberg
