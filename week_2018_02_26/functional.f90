program function_passing
  implicit none
  integer, parameter :: dp =selected_real_kind(15,307)
  print *, basic_deriv(constant, 1.0_dp)
  contains

  function basic_deriv(func, query, step_in) result(dfunc)
    real(kind=dp), intent(in) :: func
    real(kind=dp), intent(in) :: query
    real(kind=dp), optional :: step_in
    real(kind=dp) :: dfunc, step

    if (present(step_in)) then
      step = step_in
    else
      step = 10.0_dp**(-3.0_dp)
    end if

    dfunc = 2.0_dp
  end function

  function constant(x) result(y)
    real(kind=dp), intent(in) :: x
    real(kind=dp) :: y
    y = x
    y = 2.0_dp
  end function
end program function_passing
