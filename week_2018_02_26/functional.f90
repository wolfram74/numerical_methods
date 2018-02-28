program function_passing
  implicit none
  integer, parameter :: dp =selected_real_kind(15,307)
  write(*,*) basic_deriv(constant_f, 1.0_dp)
  write(*,*) basic_deriv(linear_f, 1.0_dp)
  write(*,*) basic_deriv(quadra_f, 1.0_dp)
  contains

  function basic_deriv(func, query, step_in) result(dfunc)
    real(kind=dp) :: func
    ! real(kind=dp), intent(in) :: func !declaring intent breaks?
    real(kind=dp), intent(in) :: query
    real(kind=dp), optional :: step_in
    real(kind=dp) :: dfunc, step

    if (present(step_in)) then
      step = step_in
    else
      step = 10.0_dp**(-3.0_dp)
    end if

    dfunc = (func(query+step/2.0)-func(query-step/2.0))/step
  end function basic_deriv

  function constant_f(x) result(y)
    real(kind=dp), intent(in) :: x
    real(kind=dp) :: y
    y = x
    y = 2.0_dp
  end function constant_f
  function linear_f(x) result(y)
    real(kind=dp), intent(in) :: x
    real(kind=dp) :: y
    y = 4.0_dp*x
  end function linear_f

  function quadra_f(x) result(y)
    real(kind=dp), intent(in) :: x
    real(kind=dp) :: y
    y = 4.0_dp*x+2.0_dp*x**2.0
  end function quadra_f
end program function_passing
