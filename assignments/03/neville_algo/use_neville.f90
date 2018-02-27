program use_neville
  use neville
  implicit none
  integer :: n, i
  real(kind=dp), allocatable :: x_vals(:), y_vals(:)
  real(kind=dp), allocatable :: x_vals_exp(:), y_vals_exp(:)
  real(kind=dp) :: result(2), pi
  n = 11
  allocate(x_vals(n))
  allocate(y_vals(n))
  allocate(x_vals_exp(3))
  allocate(y_vals_exp(3))
  pi = 3.1415_dp
  do i = 1, n
    x_vals(i) = 2.0_dp*pi*i/(n-1)
  end do
  y_vals = cos(x_vals)
  result = neville_algo(x_vals, y_vals, pi/4.0)
  print *, result
  print *, 'actual difference', result(1)-cos(pi/4.0)
  print *, 'estimated error via formula', error_estimate_for_trig(pi/4.0, x_vals)
  result = neville_algo(x_vals, y_vals, 5.0*pi/4.0)
  print *, result
  print *, 'actual difference', result(1)-cos(5.0*pi/4.0)
  print *, 'estimated error via formula', error_estimate_for_trig(5.0*pi/4.0, x_vals)
  ! x_vals_exp = [0.0_dp, 0.1_dp, 0.3_dp]
  ! y_vals_exp = [1.0_dp, 1.1052_dp, 1.3499_dp]
  ! result = neville_algo(x_vals_exp, y_vals_exp, 0.2_dp)
  ! print *, result
  contains
  function error_estimate_for_trig(query, x_vals) result(esti)
    real(kind=dp) :: query
    real(kind=dp) :: x_vals(:)
    real(kind=dp) :: esti
    esti = product(x_vals-query)/float(fact(size(x_vals)))
  end function error_estimate_for_trig

  !copied from reference as no built in factorial function was found
  recursive function fact(i) result(j)
      integer, intent(in) :: i
      integer :: j
      if (i == 1) then
          j = 1
      else
          j = i * fact(i - 1)
      end if
  end function fact
  end program use_neville
