program use_neville
  use neville
  implicit none
  integer :: n, i
  real(kind=dp), allocatable :: x_vals(:), y_vals(:)
  real(kind=dp) :: result(2), pi
  n = 11
  allocate(x_vals(n))
  allocate(y_vals(n))
  pi = 3.1415_dp
  do i = 0, n-1
    x_vals(i) = 2.0_dp*pi*i/(n-1)
  end do
  y_vals = cos(x_vals)
  result = neville_algo(x_vals, y_vals, pi/4.0)
  print *, result
  result = neville_algo(x_vals, y_vals, 5.0*pi/4.0)
  print *, result
  print *, 'running use case'
end program use_neville
