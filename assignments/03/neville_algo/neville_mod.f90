module neville
  implicit none
  integer, parameter :: dp =selected_real_kind(15,307)

  contains
  function neville_algo(x_vals, y_vals, query) result(guess_error)
  real(kind=dp), intent(in) :: x_vals(:), y_vals(:), query
  real(kind=dp) :: guess_error(2)
  guess_error = 0
  end function neville_algo
end module neville
