module kaczmarz
  implicit none
  integer, parameter :: dp =selected_real_kind(15,307)

  contains
  function dot(vec1, vec2) result(scalar)
    real(kind=dp), dimension(:), intent(in) :: vec1, vec2
    real(kind=dp) :: scalar
    scalar = sum(vec1*vec2)
  end function dot

  function magnitude(vec1) result(scalar)
    real(kind=dp), dimension(:), intent(in) :: vec1
    real(kind=dp) :: scalar
    scalar = (dot(vec1, vec1))**0.5_dp
  end function magnitude

  function delta_relative_magnitude(vec1, vec2) result(scalar)
    real(kind=dp), dimension(:), intent(in) :: vec1, vec2
    real(kind=dp) :: scalar
    scalar= 2*magnitude(vec1-vec2)
    scalar = scalar/ (magnitude(vec1)+magnitude(vec2))
  end function delta_relative_magnitude

  function kaczmarz_algo(eqn_mat, b_vec, max_loops_in, precision_in) result(guess)
  ! find guess such that eqn_man*guess - b_vec is close to 0
    real(kind=dp), dimension(:, :), intent(in) :: eqn_mat
    real(kind=dp), dimension(:), intent(in) :: b_vec
    real(kind=dp), allocatable :: guess(:)
    real(kind=dp), optional :: precision_in
    integer, optional :: max_loops_in
    real(kind=dp) :: precision
    integer :: max_loops
    allocate(guess(shape(eqn_mat)(2)))

    print *, shape(eqn_mat)(2)
  ! http://www.personal.psu.edu/jhm/f90/intrinsics/matmul.html
    max_loops = 30
    guess = 0
  end function kaczmarz_algo

end module kaczmarz
