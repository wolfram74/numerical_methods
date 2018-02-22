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
    real(kind=dp), allocatable :: guess(:), curr_row(:)

    real(kind=dp), optional :: precision_in
    integer, optional :: max_loops_in
    real(kind=dp) :: precision
    integer :: max_loops

    integer :: loop_count, num_of_rows, curr_row_index
    allocate(guess(size(eqn_mat,2)))
    allocate(curr_row(size(eqn_mat,2)))
    num_of_rows = size(eqn_mat, 1)

    if (present(max_loops_in)) then
      max_loops = max_loops_in
    else
      max_loops = 100
    end if
    if (present(precision_in)) then
      precision = precision_in
    else
      precision = 10.0_dp**(-6.0_dp)
    end if

  ! http://www.personal.psu.edu/jhm/f90/intrinsics/matmul.html
    guess = 0


    do loop_count=1,max_loops
      curr_row_index = modulo(loop_count, num_of_rows)+1
      curr_row = eqn_mat(curr_row_index, :)
      guess = guess + curr_row*(&
        b_vec(curr_row_index) -dot(curr_row, guess)&
        )/(magnitude(curr_row)**2.0_dp)
    end do

  end function kaczmarz_algo

end module kaczmarz
