program use_kaczmarz
  use kaczmarz
  implicit none
  real(kind=dp) :: eq_mat(8,9)
  real(kind=dp) :: constraints(8)
  real(kind=dp) :: guess(9)
  eq_mat = reshape(&
    [&
    1.0_dp,1.0_dp,1.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp,&
    1.0_dp,0.0_dp,0.0_dp,1.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp,&
    1.0_dp,0.0_dp,0.0_dp,0.0_dp,1.0_dp,1.0_dp,0.0_dp,0.0_dp,&
    0.0_dp,1.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp,1.0_dp,0.0_dp,&
    0.0_dp,0.0_dp,1.0_dp,1.0_dp,1.0_dp,0.0_dp,1.0_dp,0.0_dp,&
    0.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp,1.0_dp,1.0_dp,0.0_dp,&
    0.0_dp,1.0_dp,0.0_dp,0.0_dp,1.0_dp,0.0_dp,0.0_dp,1.0_dp,&
    0.0_dp,0.0_dp,0.0_dp,1.0_dp,0.0_dp,0.0_dp,0.0_dp,1.0_dp,&
    0.0_dp,0.0_dp,1.0_dp,0.0_dp,0.0_dp,1.0_dp,0.0_dp,1.0_dp &
    ],&
     [8,9])
  constraints = [6.0_dp,12.0_dp,15.0_dp,15.0_dp,15.0_dp,18.0_dp,15.0_dp,24.0_dp]
  guess = kaczmarz_algo(eq_mat, constraints)
  call tensor_r2_print(eq_mat)
  print *, 'with default processing of 100 loops'
  print *,  guess
  print *, 'deviation', delta_relative_magnitude(constraints, matmul(eq_mat, guess))
  guess = kaczmarz_algo(eq_mat, constraints, max_loops_in=100000)
  print *, 'with higher loop cap'
  print *,  guess
  print *, 'deviation', delta_relative_magnitude(constraints, matmul(eq_mat, guess))
  guess = kaczmarz_algo(eq_mat, constraints, precision_in=(10.0_dp**(-8.0_dp)), max_loops_in=100000)
  print *, 'with higher loop cap and higher precision'
  print *,  guess
  print *, 'deviation', delta_relative_magnitude(constraints, matmul(eq_mat, guess))
  ! call performance_trend()
  contains
  subroutine tensor_r2_print(tensor)
    real(kind=dp) :: tensor(:, :)
    integer :: row_index, col_index
    do row_index=1,size(tensor,1)
      !0.nnnE±ee
      do col_index=1,size(tensor,2)
        write( *, '(E9.3E2,2x)', advance="no")&
          (tensor(row_index, col_index))
      end do
      write( *, *) ""
    end do
  end subroutine tensor_r2_print

  subroutine performance_trend()
    integer :: loop_count
    integer :: loops
    do loop_count=1, 20
      loops = 10*int(1.35**float(loop_count))
      guess = kaczmarz_algo(eq_mat, constraints, &
        precision_in=(10.0_dp**(-20.0_dp)), &
        max_loops_in= loops)
      print *, loops, ",", delta_relative_magnitude(constraints, matmul(eq_mat, guess))
      ! print *, delta_relative_magnitude(constraints, matmul(eq_mat, guess))
    end do
  end subroutine performance_trend
end program use_kaczmarz
