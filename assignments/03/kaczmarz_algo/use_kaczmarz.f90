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
    0.0_dp,0.0_dp,0.0_dp,1.0_dp,0.0_dp,1.0_dp,0.0_dp,1.0_dp,&
    0.0_dp,0.0_dp,1.0_dp,0.0_dp,0.0_dp,0.0_dp,0.0_dp,1.0_dp &
    ],&
     [8,9])
  constraints = [6.0_dp,12.0_dp,15.0_dp,15.0_dp,15.0_dp,18.0_dp,15.0_dp,24.0_dp]
  guess = kaczmarz_algo(eq_mat, constraints)
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
end program use_kaczmarz
