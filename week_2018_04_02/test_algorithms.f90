
program test_algorithms
  use algorithms
  ! use assets
  implicit none
  real(kind=dp) :: length
  print *, dp
  type(body2d) :: testBody
  testBody%mass = 0.0_dp
  testBody%location = [5.0_dp, 0.0_dp]
  testBody%velocity = [0.0_dp, 0.0_dp]
  length = testBody%distance(nullBody)
  print *, length
  contains
end program test_algorithms

