
program test_algorithms
  use algorithms
  ! use assets
  implicit none
  real(kind=dp) :: length
  type(body2d) :: testBody
  print *, dp
  print *, nullBody%mass
  testBody%mass = 0.0_dp
  testBody%location = [3.0_dp, 4.0_dp]
  testBody%velocity = [0.0_dp, 0.0_dp]
  length = testBody%distance(nullBody)
  ! length = distanceIsolated(testBody, nullBody)
  print *, length
  contains
end program test_algorithms

