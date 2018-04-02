
program test_algorithms
  use algorithms
  ! use assets
  implicit none
  real(kind=dp) :: length
  type(body2d) :: testBody
  print *, dp
  testBody%mass = 0.0_dp
  testBody%location = [5.0_dp, 0.0_dp]
  testBody%velocity = [0.0_dp, 0.0_dp]
  length = testBody%distance(nullBody)
  ! length = distanceIsolated(testBody, nullBody)
  print *, length
  print *, nullBody%mass
  contains
end program test_algorithms

