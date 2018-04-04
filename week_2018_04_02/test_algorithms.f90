
program test_algorithms
  use algorithms
  ! use assets
  implicit none
  real(kind=dp) :: length, direction(2)
  type(body2d) :: testBody
  print *, dp
  print *, nullBody%mass
  testBody%mass = 0.0_dp
  testBody%location = [3.0_dp, 4.0_dp]
  testBody%velocity = [0.0_dp, 0.0_dp]
  testBody%interaction = harmonic
  length = testBody%distance(nullBody)
  direction = testBody%force(nullBody)
  ! length = distanceIsolated(testBody, nullBody)
  print *, length
  print *, direction
  contains
  function harmonic(body1, body2) result(vector)
    class(body2d), intent(in):: body1, body2
    real(kind=dp) :: vector(2)
    vector = 0
  end function
end program test_algorithms

