program test_runge
  use runge
  implicit none
  real(kind=dp) :: test1(3), test2(3), test3(3)
  real(kind=dp) :: output1(3), output2(3), output3(3)
  real(kind=dp) :: step
  step = 0.5_dp
  test1 = [0.0_dp, 0.0_dp, 1.0_dp]
  output1 = rk4Step(noForce1D, test1, step)

  contains
  function noForce1D(state) result(deltas)
    real(kind=dp), intent(in) :: state(:)
    real(kind=dp), allocatable :: deltas(:)
    allocate(deltas(size(state)))
    deltas(1) = 1.0_dp
    deltas(2) = state(3)
    deltas(3) = 0.0_dp
  end function noForce1D
end program test_runge

