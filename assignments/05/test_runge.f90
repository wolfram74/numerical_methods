program test_runge
  use runge
  implicit none
  real(kind=dp) :: test1(3), test2(3), test3(3)
  real(kind=dp) :: output1(3), output2(3), output3(3)
  real(kind=dp) :: step, precision
  step = 0.5_dp
  precision = 10.0_dp**(-6.0_dp)
  test1 = [0.0_dp, 0.0_dp, 1.0_dp]
  output1 = rk4Step(noForce1D, test1, step)
  print*, abs(output1(2)-0.5_dp)<precision
  test1 = [0.0_dp, 0.0_dp, 0.0_dp]
  step = 150000.0_dp
  output1 = rk4Step(constForce1D, test1, step)
  output2 = rk4Step(constForce1D, test1, step/2.0_dp)
  test2 = test1 + output2
  output3 = rk4Step(constForce1D, test2, step/2.0_dp)
  test3 = test2+output3
  print*, abs(output1(2)-step**2) > abs(test3(2)-step**2)
  print*, abs(output1(1)-step) < precision
  print*, abs(test3(1)-step) < precision
  print*, output1
  print*, test3
  print*, output1-test3, step**2
  print*, test3(2) - step**2

  contains

  function noForce1D(state) result(deltas)
    real(kind=dp), intent(in) :: state(:)
    real(kind=dp), allocatable :: deltas(:)
    allocate(deltas(size(state)))
    deltas(1) = 1.0_dp
    deltas(2) = state(3)
    deltas(3) = 0.0_dp
  end function noForce1D

  function constForce1D(state) result(deltas)
    real(kind=dp), intent(in) :: state(:)
    real(kind=dp), allocatable :: deltas(:)
    allocate(deltas(size(state)))
    deltas(1) = 1.0_dp
    deltas(2) = state(3)
    deltas(3) = 2.0_dp
  end function constForce1D
end program test_runge

