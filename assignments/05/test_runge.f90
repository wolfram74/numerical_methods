program test_runge
  use runge
  implicit none
  real(kind=dp) :: test1(3), test2(3), test3(3)
  real(kind=dp) :: output1(3), output2(3), output3(3)
  real(kind=dp) :: path1(1, 3), path10(10, 3)
  real(kind=dp), allocatable :: pathLong(:, :)
  real(kind=dp) :: step, precision, expected, pi
  integer :: steps
  pi = 3.141592653_dp
  step = 0.5_dp
  precision = 10.0_dp**(-6.0_dp)
  call testNoForce()
  call increasePrecisionTest()
  call testNonAdaptivePath()

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

  function harmonicForce(state) result(deltas)
    real(kind=dp), intent(in) :: state(:)
    real(kind=dp), allocatable :: deltas(:)
    allocate(deltas(size(state)))
    deltas = 0
    deltas(1) = 1.0_dp
    deltas(2) = state(3)
    deltas(3) = -state(2)
  end function harmonicForce

  subroutine testNoForce()
    test1 = [0.0_dp, 0.0_dp, 1.0_dp]
    output1 = rk4Step(noForce1D, test1, step)
    print*, 'no force test'
    print*, abs(output1(2)-0.5_dp)<precision
  end subroutine testNoForce

  subroutine increasePrecisionTest()
    test1 = [0.0_dp, 1.0_dp, 0.0_dp]
    step = 0.5_dp*3.141592653_dp
    expected = 0.0_dp
    output1 = rk4Step(harmonicForce, test1, step)
    output2 = rk4Step(harmonicForce, test1, step/2.0_dp)
    test2 = test1 + output2
    output3 = rk4Step(harmonicForce, test2, step/2.0_dp)
    test3 = test2+output3
    print*, 'varrying precision tests'
    print*, abs(output1(2)-expected) > abs(test3(2)-expected)
    print*, output1(2)-expected
    print*, test3(2)-expected
    print*, output1-test3, expected
  end subroutine increasePrecisionTest

  subroutine testNonAdaptivePath()
    path1 = nonAdaptiveRK4(harmonicForce, test1, 1, pi)
    steps = 100
    allocate(pathLong(steps,3))
    print*, 'varrying path length tests'
    print*, path1
    pathLong = nonAdaptiveRK4(harmonicForce, test1, steps, 2.0_dp*pi)
    print*, pathLong(steps,2),pathLong(1,2), precision
    print*, abs(pathLong(steps,2)-pathLong(1,2))<precision
    steps = writeOutAtTime(pathLong)

  end subroutine testNonAdaptivePath

end program test_runge

