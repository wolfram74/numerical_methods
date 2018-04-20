program main_project
  use runge
  implicit none
  real(kind=dp) :: initial(3), planetInit(5)
  real(kind=dp), allocatable :: path(:, :)
  real(kind=dp) :: pi, precision
  integer :: steps, status
  pi = 3.141592653_dp
  ! call useNonAdaptive()
  ! call useAdaptive1()
  ! call nonAdaptiveOrbit()
  call adaptiveOrbit()
  contains
  subroutine useNonAdaptive()
    initial = 0.0_dp
    steps = 4000
    allocate(path(steps, 3))
    path = nonAdaptiveRK4(drivenSHO, initial, steps, 100.0_dp*pi)
    status = writeOutAtTime(path)
    deallocate(path)
  end subroutine useNonAdaptive

  subroutine useAdaptive1()
    initial = 0.0_dp
    precision = 10.0_dp**(-9.0_dp)
    steps = 10000
    allocate(path(steps, 3))
    path = adaptiveRK4(drivenSHO, initial, 100.0_dp*pi, precision)
    status = writeOutAtTime(path)
    deallocate(path)
  end subroutine useAdaptive1

  subroutine nonAdaptiveOrbit()
    planetInit = [0.0_dp, 1.0_dp, 0.0_dp, 0.0_dp, 2.0_dp*pi]
    steps = 120
    allocate(path(steps, 3))
    path = nonAdaptiveRK4(keplerian, planetInit, steps, 1.0_dp)
    status = writeOutAtTime(path)
    deallocate(path)
  end

  subroutine adaptiveOrbit()
    planetInit = [0.0_dp, 1.0_dp, 0.0_dp, 0.0_dp, 2.0_dp*pi]
    precision = 10.0_dp**(-3.0_dp)
    steps = 10000
    allocate(path(steps, 3))
    path = adaptiveRK4(keplerian, planetInit, 1.0_dp, precision)
    status = writeOutAtTime(path)
    deallocate(path)

  end subroutine adaptiveOrbit
  function drivenSHO(state) result(deltas)
    real(kind=dp), intent(in) :: state(:)
    real(kind=dp), allocatable :: deltas(:)
    allocate(deltas(size(state)))
    deltas = 0.0_dp
    deltas(1) = 1.0_dp
    deltas(2) = state(3)
    deltas(3) = cos(1.1_dp*state(1)) - state(2) -(0.05_dp)*state(3)
  end function drivenSHO

  function harmonicForce(state) result(deltas)
    real(kind=dp), intent(in) :: state(:)
    real(kind=dp), allocatable :: deltas(:)
    allocate(deltas(size(state)))
    deltas = 0
    deltas(1) = 1.0_dp
    deltas(2) = state(3)
    deltas(3) = -state(2)
  end function harmonicForce

  function keplerian(state) result(deltas)
    real(kind=dp), intent(in) :: state(:)
    real(kind=dp), allocatable :: deltas(:)
    real(kind=dp) :: radius, force
    allocate(deltas(size(state)))
    radius = (state(2)**2+state(3)**2)**(0.5_dp)
    force = -4.0_dp*pi**(2.0)/radius**3.0
    deltas(1) = 1.0_dp
    deltas(2) = state(4)
    deltas(3) = state(5)
    deltas(4) = state(2)*force
    deltas(5) = state(3)*force
  end function keplerian
end program main_project

