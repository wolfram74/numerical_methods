program main_project
  use runge
  implicit none
  real(kind=dp) :: initial(3)
  real(kind=dp), allocatable :: path(:, :)
  real(kind=dp) :: pi
  integer :: steps, status
  pi = 3.141592653_dp
  call useNonAdaptive()
  contains
  subroutine useNonAdaptive()
    ! initial = [0.0_dp,1.0_dp,0.0_dp]
    initial = 0.0_dp
    steps = 4000
    allocate(path(steps, 3))
    path = nonAdaptiveRK4(drivenSHO, initial, steps, 100.0_dp*pi)
    status = writeOutAtTime(path)
  end subroutine useNonAdaptive

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
end program main_project

