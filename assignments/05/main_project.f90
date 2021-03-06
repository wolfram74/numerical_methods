program main_project
  use runge
  implicit none
  real(kind=dp) :: initial(3), planetInit(5), lorenzInit(4)
  real(kind=dp), allocatable :: path(:, :)
  real(kind=dp) :: pi, precision
  integer :: steps, status
  pi = 3.141592653_dp
  ! call useNonAdaptive()
  ! call useAdaptive1()
  ! call nonAdaptiveOrbit()
  ! call adaptiveOrbit()
  ! call adaptiveElliptic()
  ! call cometInquiries()
  call lorenzInquiries()
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

  subroutine adaptiveElliptic()
    planetInit = [0.0_dp, 1.0_dp, 0.0_dp, 0.0_dp, pi/2.0_dp]
    precision = 10.0_dp**(-7.0_dp)
    steps = 10000
    allocate(path(steps, 3))
    path = adaptiveRK4(keplerian, planetInit, 1.5_dp, precision)
    status = writeOutAtTime(path)
    print*, planetInit
    deallocate(path)
  end subroutine adaptiveElliptic

  subroutine cometInquiries()
    real(kind=dp) :: e, q, a
  ! vy = 2.0_dp*pi*(a**(-0.5))
    planetInit = [0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp]
    precision = 10.0_dp**(-4.0_dp)
    steps = 10000
    status = cometOrbitCalc(0.847_dp, 0.339_dp, 'encke.txt', 80.0_dp)
    status = cometOrbitCalc(0.756_dp, 0.861_dp, 'biela.txt', 80.0_dp)
    status = cometOrbitCalc(0.132_dp, 5.540_dp, 'wachmann.txt', 80.0_dp)
    status = cometOrbitCalc(0.967_dp, 0.587_dp, 'halley.txt', 80.0_dp)
    status = cometOrbitCalc(0.969_dp, 0.923_dp, 'grigg.txt', 200.0_dp)
    status = cometOrbitCalc(0.995_dp, 0.913_dp, 'bopp.txt', 3000.0_dp)
  end subroutine cometInquiries

  subroutine lorenzInquiries()
    precision = 10.0_dp**(-3.0_dp)
    steps = 10000
    allocate(path(steps, 4))
    lorenzInit = [0.0_dp, 1.0_dp, 1.0_dp, 20.0_dp]
    path = adaptiveRK4(lorenzAttractor, lorenzInit, 20.0_dp, precision)
    status = writeOutAtTime(path, 'lorenz20.txt')
    deallocate(path)
    allocate(path(steps, 4))
    lorenzInit = [0.0_dp, 1.0_dp, 1.0_dp, 20.01_dp]
    path = adaptiveRK4(lorenzAttractor, lorenzInit, 20.0_dp, precision)
    status = writeOutAtTime(path, 'lorenz20p01.txt')
    deallocate(path)
  end subroutine lorenzInquiries

  function cometOrbitCalc(e, q, name, time) result(status)
    real(kind=dp), intent(in) :: e, q, time
    character(len=*), intent(in) :: name
    real(kind=dp) :: a
    integer :: status
    a = q/(1.0_dp-e)
    planetInit(2)=a
    planetInit(5)=2.0_dp*pi*a**(-0.5_dp)
    print*, name, a
    allocate(path(steps, 3))
    path = adaptiveRK4(keplerian, planetInit, time, precision/(a**3.0))
    status = writeOutAtTime(path, name)
    deallocate(path)
  end function cometOrbitCalc

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

  function lorenzAttractor(state) result(deltas)
    real(kind=dp), intent(in) :: state(:)
    real(kind=dp), allocatable :: deltas(:)
    real(kind=dp) :: sig, r, b
    allocate(deltas(size(state)))
    sig = 10.0_dp
    b = 8.0_dp/3.0
    r = 28.0_dp
    deltas(1) = 1.0_dp
    deltas(2) = sig*(state(3)-state(2))
    deltas(3) = r*state(2)-state(3)-state(2)*state(4)
    deltas(4) = state(2)*state(3) - b * state(4)
  end function lorenzAttractor
end program main_project

