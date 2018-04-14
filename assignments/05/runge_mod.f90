module runge
  implicit none
  integer, parameter :: dp =selected_real_kind(15,307)
  contains
  function gradForm(vectArg) result(vector)
    real(kind=dp), intent(in) :: vectArg(:)
    real(kind=dp), allocatable :: vector(:)
    allocate(vector(size(vectArg)))
  end function gradForm

  function rk4Step(gradFunc, state, stepSize) result(deltaState)
    procedure(gradForm) :: gradFunc
    real(kind=dp), intent(in) :: state(:)
    real(kind=dp), intent(in) :: stepSize
    real(kind=dp), allocatable :: deltaState(:)
    real(kind=dp), allocatable :: kernel1(:), kernel2(:),&
                                  kernel3(:), kernel4(:)
    integer :: stateSize
    stateSize = size(state)
    allocate(deltaState(stateSize) )
    allocate(kernel1(stateSize))
    allocate(kernel2(stateSize))
    allocate(kernel3(stateSize))
    allocate(kernel4(stateSize))
    kernel1 = stepSize * gradFunc(state + 0)
    kernel2 = stepSize * gradFunc(state + kernel1*0.5_dp)
    kernel3 = stepSize * gradFunc(state + kernel2*0.5_dp)
    kernel4 = stepSize * gradFunc(state + kernel3)
    deltaState = (6.0_dp**(-1))*(&
      kernel1 +&
      2.0_dp * kernel2 +&
      2.0_dp * kernel3 +&
      kernel4 &
      )
  end function rk4Step

  function nonAdaptiveRK4(gradFunc, state, totalSteps, endTime) result(path)
    procedure(gradForm) :: gradFunc
    real(kind=dp), intent(in) :: state(:)
    real(kind=dp), intent(in) :: endTime
    integer, intent(in) :: totalSteps
    real(kind=dp), allocatable :: path(:, :)
    real(kind=dp) :: stateSize, stepSize
    integer :: stepNum
    stateSize = size(state)

    if (totalSteps < 2) then
      allocate(path(1, stateSize))
      path(1, :) = state
      return
    end if

    allocate(path(totalSteps, stateSize))
    stepSize = (endTime-state(1))/(totalSteps-1)
    path(1, :) = state

    do stepNum=2,totalSteps
      path(stepNum, :) = (&
        path(stepNum-1,:)+ rk4Step(&
          gradFunc, path(stepNum-1,:), stepSize &
          ) &
        )
    end do
  end function nonAdaptiveRK4
end module runge

