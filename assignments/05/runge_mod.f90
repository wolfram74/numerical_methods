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
    allocate(&
      deltaState(stateSize),&
      kernel1(stateSize),&
      kernel2(stateSize),&
      kernel3(stateSize),&
      kernel4(stateSize)&
      )
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
    real(kind=dp) ::  stepSize
    integer :: stepNum, stateSize
    stateSize = size(state)

    if (totalSteps < 2) then
      allocate(path(1, stateSize))
      path(1, :) = state
      return
    end if

    allocate(path(totalSteps, stateSize))
    stepSize = (endTime-state(1))/(totalSteps-1)
    path(1, :) = state
    print*, stepSize
    do stepNum=2,totalSteps
      path(stepNum, :) = (&
        path(stepNum-1,:)+ rk4Step(&
          gradFunc, path(stepNum-1,:), stepSize &
          ) &
        )
    end do
  end function nonAdaptiveRK4

  function adaptiveRK4(gradFunc, state, endTime, precision) result(path)
    procedure(gradForm) :: gradFunc
    real(kind=dp), intent(in) :: state(:)
    real(kind=dp), intent(in) :: endTime, precision
    real(kind=dp), allocatable :: path(:, :), doubleStep(:), twoSingleStep(:), lastState(:)
    real(kind=dp) ::  stepSize, timeLeft, maxDisagreement
    ! logical :: running = .true., lastLoop =.false. ! bull shit operation, never do this
    logical :: running, lastLoop
    integer :: totalSteps, stepNum, stateSize
    stateSize = size(state)
    stepSize = 2.0_dp**(-10)
    timeLeft = endTime-state(1)
    totalSteps = 10000
    if (timeLeft < 0.0_dp) then
      print*, 'in the future'
      allocate(path(1, stateSize))
      path(1, :) = state
      return
    end if

    allocate(path(totalSteps, stateSize))
    allocate(lastState(stateSize))
    path = 0.0_dp
    path(1, :) = state
    stepNum = 2
    running = .true.
    lastLoop = .false.
    print*, 'starting loop', running
    do while( running )
      lastState = path(stepNum-1, :)
      if(lastLoop) then
        running = .false.
      end if
      if(stepNum == totalSteps) then
        running = .false.
        print*, 'did not finish, time left'
        print*, timeLeft
      end if
      doubleStep = lastState + rk4Step(gradFunc, lastState, 2.0_dp*stepSize)
      twoSingleStep = lastState + rk4Step(gradFunc, lastState, 1.0_dp*stepSize)
      twoSingleStep = twoSingleStep + rk4Step( &
        gradFunc, twoSingleStep, 1.0_dp*stepSize &
        )
      maxDisagreement = maxRelativeError(doubleStep, twoSingleStep)
      if (maxDisagreement > precision) then
        stepSize = stepSize/1.25_dp
        continue
      end if

      path(stepNum, :) = twoSingleStep

      if (maxDisagreement < (precision*10.0_dp**(-3.0_dp))) then
        stepSize = stepSize*1.25_dp
      end if

      timeLeft = endTime-path(stepNum, 1)
      if (timeLeft < 2.*stepSize .and. running) then
        print*, 'finished with spare cells', stepNum
        stepSize = timeLeft/2.
        lastLoop = .true.
      end if
      ! print*, stepSize
      stepNum = stepNum + 1
    end do
  end function adaptiveRK4

  function maxRelativeError(vec1, vec2) result(scalar)
    real(kind=dp), intent(in) :: vec1(:), vec2(:)
    real(kind=dp) :: scalar
    real(kind=dp), allocatable :: discrepencies(:)
    allocate(discrepencies(size(vec1)))
    discrepencies = vec1-vec2
    scalar = maxval(discrepencies)
  end function maxRelativeError

  function writeOutAtTime(dataVals, nameIn) result(status)
    real(kind=dp), intent(in) :: dataVals(:, :)
    character(len=*), optional :: nameIn
    integer(kind=4) :: fileNumber
    character(len=30) :: fileName
    integer :: status, lineCount

    fileNumber = time()
    if (present(nameIn)) then
      fileName = nameIn
    else
      fileNumber = time()
      write(fileName, '(i10, a4)') fileNumber, '.txt'
    end if

    open(1, file=fileName)
    do lineCount=1,size(dataVals,1)
      write(1, *) dataVals(lineCount, :)
    end do
    close(1)
    status = 4
  end function writeOutAtTime

end module runge

