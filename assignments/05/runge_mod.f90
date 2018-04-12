module runge
  implicit none
  integer, parameter :: dp =selected_real_kind(15,307)
  contains
  function gradForm(vectArg) result(vector)
    real(kind=dp), intent(in) :: vectArg(:)
    real(kind=dp), allocatable :: vector(:)
    allocate(vector(size(vectArg)))

  end function gradForm

  function rk4Step(gradFunc, state, stepSize) result(stateStep)
    procedure(gradForm) :: gradFunc
    real(kind=dp), intent(in) :: state(:)
    real(kind=dp), intent(in) :: stepSize
    real(kind=dp), allocatable :: stateStep(:)
    allocate(stateStep(size(state)))
    stateStep = 0.0_dp
  end function rk4Step
end module runge

