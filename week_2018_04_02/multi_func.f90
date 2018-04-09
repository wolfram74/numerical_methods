program jimjam
  implicit none
  integer, parameter :: dp =selected_real_kind(15,307)
  real(kind=dp) :: state_0(2), state_1(2)
  ! state_1 = evolver([yp, ypp], [1.0_dp, 2.0_dp])
  ! print *, state_1
  print *, yp(state_0)
  contains
  function evolver(transformations, state) result(new_state)
    real(kind=dp) :: transformations(2)
    real(kind=dp), intent(in) :: state(2)
    real(kind=dp) :: new_state(2)

    new_state(1) = state(1)+transformations(1)(state)
    new_state(2) = state(2)+transformations(2)(state)

  end function evolver

  function yp(state) result(scalar)
    real(kind=dp), intent(in) :: state(2)
    real(kind=dp):: scalar
    scalar = 4.0_dp
  end function yp
  function ypp(state) result(scalar)
    real(kind=dp), intent(in) :: state(2)
    real(kind=dp):: scalar
    scalar = 4.0_dp
  end function ypp
end program jimjam
