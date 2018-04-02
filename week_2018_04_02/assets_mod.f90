
module assets
  implicit none
  integer, parameter :: dp =selected_real_kind(15,307)

  type body2d
    real(kind=dp) :: mass, location(2), velocity(2)
    contains
    procedure :: distance => distanceIsolated
  end type body2d
  type(body2d), parameter :: nullBody= body2d(&
    0.0_dp,&
    [0.0_dp, 0.0_dp], &
    [0.0_dp, 0.0_dp] )

  contains

  function distanceIsolated(body1, body2) result(scalar)
    class(body2d), intent(in):: body1, body2
    real(kind=dp) :: scalar
    scalar = &
      sum((body1%location-body2%location)**2)**(0.5_dp)
  end function

end module assets

