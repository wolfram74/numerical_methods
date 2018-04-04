
module assets
  implicit none
  integer, parameter :: dp =selected_real_kind(15,307)

  type body2d
    real(kind=dp) :: mass, location(2), velocity(2)
    procedure :: interaction
    contains
    procedure :: distance => distanceIsolated
    procedure :: force => forceIsolated
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

  function forceIsolated(body1, body2) result(vector)
    class(body2d), intent(in) :: body1, body2
    real(kind=dp) :: vector(2)
    real(kind=dp) :: distance
    distance = body1%distance(body2)
    vector = (body1%location-body2%location)/distance
  end function


end module assets

