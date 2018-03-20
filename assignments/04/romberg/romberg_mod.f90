! romberg_algo
  ! needs
    ! a function, a lower and upper bound
  ! optionally
    ! a precision
! R(sub_division_power,layer) == romberg with a certain number of subdivisions and at certain layer
! sub_division_power=SDP
! layer == L
! R(SDP, L+1) = (4**L R(SDP, L) - R(SDP-1, L))/(4**L-1)
! two parameters for R impact precision, larger SDP, and larger L
! SDP >= L
! so if L < SDP and precision not met, try increasing L
! start by initializing a 20x20 array for values of R(SDP, L)
! call array approximations
! R(20,20) if i'm interpretting this correctly would have a precision vastly smaller than floating point, so no point going past.
! loop from 0 to 20 for SDP
!   loop from 0 to SDP for L
!     if L = 0, then approximations(SDP, 0) = trapezoid(SDP, lower, upper, func)
!     else use recurrence relation to find next next value
!     if L === SDP do exit check
!
!
!
!


module romberg
  implicit none
  integer, parameter :: dp =selected_real_kind(15,307)

  contains
  function romberg_algo(func, lower, upper) result(scalar)
    real(kind=dp) :: func
    real(kind=dp), intent(in) :: lower, upper
    real(kind=dp) :: scalar
    real(kind=dp) :: precision=(10_dp**(-3._dp))

    scalar = func(lower)+ func(upper)+4
  end function

  function trapezoid_algo(func, lower, upper, resolution) result(scalar)
    real(kind=dp) :: func
    real(kind=dp), intent(in) :: lower, upper
    integer :: resolution
    real(kind=dp) :: scalar
    real(kind=dp) :: x_spacing
    integer :: sample_points, i
    real(kind=dp), dimension(:),allocatable :: weights, sample_xs

    sample_points = 2**resolution
    x_spacing = (upper-lower)/(2**resolution)
    allocate(sample_xs (0:sample_points))
    allocate(weights (0:sample_points))
    sample_xs = x_spacing*(/(i, i=0,sample_points, 1)/)+lower
    weights = 2.0_dp
    weights(0) = weights(0)-1.0_dp
    weights(sample_points) = weights(sample_points)-1.0_dp
    scalar = func(lower)+ func(upper)+4
  end function
end module romberg
