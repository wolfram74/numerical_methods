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
  function oneD(arg) result(scalar)
    real(kind=dp), intent(in) :: arg
    real(kind=dp):: scalar
  end function

  function romberg_algo(func, lower, upper) result(scalar)
    procedure(oneD) :: func
    real(kind=dp), intent(in) :: lower, upper
    real(kind=dp) :: scalar, current_best, last_best
    real(kind=dp) :: precision=(10_dp**(-6._dp))
    real(kind=dp), dimension(0:19 , 0:19) :: approximations
    integer :: subdivision_power, layer_number
    approximations = 0
    do subdivision_power=0,19
      do layer_number=0, subdivision_power
        if( layer_number .eq. 0) then
          approximations(&
            subdivision_power, layer_number&
            ) = trapezoid_algo(func, lower, upper, subdivision_power)
        else
          approximations(&
            subdivision_power, layer_number&
            ) = (&
            4**layer_number*approximations(subdivision_power, layer_number-1)&
            -approximations(subdivision_power-1, layer_number-1)&
            )/(4**layer_number - 1)
        end if
        if((layer_number == subdivision_power) .and. (subdivision_power > 5)) then
          current_best = approximations(subdivision_power, layer_number)
          last_best = approximations(subdivision_power-1, layer_number-1)
          if(abs(last_best-current_best) < precision) then
            scalar = current_best
            print *, "layer level", subdivision_power
            return
          end if
        end if
      end do
      ! print *, approximations(subdivision_power, 0:5)
    end do
  end function

  function trapezoid_algo(func, lower, upper, resolution) result(scalar)
    real(kind=dp) :: func
    real(kind=dp), intent(in) :: lower, upper
    integer :: resolution
    real(kind=dp) :: scalar
    real(kind=dp) :: x_spacing
    integer :: sample_points, i
    real(kind=dp), dimension(:),allocatable :: weights, sample_xs
    scalar = 0.0_dp
    sample_points = 2**resolution
    x_spacing = (upper-lower)/(2**resolution)
    allocate(sample_xs (0:sample_points))
    allocate(weights (0:sample_points))
    sample_xs = x_spacing*[(i, i=0, sample_points, 1)]+lower
    weights = 1.0_dp
    weights(0) = weights(0)-0.5_dp
    weights(sample_points) = (weights(sample_points)-0.5_dp)
    ! print *, scalar, func(sample_xs(0))*weights(0)*x_spacing
    ! print *, sample_xs(1), func(sample_xs(1))
    ! print *, scalar+func(sample_xs(0))*weights(0)*x_spacing
    do i=0,sample_points
      scalar = scalar + func(sample_xs(i))*weights(i)*x_spacing
      ! print *, i, scalar
    end do
  end function

  subroutine tensor_r2_print(tensor)
    real(kind=dp) :: tensor(:, :)
    integer :: row_index, col_index
    do row_index=1,size(tensor,1)
      !0.nnnE±ee
      do col_index=1,size(tensor,2)
        write( *, '(E9.3E2,2x)', advance="no")&
          (tensor(row_index, col_index))
      end do
      write( *, *) ""
    end do
  end subroutine tensor_r2_print
end module romberg
