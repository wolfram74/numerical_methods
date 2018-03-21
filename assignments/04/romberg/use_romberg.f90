program use_romberg
  ! use romberg only : romberg_algo
  use romberg
  implicit none
  real(kind=dp) :: pi = 3.141592653589793_dp

  call part_b()
  ! call part_c()
  contains
  function b_a(arg) result(scalar)
    real(kind=dp), intent(in) :: arg
    real(kind=dp) :: scalar
    scalar = exp(arg)
  end function
  function b_b(arg) result(scalar)
    real(kind=dp), intent(in) :: arg
    real(kind=dp) :: scalar
    scalar = (sin(8*arg))**2
  end function
  function b_c(arg) result(scalar)
    real(kind=dp), intent(in) :: arg
    real(kind=dp) :: scalar
    scalar = exp(-arg)*(1.0_dp+arg**2.0_dp)**(-1.0_dp)
  end function
  function b_c_alt(arg) result(scalar)
    real(kind=dp), intent(in) :: arg
    real(kind=dp) :: scalar, sub_arg, dsub_arg
    sub_arg = arg/(1.0_dp-arg)
    dsub_arg = 1.0_dp/(1.0_dp-arg)**2
    scalar = exp(-sub_arg)/(1.0_dp+sub_arg**2)*dsub_arg
  end function

  function capacity_integrand(arg) result(scalar)
    real(kind=dp), intent(in) :: arg
    real(kind=dp) :: scalar
    scalar = exp(arg)*(arg**4.0_dp)*(exp(arg)-1.0_dp)**(-2.0_dp)
    if(isnan(scalar)) then
      scalar=0.0_dp
    end if
  end function

  subroutine part_b()
    real(kind=dp) :: val_a, val_b, val_c, val_c_alt
    val_a = romberg_algo(b_a, 0.0_dp, 1.0_dp)
    print *, val_a
    val_b = romberg_algo(b_b, 0.0_dp, 2.0_dp*pi)
    print *, val_b
    print *, 'value b in multiples of pi', val_b/pi
    val_c = romberg_algo(b_c, 0.0_dp, 10.0_dp**(3.0_dp))
    print *, val_c
    print *, 'error using naive form'
    print *, (val_c-0.62144962423581_dp)
    val_c_alt = romberg_algo(b_c_alt, 0.0_dp, 1.0_dp-10.0_dp**(-6.0_dp))
    print *, val_c_alt
    print *, 'error using alt form'
    print *, (val_c_alt-0.62144962423581_dp)
  end subroutine

  subroutine part_c()
    real(kind=dp) :: spec_heat=309, ratio, result
    integer :: samples = 1083, i
    real(kind=dp), dimension(:), allocatable :: temp
    allocate(temp(samples))
    temp = [(i, i=1, samples, 1)]
    do i=1,samples
      ratio = temp(i)/spec_heat
      ! print *, temp(i), &
      !   ratio**3, 1.0_dp/ratio
      result = romberg_algo(capacity_integrand,10.0_dp**(-9),1.0_dp/ratio)
      ! print *, result
      ! print *, temp(i), ratio**3*result
      ! print *, temp(i)
      ! print *, ratio**3*result
    end do
  end subroutine
end program use_romberg
! 3b3 ~ 0.621449624235813357
