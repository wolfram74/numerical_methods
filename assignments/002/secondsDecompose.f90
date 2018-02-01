program seconds_decompose
  implicit none
  print *, 'working'
  ! call testing
  print *, seconds_to_hms(4)
  contains
    function seconds_to_hms(seconds) result(hours_mins_secs)
      integer, dimension(3) :: hours_mins_secs
      integer, intent(in) :: seconds
      hours_mins_secs = [1,1,1]
    end function
end program seconds_decompose

! subroutine testing()
!   print *, seconds_to_hms(4)
!   print *, all((/1,2/) == (/1,2/))
!   print *, all((/1,2, 3/) == (/1,1,1/))
! end subroutine
