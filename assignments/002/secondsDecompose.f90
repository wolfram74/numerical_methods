program seconds_decompose
  implicit none
  print *, 'working'
  call testing
  stop
  contains
    subroutine testing()
      print *, all([0,0,4] ==seconds_to_hms(4))
      print *, all([1,2,4] ==seconds_to_hms(1*3600 + 2*60 + 4))
    end subroutine
    function seconds_to_hms(seconds) result(hours_mins_secs)
      integer, dimension(3) :: hours_mins_secs
      integer :: secs_per_hour=3600, secs_per_minute=60, remaining_secs
      integer, intent(in) :: seconds

      hours_mins_secs(1) = seconds/secs_per_hour
      remaining_secs = seconds - hours_mins_secs(1)*secs_per_hour
      hours_mins_secs(2) = remaining_secs/secs_per_minute
      remaining_secs = remaining_secs - hours_mins_secs(2)*secs_per_minute
      hours_mins_secs(3) = remaining_secs
     end function
end program seconds_decompose

