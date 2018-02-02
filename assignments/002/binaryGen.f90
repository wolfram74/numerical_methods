program float_to_bin
  ! assumes a floating point
  ! greater than 0 less than 2**16
  implicit none
  ! print *, ['0000000000001001', '1010000000000000']
  ! print *, ['0000000000001001', '1010000000000000'] == ['0000000000001001', '1100000000000000']

  stop
  contains
    function float_splitter(float_in) result(portions)
      real, intent(in) :: float_in
      real :: portions(2)

    end function

    subroutine testing()
      print *, all([40., .25] == float_splitter(40.25))
      ! print *, all([])
    end subroutine
end program
