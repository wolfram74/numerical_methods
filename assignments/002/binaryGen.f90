program float_to_bin
  ! assumes a floating point
  ! greater than 0 less than 2**16
  implicit none
  ! print *, ['0000000000001001', '1010000000000000']
  ! print *, ( &
  !   ['0000000000001001', '1010000000000000'] &
  !   == ['0000000000001001', '1100000000000000']&
  !   )
  ! print *, size(['0000000000001001', '1010000000000000'])
  ! print *, shape(['0000000000001001', '1010000000000000'])

  stop
  contains
    function bin_generator(float_in) result(bin_vals)
      real, intent(in) :: float_in
      character(16), dimension(2) :: bin_vals

    end function

    function float_splitter(float_in) result(portions)
      real, intent(in) :: float_in
      real :: portions(2)

    end function

    subroutine testing()
      print *, all([40., .25] == float_splitter(40.25))
      print *, all(&
        ['0000000000001001', '1010000000000000']==&
        bin_generator(( 8.+1.+.5 + 2.**(-3) )))
    end subroutine
end program
