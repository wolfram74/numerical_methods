program float_to_bin
  ! assumes a floating point
  ! greater than 0 less than 2**16
  implicit none
  call testing
  stop
  contains
    function bin_generator(float_in) result(bin_vals)
      real, intent(in) :: float_in
      ! character(16), dimension(2) :: bin_vals
      character(16) :: bin_vals(2)

    end function

    function float_splitter(float_in) result(portions)
      real, intent(in) :: float_in
      real :: portions(2)
      portions(1) = float(floor(float_in))
      portions(2) = float_in-portions(1)
    end function

    function int_portion(int_float) result(int_bin)
      real, intent(in) :: int_float
      character(16) :: int_bin
      integer :: current_val, pow, index
      current_val = int(int_float)
      do pow=0,15
        index = 16-pow
        if(mod(current_val, 2)==1) then
          int_bin(index:index) = '1'
        else
          int_bin(index:index) = '0'
        end if
        current_val = current_val/2
      end do
    end function

    function frac_portion(frac_float) result(frac_bin)
      real, intent(in) :: frac_float
      character(16) :: frac_bin
    end function

    subroutine testing()
      real :: test1(2)
      character(16) :: test2(2), test3, test4
      test1 = float_splitter(40.25)
      print *, all([40., .25] == test1 )
      test2 = bin_generator(( 8.+1.+.5 + 2.**(-3) ))
      print *, all(&
        ['0000000000001001', '1010000000000000']==test2 &
        )
      test3 = int_portion(16.+2.)
      print *, '0000000000010010' == test3
      test4 = frac_portion(2.**(-3)+2.**(-5))
      print *, '0010100000000000' == test4
    end subroutine
end program
