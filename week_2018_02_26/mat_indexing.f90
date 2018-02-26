program trials
  implicit none
  real, allocatable :: testing(:, :)
  integer :: row_index, col_index
  allocate(testing(3, 4))
  testing = 0
  do row_index=1,size(testing,1)
    do col_index = 1, size(testing, 2)
      testing(row_index, col_index) = col_index+(row_index-1)*(size(testing, 2))
    end do
  end do

  call tensor_r2_print(testing)
  deallocate(testing)
  contains

  function tensor_r2_printer(tensor) result(output)
    real :: tensor(:, :)
    integer :: output, row_index
    output = 4
    print *, 'farts'
    do row_index=1,size(tensor,1)
      print *, tensor(row_index, :)
    end do
  end function

  subroutine tensor_r2_print(tensor)
    real :: tensor(:, :)
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

end program trials
