program trials
  implicit none
  real, allocatable :: testing(:, :)
  integer :: result, row_index, col_index
  allocate(testing(3, 4))
  testing = 0
    do row_index=1,size(testing,1)
      do col_index = 1, size(testing, 2)
        testing(row_index, col_index) = col_index+(row_index-1)*(size(testing, 2))
      end do
    end do

  result = tensor_2_printer(testing)
  contains
  function tensor_2_printer(tensor) result(output)
    real :: tensor(:, :)
    integer :: output, row_index
    output = 4
    print *, 'farts'
    do row_index=1,size(tensor,1)
      print *, tensor(row_index, :)
    end do
  end function

end program trials
