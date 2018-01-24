program arrs
  implicit none
  integer, dimension(10) :: contents
  integer :: loop_num
  print *, 'array jim jams'
  do loop_num=1,12
    print *, loop_num
    contents(loop_num) = loop_num**2
    print *, contents
  end do

end program arrs
