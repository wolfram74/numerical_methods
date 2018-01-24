program collatz
  implicit none
  integer, dimension(100) :: function collatzPath
  integer :: current, start
  print *, collatzPath(4)
  print *, collatzPath(7)
  path = collatzPath(8)
  call shout()
end program collatz


subroutine shout()
  print *, 'shout'
end subroutine shout

function collatzPath(start) result(path)
  implicit none
  ! integer, dimension(100) :: collatzPath
  integer :: start, step_num
  integer, dimension(100) :: path
  step_num = 1
  print *, 'running collatz'
  write(*, *), 'running collatz'
  print *, step_num
  path(1)=4
end function collatzPath
