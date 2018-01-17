program collatz
  implicit none
  character(len=10) :: fork
  integer, dimension(100) :: collatzPath, path
  integer :: current, step_num
  print *, fork(4)
  print *, fork(5)
  print *, collatzPath(path, 4, 1)
end program collatz

function fork(int)
  implicit none
  integer :: int
  character(len=10) :: fork
  if(modulo(int, 2)==0) then
    fork = 'shrink'
  else
    fork = 'grow'
  end if
end function fork

function collatzPath(path, current, step_num)
  implicit none
  integer, dimension(100) :: collatzPath, path
  integer :: current, step_num
  path(step_num) = current
  if(current==1) then
    collatzPath = path
  end if
  if(modulo(current, 2)==0) then
    collatzPath = collatzPath(path, current/2, step_num+1)
  end if
  collatzPath = collatzPath(path, (current*3+1), step_num+1)
end function collatzPath
