program collatz
  implicit none
  character(len=10) :: fork
  print *, fork(4)
  print *, fork(5)
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
end function
