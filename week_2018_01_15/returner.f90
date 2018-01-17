program collatz
  implicit none
  call fork(4)
  call fork(5)
end program collatz

subroutine fork(int)
  if(modulo(int, 2)==0) then
    print *, 'shrink'
  else
    print *, 'grow'
  end if
end subroutine
