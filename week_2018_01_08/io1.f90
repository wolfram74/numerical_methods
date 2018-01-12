program io1
  real :: total, x, y
  print *, 'input 2 values'
  ! read *, x, y
  x = 2.5
  y = 3.7
  total = x+y
  print *, 'answer is ',x+y
  print *, 'answer is via sum', total

end program io1

subroutine collatz
