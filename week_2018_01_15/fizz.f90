program fizz
  implicit none
  integer :: total, loop_count, divisor_index
  integer, dimension(2) :: divisors
  character(len=120), dimension(2) :: fillers
  character(len=120) :: output
  divisors(1)=3
  divisors(2)=5
  fillers(1)='fizz'
  fillers(2)='buzz'
  total = 100
  do loop_count=1,total
    output = ''
    do divisor_index=1,size(divisors)
      if(modulo(loop_count, divisors(divisor_index))==0) then
        output = trim(output)//trim(fillers(divisor_index))
      end if
    end do
    print *, trim(output)
    if(trim(output)=='') then
      print *, loop_count
    end if
  end do
  print *, 'counting to: ',total
end program fizz
