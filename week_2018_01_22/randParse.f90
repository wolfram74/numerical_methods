program randparse
  implicit none
  integer(kind=4),dimension(12) :: new_seed
  integer :: lap
  integer :: seed_size
  real(kind=8) :: sample

  !initializing random generator
  !figure out length of seed array size
  call random_seed(size=seed_size)
  do lap=1,seed_size
    new_seed(lap)=time()
  end do
  !put accepts array of pre-defined length consisting of pre-defined data type
  !can not be changed?
  !question: how to find out length and data type and make array without use of magic numbers on line 3
  call random_seed(put=new_seed)


  do lap=1,10
    call random_number(sample)
    print *, sample
  end do
end program randparse
