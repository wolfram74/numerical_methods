program randparse
  implicit none
  real :: sample
  integer(kind=16),dimension(1) :: seed
  integer :: seed_size
  ! integer(kind=16), dimension(1) :: curr_seed
  integer(kind=4) :: curr_seed
  integer :: lap
  call random_seed(seed_size)
  seed(1) = time()
  call random_seed(get=curr_seed)
  ! print *, seed_size, curr_seed
  print *, seed_size
  ! print *, seed
  ! print *, size( seed)
  ! print *, kind( seed(1))
  ! print *, kind(seed_size)
  ! call random_seed!(put=seed)
  ! call random_seed(put=seed)
  ! do lap=1,10
  !   call random_number(sample)
  !   print *, sample
  ! end do
end program randparse
