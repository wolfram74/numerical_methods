program randparse
  implicit none
  real :: sample
  integer, dimension(1) :: seed
  integer :: seed_size
  integer :: lap
  seed(1) = time()
  ! print *, call
  call random_seed(seed_size)
  print *, seed_size
  ! call random_seed!(put=seed)
  ! call random_seed(put=seed)
  ! do lap=1,10
  !   call random_number(sample)
  !   print *, sample
  ! end do
end program randparse
