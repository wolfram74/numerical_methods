program stuffs
  real, dimension(0:1000) :: x

  real :: dx
  dx = .1

  x = (/(k*dx, k=0,int(100/dx))/)
  print *, x(0)
  print *, x(3)
end program stuffs


subroutine InitialiseRNG()
  integer                                 :: seed_size, clock, i
  integer,              allocatable, save :: seed(:)

  if (allocated(seed)) then
    deallocate(seed)
  end if
  if (.not. allocated(seed)) then
    call random_seed(size=seed_size)
    allocate(seed(seed_size))
    call system_clock(count=clock)
    seed = clock + 37 * [( i - 1, i = 1, seed_size )]
    call random_seed(put=seed)
  end if
end subroutine
