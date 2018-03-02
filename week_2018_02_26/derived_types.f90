program data_types
  integer, parameter :: dp =selected_real_kind(15,307)
  type body
    real(kind=dp) :: mass, vel(3), pos(3)
  end type body
  type(body) :: earth
  earth%mass = 4.0
  earth%pos = [2.0, 1.2,4.2]
  earth%vel = [0.0, 0.2,0.2]
  print *, earth
  print *, earth%mass
  print *, earth%pos
  print *, earth%vel
end program data_types
