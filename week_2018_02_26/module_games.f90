module mod_a
  integer, parameter :: dp =selected_real_kind(15,307)
  integer :: test1 = 5
  type body
    real(kind=dp) :: mass, vel(3), pos(3)
  end type body
end module mod_a

module mod_b
  use mod_a
  integer :: test2 = 6
end module mod_b

program main
  ! use mod_a
  use mod_b
  implicit none
  type(body) :: earth
  earth%mass = 4.0
  earth%pos = [2.0, 1.2,4.2]
  earth%vel = [0.0, 0.2,0.2]
  print *, earth
  print *, earth%mass
  print *, earth%pos
  print *, earth%vel
  ! print *, test1, test2
end program main
