program test_dumby_mod
  use dumby_mod
  print *, increaser(4.0) > 4.0
  print *, increaser(5.0) > 5.5
  print *, increaser(5.0) < 6.5
end program
