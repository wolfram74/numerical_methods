program concat
  implicit none
  character(len=120) :: str1, str2, combo
  str1 = 'fart'
  str2 = 'noises'
  print *, 'trying'
  print *, trim(str1)
  print *, trim(str2)
  print *, trim(str1) // trim(str2)
  combo = trim(str1) // trim(str2)

  print *, 'combo'
  print *, combo

end program concat
