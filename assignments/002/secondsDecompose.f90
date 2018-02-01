program secondsDecompose

  print *, 'working'
  call testing
end program secondsDecompose

subroutine testing()
  print *, all((/1,2/) == (/1,2/))
end
