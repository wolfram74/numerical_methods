module dumby_mod
  implicit none
  contains
  subroutine shout_routine()
    print *,'fortran is cranky'
    print *,'like, really cranky'
  end subroutine shout_routine

  function increaser(float) result(output)
    real, intent(in) :: float
    real :: output
    output = float+ 1.0
  end function increaser
end module dumby_mod
