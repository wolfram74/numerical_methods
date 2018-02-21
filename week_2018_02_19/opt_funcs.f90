program opt_funcs
  implicit none
  integer :: placeholder
  print *, 'shout'
  placeholder = shouter(4)
  placeholder = shouter(4, 2)
  contains
  function shouter(mando, opto) result(output)
    integer, intent(in) :: mando
    integer, optional :: opto
    integer :: opto_used
    integer :: output
    if (present(opto)) then
      opto_used = opto
    else
      opto_used = 6
    end if
    output=4
    print *, 'in shout', mando, opto_used
  end function shouter
end program opt_funcs
