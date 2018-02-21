program opt_funcs
  implicit none
  integer :: placeholder
  print *, 'shout'
  placeholder = shouter(4)
  placeholder = shouter_need_opto(4, 2)
  placeholder = shouter_and_opts(4)
  placeholder = shouter_and_opts(4,2)
  placeholder = shout_with_keys(4)
  placeholder = shout_with_keys(4, opto_in=2)
  contains

  function shout_with_keys(mando, opto_in) result(output)
    integer, intent(in) :: mando
    integer, optional :: opto_in
    integer :: opto
    integer :: output
    if (present(opto_in)) then
      opto = opto_in
    else
      opto = 6
    end if
    output=4
    print *, 'in shout', mando, opto
  end function shout_with_keys

  function shouter(mando) result(output)
    integer, intent(in) :: mando
    integer :: output
    output = shouter_need_opto(mando, 6)
  end function shouter

  function shouter_need_opto(mando, opto) result(output)
    integer, intent(in) :: mando, opto
    integer :: output
    output=4
    print *, 'in shout', mando, opto
  end function shouter_need_opto

  function shouter_and_opts(mando, opto) result(output)
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
  end function shouter_and_opts

end program opt_funcs
