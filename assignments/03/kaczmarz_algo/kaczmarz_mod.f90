module kaczmarz
  implicit none
  integer, parameter :: dp =selected_real_kind(15,307)

  contains
  function dot(vec1, vec2) result(scalar)
    real(kind=dp), dimension(:), intent(in) :: vec1, vec2
    real(kind=dp) :: scalar
    scalar = sum(vec1*vec2)
  end function dot
end module kaczmarz
