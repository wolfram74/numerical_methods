module neville
  implicit none
  integer, parameter :: dp =selected_real_kind(15,307)

  contains
  function neville_algo(x_vals, y_vals, query) result(guess_error)
    real(kind=dp), intent(in) :: x_vals(:), y_vals(:), query
    real(kind=dp) :: guess_error(2)
    real(kind=dp), allocatable :: interpolations(:, :)
    integer :: layer, row, point_count, upper_row

    allocate(interpolations(size(x_vals), size(x_vals)))
    guess_error = 0
    interpolations = 0
    interpolations(1, :) = y_vals
    point_count = size(x_vals)
    do layer=2, point_count
      do row=1, (point_count-layer+1)
        upper_row = (row+layer-1)
        print *, layer, row, upper_row
        ! print *, interpolations(layer-1, row)
        ! print *, interpolations(row, layer-1)
        ! print *, (x_vals(row)-x_vals(upper_row))
        ! print *, (query-x_vals(upper_row))
        ! print *, (query-x_vals(upper_row))*interpolations(row, layer-1)
        ! print *, (query-x_vals(row))*interpolations(upper_row, layer-1)
        ! print *, (query-x_vals(upper_row))*interpolations(row, layer-1)+(query-x_vals(row))*interpolations(upper_row, layer-1)
        interpolations(layer, row) = (&
           (query-x_vals(upper_row))*interpolations(layer-1, row)&
          -(query-x_vals(row))*interpolations(layer-1, row+1)&
          )/(x_vals(row)-x_vals(upper_row))
        ! interpolations(layer, row) = row+layer*point_count
      end do
    end do
    print *, interpolations
    guess_error(1) = interpolations(point_count,1)
    guess_error(2) = (&
      interpolations(point_count-1,1)&
      -interpolations(point_count-1,2)&
      )

  end function neville_algo
end module neville
