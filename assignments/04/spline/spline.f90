PROGRAM spline
 
 IMPLICIT NONE

 INTEGER            :: i, k, loc
 INTEGER, PARAMETER :: n = 21    ! # of support points in data file
 REAL, DIMENSION(n) :: x, y      ! x and y values of support points
 REAL, DIMENSION(n) :: dy_2      ! 2nd derivative
 REAL, DIMENSION(2) :: dy        ! 1st derivative at endpoints
 REAL               :: x0, y0    ! x and y values of interpolated points
 LOGICAL            :: natural   ! choose between natural or clamped spline

 INTERFACE
  SUBROUTINE GET_2ND_DERIVATIVE(X,Y,DY_2,DY,N,NATURAL)
    IMPLICIT NONE
    INTEGER, INTENT(IN)             :: n         ! # of support points in data file
    REAL, INTENT(IN),  DIMENSION(n) :: x, y      ! x and y values of support points
    REAL, INTENT(OUT), DIMENSION(n) :: dy_2      ! 2nd derivative
    REAL, INTENT(IN),  DIMENSION(2) :: dy        ! 1st derivative at endpoints
    LOGICAL, INTENT(IN)             :: natural   ! choose between natural or clamped spline
  END SUBROUTINE GET_2ND_DERIVATIVE

  SUBROUTINE CUBIC_SPLINE(x,y,dy_2,x0,y0)
    IMPLICIT NONE
    REAL, INTENT(IN),  DIMENSION(2) :: x, y      ! x and y values of support points
    REAL, INTENT(IN),  DIMENSION(2) :: dy_2      ! 2nd derivative
    REAL, INTENT(IN)                :: x0        ! x value of interpolated point
    REAL, INTENT(OUT)               :: Y0        ! Y value of interpolated point
  END SUBROUTINE CUBIC_SPLINE

  SUBROUTINE BISECT_SEARCH(x,x0,loc,n)
    IMPLICIT NONE
    INTEGER, INTENT(IN)             :: n         ! # of support points in data file
    REAL, INTENT(IN),  DIMENSION(n) :: x         ! x values of support points
    REAL, INTENT(IN)                :: x0        ! x value of interpolated point
    INTEGER, INTENT(IN OUT)         :: loc
  END SUBROUTINE BISECT_SEARCH
 END INTERFACE

 OPEN(unit=10,file="SPLINE_DATA.DAT",status="old")
 OPEN(unit=20,file="OUTPUT.DAT",status="unknown")
 DO i=1,n
   READ(10,*) x(i), y(i)
 END DO

 natural = .TRUE.
 IF( .NOT. NATURAL) THEN   !Define the 1st Derivative at End Points
   dy(1) = .75
   dy(2) = -.75
 END IF

!Calcualte the second derivatives needed for the 
!calculation of the splines
 CALL GET_2ND_DERIVATIVE(x,y,dy_2,dy,n,natural)

!Calculate the interpolated values in 1000 steps form the 
!first to the last x postition
 DO i=1,1000,1
   x0 = x(1) + (x(n)-x(1))/1000.*(i-1)
   CALL bisect_search(x,x0,loc,n)
   k = MIN(MAX(loc-1/2,1),n-1) 
   CALL CUBIC_SPLINE(x(k),y(k),dy_2(k),x0,y0)
   WRITE(20,*) x0, y0
 END DO

 CLOSE(10) 
 CLOSE(20)

STOP
END PROGRAM spline
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE GET_2ND_DERIVATIVE(X,Y,DY_2,DY,N,NATURAL)
!
! Calculates the array of second derivatives, dy_2, needed for evaluating the
! cubic spline formula, g(x) = A y(i) + B y(i+1) + C dy_2(i) + D dy_2(i+1), given
! length-n arrays of support points, x and y.  For natural spline (natural = .true.), 
! dy_2(1) = 0 and dy_2(n) = 0 with the tridiagonal solver called using the 
! reduced (n-2) x (n-2)
! system.  For clamped spline (natural = .false.), the user must specify the first
! derivatives at x(1) and x(n), namely, dy(1) and dy(2), respectively, which are
! used in the first and nth equations derived by calculating g'(1) and g'(n).
!
   IMPLICIT NONE
   INTEGER, INTENT(IN)               :: n
   REAL, DIMENSION(n), INTENT(IN)    :: x, y
   REAL, DIMENSION(n), INTENT(OUT)   :: dy_2
   REAL, DIMENSION(2), INTENT(IN)    :: dy
   LOGICAL, INTENT(IN)               :: natural
   REAL, DIMENSION(n)                :: a, b, c, rhs
   integer                           :: i


!  Initialize
   dy_2=0.
   
!  You need to calulate your values (vectors) a, b, c, and the right-hand side
!  that are needed for the calculation of the second derivative.
!!!!
! Some more work needs to be done here for the case of a clamped spline!!!
  do i=2,n-1
    a(i)=x(i)-x(i-1)
    b(i)=2.*(x(i+1)-x(i-1))
    c(i)=x(i+1)-x(i)
    rhs(i)=6.*( (y(i+1)-y(i))/(x(i+1)-x(i)) - (y(i)-y(i-1))/(x(i)-x(i-1)) )
  end do

!! YOUR WORK GOES IN HERE!!!!!
!! CALL YOUR SOLVER FOR THE 2nd DERIVATIVE
!  call thomas(...)
 
 RETURN
END SUBROUTINE GET_2ND_DERIVATIVE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE CUBIC_SPLINE(x,y,dy_2,x0,y0)
!
! Evaluates the cubic spline formula, g(x) = A y(1) + B y(2) + C dy_2(1) + D dy_2(2).  
! Note a call to cubic_spline is first necessary to set of the array of second derivatives, 
! dy_2.  Furthermore a search must be performed to locate the integer i such that x(i) < x0 < x(i+1).
!
   IMPLICIT NONE
   REAL, DIMENSION(2), INTENT(IN) :: x, y, dy_2
   REAL, INTENT(IN)               :: x0
   REAL, INTENT(OUT)              :: y0
   REAL                           :: A, B, del_x
 
    del_x = x(2) - x(1)
    A = (x(2) - x0)/del_x
    B = (x0 - x(1))/del_x
    y0 = A*y(1) + B*y(2) + (del_x**2/6.)*((A**3-A)*dy_2(1) + (B**3-B)*dy_2(2))
 
 RETURN
END SUBROUTINE CUBIC_SPLINE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE bisect_search(x_arr,x0,loc,n)
!
! This routine performs a bisection search on monotonically increasing data
! returning the integer loc such that x(loc) < x0 < x(loc+1).  If x0 is
! out of bounds, loc = 0 or loc = n+1.
!
 IMPLICIT NONE
 
 INTEGER                 :: il, iu, im
 INTEGER, INTENT(in)     :: n
 INTEGER, INTENT(IN OUT) :: loc
 REAL, INTENT(IN)        :: x0, x_arr(n)
 
! Perform bisection search on monotonically increasing data
  il = 1
  iu = n
  DO WHILE(iu-il.gt.1)
     im = (iu+il)/2
     IF(x0.lt.x_arr(im)) THEN
      iu = im
     ELSE
      il = im
     END IF
  END DO
  loc = il
 
! If x0 is out of bounds return 0 or n+1 or n if x0 = x_arr(n).
  IF(x0.lt.x_arr(1)) THEN
     loc = 0
  ELSE IF(x0.gt.x_arr(n)) THEN
     loc = n+1
  ELSE IF(x0.eq.x_arr(n)) THEN
     loc = n
  END IF
 
 RETURN
END SUBROUTINE bisect_search                    
