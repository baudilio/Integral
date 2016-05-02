!> module definition... Emacs VC operation and control
MODULE MyMod
  IMPLICIT NONE

  Integer, Parameter :: WP = KIND(1.0D+00) !> Working precision for real variables.
  Integer  :: N
  Real(wp) :: Dx

CONTAINS

  REAL(KIND=WP) FUNCTION Integrate(A)
    IMPLICIT NONE
    Real(WP), Dimension(:), Pointer :: A
    REAL(WP) :: S1, S2

    S1 = Sum( A(2:N-1) )
    S2 = A(1) + A(N)
    Integrate = dx * ( S1 + S2 / 2.0 )

  END FUNCTION Integrate

end MODULE MyMod

!> Program main. Adding a new comment to alter the source.
!> Changing further.
PROGRAM MAIN
  USE MyMod
  implicit none

  Real(wp) :: xi, xf                              ! Integration limits
  REAL(WP), Dimension(:), Pointer :: A => NULL()  ! Array that will contain the data.


  ! ---
  xi= -1.0_wp
  xf=  1.0_wp

  Print '(A$)', "Enter N: "
  Read *, N

  N=4  ! For testing purposes.

  Dx = (xf-xi)/(n-1) ! 
  Allocate(A(N))  ! Allocate space for the data array.

! Load data into the array:
  A = [1., -1., -1., 1.]

! Call the integrate function, return the value of the integral and print it
  Print '(//"Integrate Func: ", F12.5)', Integrate(A)

! Free memory used by array A. (Not necessary here, but it's a good programming practice.)
  Deallocate(A)

  Print '(T12A)', "-- the End --"
  call exit(0)
END PROGRAM MAIN
