MODULE MyMod
  Implicit none
  Integer, Parameter :: WP = KIND(1.0D+00)
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

PROGRAM MAIN
  USE MyMod
  implicit none

  Real(wp) :: xi,xf
  Real(wp) :: S1, S2
  REAL(WP), Dimension(:), Pointer :: A

  ! ---
  xi=-1.0_wp
  xf= 1.0_wp

  Print '(A$)', "Enter N: "
  Read *, N

  N=4
  Dx = (xf-xi)/(n-1)
  Allocate(A(N))  

  A = [1., -1., -1., 1.]


  Print '(//"Integrate Func: ", F12.5)', Integrate(A)


  Print '(T12A)', "-- the End --"
  call exit(0)
end Program main
