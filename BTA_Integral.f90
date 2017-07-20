!------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!------------------------------------------------------------------------------
!
! MODULE: Module Name
!
!> @author
!> Module Author Name and Affiliation
!
! DESCRIPTION:
!> Brief description of module.
!
! REVISION HISTORY:
! DD Mmm YYYY - Initial Version
! TODO_dd_mmm_yyyy - TODO_describe_appropriate_changes - TODO_name
!------------------------------------------------------------------------------

MODULE MyMod
  IMPLICIT NONE

  Integer, Parameter :: WP = KIND(1.0D+00) !> Working precision for real variables.
  Integer  :: N  !> Number of integration intervals.
  Real(wp) :: Dx

CONTAINS

  !---------------------------------------------------------------------------
  !> @author
  !> Baudilio Tejerina - Home.
  !
  ! DESCRIPTION:
  !> Compute the integral of the function A = f(x) by the trapecium quadrature.
  !> @brief
  !> Flow method (rate of change of position) used by integrator.
  !> Compute \f$ \frac{d\lambda}{dt} , \frac{d\phi}{dt},  \frac{dz}{dt} \f$
  !
  ! REVISION HISTORY:
  ! TODO_dd_mmm_yyyy - TODO_describe_appropriate_changes - TODO_name
  !
  !> @param[in] inParam
  !> @param[out] outParam
  !> @return returnValue
  !---------------------------------------------------------------------------
  !
  REAL(KIND=WP) FUNCTION Integrate(A)
    IMPLICIT NONE
    Real(WP), Dimension(:), Pointer :: A
    REAL(WP) :: S1, S2 !> @var Var description

    S1 = Sum( A(2:N-1) )
    S2 = A(1) + A(N)
    Integrate = dx * ( S1 + S2 / 2.0 )

  END FUNCTION Integrate

end MODULE MyMod

!> Main program. Adding a new comment to alter the source.
!> Changing further.
PROGRAM MAIN
  USE MyMod
  implicit none

  Real(wp) :: xi, xf                              ! Integration limits
  REAL(WP), Dimension(:), Pointer :: A => NULL()  ! Array that will contain the data.


  ! ---
  xi= -1.0_wp
  xf=  1.0_wp

  Print '(A$)', "Enter the number of integration intervals: "
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
  STOP "All Done"
! CALL EXIT(0)
END PROGRAM MAIN
