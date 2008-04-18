FUNCTION integrand(x)
use nrtype

REAL(kind=8),PARAMETER :: e=1.602176487E-19,k=8.617343E-5
REAL(SP),INTENT(IN) :: x
REAL(KIND=8) :: integrand

!integrand = (x+epsilon)/((x*(x+2*epsilon))**(0.5)) * ( fermi(x+epsilon-e*v) - fermi(x+epsilon+e*v) )  
integrand = 17*v

!CONTAINS
!FUNCTION fermi(y)
!REAL(KIND=8),INTENT(IN) :: Y
!REAL(KIND=8) :: fermi
!	fermi = 1 / ( 1 + exp(y/(k*t)) )
!END FUNCTION fermi

END FUNCTION integrand


