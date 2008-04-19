FUNCTION integrand(x)
use nrtype

REAL(SP),PARAMETER :: e=1.602176487E-19,k=8.617343E-5
REAL(SP), DIMENSION(:), INTENT(IN) :: x
REAL(SP), DIMENSION(size(x)) :: integrand
integer :: n
!integrand = (x+epsilon)/((x*(x+2*epsilon))**(0.5)) * ( fermi(x+epsilon-e*v) - fermi(x+epsilon+e*v) )  

REAL(SP) :: c,epsilon,t
REAL(SP) :: i,ia,ib,v
epsilon = 0.002
t = 5
c = 0.01
v=-.001


do n = 1,size(x)
integrand(n) = (x(n)+epsilon)/((x(n)*(x(n)+2*epsilon))**(0.5)) * ( fermi(x(n)+epsilon-v) - fermi(x(n)+epsilon+v) )
!print*,x(n),integrand(n)
end do

CONTAINS
FUNCTION fermi(y)
REAL(SP),INTENT(IN) :: y
REAL(SP) :: fermi
	fermi = 1 / ( 1 + exp(y/(k*t)) )
END FUNCTION fermi

END FUNCTION integrand


