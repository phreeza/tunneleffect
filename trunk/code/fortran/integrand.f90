FUNCTION integrand(x,v,temp,epsilon)
use nrtype

REAL(SP),PARAMETER :: e=1.602176487E-19,k=8.617343E-5
REAL(SP), DIMENSION(:), INTENT(IN) :: x
REAL(SP), DIMENSION(size(x)) :: integrand
REAL(SP) :: v,temp,epsilon
INTEGER :: n

do n = 1,size(x)
!integrand(n) = (x(n)+epsilon)/sqrt((x(n)*(x(n)+2*epsilon))) * &
!				& ( (exp(v)*(1.0-exp(-2*v))) / (exp(2.0)+exp(v)+exp(-v)+exp(-(x(n)+epsilon)/(k*temp))) )
integrand(n) = (x(n)+epsilon)/((x(n)*(x(n)+2*epsilon))**(0.5)) * ( fermi(x(n)+epsilon-v) - fermi(x(n)+epsilon+v) )
end do


CONTAINS
FUNCTION fermi(y)
REAL(SP),INTENT(IN) :: y
REAL(SP) :: fermi
	fermi = 1 / ( 1 + exp(y/(k*temp)) )
END FUNCTION fermi
END FUNCTION integrand


