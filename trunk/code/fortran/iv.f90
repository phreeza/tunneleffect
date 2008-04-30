program iv
use nrtype
use nr, only: midpnt,qromo

INTERFACE
FUNCTION integrand(x,v,temp,epsilon)
use nrtype
REAL(SP),PARAMETER :: e=1.602176487E-19,k=8.617343E-5
REAL(SP), DIMENSION(:), INTENT(IN) :: x
REAL(SP), DIMENSION(size(x)) :: integrand
REAL(SP) :: v,epsilon,temp
END FUNCTION integrand
END INTERFACE

REAL(SP) :: c,v,epsilon,temp
REAL(SP),PARAMETER :: e=1.602176487E-19,k=8.617343E-5,vmax=0.1
INTEGER :: n,m,signo,l,puntos
REAL(SP) :: i,ia,ib


epsilon = 0.002
temp = 5.0
c = 0.01


open(unit=1,file="iv_teorico.txt",status="replace",action="write",position="rewind")
puntos=200

do n=-puntos,puntos
v=real(n)/puntos*vmax
i=0.0
   ia=1e-4
   ib=vmax + 2*epsilon
   i = i + qromo(integrand,ia,ib,midpnt,v,temp,epsilon) &
		& + sqrt(ia*(2*epsilon+ia))*(1/(1+exp((epsilon-v)/(k*temp)))-1/(1+exp((epsilon+v)/(k*temp))))
	print*,n,i,v
	write(unit=1,fmt=*) v,i
end do
   
i = i*c
close(unit=1,status="keep")

!-------------------
end program iv
