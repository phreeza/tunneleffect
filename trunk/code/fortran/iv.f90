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
REAL(SP),PARAMETER :: e=1.602176487E-19,k=8.617343E-5,vmax=0.03
INTEGER :: n,m,signo,l,puntos
REAL(SP) :: i,ia,ib


epsilon = 0.0011336042
temp = 3.037088
c = 0.005762267


open(unit=1,file="iv_teorico.txt",status="replace",action="write",position="rewind")
puntos=1000

ia=1.0e-4
ib=vmax+2*epsilon !Porque se ve que a partir de ese valor la funcion es asintoticamente nula
do n=-puntos,puntos
v=real(n)/puntos*vmax
   ia=1e-4
   ib=vmax + 2*epsilon
   i = qromo(integrand,ia,ib,midpnt,v,temp,epsilon) &
		& + sqrt(ia*(2*epsilon+ia))*(1/(1+exp((epsilon-v)/(k*temp)))-1/(1+exp((epsilon+v)/(k*temp))))
	i = i*c
	write(unit=1,fmt=*) v,i
end do
   
close(unit=1,status="keep")

!-------------------
end program iv
