module m_bcs

CONTAINS

SUBROUTINE BCS(v,param,int,didparam)
use nrtype
use nr, only: midinf,qromo

INTERFACE
	FUNCTION integrand(x,v,temp,epsilon)
	use nrtype
	REAL(SP),PARAMETER :: e=1.602176487E-19,k=8.617343E-5 !Units of k --> eV/K
	REAL(SP), DIMENSION(:), INTENT(IN) :: x
	REAL(SP), DIMENSION(size(x)) :: integrand
	REAL(SP) :: v,epsilon,temp
	END FUNCTION integrand
END INTERFACE

REAL(SP), DIMENSION(:,:), INTENT(OUT) :: didparam
REAL(SP) :: c,epsilon,temp
REAL(SP),PARAMETER :: e=1.602176487E-19,k=8.617343E-5,vmax=0.003,hEpsilon=0.000001,hTemp=0.00001
INTEGER :: n,m,signo,l,puntos
REAL(SP) :: ia,ib
REAL(SP),DIMENSION(:) :: param
REAL(SP),DIMENSION(:) :: v
REAL(SP),DIMENSION(:),intent(out) :: int


epsilon = param(1)	!Gap
temp = param(2)		!Temperature
c = param(3)		!Normal-Normal Conductance

print*,"Tentative parameters: epsilon, temperature and normal-normal conductance:", param

ia=k*temp
ib=1.0
!ib=maxval(abs(v))+2*epsilon !Porque se ve que a partir de ese valor la funcion es asintoticamente nula
do n=1,size(v)
   int(n) = qromo(integrand,ia,ib,midinf,v(n),temp,epsilon)
   
   !Ahora le sumo la parte que falta, de 0 hasta ia, que lo saco analiticamente...
   !int(n) = int(n) + sqrt(ia*(2*epsilon+ia))*(1/(1+exp((epsilon-v(n))/(k*temp)))-1/(1+exp((epsilon+v(n))/(k*temp))))
   
   didparam(n,1) =  - (int(n) -  qromo(integrand,ia,ib,midinf,v(n),temp,epsilon+hEpsilon))/hEpsilon
   !didparam(n,1) = didparam(n,1)+sqrt(ia*(2*epsilon+hEpsilon+ia))*(1/(1+exp((epsilon+hEpsilon-v(n))/(k*temp))) &
   !& -1/(1+exp((epsilon+hEpsilon+v(n))/(k*temp))))/hEpsilon
   
   didparam(n,2) = - (int(n) -  qromo(integrand,ia,ib,midinf,v(n),temp+hTemp,epsilon))/hTemp 
   !didparam(n,2) = didparam(n,2) + sqrt(ia*(2*epsilon+ia))*(1/(1+exp((epsilon-v(n))/(k*(temp+hTemp)))) &
   !&-1/(1+exp((epsilon+v(n))/(k*(temp+hTemp)))))/hTemp
   
   didparam(n,3) = 0.0
end do

didparam = c*didparam
didparam(:,3) = int
int=c*int

open(unit=1,file="iv_teorico.txt",status="replace",action="write",position="rewind")
	do n=1,size(v)
	   write(unit=1,fmt=*) v(n),int(n)
	end	do
close(unit=1,status="keep")

!--------------------------------------------
END SUBROUTINE BCS
!--------------------------------------------
!--------------------------------------------
end module m_bcs