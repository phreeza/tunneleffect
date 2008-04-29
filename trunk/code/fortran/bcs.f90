module m_bcs

CONTAINS

SUBROUTINE BCS(v,param,int,didparam)
use nrtype
use nr, only: midinf,qromo

INTERFACE
FUNCTION integrand(x,v,temp,epsilon)
use nrtype
REAL(SP),PARAMETER :: e=1.602176487E-19,k=8.617343E-5
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
!do n = 1,10000
!print*,n,integrand((/ 1.0/(n*n) , 2.0 /))

!end do
!pause

!print*,"los parametros en el bcs son:",param
epsilon = param(1)
temp = param(2)
c = param(3)

!print*,epsilon,temp,c

!open(unit=1,file="iv_teorico.txt",status="replace",action="write",position="rewind")

int=0.0

print*,"param",param
do n=1,size(v)
!v=real(n)/puntos*vmax
   !print*,k*temp-epsilon+v(n),k*temp-epsilon-v(n)
   ia=1e-6
   ib=maxval(k*temp-epsilon+v)*10
   int(n) = qromo(integrand,ia,ib,midinf,v(n),temp,epsilon)
   int(n) = int(n) + sqrt(ia*(2*epsilon+ia))*(1/(1+exp((epsilon-v(n))/(k*temp)))-1/(1+exp((epsilon+v(n))/(k*temp))))
   !print*,sqrt(ia*(2*epsilon+ia))*(1/(1+exp((epsilon-v(n))/(k*temp)))-1/(1+exp((epsilon+v(n))/(k*temp))))/int(n)
   didparam(n,1) =  - (int(n) -  qromo(integrand,ia,ib,midinf,v(n),temp,epsilon+hEpsilon))/hEpsilon
   didparam(n,1) = didparam(n,1)+sqrt(ia*(2*epsilon+hEpsilon+ia))*(1/(1+exp((epsilon+hEpsilon-v(n))/(k*temp))) &
   & -1/(1+exp((epsilon+hEpsilon+v(n))/(k*temp))))/hEpsilon
   didparam(n,2) = - (int(n) -  qromo(integrand,ia,ib,midinf,v(n),temp+hTemp,epsilon))/hTemp 
   didparam(n,2) = didparam(n,2) + sqrt(ia*(2*epsilon+ia))*(1/(1+exp((epsilon-v(n))/(k*(temp+hTemp)))) &
   &-1/(1+exp((epsilon+v(n))/(k*(temp+hTemp)))))/hTemp
   didparam(n,3) = 0.0
   !ia=1e-4
   !ib=1.0
   !int(n) = int(n) + qromo(integrand,ia,ib,midinf,v(n),temp,epsilon)
   !ia=1.0
   !ib=1e20
   !int(n) = int(n) + qromo(integrand,ia,ib,midinf,v(n),temp,epsilon)   
	!write(unit=1,fmt=*) v,i
	!print*,n,int(n),v(n)
end do




!didparam=1.0
   
   
!close(unit=1,status="keep")
didparam = c*didparam
didparam(:,3) = int
int=c*int
!print*,int
!print*,"didparam",didparam
!print*,"size(int) en bcs",size(int),int

!-------------------
END SUBROUTINE BCS

end module m_bcs