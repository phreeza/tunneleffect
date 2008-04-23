module m_bcs

CONTAINS

SUBROUTINE BCS(v,param,i,didparam)
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
REAL(SP),PARAMETER :: e=1.602176487E-19,k=8.617343E-5,vmax=0.003
INTEGER :: n,m,signo,l,puntos
REAL(SP) :: ia,ib
REAL(SP),DIMENSION(:) :: param
REAL(SP),DIMENSION(:) :: v
REAL(SP),DIMENSION(SIZE(v)) :: i

!do n = 1,10000
!print*,n,integrand((/ 1.0/(n*n) , 2.0 /))

!end do
!pause

print*,"los parametros en el bcs son:",param
epsilon = param(1)
temp = param(2)
c = param(3)

print*,epsilon,temp,c

!open(unit=1,file="iv_teorico.txt",status="replace",action="write",position="rewind")

i=0.0
do n=1,size(v)
!v=real(n)/puntos*vmax
   ia=1e-20
   ib=1e-4
   i(n) = i(n) + qromo(integrand,ia,ib,midinf,v(n),temp,epsilon)
   ia=1e-4
   ib=1.0
   i(n) = i(n) + qromo(integrand,ia,ib,midinf,v(n),temp,epsilon)
   ia=1.0
   ib=1e20
   i(n) = i(n) + qromo(integrand,ia,ib,midinf,v(n),temp,epsilon)   
	!write(unit=1,fmt=*) v,i
	print*,n,i(n),v(n)
end do

didparam=1.0
   
   
!close(unit=1,status="keep")

i=c/e*i
!-------------------
END SUBROUTINE BCS

end module m_bcs