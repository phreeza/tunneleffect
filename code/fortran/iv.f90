program iv
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

REAL(SP) :: c,v,epsilon,temp
REAL(SP),PARAMETER :: e=1.602176487E-19,k=8.617343E-5,vmax=0.003
INTEGER :: n,m,signo,l,puntos
REAL(SP) :: i,ia,ib


!do n = 1,10000
!print*,n,integrand((/ 1.0/(n*n) , 2.0 /))
!end do
!pause


epsilon = 0.0020001
temp = 0.5
c = 0.01


open(unit=1,file="iv_teorico.txt",status="replace",action="write",position="rewind")

puntos=1000

do n=-puntos,puntos
v=real(n)/puntos*vmax
i=0.0
   print*,n,i,v,vmax
   ia=1e-20
   ib=1e-4
   i = i + qromo(integrand,ia,ib,midinf,v,temp,epsilon)
   ia=1e-4
   ib=1.0
   i = i + qromo(integrand,ia,ib,midinf,v,temp,epsilon)
   ia=1.0
   ib=1e20
   i = i + qromo(integrand,ia,ib,midinf,v,temp,epsilon)   
	write(unit=1,fmt=*) v,i
	print*,n,i,v,vmax
end do
   
close(unit=1,status="keep")

!-------------------
end program iv
