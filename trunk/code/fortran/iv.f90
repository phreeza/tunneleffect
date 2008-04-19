program iv
use nr, only: midinf,qromo
use nrtype

INTERFACE
FUNCTION integrand(x)
use nrtype

REAL(kind=8),PARAMETER :: e=1.602176487E-19,k=8.617343E-5
REAL(SP), DIMENSION(:), INTENT(IN) :: x
REAL(SP), DIMENSION(size(x)) :: integrand
END FUNCTION integrand

end interface




REAL(kind=8) :: c,epsilon,t
REAL(kind=8),PARAMETER :: e=1.602176487E-19,k=8.617343E-5
integer :: n,m,signo,l,puntos
REAL(SP) :: i,ia,ib,v

!do n = 1,10000
!print*,n,integrand((/ 1.0/(n*n) , 2.0 /))
!end do
!pause

i=0.0
epsilon = 0.002
t = 5
c = 0.01


open(unit=1,file="iv_teorico.txt",status="replace",action="write",position="rewind")
v=2.0
puntos=100

   ia=1e-4
   ib=1.
   i = i + qromo(integrand,ia,ib,midinf)
   ia=1e-20
   ib=1e-4
   i = i + qromo(integrand,ia,ib,midinf)
   ia=1.
   ib=1e20
   i = i + qromo(integrand,ia,ib,midinf)   
   print*, i
close(unit=1,status="keep")

!-------------------
end program iv
