program iv
use nr, only: bessk1, qromb
use nrtype
external integrand

REAL(kind=8) :: c,epsilon,t,v
REAL(kind=8),PARAMETER :: e=1.602176487E-19,k=8.617343E-5
integer :: n,m,signo,l,puntos
REAL(SP) :: i,ia,ib

!epsilon = 0.002
t = 5
!c = 0.01
ia=1
ib=2

open(unit=1,file="iv_teorico.txt",status="replace",action="write",position="rewind")
v=2
puntos=1
do n=-puntos,puntos
   print*,integrand(1)
   call trapzd(integrand,ia,ib,i,100)
   print*, i
end do
close(unit=1,status="keep")

!-------------------
end program iv
