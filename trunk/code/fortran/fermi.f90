program fermii
use nrtype

REAL(SP) :: c,x,epsilon,temp
REAL(SP),PARAMETER :: e=1.602176487E-19,k=8.617343E-5,vmax=0.02
INTEGER :: n,m,signo,l,puntos

epsilon = 0.002
temp = 1.0
v=0.01

open(unit=1,file="fermi.txt",status="replace",action="write",position="rewind")

puntos=500
do n=-puntos,puntos
x=real(n)/puntos*vmax
	write(unit=1,fmt=*) x,fermi(x+epsilon-v) - fermi(x+epsilon+v)
end do

close(unit=1,status="keep")

CONTAINS
FUNCTION fermi(y)
REAL(SP),INTENT(IN) :: y
REAL(SP) :: fermi
	fermi = 1 / ( 1 + exp(y/(k*temp)) )
END FUNCTION fermi

!-------------------
end program fermii
