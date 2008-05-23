program iv
use nrtype
use nr, only: midpnt,qromo

INTERFACE
FUNCTION integrand(x,v,temp,epsilon)
use nrtype
REAL(SP),PARAMETER :: e=1.602176487E-19,k=8.617343E-5
REAL(SP), DIMENSION(:), INTENT(IN) :: x
REAL(SP), DIMENSION(size(x)) :: integrand
REAL(SP) :: v,epsilon,temp,vmax
END FUNCTION integrand
END INTERFACE

REAL(SP) :: epsilon,temp,c
REAL(SP),DIMENSION(4000) :: v,i
REAL(SP),PARAMETER :: e=1.602176487E-19,k=8.617343E-5
INTEGER :: n,l,io,dim
REAL(SP) :: ia,ib
CHARACTER(LEN=50) :: file_name


!call getarg(1,file_name)	
!Hay que escribir en el shell "iv epsilofile_name"

epsilon	= 0.0014
temp	= 1.45
c		= 0.0057
!open(unit=1,file=file_name,status="old",action="read",position="rewind")
!   dim=1
!   !La variable io es cero solo si lee algo. Asi hacemos un bucle de numero de terminos desconocido
!   do
!      read(unit=1,fmt=*,iostat=io) v(dim),i(dim)
!      if (io/=0) then
!         exit
!      end if
!   dim=dim+1
!   end do
!close(unit=1,status="keep")
!dim=dim-1

vmax=0.02
dim=2001
do n=1,dim
v(n)=vmax-(n-1)*(2*vmax/dim)
end do

open(unit=1,file="iv_teorico.txt",status="replace",action="write",position="rewind")

ia=1.0e-5
ib=maxval(abs(v))+2*epsilon !Porque se ve que a partir de ese valor la funcion es asintoticamente nula
do n=1,dim
	i(n) = qromo(integrand,ia,ib,midpnt,v(n),temp,epsilon) &
		& + sqrt(ia*(2*epsilon+ia))*(1/(1+exp((epsilon-v(n))/(k*temp)))-1/(1+exp((epsilon+v(n))/(k*temp))))
	i(n) = i(n)*c
	write(unit=1,fmt=*) v(n),i(n)
end do
   
close(unit=1,status="keep")

!-------------------
end program iv
