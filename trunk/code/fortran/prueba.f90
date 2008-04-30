program prueba
use nrtype
use nr, only: mrqmin
use m_bcs

REAL(SP), DIMENSION(4000) :: veff,ieff
REAL(SP), DIMENSION(:),ALLOCATABLE :: i,v,sigi,sigv
REAL(SP), DIMENSION(3) :: a
REAL(SP), DIMENSION(3,3) :: covar,alpha
REAL(SP) :: chisq
REAL(SP) :: alamda
LOGICAL(LGT), DIMENSION(3) :: maska
INTEGER :: io,n,ii
CHARACTER(LEN=40) :: file_name

call getarg(1,file_name)
!getarg(pos,name) Sets name to the pos-th command-line argument 	
!Hay que escribir en el shell: prueba 'nombre fichero input' y ejecuta solito todo
!WE READ THE EXPERIMENTAL DATA FILE WITH UNKNOWN NUMBER OF ENTRIES

open(unit=1,file=file_name,status="old",action="read",position="rewind")
   n=1
   io=0
   do
      if (io/=0) then
         exit
      end if
      read(unit=1,fmt=*,iostat=io) veff(n),ieff(n)
      !print*,veff(n),ieff(n)
	  n=n+1
   end do
close(unit=1,status="keep")

allocate(i(n-2),v(n-2),sigv(n-2),sigi(n-2))

!Ahora escribimos un archivo llamado iv_experimental, para poder manejarlo bien
open(unit=1,file="iv_experimental.txt",status="old",action="write",position="rewind")
do ii=1, n-2
v(ii)=veff(ii)
i(ii)=ieff(ii)
write(unit=1,fmt=*) v(ii),i(ii)
end do
close(unit=1,status="keep")

sigi=1.0e-7 !Este es el error de las intensidades, debido a la inestabilidad de las medidas
sigv=0.0001*v	!Este es el error del potencial, que depende de cada valor. Dato del aparato.

a=(/0.00138,1.3508,0.005958/) !INITIAL PARAMETER VALUES FOR THE FIT

maska=(/.true.,.true.,.true./)	!The program fits the parameter with its covar value=true

alamda=-0.1	!The initial step of Levenberg-Marquardt needs two iterations and this initial value of the alamda
			!The fitting uses the gradient, but with the factor alambda


do ii=1,50	!ITERATIONS
	call mrqmin(v,i,sigv,sigi,a,maska,covar,alpha,chisq,bcs,alamda)
	print*,"ITERATION:",ii,"PARAMETERS: EPSILON, TEMPERATURE AND NORMAL-NORMAL CONDUCTANCE=",a(1),a(2),a(3),&
			& "CHI SQUARE=",chisq,"ALAMDA=",alamda
	if(alamda > 1.0e7) exit
end do

!Para obtener los errores de los parametros, hay que llamar a mrqmin con alamda=0
!Los errores seran los elementos de la diagonal de covar, que es la matriz de covarianza
alamda=0.0
call mrqmin(v,i,sigv,sigi,a,maska,covar,alpha,chisq,bcs,alamda)
print*,"Epsilon, temperature and n-n conductance errors:",covar(1,1),covar(2,2),covar(3,3)
print*,"After. I have finished"
!------------------
end program prueba

