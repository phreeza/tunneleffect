program prueba
use nrtype
use nr, only: mrqmin
use m_bcs

REAL(SP), DIMENSION(4000) :: veff,ieff
REAL(SP), DIMENSION(:),ALLOCATABLE :: i,v,sig
REAL(SP), DIMENSION(3) :: a
REAL(SP), DIMENSION(3,3) :: covar,alpha
REAL(SP) :: chisq
REAL(SP) :: alamda
LOGICAL(LGT), DIMENSION(3) :: maska
integer :: io,n,ii

!WE READ THE EXPERIMENTAL DATA FILE WITH UNKNOWN NUMBER OF ENTRIES
open(unit=1,file="iv_experimental.txt",status="old",action="read",position="rewind")
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

allocate(i(n-2),v(n-2),sig(n-2))

do ii=1, n-2
v(ii)=veff(ii)
i(ii)=ieff(ii)
end do

sig=5e-8 !Este es el error de las intensidades, debido a la inestabilidad de las medidas

a=(/0.0014,1.0,0.0063/) !INITIAL PARAMETER VALUES FOR THE FIT

maska=(/.true.,.false.,.true./)	!The program fits the parameter with its covar value=true

alamda=-0.1	!The initial step of Levenberg-Marquardt needs two iterations and this initial value of the alamda
			!The fitting uses the gradient, but with the factor alambda


do ii=1,100	!ITERATIONS
	call mrqmin(v,i,sig,a,maska,covar,alpha,chisq,bcs,alamda)
	print*,"Iteration:",ii,"Parameters; Epsilon:",a(1),"Temperature:",a(2),"Normal-Normal Conductance:",a(3)
	print*,"Iteration:",ii,"Chi square:",chisq,"Alamda:",alamda
end do


print*,"After. I have finished"
!------------------
end program prueba

