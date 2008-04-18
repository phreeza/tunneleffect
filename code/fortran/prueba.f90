program prueba
use nrtype
use nr, only: mrqmin, fgauss

INTEGER :: i
REAL(SP), DIMENSION(10) :: x,y,sig
REAL(SP), DIMENSION(3) :: a
REAL(SP), DIMENSION(3,3) :: covar,alpha
REAL(SP) :: chisq
REAL(SP) :: alamda
LOGICAL(LGT), DIMENSION(3) :: maska

print*,"hahah"
alamda = -0.1
x=1
y=1
sig=0.05
covar = 1
covar(1,1) = 0
covar(2,2) = 0
covar(3,3) = 0
alpha = covar
a = 17
chisq = 100

!a=(/0,0,0/)
maska=(/.true.,.true.,.true./)
!covar=(/0,0/)

print*,"blabla"
print*,alpha

call mrqmin(x,y,sig,a,maska,covar,alpha,chisq,fgauss,alamda)
!print*,alamda
do i = 1,1000
call mrqmin(x,y,sig,a,maska,covar,alpha,chisq,fgauss,alamda)
print*,a
end do
call mrqmin(x,y,sig,a,maska,covar,alpha,chisq,fgauss,alamda)
call mrqmin(x,y,sig,a,maska,covar,alpha,chisq,fgauss,alamda)
call mrqmin(x,y,sig,a,maska,covar,alpha,chisq,fgauss,alamda)
call mrqmin(x,y,sig,a,maska,covar,alpha,chisq,fgauss,alamda)
print*,a,chisq
!------------------
end program prueba
