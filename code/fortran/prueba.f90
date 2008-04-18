program prueba
use nrtype
use nr, only: mrqmin, fgauss

REAL(SP), DIMENSION(10) :: x,y,sig
REAL(SP), DIMENSION(3) :: a
REAL(SP), DIMENSION(3,3) :: covar,alpha
REAL(SP) :: chisq
REAL(SP) :: alamda
LOGICAL(LGT), DIMENSION(3) :: maska

x=1
y=1
sig=0.05
a=(/0,0,0/)
maska=(/.true.,.true.,.true./)
!covar=(/0,0/)

call mrqmin(x,y,sig,a,maska,covar,alpha,chisq,fgauss,alamda)

!------------------
end program prueba
