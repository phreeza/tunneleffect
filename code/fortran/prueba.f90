program prueba
use nrtype
use nr, only: mrqmin, fgauss

REAL(SP), DIMENSION(1000) :: x,y,sig
REAL(SP), DIMENSION(3) :: a,dyda
REAL(SP), DIMENSION(3,3) :: covar,alpha
REAL(SP) :: chisq
REAL(SP) :: alamda
LOGICAL(LGT), DIMENSION(3) :: maska
integer :: io,n,i

open(unit=1,file="gauss_data.txt",status="old",action="read",position="rewind")
   n=1
   io=0
   do
      if (io/=0) then
         exit
      end if
      read(unit=1,fmt=*,iostat=io) x(n),y(n)
      sig(n)=0.5
      n=n+1
   end do
close(unit=1,status="keep")

a=(/4,5,15/)

covar = 1
covar(1,1) = 0
covar(2,2) = 0
covar(3,3) = 0

maska=(/.true.,.true.,.true./)
covar = 2
chisq=1
alamda=-0.1
alpha=covar

print*, "Before the loop"
do i=1,1000
call mrqmin(x,y,sig,a,maska,covar,alpha,chisq,fgauss,alamda)
print*,a,chisq
end do

print*,"After. I have finished"
!------------------
end program prueba

