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
sig=1e-7

a=(/0.00094,1.0,0.0063/)

covar = 1

maska=(/.true.,.true.,.true./)
covar = 2
chisq=1
alamda=-0.1
alpha=covar

print*, "Before the loop"
do ii=1,100
call mrqmin(v,i,sig,a,maska,covar,alpha,chisq,bcs,alamda)

print*,"iter:",ii,a,"chisq:",chisq,"alamda",alamda
!pause
end do

print*,"After. I have finished"
!------------------
end program prueba

