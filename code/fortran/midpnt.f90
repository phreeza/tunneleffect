	SUBROUTINE midpnt(func,a,b,s,n,v,temp,epsilon)
	USE nrtype; USE nrutil, ONLY : arth
	IMPLICIT NONE
	REAL(SP), INTENT(IN) :: a,b
	REAL(SP), INTENT(INOUT) :: s
	INTEGER(I4B), INTENT(IN) :: n
	INTERFACE
		FUNCTION func(x,v,temp,epsilon)
		USE nrtype
		REAL(SP), DIMENSION(:), INTENT(IN) :: x
		REAL(SP), DIMENSION(size(x)) :: func
		REAL(SP) :: v,temp,epsilon
		END FUNCTION func
	END INTERFACE
	REAL(SP) :: del
	INTEGER(I4B) :: it
	REAL(SP), DIMENSION(2*3**(n-2)) :: x
	REAL(SP) :: v,temp,epsilon
	if (n == 1) then
		s=(b-a)*sum(func( (/0.5_sp*(a+b)/),v,temp,epsilon ))
	else
		it=3**(n-2)
		del=(b-a)/(3.0_sp*it)
		x(1:2*it-1:2)=arth(a+0.5_sp*del,3.0_sp*del,it)
		x(2:2*it:2)=x(1:2*it-1:2)+2.0_sp*del
		s=s/3.0_sp+del*sum(func(x,v,temp,epsilon))
	end if
	END SUBROUTINE midpnt
