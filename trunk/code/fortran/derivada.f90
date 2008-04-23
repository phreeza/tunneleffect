program derivada
!-------------------------------------------------------------------------------------
!Este programa calcula dI/dV=G(V).
!El archivo que lee tiene los datos en dos columnas: V - I
!La derivada la calcula utilizando 2n+1 puntos equiespaciados en eje x
!-------------------------------------------------------------------------------------
INTEGER,PARAMETER:: dp=selected_real_kind(15)
REAL(KIND=dp),DIMENSION(2000):: i,v,di,dv
INTEGER:: n,io,l,m,dn,dj,fact1,fact2,fact3,orden
CHARACTER(LEN=40):: file_name, file_name_deriv 
REAL(KIND=dp):: d

call getarg(1,file_name)	! Hay que escribir en el shell: derivada 'nombre fichero' y ejecuta solito todo
print*, file_name

!--------------------------------------------
!Vamos a leer los datos del archivo, V-I
!--------------------------------------------
open(unit=1,file=file_name,status="old",action="read",position="rewind")
   n=1
   io=0		!la variable io es cero si lee algo. asi hacemos un bucle de numero de terminos desconocido
   do
      if (io/=0) then
         exit
      end if
      read(unit=1,fmt=*,iostat=io) v(n),i(n)
      n=n+1
	  print*, v(n),i(n)
   end do
close(unit=1,status="keep")

!--------------------------------------------------------
!Vamos a hacer la derivada y a escribirla en un archivo
!--------------------------------------------------------
!Formula de orden 2, pero bien hecha
!do l=1,n-1
!   di(l) = (i(l+2)+i(l+1)-i(l-1)-i(l-2)) / ((v(l+2)-v(l-2)))
!end do

!Formula de orden 5
!do l=1,n-1
!   di(l) = (-i(l+2)+8*i(l+1)-8*i(l-1)+i(l-2)) / (3*(v(l+2)-v(l-2)))
!end do

!formula de orden 2
do l=1,n-1
   di(l) = (i(l+1)-i(l-1)) / ((v(l+1)-v(l-1)))
end do

!orden=1	!Hacemos la derivada con (2*orden+1) puntos
!do l=1,n
!   di(l)=0
!   do m=-orden,orden
!	call coef_d(orden,m,d,fact1,fact2,fact3)
!	di(l) = di(l) + d*di(l+m)
!   end do
!   di(l) = (1.0/(v(l)-v(l-1)))*di(l)
!end do

open(unit=1,file="derivada.txt",status="replace",action="write",position="rewind")
        do l=1,n
                write(unit=1,fmt=*) v(l),di(l)
        end do
close(unit=1,status="keep")

print*, "Enter integers n and j:"
read*, dn,dj

call coef_d(dn,dj,d,fact1,fact2,fact3)
print*, d
print*,fact1,fact2,fact3



CONTAINS
!--------------------------------------------
!--------------------------------------------
!Subrutina de los coeficientes d_(2n+1,0,j) para el calculo de la derivada
!--------------------------------------------
subroutine coef_d(n,j,d,fact1,fact2,fact3)

INTEGER,INTENT(IN) :: n,j
REAL(KIND=dp),INTENT(OUT) :: d
INTEGER,INTENT(OUT) :: fact1,fact2,fact3
INTEGER :: k

!!!calculo de (n)!
if (n==0) then
   fact1=1
else
   fact1=1
        do k=1,n
        fact1=k*fact1
        end do
end if

!!!calculo de (n-j)!
if ((n-j)==0) then
   fact2=1
else
   fact2=1
	do k=1,n-j
	fact2=k*fact2   
	end do  
end if

!!!calculo de (n+j)!
if ((n+j)==0) then
   fact3=1
else
   fact3=1
        do k=1,n+j
        fact3=k*fact3
        end do
end if

!!!calculo del coeficiente d_(2n+1,0,j)
if (j /= 0) then
   if ( (j/2)*2 /= 0 ) then	!si j es par se cumple. Recordar que j es entero.
	d = - (fact1**2.0)/(j*fact2*fact3)
   else
	d = (fact1**2.0)/(j*fact2*fact3)
   end if
else
   d = 0
end if


end subroutine coef_d
!---------------------------------------
!-----------------------------------
end program derivada
