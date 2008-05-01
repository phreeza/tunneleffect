program derivada
!-------------------------------------------------------------------------------------
! Este programa calcula dI/dV=G(V).
! El archivo que lee tiene los datos en dos columnas: V - I
! La derivada la calcula utilizando la formula a orden 2. No se utiliza mas orden porque los puntos no estan
! equiespaciados y las formulas son horribles... tengo un paso valiableeeee 
!-------------------------------------------------------------------------------------
INTEGER,PARAMETER:: DP=SELECTED_REAL_KIND(15)
REAL(KIND=DP),DIMENSION(4000):: v,i,di
INTEGER:: n,io,l
CHARACTER(LEN=40):: file_name, file_name_deriv 

call getarg(1,file_name)		!getarg(pos,name) Sets name to the pos-th command-line argument 
call getarg(2,file_name_deriv)	

! Hay que escribir en el shell: derivada 'nombre fichero input' 'nombre fichero output' y ejecuta solito todo

!--------------------------------------------
!Vamos a leer los datos del archivo, V-I
!--------------------------------------------
open(unit=1,file=file_name,status="old",action="read",position="rewind")
   n=1
   !La variable io es cero solo si lee algo. Asi hacemos un bucle de numero de terminos desconocido
   do
      read(unit=1,fmt=*,iostat=io) v(n),i(n)
      if (io/=0) then
         exit
      end if
	  n=n+1
   end do
close(unit=1,status="keep")

n=n-1	! Porque al salir del bucle la variable de conteo se incrementa en uno

di(1) = (i(2)-i(1)) / ((v(2)-v(1)))	! Utilizo para el primero la formula forward
do l=2,n-1	
	di(l) = (i(l+1)-i(l-1)) / ((v(l+1)-v(l-1)))
		if (v(l+1)-v(l-1)==0.0 .or. di(l)<0.0) then
		di(l) = di(l-1)
		end if
end do
di(n) = (i(n)-i(n-1)) / ((v(n)-v(n-1)))	! Utilizo para el ultimo la formula backward

open(unit=1,file=file_name_deriv,status="replace",action="write",position="rewind")
        do l=1,n
                write(unit=1,fmt=*) v(l),di(l)
        end do
close(unit=1,status="keep")

!-------------------------------------------
end program derivada
