program derivada
!-------------------------------------------------------------------------------------
!Este programa calcula dI/dV=G(V).
!El archivo que lee tiene los datos en dos columnas: V - I
!La derivada la calcula utilizando 2n+1 puntos equiespaciados en eje x
!-------------------------------------------------------------------------------------
INTEGER,PARAMETER:: dp=selected_real_kind(15)
REAL(KIND=dp),DIMENSION(2000):: i,v,di,dv
INTEGER:: n,io,l
CHARACTER(LEN=40):: file_name, file_name_deriv 
REAL(KIND=dp):: d

call getarg(1,file_name)		!getarg(pos,name) Sets name to the pos-th command-line argument 
call getarg(2,file_name_deriv)	

! Hay que escribir en el shell: derivada 'nombre fichero input' 'nombre fichero output' y ejecuta solito todo

!--------------------------------------------
!Vamos a leer los datos del archivo, V-I
!--------------------------------------------
open(unit=1,file=file_name,status="old",action="read",position="rewind")
   n=1
   io=0		!La variable io es cero si lee algo. asi hacemos un bucle de numero de terminos desconocido
   do
      if (io/=0) then
         exit
      end if
      read(unit=1,fmt=*,iostat=io) v(n),i(n)
      n=n+1
   end do
close(unit=1,status="keep")

!formula de orden 2. No utilizo mas orden porque tengo un PASO VARIABLEEEEEE
do l=1,n-1
   di(l) = (i(l+1)-i(l-1)) / ((v(l+1)-v(l-1)))
end do

open(unit=1,file=file_name_deriv,status="replace",action="write",position="rewind")
        do l=1,n
                write(unit=1,fmt=*) v(l),di(l)
        end do
close(unit=1,status="keep")

!-------------------------------------------
end program derivada
