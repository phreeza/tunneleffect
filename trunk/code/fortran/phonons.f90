program phonons

INTEGER,PARAMETER:: DP=SELECTED_REAL_KIND(15)
REAL(KIND=DP),DIMENSION(4000):: v,g_theor,g_exper
INTEGER:: n,io,l
CHARACTER(LEN=60):: file_gv_theor, file_gv_exper

call getarg(1,file_gv_theor)		!getarg(pos,name) Sets name to the pos-th command-line argument 
call getarg(2,file_gv_exper)
! Hay que escribir en el shell: 
! phonons 'nombre fichero gv teorico' 'nombre fichero gv experimental' y ejecuta solito todo



open(unit=1,file=file_gv_theor,status="old",action="read",position="rewind")
   n=1
   !La variable io es cero solo si lee algo. Asi hacemos un bucle de numero de terminos desconocido
   do
      read(unit=1,fmt=*,iostat=io) v(n),g_theor(n)
      if (io/=0) then
         exit
      end if
	  n=n+1
   end do
close(unit=1,status="keep")

open(unit=1,file=file_gv_exper,status="old",action="read",position="rewind")
   n=1
   !La variable io es cero solo si lee algo. Asi hacemos un bucle de numero de terminos desconocido
   do
      read(unit=1,fmt=*,iostat=io) v(n),g_exper(n)
      if (io/=0) then
         exit
      end if
	  n=n+1
   end do
close(unit=1,status="keep")

n=n-1 !because it gets out the loop with an increment of 1
open(unit=1,file="phonons.txt",status="replace",action="write",position="rewind")
        do l=1,n
                write(unit=1,fmt=*) v(l),g_theor(l)-g_exper(l)
        end do
close(unit=1,status="keep")

end program phonons
