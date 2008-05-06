program smooth
!-------------------------------------------------------------------------------------
! Este programa calcula la derivada de unos datos V-I a traves de regresion lineal
! Para cada punto, toma 2n+1 puntos de su alrededor y los ajusta a una recta. La derivada es su pendiente.
! No se hace todo el ajuste, simplemente se calcula la pendiente con su formula analitica
! Cuantos mas puntos mas suaviza y menor es la resolucion obtenida, mas informacion se pierde.
! Es necesario el equilibrio entre quitar ruido y perdida de informacion, que se consigue a ojo
! El archivo que lee tiene los datos en dos columnas: V - I
!-------------------------------------------------------------------------------------
INTEGER,PARAMETER:: DP=SELECTED_REAL_KIND(15)
REAL(KIND=DP),DIMENSION(4000):: v,i,di
REAL(KIND=DP) :: s,sx,sy,sxy,delta,sigma
INTEGER:: dim,io,l,n,k
CHARACTER(LEN=50):: file_name, file_name_deriv 

call getarg(1,file_name)		!getarg(pos,name) Sets name to the pos-th command-line argument 
call getarg(2,file_name_deriv)	
!call getarg(3,n)	

! Hay que escribir en el shell: 
! derivada 'nombre fichero input' 'nombre fichero output' 'numero de puntos que toma' y ejecuta solito todo

!--------------------------------------------
!Vamos a leer los datos del archivo, V-I
!--------------------------------------------
open(unit=1,file=file_name,status="old",action="read",position="rewind")
   dim=1
   !La variable io es cero solo si lee algo. Asi hacemos un bucle de numero de terminos desconocido
   do
      read(unit=1,fmt=*,iostat=io) v(dim),i(dim)
      if (io/=0) then
         exit
      end if
   dim=dim+1
   end do
close(unit=1,status="keep")
dim=dim-1	! Porque al salir del bucle la variable de conteo se incrementa en uno

sigma = 1.0e-7

n=1		!Puntos que toma a cada lado para hacer la derivada por regresion, que son 2n+1
s=(2*n+1)/sigma	!Lo escribo asi porque es la misma sigma para todos

do k=1,dim
   if( (k-n) < 1 .or. (k+n) > dim ) then
		di(k) = 0.0
   else
	sx=0.0
	sy=0.0
	sxx=0.0
	sxy=0.0
	do l=-n,n
		sx = sx + v(k+l)
		sy = sy + i(k+l)
		sxx = sxx + v(k+l)**2
		sxy = sxy + v(k+l)*i(k+l) 
	end do
	sx = sx/sigma
	sy = sy/sigma
	sxx = sxx/sigma
	sxy = sxy/sigma
	delta = s*sxx - sx**2
	di(k) = (s*sxy-sx*sy)/delta
   end if
!print*,s,sx,sy,sxx,sxy,delta
!print*,k,di(k)
!pause
end do
open(unit=1,file=file_name_deriv,status="replace",action="write",position="rewind")
        do l=1,dim
                write(unit=1,fmt=*) v(l),di(l)
        end do
close(unit=1,status="keep")

!-------------------------------------------
end program smooth
