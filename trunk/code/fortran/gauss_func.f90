program gauss_func

real, dimension(1000) :: x,y
real :: b,e,g,rnum
integer :: i

b=1
e=2
g=5

open(unit=1,file="gauss_data.txt",status="replace",action="write",position="rewind")
do i=1,1000
	x(i)=i/100.0
	call random_number(rnum)
	y(i) = b*exp(-((x(i)-e)/g)**2.0) + rnum/10 
	write(unit=1,fmt="(f11.5,f11.5)") x(i),y(i)
	print*,rnum
end do
close(unit=1,status="keep")

end program gauss_func
