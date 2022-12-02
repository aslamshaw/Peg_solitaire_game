! PEG SOLITARE !

program peg_solitare

implicit none

integer    A(7,7)              ! Initial board 7x7
integer    A1(7,7)             ! Temporary Initial board 7x7(For Visualization)
integer :: i,j,k               ! Loop indices
logical :: findSolution, ans   ! Logical conditions
integer :: x, numOfSolutions   ! Number of remaining pegs and no. of solutions
integer:: r(31,3)              ! Moving peg coordinate and directions
real t1, t2, ep, t3, t4        ! Computational and Visualisation Time

! Calling Cpu Time 
!**************************************************************************!
call cpu_time(t1)

! Defining Initaial Board 
!**************************************************************************!
do i=1, 7
		do j=1, 7
				if (i<3 .and. j<3) then
				A(i,j)=-1                           ! Left top Corner of the Board
				else if (i<3 .and. j>5) then
				A(i,j)=-1                           ! Right top Corner of the Board     
				else if (i>5 .and. j<3) then
				A(i,j)=-1                           ! Left Bottom Corner of the Board
				else if (i>5 .and. j>5) then
				A(i,j)=-1                           ! Right Bottom Corner of the Board
				else if (i==4 .and. j==4) then
				A(i,j)=0			                ! No peg ("0" represents the free spaces) at the centre of the board at first	
				else 
				A(i,j)=1                            ! "1" Represents the pegs
				end if
		enddo
enddo

! Opening the file for the Visualization
!**************************************************************************!
open(2, file = 'data1.txt')

! Opening the file for the Storing Time Data
!**************************************************************************!

open(3, file = 'data_time_rec.txt')

do i=1,7
				write(*,*) (A(i,j), j=1,7)
				write(2,*)                          ! Printing the Initial board as Matrix
				write(2,*) (A(i,j), j=1,7)
enddo

A1=A				
x = 32                                              ! Initial No. of Remaining Pegs
numOfSolutions = 0

! Calling Recursive Function
!**************************************************************************!

ans = findSolution(x, A,A1, numOfSolutions,r,t1)

call cpu_time(t3)                                   ! Total time taken for whole program
	ep = t3 - t1
	write(2,*)
	write(2,*) "total time=", ep ,"Sec"
	write(3,*)
	write(3,*) "total time=", ep ,"Sec"

if(ans .eqv. .false.) then	                       ! When solution is not found
	print *, "Solution is not found!"
else 
	print *, "End of searching"
end if

! Closing both files
!**************************************************************************!
close(2)
close(3)

end program
