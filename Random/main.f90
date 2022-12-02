! PEG SOLITARE !

program main 

implicit none 

integer    E(7,7)              ! Initial board matrix 7x7
integer :: i,j,k               ! Loop indices

! Opening the Initial board matrix 7x7
!**************************************************************************!

open(10,file='initial.txt')

    do i=1,7
        read(10,*)(E(i,j),j=1,7)
    enddo
	
close(10)

! Calling the subroutine 
!**************************************************************************!

call peg_solitaire(E)


end program
