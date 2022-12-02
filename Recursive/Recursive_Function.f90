recursive function findSolution(numOfPegs, A,A1,numOfSolutions,r,t1) result(validSolution)

Implicit none

	integer    :: numOfPegs                      ! Input
	logical    :: validSolution, ans             ! Output
	integer    :: i,j,k,e,l, m, n, o , q, w, z   ! loop indices
	integer                A(7,7)
	integer                A1(7,7)
	integer    :: numOfSolutions                 ! Number of soltions found
	integer    :: p                              ! Number of moves
	integer    :: r(31,3)                        ! Moving peg coordinate and directions
	real         t1, t2, ep, t3, t4              ! Computational and Visualisation Time
	
	
! Checking Whether solution is found or not 
!**************************************************************************!
	
	if(numOfPegs == 1 ) then                        !final No. Of Remaining Pegs = 1 and Peg is at the Centre
	
! Callimg cpu time to calculate computational time 
!**************************************************************************!	    
		call cpu_time(t4)
	       ep = t4 - t1
		   write(2,*)
	       write(2,*) "computational time=", ep ,"Sec"
	       
		   numOfSolutions = numOfSolutions + 1
	       write(3,*)'for ', numOfSolutions, 'iteration'		
		   write(3,*)
	       write(3,*) "computational time=", ep ,"Sec"
		   
		   print *, "Done"	
		
		   write(2,*)
		   write(2,*)"Solution Found"
		   write(2,*)
		
! Visualization of the Peg movement 
!**************************************************************************!	
	
		do i=1,7
				write(*,*) (A(i,j), j=1,7)
				write(2,*)                                            ! Printing the Final board as Matrix
				write(2,*) (A(i,j), j=1,7)
				enddo
					
				do e=1,31
				     write(*,*) 'Coordinate is', (r(e,n), n=1,2)
					 write(*,*)
					 
					 write(2,*)
					 write(2,*) 'Coordinate is', (r(e,n), n=1,2)      ! Printing the Coordinate of the peg which start each move
					 write(2,*) 
					 
					 write(*,*) 'Direction is', (r(e,3)) 
	                 write(*,*)
					 					 
					 write(2,*) 'Direction is', (r(e,3))              ! Printing the Direction of the move
					 write(2,*)
  					 					 
					l= r(e,1) 
					m= r(e,2)
					n= r(e,3)
                    if(n.eq.1)then                                    !Jumping to right is number 1
				       A1(l,m)=0
				      A1(l,m+1)=0
				      A1(l,m+2)=1
					  do o=1,7
				            write(*,*) (A1(o,q), q=1,7)
					        write(2,*) (A1(o,q), q=1,7)
				      enddo
					endif
					
					if(n.eq.2)then                                     !Jumping to Upward is number 2
				      A1(l,m)=0
				      A1(l-1,m)=0
				      A1(l-2,m)=1
					  do o=1,7
				            write(*,*) (A1(o,q), q=1,7)
					        write(2,*) (A1(o,q), q=1,7)
				      enddo
					endif
					
					if(n.eq.3)then                                    !Jumping to Downward is number 3
				      A1(l,m)=0
				      A1(l+1,m)=0
				      A1(l+2,m)=1
					  do o=1,7
				            write(*,*) (A1(o,q), q=1,7)
					        write(2,*) (A1(o,q), q=1,7)
				      enddo 
					endif
					
					if(n.eq.4)then                                   !Jumping to Left is number 4
				      A1(l,m)=0
				      A1(l,m-1)=0
				      A1(l,m-2)=1
					  do o=1,7
				            write(*,*) (A1(o,q), q=1,7)
					        write(2,*) (A1(o,q), q=1,7)
				      enddo
					endif
				enddo

! Callimg cpu time to calculate Visualisation time 
!**************************************************************************!
				
call cpu_time(t2)
	
	
! Recovering of the Initial board (A1 matrix) for further visualization	             
	    
		do w=1, 7
		        do z=1, 7
				    if (w<3 .and. z<3) then
				      A1(w,z)=-1
				      else if (w<3 .and. z>5) then
				      A1(w,z)=-1
				      else if (w>5 .and. z<3) then
				      A1(w,z)=-1
				      else if (w>5 .and. z>5) then
				      A1(w,z)=-1
				      else if (w==4 .and. z==4) then
				      A1(w,z)=0				
				      else 
				      A1(w,z)=1
				    end if
		        enddo
        enddo
	
	ep = t2 - t4

    write(2,*)
	write(2,*) "visualisation time=", ep ,"Sec"	
	write(3,*)
	write(3,*) "visualisation time=", ep ,"Sec"
	write(3,*)

! prompt as a "pause"	
			
		read(*,*)

! Jumping of pegs if pegs are remaining
	
	else 
		do i = 1, 7
			do j = 1, 7
				
					
				if(j.le.5 .and. A(i,j)==1 .and. A(i,j+1)==1 .and. A(i,j+2)==0)then     ! Jumping to right is number 1
				A(i,j)=0
				A(i,j+1)=0
				A(i,j+2)=1
				numOfPegs=numOfPegs-1
				
				r((32-numOfPegs),1)=i                                                  ! Storing x coordinate of jumped peg in r matrix
				r((32-numOfPegs),2)=j                                                  ! Storing y coordinate of jumped peg in r matrix
				r((32-numOfPegs),3)=1	                                               ! Storing jumped direction of the peg in r matrix
				
					ans = findSolution(numOfPegs, A,A1,numOfSolutions,r,t1)            ! Calling recursive function  
					
				
					A(i,j)=1                                                           ! Restoring the previous position if a dead end comes
					A(i,j+1)=1
					A(i,j+2)=0
					numOfPegs=numOfPegs+1
					
				
				else if(i.ge.3 .and. A(i,j)==1 .and. A(i-1,j)==1 .and. A(i-2,j)==0)then  ! Jumping to upward is number 2 
				A(i,j)=0
				A(i-1,j)=0
				A(i-2,j)=1
				numOfPegs=numOfPegs-1
				r((32-numOfPegs),1)=i
				r((32-numOfPegs),2)=j
				r((32-numOfPegs),3)=2	
					ans = findSolution(numOfPegs, A,A1, numOfSolutions,r,t1)
					
						
					A(i,j)=1
					A(i-1,j)=1
					A(i-2,j)=0
					numOfPegs=numOfPegs+1
				   
					
				else if(i.le.5 .and. A(i,j)==1 .and. A(i+1,j)==1 .and. A(i+2,j)==0)then  ! Jumping to downward is 3
				A(i,j)=0
				A(i+1,j)=0
				A(i+2,j)=1
				numOfPegs=numOfPegs-1
				
				r((32-numOfPegs),1)=i
				r((32-numOfPegs),2)=j
				r((32-numOfPegs),3)=3			
					ans = findSolution(numOfPegs, A,A1, numOfSolutions,r,t1)
					
											    
					A(i,j)=1
					A(i+1,j)=1
					A(i+2,j)=0
					numOfPegs=numOfPegs+1
					
					
					
				else if(j.ge.3 .and. A(i,j)==1 .and. A(i,j-1)==1 .and. A(i,j-2)==0)then   !Jumping to left is 4
				A(i,j)=0
				A(i,j-1)=0
				A(i,j-2)=1
				numOfPegs=numOfPegs-1
			
				r((32-numOfPegs),1)=i
				r((32-numOfPegs),2)=j
				r((32-numOfPegs),3)=4		
				ans = findSolution(numOfPegs, A,A1, numOfSolutions,r,t1)
					
																
					A(i,j)=1
					A(i,j-1)=1
					A(i,j-2)=0
					numOfPegs=numOfPegs+1
								
				endif
													
			enddo
		enddo
	end if
	
	if (numOfSolutions > 0) then
		validSolution = .true.
	else 
		validSolution = .false.
	end if
end function