subroutine peg_solitaire (E)

implicit none

    integer :: E(1:7,1:7), F(1:7, 1:7)                           ! Initial board 7x7
    real :: ll, mm, yy                                           ! Random Number 
    !integer :: values(1:8), tt
    !integer, dimension(:), allocatable :: seed
    integer i,j, n, o, p, q, r, s, x, k, g, h, v,u, ctr          ! Loop indices
    integer mov(32,1)                                            !movement matrix
    integer pos(32,2)  										     !position matrix
    real t1, t2, ep


ctr=0

! Calling out the random seed 
!**************************************************************************!

!call date_and_time(values=values)
!call random_seed(size=tt)
!allocate(seed(1:tt))
!seed(:) = values(8)
!call random_seed(put=seed)

! Opening the file for the Visualization
!**************************************************************************!

open(2, file = 'data1.txt') 

! Opening the file for the Storing Time Data
!**************************************************************************!

open(3, file = 'data_time_rnd.txt') 

! Printing the Initial board as Matrix
!**************************************************************************!
do i=1,7
        
		             write(2,*) (E(i,j), j=1,7)                                         
		
enddo
F=E

call cpu_time(t1)                                                                      ! Time taken to complete inner loop       
	
! Entering the outer loop
!**************************************************************************!
	
        do u=1000, 2000, 1000			!counter for inner loop
	
                      write(2,*) 'for inner loop =', u
                      write(3,*) 'for inner loop =', u
					  
                      ctr=0     

                                                                            
            do x=1, 1000000             !outer loop
			
               if(ctr.gt.5) then                                                       ! Exit the program when ctr is greater than five
	      
		        exit
			
                endif

                   p=32     
				   
                do o=1,32      
		           mov(o, 1)=0
                enddo
				
		        do o=1,32      
		           pos(o,1)=0
		           pos(o,2)=0
                enddo

                E=F                                                                    ! Initialise the board with original board

! Entering the inner loop
!**************************************************************************!

                do q=1, u								                                ! Inner loop
				
                    if(p.eq.1) then                                                     ! Condition for solution
	                           write(2,*) 'solution found'                              ! Visualtion part 
		                       write(2,*) 'the final matrix' 
		                do o=1,7
		                       write(2,*) (E(o, k), k=1,7)
                        enddo
		
		                  E=F                                                           ! Initialising the matrix with original to visualise
		                  ctr=ctr+1                                                     ! Counter for solution 
		
		                do o=32, 2, -1
					 
		                      i=pos(o,1)                                                ! Store x coordinate
			                  j=pos(o,2)	                                            ! Store y coordinate
							  
			                  write(2,*) 'the coordinates are', pos(o,1), pos(o,2)      ! Print x and y coordinate
			                  write(2,*) 
			                  write(2,*) 'the steps are', (mov(o, 1))                   ! Print movements
			                  write(2,*) 
			 
			 
		                    if(mov(o,1).eq.1) then                                      ! Right 
                              E(i,j)=0
			                  E(i,j+1)=0
			                  E(i,j+2)=1
			   
                                    do g=1,7
		                                   write(2,*) (E(g, k), k=1,7)
                                    enddo
			                endif	
			
		                    if(mov(o,1).eq.2) then                                       ! Downward
		                       E(i,j)=0
			                   E(i-1,j)=0
			                   E(i-2,j)=1
			    
				                    do g=1,7
		                                   write(2,*) (E(g, k), k=1,7)
                                    enddo 
		                    endif		
		     
		                    if(mov(o,1).eq.3) then                                        ! Upward
		                       E(i,j)=0
		                       E(i+1,j)=0
		                       E(i+2,j)=1
		          
				                    do g=1,7
		                                   write(2,*) (E(g, k), k=1,7)
                                    enddo
		                    endif
		   
		                    if(mov(o,1).eq.4) then                                         ! Left
		                        E(i,j)=0
		                        E(i,j-1)=0
		                        E(i,j-2)=1
		       
			                        do g=1,7
		                                   write(2,*) (E(g, k), k=1,7)
                                    enddo
	                        endif

                        enddo
		
call cpu_time(t2)                                                                          ! Total time taken for computational and visualisation  
	
	                              ep = t2 - t1            
	                              write(2,*)
	                              write(2,*) "time=", ep ,"Sec"
	                              write(2,*)
	                              write(3,*)
	                              write(3,*) "time=", ep ,"Sec"
	                              write(3,*)
								  
		                if(ctr.gt.5) then
		                    exit
		                endif  
		            endif



                    CALL RANDOM_NUMBER(ll)                                                  ! Calling the random numbers
	                CALL RANDOM_NUMBER(mm)
	                CALL RANDOM_NUMBER(yy)
	                r=INT(7*ll+1)                                                           ! Gives a value between 1-7
	                s=INT(7*mm+1)                                                           ! Gives a value between 1-7
	                v=INT(4*yy+1)                                                           ! Gives a value between 1-4
	  
	                 i=r
	                 j=s

! For pegs more than one
!**************************************************************************!

	                do g=1, v                                                     ! Optimisation principle 
        		if((j+2).le.7 .and. E(i,j)==1 .and. E(i,j+1)==1 .and. E(i,j+2)==0 .and. E(i,j).ne.-1 .and. E(i,j+1).ne.-1 .and. &
				& E(i,j+2).ne.-1 .and. g.eq.1)then                                ! Right 
				
				                       E(i,j)=0
				                       E(i,j+1)=0
				                       E(i,j+2)=1
									   
				                       mov(p,1)=g
				                       pos(p,1)=r
				                       pos(p,2)=s
				                       p=p-1
									   
				endif
				if((i-2).gt.1 .and. E(i,j)==1 .and. E(i-1,j)==1 .and. E(i-2,j)==0 .and. E(i,j).ne.-1 .and. E(i-1,j).ne.-1 .and. &
				& E(i-2,j).ne.-1 .and. g.eq.2)then                               ! Downward   !Faster optimisation with i gt 3
				
				                       E(i,j)=0
				                       E(i-1,j)=0
				                       E(i-2,j)=1
									   
			                           mov(p,1)=g
				                       pos(p,1)=r
				                       pos(p,2)=s
				                       p=p-1
									   
				endif
				if((i+2).le.7 .and. E(i,j)==1 .and. E(i+1,j)==1 .and. E(i+2,j)==0 .and. E(i,j).ne.-1 .and. E(i+1,j).ne.-1 &
				& .and. E(i+2,j).ne.-1  .and. g.eq.3)then                        ! Upward    
				
				                       E(i,j)=0
				                       E(i+1,j)=0
				                       E(i+2,j)=1
									   
				                       mov(p,1)=g
				                       pos(p,1)=r
				                       pos(p,2)=s
				                       p=p-1
									   
				endif
				if((j-2).gt.1 .and. E(i,j)==1 .and. E(i,j-1)==1 .and. E(i,j-2)==0 .and. E(i,j).ne.-1 .and. E(i,j-1).ne.-1 .and. &
				& E(i,j-2).ne.-1  .and. g.eq.4)then                             ! Left    
				
				                       E(i,j)=0         
				                       E(i,j-1)=0
				                       E(i,j-2)=1
									   
				                       mov(p,1)=g
				                       pos(p,1)=r
				                       pos(p,2)=s
				                       p=p-1
									   
				endif 
			        enddo		

	                            if(p.le.0) then                                 ! Unnatural condition check 
	                                   write(*,*) E
		                               write(2,*) p
	                                exit
	                            endif
	
                enddo
            enddo
        enddo	
		
! Closing the txt file and ending subroutine
!**************************************************************************!
		
close(2)

end subroutine 