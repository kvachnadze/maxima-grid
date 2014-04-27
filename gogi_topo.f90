     program gogi_topo
     
     implicit none
     integer                              :: nblock,nb,i,j,k
     integer                              :: mshwin(6),mshwbc(6),mshwbcfam(6),mshwdf(4,6)
     integer, dimension(:), allocatable   :: n1,n2,n3,nwin,npbc,nbbc
     integer, dimension(:,:), allocatable :: blkdef,blkdir,blkcon,blkfac     
     character(len=30)                    :: mshtype,mshpers
     character(len=80)                    :: mshtitle,mshcomment
     real(kind=8)                         :: xyzref(3),sref,cref

     ! MDES table information      
     mshtitle="Splitter_scale4"
     mshcomment="Single splitter mesh"
     mshtype="2D with 2 cells in z-direction"
     mshpers="Gogi+Litvinov"
     cref=1.0 ! Reference length
     sref=1.0 ! Reference surface
     !Reference origin point
     xyzref(1)=0
     xyzref(2)=0 
     xyzref(3)=0

     !nblock = 4 ! number of blocks (can read from grid file first line).
                !For this routine should be > 1

         
    

     !number of cells for each block (can read from grid file second line -
     !node numbers)
     
     open (unit=11,file="g.fmt")
       read (11,*) nblock       
     close(11)    
     
     !nblock=7
     
     allocate(n1(nblock))
     allocate(n2(nblock))
     allocate(n3(nblock))
    
     allocate(nwin(nblock))
     allocate(npbc(nblock))
     allocate(nbbc(nblock))

     allocate(blkcon(nblock,2))
     allocate(blkfac(nblock,2))

     allocate(blkdef(4,2))
     allocate(blkdir(2,2))


     open (unit=11,file="g.fmt")     
       read (11,*)
       read (11,*) (n1(nb),n2(nb),n3(nb), nb=1,nblock)
     close(11)
     !print *, "read from file", nblock, n1(1), n2(1), n3(1), n1(3), n2(3), n3(3)

     do nb=1,nblock
       n1(nb)=n1(nb)-1 !calc the number of cells from number of nodes
       n2(nb)=n2(nb)-1
       n3(nb)=n3(nb)-1
       npbc(nb)=0 ! periodic b. conditions
       nbbc(nb)=2 ! block connectivity boundaries     
       nwin(nb)=6 ! number of windows in each block. Equals 6 if we're not splitting windows
     
     enddo

     blkcon(1,1)=3
     blkcon(1,2)=2
     blkcon(2,1)=4
     blkcon(2,2)=1
     blkcon(3,1)=7
     blkcon(3,2)=1
     blkcon(4,1)=5
     blkcon(4,2)=2
     blkcon(5,1)=4
     blkcon(5,2)=6
     blkcon(6,1)=5
     blkcon(6,2)=7
     blkcon(7,1)=3
     blkcon(7,2)=6

     blkfac(1,1)=2
     blkfac(1,2)=2
     blkfac(2,1)=2
     blkfac(2,2)=2
     blkfac(3,1)=2
     blkfac(3,2)=1
     blkfac(4,1)=2
     blkfac(4,2)=1
     blkfac(5,1)=1
     blkfac(5,2)=3
     blkfac(6,1)=4
     blkfac(6,2)=3
     blkfac(7,1)=1
     blkfac(7,2)=4


     ! Writing out topology file for convmesh

     open (unit=10,file="gogi_topo.out")

       write (10,'(a)') mshtitle
       write (10,'(a)') mshcomment
       write (10,'(a)') mshtype
       write (10,'(a)') mshpers
       write (10,*)     cref
       write (10,*)     sref
       write (10,*)     xyzref(1),xyzref(2),xyzref(3)
       write (10,*)     nblock
   
     do nb=1,nblock     

         do i=1,6
           if (nwin(nb).eq.6) then
             mshwin(i)=1
           endif
         enddo
     !
        if (nb.lt.5) then
          mshwbc(1)=500
          mshwbc(2)=500
          mshwbc(3)=300
          mshwbc(4)=130
          mshwbc(5)=410
          mshwbc(6)=410
         do k=1,2 
          blkdef(1,k)=1
          blkdef(2,k)=n2(nb)
          blkdef(3,k)=1
          blkdef(4,k)=n3(nb)
          blkdir(1,k)=2
          blkdir(2,k)=3          
         enddo
        end if
        if (nb.eq.5) then
            mshwbc(1)=230
            mshwbc(2)=500
            mshwbc(3)=130
            mshwbc(4)=500
            mshwbc(5)=410
            mshwbc(6)=410
            blkdef(1,1)=1
            blkdef(2,1)=n2(nb)
            blkdef(3,1)=1
            blkdef(4,1)=n3(nb)
            blkdef(1,2)=1
            blkdef(2,2)=n3(nb)
            blkdef(3,2)=1
            blkdef(4,2)=n1(nb)
            blkdir(1,1)=-2
            blkdir(2,1)=3
            blkdir(1,2)=3
            blkdir(2,2)=1   
        end if 
        if (nb.eq.6) then
            mshwbc(1)=230
            mshwbc(2)=300
            mshwbc(3)=500
            mshwbc(4)=500
            mshwbc(5)=410
            mshwbc(6)=410
         do k=1,2 
          blkdef(1,k)=1
          blkdef(2,k)=n3(nb)
          blkdef(3,k)=1
          blkdef(4,k)=n1(nb)
          blkdir(1,k)=3
          blkdir(2,k)=1  
         enddo
        end if 
        if (nb.eq.7) then
            mshwbc(1)=230
            mshwbc(2)=500
            mshwbc(3)=500
            mshwbc(4)=130
            mshwbc(5)=410
            mshwbc(6)=410
            blkdef(1,1)=1
            blkdef(2,1)=n2(nb)
            blkdef(3,1)=1
            blkdef(4,1)=n3(nb)
            blkdef(1,2)=1
            blkdef(2,2)=n3(nb)
            blkdef(3,2)=1
            blkdef(4,2)=n1(nb)
            blkdir(1,1)=2
            blkdir(2,1)=3
            blkdir(1,2)=3
            blkdir(2,2)=1 
        end if  
                  
        do i=1,nwin(nb)          
          if (i.lt.3) then
            mshwdf(1,i)=1
            mshwdf(2,i)=n2(nb)
            mshwdf(3,i)=1
            mshwdf(4,i)=n3(nb)
          end if
          if (i.lt.5) then
            mshwdf(1,i)=1
            mshwdf(2,i)=n3(nb)
            mshwdf(3,i)=1
            mshwdf(4,i)=n1(nb)
          end if
          if (i.lt.7) then
            mshwdf(1,i)=1
            mshwdf(2,i)=n1(nb)
            mshwdf(3,i)=1
            mshwdf(4,i)=n2(nb)
          end if          
        enddo              

         write(10,'(3i12,3x,a,i5)') n1(nb),n2(nb),n3(nb),'Block:',nb
         write (10,*) nwin(nb),npbc(nb),nbbc(nb)
         write (10,*) (mshwin(i),i=1,6)
         write (10,*) (mshwbc(i),i=1,6)
         write (10,*) (mshwbcfam(i),i=1,6)
         write (10,*) ((mshwdf(j,i),j=1,4),i=1,6)
         
         !place for periodic b.c
         !      if (npbc .gt. 0) then

         !            write (10,*) (mshwcf(i),i=1,npbc)

         !            write (10,*) ((mshwcd(j,i),j=1,4),i=1,npbc)
         !         endif

         !Block connectivity
            write (10,*) (blkcon(nb,i),i=1,nbbc(nb))
            write (10,*) (blkfac(nb,i),i=1,nbbc(nb))
            write (10,*) ((blkdef(j,i),j=1,4),i=1,nbbc(nb))
            write (10,*) ((blkdir(j,i),j=1,2),i=1,nbbc(nb)) 
         
         
     end do
   
     close(10)

     end program gogi_topo
