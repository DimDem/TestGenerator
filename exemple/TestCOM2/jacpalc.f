        program    jacpal
        double precision maxepsd,maxeps
        parameter (nxd=128,nyd=256,nzd=128)
        parameter(maxepsd = 0.1,  itmaxd=50)
        double precision,dimension(nxd,nyd,nzd):: a,b
        common /arr/ a,b
        double precision relax,eps
        integer trace        
        
c---------------------------------------------------------------------
c      Read input file (if it exists), else take
c      defaults from parameters
c---------------------------------------------------------------------
          
       write(*, 1000)
       nx=nxd
       ny=nyd
       nz=nzd
       maxeps=maxepsd
       itmax=itmaxd
       trace=1       

       write(*, 1001) nx,ny,nz
       write(*, 1002) itmax,maxeps

 1000 format(//,' JAC-Benchmark', /)
 1001     format(' Size: ', i3, 'x', i3, 'x', i3)
 1002     format(' Iterations: ', i3, '    eps: ', F10.6)
       if(nx*ny*nz .gt. nxd*nyd*nzd) then
         print *,
     >   ' Problem size too big! Increase parameters: nxd,nyd,nzd'
         stop
       endif

       call init ()
       
       do it = 1,itmax
         eps = relax()
         if(trace.ne.0) then
           if(it .eq.1 .or. mod(it,trace).eq.0) 
     >       print *, ' it= ',it, '   eps= ',eps     
         endif
         if(eps.lt.maxeps) exit
       end do

       write(*,1005) eps

1005   format(' eps', 14x,f10.6)       
       if(trace.ne.0) call verify()
       
       end


c---------------------------------------------------------------------
c---------------------------------------------------------------------
      subroutine init()
c---------------------------------------------------------------------
c---------------------------------------------------------------------
      parameter (nxd=128,nyd=256,nzd=128)

      double precision a(nxd,nyd,nzd), b(nxd,nyd,nzd), solution
      
      common /arr/ a,b
      solution(i,j,k) = 10.*(i-1)/(nxd-1) +10.*(j-1)/(nyd-1)+
     >                  10.*(k-1)/(nzd-1)

      do k = 1,nzd
        do j = 1,nyd
          do i = 1,nxd
             if(k.eq.1 .or. k.eq.nzd .or. j.eq.1 .or. j.eq.nyd .or.
     >          i.eq.1 .or. i.eq.nxd) then
                a(i,j,k) = solution(i,j,k)
             else
                a(i,j,k) = 0.D0                
             endif
           enddo
         enddo
       enddo
       end

c---------------------------------------------------------------------
c---------------------------------------------------------------------
      double precision function relax()
c---------------------------------------------------------------------
c---------------------------------------------------------------------
      parameter (nxd=128,nyd=256,nzd=128)

      double precision a(nxd,nyd,nzd), b(nxd,nyd,nzd), eps
      
      common /arr/ a,b

      do k = 2,nzd-1
        do j = 2,nyd-1
          do i = 2,nxd-1
             b(i,j,k) = (a(i-1,j,k) + a(i+1,j,k) +
     >                   a(i,j-1,k) + a(i,j+1,k) +
     >                   a(i,j,k-1) + a(i,j,k+1)) / 6  
          enddo
        enddo
      enddo
      eps = 0.D0

      do k = 2,nzd-1
        do j = 2,nyd-1
          do i = 2,nxd-1
             bt = b(i,j,k)
             eps = max(eps, abs(bt-a(i,j,k)))
             a(i,j,k) = bt
          enddo
        enddo
      enddo
      relax = eps
      end
 
   
c---------------------------------------------------------------------
c---------------------------------------------------------------------
      subroutine verify()
c---------------------------------------------------------------------
c---------------------------------------------------------------------
      parameter (nxd=128,nyd=256,nzd=128)

      double precision a(nxd,nyd,nzd), b(nxd,nyd,nzd), eps, solution
      
      common /arr/ a,b

      solution(i,j,k) = 10.*(i-1)/(nxd-1) +10.*(j-1)/(nyd-1)
     >                  +10.*(k-1)/(nzd-1)
      eps = 0.D0
      do k = 1,nzd
        do j = 1,nyd
          do i = 1,nxd
            eps = max(eps,abs(a(i,j,k)-solution(i,j,k)))
           enddo
         enddo
      enddo
      write(*,1004) eps
1004  format(' Delta',12x, f10.6)
    
      end
     

