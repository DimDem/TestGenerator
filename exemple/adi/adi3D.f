        program adi
        double precision maxepsd,maxeps
        parameter (nxd=32,nyd=32,nzd=32)
        parameter(maxepsd = 0.1,  itmaxd=100)
        double precision, allocatable, dimension (:,:,:) :: pa,pb
        double precision eps,dvtime,time,time0,relax
        integer trace 
        external verify          
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
       trace=0       

       write(*, 1001) nx,ny,nz
       write(*, 1002) itmax,maxeps

 1000 format(//,' ADI-Benchmark', /)
 1001     format(' Size: ', i3, 'x', i3, 'x', i3)
 1002     format(' Iterations: ', i3, '    eps: ', F10.6)
       allocate (pa (nxd, nyd, nzd))
       call init (pa,nx,ny,nz)

       do it = 1,itmax
         eps = relax(pa,nx,ny,nz)
         if(trace.ne.0 .and. trace.ne.999) then
           if(it .eq.1 .or. mod(it,trace).eq.0) 
     >       print *, ' it= ',it, '   eps= ',eps     
         endif
       end do
3      continue

       write(*,1005) eps
1003   format(' Time in seconds', f12.2)
1005   format(' eps', 14x,f10.6)       
       if(trace.ne.0) call verify(pa,nx,ny,nz)
       deallocate(pa)
       end


c---------------------------------------------------------------------
c---------------------------------------------------------------------
      subroutine init(a,nx,ny,nz)
c---------------------------------------------------------------------
c---------------------------------------------------------------------
      double precision a(nx,ny,nz),solution
      integer nx,ny,nz

      do k = 1,nz
        do j = 1,ny
          do i = 1,nx
             if(k.eq.1 .or. k.eq.nz .or. j.eq.1 .or. j.eq.ny .or.
     >          i.eq.1 .or. i.eq.nx) then
                a(i,j,k) = 10.*(i-1)/(nx-1) +10.*(j-1)/(ny-1)
     >                    +10.*(k-1)/(nz-1)
             else
                a(i,j,k) = 0.D0                
             endif
           enddo
         enddo
       enddo
       end

c---------------------------------------------------------------------
c---------------------------------------------------------------------
      double precision function relax(a,nx,ny,nz)
c---------------------------------------------------------------------
c---------------------------------------------------------------------
      double precision a(nx,ny,nz),eps
      integer nx,ny,nz
     
      eps = 0.D0
      do k = 2,nz-1
        do j = 2,ny-1
          do i = 2,nx-1
             a(i,j,k) = (a(i-1,j,k) + a(i+1,j,k)) / 2
          enddo
        enddo
      enddo

      do k = 2,nz-1
        do j = 2,ny-1
          do i = 2,nx-1
             a(i,j,k) = (a(i,j-1,k) + a(i,j+1,k)) / 2
          enddo
        enddo
      enddo

      do k = 2,nz-1
        do j = 2,ny-1
          do i = 2,nx-1
	     eps = max(eps, abs(a(i,j,k) -
     >                 (a(i,j,k-1) + a(i,j,k+1)) / 2))
             a(i,j,k) = (a(i,j,k-1) + a(i,j,k+1)) / 2 
          enddo
        enddo
      enddo

      relax = eps
      end
 
   
c---------------------------------------------------------------------
c---------------------------------------------------------------------
      subroutine verify(a,nx,ny,nz)
c---------------------------------------------------------------------
c---------------------------------------------------------------------
      double precision a(nx,ny,nz),solution,eps
      integer nx,ny,nz
      eps = 0.D0
      do k = 1,nz
        do j = 1,ny
          do i = 1,nx
            eps = max(eps,abs(a(i,j,k)
     > - 10.*(i-1)/(nx-1) + 10.*(j-1)/(ny-1)
     > +10.*(k-1)/(nz-1)))
           enddo
         enddo
      enddo
      write(*,1004) eps
1004  format(' Delta',12x, f10.6)    
      end
     
