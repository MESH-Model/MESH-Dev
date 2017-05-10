      program daily
      logical enddata
      real val(40), valtot(40)

      open(1,file = 'station.txt', status='old')
      open(2,file = 'stationtotal.txt', status='unknown')

      icount = 0
      enddata = .false.
      read(1,*)
      do while (.not. enddata)
         read(1,*,end=999)(val(i),i=1,40)
         valtot = valtot + val
         icount=icount + 1
         if(mod(icount,4)==0)then
            write(2,'(40(f12.7,1X))')(valtot(i),i=1,40)
            valtot = 0.0
         endif
      enddo
999   enddata = .true.
      close(1)
      close(2)
      end
      

