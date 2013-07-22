C    This file is part of WATROUTE.
C
C    WATROUTE is free software: you can redistribute it and/or modify
C    it under the terms of the GNU Lesser General Public License as published by
C    the Free Software Foundation, either version 3 of the License, or
C    (at your option) any later version.
C
C    WATROUTE is distributed in the hope that it will be useful,
C    but WITHOUT ANY WARRANTY; without even the implied warranty of
C    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C    GNU Lesser General Public License for more details.
C
C    You should have received a copy of the GNU Lesser General Public License
C    along with WATROUTE.  If not, see <http://www.gnu.org/licenses/>.

        PROGRAM rte   !watroute
 
!***********************************************************************
!       copyright (c) by Nick Kouwen 1987-2007
!***********************************************************************

!           the use of the main program is only to dimension the
!           variables and to decide a few options.
!
!           note that the dimensions of a variable must be the
!           same in all parts of the programs. eg: if qsyn is
!           dimensioned to (1,8,123) in the main calling program,
!           then is it is dimensioned qsyn(ls,js,ih), ls must be
!           set =1, js=8, & ih=123
!
!           the variables qi1 and qi2 have to be dimensioned to
!           the value of na while all the other variables can be
!           dimensioned to naa.  this is to allow the water to
!           run into something at the outlet.

!     rev. 9.4.01  Apr.  17/07  - NK: added deltat_report for gridflow.r2c

      use area_watflood

      USE EF_Module

      implicit none

!     SAVES THE LOCAL VARIABLES FROM ONE RUN TO NEXT
      SAVE

      CHARACTER(79)   :: comment
      CHARACTER*14    :: date
      CHARACTER(2)    :: aa1
      CHARACTER(1)    :: linetype
      integer*2 result1
      CHARACTER*1028 ApiPacket
      CHARACTER*128   QSTR
      INTEGER*2      status, RESP, len
      integer    ::  io_err,sys_err,stat,unit,cond,nj,iallocate,
     *               ios,nhr,nhf,linecount,ideallocate,iendarg,ix,
     *               file_number,i,j,ii,icase
      real*4     ::  conv,scale,smc5(16),cintv,e1
      character(20) :: junk
      CHARACTER(10) :: time
      CHARACTER(8)  :: cday
      INTEGER(2) status1
      CHARACTER(1) buf,stopflg,ensimopenflg
      logical       :: exists
      CHARACTER(1)  :: smok 
      INTEGER    :: iallcnt,icnt(5),ndir(5),nchr,
     *              igrdshft,iyshiftmin,iyshiftmax,
     *              jxshiftmin,jxshiftmax,ishift,jshift,inum,jnum,
     *              l,iw1,iw2,iv,iflg
      REAL(4)    :: errold(5),err(5),chng(5),best(5)
      REAL(4)    :: optlow,ddtemp,cc1,cc2,crit,best1
      real(4)    :: optlast
      integer*2  :: ntest
!vfo
      integer iargc
      EXTERNAL iargc

      DATA ntest/43814/qstr/'optionsss'/nchr/9/
      DATA iallcnt/0/

      program_name='rte'

        if(iargc().lt.1)then
          buf='0'
        else  
          CALL GETARG(1, buf)
        endif  

       if(buf.eq.'0')then
         iopt=0
       elseif(buf.eq.'1')then
         iopt=1
         stopflg='y'
       elseif(buf.eq.'2')then
         iopt=2
       elseif(buf.eq.'3')then
         iopt=2
       elseif(buf.eq.'4')then
         iopt=2
       elseif(buf.eq.'9')then
!       rev. 9.1.63  Sep.  29/04  - NK: Added iopt_start as an arg for quick filecheck
        iopt_start=99
       endif

!     THIS SECTION GETS THE COMPUTER'S DATE AND TIME
!     THIS INFO USED TO COME FROM HEADER.FI, HOWEVER F90 INTRINSICS
!     AND CALLS ARE DIFFERENT AND THEREFORE IT NEEDED TO BE MODFIED.
c     call date_time(cday,time)

!     INPUT FILES ARE UNITS 30-50
!     THESE ARE OPENED MOSTLY IN SUB.F BECAUSE INPUT FILES
!     HAVE TO BE OPENED AND CLOSED FOR EACH MONTH IN THE ID LOOP

!     THE RESERVOIR INPUT FILE WAS ORIGINALLY 49 BUT THIS
!     WAS CHANGED WHEN TODD NEFF's EVAPORATION WAS ADDED

!!!!!!!!!!!!!!!!!!!!!!!!!!
!     ALSO USED IN SHED: UNIT 9 FOR FLN(6)

      translateflg='n'  ! used on translate.for if 'y'
      ittoflg=0
      ssmc_firstpass='y'
      flgevp2=-1.0
      ensimopenflg='n'

!     TS - ALLOCATION FOR AREA12A ARRAYS
      allocate(fln(999),filename(999),outfln(999),stat=iAllocate)
      if (iAllocate.ne.0) STOP 
     *    'Warning: error with allocation of area12a arrays in spl9' 

! SET DEFAULT FILENAMES FOR OUTPUT FILES:
!     these names may be replaced with the outfiles.txt file in the working
!     directory to send the files to a designated place for output.
!     A default outfiles.new file is created in the working directory each 
!     time this program is run.

      do i=51,100
	  filename(i)='..'
      end do

!vfo
!       filename(51)='../simout/rte.txt'      !program information
       filename(51)='rte.txt'      !program information
!       filename(53)='../simout/res.txt'      !reservoir data
       filename(53)='res.txt'      !reservoir data
!       filename(55)='../simout/route.txt'      !routing data
       filename(55)='route.txt'      !routing data
!       filename(60)='../simout/spl.csv'      !paired observed/computed
       filename(60)='spl_rpn.csv'      !paired observed/computed 
!       filename(68)='../simout/wetland.csv'  !wetland info
       filename(68)='wetland.csv'  !wetland info
!       filename(72)='../simout/gridflow.r2c' 
       filename(72)='gridflow.r2c' 
       fln(72)=filename(72)                  ! write_r2c used fln()
!       filename(73)='../simout/resin.txt'    !lake inflow obs/computed
       filename(73)='resin.txt'    !lake inflow obs/computed
!       filename(80)='../simout/lake_sd.csv'  !  reservoir storage output
       filename(80)='lake_sd.csv'  !  reservoir storage output
!dch
       filename(81)='net_lake_inflow.csv'
       filename(82)='qi_sup.txt'
       filename(83)='qi_mhg.txt'
       filename(84)='qi_stc.txt'
       filename(85)='qi_eri.txt'
       filename(86)='qi_ont.txt'
       filename(87)='moy_qi.txt'
       filename(92)='qr_sup.txt'
       filename(93)='qr_mhg.txt'
       filename(94)='qr_stc.txt'
       filename(95)='qr_eri.txt'
       filename(96)='qr_ont.txt'
       filename(97)='moy_qr.txt'
       filename(98)='rte_info.txt'
       filename(99)='scratch5'                       ! reserved as scratch file
       filename(100)='scratch6'                      ! not used

 ! WRITE A NEW OUTFILES.TXT FILE THAT CAN BE MODIFIED BY THE USER:
      open(unit=99,file='outfiles_rte.new',form='formatted',
     *     status='unknown',iostat=ios)
      if(ios.eq.0)then
        write(99,99002)(filename(i),i=51,100)
        close(unit=99,status='keep')
      else
	print*,' error opening outfiles.new'
	print*,' new file not written'
	print*
      endif

! THIS INCLUDE OPENS THE SIMOUT FILES AND WILL BE DIFFERENT FOR UNIX 
! OR DOS\WINDOWS

      iendarg=0

!     id is used as a flag. when id=0 openfiles:
      id=1              !not used like this now  nk Apr. 8/03
      ni=1

! DURING EXECUTION, THE 'STOP' COMMAND CAN BE MADE FROM ANOTHER DOS
! WINDOW. IT WILL CHECK AT THE END OF EACH EVENT. THIS WILL CLOSE
! ALL FILES PROPERLY AS OPPOSED TO THE PROBLEMS WITH A CTRL/BRK CRASH
      open(unit=99,file='stop.txt',form='formatted',status='unknown',
     *               iostat=ios)
      if(ios.eq.0)then
        write(99,99001)
        close(unit=99,status='keep')
      else
	print*,' error opening stop.txt'
	print*,' new file not written'
	print*
      endif

      ioflg=0
! ioflg IS THE NUMBER OF OUTPUT FILES LISTED IN OUTFILES.TXT
! so it's value will be changed then

! OPEN FILE FOR ALL SPL ERROR MESSAGES:
      if(ioflg.gt.1)then
        filename(98)=outfln(98)
      endif
   
      open(unit=98,file=filename(98),status='unknown',iostat=ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if(ios.ne.0)call io_warning(98,filename(98),ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      call date_and_time(cday,time)
      write(98,6016)time(1:2),time(3:4),time(5:6)
      write(98,6017)cday(1:4),cday(5:6),cday(7:8)
      write(98,5005)

! THE OUTFILES.TXT FILE READS THE NAMES OF ALL THE OUTPUT FILES

      INQUIRE(FILE='outfiles_rte.txt',EXIST=exists)
      if(exists)then
        open(unit=99,file='outfiles_rte.txt',
     *               status='old',iostat=ios)
        if(ios.eq.0)then
          print*
!         AN OUTFILES.TXT FILE EXISTS AND WE'LL READ IT:
          print*,'reading outfile names'
          print*
          write(51,*)
          write(51,*)'Outfile names from fln: outfiles_rte.txt' 
          do i=51,100
            read(99,5001,iostat=ios)outfln(i)
            write(51,5001)outfln(i)
            if(ios.ne.0)then 
              print*,'Problems on unit 99'
              print*,'Warning: error reading file name outfiles.txt'
              print*,'possible cause: existing file is read-only'
              print*,'or end of file is reached - need 50 lines'
              print*,'iostat code =',ios
              print*,'Got as far as line ',i-50
              STOP 'program aborted in spl.for @ 345'
            endif
            ioflg=i
          end do
          close(unit=99)
!vfo we define fln(72) here
          fln(72)=outfln(72)               ! write_r2c used fln()
        else
          print*,'Error opening outfiles.txt'
          print*,'Continuing using default output files'
        endif 
      else
       print*,'outfiles.txt file not found, defaults used'
       print*
      endif
      
!vfo fln(72)=outfln(72)                  ! write_r2c used fln()

      i=51
      if(ioflg.gt.1)then
        filename(i)=outfln(i)
      endif

      open(unit=i,file=filename(i),access='sequential',
     *    recl=2096,status='unknown',iostat=ios)
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if(ios.ne.0)call io_warning(i,filename(i),ios)
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      call date_and_time(cday,time)
      write(51,6016)time(1:2),time(3:4),time(5:6)
      write(51,6017)cday(1:4),cday(5:6),cday(7:8)
      write(51,*)
      write(51,5003)i,filename(i)

      write(51,5002)
      write(51,1030)(i,i,filename(i),i=51,100)

      fln(99)='event/event.evt'

      if(iopt.eq.2)print*, 'In spl: 1 - before rdevt call'

!**********************************************************************
      call rdevt(date,conv,scale,smc5,nhr,nhf)
!**********************************************************************


! ORIGINAL FILEIO.FI SECTION - STARTS HERE: 

!   LEAVE FILEIO HERE BECAUSE rdevtA READS FILE NAMES THAT HAVE TO BE OPENED

!   Rev. 7.78 modified for error checking - Sept.29/96  AC flight 
!   Rev  7.9  modified to open files for evaporation output
!   Rev. 8.3  - May.  22/97 -     added the simout/outfiles capability

!   THIS IS TO OPEN AND READ THE OPTIONAL OUTFILES.TXT FILE THAT SETS
!   THE LOCATIONS OF THE OUTPUT FILES.  IF FILE DOES NOT EXIST, DEFAULT
!   NAMES WILL BE USED.

      i=53
        if(ioflg.gt.1)then
          filename(i)=outfln(i)
        endif
        open(unit=i,file=filename(i),access='sequential',
     *             status='unknown',iostat=ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if(ios.ne.0)call io_warning(i,filename(i),ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        write(51,5003)i,filename(i)

      i=55
        if(ioflg.gt.1)then
          filename(i)=outfln(i)
        endif
        open(unit=i,file=filename(i),access='sequential',
     *             status='unknown',iostat=ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if(ios.ne.0)call io_warning(i,filename(i),ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        write(51,5003)i,filename(i)

      i=60
        if(ioflg.gt.1)then
          filename(i)=outfln(i)
        endif
        inquire(file=filename(i),exist=exists)
        if (.not. exists) then
           open(unit=i,file=filename(i),status='new',iostat=ios)
           first_run = .true.
        else
           open(unit=i,file=filename(i),position='append',
     *          status='old',iostat=ios)
        end if
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if(ios.ne.0)call io_warning(i,filename(i),ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        write(51,5003)i,filename(i)

!     File 65 (i=65) 'watflood.wfo' is opened in wfocode.for
!     File 66 (i=65) 'error.xyz' is opened in lst.for at end of run
!     File 67 (i=65) 'error.r2s' is opened in lst.for at end of run
      if(ioflg.gt.1)then
        i=65
        filename(i)=outfln(i)
      endif

!     TS: CHANGED MAX LIMIT TO INCLUDE TRACER FILES + EVAPSEP FILE (APR 5/06)

      i=73
        if(ioflg.gt.1)then
          filename(i)=outfln(i)
        endif
        open(unit=i,file=filename(i),access='sequential',
     *             status='unknown',iostat=ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if(ios.ne.0)call io_warning(i,filename(i),ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        write(51,5003)i,filename(i)

      i=80
        if(ioflg.gt.1)then
          filename(i)=outfln(i)
        endif
        open(unit=i,file=filename(i),access='sequential',
     *             status='unknown',iostat=ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if(ios.ne.0)call io_warning(i,filename(i),ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        write(51,5003)i,filename(i)

      i=81
        if(ioflg.gt.1)then
          filename(i)=outfln(i)
        endif
        open(unit=i,file=filename(i),access='sequential',
     *             status='unknown',iostat=ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if(ios.ne.0)call io_warning(i,filename(i),ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        write(51,5003)i,filename(i)

      i=82
        if(ioflg.gt.1)then
          filename(i)=outfln(i)
        endif
        open(unit=i,file=filename(i),access='sequential',
     *             status='unknown',iostat=ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if(ios.ne.0)call io_warning(i,filename(i),ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        write(51,5003)i,filename(i)

      i=83
        if(ioflg.gt.1)then
          filename(i)=outfln(i)
        endif
        open(unit=i,file=filename(i),access='sequential',
     *             status='unknown',iostat=ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if(ios.ne.0)call io_warning(i,filename(i),ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        write(51,5003)i,filename(i)

      i=84
        if(ioflg.gt.1)then
          filename(i)=outfln(i)
        endif
        open(unit=i,file=filename(i),access='sequential',
     *             status='unknown',iostat=ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if(ios.ne.0)call io_warning(i,filename(i),ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        write(51,5003)i,filename(i)

      i=85
        if(ioflg.gt.1)then
          filename(i)=outfln(i)
        endif
        open(unit=i,file=filename(i),access='sequential',
     *             status='unknown',iostat=ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if(ios.ne.0)call io_warning(i,filename(i),ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        write(51,5003)i,filename(i)

      i=86
        if(ioflg.gt.1)then
          filename(i)=outfln(i)
        endif
        open(unit=i,file=filename(i),access='sequential',
     *             status='unknown',iostat=ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if(ios.ne.0)call io_warning(i,filename(i),ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        write(51,5003)i,filename(i)

      i=87
        if(ioflg.gt.1)then
          filename(i)=outfln(i)
        endif
        open(unit=i,file=filename(i),access='sequential',
     *             status='unknown',iostat=ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if(ios.ne.0)call io_warning(i,filename(i),ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        write(51,5003)i,filename(i)

      i=92
        if(ioflg.gt.1)then
          filename(i)=outfln(i)
        endif
        open(unit=i,file=filename(i),access='sequential',
     *             status='unknown',iostat=ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if(ios.ne.0)call io_warning(i,filename(i),ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        write(51,5003)i,filename(i)

      i=93
        if(ioflg.gt.1)then
          filename(i)=outfln(i)
        endif
        open(unit=i,file=filename(i),access='sequential',
     *             status='unknown',iostat=ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if(ios.ne.0)call io_warning(i,filename(i),ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        write(51,5003)i,filename(i)

      i=94
        if(ioflg.gt.1)then
          filename(i)=outfln(i)
        endif
        open(unit=i,file=filename(i),access='sequential',
     *             status='unknown',iostat=ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if(ios.ne.0)call io_warning(i,filename(i),ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        write(51,5003)i,filename(i)

      i=95
        if(ioflg.gt.1)then
          filename(i)=outfln(i)
        endif
        open(unit=i,file=filename(i),access='sequential',
     *             status='unknown',iostat=ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if(ios.ne.0)call io_warning(i,filename(i),ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        write(51,5003)i,filename(i)

      i=96
        if(ioflg.gt.1)then
          filename(i)=outfln(i)
        endif
        open(unit=i,file=filename(i),access='sequential',
     *             status='unknown',iostat=ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if(ios.ne.0)call io_warning(i,filename(i),ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        write(51,5003)i,filename(i)

      i=97
        if(ioflg.gt.1)then
          filename(i)=outfln(i)
        endif
        open(unit=i,file=filename(i),access='sequential',
     *             status='unknown',iostat=ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if(ios.ne.0)call io_warning(i,filename(i),ios)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        write(51,5003)i,filename(i)

      if(iopt.eq.2)print*, ' In spl -1001'

      write(51,6015)
     *  time(1:2),time(3:4),time(5:6),cday(1:4),cday(5:6),cday(7:8)

99905 write(51,1000)fln(10),fln(1)
      write(53,1000)fln(10),fln(1)

      if(iopt.eq.2)print*, ' In spl - 1183'
 
!     SHED READS IN ALL THE WATERSHED DATA, FROM SEPERATE PROGRAM:
 
      if(iopt.eq.2)print*, ' In spl - before call shed'

!     read the shed file: bsnm_shd.r2c
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	call read_shed_ef(31,1)	
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!     read the parameter file:  bsnm_ch_par.r2c
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	call read_shed_ef(271,41)	
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      if(iopt.eq.2)print*, ' In spl - before allocate'

        allocate(qrgrid(ycount+10,xcount+10),stat=iAllocate)
        if(iAllocate.ne.0) STOP
     *   'Error with allocation of area16a arrays in spl9'

!       Initialize these values because many are outside grid
!       and would otherwise be undifined.  nk June 11/03
        do i=1,ycount+10
          do j=1,xcount+10
            qrgrid(i,j)=0.0
          end do
        end do

        allocate(nhyd(ycount,xcount),stat=iAllocate)
        if(iAllocate.ne.0) STOP
     *   'Error with allocation of area6a arrays in spl9'

        allocate(p(ycount,xcount),stat=iAllocate)
        if(iAllocate.ne.0) STOP
     *   'Error with allocation of p() array in spl9'

      mo=mo1
      numa=0

!     WARNING: DON'T COMBINE NEXT SEGMENT WITH PREVIOUS SEGMENT 
!     BECAUSE IGRDSHFT CAN BE SET = 0 IN THE FILE (i.e. NO IOS ERROR)

      maxn=1

      if(iopt.eq.2)print*, ' In options: 4 - before call sub'

!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      call sub(e1,smc5,scale,icase,smok,optlow,igrdshft)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      nnn=nnn+1

      if(iopt.eq.2)print*,'in spl9 @1'

      stop ' normal ending'

! FORMATS

 1000 format(' ',2(' ',a30))
 1030 format(' ','Unit no. =',i3,' file no',i3,' = ',a30)
 5001 format(a30)
 5002 format(/' output files')
 5003 format(' opened unit'i5,' file name ',a30)
 5005 format(' Files opened:')
 6015 format(' runtime  ',a2,':',a2,':',a2,2x,a4,'-',a2,'-',a2)
 6016 format('  runtime    ',2(a2,':'),a2)
 6017 format('  rundate  ',a4,'-',a2,'-',a2)
99001 format('  0.0')
99002 format(a30)

      END PROGRAM rte     
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      subroutine io_warning(unit_number,file_name,ios)

      use area_watflood

      integer  :: unit_number,ios
      character*30  :: file_name

      print*,'Problems on unit',unit_number
      print*,'Warning:'
      print*,' error in opening file name  ',file_name
      print*,'possible cause: existing file is read-only'
      print*,'or folder does not exist    <<<'
      print*,'or file does not exist      <<<'
      print*,'or DISK does not exist      <<<'
      Print*
      print*,'iostat code =',ios
      print*
 
      STOP 'program aborted in io-warning.for (spl)  @1054'


      end subroutine io_warning

