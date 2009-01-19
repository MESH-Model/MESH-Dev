      SUBROUTINE rdpar(iprtflg,ix,e1)

!***********************************************************************
!  version numbers are added for version 7.0 and later. this allows
!  parameter files to backward compatible when more parameters are 
!  added to the file in the future. in version 7.0 some headers are 
!  added as well for readability.
!
!   REV. 7.5 seperate snow covered and bare ground - Jul/95
!   REV. 7.75 - May.  27/96 -  added ak2fs in rdpar & runof5
!
!   REV. 8.32 - June  13/97 -  bypassed non-flagged parameters in OPT
!   REV. 8.41 - July  21/97 -  added tipm to the optimization table
!   REV. 8.62 - Dec.  30/97 -  fixed param s/r comb'd et & par flgs
!   REV. 8.74 - Mar.  31/98 -  reinvented fs stuff in opt
!
!   REV. 9.00 - March 2000  -  TS: CONVERSION TO FORTRAN 90
!   REV. 9.03 - Nov   2000  -  TS: ADDED WATFLOOD SWAMP ROUTING 
!   rev. 9.07    Mar.  14/01  - fixed use of opt par's  for numa=0  
!   rev. 9.08.01 Apr.   3/01  - check wetland designation in rdpar
!   rev. 0.1.04  Oct.   4/01  - added A7 for weighting old/new sca in melt
!   rev. 0.1.05  Oct.   4/01  - new format parameter file
!   rev  9.1.20  Jun.  25/02  - Added A10 as the power on the UZ discharge function
!   rev  9.1.25  Sep.  11/02  - Added A11 as bare ground equiv. vegn height  
!   rev. 9.1.37  Mar.  22/03  - Option to turn off leakage by setting LZF < 0.0
!   rev. 9.1.40  Apr.  24/03  - Min time step A6 read in strfw over rides the A6 from the par file
!   rev. 9.1.76  Mar.  09/05  - NK: separated glacier parameters in par file
!   rev. 9.2.09  Sep.  11/05  - NK: removed write_par.for from rdpar.for
!   rev. 9.2.11  Sep.  11/05  - NK: added Manning's n  r1n & r2n
!   rev. 9.2.16  Oct.  10/05  - NK: Fixed bug for widep in rdpar
!   rev. 9.2.35  Mar.  22/06  - NK: Glacier flow bypasses wetlands
!   rev. 9.2.37  Mar.  31/06  - NK: Removed impervious area as special class
!   rev. 9.4.07  May.  15/07  - NK: converted opt to gridded routing parameters
!   rev.  check
!
!
!  iprtflg - if eq 2 write a new parameter file after optimization
!  r1      - the roughness of the flood plain
!  r2      - the roughness of the largest part of the channel
!  r1      - factor for raising r2 ie if r1=2 then f.p. roughness
!            is 2 times channel roughness
!  zz      - is an exponent in calculating the mannings  n
!  h       - crop height
!  dds_flag- flag to run the DDS optimization =0 no dds opt  =1 dds opt
!  itrace  - tracer flag 100=GW, 4=3-comp (SW, IF, GW)
!             or 5=6-comp (3-comp's + melt fractions).
!  e1       - void ratio
!  ak      - permeability
!  sm      -soil moisture(average for month)
!  nbsn    - no of basins with different r2 to be optimized
!          - must be smaller than nrvr
!  ntypeo  - no of classes to be optimized max=4
!  iiout   - melt debug function class selection number 
!  a1 .. a12 parameters variously used mostly in runoff
!  a7      - weighting factor for old vs. new sca value default=.90
!  a8      - time offset to check for min temp on rdtemp
!  a9      - heat deficit to swe ratio  default=0.333
!  a10     - power on interflow discharge function default = 1.0
!  a11     - equivalent vegetation height for bare ground evaporation
!  a12     - min precip rate for smearing used in rain.for default=0.0
!
!***********************************************************************

c      USE area1
c      USE area2
c      USE area3
c      USE area4
c      USE area8
c      USE area9
c      use area12
c      USE areaet
c      USE areamelt
c      USE areaopts
c      USE areawet
c	USE areatrc

      use area_watflood
	implicit none

      real*4, dimension(:),   allocatable :: ajunk
      real*4, dimension(:),   allocatable :: par_temp


      CHARACTER(10) :: time
      CHARACTER(8)  :: cday
      CHARACTER(128):: qstr
      CHARACTER(60) :: junk
      CHARACTER(30) :: filename1
      CHARACTER(1)  :: errorflg,answer,linetype,tempflg,firstpass
      LOGICAL       :: flzflg,pwrflg,R1nflg,R2nflg,mndrflg,
     *                 aa2flg,aa3flg,aa4flg,widepflg
      INTEGER(kind=2) :: result1
      INTEGER       :: nrvr1,ntest,nchr,iprtflg,linecount,i,
     *                 ios,iverflg,ix,iallocate,ijunko,ii,j,n
      real*4        :: hmax,e1,kcondflg,xxx1

      DATA ntest/-10906/qstr/'param'/nchr/5/
      data firstpass/'y'/

!     SAVES THE LOCAL VARIABLES FROM ONE RUN TO NEXT
      SAVE

      i=max(nrvr,ntype)
      allocate(ajunk(i),stat=iAllocate)
      if(iAllocate.ne.0) STOP 'Error with allocation of ajunk in rdpar'

      if(firstpass.eq.'y')then

!     TS - ALLOCATIONS OF AREA4A ARRAYS
!     ntype for the number of land cover classes
!     nrivertype for the number of channel or basin types
!     moved here from spl9  nk 06/07/00
!     then moved from rdpar nk 28/12/04
!     moved back from rdshed 27/07/06 because needed for bsn.for  nk


c      print*,'nrvr=',nrvr



!     these are allocated in read_shed_ef
c       allocate(s(imax,jmax),
c     *    xxx(na),yyy(na),da(na),bnkfll(na),slope(na),elev(na),
c     *    rl(na),ibn(na),sl1(na),irough(na),ichnl(na),next(na),
c     *    ireach(na),frac(na),aclass(na,ntype+1),glacier_flag(na),
c     *    flz(na),pwr(na),r1n(na),r2n(na),mndr(na),aa2(na),aa3(na),
c     *    aa4(na),widep(na),flz2(na),
c     *    stat=iAllocate)


c!     used in bsn only - added Jan. 29/07  nk
c      if(.not.allocated(ibn))then
c	print*,'allocating ibn with',na
c	  allocate(ibn(na),stat=iAllocate)
c        if(iAllocate.ne.0) STOP
c     *   'Warning: error with allocation of ibn array in rdpar'
c	 print*,'allocated ibn'
c	endif
c
c      pause


c      allocate(mndr(nrvr),         
c     *r1(nrvr),r2(nrvr),r2low(nrvr),r2hgh(nrvr),r2dlt(nrvr),
c     *aa2(nrvr),aa3(nrvr),aa4(nrvr),rivtype(nrvr),
c     *theta(nrvr),thetadlt(nrvr),thetalow(nrvr),thetahgh(nrvr),
c     *widep(nrvr),widepdlt(nrvr),wideplow(nrvr),widephgh(nrvr),
c     *kcond(nrvr),kconddlt(nrvr),kcondlow(nrvr),kcondhgh(nrvr),
c     *flz(nrvr),flz2(nrvr),flzlow(nrvr),flzhgh(nrvr),flzdlt(nrvr),
c     *pwr(nrvr),pwr2(nrvr),pwrlow(nrvr),pwrhgh(nrvr),pwrdlt(nrvr),
c     *stat=iAllocate)
c      if(iAllocate.ne.0) STOP
c     *   'Error with allocation of area4a arrays in rdpar'


!     TS - ALLOCATIONS OF AREA4A ARRAYS
!     ntype for the number of land cover classes
!     nbsn for the number of channel or basin types
!     moved here from rdshed 27/07/06 because needed for bsn.for  nk
      allocate(
     *ds(ntype+1),dsfs(ntype+1),chnl(ntype+1),
     *r3(ntype+1),r4(ntype+1),r3fs(ntype+1),rec(ntype+1),
     *ak(ntype+1),akfs(ntype+1),
     *r3low(ntype+1),r3fslow(ntype+1),reclow(ntype+1),
     *aklow(ntype+1),akfslow(ntype+1),ak2fslow(ntype+1),
     *r3hgh(ntype+1),r3fshgh(ntype+1),rechgh(ntype+1),akhgh(ntype+1),
     *akfshgh(ntype+1),ak2fshgh(ntype+1),r3dlt(ntype+1),
     *r3fsdlt(ntype+1),recdlt(ntype+1),akdlt(ntype+1),akfsdlt(ntype+1),
     *ak2fsdlt(ntype+1),retn(ntype+1),ak2(ntype+1),
     *retnlow(ntype+1),ak2low(ntype+1),
     *retnhgh(ntype+1),ak2hgh(ntype+1),
     *retndlt(ntype+1),ak2dlt(ntype+1),
     *retfs(ntype+1),ak2fs(ntype+1),fpet(ntype+1),
     *fpetdlt(ntype+1),fpetlow(ntype+1),fpethgh(ntype+1),ftall(ntype+1),
     *ftalldlt(ntype+1),ftalllow(ntype+1),ftallhgh(ntype+1),
     *nclass(ntype+1),
     *pot(ntype+1),potfs(ntype+1),   ! moved from read_shed_ef may 15/07 nk
     *iiclass(ntype*2),h(12,ntype+1),fpetmo(12,ntype+1),stat=iAllocate)
      if(iAllocate.ne.0) STOP
     *   'Warning: error with allocation of area4a arrays in spl9'



!     TS - ALLOCATIONS OF AREAOPTSA ARRAYS
      allocate(fmdlt(ntype),fmlow(ntype),fmhgh(ntype),fmndlt(ntype),
     *fmnlow(ntype),fmnhgh(ntype),uajdlt(ntype),uajlow(ntype),
     *uajhgh(ntype),mbsdlt(ntype),mbslow(ntype),mbshgh(ntype),
     *basdlt(ntype),baslow(ntype),bashgh(ntype),stat=iAllocate)
      if(iAllocate.ne.0) STOP
     *   'Warning: error with allocation of areaoptsa arrays in spl9'





!     TS - ALLOCATIONS OF AREAMELTA ARRAYS (PARTIAL)
!     SNW,DSN,TTEMP,TMX,TMN,EL ALLOCATED IN SHEDA.FOR
!     SDCD,SDCSCA ALLOCATED IN RDSDCA.FOR
!     allocate(snowc(na,ntype+1),dsnow(na),tempv(na),tempvmin(na),
!     moved here from rdshed 27/07/06 because needed for bsn.for  nk
      allocate(base(ntype+1),fm(ntype+1),fmn(ntype+1),
     *whcl(ntype+1),tipm(ntype+1),uadj(ntype+1),
     *rho(ntype+1),stat=iAllocate)
      if(iAllocate.ne.0) STOP
     *   'Warning: error with allocation of areamelta arrays in spl9'


!     TS - ALLOCATIONS OF AREAETA ARRAYS (PARTIAL)
!     RAD ALLOCATED IN SHEDA.FOR
!     TS - ADDED ALLOCATIONS FOR EVAP-SEPARATION PARAMS (22/03/06)
!     TS: CHANGED ALLOCATIONS OF alb,pet, evap TO NTYPE+1 (27/03/06)
!     rev. 9.1.80  Mar.  31/05  - NK: added sublimation   (sublim)
!     moved here from rdshed 27/07/06 because needed for bsn.for  nk
      allocate(evap(ntype+1,12),sublim_factor(ntype+1),flint(ntype+1),
     *diff(12),hu(12),ffcap(ntype+1),fcap(ntype+1),
     *spore(ntype+1),alb(ntype+1),pres(12),stat=iAllocate)
 !    *rh(na),stat=iAllocate)
      if(iAllocate.ne.0) STOP
     *   'Warning: error with allocation of areaeta arrays in spl9'

c      print*,'Allocations done in rdpar',ntype,nrvr

      endif  !firtspass


!     ADDED THIS FROM TODD'S ETPAR.FOR - FRANK S: NOV/97 
      alamb=2.478
      den=999.
      alpha=1.35

! READ SNOW COVER PARAMETERS:   
!  - CALLED ON FIRST TIME STEP FOR SIMULATION
!  - CALLED TO WRITE TO newsnow.par DURING OPTIMIZATION 
!    IF OPTIMIZATION PARAMETERS ARE UPDATED
!               -- first the snow output flags
!               -- melt factors
!               -- now mbase
!               -- negative melt factors
!               -- wind function
!               -- ati decay
!               -- conversion snow density depth to we
!               -- liquid water holding capacity
!               -- daily ground melt
!---------------------------------------------------------


        title( 7) ='dsfs '
        title(10) ='akfs '
        title(18) ='r3fs '
        title(88) ='a10   '
        title(99) ='a9   '
        title(100)='a11  '

!     REWIND B/C SOME DATA WAS READ EARLIER
      rewind 32

!     PASS OVER THE COMMENT LINES ON THE PARAMETER FILE:
!     SEE SAME CODE IN SPL9.FOR
      linecount=0
      do i=1,20
        read(32,3010,iostat=ios)linetype,notes(i)
        if(ios.ne.0)then
          print*,' Problem reading comment lines'
        endif
        if(linetype.eq.'#')linecount=linecount+1
      end do

      rewind 32
      do i=1,linecount
        read(32,3010,iostat=ios)
        if(ios.ne.0)then
          print*,' Problem reading comment lines'
        endif
      end do

!     if we can read the next line, we have the old format  
      if(linecount.eq.0)rewind 32
      read(32,3000,iostat=ios)title(1),iopt,itype,numa,nper,kc,maxn,ver
      write(98,3000)title(1),iopt,itype,numa,nper,kc,maxn,ver

      if(iopt_start.eq.99)then
        iopt=99
        print*,'iopt temporarily changed to 99 via argument'
        print*
      endif

      if(ios.eq.0)then
        iverflg=0
        if(ver.le.9.1)iverflg=1
        read(32,1000,iostat=ios)title(2),e1,ix,iiout
        if(ios.ne.0)then
          print*,' Old format par file found but'
          print*,' problems reading data line 2'
          print*
          stop ' Program aborted at 151 in rdpar'
        endif
        if(iiout.le.0.or.iiout.ge.7)then 
          write(51,5007)iiout
          iiout=1
        endif
        read(32,5000,iostat=ios)title(3),ntypeo,nbsn
        if(ios.ne.0)then
          print*,' Old format par file found but'
          print*,' problems reading data line 3'
          print*
          stop ' Program aborted at 152 in rdpar'
        endif
        if(nbsn.gt.nrvr)then
          print*,'The number of river par sets to be optimized is '
          print*,'larger than the number of rivers.'
          print*,'No of river classes from the .shd file = ',nrvr
          print*,'No of river classes from the par file  = ',nbsn
          print*,'Please change the par file'
          print*,'Program will proceed with nbsn set to nrvr'
          nbsn=nrvr
          print*
          pause 'Hit enter to continue  --  hit Ctrl C to abort'
        endif

        title(71)='ver'
        title(72)='iopt'
        title(73)='itype'
        title(74)='numa'
        title(75)='nper'
        title(76)='kc'
        title(77)='maxn'
        title(78)='ddsflg'
        title(79)='itrace'
        title(80)='iiout'
        title(81)='typeo'
        title(82)='nbsn'
        title(83)='mndr'
        title(84)='aa2'
        title(85)='aa3'
        title(86)='aa4'
        title(87)='theta'
        title(88)='widep'
        title(89)='kcond'
        title(91)='mndr'
        title(92)='a2'
        title(93)='a3'
        title(94)='a4'
        title(95)='a5'
        title(96)='a6'
        title(97)='a7'
        title(98)='a8'
        title(99)='a9'
        title(67)='a10'
        title(68)='a11'
        title(69)='a12'
        notes(71)='parameter file version number'
        notes(72)='debug level'
        notes(73)=''
        notes(74)='optimization 0=no 1=yes'
        notes(75)='opt delta 0=absolute 1=ratio'
        notes(76)='no of times delta halved'
        notes(77)='max no of trials'
        notes(78)=''
        notes(79)=''
        notes(80)=''
        notes(81)='no of land classes optimized(part 2)'
        notes(82)='no of river classes optimized (part 2)'
        notes(91)=''
        notes(92)=''
        notes(93)=''
        notes(94)=''
        notes(95)='API coefficient'
        notes(96)='Minimum routing time step in seconds'
        notes(97)='weighting factor - old vs. new sca value'
        notes(98)='min temperature time offset'
        notes(99)='max heat deficit /swe ratio'
        notes(67)='exponent on uz discharce function'
        notes(68)='bare ground equivalent veg height for evap`n'
        notes(69)='min precip rate for smearing'
      else

!       new format file found

!!!!!!!!NOTE: void ratio e1 is dropped from the par file.
!             needs to be changed to use spore(ii) etv.
!             for now, use por = average(spore(ii))
!             done in runof6.for, see por=.......
        e1=-1.0     ! flag to use average in runof6.for

        backspace 32
        read(32,9807,iostat=ios)title(71),ver,notes(71)
        print*,title(71),ver,notes(71)
        if(ios.ne.0)then
          print*,'Parameter file ver 9.1 or greater'
          print*,'Error reading ver in the parameter file data'
          print*
          stop '@ ~227a in rdpar'
        endif

        if(ver.le.9.1)iverflg=1

        read(32,9805,iostat=ios)title(72),iopt,notes(72)
        if(ios.ne.0)then
          print*,'Parameter file ver 9.1 or greater'
          print*,'Error reading iopt in the parameter file data'
          print*
          stop '@ ~227b in rdpar'
        endif

        if(iopt_start.eq.99)then
          iopt=99
          print*,'iopt temporarily changed to 99 via argument'
          print*
        endif

        read(32,9805,iostat=ios)title(73),itype,notes(73)
        if(ios.ne.0)then
          print*,'Parameter file ver 9.1 or greater'
          print*,'Error reading itype in the parameter file data'
          print*
          stop '@ ~227c in rdpar'
        endif
        read(32,9805,iostat=ios)title(74),numa,notes(74)
        if(ios.ne.0)then
          print*,'Parameter file ver 9.1 or greater'
          print*,'Error reading numa in the parameter file data'
          print*
          stop '@ ~227d in rdpar'
        endif
        read(32,9805,iostat=ios)title(75),nper,notes(75)
        if(ios.ne.0)then
          print*,'Parameter file ver 9.1 or greater'
          print*,'Error reading nper in  the parameter file data'
          print*
          stop '@ ~227e in rdpar'
        endif
        read(32,9805,iostat=ios)title(76),kc,notes(76)
        if(ios.ne.0)then
          print*,'Parameter file ver 9.1 or greater'
          print*,'Error reading kc in  the parameter file data'
          print*
          stop '@ ~227f in rdpar'
        endif
        read(32,9805,iostat=ios)title(77),maxn,notes(77)
        if(ios.ne.0)then
          print*,'Parameter file ver 9.1 or greater'
          print*,'Error reading maxn in  the parameter file data'
          print*
          stop '@ ~227g in rdpar'
        endif
        read(32,9805,iostat=ios)title(78),dds_flag,notes(78)
        if(ios.ne.0)then
          print*,'Parameter file ver 9.1 or greater'
          print*,'Error reading dds_flag in the parameter file data'
          print*
          stop '@ ~227h in rdpar'
        endif
        read(32,9805,iostat=ios)title(79),itrace,notes(79)
        if(ios.ne.0)then
          print*,'Parameter file ver 9.1 or greater'
          print*,'Error reading itrace in the parameter file data'
          print*
          stop '@ ~227i in rdpar'
        endif
        read(32,9805,iostat=ios)title(80),iiout,notes(80)
        if(ios.ne.0)then
          print*,'Parameter file ver 9.1 or greater'
          print*,'Error reading iiout in the parameter file data'
          print*
          stop '@ ~227j in rdpar'
        endif
        read(32,9805,iostat=ios)title(81),ntypeo,notes(81)
        if(ios.ne.0)then
          print*,'Parameter file ver 9.1 or greater'
          print*,'Error reading ntypeo in the parameter file data'
          print*
          stop '@ ~227k in rdpar'
        endif
        read(32,9805,iostat=ios)title(82),nbsn,notes(82)
        if(ios.ne.0)then
          print*,'Parameter file ver 9.1 or greater'
          print*,'Error reading nbsn in  the parameter file data'
          print*
          stop '@ ~227l in rdpar'
        endif
        if(nbsn.gt.nrvr)then
          print*,'The number of river par sets to be optimized is '
          print*,'larger than the number of rivers.'
          print*,'No of river classes from the .shd file = ',nrvr
          print*,'No of river classes from the par file  = ',nbsn
          print*,'Please change the par file'
          print*,'Program will proceed with nbsn set to nrvr'
          nbsn=nrvr
          print*
          pause 'Hit enter to continue  --  hit Ctrl C to abort'
        endif

c        if(ios.ne.0)then
c          print*,'Parameter file ver 9.1 or greater'
c          print*,'Error in first 12 lines of the parameter file'
c          print*
c          stop '@ ~227 in rdpar'
c        endif
      endif


!     Note: r2n,theta,widep,kcond,flz & pwr are allocated in read_shed_ef
      if(numa.gt.0)then
        allocate(r2nlow(nrvr),r2nhgh(nrvr),r2ndlt(nrvr),        
     *         thetadlt(nrvr),thetalow(nrvr),thetahgh(nrvr),
     *         widepdlt(nrvr),wideplow(nrvr),widephgh(nrvr),
     *         kconddlt(nrvr),kcondlow(nrvr),kcondhgh(nrvr),
     *         flzlow(nrvr),flzhgh(nrvr),flzdlt(nrvr),
     *         pwrlow(nrvr),pwrhgh(nrvr),pwrdlt(nrvr),
     *  stat=iAllocate)
        if(iAllocate.ne.0) STOP
     *        'Error with allocation of area4 arrays in rdpar -1'
      endif

      allocate(flz_o(nrvr),pwr_o(nrvr),r1n_o(nrvr),        
     *         r2n_o(nrvr),mndr_o(nrvr),aa2_o(nrvr),
     *         aa3_o(nrvr),aa4_o(nrvr),theta_o(nrvr),
     *         widep_o(nrvr),kcond_o(nrvr),
     *  stat=iAllocate)
      if(iAllocate.ne.0) STOP
     *        'Error with allocation of area4 arrays in rdpar -2'



      if(iopt.eq.2)print*, 'param 2'

      if(ver.lt.9.10)then  !<<<<<<check version number in previous file


        read(32,1001,iostat=ios)title(4),a1,a2,a3,a4,a5  
        read(32,1001,iostat=ios)title(5),a6,a7,a8,a9,a10,a11,a12
        if(ios.ne.0)then
          print*,' Old format par file found but'
          print*,' problems reading data line 3 or 4'
          print*
          stop ' Program aborted at 152 in rdpar'
        endif
      else
        if(ios.ne.0)then
          print*,'Parameter file ver 9.1 or greater'
          print*,'Error reading mndr in  the parameter file data'
          print*
          stop '@ ~227l in rdpar'
        endif
        read(32,9806,iostat=ios)title(91),a1,notes(91) 
        if(ios.ne.0)then
          print*,'Parameter file ver 9.1 or greater'
          print*,'Error reading a1 in  the parameter file data'
          print*
          stop '@ ~227l in rdpar'
        endif
        read(32,9806,iostat=ios)title(92),a2,notes(92)  
        if(ios.ne.0)then
          print*,'Parameter file ver 9.1 or greater'
          print*,'Error reading a2 in  the parameter file data'
          print*
          stop '@ ~227l in rdpar'
        endif
        read(32,9806,iostat=ios)title(93),a3,notes(93)
        if(ios.ne.0)then
          print*,'Parameter file ver 9.1 or greater'
          print*,'Error reading a3 in  the parameter file data'
          print*
          stop '@ ~227l in rdpar'
        endif
        read(32,9806,iostat=ios)title(94),a4,notes(94)
        if(ios.ne.0)then
          print*,'Parameter file ver 9.1 or greater'
          print*,'Error reading a4 in  the parameter file data'
          print*
          stop '@ ~227l in rdpar'
        endif
        read(32,9806,iostat=ios)title(95),a5,notes(95)
        if(ios.ne.0)then
          print*,'Parameter file ver 9.1 or greater'
          print*,'Error reading a5 in  the parameter file data'
          print*
          stop '@ ~227l in rdpar'
        endif
        read(32,9806,iostat=ios)title(96),a6,notes(96)
        if(ios.ne.0)then
          print*,'Parameter file ver 9.1 or greater'
          print*,'Error reading a6 in  the parameter file data'
          print*
          stop '@ ~227l in rdpar'
        endif
        read(32,9806,iostat=ios)title(97),a7,notes(97)
        if(ios.ne.0)then
          print*,'Parameter file ver 9.1 or greater'
          print*,'Error reading a7 in  the parameter file data'
          print*
          stop '@ ~227l in rdpar'
        endif
        read(32,9806,iostat=ios)title(98),a8,notes(98)
        if(ios.ne.0)then
          print*,'Parameter file ver 9.1 or greater'
          print*,'Error reading a8 in  the parameter file data'
          print*
          stop '@ ~227l in rdpar'
        endif
        read(32,9806,iostat=ios)title(99),a9,notes(99)
        if(ios.ne.0)then
          print*,'Parameter file ver 9.1 or greater'
          print*,'Error reading a9 in  the parameter file data'
          print*
          stop '@ ~227l in rdpar'
        endif
        read(32,9806,iostat=ios)title(67),a10,notes(67)
        if(ios.ne.0)then
          print*,'Parameter file ver 9.1 or greater'
          print*,'Error reading a10 in  the parameter file data'
          print*
          stop '@ ~227l in rdpar'
        endif
        read(32,9806,iostat=ios)title(68),a11,notes(68)
        a11=amax1(0.01,a11)   ! bare ground equiv. veg height
        if(ios.ne.0)then
          print*,'Parameter file ver 9.1 or greater'
          print*,'Error reading a11 in  the parameter file data'
          print*
          stop '@ ~227l in rdpar'
        endif
        read(32,9806,iostat=ios)title(69),a12,notes(69)
        if(ios.ne.0)then
          print*,'Parameter file ver 9.1 or greater'
          print*,'Error reading a12 in  the parameter file data'
          print*
          stop '@ ~227l in rdpar'
        endif
      endif

      if(ios.ne.0)then
        print*,'Parameter file ver 9.1 or greater'
        print*,'Error in the a1 - a12 section of the parameter file'
        print*
        print*,title(91),a1,notes(91) 
        print*,title(92),a2,notes(92)  
        print*,title(93),a3,notes(93)
        print*,title(94),a4,notes(94)
        print*,title(95),a5,notes(95)
        print*,title(96),a6,notes(96)
        print*,title(97),a7,notes(97)
        print*,title(98),a8,notes(98)
        print*,title(99),a9,notes(99)
        print*,title(67),a10,notes(67)
        print*,title(68),a11,notes(68)
        print*,title(69),a12,notes(69)
        stop '@ ~256 in rdpar'
      endif

      if(iopt.eq.2)print*, 'in rdpar @ 581'

      tempflg='n'
!     a7 - weighting factor for old vs. new sca value default=.90
!     rev. 9.1.04  Oct. 12/01 nk
      if(a7.lt.0.50.or.a7.gt.0.99)then
        print*,' Value for weighting factor - old vs. new sca value'
        print*,' a7 not between 0.5-0.99in the par file'
        print*,' a7=0.50 assumed - matches spl8 results'
        write(98,9811)
        write(98,9812)
        write(98,9813)
        write(98,9814)
        write(98,9815)
        write(98,9811)
9811    format(' ')
9812    format(' WARNING - in rdpar')
9813    format('Value for weighting factor - old vs. new sca value')
9814    format('a7 not between 0.5-0.99in the par file')
9815    format('a7=0.50 assumed - matches spl8 results')
        a7=0.5
        tempflg='y'
      endif

      if(iopt.eq.2)print*, 'in rdpar @ 605'


!     rev. 9.03 jan 07/01 NK
      if(a12.le.0.0.and.smrflg.eq.'y')then
        print*,' Value for a12 not defined in the paramerter file'
        print*,' a12=0.001 assumed - matches previous results'
        print*,' Paused in rdpar after reading aNN values'
        print*
        tempflg='y'
      endif
      
      if(tempflg.eq.'y')then
        print*,' Hit enter to accept default & continue'
        pause ' Program paused in rdpar @ 480'
      endif
       
      tempflg='n'
      if(a9.gt.5.0.or.a9.lt.0.3)then
!       change range from 0.33-2.0 to 0.33-5.0 Oct. 20/03 NK
        print*
        print*,' a9 (heat deficit / swe ratio outside range 0.333 - 5.0'
        print*,' a9 = .333 assumed'
        print*,' If this default value is unsuitable, please'
        print*,' edit the bsnm.par file and enter proper value'
        print*,' between 0.333 and 5.0'
        print*
        a9=0.333
        write(51,5110)a9
        tempflg='y'
      endif

      if(iopt.eq.2)print*, 'in rdpar @ 637'

      if(a10.gt.3.0.or.a10.lt.0.5)then
        print*
        print*,' a10 uz discharge exponent outside range 0.5 - 3.0'
        print*,' a10 = 1.0 assumed'
        print*,' If this default value is unsuitable, please'
        print*,' edit the bsnm.par file and enter proper value'
        print*,' between 0.5 and 3.0'
        print*
        a10=1.0
        write(51,5111)a10
        tempflg='y'
      endif
      if(tempflg.eq.'y')then
        print*
        print*,' To continue with the default value(s) hit enter'
        print*,' To prevent this message from reoccurring, please enter'
        print*,' values for a9 and/or a10 in the parameter .par file'
        print*
        pause '  Program paused in rdpar @ 512'
      endif

      if(iopt.eq.2)print*, 'in rdpar @ 660'


!     REVISED TO CONSOLIDATE CLASSES
      if(ver.ge.8.99)then
!       read the column headers
        read(32,5009,iostat=ios)(rivtype(i),i=1,nrvr)
        if(ios.ne.0)then
          print*,'Error reading rivtype in the parameter file data'
          print*
          stop '@ ~571 in rdpar'
        endif

!       read the lowerzone function
        read(32,1001,iostat=ios)title(14),(flz_o(i),i=1,nrvr)
        if(ios.ne.0)then
          print*,'Error reading flz in the parameter file data'
          print*
          stop '@ ~578 in rdpar'
        endif
!       findout how many river classes there are:
        do i=1,nrvr
          if(ajunk(i).gt.0.0)then
            nrvr1=i
          endif
        end do

        if(iopt.eq.2)print*, 'in rdpar @ 686'

!       check no of rivers is ok
       read(32,1001,iostat=ios)title(15),(pwr_o(i),i=1,nrvr)
        if(ios.ne.0)then
          print*,'Error reading pwr in the parameter file data'
          print*
          stop '@ ~747 in rdpar'
        endif

!       ERROR CHECK FOR REVISED LZS CLASSES
        do i=1,nrvr
!       rev. 9.1.37  Mar.  22/03  - Option to turn off leakage by setting LZF < 0.0
          if(flz_o(nrvr).lt.0.0)flz_o(nrvr)=0.1e-30
!          removed Jan. 26/07  nk (no longer needed)
c          if(flz(n).eq.0.0.or.pwr(n).eq.0.0)then
c            print*,nrvr,' classes found in file ',fln(2)
c            print*,i-1,' non zero lzf & pwr values found in',fln(2)
c            print*,'Program revised to have nrvr classes for'
c            print*,'flz & pwr - please fix ',fln(2)
c            print*,' '
c            STOP 'Program halted in rdpar.for line 340'
c          endif
	    if(pwr_o(nrvr).lt.0.1.or.pwr_o(nrvr).gt.4.0)then   !  Feb. 22/06  nk
	      print*
	      print*,'The value of pwr for land cover class ',nrvr
	      print*,'is ',pwr(nrvr),' which' 
	      print*,'is not in the range of 0.1 - 4.0'
	      print*,'Please check all values'
	      print*
	      print*,'Program aborted in rdpar @ 607'
	    endif
        end do

      if(iopt.eq.2)print*, 'in rdpar @ 712'


!      pause 33
!       read the over bank multiplier and river roughness parameter
        manningflg='n'
        read(32,1001,iostat=ios)title(16)  
        if(title(16).eq.'R1n  '.or.title(16).eq.'r1n  ')then
!         rev. 9.2.11  Sep.  11/05  - NK: added Manning's n  r1n & r2n
!         Manning's n is used instead of r1
          manningflg='y'

          backspace 32
          read(32,1001,iostat=ios)title(16),(r1n_o(i),i=1,nrvr)   
          if(ios.ne.0)then
            print*,'Error reading r1n in the parameter file data'
            print*
            stop '@ ~625 in rdpar'
          endif

          read(32,1001,iostat=ios)title(17),(r2n_o(i),i=1,nrvr)
          if(ios.ne.0)then
            print*,'Error reading r2n in the parameter file data'
            print*
            stop '@ ~631 in rdpar'
          endif

        else

          print*,'R1 and R2 values no longer accepted'
	    print*,'Please change values to Manning`s n'
	    print*,'and titles to R1n and R2n respectively'
	    Print*,'R2n = R2/10 approximately'
	    print*,'Values for width/depth ratio also required'
	    print*
	    stop 'Program aborted in rdpar @ 845'
        endif
      endif

      if(iopt.eq.2)print*, 'in rdpar @ 817'

      if(ver.ge.9.1)then
        read(32,1001,iostat=ios)title(83),(mndr_o(i),i=1,nrvr)
        if(ios.ne.0)then
          print*,'Error reading mndr in the parameter file data'
          print*
          stop '@ ~675 in rdpar'
        endif

!       read the three bankfull function coefficients
        read(32,1001,iostat=ios)title(84),(aa2_o(i),i=1,nrvr)
        if(ios.ne.0)then
          print*,'Error reading aa2 in the parameter file data'
          print*
          stop '@ ~682 in rdpar'
        endif

        read(32,1001,iostat=ios)title(85),(aa3_o(i),i=1,nrvr)
        if(ios.ne.0)then
          print*,'Error reading aa3 in the parameter file data'
          print*
          stop '@ ~688 in rdpar'
        endif

        read(32,1001,iostat=ios)title(86),(aa4_o(i),i=1,nrvr)
        if(ios.ne.0)then
          print*,'Error reading aa4 in  the parameter file data'
          print*
          stop '@ ~694 in rdpar'
        endif

        read(32,1001,iostat=ios)title(87),(theta_o(i),i=1,nrvr)
        if(ios.ne.0)then
          print*,'Error reading theta in the parameter file data'
          print*
          stop '@ ~700 in rdpar'
        endif

        read(32,1001,iostat=ios)title(88),(widep_o(i),i=1,nrvr)
c        print*,title(88),(widep(i),i=1,nrvr)
        if(ios.ne.0)then
          print*,'Error reading widep in the parameter file data'
          print*
          stop '@ ~706 in rdpar'
        endif

        if(manningflg.eq.'y')then
          do i=1,nrvr
            if(widep_o(i).le.0.0)then
              print*,'Width/depth ratio must be specified when using'
              print*,'Manning n as the roughness parameter.'
              print*,'Please fix the par file'
              print*
              stop 'Program aborted in rdpar @ 715'
            endif
          end do
        endif

        read(32,1001,iostat=ios)title(89),(kcond_o(i),i=1,nrvr)
c        print*,title(89),(kcond(i),i=1,nrvr)
        if(ios.ne.0)then
          print*,'Problems reading kcond in the parameter'
          print*,' '
          stop 'Program aborted in rdpar @ 723'
        endif



!     convert basin based parameters to grid based parameters
c        allocate(par_temp(nrvr),stat=iAllocate)
c        if(iAllocate.ne.0) STOP
c     *   'Warning: error with allocation of par_temp array in spl9'


!     rev. 9.4.07  May.  15/07  - NK: converted opt to gridded routing parameters
	  do n=1,naa
	    flz(n)=flz_o(ibn(n))
	    pwr(n)=pwr_o(ibn(n))
	    r1n(n)=r1n_o(ibn(n))
	    r2n(n)=r2n_o(ibn(n))
	    mndr(n)=mndr_o(ibn(n))
	    aa2(n)=aa2_o(ibn(n))
	    aa3(n)=aa3_o(ibn(n))
	    aa4(n)=aa4_o(ibn(n))
	    theta(n)=theta_o(ibn(n))
	    widep(n)=widep_o(ibn(n))
	    kcond(n)=kcond_o(ibn(n))
	  end do

c        deallocate(par_temp)

!       Check kcond < 1.0    If larger, things blow up.
!        kcondflg=0
!        do i=1,nrvr
!          if(kcond(i).ge.1.0)then
!            print*,' kcond > 1.0 for river class ',i
!            kcondflg=1
!          endif
!        end do
!        if(kcondflg.eq.1)stop 'Program aborted in rdpar - please fix'

      else    ! ver.lt.9.1
        print*,'parameter file versions before ver. 9.1'
	  print*,'no longer accepted'
	  print*,'Please update'
	  print*
	  stop 'Program aborted in rdpar @ 930'
      endif

      tempflg='n'
      if(wetflg.eq.'y')then
!       parameter checking
        do i=1,nrvr
          if(abs(theta_o(i)).lt.0.00001)then
            print*,' Value for theta for river class ',i
            print*,' too close to zero. Please provide reasonable'
            print*,' value  (0.01 to 0.5)'
            tempflg='y'
          endif
          if(abs(widep_o(i)).lt.1.0)then
            print*,' Value for width/depth ratio for river class',i
            print*,' too close to zero. Please provide reasonable'
            print*,' value say (1 to 100)'
            tempflg='y'
          endif
          if(abs(kcond_o(i)).lt.0.99e-09)then
            print*,' Value for kcond for river class ',i
            print*,' too close to zero. Please provide reasonable'
            print*,' value  (1.0e-09 to 1.00)'
            tempflg='y'
          endif
        end do
        if(tempflg.eq.'y')stop 'Program aborted in rdpar @ 595'
      endif       ! wetflg = y


      if(iopt.eq.2)print*, 'param 5'

      read(32,5009,iostat=ios)(nclass(i),i=1,ntype+1)
      if(ios.ne.0)then
        print*,'Error reading nclass in the parameter file data'
        print*
        stop '@ ~694 in rdpar'
      endif


!     rev. 9.2.35  Mar.  22/06  - NK: Glacier flow bypasses wetlands
!     glacier_class_number will be used in tracer
	glacier_class_number=0  !default value

      do i=1,ntype+1      ! added 09/03/05  nk
        if(nclass(i).eq.'GLACIER   ')nclass(i)='glacier   '
        if(nclass(i).eq.'Glacier   ')nclass(i)='glacier   '
        if(nclass(i).eq.' glacier  ')nclass(i)='glacier   '
        if(nclass(i).eq.' GLACIER  ')nclass(i)='glacier   '
        if(nclass(i).eq.' Glacier  ')nclass(i)='glacier   '
	  if(nclass(i).eq.'glacier   ')glacier_class_number=i
        if(nclass(i).eq.'WETLAND   ')nclass(i)='wetland   '
        if(nclass(i).eq.'Wetland   ')nclass(i)='wetland   '
        if(nclass(i).eq.' wetland  ')nclass(i)='wetland   '
        if(nclass(i).eq.' WETLAND  ')nclass(i)='wetland   '
        if(nclass(i).eq.' Wetland  ')nclass(i)='wetland   '
        if(nclass(i).eq.'WATER     ')nclass(i)='water     '
        if(nclass(i).eq.'Water     ')nclass(i)='water     '
        if(nclass(i).eq.' water    ')nclass(i)='water     '
        if(nclass(i).eq.' WATER    ')nclass(i)='water     '
        if(nclass(i).eq.' Water    ')nclass(i)='water     '
      end do

!     check added Jul. 31/06 nk
      if(glacier_class_number.eq.0.and.itrace.eq.1.
     *                           and.trcflg.eq.'y')then
	    trcflg='n'
          print*,'Tracer is set to track glacier melt'
          print*,'and the tracer flag (trcflg) is set to "y"'
          print*,'but there is no glacier class'
	    print*,'trcflg is set to "n" '
	    print*
	    pause 'Continue by hitting enter'
	endif
          	    
!     rev. 9.08.01 Apr.   3/01  - check wetland designation in rdpar
      if(wetflg.eq.'y')then
      if(nclass(ntype-1).eq.'wetland   ')then
!       everything just fine
!         only class ntype-1 is acceptable as a coupled wetland
      else
        print*,' Class no. ',ntype-1,' is expected to be'
        print*,' the wetland class and be labelled as such'
        print*,' Name found ***',nclass(ntype-1),'***'
        print*,' It should be ***wetland   ***'

        print*,' Please be sure that the order of the parameters'
        print*,' and the land cover data are in the same order'
        print*,' Please fix the parameter file and try again'
        print*
        stop ' Program aborted in rdpar at line ~217'
      endif
      endif
      if(iopt.eq.2)print*, 'param 6'
!      if(ver.ge.9.0)then
!        read(32,*)(iiclass(i),i=1,ntype+1)
!        print*,(iiclass(i),i=1,ntype+1)
!      endif
      read(32,1001,iostat=ios)title(6),(ds(i),i=1,ntype+1)   
      if(ios.ne.0)then
        print*,'Error reading depression storage (ds)'
        print*,' in the parameter file data'
        print*,ds
        print*
        stop '@ ~825 in rdpar'
      endif
      if(ver.ge.7.5)read(32,1001,iostat=ios) 
     *                  title(7),(dsfs(i),i=1,ntype+1)   
      if(ios.ne.0)then
        print*,'Error reading dsfs in the parameter file data'
        print*
        stop '@ ~832 in rdpar'
      endif

!     The true meaning of REC is that for a slope of 1.0, rec will
!     be the fraction of water taken out of the UZ. This amount is 
!     reduced by slope sl1   nk Jul. 21/04

      read(32,1001,iostat=ios)title(8),(rec(i),i=1,ntype+1)  
      if(ios.ne.0)then
        print*,'Error reading rec in the parameter file data'
        print*,rec
        print*
        stop '@ ~843 in rdpar'
      endif
	do i=1,ntype+1
	  if(rec(i).gt.1.00.or.rec(i).lt.0.0)then
!         note: larger value will give NaN for x4 in runof6 
	    print*,'the value for rec can not be larger than 1.00'
	    print*,'or less than 0.00'
	    print*,'Please change the value in the par file'
	    print*
	    stop 'Program aborted in rdpar @ 1040'
	  endif
	end do
      ijunko=1

      if(iopt.eq.2)print*, 'param 7'

      if(ijunko.eq.0)then
        STOP 'Program aborted in rdpar.for @ ~116'
      endif
      read(32,1001,iostat=ios)title(9),(ak(i),i=1,ntype+1)   
      if(ios.ne.0)then
        print*,'Error reading ak in the parameter file data'
        print*,ak
        print*
        stop '@ ~856 in rdpar'
      endif

      if(ver.ge.7.5)
     *    read(32,1001,iostat=ios)title(10),(akfs(i),i=1,ntype+1)
      if(ios.ne.0)then
        print*,'Error reading akfs in the parameter file data'
        print*,akfs
        print*
        stop '@ ~864 in rdpar'
      endif
      read(32,1001,iostat=ios)title(11),(retn(i),i=1,ntype+1)   
      if(ios.ne.0)then
        print*,'Error reading retn in the parameter file data'
        print*
        stop '@ ~870 in rdpar'
      endif
      read(32,1001,iostat=ios)title(12),(ak2(i),i=1,ntype+1)   
      if(ios.ne.0)then
        print*,'Error reading ak2 in the parameter file data'
        print*
        stop '@ ~876 in rdpar'
      endif
      if(ver.ge.7.7)
     *    read(32,1001,iostat=ios)title(13),(ak2fs(i),i=1,ntype+1)
      if(ios.ne.0)then
        print*,'Error reading ak2fs in the parameter file data'
        print*
        stop '@ ~883 in rdpar'
      endif
   

!      pause 6


!     REVISED PAR FILE TO GROUP CLASSES:
      if(ver.lt.8.99)then
        read(32,1001,iostat=ios)title(14),(flz(i),i=1,nrvr)
        if(ios.ne.0)then
          print*,'Error reading flz in the parameter file data'
          print*
          stop '@ ~896 in rdpar'
        endif
        read(32,1001,iostat=ios)title(15),(pwr(i),i=1,nrvr)
        if(ios.ne.0)then
          print*,'Error reading pwr in the parameter file data'
          print*
          stop '@ ~902 in rdpar'
        endif
!       ERROR CHECK FOR REVISED LZS CLASSES
        do i=1,nrvr
          if(flz(i).eq.0.0.or.pwr(i).eq.0.0)then
            print*,'Program revised to have nrvr classes for'
            print*,'flz & pwr - please fix parameter file'
            print*,' '
            STOP 'Program halted in rdpar.for line 123'
          endif
        end do
        read(32,1001,iostat=ios)title(16),(r1(i),i=1,nrvr)   
        if(ios.ne.0)then
          print*,'Error reading r1 in the parameter file data'
          print*
          stop '@ ~917 in rdpar'
        endif
        read(32,1001,iostat=ios)title(17),(r2(i),i=1,nrvr)
        if(ios.ne.0)then
          print*,'Error reading r2 in the parameter file data'
          print*
          stop '@ ~923 in rdpar'
        endif
      endif

!     rev. 9.2.37  Mar.  31/06  - NK: Removed impervious area as special class
!         now need the values below:
      if(ds(ntype+1).eq.0.0)ds(ntype+1)=1.0
      if(dsfs(ntype+1).eq.0.0)dsfs(ntype+1)=1.0
      if(ds(ntype+1).eq.0.0)ds(ntype+1)=1.0
      if(rec(ntype+1).eq.0.0)rec(ntype+1)=0.1
      if(ak(ntype+1).eq.0.0)ak(ntype+1)=1.0e-10
      if(akfs(ntype+1).eq.0.0)akfs(ntype+1)=1.0e-10
      if(ak2(ntype+1).eq.0.0)ak2(ntype+1)=1.0e-10
      if(ak2fs(ntype+1).eq.0.0)ak2fs(ntype+1)=1.0e-10
      if(r3(ntype+1).eq.0.0)r3(ntype+1)=0.04
      if(r3fs(ntype+1).eq.0.0)r3fs(ntype+1)=0.04
      if(r4(ntype+1).eq.0.0)r4(ntype+1)=10.0

      if(iopt.eq.2)print*, 'param 8'

      read(32,1001,iostat=ios)title(18),(r3(i),i=1,ntype+1) 
      if(ios.ne.0)print*,'last read =',title(18),(r3(i),i=1,ntype+1)
      if(ver.ge.7.5)read(32,1001,iostat=ios) 
     *                  title(19),(r3fs(i),i=1,ntype+1) 
      if(ios.ne.0)print*,'last read =',title(19),(r3fs(i),i=1,ntype+1)
      read(32,1001,iostat=ios)title(20),(r4(i),i=1,ntype+1)   
      if(ios.ne.0)print*,'last read =',title(20),(r4(i),i=1,ntype+1)
      read(32,1001,iostat=ios)title(21),(chnl(i),i=1,5) 
      if(ios.ne.0)print*,'last read =',title(21),(chnl(i),i=1,ntype+1)
      read(32,1001,iostat=ios)title(22),(fm(ii),ii=1,ntype+1)
      if(ios.ne.0)print*,'last read =',title(22),(fm(i),i=1,ntype+1)
      read(32,1001,iostat=ios)title(23),(base(ii),ii=1,ntype+1)
      if(ios.ne.0)print*,'last read =',title(23),(base(i),i=1,ntype+1)
      read(32,1001,iostat=ios)title(24),(fmn(ii),ii=1,ntype+1)
      if(ios.ne.0)print*,'last read =',title(24),(fmn(i),i=1,ntype+1)
      read(32,1001,iostat=ios)title(25),(uadj(ii),ii=1,ntype+1)
      if(ios.ne.0)print*,'last read =',title(25),(uadj(i),i=1,ntype+1)
      read(32,1001,iostat=ios)title(26),(tipm(ii),ii=1,ntype+1)
      if(ios.ne.0)print*,'last read =',title(26),(tipm(i),i=1,ntype+1)
      read(32,1001,iostat=ios)title(27),(rho(ii),ii=1,ntype+1)
      if(ios.ne.0)print*,'last read =',title(27),(rho(i),i=1,ntype+1)
      read(32,1001,iostat=ios)title(28),(whcl(ii),ii=1,ntype+1)
      if(ios.ne.0)print*,'last read =',title(28),(whcl(i),i=1,ntype+1)
!     rev. 9.1.76  Mar.  09/05  - NK: separated glacier parameters in par file
      if(iopt.eq.2)print*, 'param 8a'
      if(ver.ge.9.3)then
        read(32,5004,iostat=ios)title(29),fmadjust
        if(ios.ne.0)print*,'last read =',title(29),fmadjust
        read(32,5004,iostat=ios)title(102),fmalow
        if(ios.ne.0)print*,'last read =',title(102),fmalow
        read(32,5004,iostat=ios)title(103),fmahigh
        if(ios.ne.0)print*,'last read =',title(103),fmahigh
        read(32,5004,iostat=ios)title(104),gladjust
        if(ios.ne.0)print*,'last read =',title(104),gladjust
        read(32,5004,iostat=ios)title(105),rlapse
        if(ios.ne.0)print*,'last read =',title(105),rlapse
        read(32,5004,iostat=ios)title(105),elvref
        if(ios.ne.0)print*,'last read =',title(106),elvref
	  if(iopt.eq.2)print*, 'param 8b1'
      else
        read(32,5004,iostat=ios) 
     *    title(29),fmadjust,fmalow,fmahigh,gladjust,rlapse,elvref
        if(ios.ne.0)print*,'last read =',
     *    title(29),fmadjust,fmalow,fmahigh,gladjust,rlapse,elvref
!             ELEVATION IS IN HUNDREDS OF METERS
!             LAPSE RATE IS IN DEGREE C / 100 m - USUALLY 0.5 DEGREE C
	  if(iopt.eq.2)print*, 'param 8b2'
      endif

      if(iopt.eq.2)print*, 'param 9'
      if(fmadjust.gt.0.0)then
         if(fmalow.eq.0.0.or.fmahigh.eq.0.0)then
            print*,'fmadjust,fmalow,fmahigh/',fmadjust,fmalow,fmahigh
            print*,'fmadjust is set to 0.0'
            print*,' because there are no limits set in bsnm.par'
            fmadjust=0.0
            write(*,'(A)',advance='no')' hit enter to continue'
            read(*,*)
         endif
      endif
      if(ios.ne.0)then
        print*,''
        print*,'Error in ds - whcl part of the parameter file'
        print*
        stop '@ ~469 in rdpar'
      endif

      if(ver.ge.8.5)then
        read(32,9804,iostat=ios)title(31),flgevp2,junk
!       REV. 8.71 - Feb.  24/98 -  ADDED EVPFLG2 TO INPEVTA.FOR
!       REV. 8.71 - deleted Nov. 6/98
        read(32,9802,iostat=ios)title(32),albe
        if(ios.ne.0)print*,'last read =',title(32),albe
!! TS: ADDED READ FOR IMPERV ALBEDO VALUE AND/OR DEFAULT VALUE: MAR 27/06
        read(32,9802,iostat=ios)title(33),(alb(i),i=1,ntype+1)
        if(ios.ne.0)print*,'last read =',title(33),alb
        if(alb(ntype+1).eq.0)then
          alb(ntype+1)=0.18
	    write(51,*)
     *     '** Albedo for IMPERV class not found, assumed 0.18 **'
	  endif     
        read(32,9802,iostat=ios)title(34),(fpet(i),i=1,ntype+1)
        if(ios.ne.0)print*,'last read =',title(34),fpet
        if(fpet(ntype+1-1).le.0.0)then
	    print*
	    print*,'Value for fpet for water class =',fpet(i)
	    print*,'This value is multiplied by the pet for water'
	    print*,'so for this value there will be no evaporation'
	    print*,'from water bodies'
	    print*,'Reasonable values are 0.5-1.0'
	    print*,'Please change the par file accordingly'
	    print*
	    stop 'Program aborted in rdpar @ 1356'
	  endif
        read(32,9802,iostat=ios)title(35),(ftall(i),i=1,ntype+1)
        if(ios.ne.0)print*,'last read =',title(35),ftall
        read(32,9802,iostat=ios)title(36),(flint(i),i=1,ntype+1)
        if(ios.ne.0)print*,'last read =',title(36),flint
        read(32,9802,iostat=ios)title(37),(fcap(i),i=1,ntype+1)
        if(ios.ne.0)print*,'last read =',title(37),fcap
        read(32,9802,iostat=ios)title(38),(ffcap(i),i=1,ntype+1)
        if(ios.ne.0)print*,'last read =',title(38),ffcap
	  do i=1,ntype+1
	    if(ffcap(i).le.0.0)then
	      print*,'ffcap(',i,') set too low. Must be > 0.0'
	      print*,'ffcap(',i,') set to 0.01'
	      print*
	    endif
	  end do
        read(32,9802,iostat=ios)title(39),(spore(i),i=1,ntype+1)
        if(ios.ne.0)print*,'last read =',title(39),spore
        do ii=1,ntype+1
          if(spore(ii).le.0.0)then
            print*,'spore(',ii,')=',spore(ii)
            print*,' This value can not be .le. 0.0'
            print*,' Please correct the parameter file'
            print*
            stop 'Program aborted in rdpar at ~1254'
          endif
        end do

!     rev. 9.2.37  Mar.  31/06  - NK: Removed impervious area as special class
!         now need the values below:
      if(fpet(ntype+1).eq.0.0)fpet(ntype+1)=1.0
      if(ftall(ntype+1).eq.0.0)ftall(ntype+1)=1.0
      if(flint(ntype+1).eq.0.0)flint(ntype+1)=1.0
      if(fcap(ntype+1).eq.0.0)fcap(ntype+1)=1.0
      if(ffcap(ntype+1).eq.0.0)ffcap(ntype+1)=1.0
      if(spore(ntype+1).eq.0.0)spore(ntype+1)=1.0
        
!     rev. 9.1.80  Mar.  31/05  - NK: added sublimation   (sublim)
!                  modified this Feb. 6/06
        read(32,9801,iostat=ios)title(40),tempa1
!       tempa1 no longer used. Line used to read sublimation factor
        if(title(40).eq.'sublm')then
          backspace 32
          read(32,9801,iostat=ios)title(40),
     *                       (sublim_factor(i),i=1,ntype+1)
          if(ios.ne.0)print*,'last read =',title(40),sublim_factor
	    do i=1,ntype+1
	      if(sublim_factor(i).gt.0.5)then
	        print*,'Warning: sublimation factor for class',i
	        print*,'is set too high. Sublim reduced to 0.5'
	      endif
	      sublim_factor(i)=amin1(0.5,sublim_factor(i))
	    end do
	  else
	    do i=1,ntype+1
            sublim_factor(i)=0.0
	    end do
        endif
        read(32,9801,iostat=ios)title(41),tempa2
        if(ios.ne.0)print*,'last read =',title(41),tempa2
        read(32,9801,iostat=ios)title(42),tempa3
        if(ios.ne.0)print*,'last read =',title(42),tempa3
	  if(tempa3.lt.0.0001)then
          tempa3=0.0001
	    write(51,*)'WARNING:'
	    write(51,*)'Temp3 <0.0001  set temp3=0.001'
	    write(51,*)
	  endif
        read(32,9801,iostat=ios)title(43),tton
        if(ios.ne.0)print*,'last read =',title(43),tton
	  if(tton.lt.0.000)tton=0.0
        read(32,9801,iostat=ios)title(44),lat
        if(ios.ne.0)print*,'last read =',title(44),lat
        read(32,9803,iostat=ios)title(45),(diff(i),i=1,12)
        if(ios.ne.0)print*,'last read =',title(45),diff
        read(32,9803,iostat=ios)title(46),(hu(i),i=1,12)
        if(ios.ne.0)print*,'last read =',title(46),hu
        read(32,9803,iostat=ios)title(47),(pres(i),i=1,12)
        if(ios.ne.0)print*,'last read =',title(47),pres
      endif
      if(iopt.eq.2)print*, 'param 10'
      if(ios.ne.0)then
        print*,''
        print*,'Error in the evaporation part of the parameter file'
        print*
        stop '@ ~497 in rdpar'
      endif

      do ii=1,ntype+1
        if(spore(ii).le.0.0)then
          print*,'ii,spore',ii,spore(ii)
          print*,' line title =',title(39)
          print*,' value for spore can not be <= 0.0'
          STOP 'Program stopped in rdpar @ 9443'
        endif
      end do

      read(32,5005,iostat=ios)heading(2)
!     read the interception capacities
      do ii=1,ntype+1
        read(32,*,iostat=ios)title(50+ii),(h(i,ii),i=1,12)
        write(51,1003)title(50+ii),(h(i,ii),i=1,12)
	  if(ios.ne.0)then
	    print*,'error reading the h(i,ii) values'
	    print*,'Please ensure there is a line for each class'
	    print*,'including the impervious class (last class)'
	    print*,'There is now 1 more line with the new ensim'
          print*,'format shed file'
	    print*
	    stop 'Program aborted in rdpar @ 1312'
	  endif
        if(ios.ne.0)then
          print*,''
          print*,'Error reading title(',50+ii,') in the parameter file'
	    print*,'ii=',ii
          do j=1,ii
	      print*,title(50+j),(h(i,j),i=1,12)
	    end do
	    print*,'last heading read =',heading(2)
          print*
          stop 'program aborted @ ~934 in rdpar'
        endif
      end do

!     rev. 9.2.37  Mar.  31/06  - NK: Removed impervious area as special class
!         now need the values below:
      do i=1,12
	  h(i,ntype+1)=0.0
	end do

	do ii=1,ntype+1

!       adjust potential evaporation from vegetation by amount
        hmax=0.0  
!     rev. 9.2.22  Nov.  15/05  - NK: Fixed hmax bug in rdpar 
!             hmax=0 was erroniously moved down between 
!                    ver 9.2.03 & 9.2.2
        do i=1,12
!     rev  9.1.25  Sep.  11/02  - Added A11 as bare ground equiv. vegn height  
!          h(i,ii)=h(i,ii)+a11    ! taken out and replaced Jan. 25/06 nk
         h(i,ii)=amax1(a11,h(i,ii)) !  Jan. 25/06 nk
         hmax=amax1(hmax,h(i,ii))
        end do 

        do i=1,12
!         problems here nk Nov. 20/02
!         rev. 9.1.32  Nov.  20/02  - Fixed fpetmon() wrt. h()
!         h(month,class) is defined as the max interception storage. 
!         So for soil evaporation it has to become an index 
!         fpetmo() is that index - it modifies ftall(ii) to account for veg height

          fpetmo(i,ii)=ftall(ii)*h(i,ii)/hmax

!          fpetmo(i,ii)=ftall(ii)*h(i,ii)

        end do
        write(51,1003)'hmax ',hmax
        write(51,1003)'h(  )',(h(i,ii),i=1,12)
        write(51,1003)'fpetm',(fpetmo(i,ii),i=1,12)
      end do

      if(iopt.eq.2)print*, 'param 10a'

!     TO PREVENT DIVISION BY ZERO IN ETIN
      do ii=1,ntype+1
        do i=1,12
          h(i,ii)=amax1(0.01,h(i,ii))
        end do
      end do

      do ii=1,ntype+1
        if(ak2(ii).gt.1.0)then
          write(*,1024)ii
1024      format(' AK2(',i5,') =',e12.3)
          STOP ' Please change value for AK2- must be lt 1'
        endif

        if(ver.lt.7.5)then
          akfs(ii)=ak(ii)
          dsfs(ii)=ds(ii)
          r3fs(ii)=r3(ii)
        endif

        if(ver.lt.7.7)then
          ak2fs(ii)=ak2(ii)
        endif

        if(ak2fs(ii).gt.1.0)then
          write(*,1027)ii
1027      format(' AK2fs(',i5,') =',e12.3)
          STOP ' Please change value for AK2fs - must be lt 1'
        endif
      end do

!     SET POTENTIALS FOR EACH SOIL TYPE:
!     moved from soilinit   Mar. 14/07  nk
      do ii=1,ntype+1
!        NOTE FOR AK<0.0 WE HAVE WATER AREA & POT ISN'T USED
         if(ak(ii).gt.0.0)then
!           AK IS IN MM/HR
!           BUT FORMULA IS FOR MM/SEC
            xxx1=-alog10(ak(ii)/3600.)
            pot(ii)=250.*xxx1+100.
         endif
!        REV. 7.5 SEPERATE SNOW COVERED AND BARE GROUND
         if(akfs(ii).gt.0.0)then
!           ak is in mm/hr
!           but formula is for mm/sec
            xxx1=-alog10(akfs(ii)/3600.)
            potfs(ii)=250.*xxx1+100.
         endif
      end do



      if(iopt.eq.2)print*, 'param 10b'
        read(32,5005)heading(3)

!     This was fixed between version 9.1.11 and 9.1.13
!     It used to be all sin(...). End result: pet too high
      sinlat=sin(3.1416*lat/180.0)
      coslat=cos(3.1416*lat/180.0)
      tanlat=tan(3.1416*lat/180.0)

!     PART II     OPTIMIZATION      SECTION
!     PART II     OPTIMIZATION      SECTION
!     PART II     OPTIMIZATION      SECTION
!     PART II     OPTIMIZATION      SECTION

c      if(numa.ge.1.or.iverflg.eq.1.or.dds_flag.ge.1)then
      if(numa.ge.1.or.dds_flag.ge.1)then   !nk 21/09/06

	  numa=1        !used as a flag if =1
!	  dds_flag=1    !used as a flag if =1

!      if(numa.ge.1.or.iverflg.eq.1)then

!       IF NUMA >0 THEN WE ARE OPTIMIZING AND WE NEED THE LIMITS, ETC. 
!       READ THE OPTIMIZATION LIMITS AND STARTING VALUES
!       NOTE: FOR VER 6.2 AND LATER THE VALUES IN THE BOTTOM 
!       TABLE ARE USED AS THE INITIAL VALUES AND THE EARLIER VALUES ARE
!       IGNORED. THIS MAKES IT A LITTLE EASIER TO CHECK THE CONSTRAINTS.

!  REV. 8.32 - June  13/97 -  BYPASSED NON-FLAGGED PARAMETERS IN OPTA

!       WHEN A PARAMETER IS NOT BEING OPTIMIZED, THE VALUE FROM THE
!       TOP OF THE TABLE HAS TO BE USED. IN THE NEW.PAR FILE, THE TOP 
!       VALUE WILL REPLACE THE LOWER VALUE

!     rev. 9.07    Mar.  14/01  - fixed use of opt par's  for numa=0  
        if(ntypeo.ge.1)then
          read(32,1002,iostat=ios)
     *    (title(101),akdlt(i),aklow(i),akhgh(i),ajunk(i),i=1,ntypeo)

          do i=1,ntypeo
            if(akdlt(i).gt.0.0.and.numa.ge.1)ak(i)=ajunk(i)
          end do

          read(32,1002,iostat=ios)
     *    (title(101),akfsdlt(i),akfslow(i),akfshgh(i),ajunk(i),
     *                         i=1,ntypeo)
          do i=1,ntypeo
            if(akfsdlt(i).gt.0.0.and.numa.ge.1)akfs(i)=ajunk(i)
          end do

          read(32,1002,iostat=ios)(title(101),
     *    recdlt(i),reclow(i),rechgh(i),ajunk(i),i=1,ntypeo)
          do i=1,ntypeo
            if(recdlt(i).gt.0.0.and.numa.ge.1)rec(i)=ajunk(i)
          end do
          read(32,1002,iostat=ios)
     *    (title(101),r3dlt(i),r3low(i),r3hgh(i),ajunk(i),i=1,ntypeo)
          do i=1,ntypeo
            if(r3dlt(i).gt.0.0.and.numa.ge.1)r3(i)=ajunk(i)
          end do

          if(ver.ge.8.5)then
            read(32,1002)
     *      (title(101),fpetdlt(i),fpetlow(i),fpethgh(i),ajunk(i),
     *                                               i=1, ntypeo)
            do i=1,ntypeo
              if(r3dlt(i).gt.0.0.and.numa.ge.1)fpet(i)=ajunk(i)
            end do
            read(32,1002)
     *      (title(101),ftalldlt(i),ftalllow(i),ftallhgh(i),ajunk(i),
     *                                               i=1,ntypeo)
            do i=1,ntypeo
              if(r3dlt(i).gt.0.0.and.numa.ge.1)ftall(i)=ajunk(i)
            end do
          endif

          read(32,5003,iostat=ios)(title(101),
     *    fmdlt(i),fmlow(i),fmhgh(i),ajunk(i),i=1,ntypeo)
          do i=1,ntypeo
            if(fmdlt(i).gt.0.0.and.numa.ge.1)fm(i)=ajunk(i)
          end do

          read(32,5003,iostat=ios)(title(101),
     *    basdlt(i),baslow(i),bashgh(i),ajunk(i),i=1,ntypeo)
          do i=1,ntypeo
            if(basdlt(i).gt.0.0.and.numa.ge.1)base(i)=ajunk(i)
          end do

          read(32,5003,iostat=ios)(title(101),
     *    fmndlt(i),fmnlow(i),fmnhgh(i),ajunk(i),i=1,ntypeo)
          do i=1,ntypeo
            if(fmndlt(i).gt.0.0.and.numa.ge.1)fmn(i)=ajunk(i)
          end do

          read(32,5003,iostat=ios)
     *    (title(101),retndlt(i),retnlow(i),retnhgh(i),ajunk(i),
     *                                               i=1,ntypeo)
          do i=1,ntypeo
            if(retndlt(i).gt.0.0.and.numa.ge.1)retn(i)=ajunk(i)
          end do

          read(32,5003,iostat=ios)
     *    (title(101),ak2dlt(i),ak2low(i),ak2hgh(i),ajunk(i),
     *                                            i=1,ntypeo)
          do i=1,ntypeo
            if(ak2dlt(i).gt.0.0.and.numa.ge.1)ak2(i)=ajunk(i)
          end do

          read(32,5003,iostat=ios)
     *    (title(101),ak2fsdlt(i),ak2fslow(i),ak2fshgh(i),ajunk(i),
     *                                           i=1,ntypeo)
          do i=1,ntypeo
            if(ak2fsdlt(i).gt.0.0.and.numa.ge.1)ak2fs(i)=ajunk(i)
          end do
          if(iopt.eq.2)print*, 'param 10c'

        endif

        if(ver.lt.9.0)then
!         old version without wetlands
          if(nbsn.ge.1)then
            read(32,5003,iostat=ios)(title(101),
     *       flzdlt(i),flzlow(i),flzhgh(i),ajunk(i),i=1,nbsn)
            do i=1,nbsn
              if(flzdlt(i).gt.0.0.and.numa.ge.1)flz_o(i)=ajunk(i)
            end do
            read(32,5003,iostat=ios)(title(101),
     *       pwrdlt(i),pwrlow(i),pwrhgh(i),ajunk(i),i=1,nbsn)  
            do i=1,nbsn
              if(pwrdlt(i).gt.0.0.and.numa.ge.1)pwr_o(i)=ajunk(i)
            end do
            read(32,5003,iostat=ios)
     *        title(101),a5dlt,a5low,a5hgh,ajunk(1)
            if(a5dlt.gt.0.0.and.numa.ge.1)a5=ajunk(1)
            read(32,1002,iostat=ios)
     *       (title(101),r2dlt(i),r2low(i),r2hgh(i),ajunk(i),i=1,nbsn)
            if(title(101).ne.'pwr  ')then
              print*,' Please note that bottom of par file needs to be '
              print*,' changed. Order should be pwr(nbsn),A5,A9,A10,A11'
              print*,' Hit enter to continue if not optimizing these'
        !      pausee ' else hit ^break'
            endif
            do i=1,nbsn
              if(r2dlt(i).gt.0.0.and.numa.ge.1)r2(i)=ajunk(i)
            end do
          endif
          if(iopt.eq.2)print*, 'param 10d'
        else    !    ver.ge.9.0
!         new version for wetlands
          if(nbsn.ge.1)then
            read(32,5003,iostat=ios)(title(101),
     *       flzdlt(i),flzlow(i),flzhgh(i),ajunk(i),i=1,nbsn)
            do i=1,nbsn
              if(flzdlt(i).gt.0.0.and.numa.ge.1)flz_o(i)=ajunk(i)
            end do
            read(32,5003,iostat=ios)(title(101),
     *       pwrdlt(i),pwrlow(i),pwrhgh(i),ajunk(i),i=1,nbsn)  
            do i=1,nbsn
              if(pwrdlt(i).gt.0.0.and.numa.ge.1)pwr_o(i)=ajunk(i)
            end do
!           rev. 9.2.11  Sep.  15/05  - NK: added Manning's n  r1n & r2n
c            if(manningflg.eq.'y')then
              read(32,1002,iostat=ios)(title(101),
     *         r2ndlt(i),r2nlow(i),r2nhgh(i),ajunk(i),i=1,nbsn)
              do i=1,nbsn
                if(r2ndlt(i).gt.0.0.and.numa.ge.1)r2n_o(i)=ajunk(i)
              end do
c            else
c              read(32,1002,iostat=ios)(title(101),
c     *         r2dlt(i),r2low(i),r2hgh(i),ajunk(i),i=1,nbsn)
c              do i=1,nbsn
c                if(r2dlt(i).gt.0.0.and.numa.ge.1)r2(i)=ajunk(i)
c              end do
c            endif

          endif
          if(ver.lt.9.2)then
            read(32,5003,iostat=ios)
     *        title(101),a5dlt,a5low,a5hgh,ajunk(1)
            if(a5dlt.gt.0.0.and.numa.ge.1)a5=ajunk(1)
            read(32,5003,iostat=ios)
     *        title(101),a9dlt,a9low,a9hgh,ajunk(1)
            if(a9dlt.gt.0.0.and.numa.ge.1)a9=ajunk(1)
            read(32,5003,iostat=ios)
     *        title(101),a10dlt,a10low,a10hgh,ajunk(1)
            if(a10dlt.gt.0.0.and.numa.ge.1)a10=ajunk(1)
            read(32,5003,iostat=ios)
     *        title(101),a11dlt,a11low,a11hgh,ajunk(1)
            if(a11dlt.gt.0.0.and.numa.ge.1)a11=ajunk(1)
          else      !   ver.ge.9.2
            read(32,5003,iostat=ios)(title(101),
     *       thetadlt(i),thetalow(i),thetahgh(i),ajunk(i),i=1,nbsn)  
            do i=1,nbsn
              if(thetadlt(i).gt.0.0.and.numa.ge.1)theta_o(i)=ajunk(i)
            end do
            read(32,5003,iostat=ios)(title(101),
     *       kconddlt(i),kcondlow(i),kcondhgh(i),ajunk(i),i=1,nbsn)  
!     rev. 9.2.16  Oct.  10/05  - NK: Fixed bug for widep in rdpar
            do i=1,nbsn
              if(kconddlt(i).gt.0.0.and.numa.ge.1)kcond_o(i)=ajunk(i)
            end do
            read(32,5003,iostat=ios)
     *        title(101),a5dlt,a5low,a5hgh,ajunk(1)
            if(a5dlt.gt.0.0.and.numa.ge.1)a5=ajunk(1)
          endif
          if(iopt.eq.2)print*, 'param 10e'

        endif

!     rev. 9.4.07  May.  15/07  - NK: converted opt to gridded routing parameters
	  do n=1,naa
	    flz(n)=flz_o(ibn(n))
	    pwr(n)=pwr_o(ibn(n))
	    r2n(n)=r2n_o(ibn(n))
	    theta(n)=theta_o(ibn(n))
	    kcond(n)=kcond_o(ibn(n))
	  end do

!       NOTE:
!       TABLE WILL BE BLENDED DEPENDING ON WHETHER A PARAMETER WAS 
!       FLAGGED TO BE OPTIMIZED
!       VALUES FROM TOP PART OF TABLE IF NOT OPTIMIZED
!       VALUES FROM LOWER PART IF FLAGGED TO BE OPTIMIZED
!       THIS HAPPENS ONLY IN AN OPT RUN


!       DDELTA IS CHANGED DURING EXECUTION - NEED TEMP VARIABLE TO 
!       SAVE ORIGINAL VALUES:

      endif

!     close the parameter file
      close(unit=32) 

d      print*,'Closed unit 32  fln=',fln(2)

      if(iopt.eq.2)print*, 'param 11'

!      if(numa.eq.0)then                     
      if(nnn.eq.0)then                     
!       Echos par file TO SIMOUT/SPL.TXT FOR THE RECORD

!       rev. 9.2.09  Sep.  11/05  - NK: removed write_par.for from rdpar.for
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c        call write_par(51,ix,e1)
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      endif

      if(iopt.eq.2)print*, 'param 12'

      if(ios.ne.0)then
        print*,''
        print*,' An error has been encountered in the parameter file'
        print*,' Please see the simout/spl.txt file to find out how'
        print*,' the program got before encountering the error'
        print*
c        stop ' Program aborted in rdpar.for @ ~716'
      endif

      if(flz(1).le.0.0)then
        write(*,8429)flz
        STOP
      endif

!     POTENTIAL EVAPORATION:
!     DEFAULT VALUES:
!     CHANGED SO IT WOULD COMPILE IN UNIX - FRANK S OCT/97
!     TS: ADDED IMPERV=ntype+1 EVAP VALUE (0 CAUSE NO GROWTH)
      do i=1,ntype+1       
        do j=1,12
          evap(i,j)=0.0
	    if(i.eq.ntype+1) evap(i,j)=0.0
        end do
      end do

!     READ THE MONTHLY EVAP:
      filename1='basin/evap.dat'
      open(unit=99,file=filename1,status='unknown',err=99901)
      do n=1,ntype+1
        read(99,3101,end=3201)(evap(n,j),j=1,12)
        write(51,3101)(evap(n,j),j=1,12)
      end do

      if(iopt.eq.2)print*, 'param 13'

      GO TO 3202

99901 write(*,5100)
      write(98,5100)

      GO TO 3202

 3201 write(98,3203)

 3202 CONTINUE

      close(unit=99,status='keep')


      if(iopt.eq.2)print*, 'param 13'

      firstpass='n'
      answer='n'
      if(iverflg.eq.0)RETURN

      print*,' An old parameter file format was found and a new'
      print*,' format file called basin/new.par will be created '
      print*,' This message will not reappear if you copy the '
      print*,' new.par file to the bsnm.par file'
      print*,' However, the new.par file will NOT be compatible'
      print*,' with the old spl8 program.'
      print*
      pause 'Hit enter to continue - param @ 1294'
      answer='y'

!     REPLACE AN OLD VERSION PAR FILE WITH THE NEW FORMAT

!     rev. 9.2.09  Sep.  11/05  - NK: removed write_par.for from rdpar.for
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c      call write_par(99,ix,e1)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      if(iopt.eq.2)print*, 'param before return @ 9989'


! 9989 RETURN

      firstpass='n'
      RETURN

 9999 CONTINUE

      print*,' stopped in rdpar due to data problems - see spl.txt'
      print*,' spl.txt will show how much data was properly read'
      STOP 'program stopped in rdpar.for'

! FORMATS

 1000 format(a5,g10.3,2i5,a65)
 1001 format(a5,17e10.3)
 1002 format(a5,4e10.3)
 1003 format(a5,12f5.2)
 1004 format(a5,17e10.3)
 1005 format(a5,17e10.3)
 1006 format(a5,17e10.3)
 1007 format(a5,17e10.3)
 1008 format(a5,17e10.3)
 1009 format('     e       ix')
 1010 format(' maximun interception storage in mm')
 1011 format(' depression storage ds(i)')
 1012 format(' permeabilities ak(i)')
 1013 format(' roughness r3(i)  -  pervious area')
 1014 format(' velocity factor for channels/square   chnl(i)')
 1015 format(' roughness r4(i)  -  impervious area')
 1019 format(' a1 ... a12')
 1022 format(' interflow recession constant rec(i)')
 1023 format(' ','ti            delta        low         high   param') 
 1025 format(/' ','flgevp2 set to',f5.1,' in rdpar.for'/)

 1031 format(' lower zone discharge function flz(i)')
 1032 format(' lower zone power pwr(i)')
 1033 format(' flood plain roughness multiplier r1(i)')
 1034 format(' river roughness r2(i)')
 1035 format(' meander length multiplier mndr(i)')
 1036 format(' bankfull constant aa2(i)')
 1037 format(' bankfull constant aa3(i)')
 1038 format(' bankfull constant aa4(i)')
 1039 format(' wetland porosity theta(i)')
 1040 format(' wetland width/depth ratio widep(i)')
 1041 format(' wetland conductivity kcond(i)')

 3000 format(a5,6i5,25x,f10.0) 
 3010 format(a1,a79)
 3101 format(12f5.0)
 3203 format(' Warning: basin/evap.dat table incomplete'/
     *         '          zero values are inserted for evap.dat'/)
 5000 format(a5,2i5,a75)
 5001 format(a5,17f10.3)
 5002 format(17f10.3)
 5003 format(a5,4e10.3)
 5004 format(a5,6f10.3)
 5005 format(a80)
 5006 format(' warning: default values for retn, ak2, flz & pwr'/
     *'          are used. par file is out of date'/)
 5007 format(' warning: iiout in d2 =',i5,' reset to 1 in rdpar')
 5009 format(5x,17a10)
 5010 format(a1)
        write(51,5111)a10

 5100 format(' in rdpar - problem opening basin/evap.dat file'/
     *         '          zero values are inserted for evap.dat'/)
5110  format(//,' Value for a9 heat deficit to swe outside range',/
     *     f10.3,' assumed')
5111  format(//,' Value for a10 uz exponent outside range',/
     *     f10.3,' assumed')
 5999 format(' mflag=',i5,'   iiout=',i5)
 6003 format(a5,17e10.3)

 6011 format('# runtime    ',2(a2,':'),a2)
 6012 format('# rundate  ',a4,'-',a2,'-',a2)


 8429 format(//' in runof5,flz/',5f10.6/
     * ' wrong parameter value - change flz(i) to +ve value'/)
 9801 format(a5,17f10.0)
 9802 format(a5,17f10.2)
 9803 format(a5,12f5.1)
 9804 format(a5,f10.2,a60)
 9805 format(a5,i10,5x,a40)
 9806 format(a5,f10.0,5x,a40)
 9807 format(a5,f10.3,5x,a40)
 9808 format(//' Parameters used for this run:'/)
 9809 format('A new par file called basin\new.par has been created')

      firstpass='n'

      RETURN

      END SUBROUTINE rdpar

