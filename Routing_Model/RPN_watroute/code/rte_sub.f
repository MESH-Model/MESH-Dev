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

      SUBROUTINE sub(e1,smc5,scale,icase,smok,optlow,igrdshft)

!***********************************************************************
!       copyright (c) by Nick Kouwen 1987-2007
!***********************************************************************
!
!     May 2010
!      - inserted call to read_flow_ef; (D. Deacu)
!      - inserted code to check the values read in for 'kt' and 'irdt';
!        'irdt' is set to 1 if kt=1; (D. Deacu)
!
!
      use area_watflood
      
C///////////////////////// 
C// Added by Dave
      USE EF_Module
C// End Dave addition
C/////////////////////////

      ! The fst-based IO is included in a module; this allows
      ! for token stubs to be included if building on a system
      ! without the appropriate libraries
      use fst_io

      implicit none

!     SAVES THE LOCAL VARIABLES FROM ONE RUN TO NEXT
      SAVE

      DIMENSION     :: smc5(16)
      CHARACTER(14) :: date
      CHARACTER(3)  :: eofmark
      CHARACTER(1)  :: lineflg,smok
      CHARACTER(20) :: junk 
      character*999 flnname
      REAL(4)            :: optlow,time,tot1,qwert,conv,scale,
     *                 smc5,tj1,clock,t,thr,dtmin,dtmax,div,aintvl,
     *                 sintvl,tot2,e1,tdum,qtemp,diff2,sdlz,dlz,
     *                 wfo_spec_version_number,
     *                 sec_div,route_dt,hr_div
      INTEGER       :: rbin,inta,block,i,j,n,ii,zz,
     *                 iallcnt1,n1,ii1,jan,m,ios,iallocate,
     *                 nnch,l,juold,jj,lun,nhr,nhf,
     *                 icase,iz,jz,nchr,mz,ju,mon,
     *                 iwest,ieast,isouth,inorth,nocolumns,nno,k,
     *                 nu,igrdshft,minsnwflg,oldjan,flgevp22,
     *                 noresv1,nj,npick,n_trick,no_frames,frame_no
      CHARACTER(128):: qstr
      CHARACTER(10) :: ctime
      CHARACTER(8)  :: cday
      LOGICAL       :: exists,newpafflg
      INTEGER(kind=2) :: result1,ntest
      CHARACTER(10) :: coordsys
      REAL          :: a66,areasum,areaclass(99)
      REAL          :: ha,fpw,kdn,nratio
!     *                 somme82,somme83,somme84,somme85,somme86,
!     *                 somme92,somme93,somme94,somme95,somme96

      DATA ntest/20492/qstr/'watflood.wfo'/nchr/18/
      DATA iallcnt1/0/

!     NOTE: FOR MONTHLY RUNS, DIMENSIONS CAN BE CHANGED FROM 
!           3,8784,500  TO  12,366,3000

!>>>>>>>>>>>>>  AB: STUFF FOR ENSIM
      INTEGER(4) :: wfo_yy,wfo_mm,wfo_dd,wfo_hh,wfo_mi,wfo_ss,
     *              wfo_ms
      INTEGER(4) :: wfo_seq

!     WFO IO FUNCTIONS
      INTEGER :: wfo_write_attribute_data
      INTEGER :: wfo_write_timestamp
!>>>>>>>>>>>>>

      integer :: fnumstr_qi,fnumstr_qr,fnum
      real*4, dimension(:), allocatable :: reach_lvl,somme_qi,somme_qr

      allocate(reach_lvl(Nreaches),reach_last(Nreaches),
     *         somme_qi(Nreaches),somme_qr(Nreaches),stat=iAllocate)
      if (iAllocate.ne.0) STOP 

c input/lake_levs/reach1_lvl.txt
      do i=1,Nreaches
        open(unit=500+i,file=infln(2+i),status='old')
        read(500+i,*) reach_lvl(i)
        reach_last(i)=reach_lvl(i)
!        lake_elv(i,1)=reach_lvl(i)

!        if(resname(i).eq.'Superior     ') sup_last = reach_lvl(i)  ! Read from the reservoir release file
!        if(resname(i).eq.'Huron        ') mhu_last = reach_lvl(i)
!        if(resname(i).eq.'StClair      ') stc_last = reach_lvl(i)
!        if(resname(i).eq.'Erie         ') eri_last = reach_lvl(i)
!        if(resname(i).eq.'Ontario      ') ont_last = reach_lvl(i)
      end do

      if(iopt.eq.2)print*,' In sub after definitions'

!     RESET SETS ALL INITIAL VARIABLES

!     CHECK FILES MODE  iopt=99
!     FOR IOPT=99 NL AND MHRD ARE SET TO KT AND THE PROGRAM WILL RUN
!     FOR ONE TIME STEP ONLY - THIS WILL CHECK AND ECHO ALL INPUT FILES
!     >> VERY HANDY FOR CHECKING DATA FILE PRIOR TO LONG RUNS

!     JAN=1 FIRST PASS - SET IN RESET()
!     JAN=2 SUBSEQUENT PASSES
!     JAN=3 LAST PASS  - SET BELOW

      jan=1
      m=1
      tot1=0.0
      totaltime=0.0                ! used for ensim time series

!     These values used to come from read_flow_ef 
      irdt=1     ! initial gues for routing time step in hours 
      kt=1       ! kt=data timestep in hours. 

!     Section added to allow for lengthened routing time step for large grids
      if(irdt.gt.kt)then
        a6=float(irdt)*3600.0
        write(51,*)'Warning'
        write(51,*)'Min time step a6 changed to',a6
        write(51,*)
        write(*,*)'Warning'
        write(*,*)'Min time step a6 changed to',a6
        write(*,*)
        pause 'hit enter to continue - in sub @167'
      endif

      if(iallcnt1.eq.0)then
        iallcnt1=1
      endif   ! iallcnt1=0

      if(iopt.eq.2)print*,'In sub after allocations 1'

!     SINGLE RUN USING SOIL MOISTURE GRID IS THE DEFAULT.
!     NO SOIL MOISTURE OPTIMIZATION - THIS CAN BE CHANGED WITH 
!     SETTING ICASE=-1 IN THE PARAMETER FILE & SM GRID WILL BE IGNORED

!     SAVE THE ORIGINAL VALUE:
      flgevp22=flgevp2
      
      if(iopt.eq.2)print*,'In sub after allocations 3'

      do n=1,naa
        rechrg(n)=0.0
!        qstream & strloss need to be initialized for watroute
        qstream(n)=0.0
        strloss(n)=0.0
!        rh(n)=.50   ! moved to rdtemp 28/12/04 nk
      end do
      
      juold=0

      allocate(outarray(ycount,xcount),stat=iAllocate)
      if(iAllocate.ne.0) STOP
     *    'Error with allocation of ensim arrays in sub'      

!     Initialize all grids for write_r2c
      do i=1,ycount
        do j=1,xcount
          outarray(i,j)=0.0
        end do
      end do

      if(iopt.eq.2)print*,'In sub before writing headers'
      ! only write out these header/r2c files if fstflag isn't 'y'
      if (fstflg /= 'y') then

        author='watroute (rte)'    
        if(modelflg.eq.'i')author='watroute (rte -i)'
        if(modelflg.eq.'r')author='watroute (rte -r)'
        if(modelflg.eq.'l')author='watroute (rte -l)'

        name='Gridded Channel Flow'
        coordsys_temp=coordsys1
        zone_temp=zone1
        datum_temp=datum1
        xorigin_temp=xorigin
        yorigin_temp=yorigin
        xcount_temp=xcount
        ycount_temp=ycount
        xdelta_temp=xdelta
        ydelta_temp=ydelta
        attribute_name='discharge'
        attribute_units='mm' 
        attribute_type='flow'  
        if(modelflg.eq.'i')source_file_name='rff files'
        if(modelflg.eq.'r')source_file_name='rff,rch files'
        if(modelflg.eq.'l')source_file_name='rff,lkg files'
        no_frames=0
        frame_no=0

!       write the header          
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        call write_r2c(56,56,0,1,0,1,1)   
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!       This file is for debugging - so leakage can be compared
!       to qlz from watflood as given in the lkage\yyyymmdd_lkg.r2c file
!       tracer 100 set 

        name='Gridded Leakage'
        attribute_name='Leakage'
        attribute_units='cms' 
        attribute_type='discharge'  
        if(modelflg.eq.'i')source_file_name='rff files'
        if(modelflg.eq.'r')source_file_name='rff,rch files'
        if(modelflg.eq.'l')source_file_name='rff,lkg files'
        no_frames=0
        frame_no=0
        fln(811)='gridded_lkg.r2c'

!       write the header          
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        !call write_r2c(811,811,0,1,0,1,1)   
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        name='Gridded LZS'
        attribute_name='lzs'
        attribute_units='cm' 
        attribute_type='storage'  
        if(modelflg.eq.'i')source_file_name='rff files'
        if(modelflg.eq.'r')source_file_name='rff,rch files'
        if(modelflg.eq.'l')source_file_name='rff,lkg files'
        no_frames=0
        frame_no=0
        fln(812)='gridded_lzs.r2c'

!       write the header          
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        !call write_r2c(812,812,0,1,0,1,1)   
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      endif ! if (fstflg /= 'y')

      if(iopt.eq.2)print*,'In sub, passed location 201'

      no_frames=0

!     RESET TO THE ORIGINAL VALUE -  WILL BE CHANGED IF NO DATA
      flgevp2=flgevp22

      index=1
!     INDEX = 1 FOR FIRST PASS REROUT ONLY
!     INDEX = 2 FOR SUBSEQUENT PASSES
!     SET   = 1 FOR EACH NEW LINKED EVENT TO READ IN NEXT SET OF 
!               RESERVOIR RELEASES

      if(iopt.eq.2)print*,' In sub, passed location 2012'

!     check that a model type has been picked
      if(modelflg.eq.'i'.or.modelflg.eq.'r'
     *                  .or.modelflg.eq.'l')then
!       no problem - continue
      else      
        print*,'modelflg not set to i, r, or l'
        print*,'in the first event file please fix and retry'
        print*
        stop 'program aborted in sub @ 252'
      endif

      if(iopt.eq.2)print*,'In sub, passed location 202'

!     rev. 9.1.60  Jul.  27/04  - NK: reversed definitions for sl1 & sl2 Int. Slope
      do n=1,naa
        !sl2(n)=sqrt(sl1(n)) ! csubich -- this is now set in read_shed_ef
                               ! at the same time as sl1
        if(a4.eq.0)a4=1.0
! * * * TS * * * 
!       CAP IS THE VOLUME OF WATER IN A REACH FOR THE MEAN ANNUAL FLO
!       widep=a11
        ! Compute the channel cross-sectional area based on a rather
        ! complicated fitting formula.  aa2/3/4 are tunable parameters.
        if(aa4(n).gt.0.0)then
          chaxa(n)=(aa2(n)+aa3(n)*da(n)**aa4(n))
        else
!         csubich -- da(n) should never be less than zero, but it can
!         happen if the rank listing is improperly configured
          if (da(n) .le. 0) then
            print *, "WARNING: da(n) is <= 0 at index",n
            print *, "Grid cell:", xxx(n), yyy(n)
            chaxa(n) = 0
            if (xxx(n) .eq. 0 .and. yyy(n) .eq. 0) then
              ! If xxx/yyy are both 0 for this cell,
              ! then we have a missing index.  In theory,
              ! this cell shouldn't affect the rest of the
              ! computation, so all we really want is for
              ! the remaidner of this procedure to not
              ! die with a floating point exception
              widep(n) = 1
              chadep(n) = 1
            end if
          else
!         rev. 9.2.12  Sep.  15/05  - NK: added EXCEL eqn to flowinit
!         EXCEL compatible equation. aa4 must be -ve in the par file
            chaxa(n)=10.0**(aa2(n)*alog10(da(n))+aa3(n))
!         had to put a lower bound on channel area to avoid NaN in resume file
!         NK  Oct. 5/05
          end if
          chaxa(n)=amax1(1.0,chaxa(n))
        endif
        ! Channel capacity is the cross-sectional area times channel length
        cap(n)=chaxa(n)*rl(n)

        ! Since a channel has a deep part plus a shallow, sloping bank,
        ! compute an effective channel depth
        chadep(n)=SQRT(chaxa(n)/widep(n))
        chawid(n)=chaxa(n)/chadep(n)
        flz2(n)=1.0-(1.0-flz(n))

        ! Fix suggested by Frank Saglenkis, based on changes made
        ! to WATFLOOD ca. 2007: if we keep track of biome types
        ! and a grid cell has more water fraction than channel
        ! area, then the channel area calculation must have been
        ! incorrect -- replace the computed channel area with
        ! water_fracion * grid size, and then from that recompute
        ! capacity.

        chaarea(n) = chawid(n)*rl(n) ! Define channel area

        ! Now, check to see if that's sensible, based on land-use

        ! aclass(:,ntype) is the fraction of the grid cell that is water;
        ! this is only enforced at read-in of the shed/par files, and
        ! needs to be properly maintained.
        if (ntype .ge. 1 .and. aclass(n,ntype) 
     *           .gt. chaarea(n)/grid_area(n)) then 
          ! Replace the areas with ones based on the land-use data
          chaarea(n) = grid_area(n)*aclass(n,ntype)
          chawid(n) = chaarea(n)/rl(n) ! New width using the same effective depth
          cap(n) = chaarea(n)*chadep(n)
          ! Leave chaxa untouched for now, this may be a mistake.
          ! csubich -- experimental: update chaxa appropriately also
          chaxa(n) = cap(n)/rl(n) ! Capacity divided by length
        endif

      end do ! do n=1,naa

      if(iopt.eq.2)print*,'In sub, passed location 207'

!     RESET THE CLOCK:   >>>>>>>>>>>> CHECK THIS OUT
      m=1
      tot1=0.0

      if(iopt.eq.2)print*,'In sub, passed location 209'

      if(iopt.eq.2)print*,'In sub, gone to flowinit'

c          call flowinit()
!          replaced  Oct. 9/06  nk
!          initialize channel flows & storages
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (len_trim(infln(4+Nreaches)) .gt. 2) then
        fln(99)=infln(4+Nreaches)  ! The filename index of flow_init.fst varies with the number of reaches processed
      else
        fln(99)='flow_init.r2c'
      end if
      if (fstflg .eq. 'y') then
        ! Read flow_init from the .fst file, using today
        !flnname(1:13)='flow_init.fst'
        call read_flowinit_fst(611,fln(99),!flnname,
     *                         year1,month1,day1,hour1)
c        call read_flowinit_fst(611,'flow_init.fst',!flnname,
c     *                         year1,month1,day1,hour1)
      else
        call read_flowinit_ef()
      endif
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      if(iopt.eq.2)print*,'In sub, back from read_flowinit_ef()'

!     rev. 9.2.07  Jul.  29/05  - NK: soilinit moved from runoff to sub 

      tdum=1000.*step2/3600.   !cms/mm conversion

!     what class is the water class?
      ii_water=ntype

!     END OF INITIALIZATION SECTION
 
!     * * * * * * * *  EVENT LOOP START  * * * * * * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP START  * * * * * * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP START  * * * * * * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP START  * * * * * * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP START  * * * * * * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP START  * * * * * * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP START  * * * * * * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP START  * * * * * * * * * * * * * * *

      DO id=1,ni

!     ***************************************************************

      print*,'*********************************************************'
      print*,'*                                                       *'
      print*,'*           RRRRRRR   TTTTTTTT  EEEEEEE                 *'
      print*,'*           RRRRRRRR  TTTTTTTT  EEEEEEE                 *'
      print*,'*           RR    RR     TT     EE                      *'
      print*,'*           RR    RR     TT     EE                      *'
      print*,'*           RRRRRRRR     TT     EEEE                    *'
      print*,'*           RRRRRRR      TT     EEEE                    *'
      print*,'*           RR   RR      TT     EE                      *'
      print*,'*           RR    RR     TT     EEEEEEE                 *'
      print*,'*           RR     RR    TT     EEEEEEE                 *'
      print*,'*                                                       *'
      print*,'*                  WATFLOOD (TM)                        *'
      print*,'*           Version BETA    July, 2007                  *'
      print*,'*           (c) N. Kouwen, 1972-2007                    *'
      print*,'*                                                       *'
      print*,'*********************************************************'
      print*

        if(iopt.eq.2)print*,'id=',id

!       READ THE EVENT FILE - HAS TO BE DONE FOR EACH ID

        if(id.gt.1)then
          fln(99)=fln(100+id)
!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          call rdevt(date,conv,scale,smc5,nhr,nhf)
!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        endif


        call getarg(1,strfw_option)

        if (trim(strfw_option)=='streamflow_insertion' .or. 
     *      trim(strfw_option)=='streamflow_comparison' ) then 
          if(iopt.eq.2)print*,'In sub - gone to read_flow_ef'
!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          call read_flow_ef()  !EnSim compatible tb0 file
!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!         'read_flow_ef' may reset kt and irdt;
!         temporary solution: set irdt=kt (D. Deacu)
!
          if (kt.eq.1) then
            if(irdt.ne.kt) irdt=1
          else
            print *, 'kt:', kt, 'irdt:', irdt
            stop 'Program aborted in sub after the call to read_flow_ef'
          endif
          if (trim(strfw_option)=='streamflow_insertion') then
            print *,'This is a run with streamflow insertion.'
          elseif (trim(strfw_option)=='streamflow_comparison') then
            print *,'This is a run without streamflow insertion.'
            print *,'  However, observed streamflows are read in and'
            print *,'  and then written out alongside flows simulated '
            print *,'  at the stations for comparison purposes.'
          end if
        else
          print*,'This is a run WITHOUT streamflow insertion.'
          print*,'No values are read in from the streamflow files.'
        endif

        if(iopt.eq.2)print*,'In sub - gone to read_resv_ef'

!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        call read_resv_ef()  !EnSim compatible tb0 file
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if(iopt.eq.2)print*,'In sub - back from read_resv_ef'

!       WATROUTE START  WATROUTE START  WATROUTE START  WATROUTE START

!       read the headers (open files also) :

        if(modelflg.ne.'n')then

!         read the header in the runoff file:
!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          if (fstflg .eq. 'y') then
            call read_fst(261,fln(31),'1','RFF ',
     *                    year1,month_now,day_now,-1)
          else 
            call read_r2c(261,31,'1')
          endif
          if(xcount.ne.xcount_temp.or.ycount.ne.ycount_temp)then
            print*,'runoff grid size does not match the shed grid'
            print*
            stop 'Program aborted in sub @ 371'
          endif

          if(modelflg.eq.'r')then

!           read the header in the baseflow file:
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if (fstflg .eq. 'y') then
              call read_fst(262,fln(32),'1','RCH ',
     *                      year1,month_now,day_now,-1)
            else 
              call read_r2c(262,32,'1')
            endif
            if(xcount.ne.xcount_temp.or.ycount.ne.ycount_temp)then
              print*,'recharge grid size does not match the shed grid'
              print*
              stop 'Program aborted in sub @ 379'
            endif

          elseif(modelflg.eq.'l')then
!           read the header in the leakage file:

!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            call read_r2c(263,33,'1')
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

            if(xcount.ne.xcount_temp.or.ycount.ne.ycount_temp)then
              print*,'leakage grid size does not match the shed grid'
              print*
              stop 'Program aborted in sub @ 387'
            endif

          endif
        endif ! if(modelflg.ne.'n')then

!       WATROUTE END   WATROUTE END    WATROUTE END    WATROUTE END 

        if(iopt.eq.2)print*,' In sub, passed location  244'

!       TIMER SETS ALL THE CLOCKS i.e. SUM HOURS, SUM SECONDS, ETC.

        time=0.0
        m=1

        if(iopt.eq.99)then
!         THIS OPTION IS TO CHECK ALL INPUT FILES
!         see above for more
          mhtot=kt*2
        endif

!       Commented out by csubich
        !if(iopt.eq.2)pause 'before time loop'

! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
!           THIS IS THE MAIN TIME LOOP, EXECUTED FOR EACH TIME STEP
!           
!           START TIME LOOP
!           
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

        a6=900.    ! minimum time step (in par file)
        ! Christopher Subich (9/12): A minimum time step of 900s
        ! is too large for some tested grids, so inclulde here a
        ! sample (smaller) value that happened to work on one
        ! grid that I used.  This is commented out to preserve
        ! compatibility with other users.
        !a6=45.00
        a66=a6

!       so we have to just go to the endof the yyyymmdd_rff.r2c file and quit

        found_data_end=.false.

        DO WHILE(.NOT.found_data_end)

!         the -1 is because time starts at 0.0

          time=time+1.000
          totaltime=totaltime+1.0

          if(iopt.eq.2)print*,'Gone to timer'

!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          call timer(iz,jz,mz,clock,time,t,thr,dtmin,dtmax,div,m,
     *               ju,a66)
!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

          if(iopt.eq.2)print*,'Back from timer - time=',time
          print*,'time=',time,' hours'

          if(iz.lt.jz)then 

!           STATUS LINE:
            if(iopt.lt.99)then
              if(mod(jz,120).eq.0)then
                write(*,5003)id,ni,jz
              endif
            endif

!       WATROUTE input  WATROUTE input  WATROUTE input  WATROUTE input

            if(iz.lt.jz)then
      
!             WATROUTE only <<<<<< !!!!!!
!             the headers have been read above.
!             surface flow is always routed
!             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              if (fstflg .eq. 'y') then
                call read_fst(261,fln(31),'0','RFF ',
     *                 year1,month_now,day_now,hour_now)
                if (found_data_end) then
                  exit
                endif
              else 
                call read_r2c(261,31,'0')
              endif
!             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!             vectorize & convert mm to flow
              do n=1,naa
                i=yyy(n)
                j=xxx(n)
                ! read_fst uses the transpose of the array, from
                ! the point of view of code built for read_r2c
                if (fstflg .eq. 'y') then
                  qr(n)=inarray(j,i)*tdum*frac(n)
                else 
                  qr(n)=inarray(i,j)*tdum*frac(n)
                endif
              end do

              if(modelflg.eq.'r')then
!               read the recharge and route through the lz
!               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                if (fstflg .eq. 'y') then
                  call read_fst(262,fln(32),'0','RCH ',
     *                 year1, month_now, day_now, hour_now)
                  if (found_data_end) then
                    exit
                  endif
                else
                  call read_r2c(262,32,'0')
                endif
!               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!               vectorize & convert mm to flow
!               recharge is added to lzs
                do n=1,naa
                  i=yyy(n)
                  j=xxx(n)
                  if (fstflg .eq. 'y') then
                    lzs(n)=lzs(n)+inarray(j,i)
                  else
                    lzs(n)=lzs(n)+inarray(i,j)
                  endif
!                 route the recharge thru the lz:
!                 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  call baseflow(n,dlz,sdlz,tdum)
!                 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  qr(n)=qr(n)+qlz(n)
                end do
              endif

              if(modelflg.eq.'l')then
!               read qlz = groundwater flow (leakage/baseflow)
!               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                call read_r2c(263,33,'0')
!               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!               vectorize & convert mm to flow
                do n=1,naa
                  i=yyy(n)
                  j=xxx(n)
                  qr(n)=qr(n)+inarray(i,j)*tdum*frac(n)
                end do
              endif

            endif ! if(iz.lt.jz)then : 2nd time 

            juold=ju
            jan=2

!           JAN HAS TO STAY HERE BECAUSE ROUTE MAY BE CALLED MORE
!           THAN FOR EACH TIME RUNOF5 IS CALLED - SO WE CAN'T USE 
!           JAN IN ROUTE.

          endif  ! if(iz.lt.jz)then : 1st time        ! CALL read_r2c NEW HOUR 

!         ROUTE ROUTES WATER THROUGH EACH SQUARE USING STORAGE ROUTING
!         RESET QDWPR=0 INCASE THIS IS SECOND DT DURING FLOW INCREMENT

!         >>>>>>>IS THIS LOOK OK?? CHANGED 6 TO NORESVO     
 
!         write reach inflow to a file instead of routing it 
!         qdwpr is for dwoper formats but can be changed to any other 
!         format say for river_1d

!         rev. 9.3.11  Feb.  17/07  - NK: force hourly time steps for runoff
!         rev. 9.3.12  Feb.  20/07  - NK: changed dtmin & call to route

          dtmin=900.0
          !dtmin=45.00 ! csubich -- set small minimum time step
          no_dt=max(int(3599./dtmin)+1,1)
          route_dt=3600.0/float(no_dt)
          sec_div=route_dt/2.0
          hr_div=sec_div/3600.

!dch
!          dtmin=3600.0

!         The value of dtmin has been used to determine how many
!         times route is called. Route will determine a new dtmin
!         for the next time step.

          do n=1,no_dt

!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            call route(sec_div,hr_div,dtmin,jz,n,time,date)
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

! Chris Subich email of 20140219:
! qi2 is the channel-based inflow at the end (?) of the timestep
! qi1 is the inflow at the beginning of the timestep
! qr is the combination of incoming runoff ('RFF') and baseflow (computed by the 'baseflow' subroutine after including the input 'RCH' recharge)
! the outflow variables ('qo')
            if (n.eq.no_dt) then

              do i=1,Nreaches
                somme_qi(i) = 0.0
                somme_qr(i) = 0.0
              end do

              do m=1,naa
                fnumstr_qi = 64
                fnumstr_qr = 81
                if (ireach(m) .eq. 0) then
                  do i=1,Nreaches
                    if (ireach(next(m)) .eq. i) then
                      somme_qi(i) = somme_qi(i) + qi2(m)
                      fnum = fnumstr_qi + i - 1
                      write(fnum,8003) m,xxx(m),yyy(m),qi2(m),    ! loop # (i.e. hour?),x-coord?,y-coord?,inflow?,reach #
     *                                   ireach(next(m))
                    end if
                  end do
                else
                  do i=1,Nreaches
                    if (ireach(m) .eq. i) then
                      somme_qr(i) = somme_qr(i) + qr(m)
                      fnum = fnumstr_qr + i - 1
                      write(fnum,8003) m,xxx(m),yyy(m),qr(m),ireach(m)
                    end if
                  end do
                end if
              end do ! do m=1,naa

              fnum = fnumstr_qi + Nreachesmax
              write(fnum,8003) (somme_qi(i),i=1,Nreaches)
              fnum = fnumstr_qr + Nreachesmax
              write(fnum,8003) (somme_qr(i),i=1,Nreaches)

            endif !if (n.eq.no_dt)


          end do  ! do n=1,no_dt

          if(iopt.eq.2)print*,'Back from route'

! Write lake water budget info to output file lake_sd.csv (unit 58)
!! Write lake water budget info to output file lake_sd.csv (unit 58) and net_lake_inflow.csv (unit 59)
!          if(resname(1).eq.'Superior')then
!           output - for GLAKE
            if(noresv.gt.0)then
              write(58,8001)(lake_elv(l,jz),lake_stor(l,jz),
     *                       lake_inflow(l,jz),
     *                       lake_outflow(l,jz),
     *                       del_stor(l,jz),l=1,noresv)
!     *                       lake_inflow(l,jz),net_lake_inflow(l,jz),
!     *                       lake_outflow(l,jz),net_lake_outflow(l,jz),
!              write(59,8002)(net_lake_inflow(l,jz),l=1,noresv)
            endif
!          endif

!         write to r2c file every hour
!         take out conditional will write each time route is called
!         need to fix time stamp for frame header if this is done
!         rev. 9.4.01  Apr.  17/07  - NK: added deltat_report for gridflow.r2c
!         deltat_report is read from the event file

          if(mod(jz,deltat_report).eq.0)then

            do n=1,naa
              i=yyy(n)
              j=xxx(n)
              outarray(i,j)=qo2(n)
            end do     

!           trick to keep file open - it's closed after id loop below
            frame_no=frame_no+1
            no_frames=frame_no+1
            
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if (fstflg .eq. 'y') then
              call write_fst(59,filename(59),'DISC',
     *                       year1,month_now,day_now,hour_now)
            else
              call write_r2c(56,56,no_frames,1,frame_no,1,8) 
            endif
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

          endif !if(mod(jz,deltat_report).eq.0)

   82     m=m+1

   
! DD
!      if (time .eq. 3) found_data_end=.TRUE.
        END DO    ! DO WHILE(.NOT.found_data_end) 

! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
!
!           END TIME LOOP
!
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

        write(*,5003)id,ni,jz
  
        do i=1,Nreaches
          reach_last(i)=lake_elv(i,24)
          write(500+i,*) reach_last(i)
!          if(resname(i).eq.'Superior     ') sup_last = lake_elv(i,24)  ! Read from the reservoir release file
!          if(resname(i).eq.'Huron        ') mhu_last = lake_elv(i,24)
!          if(resname(i).eq.'StClair      ') stc_last = lake_elv(i,24)
!          if(resname(i).eq.'Erie         ') eri_last = lake_elv(i,24)
!          if(resname(i).eq.'Ontario      ') ont_last = lake_elv(i,24)
        end do

!       close files:
        print*
        close(unit=261,status='keep')
        write(*,'(a26,a)') 'Closed unit 261 filename=', trim(fln(31))
        if(modelflg.eq.'r')then
          close(unit=262,status='keep')
          write(*,'(a26,a)') 'Closed unit 262 filename=', trim(fln(32))
        elseif(modelflg.eq.'l')then
          close(unit=263,status='keep')
          write(*,'(a26,a)') 'Closed unit 263 filename=', trim(fln(33))
        endif

      END DO ! DO id=1,ni
 
!     * * * * * * * *  EVENT LOOP END  * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP END  * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP END  * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP END  * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP END  * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP END  * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP END  * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP END  * * * * * * * * * *

!      close(unit=56,status='keep')
!      print*
!      print*,'Closed unit 56'
!      print*,'Output file',fln(56)
!      print*,'written in the working directory'

!     write flowinit to resume routing

      author='rte.exe'

!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (fstflg .eq. 'y') then
        fln(99)=infln(Nreaches+4)
        ! Write out flowinit with hour_now-1, because the hour
        ! was incremented by the timer routine -before- we saw
        ! that the run was complete/no more recharge/runoff.
        call write_flowinit_fst(611,fln(99),
     *                  year1,month_now,day_now,hour_now-1)
!        call write_flowinit_fst(611,'flow_init.fst',
!     *                  year1,month_now,day_now,hour_now-1)
      else
        call write_flowinit()
      endif
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      RETURN

! FORMATS

      do i=1,Nreaches
        close(unit=500+i)
      end do

 5003 format('+',1x,'id=',i3,'/',i3,' mz=',i5)
 8001 format(g14.6,999(',',g14.6))
 8002 format(g14.6,999(',',g14.6))
 8003 format(g14.6,999(' ',g14.6))

      END SUBROUTINE sub

