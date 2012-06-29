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

      implicit none

!     SAVES THE LOCAL VARIABLES FROM ONE RUN TO NEXT
      SAVE

      DIMENSION     :: smc5(16)
      CHARACTER(14) :: date
      CHARACTER(3)  :: eofmark
      CHARACTER(1)  :: lineflg,smok
      CHARACTER(20) :: junk 
      REAL(4)	    :: optlow,time,tot1,qwert,conv,scale,
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
      REAL          :: ha,fpw,kdn,nratio,
     *                 somme82,somme83,somme84,somme85,somme86,
     *                 somme92,somme93,somme94,somme95,somme96

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

!dch
      REAL :: sup_lvl(1),mhu_lvl(1),stc_lvl(1),eri_lvl(1),ont_lvl(1)

      open(unit=75,file='sup_lvl.txt',status='old')
      open(unit=76,file='mhu_lvl.txt',status='old')
      open(unit=77,file='stc_lvl.txt',status='old')
      open(unit=78,file='eri_lvl.txt',status='old')
      open(unit=79,file='ont_lvl.txt',status='old')

      read(75,*) sup_lvl(1)
      read(76,*) mhu_lvl(1)
      read(77,*) stc_lvl(1)
      read(78,*) eri_lvl(1)
      read(79,*) ont_lvl(1)

      sup_last=sup_lvl(1)
      mhu_last=mhu_lvl(1)
      stc_last=stc_lvl(1)
      eri_last=eri_lvl(1)
      ont_last=ont_lvl(1)

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
      totaltime=0.0		! used for ensim time series

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

!     write the header          
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      call write_r2c(72,72,0,1,0,1,1)   
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!     This file is for debugging - so leakage can be compared
!     to qlz from watflood as given in the lkage\yyyymmdd_lkg.r2c file
!     tracer 100 set 

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

!     write the header          
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      call write_r2c(811,811,0,1,0,1,1)   
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

!     write the header          
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      call write_r2c(812,812,0,1,0,1,1)   
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if(iopt.eq.2)print*,'In sub, passed location 201'

	no_frames=0

!       RESET TO THE ORIGINAL VALUE -  WILL BE CHANGED IF NO DATA
        flgevp2=flgevp22

        index=1
!       INDEX = 1 FOR FIRST PASS REROUT ONLY
!       INDEX = 2 FOR SUBSEQUENT PASSES
!       SET   = 1 FOR EACH NEW LINKED EVENT TO READ IN NEXT SET OF 
!                 RESERVOIR RELEASES

        if(iopt.eq.2)print*,' In sub, passed location 2012'

!       check that a model type has been picked
        if(modelflg.eq.'i'.or.modelflg.eq.'r'.
     *                        or.modelflg.eq.'l')then
!         no problem - continue
        else      
          print*,'modelflg not set to i, r, or l'
          print*,'in the first event file please fix and retry'
          print*
          stop 'program aborted in sub @ 252'
        endif

        if(iopt.eq.2)print*,'In sub, passed location 202'

!       rev. 9.1.60  Jul.  27/04  - NK: reversed definitions for sl1 & sl2 Int. Slope
        do n=1,naa
          sl2(n)=sqrt(sl1(n))
          if(a4.eq.0)a4=1.0
! * * *   TS * * * 
!         CAP IS THE VOLUME OF WATER IN A REACH FOR THE MEAN ANNUAL FLO
!         widep=a11
          if(aa4(n).gt.0.0)then
            chaxa(n)=(aa2(n)+aa3(n)*da(n)**aa4(n))
          else
!           rev. 9.2.12  Sep.  15/05  - NK: added EXCEL eqn to flowinit
!           EXCEL compatible equation. aa4 must be -ve in the par file
            chaxa(n)=10.0**(aa2(n)*alog10(da(n))+aa3(n))
!           had to put a lower bound on channel area to avoid NaN in resume file
!           NK  Oct. 5/05
            chaxa(n)=amax1(1.0,chaxa(n))
          endif
	    cap(n)=chaxa(n)*rl(n)
            chadep(n)=SQRT(chaxa(n)/widep(n))
	    chawid(n)=chaxa(n)/chadep(n)
            flz2(n)=1.0-(1.0-flz(n))
        end do

        if(iopt.eq.2)print*,'In sub, passed location 207'

!       RESET THE CLOCK:   >>>>>>>>>>>> CHECK THIS OUT
        m=1
        tot1=0.0

        if(iopt.eq.2)print*,'In sub, passed location 209'

	if(iopt.eq.2)print*,'In sub, gone to flowinit'

c            call flowinit()
!            replaced  Oct. 9/06  nk
!            initialize channel flows & storages
             fln(99)='flow_init.r2c'
!            ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
             call read_flowinit_ef()
!            ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
	if(iopt.eq.2)print*,'In sub, back from read_flowinit_ef()'

!     rev. 9.2.07  Jul.  29/05  - NK: soilinit moved from runoff to sub 

            tdum=1000.*step2/3600.   !cms/mm conversion

!     what class is the water class?
            ii_water=ntype

!       END OF INITIALIZATION SECTION
 
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
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           call read_flow_ef()  !EnSim compatible tb0 file
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!       'read_flow_ef' may reset kt and irdt;
!       temporary solution: set irdt=kt (D. Deacu)
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

!        read the header in the runoff file:
!        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         call read_r2c(261,31,'1')
!        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	  if(xcount.ne.xcount_temp.or.ycount.ne.ycount_temp)then
	    print*,'runoff grid size does not match the shed grid'
	    print*
	    stop 'Program aborted in sub @ 371'
	  endif

         if(modelflg.eq.'r')then

!         read the header in the baseflow file:
!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          call read_r2c(262,32,'1')
!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	  if(xcount.ne.xcount_temp.or.ycount.ne.ycount_temp)then
	    print*,'recharge grid size does not match the shed grid'
	    print*
	    stop 'Program aborted in sub @ 379'
	  endif

         elseif(modelflg.eq.'l')then
!         read the header in the leakage file:

!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          call read_r2c(263,33,'1')
!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	  if(xcount.ne.xcount_temp.or.ycount.ne.ycount_temp)then
	    print*,'leakage grid size does not match the shed grid'
	    print*
	    stop 'Program aborted in sub @ 387'
	  endif

         endif

	endif

!       WATROUTE END   WATROUTE END    WATROUTE END    WATROUTE END 

        if(iopt.eq.2)print*,' In sub, passed location  244'

!           TIMER SETS ALL THE CLOCKS i.e. SUM HOURS, SUM SECONDS, ETC.

        time=0.0
	m=1

        if(iopt.eq.99)then
!         THIS OPTION IS TO CHECK ALL INPUT FILES
!         see above for more
          mhtot=kt*2
        endif

        if(iopt.eq.2)pause 'before time loop'

! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
!           THIS IS THE MAIN TIME LOOP, EXECUTED FOR EACH TIME STEP
!           
!           START TIME LOOP
!           
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


      a6=900.    ! minimum time step (in par file)
      a66=a6

!     so we have to just go to the endof the yyyymmdd_rff.r2c file and quit

         found_data_end=.false.

         DO WHILE(.NOT.found_data_end)

!         the -1 is because time starts at 0.0

          time=time+1.000
	  totaltime=totaltime+1.0

          if(iopt.eq.2)print*,'Gone to timer'

!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          call timer(iz,jz,mz,clock,time,t,thr,dtmin,dtmax,div,m,
     *                    ju,a66)
!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

          if(iopt.eq.2)print*,'Back from timer - time=',time

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
              call read_r2c(261,31,'0')
!             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!             vectorize & convert mm to flow
              do n=1,naa
	        i=yyy(n)
	        j=xxx(n)
                qr(n)=inarray(i,j)*tdum*frac(n)
	      end do

              if(modelflg.eq.'r')then
!               read the recharge and route through the lz
!               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                call read_r2c(262,32,'0')
!               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!               vectorize & convert mm to flow
!               recharge is added to lzs
                do n=1,naa
	          i=yyy(n)
	          j=xxx(n)
                  lzs(n)=lzs(n)+inarray(i,j)
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

          endif  

            juold=ju
            jan=2

!           JAN HAS TO STAY HERE BECAUSE ROUTE MAY BE CALLED MORE
!           THAN FOR EACH TIME RUNOF5 IS CALLED - SO WE CAN'T USE 
!           JAN IN ROUTE.

          endif         ! CALL read_r2c NEW HOUR 

!         ROUTE ROUTES WATER THROUGH EACH SQUARE USING STORAGE ROUTING
!         RESET QDWPR=0 INCASE THIS IS SECOND DT DURING FLOW INCREMENT

!         >>>>>>>IS THIS LOOK OK?? CHANGED 6 TO NORESVO     
 
!         write reach inflow to a file instead of routing it 
!         qdwpr is for dwoper formats but can be changed to any other 
!         format say for river_1d

!     rev. 9.3.11  Feb.  17/07  - NK: force hourly time steps for runoff
!     rev. 9.3.12  Feb.  20/07  - NK: changed dtmin & call to route

              dtmin=900.0
              no_dt=max(int(3599./dtmin)+1,1)
              route_dt=3600.0/float(no_dt)
              sec_div=route_dt/2.0
	      hr_div=sec_div/3600.

!dch
c              dtmin=3600.0

!             The value of dtmin has been used to determine how many
!             times route is called. Route will determine a new dtmin
!             for the next time step.

              do n=1,no_dt

!               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                call route(sec_div,hr_div,dtmin,jz,n,time,date)
!               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!dch
               if (n.eq.no_dt) then

                 somme82=0.0
                 somme83=0.0
                 somme84=0.0
                 somme85=0.0
                 somme86=0.0
                 somme92=0.0
                 somme93=0.0
                 somme94=0.0
                 somme95=0.0
                 somme96=0.0

                 do m=1,naa

      if(ireach(m).eq.0.and.ireach(next(m)).eq.1.)then
        somme82=somme82+qi2(m)
        write(82,8003)(m,xxx(m),yyy(m),qi2(m),ireach(next(m)))
      elseif(ireach(m).eq.0.and.ireach(next(m)).eq.2.)then
        somme83=somme83+qi2(m)
        write(83,8003)(m,xxx(m),yyy(m),qi2(m),ireach(next(m)))
      elseif(ireach(m).eq.0.and.ireach(next(m)).eq.3.)then
        somme84=somme84+qi2(m)
        write(84,8003)(m,xxx(m),yyy(m),qi2(m),ireach(next(m)))
      elseif(ireach(m).eq.0.and.ireach(next(m)).eq.4.)then
        somme85=somme85+qi2(m)
        write(85,8003)(m,xxx(m),yyy(m),qi2(m),ireach(next(m)))
      elseif(ireach(m).eq.0.and.ireach(next(m)).eq.5.)then
        somme86=somme86+qi2(m)
        write(86,8003)(m,xxx(m),yyy(m),qi2(m),ireach(next(m)))
      elseif(ireach(m).eq.1)then
        somme92=somme92+qr(m)
        write(92,8003)(m,xxx(m),yyy(m),qr(m),ireach(m))
      elseif(ireach(m).eq.2)then
        somme93=somme93+qr(m)
        write(93,8003)(m,xxx(m),yyy(m),qr(m),ireach(m))
      elseif(ireach(m).eq.3)then
        somme94=somme94+qr(m)
        write(94,8003)(m,xxx(m),yyy(m),qr(m),ireach(m))
      elseif(ireach(m).eq.4)then
        somme95=somme95+qr(m)
        write(95,8003)(m,xxx(m),yyy(m),qr(m),ireach(m))
      elseif(ireach(m).eq.5)then
        somme96=somme96+qr(m)
        write(96,8003)(m,xxx(m),yyy(m),qr(m),ireach(m))
      endif

                 enddo

        write(87,8003)(somme82,somme83,somme84,somme85,somme86)
        write(97,8003)(somme92,somme93,somme94,somme95,somme96)

               endif

 8003   format(g14.6,999(' ',g14.6))

              end do

          if(iopt.eq.2)print*,'Back from route'

            if(resname(1).eq.'Superior')then
!         output - for GLAKE
              if(noresv.gt.0)then
                write(80,8001)(lake_elv(l,jz),lake_stor(l,jz),
     *       lake_inflow(l,jz),net_lake_inflow(l,jz),lake_outflow(l,jz),
     *       net_lake_outflow(l,jz),del_stor(l,jz),l=1,noresv)
 8001           format(g14.6,999(',',g14.6))
                write(81,8002)(net_lake_inflow(l,jz),l=1,5)
 8002           format(g14.6,999(',',g14.6))
              endif
            endif

!         write to r2c file every hour
!         take out conditional will write each time route is called
!         need to fix time stamp for frame header if this is done
!     rev. 9.4.01  Apr.  17/07  - NK: added deltat_report for gridflow.r2c
!         deltat_report is read from the event file

          if(mod(jz,deltat_report).eq.0)then

            do n=1,naa
               i=yyy(n)
               j=xxx(n)
               outarray(i,j)=qo2(n)
            end do     

!            trick to keep file open - it's closed after id loop below
             frame_no=frame_no+1
             no_frames=frame_no+1
            
!            ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
             call write_r2c(72,72,no_frames,1,frame_no,1,8) 
              
!            ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

c             if(resname(l).eq.'Superior     ')then
!            for the great lakes only:

            do n=1,naa
               i=yyy(n)
               j=xxx(n)
               outarray(i,j)=qlz(n)
            end do     

!            ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
             call write_r2c(811,811,no_frames,1,frame_no,1,8)  
!            ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

            do n=1,naa
               i=yyy(n)
               j=xxx(n)
               outarray(i,j)=lzs(n)
            end do     

!            ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
             call write_r2c(812,812,no_frames,1,frame_no,1,8)  
!            ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

          endif

   82     m=m+1

         END DO    ! while(time.lt.float(mhtot)

! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
!
!           END TIME LOOP
!
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

        write(*,5003)id,ni,jz
  
        sup_last=lake_elv(1,24)
        mhu_last=lake_elv(2,24)
        stc_last=lake_elv(3,24)
        eri_last=lake_elv(4,24)
        ont_last=lake_elv(5,24)

        write(75,*) sup_last
        write(76,*) mhu_last
        write(77,*) stc_last
        write(78,*) eri_last
        write(79,*) ont_last

!       close files:
        print*
        close(unit=261,status='keep')
        print*,'Closed unit 261 filename=',fln(31)
        if(modelflg.eq.'r')then
          close(unit=262,status='keep')
	  print*,'Closed unit 262 filename=',fln(32)
        elseif(modelflg.eq.'l')then
          close(unit=263,status='keep')
          print*,'Closed unit 263 filename=',fln(33)
        endif

      END DO
 
!     * * * * * * * *  EVENT LOOP END  * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP END  * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP END  * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP END  * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP END  * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP END  * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP END  * * * * * * * * * *
!     * * * * * * * *  EVENT LOOP END  * * * * * * * * * *

      close(unit=72,status='keep')
      print*
      print*,'Closed unit 72'
      print*,'Output file',fln(72)
      print*,'written in the working directory'

!     write flowinit to resume routing

      author='rte.exe'

!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      call write_flowinit()
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      RETURN

! FORMATS

      close(unit=75)
      close(unit=76)
      close(unit=77)
      close(unit=78)
      close(unit=79)

 5003 format('+',1x,'id=',i3,'/',i3,' mz=',i5)

      END SUBROUTINE sub

