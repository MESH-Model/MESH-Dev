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

      SUBROUTINE rerout(n,div,thr,l,jz,at,dtmin,date,firstpass)

!***********************************************************************
!       copyright (c) by Nick Kouwen 1987-2007
!***********************************************************************

!***********************************************************************
!   THIS S/R  ROUTES WATER THROUGH A RESERVOIR
!   or SIMPLY PASSES ENTERED FLOWS TO THE RIVER
!
!     REV. 7.80   Oct.  29/96 -  SPL7 ADDED YYMMDD.RIN FOR RES INFLOWS
!                             -  UNIT = 39   FLN = 09
!     rev  9.1.03  July  24/01  - added polinomial to reservoir routing
!     rev. 8.99l  Oct.    2001-     fixed reservoir release timing
!     rev. 8.99n  Dec. 31/2001-     fixed nat. res initial flow (JW)
!     rev. 9.1.11  Feb.  07/02  - fixed bug in reservoir routing 
!     rev. 9.1.13  Mar.  23/02  - fixed resv. timing, moved to beginning of dt
!     rev. 9.1.56  Jun.  18/04  - NK: write new rel & rin files to resrl\newfmt folder.
!     rev. 9.1.59  Jul.  15/04  - NK: split rerout into two parts: rdresv & rerout
!     rev. 9.2.39  May.  09/06  - NK: thr added to route & rerout arg list
!     rev. 9.4.11  Jun.  22/07  - NK: reordered rerout for glake 
!***********************************************************************

      use area_watflood
	implicit none

      Integer  :: ios,nnu,j,k,nrr,i,n,l,ic,jm,jz
	integer  :: newrel,newrin
      integer  :: dayrad(12)
      real*4   :: old,hold,wt,dtmin,at,div,thr
      real*4   :: sup,mhu,stc,eri,ont,mean_elv,delta_elv
      real*4   :: sup_init,mhu_init,stc_init,eri_init,ont_init
      real*4   :: retard_factor(12,5)  ! for great lakes ice-weed retardation
      real*4   :: monthly_evap(12,5),hourly_evap(12,5)    ! for great lakes evap
!     rev. 9.1.55  Jun.  12/04  - NK: write new str files to strfw\newfmt folder.
      LOGICAL exists
      character(20) :: junk
      character(30) :: newfilename
c      character(10) :: fileformat,starttime,startdate
      character(10) :: fileformat
	character(14) :: date
      character*1 :: firstpass

!     SAVES THE LOCAL VARIABLES FROM ONE RUN TO NEXT
      SAVE

c	DATA firstpass/'y'/

!     Used for glake model only:
      DATA retard_factor/0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.,0.,
     * 590.,480.,110.,110.,0.0,0.0,0.0,0.0,0.0,0.0,0.0,110.,
     * 650.,510.,110.,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,140.,
     * 110.,140.,80.,140.,0.,60.,230.,140.,80.,60.,0.0,0.0,
     * 170.,300.,150.,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,10./

!     Initial water elevations
	data sup_init,mhu_init,stc_init,eri_init,ont_init/
     *      183.2,175.98,174.8,174.01,74.61/

!     see s/r lake_evap.f for lake evaporation model

!       CHARACTER(12) :: resname(25),resnamei(25)  !to area 5  NK

!     WHEN LOCAL IS -VE, RESERVOIR RELEASES ARE NEED IN AND THESE 
!     BECOME THE OUTFLOWS OF THE SQUARE IN WHICH THE DAM OUTLET 
!     STRUCTURE IS LOCATED.  
!     WHEN LOCAL = 0 , THERE ARE NO RESERVOIRS, REROUT IS NOT CALLED.

!     WHEN LOCAL IS +VE, THE INFLOW TO THE SQUARE IN WHICH THE
!     RESERVOIR IS LOCATED IS PRINTED OUT ON FILE 11 FOR USE IN
!     HEC-5 OR OTHER OPERATING PROGRAM.

!     THE OVERALL OPERATING SEQUENCE IS THEN THE FIRST RUN SIMPLE
!     TO CALCULATE THE RUNOFF PRODUCED IN VARIOUS SUB-BASINS THEN
!     TO DETERMINE RELEASES, AND THEN TO ROUTE THE RELEASES AND LOCAL
!     INFLOWS USING HYMO.  THE RELEASES ARE READ IN FROM FILE 12.


      nnu=0

      if(iopt.eq.2)print*,'in rerout passed 58'

!     index = 1 for first pass each new chained event
!     index = 2 for subsequent passes. set in sub

      if(iopt.eq.2)print*,'in rerout passed 615'

!     rev. 9.1.11  Feb.  07/02  - fixed bug in reservoir routing 

      store1(n)=store2(n)   !  moved from below 'if'  09/11/04 nk

      if(resname(l).eq.'Superior     ')then

!       Lake Superior
        if(firstpass.eq.'y')then
!         initialize storage	   
!         storage = live storage 
	    sup=sup_init
	    store1(n)=(sup-181.43)*82.1e+09
	    store2(n)=store1(n)
          qo2(n)=824.7*(sup-181.43)**1.5-retard_factor(mo1,1)        
          qo1(n)=qo2(n)
	  endif

        qo2(n)=824.7*(sup-181.43)**1.5-retard_factor(mo1,1)    
         
        store2(n)=store1(n)+(qi1(n)+qi2(n)-qo1(n)-qo2(n))*div
c     *        -hourly_evap(mo1,1)*thr*82.1e+09

        sup=store2(n)/82.1e+09+181.43

        lake_elv(l,jz)=sup
        lake_inflow(l,jz)=qi2(n)
        net_lake_inflow(l,jz)=qi2(n)

      elseif(resname(l).eq.'Huron        ')then

!       Lake Michigan-Huron
        if(firstpass.eq.'y')then
!         initialize storage	   
!         storage = live storage 
	    mhu=mhu_init
	    store1(n)=(mhu-166.98)*117.4e+09
	    store2(n)=store1(n)
	    delta_elv=mhu-stc_init
	    mean_elv=(mhu_init+stc_init)/2.0
	  else
!     rev. 9.4.11  Jun.  22/07  - NK: reordered rerout for glake 
	    delta_elv=mhu-stc
	    mean_elv=amax1(166.98+0.1,mean_elv)   ! prevent div by 0
	    delta_elv=amax1(0.001,delta_elv)        ! prevent div by 0
	  endif
!       use stc from the previous time step. Slow change anyway.

        qo2(n)=82.2*(mean_elv-166.98)**1.87*(delta_elv)**0.36
     *           -retard_factor(mo1,2)        

        store2(n)=store1(n)+(qi1(n)+qi2(n)-qo1(n)-qo2(n))*div
c     *            -hourly_evap(mo1,2)*thr*117.4e+09

        mhu=store1(n)/117.4e+09+166.98

!        print*,n,resname(l),mhu,qo2(n),store1(n),store2(n)
        lake_elv(l,jz)=mhu
        lake_inflow(l,jz)=qi2(n)
        net_lake_inflow(l,jz)=qi2(n)-lake_outflow(l-1,jz)

      elseif(resname(l).eq.'StClair      ')then

!       Lake St. Clair
        if(firstpass.eq.'y')then
!         initialize storage	   
!         storage = live storage 
	    stc=stc_init
	    store1(n)=(stc-164.91)*1.11e+09
	    store2(n)=store1(n)
          delta_elv=stc_init-eri_init
	  else
!     rev. 9.4.11  Jun.  22/07  - NK: reordered rerout for glake 
          delta_elv=stc-eri
	    delta_elv=amax1(0.001,delta_elv)        ! prevent div by 0
	  endif
        stc=amax1(0.1,stc)                    ! prevent div by 0
!       use eri from the previous time step. Slow change anyway.

        qo2(n)=28.8*(stc-164.91)**2.28*(delta_elv)**0.305        
     *           -retard_factor(mo1,3)        

        store2(n)=store1(n)+(qi1(n)+qi2(n)-qo1(n)-qo2(n))*div
c     *            -hourly_evap(mo1,3)*thr*1.11e+09

        stc=store1(n)/1.11e+09+164.91

!        print*,n,resname(l),stc,qo2(n),store1(n),store2(n)
        lake_elv(l,jz)=stc
        lake_inflow(l,jz)=qi2(n)
        net_lake_inflow(l,jz)=qi2(n)-lake_outflow(l-1,jz)

      elseif(resname(l).eq.'Erie         ')then

!       Lake Erie
        if(firstpass.eq.'y')then
!         initialize storage	   
!         storage = live storage 
	    eri=eri_init
	    store1(n)=(eri-169.86)*25.7e+09
	    store2(n)=store1(n)
        endif       

        qo2(n)=558.3*(eri-169.86)**1.60-retard_factor(mo1,4)
         
        store2(n)=store1(n)+(qi1(n)+qi2(n)-qo1(n)-qo2(n))*div
c     *            -hourly_evap(mo1,4)*thr*25.7e+09

        eri=store1(n)/25.7e+09+169.86  

c        print*,n,resname(l),eri,qo2(n),store1(n),store2(n)
        lake_elv(l,jz)=eri
        lake_inflow(l,jz)=qi2(n)
        net_lake_inflow(l,jz)=qi2(n)-lake_outflow(l-1,jz)

      elseif(resname(l).eq.'Ontario      ')then

!       Lake Ontario
        if(firstpass.eq.'y')then
!         initialize storage	   
!         storage = live storage 
	    ont=ont_init
	    store1(n)=(ont-69.474)*18.96e+09
	    store2(n)=store1(n)
	  endif

        qo2(n)=555.823*(ont-0.0014*real(2000-1985)-69.474)**1.5
     *           -retard_factor(mo1,5)        

        store2(n)=store1(n)+(qi1(n)+qi2(n)-qo1(n)-qo2(n))*div
c     *            -hourly_evap(mo1,5)*thr*18.96e+09

        ont=store1(n)/18.96e+09+69.474         

!        print*,n,resname(l),ont,qo2(n),store1(n),store2(n)
!        print*,'year=',year
        lake_elv(l,jz)=ont
	  lake_inflow(l,jz)=qi2(n)
        net_lake_inflow(l,jz)=qi2(n)-lake_outflow(l-1,jz)

      elseif(b1(l).ne.0.0)then
  
!       natural lake or uncontrolled reservoir routing:

        store2(n)=store1(n)+(qi1(n)+qi2(n)-qo1(n)-qo2(n))*div
        if(store2(n).le.0.0)then
          write(98,9801)l,n,store2(n)
          store2(n)=1.0
        endif
        old=qo1(n)
        hold=1.0e+25
        if(b3(l).eq.0.0)then
!         tried to put this in the iteration loop but got spikes
          qo2(n)=b1(l)*store2(n)**b2(l)
        else
          do ic=1,20 
            if(abs(hold-store2(n)).gt.0.003*hold)then
!             rev  9.1.03  July  24/01  - added polinomial 
              qo2(n)=store2(n)*(b1(l)+store2(n)*(b2(l)+store2(n)*
     *           (b3(l)+store2(n)*(b4(l)+b5(l)*store2(n)))))
      
              wt=amax1(0.5,float(ic)/21.0)
              qo2(n)=(1.0-wt)*qo2(n)+wt*old
              old=qo2(n)
              hold=store2(n)
              store2(n)=store1(n)+(qi1(n)+qi2(n)-qo1(n)-qo2(n))*div
            else
              go to 26
            endif   ! hold-store

          end do   !ic=1,20
26        continue   ! sorry about that   
        endif      
        if(store2(n).le.0.0)then
!            qo2(n)=store1(n)/div+qi1(n)+qi2(n)-qo1(n)
          qo2=0.0
          store2(n)=1.0
c          dtmin=a6
          write(53,6804)n,l
        endif
        if(iopt.ge.2)then
          write(53,6004)
     *           qi1(n),qi2(n),store1(n),store2(n),qo1(n),qo2(n)
        endif
        
!       CALCULATE THE DETENTION TIME
!        if(qo1(n).gt.0.001) at=store2(n)/qo1(n)
!         yeah.... fix this:
        if(qo1(n).gt.0.001) at=store2(n)/qo2(n)

!       SELECT MINIMUM TRAVEL TIME FOR THE TIME STEP CALCULATION
c        dtmin=amin1(at,dtmin)
	
!       DTMIN IS THE TIME REQUIRED TO COMPLETELY DRAIN THE FASTEST
!       EMPTYING ELEMENT              
        if(iopt.eq.2)print*,'in rerout passed 679'

      else

!       FROM reservoir RELEASE TABLE:
!       rev. 8.99l  Oct.    2001-     fixed reservoir release timing
!         jm=jz+1        old way see JW's e-mail Oct. 23/01

        jm=jz
        if(jz.gt.nrel)jm=nrel

	  if(jm.lt.1)then
          qo2(n)=qrel(l,1)
	  else
          qo2(n)=qrel(l,jm)
	  endif

        if(qo2(n).lt.0.0)qo2(n)=0.0

!        this line is for the water balance only
!        it doesn't work for releases

        store2(n)=store1(n)+(qi1(n)+qi2(n)-qo1(n)-qo2(n))*div

        if(iopt.ge.2)write(53,6803)l,jm,n,ireach(n),qo2(n)
      endif   !        if(b1(l).ne.0.0)

      if(iopt.eq.2)print*,'in rerout passed 698'


  999 RETURN

! FORMATS

  500 format(256f10.3)
  501 format(3i5,4x,a1)
  502 format(' resv flow data extrapolated ',i5,' hours')
  504 format(' noresv,nrel,ktr/',3i5)
 1011 format(' ',3x,'  i  ires(i) jres(i)    b1(i)     b2(i)',
     *	'    b3(i)     b4(i)')
 1013 format(' ',3x,i3,2i8,5f10.5,a12/)
 4901 format(25i1)
 4902 format(3i5)
 4903 format(a12)
 4904 format(256f10.0)
 4905 format(256f10.3)
 5003 format(2i5,4g10.3,5x,a12)
 3704 format(2i5,5g10.3,5x,a12)

!     rev. 9.1.55  Jun.  12/04  - NK: write new files to resrl\newfmt folder.
 5004 format(a20,a10)
 5005 format(a20,i5)
 5006 format(a20,a1)
 5007 format(a20,256i1)
 5008 format(a20,f12.0)
 5009 format(a20,a2,'-',a2,'-nn',a2)
 5010 format(a20,a2,a4)

 5301 format(' ','Reservoir inflow data echoed:')
 5303 format(6(' ',a12))
 5304 format(' ','Error on unit=99,fln=',a30,'(',i2,')'//)
 5310 format(' -ve flow for reservoir #',i3,'zero flow assumed',i3)
 6004 format(' qi,store,qo/',2f10.3,2f12.0,2f10.3)
 6801 format('   rerout: reservoir no =',i3,' mhtot =',i5)
 6802 format('   ',256f8.2)
 6803 format(' rerout: l,m,n,ireach(n),qo2(n)/',4i5,f10.2,f12.0)
 6804 format(' warning: store2(',i5,') set = 0.0 for resv no.',i5) 
 9005 format(' iymin,iymax,jxmin,jxmax/',4i5)
9801  format(' resv',i3,' grid',i6,' store2=',g12.3,' < 0  1.0 assumed')
99182   format(' Warning: Error opening or reading fln:',a30/
     *  ' Probable cause: missing strfw/yymmdd.str input file'/
     *  ' OR: in config.sys have you set files=100 & buffers=50?'/)

      END SUBROUTINE rerout

