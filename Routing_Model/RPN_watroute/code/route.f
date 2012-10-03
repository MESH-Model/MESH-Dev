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

      SUBROUTINE route(div,thr,dtmin,jz,iz,time,date)

!***********************************************************************
!       copyright (c) by Nick Kouwen 1987-2007
!***********************************************************************

!***********************************************************************
!
!  THE INFLOW INTO THE CHANNEL IN EACH ELEMENT IS THE SUM OF THE
!  OUTFLOW OF THE ELEMENTS WHICH DRAIN INTO IT FROM ABOVE AND THE
!  SURFACE RUNOFF AND SUBSURFACE FLOW FROM THE ELEMENT IN WHICH THE
!  OUTFLOW IS BEING CALCULATED                                 
!
!     rev. 8.23   Mar.  25/96   - fixed bug in route - keep qo2 for res
!     rev. 8.99mm Dec. 13/2001  - added check for <= 0 init res flow
!     rev. 8.99n  Dec. 31/2001  - fixed nat. res initial flow (JW)
!
!     rev. 9.00    Mar.  2000   - TS: CONVERTED TO FORTRAN 90
!     rev. 9.03    Nov.  2000   - TS: ADDED WATFLOOD SWAMP ROUTING 
!     rev  9.1.03  July  24/01  - added polinomial to reservoir routing
!     rev. 9.1.10  Jan.  29/02  - flow nudging added for nopt(l)=2
!     rev. 9.1.14  Mar.  24/02  - fixed wetland min time step and outflow
!     rev. 9.1.16  Apr.  03/02  - Added wetland conditional to select river w/wo wetland
!     rev. 9.1.31  Nov.  13/02  - Fixed the wetland Q to account for wetland area
!     rev. 9.1.33  Dec.  05/02  - Fixed instability in wetland flow    
!     rev. 9.1.38  Mar.  31/03  - revised str header and routing dt selectable
!     rev. 9.1.39  Apr.  06/03  - Fixed wetland routing when channel is dry
!     rev. 9.2.11  Sep.  11/05  - NK: added Manning's n  r1n & r2n
!     rev. 9.2.13  Sep.  28/05  - NK: added freeze and break up to route
!     rev. 9.2.23  Nov.  22/05  - NK: Fixed res(n)=0 bug in route 
!     rev. 9.2.35  Mar.  22/06  - NK: Glacier flow bypasses wetlands
!     rev. 9.2.39  May.  09/06  - NK: t added to route & rerout arg list
!     rev. 9.2.43  Jun.  21/06  - NK: fixed spikes in route
!     rev. 9.3.04  Oct.  24/06  - NK: routing parameters dim to na in rte
!     rev. 9.3.10  Jan.  29/07  - NK: routing pars changed to gridded values
!                                 eg: lzf(ii) -> lzf(n) 
!
!     changes made to include c&g model stuff  nk  April. 26/07
!
!***********************************************************************

      use area_watflood
      implicit none

!     SAVES THE LOCAL VARIABLES FROM ONE RUN TO NEXT
      SAVE

! Christopher Subich (9/12): istate was allocated as (400,400),
! which led to bad array-out-of-bounds errors when the full 2D
! grid was larger than that
      INTEGER :: istate(imax,jmax),
     *           rbin,lsta,nnn1,jz,n,lll,l,iz,jjz,
     *           i,j,ii,ic,jj,ios,itracker,unt,ln,n_dt_min,hour_offset
      REAL    :: old,oldwet,convert
      REAL(4) :: time,newstore,try1,try2,try3,div,thr,at,dtmin,hold,
     *             wt,atemp,ax,xa,at1,ice_fctr,dt_min_n
      character*1 :: flowsta,firstpass
      character*80 :: junk
      character(14) :: date
      logical :: exists      
      INTEGER :: spl_csv_flag

      DATA firstpass/'y'/

! csubich -- use array initialization syntax to set istate=0
      istate = 0

      if(firstpass.eq.'y')then   ! changed May 8/06 nk

!       skip this section if using a resume file !!!!  nk 11/11/04
!       INDEX IS INITIALIZED IN S/R SUB
!       CALLED TO READ IN THE RELEASES AND RESERVOIR DETAILS
!       CAN'T USE JAN HERE BECAUSE WE MIGHT CALL IT SEVERAL TIMES
!       USE RESERVOIR L=1 HERE JUST TO CALL REROUT FOR INT VALUES
!       USE ELEMENT N=NA JUST TO CALL REROUT
!       NNN1=NA IS JUST A DUMMY VARIABLE FOR AN UNUSED STORAGE
        
        nnn1=na
        convert=al*al*1000.0  ! converts vol in m^3 to mm on the unit grid
        
!       first time through
        do n=1,naa
          res(n)=0
        end do

!       This should probably be moved to flowinit or sub where qda is initialized
!       but it is much easier to do this here because rerout has been called now
!       and we have the values for b1().....b4()
!       But we must initialize the storage only once so use id=1
!       for this part only. Can't use jan here because we visit more than
!       once per time step
        
        do n=1,naa
          lll=next(n)
          do l=1,noresv

!           LOOK TO SEE IF THERE IS A RESERVOIR IN THIS GRID:

            if(yyy(n).eq.ires(l).and.xxx(n).eq.jres(l))then
              res(n)=l

!             IF THERE IS, SET INITIAL RESERVOIR STORAGE:

!              if(b1(l).gt.0.0.and.b2(l).gt.0.0)then
              if(b1(l).ne.0.0.or.b2(l).ne.0.0)then

!               WE HAVE A NATURAL CONTROL & WE NEED INITIAL
!               RESERVOIR STORAGE
!               INITIAL FLOWS ARE CALC IN SUB SO LEAVE THEM ALONE 

                if(id.eq.1)then

!     rev. 9.1.07  Jan. 03/02  - check that outlet is in a lake NK

                if(ireach(n).eq.0)then
                  print*,'in route in grid number',n
                  print*,'reach number =',ireach(n)
                  print*,'i.e. lake or reservoir #',l,' outlet' 
                  print*,'is not in a lake'
                  print*,'please make grid part of a lake or'
                  print*,'move outlet to a grid in a lake'
!                 note that dam locations with releases do not have to be in a lake!! 
                  print*
                  stop 'Program aborted in route @ 170'
                endif
!               rev. 8.99mm Dec. 13/2001-     added check for <= 0 init res flow
!               check for +ve flow for res. initializations

!               probable don't need this if we are using a resume file

                if(qo1(n).le.0.0)then
                  print*,'Initial flow at reservoir',l
                  print*,'is .le. 0.0 and can not be initialized'
                  print*,'please ensure there is a downstream '
                  print*,'flow station with a flow > 0.0 '
                  print*,'We are in row',yyy(n),'column',xxx(n)
                  print*,'grid no',n,'with init flow =',qo1(n) 
                  print*
!                  stop ' Program aborted in route @ 64'
                endif

!                  if(poliflg.ne.'y'.or.
!     *                 abs(b3(l)+b4(l)+b5(l)).le.0.1e-32)then
                if(poliflg.ne.'y')then

!                 don't get rid of the poliflg because of the format
!                 for the header section is not compatible with the old files
!                 use 2 coefficients  

                  if(resumflg.ne.'y')then
                    store1(n)=(qo1(lll)/b1(l))**(1.0/b2(l))
                  endif

                else

                  if(b3(l).eq.0.0)then
!                   use 2 coefficients  
                    if(resumflg.ne.'y')then
                      store1(n)=(qo1(lll)/b1(l))**(1.0/b2(l))
                    endif
                  else
!                   use bisection to get init flow
!                   actually, I made the int. storage a little larger
                    if(resumflg.ne.'y')then
                      store1(n)=100.
                      try1=0.0
!                     use 2-5 coefficients
                      write(53,*)'         n           l          try1',    
     *                     '         qo1        store1'
                      do while(try1.lt.qo1(lll))
!                       keep doubling the res. storage until the corresponding
!                       flow is larger than the initialized streamflow.
                        store1(n)=2.0*store1(n)                      
                        try1=b1(l)*store1(n)+
     *                       b2(l)*store1(n)**2+
     *                       b3(l)*store1(n)**3+
     *                       b4(l)*store1(n)**4+
     *                       b5(l)*store1(n)**5
                        write(53,*)n,l,try1,qo1(n),store1(n),'poli'
                        if(try1.lt.0.0)then
                          print*,'trial value for flow =',try1
                          print*,'trial reservoir outflow < 0.0'
                          print*,'polinomial functions not'
                          print*,'monotonically increasing'
                          print*,'for reservoir',l
                          print*,'Please fix the function'
                          print*,'Program may excecute but results' 
                          print*,'are approximate'
                          print*,' '
                          print*
                          pause 'Hit enter to continue. @ 167/route'
                        endif
                      end do

                    endif
                  endif

                write(53,*)n,l,try1,qo1(n),store1(n),'done'
                write(53,*)

                endif

                  if(resumflg.ne.'y')then
                    store1(n)=max(100.0,store1(n))
                    store2(n)=store1(n)
                    if(iopt.ge.1)then
                      write(51,8492)
                      write(51,8493)n,l,b1(l),b2(l),qda(n),store1(n)
                      write(55,8492)
                      write(55,8493)n,l,b1(l),b2(l),qda(n),store1(n)
                    endif
                  endif

                endif             ! id=1

              endif   
            endif
          end do
        end do  

        junk='Into route, passed 90'
 
       if(iopt.eq.-9)write(98,9801)junk

        if(iopt.ge.2)then
          write(55,6007)
          write(55,6000)(res(n),n=1,naa)
        endif

      endif           ! firstpass

      index=2

      if(firstpass.eq.'y')then   !section added Apr. 28/06  nk
!       check that qdwpr memory has been allocated
        if(ireach(n).gt.0.or.res(n).gt.0)then
          if(.not.allocated(qdwpr))then
             print*
             print*,'Memory not allocated for qdwpr'
             print*,'No of reservoirs in the .rel file is 0'
             print*,'but reaches in the map & shed files have been'
             print*,'defined. This is a problem.'
             print*,'Please either set all reach values = 0'
             print*,'or code in the reservoir locations in the rel'
             print*,'files'
             stop 'Program aborted in route @ 279'
          endif      
        endif
      endif

!     CALCULATIONS START IN THE HIGHEST ELEMENT IN THE WATERSHED 
!     AND PROCEED TO THE LOWEST.

      if(iopt.ge.2)then
        write(55,6002)jz,jz
        write(53,6002)jz,jz
        write(55,6003)
      endif
 
      jjz=jz
      if(jjz.lt.1) jjz=1
      dt_min_n=1.0e32

      do rbin=1,noresv
        qdwpr(rbin,jjz)=0.0
      end do

! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

      if(iopt.eq.2)print*, 'In route before the 1-naa loop'

      do n=1,naa
       
!       REV. 8.23 - Mar.  25/96 - FIXED BUG IN ROUTE, KEEP QO2 FOR RES
        store1(n)=store2(n)
        wstore1(n)=wstore2(n)
        hwet1(n)=hwet2(n)
        hcha1(n)=hcha2(n)
        qo1(n)=qo2(n)
        if(res(n).eq.0)qo2(n)=0.0
        qowet1(n)=qowet2(n)
        qi1(n)=qi2(n)
        qi2(n)=1.0e-10
        qiwet1(n)=qiwet2(n)
        qiwet2(n)=1.0e-10
      end do

        qi2(na)=0.0
        wt=0.5

!     ROUTING LOOP:

      do n=1,naa

        if(iopt.eq.2.and.n.eq.1)print*,'In route, passed 101'

        i=yyy(n)
        j=xxx(n)
        l=nhyd(i,j)

!         WHEN THE SLOPE <= 0.0 THE ELEMENT IS NOT IN THE BASIN
!         AND THE ROUTING SEQUENCE IS SKIPPED

          if(slope(n).gt.0.0)then

!           REV. 7.2 Sept. 19/94 - ADDED IREACH() FOR DWOPER INPUT  

! * * * * * * * * * * LAKE or RESERVOIR ROUTING * * * * * * * * * * * * * * * * 

              if(ireach(n).gt.0.or.res(n).gt.0)then

!               WE ARE ROUTING EXTERNALLY WITH DWOPER OR DOING IT IN A 
!               CONTROLLED RESERVOIR DOWNSTREAM IF THERE IS ONE
!               THE FLOWS ARE ACCUMULATED IN A REACH-BIN FOR DWOPER 
!               SEVERAL ELEMENTS CAN CONTRIBUTE BUT NONE IS ROUTED TO 
!               DOWNSTREAM IN ed :

!               ADD UPSTREAM CONTRIBUTIONS AND LOCAL INFLOW
              
                lll=next(n)
                rbin=ireach(n)
 
                if(res(n).eq.0.)then

!                 grid is part of a reservoir or lake
                  qdwpr(rbin,jjz)=qdwpr(rbin,jjz)+qi2(n)+qr(n)

                else

                  if(iopt.ge.3)write(55,6004)n,res(n),index

!                 THERE IS A DAM IN THIS SQUARE AND THE 
!                 RESERVOIR ROUTING SUBROUTINE REROUT IS CALLED.
!                 * * * FOR RESERVOIR ROUTING:* * *      
!                 REROUT IS THE RESERVOIR ROUTING SUBROUTINE       
!                 IT'S ASSUMED THAT THE DAM IS LOCATED IN A REACH NUMBER

!     rev. 9.1.43  Jun.  01/03  - Fixed the qdwpr.txt function -

!                 re: last grid in lake corrected this June 1/03
!                 previously, the last grid was not added to the qdwpr.txt file
!                 this next line copied from above

                  if(rbin.gt.0)then
!                 there may be a dam but the grid may not have been 
!                 designated as part of a lake    

                    qdwpr(rbin,jjz)=qdwpr(rbin,jjz)+qi2(n)+qr(n)
                    qi2(n)=qdwpr(rbin,jjz)     

                  endif
  
!               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                call rerout(n,div,thr,res(n),jz,at,dtmin,date,firstpass)
!               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!     rev. 9.1.67  Oct.  21/04  - NK; added unit 80 for lake_stor & lake_outflow
!                  May   02/05  - TS: revised

                  ln=res(n)

                  if(jz.ge.1)then      ! added conditional Rev. 9.2.43  nk
                    lake_stor(ln,jz)=store2(n)
                    lake_outflow(ln,jz)=qo2(n)
                    net_lake_outflow(ln,jz)=qo2(n)
                    if(ln.gt.1)net_lake_outflow(ln,jz)=
     *                          qo2(n)-lake_outflow(ln-1,jz)
                    del_stor(ln,jz)=(qi2(n)-qo2(n))*div
                    !div added May 9/06 nk
                  endif

                endif       ! res(n)

! * * * * * * * * * *  TS - WETLAND ROUTING  * * * * * * * * * * * * * * * * 

            ! csubich: This gives an out-of-bounds error if ntype <=1
            ! so fix by splitting up the statement
            elseif (ntype .gt. 1
     *            .and. aclass(n,ntype-1).gt.0.0
     *            .and.wetflg.eq.'y'
     *            .and.theta(ibn(n)).gt.0.00001
     *            .and.glacier_flag(n).ne.'y')then

!     rev. 9.2.35  Mar.  22/06  - NK: Glacier flow bypasses wetlands
!                we have to assume that if there is a glacier, 
!                there is no wetland in that grid
!                i.e. we run this code only when NOT in a glacier grid

!     rev. 9.1.16  Apr.  03/02  - Added wetland conditional to select river w/wo wetland
!     rev. 9.1.31  Nov.  13/02  - Fixed the wetland Q to account for wetland area
!     rev. 9.1.33  Dec.  05/02  - Fixed instability in wetland flow    

!             WETLAND+CHANNEL ROUTING:
!             dacheck is just there to be able to do wetlands in headwater watersheds
!             and not do it in the larger rivers. This should be replaced by a
!             rivertype with -ve wetland parameters.
!             CHANNEL ROUTING PORTION OF CODE:
!             ADD UPSTREAM CONTRIBUTIONS AND LOCAL INFLOW:

!Note:wetland routing can not be part of watroute because 
!     qstream aand strloss are included in the runof\yyyymmdd.r2c
!     file. To have wetland routing in watroute, a separate file containing
!     hourly values of qstream(n)-strloss(n) must be read in for use here.
!     This is possible of course.

!     However, it is left here so CHARM can be run using the wetland routine

              qi2(n)=qi2(n)  ! qr goes in below
              qi2(n)=qi2(n)       
              qin(n)=qi2(n)
!             qstream portion of qr added to channel, not wetland
              qiwet2(n)=(qr(n)-qstream(n)+strloss(n)) 
     *                 +qswrain(n)+qlz(n)-qswevp(n)
              ii=ibn(n)
              lll=next(n)
              old=qo1(n)
              oldwet=qowet2(n)
              hold=1.0e+25
              do ic=1,20
                  if(hwet2(n).le.0.0)hwet2(n)=qlz(n)*div*2/
     *                                       wetarea(n)/theta(n)
                  if(hcha2(n).lt.0.0)hcha2(n)=0.001
                  qi2(n)=qin(n)+qowet2(n)
!               UP TO 20 ITERATIONS ARE ALLOWED
                  if(abs(hold-wstore2(n)).gt.0.00001*hold)then
!               THIS ITERATES TO 3% OR ALLOWS UP TO 20 ITERATIONS
                  if(store2(n).le.0.0)then
!                   NO OUTFLOW - CHANNEL IS EMPTY
                    itracker=0
                    flowxa(n)=0.0
                    hcha2(n)=0.0
                    qo2(n)=0.001
!     rev. 9.1.38 Apr. 06/03  - Fixed wetland routing when channel is dry
!                   Added this section to calculate wetland outflow
!                   even if the channel is empty.
!                   Also, made the convergence check to 1 mm in wetheight.
                    over=0.0
                    hcha2(n)=store2(n)/chaarea(n)
                    hwet2(n)=wcap(n)/wetarea(n)/theta(n)
     *                      +(wstore2(n)-wcap(n))/wetarea(n)
                    qowet2(n)=kcond(n)*
     *                       (hwet2(n)**2-hcha2(n)**2)*astep
!     *                                     /aclass(n,ntype-1)
!                   using astep makes it independent of grid size
!                   assumes eq. calculates flow/km
                  else
                    over(n)=(store2(n)-cap(n))/rl(n)
                    if(over(n).le.0.0)then
                      itracker=1
!                     CHANNEL FLOW ONLY
                      if(wstore2(n).gt.wcap(n))then
!                       WETLAND IS FULL - OVERFLOWS INTO CHANNEL:
!                        over(n)=(wstore2(n)-wcap(n))/rl(n)   
                        over(n)=0.0       !  bug found 09/03/04 nk
                        hcha2(n)=store2(n)/chaarea(n)
                        hwet2(n)=wcap(n)/wetarea(n)/theta(n)
     *                          +(wstore2(n)-wcap(n))/wetarea(n)
                        qowet2(n)=kcond(n)*
     *                            (hwet2(n)**2-hcha2(n)**2)*astep
!     *                                   /aclass(n,ntype-1)
                      else
                        itracker=2
!                       WETLAND IS NOT FULL - NOTHING OVERFLOWS:
                        over(n)=0.0         
                        hwet2(n)=wstore2(n)/
     *                               wetarea(n)/theta(n)
                        hcha2(n)=store2(n)/chaarea(n)
                        qowet2(n)=kcond(n)*
     *                            (hwet2(n)**2-hcha2(n)**2)*astep
!     *                                  /aclass(n,ntype-1)
                      endif
                      flowxa(n)=store2(n)/rl(n)
!     rev. 9.2.11  Sep.  15/05  - NK: added Manning's n  r1n & r2n
                      if(manningflg.eq.'y')then
                        qo2(n)=flowxa(n)**1.67*slope(n)/
     *                             chawid(n)**0.667/r2n(n)
                      else
                        qo2(n)=flowxa(n)**1.33*slope(n)/r2(n)
                      endif
!                        qo2(n)=ice_fctr*qo2(n)
!                     ONLY WETLAND FLOW
                    else         ! over > 0.0
!                     CHANNEL + FLOOD PLAIN FLOW
                      if(wstore2(n).gt.wcap(n))then
                        itracker=3
!                       WETLAND IS FULL - OVERLAND FLOW
                        over(n)=(store2(n)-cap(n))/rl(n)
                        hwet2(n)=wcap(n)/wetarea(n)/theta(n)
     *                          +(wstore2(n)-wcap(n))/wetarea(n)
                        hcha2(n)=cap(n)/chaarea(n)
     *                              +over(n)/(wetwid(n)+chawid(n))
                      else       !  wstore < wcap
                        itracker=4
!                       CHANNEL begins OVERFLOW INTO WETLAND:
                        over(n)=(store2(n)-cap(n))/rl(n)
                        hwet2(n)=
     *                      wstore2(n)/wetarea(n)/theta(n)
                        hcha2(n)=cap(n)/chaarea(n)
     *                           +over(n)/(chawid(n)+wetwid(n))
                      endif
                      chaxa(n)=cap(n)/rl(n)
                      if(manningflg.eq.'y')then
!     rev. 9.2.11  Sep.  15/05  - NK: added Manning's n  r1n & r2n
!                       0.17 factor is based on 100:1 fp w/d ratio
                        qo2(n)=
     *                  chaxa(n)**1.67*slope(n)/chawid(n)**0.667/r2n(n)
     *                      + over(n)**1.33*slope(n)*0.17/r1n(n)
                      else
                        qo2(n)=(chaxa(n)**1.33+over(n)**1.33
     *                             /r1(n))*slope(n)/r2(n)
                      endif
!                        qo2(n)=ice_fctr*qo2(n)
!                     OVERLAND FLOW, WETLAND FLOW NEGLIGABLE!
                      qowet2(n)=20.0*kcond(n)/
     *                           2*(hwet2(n)**2-hcha2(n)**2)
!     *                                 /aclass(n,ntype-1)
                    endif
                    wt=amax1(.5,float(ic)/21.)
                    qo2(n)=(1.0-wt)*qo2(n)+wt*old
                    old=qo2(n)
                    qowet2(n)=(1.0-wt)*qowet2(n)+wt*oldwet
                    oldwet=qowet2(n)
                  endif
                  hold=wstore2(n)
                  store2(n)=store1(n)+(qi1(n)+qi2(n)
     *                       -qo1(n)-qo2(n))*div
                  wstore2(n)=wstore1(n)+(qiwet1(n)+qiwet2(n)
     *                          -qowet1(n)-qowet2(n))*div
                  if(wstore2(n).lt.0.0)wstore2(n)=0.0
                  satxa(n)=wstore2(n)/rl(n)/theta(n)
                else
!                 CONVERGENCE TO 3%
                  GO TO 26
                endif
              end do

   26         if(store2(n).le.0.0)then
                qo2(n)=store1(n)/div+qi1(n)+qi2(n)-qo1(n)
                store2(n)=0.0
                dtmin=a6
              endif

              if(qowet2(n).gt.1.0e+10)then
                print*,'likely fp overflow - reduce kcond(',ibn(n),')=',
     *                     kcond(ibn(n))
              endif

! >  >  >     MAYBE NEXT LINE HAS TO BE CHECKED OUT
!             WHY IS IT 0 ANYWAYS ??????

              if(qo2(n).le.0.0)qo2(n)=0.001

!             CALCULATE THE VELOCITY THROUGH EACH SQUARE
!             CALCULATE TRAVEL TIME FOR MAXIMUM VELOCITY.
!     rev. 9.1.14  Mar.  24/02  - fixed wetland min time step and outflow

              at=amin1(store2(n)/qo2(n),abs(wstore2(n)/qowet2(n)))

!             SELECT MIN TRAVEL TIME FOR THE TIME STEP CALC

              dtmin=amin1(at,dtmin)
              dtmin=amax1(dtmin,a6)   ! dtmin > a6 no matter what

!             DTMIN IS THE TIME REQUIRED TO COMPLETELY DRAIN
!             THE FASTEST EMPTYING ELEMENT
!             CALCULATE THE CHANNEL STATE FOR GRAPHICAL OUTPUT:

              i=yyy(n)
              j=xxx(n)
              atemp=qo2(n)/(0.4*bnkfll(n))+1.0

!             TO PREVENT INTEGER UNDERFLOW OR OVEFLOW:  

              atemp=amax1(atemp,1.0)
              atemp=amin1(atemp,99.0)
              istate(i,j)=int(atemp)

              if(iopt.ge.4) write(55,1002)
     *                       i,j,n,istate(i,j),bnkfll(n),qo2(n)

! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
              else                     ! end of wetland routing

!             CHANNEL ROUTING:
!             ADD UPSTREAM CONTRIBUTIONS AND LOCAL INFLOW:
!              if(aclass(n,ntype-1).gt.0.0)
!     *            PAUSE 'channel routing for wetland class'

              qi2(n)=qi2(n)+qr(n)    !+qstream(n)-strloss(n)

!             qstream-strloss is included in qr
!             both from the runof\yyyymmdd_rff.r2c file 
!             and the calculation in sub
              ii=ibn(n)
              lll=next(n)
              old=qo1(n)
              hold=1.0e+25
              do ic=1,20
!               UP TO 20 ITERATIONS ARE ALLOWED
                if(abs(hold-store2(n)).gt.0.0003*hold)then
!                 THIS ITERATES TO 3% OR ALLOWS UP TO 15 ITERATIONS
                  if(store2(n).le.0.0)then
!                   NO OUTFLOW - CHANNEL IS EMPTY
                    ax=0.0
                    qo2(n)=0.001
                  else
                    over(n)=(store2(n)-cap(n))/rl(n)
                    if(over(n).le.0.0)then
!                     CHANNEL FLOW ONLY
                      ax=store2(n)/rl(n)
!     rev. 9.2.11  Sep.  15/05  - NK: added Manning's n  r1n & r2n
                      qo2(n)=ax**1.67*slope(n)/
     *                             chawid(n)**0.667/r2n(n)
!                        qo2(n)=ice_fctr*qo2(n)
                    else
!                     CHANNEL + FLOOD PLAIN FLOW
!     rev. 9.2.43  Jun.  21/06  - NK: fixed spikes in route
                      ax=cap(n)/rl(n)   ! added Jun 21/06 nk
!     rev. 9.2.11  Sep.  15/05  - NK: added Manning's n  r1n & r2n
!                       0.17 factor is based on 100:1 fp w/d ratio
!                       flood plain width/depth assumes as 100
!                       use quadratic equation to solve for fp. depth
                      hwet2(n)=(-1.0+sqrt(1.+400.0*over(n)))/200.0
!                       hcha2(n) is the bankfull depth here
                      hcha2(n)=chaxa(n)/chawid(n)
!                       xa is the total main channel cross section area
                      xa=(hwet2(n)+hcha2(n))*chawid(n)

                      if(over(n)-hwet2(n)*chawid(n).gt.0.0)then
                        qo2(n)=
     *                      xa**1.67*slope(n)/chawid(n)**0.667/r2n(n)
     *                      +(over(n)-hwet2(n)*chawid(n))**1.33
     *                               *slope(n)*0.17/r1n(n)
                      else
                        qo2(n)=
     *                      xa**1.67*slope(n)/chawid(n)**0.667/r2n(n)
                      endif
!                        qo2(n)=ice_fctr*qo2(n)
                    endif
                    wt=amax1(.5,float(ic)/21.)
                    qo2(n)=(1.0-wt)*qo2(n)+wt*old
                    old=qo2(n)
                  endif
                  hold=store2(n)

                  store2(n)=store1(n)+(qi1(n)+qi2(n)
     *                        -qo1(n)-qo2(n))*div
                else
!                 convergence to 3%
                  GO TO 16
                endif
              end do
       
   16         if(store2(n).le.0.0)then
                qo2(n)=store1(n)/div+qi1(n)+qi2(n)-qo1(n)
                store2(n)=0.0
                dtmin=a6
              endif

! >  >  >     MAYBE NEXT LINE HAS TO BE CHECKED OUT
!             WHY IS IT 0 ANYWAYS ??????

              if(qo2(n).le.0.0) qo2(n)=0.001

!             CALCULATE THE VELOCITY THROUGH EACH SQUARE
!             CALCULATE TRAVEL TIME FOR MAXIMUM VELOCITY.

              at=store2(n)/qo2(n)

!             SELECT MIN TRAVEL TIME FOR THE TIME STEP CALC

              dtmin=amin1(at,dtmin)
              dtmin=amax1(dtmin,a6)   ! dtmin > a6 no matter what
     
!             DTMIN IS THE TIME REQUIRED TO COMPLETELY DRAIN
!             THE FASTEST EMPTYING ELEMENT

!             CALCULATE THE CHANNEL STATE FOR GRAPHICAL OUTPUT:

              i=yyy(n)
              j=xxx(n)
!             csubich -- segfault here when bnkfill is 0,
!             so cap bnkfll at a tiny value away from 0
              bnkfll(n) = amax1(bnkfll(n),1e-8)
              atemp=qo2(n)/(0.4*bnkfll(n))+1.0

!             TO PREVENT INTEGER UNDERFLOW OR OVEFLOW:  

              atemp=amax1(atemp,1.0)
              atemp=amin1(atemp,99.0)
              istate(i,j)=int(atemp)

              if(iopt.ge.4)write(55,1002)
     *                          i,j,n,istate(i,j),bnkfll(n),qo2(n)

            endif                         ! END OF CHANNEL ROUTING

!     rev. 9.1.10  Jan.  29/02  - flow nudging added for nopt(l)=2
            flowsta=' '
!            write(6,*) '***** S/R route: no', no

! D. Deacu: Inserted 'hour_offset' to select the discharges  
!           corresponding to the current day and hour 
!
            hour_offset = 24*(day1-1)
!
!            if (n .eq. 0) then
!               print *,'*',year_now,month_now,day_now,hour_now
!               print *,'*',day1,hour_offset,jz,hour_offset+jz
!            end if
!
            do l=1,no
!              write(6,*) 'S/R route: l,no,nopt', l, no, nopt(l)
!             check to see if this grid has a flow station
!              if(iflowgrid(l).eq.n.and.nopt(l).eq.2)then
              if(iflowgrid(l).eq.n.and.nopt(l).eq.0)then !changed nopt 
!               we are at a flow station that is to be used for nudging
!               also,nudge only if there is observed flow 
!
                 if(jz.ge.kt.and.qhyd(l,hour_offset+jz).ge.0.0) then 
                    flowsta='y'
                    lsta=l
                 endif
              endif
           end do

           if (flowsta.eq.'y' .and. 
     *         trim(strfw_option)=='streamflow_insertion') then  
!             we are in a grid cell with an observed flow 
!             used instead of the computed flow 
              qo2(n) = qhyd(lsta,hour_offset+jz)           
           endif
!
! 
!         ADD FLOW TO DOWNSTREAM ELEMENT
!         but only if it is not a lake
!
          qi2(lll)=qi2(lll)+qo2(n)

      if(n.eq.naa.and.iopt.eq.3)
     *              write(55,6037)
     *             n,yyy(n),xxx(n),ic,at1,qr(n),qi1(n),qi2(n),
     *        qo1(n),qo2(n),qi2(lll),store1(n),store2(n),cap(n),lll


            if(iopt.ge.3)then
              at1=at/3600.0
              write(55,6037,iostat=ios)
     *             n,yyy(n),xxx(n),ic,at1,qr(n),qi1(n),qi2(n),
     *        qo1(n),qo2(n),qi2(lll),store1(n),store2(n),cap(n),lll
!              if(ios.ne.0)then
!                print*,'n,lll,na/',n,lll,na
!                stop
!              endif
            endif

      endif                            ! SLOPE IF

      if(iopt.eq.2.and.n.eq.naa)print*,'In route, passed 901'

        att(n)=at/3600.

!             added Dec. 12/00 nk.

              if(iopt.ge.1.and.n.eq.nnprint)then
                if(jz.le.1)write(55,5551)
                write(55,5550)id,time,at/3600,qi1(n),qi2(n),
     *                   qo1(n),qo2(n),store1(n),store2(n)
5550            format(i5,f8.2,5f8.1,2e12.3)
5551            format('   id   time         at     qi1     qi2'
     *              '     qo1     qo2     store1     store2')
              endif

      end do


      if ((trim(strfw_option)=='streamflow_comparison') 
     *    .and. iz .eq. no_dt) then

         spl_csv_flag = 1 ! bjd - set to 1 for a csv spl file

         if(spl_csv_flag == 1) then
            if (first_run) then
               write(unit=60,fmt='(A)') 
     *           'Observed and simulated streamflows (m^3/s)'
               write(unit=60,fmt='(A10,'//trim(nostr)//'(",",A8,","))')
     *           'Station,,,', (gage(l),l=1,no)
               write(unit=60,fmt='(A12,'//trim(nostr)
     *           //'(",",F8.3,","))') 
     *           'Longitude,,,',(xstr(l),l=1,no)
               write(unit=60,fmt='(A11,'//trim(nostr)
     *           //'(",",F8.3,","))')
     *           'Latitude,,,',(ystr(l),l=1,no)
               write(unit=60,fmt='(A14,'//trim(nostr)//'(",",I8,","))')
     *           'Xcoord_grid,,,',(jx(l),l=1,no)
               write(unit=60,fmt='(A14,'//trim(nostr)//'(",",I8,","))')
     *           'Ycoord_grid,,,',(iy(l),l=1,no)
               write(unit=60,fmt='(A)') 
     *           'YEAR,MONTH,DAY,HOUR'//repeat(',OBS,SIM',no)
               first_run = .false.
            end if

            write(unit=60,fmt=7000) 
     *        year1, month_now, day_now, hour_now,
     *        (qhyd(l,hour_offset+jz),qo2(iflowgrid(l)),l=1,no)
         else !spl_csv_flag
            if (first_run) then
               write(unit=60,fmt='(A)') 
     *           'Observed and simulated streamflows (m^3/s)'
               write(unit=60,fmt='A7,18X,'//trim(nostr)//'(A16)')
     *           'Station', (gage(l),l=1,no)
               write(unit=60,fmt='A9,11X,'//trim(nostr)//'(8X,F8.3)')
     *           'Longitude',(xstr(l),l=1,no)
               write(unit=60,fmt='A11,'//trim(nostr)//'(8X,F8.3)')
     *           'Latitude',(ystr(l),l=1,no)
               write(unit=60,fmt='A11,9X,'//trim(nostr)//'(8XI8)')
     *           'Xcoord_grid',(jx(l),l=1,no)
               write(unit=60,fmt='A11,9X,'//trim(nostr)//'(8X,I8)')
     *           'Ycoord_grid',(iy(l),l=1,no)
               write(unit=60,fmt='(A)') 
     *           'YEAR MONTH  DAY HOUR'//repeat(',OBS,SIM',no)
               first_run = .false.
            end if

            write(unit=60,fmt='I4,I6,2(I5),'//trim(nostr)//'(3F8.1)') 
     *        year1, month_now, day_now, hour_now,
     *        (qhyd(l,hour_offset+jz),qo2(iflowgrid(l)),l=1,no)
         endif
      end if

      if(iopt.eq.2)print*, 'In route after the 1-naa loop'

! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

      if(iopt.ge.2) write(55,6006)dtmin
      if(iopt.ge.3) write(53,1001)jz

      if(iopt.eq.2)print*, ' finished writing istate'

!     INFORMATION FOR WATBAL.FOR
      do n=1,naa
         netflow(n)=netflow(n)+store1(n)-store2(n)
      end do

      firstpass='n'

      RETURN

! FORMATS

 1000 format(50i3)
 1001 format(3i5)
 1002 format(' i,j,n,istate,bnkfull,qo2/',4i5,2f10.3)
 6000 format(60i2)
 6001 format(' ',3i5,3f10.2)
 6002 format(' route:iz,jz/',2i5)
 6004 format(' gone to rerout - n,res(n),index/',3i5)
 6005 format(' n,res(n),jz,qo2(n)/',3i5,2f10.2)
 6006 format(' dtmin =',f10.2)
 6007 format(' ','reservoir locations wrt 1-naa')
 6037 format(4i5,6f9.3,f10.3,3f15.0,i5)
 6003 format('    n    i    j   ic     at       qr      qi1',
     *'     qi2      qo1      qo2  qi2[lll]   store1    store2    cap   
     *lll')
 7000 format(4(i5,','),F10.3,999(',',F10.3))
 8492 format(' ','initialize resvr flow & storage - in route'/
     *            'n,l,b1,b2,qda,store1')
 8493 format(' in route/res init:',2i5,4e12.3)
 9801 format(a80)
 9802 format(i5,10g12.3)
      END SUBROUTINE route


