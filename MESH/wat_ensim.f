      subroutine ensim(NML, NLTEST, NMTEST, NCOUNT, IMIN, ireport, 
     1wfo_seq, wfo_yy, wfo_mm, wfo_dd, wfo_hh, wfo_mi, wfo_ss, wfo_ms,
     2wf_xx, wf_yy, wf_imax, wf_jmax, wfo_pick,M_Y, M_X, NLAT, NMOS,
     3PREGRD, DELT, TAGRD, TFREZ, FSDOWN, FDLGRD, UVGRD,
     4PRESGRD, QAGRD, ROFBROW, ROFSROW, ROFOROW, ROFROW, FAREROW,
     5QFSROW, QEVPROW, HFSROW, SBC, GTROW, ALVSROW,
     6ALIRROW, FC, FG, FCS, FGS, GZEROC, GZEROG, 
     7GZROCS, GZROGS, SNOROW, ZSNOW, FSNOW, TSNOROW,
     8TBARROW, THLQROW, THICROW, RHOW, DELZ,ILMOS,JLMOS, ILG,
     9CURREC)
C
c Saul Jan 28, 2008
c This are local variables, so they are no anymore in the driver
      REAL*4,       ALLOCATABLE::outwfo(:,:)
      REAL*4,       ALLOCATABLE::outwfo_temp(:,:,:)
      REAL*4,       ALLOCATABLE::outwfo_temp2(:,:,:)
C
      INTEGER wfo_write_attribute_data
      INTEGER wfo_write_timestamp
      INTEGER wf_imax, wf_jmax
      integer wfo_seq, wfo_yy, wfo_mm, wfo_dd, wfo_hh, wfo_mi, wfo_ss, 
     2       wfo_ms,wf_xx(nlat), wf_yy(nlat)
      integer wfo_pick(42), ilmos(ilg), jlmos(ilg)
      INTEGER currec !current record number in the file
c
      REAL PREGRD(NLAT), TAGRD(NLAT), FSDOWN(NLAT), FDLGRD(NLAT)
	REAL UVGRD(NLAT), PRESGRD(NLAT), QAGRD(NLAT), 
     D      QFSROW (NLAT,NMOS), ROFROW (NLAT,NMOS), ROFOROW(NLAT,NMOS),
     F	  ROFSROW(NLAT,NMOS), ROFBROW(NLAT,NMOS), WTRCROW(NLAT,NMOS), 
     G      FAREROW(NLAT,NMOS), QEVPROW(NLAT,NMOS), ALIRROW(NLAT,NMOS), 
     4      ALVSROW(NLAT,NMOS), HFSROW (NLAT,NMOS), GTROW  (NLAT,NMOS)
      REAL    TBARROW(NLAT,NMOS,3), THLQROW(NLAT,NMOS,3), 
     1		THICROW(NLAT,NMOS,3),  TSNOROW(NLAT,NMOS),
     4        SNOROW (NLAT,NMOS)
      REAL  FC     ( ILG), FG     ( ILG), FCS    ( ILG), FGS    ( ILG),
     1      ZSNOW  ( ILG), FSNOW  ( ILG),  DELZ(3),
     B      GZEROC ( ILG), GZEROG ( ILG), GZROCS ( ILG), GZROGS ( ILG)
           
c Saul Jan 28, 2008
C         Allocate the necessary arrays
          ALLOCATE (outwfo(wf_jmax,wf_imax))
          ALLOCATE (outwfo_temp(10,wf_jmax,wf_imax))
          ALLOCATE (outwfo_temp2(10,wf_jmax,wf_imax))
C
C
C Initialize ENSIM specific arrays
         do jj=1,wf_imax
            do ii=1,wf_jmax
              outwfo(ii,jj)=0
              do j=1,10
                outwfo_temp(j,ii,jj)=0
                outwfo_temp2(j,ii,jj)=0
              end do
            end do
         end do
C
C
C Write ENSIM output
c-----------------------------------------------------c
!           WRITE STUFF FOR ENSIM:
            if(ireport.ge.1)then
            if(mod(NCOUNT,ireport).eq.0)then

               wfo_seq=wfo_seq+1
!              SET TIME YR,MONTH,DAY,HR,MIN,SEC,MILLISEC FOR THIS STEP
!               wfo_yy=0
!               wfo_mm=0
!               wfo_dd=0
!               if(IMIN.EQ.0) then
!			    wfo_hh=wfo_hh+1
!	         endif
!               wfo_mi=wfo_mi+30
!               wfo_ss=0
!               wfo_ms=0

!              WRITE THE TIMESTAMP
               if(wfo_write_timestamp(wfo_seq,wfo_seq,wfo_yy,wfo_mm,
     *            wfo_dd,wfo_hh,wfo_mi,wfo_ss,wfo_ms,currec) .ne. 1)
     *              then
                  write (*,'(A)') ' '
                  write (*,'(A)') '  *** FATAL ERROR ***     '
                  write (*,'(A)') ' Unable to Write Timestamp'
                  STOP 'Program terminated in runclass.f'
               end if
!                write(57) wfo_seq, wfo_yy, wfo_mm, wfo_dd, wfo_hh
            endif
            endif

* initialize outwfo to zero
c                 do I=1,NLTEST
c					ii=wf_xx(I)
c					jj=wf_yy(I)
c					outwfo(ii,jj)=0.0
c                 end do
c     Write out attribute data. Attributes 1 to 7 are forcing data, which
c     are currently only distributed by grid-square and not GRU.

!              ATTRIBUTE 1 - PRECIP
!              prepare the output
               if(wfo_pick(1).eq.1)then
                 do I=1,NLTEST
					ii=wf_xx(I)
					jj=wf_yy(I)
					outwfo(ii,jj)=PREGRD(I)*DELT
                 end do
!                write the output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if
!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 2 - TEMP
!              prepare the output
               if(wfo_pick(2).eq.1)then
                 do I=1,NLTEST
					ii=wf_xx(I)
					jj=wf_yy(I)
					outwfo(ii,jj)=TAGRD(I)-TFREZ
c					outwfo(ii,jj)=50
                              !print*,"outwfo(ii,jj)",outwfo(ii,jj)
                 end do
!                write the output
                 !print*,wf_imax,wf_jmax
                 !do i=1,wf_jmax
                 !  print*,(outwfo(i,j),j=1,wf_imax)
                 !enddo
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if
!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 3 - Incoming Short
!              prepare the output
               if(wfo_pick(3).eq.1)then
                 do I=1,NLTEST
					ii=wf_xx(I)
					jj=wf_yy(I)
					outwfo(ii,jj)=FSDOWN(I)
                 end do
!                write the output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if
!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 4 - Incoming Long
!              prepare the output
               if(wfo_pick(4).eq.1)then
                 do I=1,NLTEST
					ii=wf_xx(I)
					jj=wf_yy(I)
					outwfo(ii,jj)=FDLGRD(I)
                 end do
!                write the output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if
!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 5 - Wind Speed
!              prepare the output
               if(wfo_pick(5).eq.1)then
                 do I=1,NLTEST
					ii=wf_xx(I)
					jj=wf_yy(I)
					outwfo(ii,jj)=UVGRD(I)
                 end do
!                write the output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if
!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 6 - Pressure
!              prepare the output
               if(wfo_pick(6).eq.1)then
                 do I=1,NLTEST
					ii=wf_xx(I)
					jj=wf_yy(I)
					outwfo(ii,jj)=PRESGRD(I)
                 end do
!                write the output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if
!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 7 - Humidity
!              prepare the output
               if(wfo_pick(7).eq.1)then
                 do I=1,NLTEST
					ii=wf_xx(I)
					jj=wf_yy(I)
					outwfo(ii,jj)=QAGRD(I)
                 end do
!                write the output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if
!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

*     Attributes 8 to 11 are GRU flow components.

!              ATTRIBUTE 8 - Baseflow
               if(wfo_pick(8).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                            +ROFBROW(N,M)*FAREROW(N,M)*DELT
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =ROFBROW(N,M)*DELT
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =ROFBROW(N,M)*FAREROW(N,M)*DELT
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 9 - Interflow
               if(wfo_pick(9).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                            +ROFSROW(N,M)*FAREROW(N,M)*DELT
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =ROFSROW(N,M)*DELT
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =ROFSROW(N,M)*FAREROW(N,M)*DELT
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 10 - Overland Flow
               if(wfo_pick(10).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                            +ROFOROW(N,M)*FAREROW(N,M)*DELT
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =ROFOROW(N,M)*DELT
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =ROFOROW(N,M)*FAREROW(N,M)*DELT
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 11 - Baseflow + Interflow + Overland Flow
               if(wfo_pick(11).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
c                    if(ii.gt.wf_jmax)print*,"i=  ",i,"ii=  ",ii  
c                    if(jj.gt.wf_imax)print*,"i=  ",i,"jj=  ",jj
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                            +ROFROW(N,M)*FAREROW(N,M)*DELT
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =ROFROW(N,M)*DELT
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =ROFROW(N,M)*FAREROW(N,M)*DELT
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
		     do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
c                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
c     *            		   outwfo).ne.1)then
!                    WRITE ERROR
c                     GO TO 98010
c                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
c                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
c     *            		   outwfo).ne.1)then
!                    WRITE ERROR
c                     GO TO 98010
c                   end if
                 end do

!                reset outwfo	           
                 do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 12 - Evapotranspiration
               if(wfo_pick(12).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                            +QFSROW(N,M)*FAREROW(N,M)*DELT
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =QFSROW(N,M)*DELT
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =QFSROW(N,M)*FAREROW(N,M)*DELT
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
                 do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
c                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
c     *            		   outwfo).ne.1)then
!                    WRITE ERROR
c                     GO TO 98010
c                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
c                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
c     *            		   outwfo).ne.1)then
!                    WRITE ERROR
c                     GO TO 98010
c                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 13 - Latent Heat Flux 
               if(wfo_pick(13).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                            +QEVPROW(N,M)*FAREROW(N,M)
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =QEVPROW(N,M)
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =QEVPROW(N,M)*FAREROW(N,M)
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
c                  if(wfo_write_attribute_data(wf_imax,wf_jmax,
c     *            		   outwfo).ne.1)then
!                    WRITE ERROR
c                     GO TO 98010
c                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
c                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
c     *            		   outwfo).ne.1)then
!                    WRITE ERROR
c                     GO TO 98010
c                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 14 - Sensible Heat Flux 
               if(wfo_pick(14).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                            +HFSROW(N,M)*FAREROW(N,M)
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =HFSROW(N,M)
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =HFSROW(N,M)*FAREROW(N,M)
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 15 - Net LW Radiation
               if(wfo_pick(15).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                  +(FDLGRD(N)-SBC*GTROW(N,M)**4)*FAREROW(N,M)
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =(FDLGRD(N)-SBC*GTROW(N,M)**4)
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =(FDLGRD(N)-SBC*GTROW(N,M)**4)*FAREROW(N,M)
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 16 - Net SW Radiation
               if(wfo_pick(16).eq.1)then
                 ! prepare the output
                 do I=1,NML

                    n = ilmos(I)
                    m = jlmos(I)

                 IF(FSDOWN(I).GT.0.0) THEN
                     ALTOT=(ALVSROW(N,M)+ALIRROW(N,M))/2.0
                 ELSE
                     ALTOT=0.0
                 ENDIF
                 FSSTAR=FSDOWN(N)*(1.0-ALTOT)

                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                            +FSSTAR*FAREROW(N,M)
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =FSSTAR
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =FSSTAR*FAREROW(N,M)
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 17 - Ground Heat Flux
               if(wfo_pick(17).eq.1)then

                 ! prepare the output
                 do I=1,NML

                 QG=FC(I)* GZEROC(I)+FG(I)* GZEROG(I)+
     +              FCS(I)*GZROCS(I)+FGS(I)*GZROGS(I)

                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                            +QG*FAREROW(N,M)
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =QG
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =QG*FAREROW(N,M)
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 18 - Snow Water Equivalent
               if(wfo_pick(18).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                            +SNOROW(N,M)*FAREROW(N,M)
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =SNOROW(N,M)
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =SNOROW(N,M)*FAREROW(N,M)
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 19 - Snow Depth
               if(wfo_pick(19).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                            +ZSNOW(I)*FAREROW(N,M)
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =ZSNOW(I)
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =ZSNOW(I)*FAREROW(N,M)
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 20 - Snow Covered Area
               if(wfo_pick(20).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                            +FSNOW(I)*FAREROW(N,M)
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =FSNOW(I)
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =FSNOW(I)*FAREROW(N,M)
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 21 - Snow Temperature
               if(wfo_pick(21).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                            +TSNOROW(N,M)*FAREROW(N,M)
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =TSNOROW(N,M)
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =TSNOROW(N,M)*FAREROW(N,M)
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 22 - Soil Temperature, Layer 1
               if(wfo_pick(22).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                            +TBARROW(N,M,1)*FAREROW(N,M)
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =TBARROW(N,M,1)
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =TBARROW(N,M,1)*FAREROW(N,M)
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 23 - Soil Temperature, Layer 2
               if(wfo_pick(23).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                            +TBARROW(N,M,2)*FAREROW(N,M)
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =TBARROW(N,M,2)
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =TBARROW(N,M,2)*FAREROW(N,M)
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 24 - Soil Temperature, Layer 3
               if(wfo_pick(24).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                            +TBARROW(N,M,3)*FAREROW(N,M)
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =TBARROW(N,M,3)
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =TBARROW(N,M,3)*FAREROW(N,M)
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 25 - Volumetric Liquid Water, Layer 1
               if(wfo_pick(25).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                            +THLQROW(N,M,1)*FAREROW(N,M)
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =THLQROW(N,M,1)
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =THLQROW(N,M,1)*FAREROW(N,M)
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
c                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
c     *            		   outwfo).ne.1)then
!                    WRITE ERROR
c                     GO TO 98010
c                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
c                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
c     *            		   outwfo).ne.1)then
!                    WRITE ERROR
c                     GO TO 98010
c                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 26 - Volumetric Liquid Water, Layer 2
               if(wfo_pick(26).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                            +THLQROW(N,M,2)*FAREROW(N,M)
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =THLQROW(N,M,2)
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =THLQROW(N,M,2)*FAREROW(N,M)
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 27 - Volumetric Liquid Water, Layer 3
               if(wfo_pick(27).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                            +THLQROW(N,M,3)*FAREROW(N,M)
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =THLQROW(N,M,3)
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =THLQROW(N,M,3)*FAREROW(N,M)
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 28 - Volumetric Frozen Water, Layer 1
               if(wfo_pick(28).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                            +THICROW(N,M,1)*FAREROW(N,M)
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =THICROW(N,M,1)
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =THICROW(N,M,1)*FAREROW(N,M)
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 29 - Volumetric Frozen Water, Layer 2
               if(wfo_pick(29).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                            +THICROW(N,M,2)*FAREROW(N,M)
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =THICROW(N,M,2)
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =THICROW(N,M,2)*FAREROW(N,M)
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 30 - Volumetric Frozen Water, Layer 3
               if(wfo_pick(30).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                            +THICROW(N,M,3)*FAREROW(N,M)
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =THICROW(N,M,3)
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =THICROW(N,M,3)*FAREROW(N,M)
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 31 - mm Liquid Water, Layer 1
               if(wfo_pick(31).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                        +THLQROW(N,M,1)*DELZ(1)*RHOW*FAREROW(N,M)
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =THLQROW(N,M,1)*DELZ(1)*RHOW
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =THLQROW(N,M,1)*DELZ(1)*RHOW*FAREROW(N,M)
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
c                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
c     *            		   outwfo).ne.1)then
!                    WRITE ERROR
c                     GO TO 98010
c                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
c                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
c     *            		   outwfo).ne.1)then
!                    WRITE ERROR
c                     GO TO 98010
c                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 32 - mm Liquid Water, Layer 2
               if(wfo_pick(32).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                         +THLQROW(N,M,2)*DELZ(2)*RHOW*FAREROW(N,M)
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =THLQROW(N,M,2)*DELZ(2)*RHOW
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =THLQROW(N,M,2)*DELZ(2)*RHOW*FAREROW(N,M)
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 33 - mm Liquid Water, Layer 3
               if(wfo_pick(33).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                       +THLQROW(N,M,3)*DELZ(3)*RHOW*FAREROW(N,M)
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =THLQROW(N,M,3)*DELZ(3)*RHOW
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =THLQROW(N,M,3)*DELZ(3)*RHOW*FAREROW(N,M)
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 34 - mm Frozen Water, Layer 1
               if(wfo_pick(34).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                        +THLQROW(N,M,1)*DELZ(1)*RHOW*FAREROW(N,M)
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =THLQROW(N,M,1)*DELZ(1)*RHOW
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =THLQROW(N,M,1)*DELZ(1)*RHOW*FAREROW(N,M)
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 35 - mm Frozen Water, Layer 2
               if(wfo_pick(35).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                        +THLQROW(N,M,2)*DELZ(2)*RHOW*FAREROW(N,M)
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =THLQROW(N,M,2)*DELZ(2)*RHOW
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =THLQROW(N,M,2)*DELZ(2)*RHOW*FAREROW(N,M)
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif

!              ATTRIBUTE 36 - mm Frozen Water, Layer 3
               if(wfo_pick(36).eq.1)then

                 ! prepare the output
                 do I=1,NML
                    n = ilmos(I)
                    m = jlmos(I)
                    ii=wf_xx(n)
                    jj=wf_yy(n)
                    ! first get the grid value
                    outwfo(ii,jj)=outwfo(ii,jj)
     *                        +THLQROW(N,M,3)*DELZ(3)*RHOW*FAREROW(N,M)
                    ! second get the GRU value
                    j=jlmos(I)
                    outwfo_temp(j,ii,jj)
     *                            =THLQROW(N,M,3)*DELZ(3)
                    ! third get the GRU contribution to the grid
                    outwfo_temp2(j,ii,jj)
     *                  =THLQROW(N,M,3)*DELZ(3)*FAREROW(N,M)
                 end do

!                write the grid-square output
                 if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                  WRITE ERROR
                   GO TO 98010
                 end if

!                reset outwfo (necessary here because all grids might not have all GRU's)
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do

!                write the GRU output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                write the GRU contribution to grid square output
                 do J=1,NMTEST
                   do jj=1,wf_imax
                     do ii=1,wf_jmax
                       outwfo(ii,jj)=outwfo_temp2(j,ii,jj)
                     end do
                   end do
                   if(wfo_write_attribute_data(wf_imax,wf_jmax,
     *            		   outwfo,currec).ne.1)then
!                    WRITE ERROR
                     GO TO 98010
                   end if
                 end do

!                reset outwfo	           
			   do jj=1,wf_imax
	              do ii=1,wf_jmax
                       outwfo(ii,jj)=0
	              end do
	           end do
               endif
      return

C     THIS IS THE WFO ATTRIBUTE WRITING ERROR
98010 write (*,'(A)') ' '
      write (*,'(A)') '  *** FATAL ERROR ***     '
      write (*,'(A)') ' Unable to Write Attribute Data'
      STOP 'Program aborted in wf_myrunclass due to ENSIM writing error'

      END subroutine ensim



       SUBROUTINE write_both_headers(coordsys,xcount,
     *           ycount,delta,xorigin,yorigin,ntype,jan, 
     *           wfo_pick,wf_landclassname,NMOS,first_record)

!***********************************************************************
!    REV. 9.00   Mar.  2000  -  TS: CONVERTED TO FORTRAN 90

!***********************************************************************

        IMPLICIT NONE

c        include "wf_inc.f"

!bjd        integer,      dimension(:), allocatable :: wfo_pick
        integer,      intent(IN) :: wfo_pick(42)
        character*50, dimension(:), allocatable :: wfo_attributes
        integer      :: iopt,itype,iymin,iymax,jxmin,jxmax,imax,imin,
     *                  jmin,jmax,ib,it,no,ndam,ni,id,nl,mhtot,kt,
     *                  irads,iradn,jradw,jrade,iyoffset,jxoffset,
     *                  iyshift,jxshift,istep,nblock,
     *                  ireport,ioflg,ichsm,nnprint
        integer         :: rgrd,rads,radw,rade,radn,rgrdn,rgrde
        integer         :: mm,yy,iall,nclt,nch
        integer(2)      :: year,month,day,hrs,mins,secs,hsecs,hh,dd
        integer, parameter :: flen=9000
        real            :: ver,grdn,grde,al,astep,step2,scalesnw,
     *                     scaletem,sstep
        character(1)    :: snwflg,sedflg,vapflg,smrflg,resinflg,
     *                     resumflg,tbcflg,contflg,routeflg,crseflg,
     *                     ensimflg,leapflg,llflg,picflg,wetflg
        character(5)    :: source,rdr
        character(80)   :: querystring
        character(10)   :: wf_landclassnameVal
        integer         :: NMOS
        character*10, intent(IN) :: wf_landclassname(NMOS)
        integer, intent(OUT) :: first_record
        integer         :: header_length

!      integer :: jan,ii,n,i,j,i3,ii1,ii2
!      real ::    aintvl


!      llflg  - when 'y', coordinates for .str .snw etc in lat-long
!      ioflg  - if .ge. 1 read the outfiles (note: integer)
!       leapflg  - to indicate a leap year - not an input

!       snwflg - whether there is snow to melt
!       sedflg - whether the sediment routine is used y or n
!       vapflg - turn on evap routine (Todd)
!       smrflg - turns on smearing (smear precip data over data dt)
!       resinflg - will use resin record for comparison
!       tbcflg - read resume.txt file for run init values
!       resumflg - resume.txt file written at end of run (mem dump)
!     contflg  - for continuing statistics upon resume = input
!     routeflg - output qr grids for routing program
!     crseflg  - read snow course data to replace resume file data
!     ensimflg - write the wfo file for ENSIM
!     ensimflg1 = 'y' for first time needed, else = 'n' (inpevt)
!      first_record - the first record location after the header

!       source - what is the data source - radar, mc2, erf, rag, etc.

        character(30), dimension(:), allocatable :: fln,
     *                                              filename
        character(30), dimension(:), allocatable :: outfln



!  INPUT PARAMETERS: FILENAME FOR WFO FILE, UNIT NUMBER, NUMER OF 
!                    COLUMNS, NUMBER OF ROWS, GRID SIZE, GRID ORIGINS
!                    (X,Y), NUMBER OF LANDCOVER TYPES

!     SAVES THE LOCAL VARIABLES FROM ONE RUN TO NEXT
      SAVE

        CHARACTER(64):: appname
        CHARACTER(2) :: num_to_char(16)
        CHARACTER(*) :: coordsys
        INTEGER      :: xcount,ycount,ntype,i,j,k,attcount,seq,step,
     *               nsteps,attnum,iAllocate,iDeallocate,jan
        REAL         :: delta,xorigin,yorigin

        CHARACTER(64), DIMENSION(:), ALLOCATABLE :: attname
        CHARACTER(32), DIMENSION(:), ALLOCATABLE :: attunits

      DATA num_to_char/'01','02','03','04','05','06','07','08',
     *                 '09','10','11','12','13','14','15','16'/
      
!       WFO IO FUNCTIONS
        INTEGER wfo_open_file,wfo_write_header
        INTEGER wfo_write_attribute_header
        INTEGER wfo_close_header

      appname = 'WATFLOW'
!     coordsys='UTM'       !UTM or LATLONG

!     SAMPLE ATTRIBUTES
!     SET UP ATTRIBUTE INFORMATION (NAMES AND UNITS)


      header_length = 0


!      attcount=5+6*ntype
      attcount=6+60*ntype
!     attcount redefined below

      allocate(attname(attcount),attunits(attcount),stat=iAllocate)
      if(iAllocate.ne.0) STOP
     *   'Error with allocation of arrays in write_both_headers'

      j=0
      if(wfo_pick(1).eq.1)then
        j=j+1
        attname(j)='Precipitation'
        attunits(j)='mm'
      endif
      if(wfo_pick(2).eq.1)then
        j=j+1
        attname(j)='Temperature'
        attunits(j)='Degrees Celsius'
      endif
      if(wfo_pick(3).eq.1)then
        j=j+1
        attname(j)='Incoming SW'
        attunits(j)='W/m2'
      endif
      if(wfo_pick(4).eq.1)then
        j=j+1
        attname(j)='Incoming LW'
        attunits(j)='W/m2'
      endif
      if(wfo_pick(5).eq.1)then
        j=j+1
        attname(j)='Wind Speed'
        attunits(j)='m/s'
      endif
      if(wfo_pick(6).eq.1)then
        j=j+1
        attname(j)='Pressure'
        attunits(j)='Pa'
      endif
      if(wfo_pick(7).eq.1)then
        j=j+1
        attname(j)='Specific Humidity'
        attunits(j)='g/g'
      endif
      if(wfo_pick(8).eq.1)then
        j=j+1
        attname(j)='Grid Square Baseflow'
        attunits(j)='mm'
        do k=1,ntype
          j=j+1
          attname(j)='GRU Baseflow:'
          attunits(j)='mm'
        enddo
        do k=1,ntype
          j=j+1
          attname(j)='GRU relative contribution to Grid Baseflow:'
          attunits(j)='mm'
        enddo
      endif
      if(wfo_pick(9).eq.1)then
        j=j+1
        attname(j)='Grid Square Interflow'
        attunits(j)='mm'
        do k=1,ntype
          j=j+1
          attname(j)='GRU Interflow:'
          attunits(j)='mm'
        enddo
        do k=1,ntype
          j=j+1
          attname(j)='GRU relative contribution to Grid Interflow:'
          attunits(j)='mm'
        enddo
      endif
      if(wfo_pick(10).eq.1)then
        j=j+1
        attname(j)='Grid Square Overland Flow'
        attunits(j)='mm'
        do k=1,ntype
          j=j+1
          attname(j)='GRU Overland Flow:'
          attunits(j)='mm'
        enddo
        do k=1,ntype
          j=j+1
          attname(j)='GRU relative contribution to Grid Overland Flow:'
          attunits(j)='mm'
        enddo
      endif
      if(wfo_pick(11).eq.1)then
        j=j+1
        attname(j)='Grid Square B+I+O'
        attunits(j)='mm'
c        do k=1,ntype
c          j=j+1
c          attname(j)='GRU B+I+O:'
c          attunits(j)='mm'
c        enddo
c        do k=1,ntype
c          j=j+1
c          attname(j)='GRU relative contribution to Grid B+I+O:'
c          attunits(j)='mm'
c        enddo
      endif
      if(wfo_pick(12).eq.1)then
        j=j+1
        attname(j)='Grid Square Evapotranspiration'
        attunits(j)='mm'
c        do k=1,ntype
c          j=j+1
c          attname(j)='GRU Evapotranspiration: '
c          attunits(j)='mm'
c        enddo
c        do k=1,ntype
c          j=j+1
c          attname(j)='GRU relative contribution to Grid ET:'
c          attunits(j)='mm'
c        enddo
      endif
      if(wfo_pick(13).eq.1)then
        j=j+1
        attname(j)='Grid Square Latent Heat flux'
        attunits(j)='W/m2'
c        do k=1,ntype
c          j=j+1
c          attname(j)='GRU Latent Heat Flux: '
c          attunits(j)='W/m2'
c        enddo
c        do k=1,ntype
c          j=j+1
c          attname(j)='GRU contr to Grid QE Flux: '
c          attunits(j)='W/m2'
c        enddo
      endif
      if(wfo_pick(14).eq.1)then
        j=j+1
        attname(j)='Grid Square Sensible Heat flux'
        attunits(j)='W/m2'
        do k=1,ntype
          j=j+1
          attname(j)='GRU Sensible Heat Flux: '
          attunits(j)='W/m2'
        enddo
        do k=1,ntype
          j=j+1
          attname(j)='GRU contr to Grid QH Flux: '
          attunits(j)='W/m2'
        enddo
      endif
      if(wfo_pick(15).eq.1)then
        j=j+1
        attname(j)='Grid Square Net LW Rad'
        attunits(j)='W/m2'
        do k=1,ntype
          j=j+1
          attname(j)='GRU Net LW Rad: '
          attunits(j)='W/m2'
        enddo
        do k=1,ntype
          j=j+1
          attname(j)='GRU relative contribution to Grid Net LW Rad:'
          attunits(j)='W/m2'
        enddo
      endif
      if(wfo_pick(16).eq.1)then
        j=j+1
        attname(j)='Grid Square Net SW Rad'
        attunits(j)='W/m2'
        do k=1,ntype
          j=j+1
          attname(j)='GRU Net SW Rad: '
          attunits(j)='W/m2'
        enddo
        do k=1,ntype
          j=j+1
          attname(j)='GRU relative contribution to Grid Net SW Rad:'
          attunits(j)='W/m2'
        enddo
      endif
      if(wfo_pick(17).eq.1)then
        j=j+1
        attname(j)='Grid Square Ground Heat Flux'
        attunits(j)='W/m2'
        do k=1,ntype
          j=j+1
          attname(j)='GRU Ground Heat Flux: '
          attunits(j)='W/m2'
        enddo
        do k=1,ntype
          j=j+1
          attname(j)='GRU contr to Grid Ground Heat Flux: '
          attunits(j)='W/m2'
        enddo
      endif
      if(wfo_pick(18).eq.1)then
        j=j+1
        attname(j)='Grid Square SWE'
        attunits(j)='mm'
        do k=1,ntype
          j=j+1
          attname(j)='GRU SWE: '
          attunits(j)='mm'
        enddo
        do k=1,ntype
          j=j+1
          attname(j)='GRU relative contribution to Grid SWE:'
          attunits(j)='mm'
        enddo
      endif
      if(wfo_pick(19).eq.1)then
        j=j+1
        attname(j)='Grid Square Snow Depth'
        attunits(j)='m'
        do k=1,ntype
          j=j+1
          attname(j)='GRU Snow Depth: '
          attunits(j)='m'
        enddo
        do k=1,ntype
          j=j+1
          attname(j)='GRU relative contribution to Grid Snow Depth:'
          attunits(j)='m'
        enddo
      endif
      if(wfo_pick(20).eq.1)then
        j=j+1
        attname(j)='Grid Square SCA'
        attunits(j)='Fraction'
        do k=1,ntype
          j=j+1
          attname(j)='GRU SCA: '
          attunits(j)='Fraction'
        enddo
        do k=1,ntype
          j=j+1
          attname(j)='GRU relative contribution to Grid SCA:'
          attunits(j)='Fraction'
        enddo
      endif
      if(wfo_pick(21).eq.1)then
        j=j+1
        attname(j)='Grid Square Snow Temperature'
        attunits(j)='Degrees Celsius'
        do k=1,ntype
          j=j+1
          attname(j)='GRU Snow Temperature: '
          attunits(j)='Degrees Celsius'
        enddo
        do k=1,ntype
          j=j+1
          attname(j)='GRU relative contribution to Grid Snow Temp:'
          attunits(j)='Degrees Celsius'
        enddo
      endif
      if(wfo_pick(22).eq.1)then
        j=j+1
        attname(j)='Grid Square Soil Temp, Layer 1'
        attunits(j)='Celsius'
        do k=1,ntype
          j=j+1
          attname(j)='GRU Soil Temp, Layer 1: '
          attunits(j)='Celsius'
        enddo
        do k=1,ntype
          j=j+1
          attname(j)='GRU relative contribution to Grid Soil Temp - L1:'
          attunits(j)='Celsius'
        enddo
      endif
      if(wfo_pick(23).eq.1)then
        j=j+1
        attname(j)='Grid Square Soil Temp, Layer 2'
        attunits(j)='Celsius'
        do k=1,ntype
          j=j+1
          attname(j)='GRU Soil Temp, Layer 2: '
          attunits(j)='Celsius'
        enddo
        do k=1,ntype
          j=j+1
          attname(j)='GRU relative contribution to Grid Soil Temp - L2:'
          attunits(j)='Celsius'
        enddo
      endif
      if(wfo_pick(24).eq.1)then
        j=j+1
        attname(j)='Grid Square Soil Temp, Layer 3'
        attunits(j)='Celsius'
        do k=1,ntype
          j=j+1
          attname(j)='GRU Soil Temp, Layer 3: '
          attunits(j)='Celsius'
        enddo
        do k=1,ntype
          j=j+1
          attname(j)='GRU relative contribution to Grid Soil Temp - L3:'
          attunits(j)='Celsius'
        enddo
      endif
      if(wfo_pick(25).eq.1)then
        j=j+1
        attname(j)='Grid Square Vol Liq Water, L1'
        attunits(j)='Fraction'
c        do k=1,ntype
c          j=j+1
c          attname(j)='GRU Vol Liq Water, L1: '
c          attunits(j)='Fraction'
c        enddo
c        do k=1,ntype
c          j=j+1
c          attname(j)='GRU relative contrib. to Grid Vol Liq Water - L1:'
c          attunits(j)='Fraction'
c        enddo
      endif
      if(wfo_pick(26).eq.1)then
        j=j+1
        attname(j)='Grid Square Vol Liq Water, L2'
        attunits(j)='Fraction'
        do k=1,ntype
          j=j+1
          attname(j)='GRU Vol Liq Water, L2: '
          attunits(j)='Fraction'
        enddo
        do k=1,ntype
          j=j+1
          attname(j)='GRU relative contrib. to Grid Vol Liq Water - L2:'
          attunits(j)='Fraction'
        enddo
      endif
      if(wfo_pick(27).eq.1)then
        j=j+1
        attname(j)='Grid Square Vol Liq Water, L3'
        attunits(j)='Fraction'
        do k=1,ntype
          j=j+1
          attname(j)='GRU Vol Liq Water, L3: '
          attunits(j)='Fraction'
        enddo
        do k=1,ntype
          j=j+1
          attname(j)='GRU relative contrib. to Grid Vol Liq Water - L3:'
          attunits(j)='Fraction'
        enddo
      endif
      if(wfo_pick(28).eq.1)then
        j=j+1
        attname(j)='Grid Vol Ice, L1'
        attunits(j)='Fraction'
        do k=1,ntype
          j=j+1
          attname(j)='GRU Vol Ice, L1: '
          attunits(j)='Fraction'
        enddo
        do k=1,ntype
          j=j+1
          attname(j)='GRU relative contrib. to Grid Vol Ice - L1:'
          attunits(j)='Fraction'
        enddo
      endif
      if(wfo_pick(29).eq.1)then
        j=j+1
        attname(j)='Grid Square Vol Ice, L2'
        attunits(j)='Fraction'
        do k=1,ntype
          j=j+1
          attname(j)='GRU Vol Ice, L2: '
          attunits(j)='Fraction'
        enddo
        do k=1,ntype
          j=j+1
          attname(j)='GRU relative contrib. to Grid Vol Ice - L2:'
          attunits(j)='Fraction'
        enddo
      endif
      if(wfo_pick(30).eq.1)then
        j=j+1
        attname(j)='Grid Square Vol Ice, L3'
        attunits(j)='Fraction'
        do k=1,ntype
          j=j+1
          attname(j)='GRU Vol Ice, L3: '
          attunits(j)='Fraction'
        enddo
        do k=1,ntype
          j=j+1
          attname(j)='GRU relative contrib. to Grid Vol Ice - L3:'
          attunits(j)='Fraction'
        enddo
      endif
      if(wfo_pick(31).eq.1)then
        j=j+1
        attname(j)='Grid Square Liq Water, L1'
        attunits(j)='mm'
c        do k=1,ntype
c          j=j+1
c          attname(j)='GRU Liq Water, L1: '
c          attunits(j)='mm'
c        enddo
c        do k=1,ntype
c          j=j+1
c          attname(j)='GRU contribution to Grid Liq Water, L1: '
c          attunits(j)='mm'
c        enddo
      endif
      if(wfo_pick(32).eq.1)then
        j=j+1
        attname(j)='Grid Square Liq Water, L2'
        attunits(j)='mm'
        do k=1,ntype
          j=j+1
          attname(j)='GRU Liq Water, L2: '
          attunits(j)='mm'
        enddo
        do k=1,ntype
          j=j+1
          attname(j)='GRU contribution to Grid Liq Water, L2: '
          attunits(j)='mm'
        enddo
      endif
      if(wfo_pick(33).eq.1)then
        j=j+1
        attname(j)='Grid Square Liq Water, L3'
        attunits(j)='mm'
        do k=1,ntype
          j=j+1
          attname(j)='GRU Liq Water, L3: '
          attunits(j)='mm'
        enddo
        do k=1,ntype
          j=j+1
          attname(j)='GRU contribution to Grid Liq Water, L3: '
          attunits(j)='mm'
        enddo
      endif
      if(wfo_pick(34).eq.1)then
        j=j+1
        attname(j)='Grid Square Ice, L1'
        attunits(j)='mm'
        do k=1,ntype
          j=j+1
          attname(j)='GRU Ice, L1: '
          attunits(j)='mm'
        enddo
        do k=1,ntype
          j=j+1
          attname(j)='GRU relative contribution to Grid Ice, L1:'
          attunits(j)='mm'
        enddo
      endif
      if(wfo_pick(35).eq.1)then
        j=j+1
        attname(j)='Grid Square Ice, L2'
        attunits(j)='mm'
        do k=1,ntype
          j=j+1
          attname(j)='GRU Ice, L2: '
          attunits(j)='mm'
        enddo
        do k=1,ntype
          j=j+1
          wf_landclassnameVal=wf_landclassname(k)
          attname(j)='GRU relative contribution to Grid Ice, L2:'
          attunits(j)='mm'
        enddo
      endif
      if(wfo_pick(36).eq.1)then
        j=j+1
        attname(j)='Grid Square Ice, L3'
        attunits(j)='mm'
        do k=1,ntype
          j=j+1
          attname(j)='GRU Ice, L3: '
          attunits(j)='mm'
        enddo
        do k=1,ntype
          j=j+1
          attname(j)='GRU relative contribution to Grid Ice, L3:'
          attunits(j)='mm'
        enddo
      endif


!     attcount redefined here
      attcount=j


!        OPEN THE FILE FOR WRITING
      if(wfo_open_file(.FALSE.).ne.1)then
        write(*,'(A)') ' '
        write(*,'(A)') '  *** FATAL ERROR ***     '
        write(*,'(A)') ' Unable to Open File for Writing'

        write(*,'(9X,(A))') ' watflow.wfo'
        STOP 'Program terminated in write_both_headers @89'
      end if

!     WRITE THE MAIN FILE HEADER
      if(wfo_write_header(appname,xorigin,yorigin,xcount,ycount,
     *                    delta,coordsys,header_length) .ne. 1)then
         write(*,'(A)') ' '
         write(*,'(A)') '  *** FATAL ERROR ***     '
         write(*,'(A)') ' Unable to Write Main Header'
         STOP 'Program terminated in write_both_headers @98'
      end if

!     WRITE THE ATTRIBUTE HEADER
      if(wfo_write_attribute_header(attcount,attname,attunits,
     *                              header_length) .ne. 1)then
         write(*,'(A)') ' '
         write(*,'(A)') '  *** FATAL ERROR ***     '
         write(*,'(A)') ' Unable to Write Attribute Header'
         STOP 'Program terminated in write_both_headers @107'
      end if	

!     CLOSE THE HEADER
      if(wfo_close_header(header_length).ne. 1)then
         write(*,'(A)') ' '
         write(*,'(A)') '  *** FATAL ERROR ***     '
         write(*,'(A)') ' Unable to Close Header'
         STOP 'Program terminated in write_both_headers @115'
      end if
      
!todo should be put into a function later
      close(57)
      if(wfo_open_file(.TRUE.).ne.1)then
        write(*,'(A)') ' '
        write(*,'(A)') '  *** FATAL ERROR ***     '
        write(*,'(A)') ' Unable to Open File for Writing'

        write(*,'(9X,(A))') ' watflow.wfo'
        STOP 'Program terminated in write_both_headers @130'
      end if
      
      first_record = header_length / 4 + 1

! DEALLOCATIONS OF ARRAYS:
      deallocate(attname,attunits,stat=iDeallocate)
      if (iDeallocate.ne.0) STOP   
     *    'Error with deallocation of ensim arrays in wfocodea'

      RETURN

      END SUBROUTINE write_both_headers

! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

!***********************************************************************
      INTEGER FUNCTION wfo_open_file(BINARY)
!***********************************************************************
!    REV. 9.00   Mar.  2000  -  TS: CONVERTED TO FORTRAN 90

! THIS FUNCTION OPENS A FILE AS BINARY

! RETURN:        = 1 SUCCESS
!                = 0 ERROR
!
!***********************************************************************

        integer,      dimension(:), allocatable :: wfo_pick
        character*50, dimension(:), allocatable :: wfo_attributes
        integer      :: iopt,itype,iymin,iymax,jxmin,jxmax,imax,imin,
     *                  jmin,jmax,ib,it,no,ndam,ni,id,nl,mhtot,kt,
     *                  irads,iradn,jradw,jrade,iyoffset,jxoffset,
     *                  iyshift,jxshift,istep,nblock,
     *                  ireport,ioflg,ichsm,nnprint
        integer         :: rgrd,rads,radw,rade,radn,rgrdn,rgrde
        integer         :: mm,yy,iall,nclt,nch
        integer(2)      :: year,month,day,hrs,mins,secs,hsecs,hh,dd
        integer, parameter :: flen=9000
        real            :: ver,grdn,grde,al,astep,step2,scalesnw,
     *                     scaletem,sstep
        character(1)    :: snwflg,sedflg,vapflg,smrflg,resinflg,
     *                     resumflg,tbcflg,contflg,routeflg,crseflg,
     *                     ensimflg,leapflg,llflg,picflg,wetflg
        character(5)    :: source,rdr
        character(80)   :: querystring
        
        LOGICAL         :: BINARY


!      integer :: jan,ii,n,i,j,i3,ii1,ii2
!      real ::    aintvl


!      llflg  - when 'y', coordinates for .str .snw etc in lat-long
!      ioflg  - if .ge. 1 read the outfiles (note: integer)
!       leapflg  - to indicate a leap year - not an input

!       snwflg - whether there is snow to melt
!       sedflg - whether the sediment routine is used y or n
!       vapflg - turn on evap routine (Todd)
!       smrflg - turns on smearing (smear precip data over data dt)
!       resinflg - will use resin record for comparison
!       tbcflg - read resume.txt file for run init values
!       resumflg - resume.txt file written at end of run (mem dump)
!     contflg  - for continuing statistics upon resume = input
!     routeflg - output qr grids for routing program
!     crseflg  - read snow course data to replace resume file data
!     ensimflg - write the wfo file for ENSIM
!     ensimflg1 = 'y' for first time needed, else = 'n' (inpevt)

!       source - what is the data source - radar, mc2, erf, rag, etc.

        character(30), dimension(:), allocatable :: fln,
     *                                              filename
        character(30), dimension(:), allocatable :: outfln

!     PARAMETER TYPE DEFINITIONS
!        CHARACTER(*) :: fln
!        INTEGER      :: lun

      wfo_open_file = 0

!     OPEN THE FILE
      if (BINARY) then !for binary output
#ifdef IFORT
        open(unit=57,file='watflow.wfo',status='unknown',
     +       form='unformatted',access='direct',
     +       recl=1, !ifort
     +       iostat=ios)
#endif
#ifdef G95
        open(unit=57,file='watflow.wfo',status='unknown',
     +       form='unformatted',access='direct',
     +       recl=4, !g95
     +       iostat=ios)
#endif
      else !for ascii output
        open(unit=57,file='watflow.wfo',status='unknown',
     +       iostat=ios)
      endif
	if(ios.ne.0)then
	  print*,' Problems opening unit 57'
	  print*,' Problem ignored. Look for file name watflow.wfo'
	  print*
	else
        print*,' Opened the wfo file for ENSIM'
        wfo_open_file = 1
	endif

      RETURN

      END FUNCTION wfo_open_file 


!***********************************************************************
      INTEGER FUNCTION wfo_write_header(appname, xorigin, yorigin,
     *	               xcount, ycount, delta, coordsys, header_length)
!***********************************************************************
!    REV. 9.00   Mar.  2000  -  TS: CONVERTED TO FORTRAN 90

! THIS FUNCTION WRITES A PROPER WFO FILE HEADER

! RETURN:       = 1 SUCCESS
!               = 0 ERROR
!
!***********************************************************************

!        USE DFLIB

!     PARAMETER TYPE DEFINITIONS
        CHARACTER(*) :: appname,coordsys
        INTEGER      :: xcount,ycount,header_length
        REAL         :: xorigin,yorigin,delta

!     LOCAL VARIABLES
        CHARACTER(128) :: line
        CHARACTER(64)  :: username
        CHARACTER(10)  :: ctime
        CHARACTER(8)   :: fmt_string,cday
        CHARACTER(5)   :: czone
        CHARACTER(4)   :: data_type
        INTEGER        :: yy,mm,dd,hh,mi,ss,ms,ivalues(8)

      print*,' Writing the wfo header for ENSIM'
      wfo_write_header = 1

      fmt_string = 'BINARY'
      data_type = 'wfo'

!     WRITE DATA
      call wfo_write_header_record(
     *'###########################################################',
     *                             header_length)

      write(line,101) data_type, fmt_string
  101 format(':FileType ',(A),' ',(A),' EnSim 1.0')

      call wfo_write_header_record(line,header_length)
      call wfo_write_header_record('#',header_length)
      call wfo_write_header_record(
     *	'# DataType       Binary Watflow Output',header_length)
      call wfo_write_header_record('#',header_length)

      write(line,102) appname
  102 format(':Application       ', (A))

      call wfo_write_header_record(line,header_length)

!      call getlog(username)
!      write(line,103) username
! 103 format(':WrittenBy         ', (A))
!      call wfo_write_header_record(line)

!     GETDAT AND GETTIM REPLACED BY DATE_AND_TIME INTRINSIC
!      call getdat(yy,mm,dd)
!      call gettim(hh,mi,ss,ms)
      call date_and_time(cday,ctime,czone,ivalues)
      yy=ivalues(1)
      mm=ivalues(2)
      dd=ivalues(3)
      hh=ivalues(5)
      mi=ivalues(6)
      ss=ivalues(7)
      ms=ivalues(8)

      write(line,104) yy,mm,dd,hh,mi,ss
  104 format(':CreationDate       ',I4.4,"/",I2.2,"/",I2.2,2x,
     *  I2.2,":",I2.2,":",I2.2)

      call wfo_write_header_record(line,header_length)
      call wfo_write_header_record(          
     *'#-----------------------------------------------------------',
     *                             header_length)
      call wfo_write_header_record('#',header_length)

      write(line,105) coordsys
  105 format(':coordsys ', (A))
      call wfo_write_header_record(line,header_length)

      write(line,106) xorigin
  106 format(':xorigin ',(f14.3))
      call wfo_write_header_record(line,header_length)

      write(line,107) yorigin
  107 format(':yorigin ',(f14.3))
      call wfo_write_header_record(line,header_length)

      write(line,108) xcount
  108 format(':xcount ',(i12))
      call wfo_write_header_record(line,header_length)

      write(line,109) ycount
  109 format(':ycount ',(i12))
      call wfo_write_header_record(line,header_length)

      write(line,110) delta
  110 format(':delta ',(f14.7))
      call wfo_write_header_record(line,header_length)

      RETURN

      END FUNCTION wfo_write_header


!***********************************************************************
      SUBROUTINE wfo_write_header_record(line, header_length)
!***********************************************************************
!    REV. 9.00   Mar.  2000  -  TS: CONVERTED TO FORTRAN 90

! THIS SUBROUTINE WRITES A HEADER RECORD TO THE OPEN LUN
! APPEND A NEWLINE CHAR SINCE THE FILE IS BINARY
!
!***********************************************************************

		
!     PARAMETER TYPE DEFINITIONS
        CHARACTER(*) :: line
        INTEGER      :: header_length

!       can't use (*) cause we are adding something to the end
!        INTEGER :: lun

!     LOCAL VARIABLES
        CHARACTER(256) :: lline
        CHARACTER(10) :: fmt
        INTEGER :: llen

!      llen = LEN_TRIM(line)
!      lline = line(1:llen)
!! comment the next line out for ascii output
!      !lline = line(1:llen)// ' '		          ! Append a space
!
!      llen = LEN_TRIM(lline)
!      if(llen .GT. 255)then
!         llen = 255
!      end if
!
!!comment out these lines for ascii output
!      llen = llen+1
!      lline(llen:llen) = CHAR(10)               ! Append a line feed

      llen = LEN_TRIM(line)
      if (llen > 255) then
        llen = 255
      end if

      lline = line(1:llen)
      lline(llen+1:llen+1) = CHAR(10)

! for binary output
      write(fmt, '(a, i3, a)') '(a', llen+1, ')'
      write(57,fmt,advance='no') lline

      header_length = header_length + llen+1
      
!	if(ios.ne.0)then
!	  print*,' Error writing to unit 57'
!	  print*,' ios = ', ios
!	  print*
!	  stop 'in wfocode'
!	endifZ


      RETURN

      END SUBROUTINE wfo_write_header_record


!***********************************************************************
      INTEGER FUNCTION wfo_write_attribute_header(attcount,
     *                 attname,attunits,header_length)
!***********************************************************************
!    REV. 9.00   Mar.  2000  -  TS: CONVERTED TO FORTRAN 90

! THIS FUNCTION WRITE THE ATTRIBUTE NAMES AND UNITS INTO THE HEADER  

! RETURN:       = 1 SUCCESS
!               = 0 ERROR
!
!***********************************************************************

!        USE DFLIB

!     PARAMETER TYPE DEFINITIONS
        INTEGER       :: attcount,header_length
        CHARACTER(64) :: attname(attcount)
        CHARACTER(32) :: attunits(attcount)


!     LOCAL VARIABLES
        CHARACTER(128) :: line
        INTEGER        :: attnum

      wfo_write_attribute_header = 1
      call wfo_write_header_record('#',header_length)

      write(line,121) attcount
  121 format(':AttributeCount ',(i12))
      call wfo_write_header_record(line,header_length)

      do attnum=1, attcount
         write(line,122) attnum, attname(attnum)
         call wfo_write_header_record(line,header_length)
         write(line,123) attnum, attunits(attnum)
         call wfo_write_header_record(line,header_length)
      end do
  122 format(':AttributeName ',(i12),' ',(A))
  123 format(':AttributeUnits ',(i12),' ',(A))

      call wfo_write_header_record('#',header_length)

      RETURN

      END FUNCTION wfo_write_attribute_header

!***********************************************************************
      INTEGER FUNCTION wfo_write_timestamp(seq,step,yy,mm,dd,
     *                 hh,mi,ss,ms,currec)
!***********************************************************************

! THIS FUNCTION WRITES A BINARY TIMESTAMP FOR THIS STEP
! YR, MONTH, DAY, HR, MiN, SEC, MiLLISEC FOR THIS STEP
 
! RETURN:       = 1 SUCCESS
!               = 0 ERROR
!
!***********************************************************************


!     PARAMETER TYPE DEFINITIONS
        INTEGER :: seq,step
        INTEGER :: yy,mm,dd,hh,mi,ss,ms
        INTEGER :: currec

      wfo_write_timestamp = 1

!     WRITE THE RECORD

      write(57, rec=currec, iostat=ios) seq
      currec = currec + 1
      write(57, rec=currec, iostat=ios) step
      currec = currec + 1
      write(57, rec=currec, iostat=ios) yy
      currec = currec + 1
      write(57, rec=currec, iostat=ios) mm
      currec = currec + 1
      write(57, rec=currec, iostat=ios) dd
      currec = currec + 1
      write(57, rec=currec, iostat=ios) hh
      currec = currec + 1
      write(57, rec=currec, iostat=ios) mi
      currec = currec + 1
      write(57, rec=currec, iostat=ios) ss
      currec = currec + 1
      write(57, rec=currec, iostat=ios) ms
      currec = currec + 1
	if(ios.ne.0)then
	  print*,' Error writing to unit 57'
	  print*,' Disk full maybe?'
	  print*,' ios=', ios
	  print*
	  stop 'in wfocode'
	endif


      RETURN

      END FUNCTION wfo_write_timestamp



!***********************************************************************
      INTEGER FUNCTION wfo_close_header(header_length)
!***********************************************************************
!    REV. 9.00   Mar.  2000  -  TS: CONVERTED TO FORTRAN 90

! THIS FUNCTION CLOSES THE HEADER

! RETURN:       = 1
!
!***********************************************************************


!     PARAMETER TYPE DEFINITIONS
!        INTEGER :: lun
      INTEGER :: header_length
      
      INTEGER :: nspace
      CHARACTER(20) :: endheader

      wfo_close_header=1

      call wfo_write_header_record('#', header_length)
!     Pad the header to be exactly a multiple of four bytes
      endheader = ':EndHeader'
      nspace = 4 - IAND(header_length+11, 3)
      if (nspace == 4) then !4
        nspace = 0
      else if (MOD(nspace, 2) == 1) then
        if (nspace > 2) then !3
          call wfo_write_header_record('##', header_length)
        else !1
          call wfo_write_header_record('####', header_length)
        end if
      else !2
        call wfo_write_header_record('#', header_length)
      end if

      call wfo_write_header_record(':EndHeader', header_length)

      if (IAND(header_length, 3) /= 0) then
        WRITE (*,*)
     +     "The header of the wfo file is not an even multiple of four."
      end if

      RETURN

      END FUNCTION wfo_close_header


!***********************************************************************
      INTEGER FUNCTION wfo_write_attribute_data(ny,nx,arr,currec)
!***********************************************************************
!    REV. 9.00   Mar.  2000  -  TS: CONVERTED TO FORTRAN 90

! THIS FUNCTION WRITES A BINARY WFO DATA RECORD
! WRITES GRID DATA FROM LEFT TO RIGHT STARTING FROM BOTTOM ROW
 
! RETURN:       = 1 SUCCESS
!               = 0 ERROR
!
!***********************************************************************


!     PARAMETER TYPE DEFINITIONS
        INTEGER :: nx,ny,currec
        REAL*4  :: arr(nx,ny)

      wfo_write_attribute_data = 1

!     WRITE THE RECORD
      do j=1,ny
        do i=1,nx
           write(57,rec=currec) arr(i,j)
           currec = currec + 1
        enddo
      enddo

!	write(57, iostat=ios) ((arr(I,J),I=1,nx),J=1,ny)
!	if(ios.ne.0)then
!	  print*,' Error writing to unit 57'
!	  print*
!	  
!	  write(51,5101)
!        write(98,5101)
!5101  format(' Error writing to unit 57 at approx line=458 in wfcode'/
!     * ' File closed and progarm continues without writing the rest'/
!     * ' of the watflow.wfo file')
!	endif

      RETURN

      END FUNCTION wfo_write_attribute_data

