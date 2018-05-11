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

      subroutine write_r2c(fls, indx, shd,
     *                     no_frames, no_classes, frame_no, class_no,
     *                     no_signf,
     *                     outarray,
     *                     attr_name, attr_units, attr_type,
     *                     attr_src, attr_author)

!***********************************************************************
!       copyright (c) by Nick Kouwen 1987-2007
!***********************************************************************

!     rev. 9.1.64  Oct.  03/04  - NK: Coded up new header in ragmet.for

!***********************************************************************
! - THIS SUBROUTINE OUTPUTS and r2c file.

! - List of arguments:

!       fls             type        File information.
!       indx            int         Index of the file in fls%fl().
!       shd             type        Basin/watershed information.
!       ic              type        Counter.
!   I - itogo   int        no. of hours until next rainfall
!   R - unit_conversion    REAL*4     conversion factor (area2)
!   I - FLN     CHAR*12    file names
!   I - DIR     CHAR*12    directory name
!   I - ocflg   int        open_close flag 1 open file -1 close file
!   I - frmflg  int        frame flag  1 new frame -1 end frame
!                          0 each call = 1 frame with frame marks
!       outarray        real(y, x)  Array of data to write to file.
!       attr_name       char(*)     Name of the data/variable.
!       attr_units      char(*)     Units of the data.
!       attr_type       char(*)     Not used. Type of attribute
!                                   (e.g., 'flow').
!       attr_src        char(*)     Source of data.
!       attr_author     char(*)     Author of the file.
!***********************************************************************

      use sa_mesh_common
      use model_dates
      use model_files_variables

      implicit none

      integer iun, ii, no_signf, hour_no, hours_togo,
     *  no_frames, no_classes, frame_no, class_no,
     *  i, j, ierr
      character(20) junk
      character(10) time
      character(8) cday

      !> Input variables.
      type(fl_ids), intent(in) :: fls
      integer, intent(in) :: indx
      type(GridParams), intent(in) :: shd
      real, dimension(shd%yCount, shd%xCount), intent(in) :: outarray
      character(*), intent(in), optional :: attr_name,
     *  attr_units, attr_type, attr_src, attr_author

!     FIRST TIME THROUGH THIS SUBROUTINE ONLY
!     OPEN OUTPUT FILE AND WRITE HEADER

      hour_no = ic%now%hour + 1

!     user notes

!     To write the header, set frame_no = 0
!     To write the data, set the frame > 1

!     this s/r will only allow:
!     multiple frames for 1 class
!                or
!     1 frame for multiple classes
      if (no_frames > 1 .and. no_classes > 1) then
        call print_error('Programming error')
        call print_message('no_frames > 1 and no_classes > 1')
        call print_message('This is not allowed')
        call print_message('')
        call program_abort()
!       This can only be cause by misuse of this s/r
!         in the calling program
      end if

!>    Set the file unit.
      iun = fls%fl(indx)%iun

!      if(frame_no.eq.1.and.class_no.eq.1)then

      if (frame_no == 0) then

!       write the header ONLY

!       FILE NAMES AND UNIT NUMBERS DIFFER BY 30
!        write(*,1400)fn,fln(fn)
! 1400   format(' opening fln(',i3,'):',a30,'---')
!        write(*,*)

        open(iun, file=adjustl(trim(fls%fl(indx)%fn)),
     *    status='unknown', action='write', iostat=ierr)
!     print*,' un fn et fln(fn) ',un,fn,fln(fn)
        if (DIAGNOSEMODE) then
          call print_message('Opening: ' // trim(fls%fl(indx)%fn))
        end if
        if (ierr /= 0) then
          call print_error(
     *      'Unable to open file: ' // trim(trim(fls%fl(indx)%fn)))
          call program_abort()
        end if
        write(iun, 3005) '########################################'
        write(iun, 3005) ':FileType r2c  ASCII  EnSim 1.0         '
        write(iun, 3005) '#                                       '
        write(iun, 3005) '# DataType               2D Rect Cell   '
        write(iun, 3005) '#                                       '
        write(iun, 3005) ':Application             EnSimHydrologic'
        write(iun, 3005) ':Version                 2.1.23         '
        write(iun, 3002) ':WrittenBy          ', attr_author
        call date_and_time(cday, time)
        write(iun, 3010) ':CreationDate       ',
     *    cday(1:4), cday(5:6), cday(7:8), time(1:2), time(3:4)
3010  format(a20, a4, '-', a2, '-', a2, 2x, a2, ':', a2)
        write(iun, 3005) '#                                       '
        write(iun, 3005) '#---------------------------------------'
        write(iun, 3005) '#                                       '
        write(iun, 3002) ':Name               ', attr_author
        write(iun, 3005) '#                                       '
        write(iun, 3004) ':Projection         ', shd%CoordSys%Proj
        if (shd%CoordSys%Proj == 'LATLONG   ') then
          write(iun, 3004) ':Ellipsoid          ', shd%CoordSys%Ellips
        end if
        if (shd%CoordSys%Proj == 'UTM       ') then
          write(iun, 3004) ':Ellipsoid          ', shd%CoordSys%Ellips
          write(iun, 3004) ':Zone               ', shd%CoordSys%Zone
        end if
        write(iun, 3005) '#                                       '
        write(iun, 3003) ':xOrigin            ', shd%xOrigin
        write(iun, 3003) ':yOrigin            ', shd%yOrigin
        write(iun, 3005) '#                                       '
        write(iun, 3002) ':SourceFile         ', attr_src
        write(iun, 3005) '#                                       '
        if (no_frames == 1 .and. no_classes >= 1) then
          do i = 1, no_classes
            write(iun, 3007) ':AttributeName', i, ' Class', i
          end do
          write(iun, 3005) '#                                       '
        else
          write(iun, 3002) ':AttributeName 1    ', attr_name
          write(iun, 3002) ':AttributeUnits     ', attr_units
c!         see note below @***
        end if
        write(iun, 3005) '#                                       '
        write(iun, 3001) ':xCount             ', shd%xCount
        write(iun, 3001) ':yCount             ', shd%yCount
        write(iun, 3003) ':xDelta             ', shd%xDelta
        write(iun, 3003) ':yDelta             ', shd%yDelta
c        if(no_frames.eq.1)then
!        if (no_frames <= 1) then
!          write(un, 3005) '#                                       '
!          write(un, 3004) ':SampleTime         ', startdate, starttime
!          write(un,3004)':StartDate          '
!        end if
        write(iun, 3005) '#                                       '
!        if (unit_conversion /= 0.0) then
!          write(un, 3003) ':UnitConverson      ', unit_conversion
!        end if
!        if (name == 'Snow Water Equivalent                   ') then
!          write(un, 3003) ':InitHeatDeficit    ', init_heat_deficit
!        end if
        write(iun, 3005) '#                                       '
        write(iun, 3005) ':endHeader                              '

        return

      end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! fix fix

!       this is just to read in the new format
!       still needs to be programmed for multiple classes

!     the header is written only the first time through
!     this part is called with each class

!     :Frame and :EndFrame lines are written only for time series

!      if (data_source == '     ') data_source = source  ! source from area2

!      if(no_frames.gt.1)write(un,3011)':Frame',frame_no,frame_no,
!     *     year1,mo1,day1,hour1,'Hour=',hour_no,hours_togo,data_source

      if (frame_no > 0) then

c        if(hour_now.eq.0)then
c          day_now=day_now-1
c     hour_now=24
c   endif

!       if(mo1.le.9.and.day_now.le.9)then
! Craig Thompson changed mo1 to month_now so that the
! output is correctly formatted
        if (ic%now%month <= 9 .and. ic%now%day <= 9) then
          if (no_frames > 1) write(iun, 3021) ':Frame',
     *      abs(frame_no), abs(frame_no), ic%now%year, ic%now%month,
     *      ic%now%day, hour_no

        else if (ic%now%month <= 9 .and. ic%now%day > 9) then
! Craig Thompson changed mo1 to month_now so that the
! output is correctly formatted
          if (no_frames > 1) write(iun, 3022) ':Frame',
     *      abs(frame_no), abs(frame_no), ic%now%year, ic%now%month,
     *      ic%now%day, hour_no

        else if (ic%now%month > 9 .and. ic%now%day <= 9) then
! Craig Thompson changed mo1 to month_now so that the
! output is correctly formatted
          if (no_frames > 1) write(iun, 3023) ':Frame',
     *      abs(frame_no), abs(frame_no), ic%now%year, ic%now%month,
     *      ic%now%day, hour_no

        else
          if (no_frames > 1) write(iun, 3024) ':Frame',
     *      abs(frame_no), abs(frame_no), ic%now%year, ic%now%month,
     *      ic%now%day, hour_no

        end if

!       NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE
!       The r2c grids are written upside down:
!                 south on top, north on bottom    !!!!!!!!!!
!   write(*,*) outarray(10,10)
        if (no_signf == 0) then
          do i = 1, shd%yCount
            write(iun, 1300) (outarray(i, j), j = 1, shd%xCount)
          end do
        else if (no_signf == 1) then   ! swe, precip and temp
          do i = 1, shd%yCount
            write(iun, 1301) (outarray(i, j), j = 1, shd%xCount)
          end do
        else if (no_signf == 2) then   ! swe, precip and temp
          do i = 1, shd%yCount
            write(iun, 1302) (outarray(i, j), j = 1, shd%xCount)
          end do
        else if (no_signf == 3) then   ! swe, precip and temp
          do i = 1, shd%yCount
            write(iun, 1303) (outarray(i, j), j = 1, shd%xCount)
          end do
        else if (no_signf == 4) then   ! swe, precip and temp
          do i = 1, shd%yCount
            write(iun, 1304) (outarray(i, j), j = 1, shd%xCount)
          end do
        else if (no_signf == 5) then   ! swe, precip and temp
          do i = 1, shd%yCount
            write(iun, 1305) (outarray(i, j), j = 1, shd%xCount)
          end do
        else if (no_signf == 6) then   ! swe, precip and temp
          do i = 1, shd%yCount
            write(iun, 1306) (outarray(i, j), j = 1, shd%xCount)
          end do
        else if (no_signf == 8) then   ! flow
          do i = 1, shd%yCount
            write(iun, 1308) (outarray(i, j), j = 1, shd%xCount)
          end do
        else                        ! init soil moisture
          do i = 1, shd%yCount
            write(iun, 1307) (outarray(i, j), j = 1, shd%xCount)
          end do
        end if

        if(class_no == no_classes) write(iun, 3012) ':EndFrame'

      end if

!      if (frame_no == no_frames .and. class_no == no_classes) then
!        close(iun)
!      end if

      return

! FORMATS
1300  format(9999(1x, f5.0))
1301  format(9999(1x, f5.1))
1302  format(9999(1x, f5.2))
1303  format(9999(1x, f6.3))
1304  format(9999(1x, f7.4))
1305  format(9999(1x, f8.5))
1306  format(9999(1x, f9.6))
1307  format(9999(1x, e12.6))
1308  format(9999(1x, e10.3))

3000  format(a10, i5)
3001  format(a20, i16)
3002  format(2a20)
3003  format(a20, f16.7)
3004  format(a20, a10, 2x, a10)
3005  format(a40)
3006  format(a3,a10)
3007  format(a14, i5, a6, i5)
3012  format(a9)

3021  format(a6, 2i10, 3x, '"', i4, '/', i1, '/', i1, 1x, i2,
     *  ':00:00.000"', 2x, a5, 2i5, 2x, a5)
3022  format(a6, 2i10, 3x, '"', i4, '/', i1, '/', i2, 1x, i2,
     *  ':00:00.000"', 2x, a5, 2i5, 2x, a5)
3023  format(a6, 2i10, 3x, '"', i4, '/', i2, '/', i1, 1x, i2,
     *  ':00:00.000"', 2x, a5, 2i5, 2x, a5)
3024  format(a6, 2i10, 3x, '"', i4, '/', i2, '/', i2, 1x, i2,
     *  ':00:00.000"', 2x, a5, 2i5, 2x, a5)

      end subroutine
