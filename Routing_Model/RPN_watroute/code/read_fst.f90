! csubich -- procedure to read variables from an RPN-Standard-format file
!            (fst), with interface somewhat compatible with read_r2c

! On call:
!     With header_only equal to '1', searches the given flname
!        for a record matching var_name/iyear/imonth/
!        iday/ihour, and records the appropriate grid sizes into the global
!        variables xcount_temp and ycount_temp
!     With header_only equal to any other character, this searches the given
!        file for the -first- record matching the supplied variables and
!        reads the data into the global 'inarray' variable, (re)allocating
!        it as necessary to fit.
!
!  For now, this procedure makes the implicit assumption that any array of
!  matching computational dimensions (ni/nj/nk) must be an appropriate array
!  for the underlying watershed grid.  This is, of course, not necessarily
!  true, and the predecessor read_r2c procedure made an explicit check that
!  the grid origin and dx/dy matched what was expected.
!
!  In the near future, this lack-of-check will be replaced with a more
!  appropriate interpolation onto the underlying .shd grid.

module read_fst

contains

subroutine read_fst(unitNum,iflname,header_only,var_name,iyear,imonth,iday,ihour)
   ! Use the global module for access to a few key global variables, most
   ! notably the allocatable 'inarray'

   use area_watflood 

   ! Define variables

   implicit none
   integer :: unitNum, & ! Unit number to use for the .fst file
              datedv,  & ! Datev parameter for rmnlib -- date of validity
!              ip1,     & ! vertical level for rmnlib
!              ip2,     & ! Forecast ihour for rmnlib
              key,     & ! Record key inside .fst file
              nnii,    & ! size of dimension 1
              nnjj,    & !                   2
              nnkk,    & !                   3
              ier,     & ! Error code
              fulldate,& ! Compiled date
              yyyymmdd,& ! Calculated human-readable date
              hhmmsshh,& ! Calculated human-readable time
              iyear,imonth,iday,ihour ! Date/time of variable to read
   character iflname*(*)! .fst flname (input)
   character flname*(90) ! Real file-name 
   character*12 etiket   ! long-form label for variable name
   character*4  var_name ! variable name in the file, also 'nomvar'
   character*1  header_only ! Read only the 'header' info (dimension size)

   ! Debugging parameters: record selection data
   integer o_dateo, & ! date time stamp
           o_deet,  & ! Time step length in seconds
           o_npas,  & ! Time steup number
           o_ni, o_nj, o_nk, &
           o_nbits, & ! Number of bits kept
           o_datyp, & ! Data type
           o_ip1, o_ip2, o_ip3, &
           o_ig1, o_ig2, o_ig3, o_ig4, &
           o_swa,   & ! Starting word address
           o_lng,   & ! Record length
           o_dltf,  & ! Delete flag
           o_ubc,   & ! Unused bit count
           o_extra1, o_extra2, o_extra3

   character*12 o_etiket    ! Run label
   character*5  o_nomvar    ! Variable name
   character*3  o_typvar    ! Data type
   character*2  o_grtyp     ! Grid type

   ! External functions from rmnlib
   integer, external :: &  
            fnom,      & ! Associate filename with unit number
            fstouv,    & ! Open the .fst file
            fstinf,    & ! Search for a record
            fstluk,    & ! Read record by index from above
            fstfrm,    & ! Close the fst file
            fstvoi,    & ! Debugging: output fst listing
            fclos,     & ! Disassociate the filename and unit number
            fstprm,    & ! Parameters of searched-for record
            newdate,   & ! Date manipulation functions
            fstopc       ! Global option settings

   ! Suppress non-error printouts
   !ier = fstopc('MSGLVL','ERRORS',0)

   flname = ' '
   flname = trim(adjustl(iflname))

   ! This code doesn't end up using the label (etiket) field, so set it to
   ! its wildcard value

   etiket = ' '

   !flname = trim(adjustl(flname))
   
   ! Finding the data end is problematic, since we don't have a guarantee that the
   ! .fst file is written in sequential order.  Instead, we'll have to adjust the
   ! logic of the caller function --- instead of "found_data_end" meaning "this here
   ! is the last record,"  it needs to mean "we've already found the last record and
   ! there is nothing more to see."
   !if (ihour >= 24) found_data_end=.true.
   if (ihour < -1) STOP 'Invalid ihour'

   ! Compile the full yyyymmdd date, -1 if mm/dd are unspecified
   if (iyear > 0 .and. imonth > 0 .and. iday > 0 .and. ihour >= 0) then
      ! Use the function 'newdate' to compute the internal-form datev
      ! for the specified yyyymmdd.

      ! The current internal date/time structure of watroute is a bit of
      ! a mess.  Most notably for this date calculation, we're getting
      ! ihours that range between 1 and 24 -- and that's represented in 
      ! the IP2 field of the current-format .fst files.  Unfortunately, 
      ! ihour-24 doesn't exist, instead being ihour-0 of the next iday, and
      ! newdate below is picky about what it accepts (returning 0 for an
      ! invalid date).

      ! So, we have to do some sanity checking on the ihour; if that also
      ! fails (we're wrapping around a imonth/iyear), we'll still get a
      ! returned date of 0, so we'll progressively try further-wraps

      if (ihour < 24) then
         yyyymmdd = 10000*iyear + 100*imonth + iday
         hhmmsshh = ihour*1000000
         ier = newdate(fulldate, yyyymmdd, hhmmsshh, 3)
      else
         yyyymmdd = 10000*iyear + 100*imonth + (iday+1)
         hhmmsshh = 0
         ier = newdate(fulldate, yyyymmdd, hhmmsshh, 3)
         if (ier > 0) then ! Try rolling over the imonth
            yyyymmdd = 10000*iyear + 100*(imonth+1) + 1
            ier = newdate(fulldate,yyyymmdd, hhmmsshh, 3)
         endif
         if (ier > 0) then ! Month didn't work, try iyear
            yyyymmdd = 10000*(iyear+1) + 100*1 + 1
            ier = newdate(fulldate,yyyymmdd,hhmmsshh,3)
         endif
      endif

      if (ier > 0) then
         print *, 'Invalid date in read_fst: year ', iyear, 'month ', imonth, 'day ', iday, 'hour ', ihour
         STOP 'Invalid date received'
      endif
      !print *, 'Calculated date: ', fulldate, ier
      !fulldate = -1
   else
      fulldate = -1
   endif
      

   ! Associate the given filename with the unit number
   ier = fnom(unitNum, & ! Unit number
              flname, & ! Trimmed filename
              'STD+RND',0)
   if (ier .lt. 0) then
      print *, 'Could not open fst file for reading: ', flname
      STOP 'Could not read FST file'
   endif

   ! Now that it's associated, open the file
   ier = fstouv(unitNum,'STD+RND')
   if (ier .lt. 0) then
      print *, 'Could not open fst file for reading: ', flname
      STOP 'Could not read FST file'
   endif

   ! Select the first record that matches the given data
   !print *, 'DEBUG:', fulldate
   key = fstinf(unitNum, nnii, nnjj, nnkk, &
                  fulldate, & ! Construct yyyymmdd date
                  etiket, & ! (unused) label name
                  -1, & ! Select any vertical level
                  -1, & ! Hour is ip2 only unreliably
                  -1, & ! ip3 is unused by this code
                  'P', & ! Variable type
                  var_name) ! Variable name
   if (key .le. 0 ) then
      ! There are two possible reasons for no record being found.
      ! The first is that we're reading the wrong file, so this
      ! variable is simply absent -- that's an unrecoverable
      ! error.  The second reason is that this file does have
      ! the proper variable, but we've run past the end of its
      ! valid data and need to move to the next 'event' file.

      ! If date > 0, that means we're looking for a specific event,
      ! so let's check again for -any- date/ihour combination
      !print *, 'DEBUG: Key not found for ', fulldate, ihour, ' ', var_name
      if (fulldate > 0) then
         key = fstinf(unitNum,nnii,nnjj,nnkk,-1,&
                      etiket,-1,-1,-1,'P',var_name)
         if (key >= 0) then 
            ! We've simply run past the end of the file
            found_data_end = .true.

            ! Close the file
            ier = fstfrm(unitNum)
            ier = fclos(unitNum)
            return
         endif
         !print *, 'DEBUG: Key still not found'
      endif
      ! Now we know this variable is missing from the file, so
      ! exit watroute with an informative error message
      print *, 'Could not find a record corresponding to the variable ', &
               var_name, ' in the file ', flname
      ier = fstvoi(unitNum,'STD+RND')
      STOP 'Could not find record in FST file'
   endif
   ! Further debug: output the field selection data

!  ier = fstprm(key, o_dateo, o_deet, o_npas, &
!               o_ni, o_nj, o_nk, o_nbits, o_datyp, &
!               o_ip1, o_ip2, o_ip3, o_typvar, o_nomvar, &
!               o_etiket, o_grtyp, o_ig1, o_ig2, o_ig3, &
!               o_ig4, o_swa, o_lng, o_dltf, o_ubc, &
!               o_extra1, o_extra2, o_extra3)

!  print *, 'key', key
!  print *, 'dateo', o_dateo
!  print *, 'extra1', o_extra1
!  print *, 'ip1', o_ip1, 'ip2', o_ip2, 'ip3', o_ip3
!  ier = newdate(o_extra1,o_ip1,o_ip2,-3)
!  print *, 'date', o_ip1, 'time', o_ip2
!  print *, 'name', o_nomvar


   ! If this is a 3d field (nnkk > 1), then something's wrong
   if (nnkk .ne. 1) then
      print *, 'ERROR: found ', nnkk, 'z-levels in ', flname ,':',var_name
      STOP 'Data set too thick'
   endif

   if (header_only .eq. '1') then
      xcount_temp = nnii
      ycount_temp = nnjj
   else
      ! Read in the field now

      ! First, check to see if inarray is allocated
      if (.not. allocated(inarray)) then
         print *, 'Allocate inarray'
         allocate(inarray(nnii,nnjj), stat=ier)
         if (ier .ne. 0) STOP 'Allocation error'
      endif

      ! Check to see if inarray has changed size -- this probably
      ! should not happen
      if (size(inarray,1) .ne. nnii .or. &
          size(inarray,2) .ne. nnjj) then
         print *, 'WARNING: inarray is changing size'
         deallocate(inarray,stat=ier)
         if (ier .ne. 0) STOP 'Deallocation error'
         allocate(inarray(nnjj,nnii), stat=ier)
         if (ier .ne. 0) STOP 'Allocation error'
      endif

      ! Load the indexed file
      key = fstluk(inarray,key,nnii,nnjj,nnkk)

   endif

   ! Close the file and return

   ier = fstfrm(unitNum)
   ier = fclos(unitNum)


end subroutine read_fst

end module read_fst
