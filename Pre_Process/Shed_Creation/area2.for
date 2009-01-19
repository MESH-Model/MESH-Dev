      MODULE area2

!     general program variables

!       flag for calling program
        character*10    calling_program_flg

        integer*1,      dimension(:), allocatable :: wfo_pick
        character*50,   dimension(:), allocatable :: wfo_attributes
        integer      :: iopt,itype,iymin,iymax,jxmin,jxmax,imax,imin,
     *                  jmin,jmax,ib,it,no,ndam,ni,id,nl,mhtot,mhrd,kt,
     *                  irads,iradn,jradw,jrade,iyoffset,jxoffset,
     *                  iyshift,jxshift,istep,nblock,
     *                  ireport,ireport_start,ireport_end,
     *                  ioflg,ichsm,nnprint,iiprint,iopt_start,
     *                  ktri,glacier_class_number
        integer*4    :: rgrd,rads,radw,rade,radn

        real*4       :: rgrdn,rgrde     !changed to real Jul. 27/04 nk
        real*4       :: evt_version

        integer*4    :: mm,yy,iall,nclt,nch,irdt,ii_water
        integer(2)   :: year,month,day,hrs,mins,secs,hsecs,hh,dd
        integer*4, parameter :: flen=9000
        real*4       :: ver,grdn,grde,al,astep,step2,scalesnw,scaleall,
     *                     scaletem,scalealltem,sstep,rdt,flowunitconv
	  logical(1)   :: keyflg,precflg,found_data_end
        CHARACTER(5) :: title(200)
        character(40) :: notes(100)
        CHARACTER(80) :: heading(10)
        character(1) :: snwflg,sedflg,vapflg,smrflg,resinflg,
     *                  resumflg,tbcflg,contflg,routeflg,crseflg,
     *                  ensimflg,leapflg,llflg,picflg,wetflg,
     *                  modelflg,shdflg,wfo_open_flg,trcflg,frcflg,
     *                  newevtflg,manningflg,translateflg,flowfillflg,
     *                  outfileflg,initflg
        character(1) :: ssmc_firstpass
!        character(1), dimension(:), allocatable :: glacier_flag
!       these things taken out of some argument lists  nk 05/10/04
        integer      :: year1,mo1,day1,hour1
	  integer      :: year_now,month_now,day_now,hour_now
	  character(2) :: yy2,mm2,dd2,hh2
        character(4) :: yyyy4

        character(5) :: source,rdr,data_source
        character(80):: querystring
!       	  character(14)   :: date
!     rev. 9.1.55  Jun.  12/04  - NK: write new str files to strfw\newfmt folder.
        character(10)   :: coordsys1,datum1,zone1
	  character(40)   :: attribute_name,attribute_units,attribute_type,
     *                      application,author,char_block,name
        character(30)   :: source_file_name
	  integer         :: attribute_count
        real*4          :: init_heat_deficit,unit_conversion
!       this set of variables needed when reading files other than the 
!       shed file so coincidence of data can be checked
!       fix fix   do this in all rd**** files someday

!       for the gridded precip file in rdrain
        character(10)   :: coordsys2,datum2,zone2
        real            :: xorigin2,yorigin2,xdelta2,ydelta2,
     *                     dtrain,convrain
        integer         :: xcount2,ycount2,deltat2,nhrain

!       for the gridded temperature file in rdtemp
        character(10)   :: coordsys3,datum3,zone3
        real            :: xorigin3,yorigin3,xdelta3,ydelta3,
     *                     dttemp,convtemp
        integer         :: xcount3,ycount3,deltat3,nhtemp

!       for the generic read & write modules (e.g write_r2c
        character(10)   :: coordsys_temp,datum_temp,zone_temp
        real            :: xorigin_temp,yorigin_temp,
     *                     xdelta_temp,ydelta_temp
        integer         :: xcount_temp,ycount_temp,deltat_temp,
     *	               deltat_report

!       please note that coordsys and datum are used elsewhere.

!      integer :: jan,ii,n,i,j,i3,ii1,ii2
!      real ::    aintvl


      END MODULE area2

!      llflg  - when 'y', coordinates for .str .snw etc in lat-long
!      ioflg  - if .ge. 1 read the outfiles (note: integer)
!       leapflg  - to indicate a leap year - not an input

!	snwflg - whether there is snow to melt
!	sedflg - whether the sediment routine is used y or n
!	vapflg - turn on evap routine (Todd)
!	smrflg - turns on smearing (smear precip data over data dt)
!	resinflg - will use resin record for comparison
!	tbcflg - read resume.txt file for run init values
!	resumflg - resume.txt file written at end of run (mem dump)
!     contflg  - for continuing statistics upon resume = input
!     routeflg - output qr grids for routing program 
!     crseflg  - read snow course data to replace resume file data
!     ensimflg - write the wfo file for ENSIM
!     ensimflg1 = 'y' for first time needed, else = 'n' (inpevt)
!     wfoflg - set to 'y' initially. Changed to 'n' once wfo header=written
!     picflg   - write the simout/pic.txt file for mapper
!     wetid1flg- run the wetland routing module - read in first event only
!     modelflg  - pick model
!     shedflg  - replace the watershed file basin\bsnm.shd

!	source - what is the data source - radar, mc2, erf, rag, etc.




