!>\file
!!Canadian Terrestrial Ecosystem Model (CTEM) 
!!Joboptions Read-In Subroutine 
!!
!! EXAMPLES:
!!
!! To set up a transient run that does a cycling of the climate at the start:
!!
!! In the example below the model will run from 1851 - 2012 using a climate dataset that
!! spans 1901 - 2012. The LUC, POPD, CO2 files all span 1850 - 2012. The climate dataset will
!! cycle over 25 years of climate (NUMMETCYLYRS; from 1901 - 1925) twice (CTEMLOOP) for the 
!! period 1851 - 1900 while the CO2, POPD, and LUC all run from 1851 - 1900 in their respective
!! files (Each file will search for 1851 at the start so skipping 1850). Once 1901 is hit
!! the MET file no longer cycles and simply runs, like the other input files, until
!! it reaches its end (year 2012).
!!
!! (only the relevant switches are shown below) \n
!! transient_run = .true. \n
!! trans_startyr = 1851 \n
!! CTEMLOOP = 2 ,  <-- note this is set to the number of loops that NUMMETCYLYRS must make to match up with the other datasets. \n
!! NCYEAR = 112 , \n
!! LNDUSEON = .TRUE. , \n
!! CYCLEMET = .TRUE. ,  <-- note this is set to TRUE. \n
!! NUMMETCYLYRS = 25 ,  <-- note this times ctemloop should allow the datasets to match up (e.g. 1850 + 2*25 yrs ends in 1900) \n
!! METCYLYRST = 1901 , \n
!! CO2ON = .TRUE. , \n
!! POPDON = .TRUE. , \n
!! OBSWETF = .false. ,
!!
!! If you are doing methane then this would be true like the CO2 switches
!!
!! ------------------------------
!!
!! If you want a transient run that does not spin over climate at the start, you need to change from above the following:
!!
!! (only the relevant switches are shown below) \n
!! trans_startyr = 2000, <-- whatever year you want to start at \n
!! CYCLEMET = .FALSE. ,  <-- note this is set to FALSE.
!!
! +++++++++++++++++++++++++++++++
!!
!! If you want a transient CO2 run that DOES spin over climate, you need to change from above the following:

!! (only the relevant switches are shown below) \n
!! trans_startyr = 2000, <-- whatever year you want the CO2 to start at (note: the run will end at the end of the CO2 file)\n
!! CO2ON = .TRUE.\n
!! SETCO2CONC = 285.00 , <-- Now ignored.\n
!!
!! ------------------------------
!!
!! To set up a spinup run:
!!
!! This example runs the model over 125 years. The climate cycles from 1901 - 1925 five times. No LUC happens,
!! the POPD is set at the 1850 value read in from the POPD file and the CO2 concentration is fixed at SETCO2CONC.
!!
!! (only the relevant switches are shown below) \n
!! transient_run = .false. \n
!! CTEMLOOP = 5 ,  \n
!! NCYEAR = 112 ,  <-- Note this should be just be the length of the climate dataset but could be less if you want. \n
!! LNDUSEON = .false. , \n
!! CYCLEMET = .TRUE. ,  <-- note this is set to TRUE. \n
!! NUMMETCYLYRS = 25 ,  <-- note this is however many years you want. \n
!! METCYLYRST = 1901 , \n
!! CO2ON = .false. , \n
!! SETCO2CONC = 285.00 , <-- set to your own appropriate value. \n
!! POPDON = .TRUE. , <-- note keep this on uses the POPD file, setting false uses the INI file value \n
!! POPCYCLEYR = 1850 , \n
!! OBSWETF = .false. ,
!!
!! If doing methane then the CH4 switches should be the same as the CO2 ones.
!!
subroutine read_from_job_options(argbuff,transient_run,trans_startyr,ctemloop,ctem_on, &
                  ncyear,lnduseon,spinfast,cyclemet,nummetcylyrs,metcylyrst,co2on, &
                  setco2conc,ch4on,setch4conc,popdon,popcycleyr,parallelrun,dofire,dowetlands,obswetf,&
                  compete,inibioclim,start_bare,rsfile,start_from_rs,leap,jmosty,idisp,izref, &
                  islfd,ipcp,itc,itcg,itg,iwf,ipai,ihgt,ialc,ials,ialg,isnoalb,igralb,jhhstd,& 
                  jhhendd,jdstd,jdendd,jhhsty,jhhendy,jdsty,jdendy)

!#ifdef nagf95
!use f90_unix
!#endif

!       History:
!
!     9 Nov 2016     - Add the "leap" switch for leap years (.TRUE. if leap years 
!     J.-S. Landry     in the .MET file have data for 366 days, .FALSE. if not) 
!
!     28  Jul  2016  - Add ability to have changing CO2 but cycling climate
!     J. Melton        this was for the TRENDY project but generally useful so
!                      keep in.
!     3   Feb  2016 - Remove mosaic flag. It is no longer required.
!     J. Melton

!     20  Mar. 2015 - Add in new CLASS flags for snow albedos -igralb & isnoalb
!     J. Melton

!     4   Sep. 2014 - Add in the transient_run flag.
!     J. Melton
!
!     2   Jul. 2013 - Removed ctem1 and ctem2, replaced with ctem_on
!     J. Melton
!
!     25  Jun. 2013 - Added inibioclim switch for compete runs
!     J. Melton
!
!     17  Oct. 2012 - Added the start_bare switch for compete runs
!     J. Melton
!
!     25  Apr. 2012 - This subroutine takes in model switches from
!     J. Melton       a job file and pushes them to RUNCLASSCTEM

implicit none

! -------------
! ctem model switches

character(80), intent(out) :: argbuff !prefix of file names

logical, intent(out) :: transient_run !< true if the run is a transient run. With this flag set
                                 !< set to .true., you can cycle over nummetcyclyrs of climate a
                                 !< ctemloop number of times then continue on through the climate
                                 !< the reason for this is to allow a transient from 1850 on, but 
                                 !< only having the climate from 1901 while having the other inputs 
                                 !< from 1850. See the bottom of this subroutine for an example of how to 
                                 !< set this correctly.  
                                 
integer, intent(out) :: trans_startyr !< the year you want the transient run to start (e.g. 1850). If you
                                      !! are not doing a transient run, set to a negative value (like -9999)

integer, intent(out) :: ctemloop !< no. of times the .met file is to be read. this
                    	         !< option is useful to see how ctem's c pools
                    	         !< equilibrate when driven with same climate data
                    	         !< over and over again.

logical, intent(out) :: ctem_on  !< set this to true for using ctem simulated dynamic
 				 !< lai and canopy mass, else class simulated specified
                 		 !< lai and canopy mass are used. with this switch on,
                 		 !< all ctem subroutines are run.

integer, intent(out) :: ncyear   !< no. of years in the .met file. 

logical, intent(out) :: lnduseon !< set this to 1 if land use change is to be
                 		 !< implimented by reading in the fractions of 9 ctem
                		 !< pfts from a file. keep in mind that once on, luc read-in is
                                 !< also influenced by the cyclemet and popcycleyr switches

integer, intent(out) :: spinfast !< set this to a higher number up to 10 to spin up
                 		 !< soil carbon pool faster

logical, intent(out) :: cyclemet !< to cycle over only a fixed number of years 
                 		 !< (nummetcylyrs) starting at a certain year (metcylyrst)
                 		 !< if cyclemet, then put co2on = false and set an appopriate setco2conc, also
                 		 !< if popdon is true, it will choose the popn and luc data for year
                 		 !< metcylyrst and cycle on that.

integer, intent(out) :: nummetcylyrs !< years of the climate file to spin up on repeatedly
                 		     !< ignored if cyclemet is false

integer, intent(out) :: metcylyrst   !< climate year to start the spin up on
                 		     !< ignored if cyclemet is false

logical, intent(out) :: co2on    !< use co2 time series, set to false if cyclemet is true

real, intent(out) :: setco2conc  !< set the value of atmospheric co2 if co2on is false. (ppmv)

logical, intent(out) :: ch4on    !< use CH4 time series, set to false if cyclemet is true
                                 !< the CO2 timeseries is in the same input file as the CO2 one.

real, intent(out) :: setch4conc  !< set the value of atmospheric CH4 if ch4on is false. (ppmv)

logical, intent(out) :: popdon   !< if set true use population density data to calculate fire extinguishing 
                 		 !< probability and probability of fire due to human causes, 
                 		 !< or if false, read directly from .ctm file

integer, intent(out) :: popcycleyr !< popd and luc year to cycle on when cyclemet is true, set to -9999
                		 !< to cycle on metcylyrst for both popd and luc. if cyclemet is false
                                 !< this defaults to -9999, which will then cause the model to cycle on
                                 !< whatever is the first year in the popd and luc datasets

logical, intent(out) :: parallelrun !< set this to be true if model is run in parallel mode for 
                            	!< multiple grid cells, output is limited to monthly & yearly 
                    		!< grid-mean only. else the run is in stand alone mode, in which 
                     		!< output includes half-hourly and daily and mosaic-mean as well.

logical, intent(out) :: dofire  !< if true the fire/disturbance subroutine will be used.

logical, intent(out) :: dowetlands !< if true the ch4wetland subroutine will be used.

logical, intent(out) :: obswetf !< if true the observed wetland fraction will be used.

logical, intent(out) :: compete !< set this to true if competition between pfts is
                 		!< to be implimented

logical, intent(out) :: inibioclim  !< set this to true if competition between pfts is
                 		    !< to be implimented and you have the mean climate values
                                    !< in the ctm files.

logical, intent(out) :: start_bare !<set this to true if competition is true, and if you wish
                                 !< to start from bare ground. if this is set to false, the 
                                 !< ini and ctm file info will be used to set up the run. 
                                 !< NOTE: This still keeps the crop fractions (while setting all pools to
                                 !< zero)

logical, intent(out) :: rsfile   !< set this to true if restart files (.ini_rs and .ctm_rs)   
                 		 !< are written at the end of each year. these files are  
                 		 !< necessary for checking whether the model reaches 
                 		 !< equilibrium after running for a certain years. 
                 		 !< set this to false if restart files are not needed 
                 		 !< (known how many years the model will run)
logical, intent(out) :: start_from_rs !< if true, this option copies the _RS INI and CTM files
                                 !< to be the .INI and .CTM files and then starts the run as per normal.
                                 !< it is handy when spinning up so you don't have to do a complicated copying of the
                                 !< RS files to restart from them. NOTE! This will not work on hadar or spica, instead
                                 !< you have to manually move the files and set this to .false.
                                 
logical, intent(out) :: leap     !< set to true if all/some leap years in the .MET file have data for 366 days 
                                 !< also accounts for leap years in .MET when cycling over meteorology (cyclemet) 

integer, intent(out) :: jmosty    !< Year to start writing out the monthly output files. If you want to write monthly outputs right 
                                  !< from the start then put in a negative number (like -9999), if you never want to have monthly
                                  !< outputs put a large positive number (like 9999). This is given in the same timescale as IYEAR                                 

! -------------
! class model switches

integer, intent(out) :: idisp    !< if idisp=0, vegetation displacement heights are ignored,
				 !< because the atmospheric model considers these to be part
				 !< of the "terrain".
				 !< if idisp=1, vegetation displacement heights are calculated.

integer, intent(out) :: izref    !< if izref=1, the bottom of the atmospheric model is taken
				 !< to lie at the ground surface.
				 !< if izref=2, the bottom of the atmospheric model is taken
				 !< to lie at the local roughness height.

integer, intent(out) :: islfd    !< if islfd=0, drcoef is called for surface stability corrections
				 !< and the original gcm set of screen-level diagnostic calculations 
				 !< is done.
				 !< if islfd=1, drcoef is called for surface stability corrections
				 !< and sldiag is called for screen-level diagnostic calculations. 
				 !< if islfd=2, flxsurfz is called for surface stability corrections
				 !< and diasurf is called for screen-level diagnostic calculations. 

integer, intent(out) :: ipcp     !< if ipcp=1, the rainfall-snowfall cutoff is taken to lie at 0 c.
				 !< if ipcp=2, a linear partitioning of precipitation betweeen 
				 !< rainfall and snowfall is done between 0 c and 2 c.
				 !< if ipcp=3, rainfall and snowfall are partitioned according to
				 !< a polynomial curve between 0 c and 6 c.

integer, intent(out) :: iwf     !< if iwf=0, only overland flow and baseflow are modelled, and
				!< the ground surface slope is not modelled.
				!< if iwf=n (0<n<4), the watflood calculations of overland flow 
				!< and interflow are performed; interflow is drawn from the top 
				!< n soil layers.

INTEGER, INTENT(OUT) :: ITC!< itc, itcg and itg are switches to choose the iteration scheme to
                           !< be used in calculating the canopy or ground surface temperature
                           !< respectively.  if the switch is set to 1, a bisection method is
                           !< used; if to 2, the newton-raphson method is used.
INTEGER, INTENT(OUT) :: ITCG!< itc, itcg and itg are switches to choose the iteration scheme to
                           !< be used in calculating the canopy or ground surface temperature
                           !< respectively.  if the switch is set to 1, a bisection method is
                           !< used; if to 2, the newton-raphson method is used.
INTEGER, INTENT(OUT) :: ITG!< itc, itcg and itg are switches to choose the iteration scheme to
                           !< be used in calculating the canopy or ground surface temperature
                           !< respectively.  if the switch is set to 1, a bisection method is
                           !< used; if to 2, the newton-raphson method is used.
   
INTEGER, INTENT(OUT) :: IPAI !< if ipai, ihgt, ialc, ials and ialg are zero, the values of 
                           !< plant area index, vegetation height, canopy albedo, snow albedo
                           !< and soil albedo respectively calculated by class are used.
                           !< if any of these switches is set to 1, the value of the
                           !< corresponding parameter calculated by class is overridden by
                           !< a user-supplied input value.  
INTEGER, INTENT(OUT) :: IHGT !< if ipai, ihgt, ialc, ials and ialg are zero, the values of 
                           !< plant area index, vegetation height, canopy albedo, snow albedo
                           !< and soil albedo respectively calculated by class are used.
                           !< if any of these switches is set to 1, the value of the
                           !< corresponding parameter calculated by class is overridden by
                           !< a user-supplied input value. 
INTEGER, INTENT(OUT) :: IALC !< if ipai, ihgt, ialc, ials and ialg are zero, the values of 
                           !< plant area index, vegetation height, canopy albedo, snow albedo
                           !< and soil albedo respectively calculated by class are used.
                           !< if any of these switches is set to 1, the value of the
                           !< corresponding parameter calculated by class is overridden by
                           !< a user-supplied input value. 
INTEGER, INTENT(OUT) :: IALS !< if ipai, ihgt, ialc, ials and ialg are zero, the values of 
                           !< plant area index, vegetation height, canopy albedo, snow albedo
                           !< and soil albedo respectively calculated by class are used.
                           !< if any of these switches is set to 1, the value of the
                           !< corresponding parameter calculated by class is overridden by
                           !< a user-supplied input value. 
INTEGER, INTENT(OUT) :: IALG !< if ipai, ihgt, ialc, ials and ialg are zero, the values of 
                           !< plant area index, vegetation height, canopy albedo, snow albedo
                           !< and soil albedo respectively calculated by class are used.
                           !< if any of these switches is set to 1, the value of the
                           !< corresponding parameter calculated by class is overridden by
                           !< a user-supplied input value. 

! -------------
! classctem output switches

! >>>> note: if you wish to use the values in the .ini file, set all to -9999 in the job options file
!            and the .ini file will be used.

integer, intent(out) :: jhhstd  !< day of the year to start writing the half-hourly output
integer, intent(out) :: jhhendd !< day of the year to stop writing the half-hourly output
integer, intent(out) :: jdstd   !< day of the year to start writing the daily output
integer, intent(out) :: jdendd  !< day of the year to stop writing the daily output
integer, intent(out) :: jhhsty  !< simulation year (iyear) to start writing the half-hourly output
integer, intent(out) :: jhhendy !< simulation year (iyear) to stop writing the half-hourly output
integer, intent(out) :: jdsty   !< simulation year (iyear) to start writing the daily output
integer, intent(out) :: jdendy  !< simulation year (iyear) to stop writing the daily output


integer, intent(out) :: isnoalb !< if isnoalb is set to 0, the original two-band snow albedo algorithms are used.  
                                !< if it is set to 1, the new four-band routines are used.

integer, intent(out) :: igralb  !< if igralb is set to 0, the wet and dry soil albedos are  calculated on the basis of 
                                !< soil texture.  if it is set to 1, they are assigned values based on the ncar clm soil "colour"  dataset.


! -------------

namelist /joboptions/ &
  transient_run,      &
  trans_startyr,      &
  ctemloop,           &
  ctem_on,            &
  ncyear,             &
  lnduseon,           &
  spinfast,           &
  cyclemet,           &
  nummetcylyrs,       &
  metcylyrst,         &
  co2on,              &
  setco2conc,         &
  ch4on,              &
  setch4conc,         &
  popdon,             &
  popcycleyr,         &
  parallelrun,        &
  dofire,             &
  dowetlands,         &
  obswetf,            &
  compete,            &
  inibioclim,         &
  start_bare,         &
  rsfile,             &
  start_from_rs,      &
  leap,               &
  IDISP,              &
  IZREF,              &
  ISLFD,              &
  IPCP,               &
  ITC,                &
  ITCG,               &
  ITG,                &
  IWF,                &
  IPAI,               &
  IHGT,               &
  IALC,               &
  IALS,               &
  IALG,               &
  isnoalb,            &
  igralb,             & 
  jhhstd,             &
  jhhendd,            &
  jdstd,              &
  jdendd,             &
  jhhsty,             &
  jhhendy,            &
  jdsty,              &
  jdendy,             &
  jmosty

character(140) :: jobfile
integer :: argcount, iargc 

!-------------------------
!read the joboptions


argcount = iargc()

       if(argcount .ne. 2)then
         write(*,*)'usage is as follows'
         write(*,*)' '
         write(*,*)'CLASS36CTEM joboptions_file site_name'
         write(*,*)' '
         write(*,*)'- joboptions_file - an example can be found '
         write(*,*)'  in the src folder - template_job_options_file.txt.'
         write(*,*)'  descriptions of the various variables '
         write(*,*)'  can be found in read_from_job_options.f90 '
         write(*,*)' '
         write(*,*)'- site_name is the prefix of your input files '
         write(*,*)' '
         !stop
      end if


!call getarg(1,jobfile)

open(10,file='job_options.txt',status='old')

read(10,nml = joboptions) !This small argument here will read the entire job_options file
                          !The rest of the file is simply declarations, explanation and creating the joboptions list variable

close(10)

call getarg(2,argbuff)

end subroutine read_from_job_options

