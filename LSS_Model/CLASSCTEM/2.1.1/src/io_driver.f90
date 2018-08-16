!>\defgroup io_driver_read_from_ctm
!>
!>This subroutine reads in the restart/starting conditions from
!!the .CTM file. The input values are checked and possibly adjusted
!!if the run is intended to be with competition on and if the start_bare flag is true. 
!------------------------------------------------------------------------------------
!>\defgroup io_driver_write_ctm_rs
!>
!>After a set period is complete the restart file for CTEM (.CTM_RS) is written
!!this restart file contains all of the CTEM level information needed to 
!!to restart the model to the same state.
!------------------------------------------------------------------------------------
!>\defgroup io_driver_create_outfiles
!>All output files are initialized in this subroutine
!------------------------------------------------------------------------------------
!>\defgroup io_driver_class_monthly_aw
!>Accumulate and write out the monthly CTEM outputs
!------------------------------------------------------------------------------------
!>\defgroup io_driver_class_annual_aw
!------------------------------------------------------------------------------------
!>\defgroup io_driver_ctem_daily_aw
!>Accumulate and write out the daily CTEM outputs
!------------------------------------------------------------------------------------
!>\defgroup io_driver_ctem_monthly_aw
!------------------------------------------------------------------------------------
!>\defgroup io_driver_ctem_annual_aw
!------------------------------------------------------------------------------------
!>\defgroup io_driver_close_outfiles
!------------------------------------------------------------------------------------

!>\file
!!Central module that handles all CTEM reading and writing of external files
module io_driver

! J. Melton Mar 30 2015

implicit none

! subroutines contained in this module:
public  :: read_from_ctm
public  :: write_ctm_rs
public  :: create_outfiles
public  :: class_monthly_aw
public  :: class_annual_aw
public  :: ctem_daily_aw
public  :: ctem_monthly_aw
public  :: ctem_annual_aw
public  :: close_outfiles

contains
!MESH Adding the STRLEN() since RUNCLASS36CTEM is no longer a program, so define the function here
!Instead of using the module inside this module, we'll just place the function here




!-------------------------------------------------------------------------------------------------------------

!>\ingroup io_driver_read_from_ctm
!!@{

subroutine read_from_ctm(nltest,nmtest,FCANROT,FAREROT,RSMNROT,QA50ROT, &
                         VPDAROT,VPDBROT,PSGAROT,PSGBROT,DRNROT,SDEPROT,DDROT, &
                         XSLPROT,GRKFROT,WFSFROT,WFCIROT,MIDROT,SANDROT, &
                         CLAYROT,ORGMROT,TBARROT,THLQROT,THICROT,TCANROT, &
                         TSNOROT,TPNDROT,ZPNDROT,RCANROT,SCANROT,SNOROT, &
                         ALBSROT,RHOSROT,GROROT,argbuff,onetile_perPFT)

use ctem_params,        only : icc,iccp1,nmos,seed,ignd,ilg,icp1,nlat,ican,abszero
use ctem_statevars,     only : c_switch,vrot,vgat

implicit none



! arguments:
character(80), intent(in) :: argbuff
integer, intent(in) :: nltest
integer, intent(inout) :: nmtest
real, dimension(nlat,nmos,icp1), intent(inout) :: FCANROT
real, dimension(nlat,nmos), intent(inout) :: FAREROT
real, dimension(nlat,nmos,ican), intent(inout) :: RSMNROT
real, dimension(nlat,nmos,ican), intent(inout) :: QA50ROT
real, dimension(nlat,nmos,ican), intent(inout) :: VPDAROT
real, dimension(nlat,nmos,ican), intent(inout) :: VPDBROT
real, dimension(nlat,nmos,ican), intent(inout) :: PSGAROT
real, dimension(nlat,nmos,ican), intent(inout) :: PSGBROT
real, dimension(nlat,nmos), intent(inout) :: DRNROT
real, dimension(nlat,nmos), intent(inout) :: SDEPROT
real, dimension(nlat,nmos), intent(inout) :: DDROT
real, dimension(nlat,nmos), intent(inout) :: XSLPROT
real, dimension(nlat,nmos), intent(inout) :: GRKFROT
real, dimension(nlat,nmos), intent(inout) :: WFSFROT
real, dimension(nlat,nmos), intent(inout) :: WFCIROT
integer, dimension(nlat,nmos), intent(inout) :: MIDROT
real, dimension(nlat,nmos,ignd), intent(inout) :: SANDROT
real, dimension(nlat,nmos,ignd), intent(inout) :: CLAYROT
real, dimension(nlat,nmos,ignd), intent(inout) :: ORGMROT
real, dimension(nlat,nmos,ignd), intent(inout) :: TBARROT
real, dimension(nlat,nmos,ignd), intent(inout) :: THLQROT
real, dimension(nlat,nmos,ignd), intent(inout) :: THICROT
real, dimension(nlat,nmos), intent(inout) :: TCANROT
real, dimension(nlat,nmos), intent(inout) :: TSNOROT
real, dimension(nlat,nmos), intent(inout) :: TPNDROT
real, dimension(nlat,nmos), intent(inout) :: ZPNDROT
real, dimension(nlat,nmos), intent(inout) :: RCANROT
real, dimension(nlat,nmos), intent(inout) :: SCANROT
real, dimension(nlat,nmos), intent(inout) :: SNOROT
real, dimension(nlat,nmos), intent(inout) :: ALBSROT
real, dimension(nlat,nmos), intent(inout) :: RHOSROT
real, dimension(nlat,nmos), intent(inout) :: GROROT
logical, intent(in) :: onetile_perPFT

! pointers:
logical, pointer :: dofire
logical, pointer :: compete
logical, pointer :: inibioclim
logical, pointer :: dowetlands
logical, pointer :: start_bare
logical, pointer :: lnduseon
logical, pointer :: obswetf
character(80), pointer :: titlec1
character(80), pointer :: titlec2
character(80), pointer :: titlec3
real, pointer, dimension(:,:,:) :: ailcminrow           !
real, pointer, dimension(:,:,:) :: ailcmaxrow           !
real, pointer, dimension(:,:,:) :: dvdfcanrow           !
real, pointer, dimension(:,:,:) :: gleafmasrow          !
real, pointer, dimension(:,:,:) :: bleafmasrow          !
real, pointer, dimension(:,:,:) :: stemmassrow          !
real, pointer, dimension(:,:,:) :: rootmassrow          !
real, pointer, dimension(:,:,:) :: pstemmassrow         !
real, pointer, dimension(:,:,:) :: pgleafmassrow        !     
real, pointer, dimension(:,:) :: twarmm            !< temperature of the warmest month (c)
real, pointer, dimension(:,:) :: tcoldm            !< temperature of the coldest month (c)
real, pointer, dimension(:,:) :: gdd5              !< growing degree days above 5 c
real, pointer, dimension(:,:) :: aridity           !< aridity index, ratio of potential evaporation to precipitation
real, pointer, dimension(:,:) :: srplsmon          !< number of months in a year with surplus water i.e.precipitation more than potential evaporation
real, pointer, dimension(:,:) :: defctmon          !< number of months in a year with water deficit i.e.precipitation less than potential evaporation
real, pointer, dimension(:,:) :: anndefct          !< annual water deficit (mm)
real, pointer, dimension(:,:) :: annsrpls          !< annual water surplus (mm)
real, pointer, dimension(:,:) :: annpcp            !< annual precipitation (mm)
real, pointer, dimension(:,:) :: dry_season_length !< length of dry season (months)
real, pointer, dimension(:,:,:) :: litrmassrow
real, pointer, dimension(:,:,:) :: soilcmasrow
real, pointer, dimension(:,:) :: extnprob
real, pointer, dimension(:,:) :: prbfrhuc
real, pointer, dimension(:,:,:) :: mlightng
integer, pointer, dimension(:,:,:) :: lfstatusrow
integer, pointer, dimension(:,:,:) :: pandaysrow
integer, pointer, dimension(:,:) :: stdaln
real, pointer, dimension(:,:,:) :: slopefrac

! local variables

integer :: i,m,j,strlen
real, dimension(ilg,2) :: crop_temp_frac

! point pointers:
dofire            => c_switch%dofire
compete           => c_switch%compete
inibioclim        => c_switch%inibioclim
dowetlands        => c_switch%dowetlands
start_bare        => c_switch%start_bare    
lnduseon          => c_switch%lnduseon
obswetf           => c_switch%obswetf
titlec1           => c_switch%titlec1
titlec2           => c_switch%titlec2
titlec3           => c_switch%titlec3
ailcminrow        => vrot%ailcmin
ailcmaxrow        => vrot%ailcmax
dvdfcanrow        => vrot%dvdfcan
gleafmasrow       => vrot%gleafmas
bleafmasrow       => vrot%bleafmas
stemmassrow       => vrot%stemmass
rootmassrow       => vrot%rootmass
pstemmassrow      => vrot%pstemmass
pgleafmassrow     => vrot%pgleafmass
twarmm            => vrot%twarmm
tcoldm            => vrot%tcoldm
gdd5              => vrot%gdd5
aridity           => vrot%aridity
srplsmon          => vrot%srplsmon
defctmon          => vrot%defctmon
anndefct          => vrot%anndefct
annsrpls          => vrot%annsrpls
annpcp            => vrot%annpcp
dry_season_length => vrot%dry_season_length
litrmassrow       => vrot%litrmass
soilcmasrow       => vrot%soilcmas
extnprob          => vrot%extnprob
prbfrhuc          => vrot%prbfrhuc
mlightng          => vrot%mlightng
slopefrac         => vrot%slopefrac
stdaln            => vrot%stdaln
lfstatusrow       => vrot%lfstatus
pandaysrow        => vrot%pandays
      
! -----------------      
! Begin      

!Here is where I defined the location of the .CTM file
open(unit=11,file='MESH_parameters_CTEM.CTM', status='old')

read (11,7010) titlec1
read (11,7010) titlec2
read (11,7010) titlec3

7010  FORMAT(A80)

!>Read from CTEM initialization file (.CTM)
!The file was modified in order to assume that each MESH GRU has the same initial values
!So it will read over each GRU first(nmtest) then spread the values of the GRU to each grid (nltest)

 
    do m=1,nmtest !This will loop over all of the MOSAIC tiles (number of GRUs)

!>The following three variables are needed to run CTEM. 1) min & 2) max leaf area index are needed to break
!>class lai into dcd and evg for trees (for crops and grasses it doesn't matter much). 3) dvdfcanrow is 
!>needed to divide needle & broad leaf into dcd and evg, and crops & grasses into c3 and c4 fractions.
      

        read(11,*) (ailcminrow(1,m,j),j=1,icc)
        read(11,*) (ailcmaxrow(1,m,j),j=1,icc)
        read(11,*) (dvdfcanrow(1,m,j),j=1,icc)

!>
!>Rest of the initialization variables are needed to run CTEM but if starting from bare ground initialize all 
!>live and dead c pools from zero. suitable values of extnprobgrd and prbfrhucgrd would still be required. set 
!>stdaln to 1 for operation in non-gcm stand alone mode, in the CTEM initialization file.
!>
        read(11,*) (gleafmasrow(1,m,j),j=1,icc)
        read(11,*) (bleafmasrow(1,m,j),j=1,icc)
        read(11,*) (stemmassrow(1,m,j),j=1,icc)
        read(11,*) (rootmassrow(1,m,j),j=1,icc)
!>
!>If fire and competition are on, save the stemmass and rootmass for use in burntobare subroutine on the first timestep.
        if (dofire .and. compete) then
            do j =1,icc
            pstemmassrow(1,m,j)=stemmassrow(1,m,j)
            pgleafmassrow(1,m,j)=rootmassrow(1,m,j)    
            end do           
        end if

        read(11,*) (litrmassrow(1,m,j),j=1,iccp1)
        read(11,*) (soilcmasrow(1,m,j),j=1,iccp1)
        read(11,*) (lfstatusrow(1,m,j),j=1,icc)
        read(11,*) (pandaysrow(1,m,j),j=1,icc)

     ENDDO !End reading over each MOSAIC TILE
!End Reading each MOSAIC tile/GRU (nmtest)


!Now start reading the last lines of the .CTM file
     read(11,*) (mlightng(1,1,j),j=1,6)  !mean monthly lightning frequency
     read(11,*) (mlightng(1,1,j),j=7,12) !flashes/km2.year, this is spread over other tiles below
     read(11,*) extnprob(1,1)
     read(11,*) prbfrhuc(1,1)
     read(11,*) stdaln(1,1)


!Not sure if this is needed, will eliminate later
      DO I=1,NLTEST
        if (compete .and. inibioclim) then  !read in the bioclimatic parameters
        ! read them into the first tile of each grid cell.
        read(11,*) twarmm(i,1), tcoldm(i,1), gdd5(i,1), aridity(i,1),srplsmon(i,1)
        read(11,*) defctmon(i,1), anndefct(i,1), annsrpls(i,1), annpcp(i,1), dry_season_length(i,1)
        else if (compete .and. .not. inibioclim) then ! set them to zero
            twarmm(i,1)=0.0
            tcoldm(i,1)=0.0
            gdd5(i,1)=0.0
            aridity(i,1)=0.0
            srplsmon(i,1)=0.0
            defctmon(i,1)=0.0
            anndefct(i,1)=0.0
            annsrpls(i,1)=0.0
            annpcp(i,1)=0.0
            dry_season_length(i,1) = 0.0
        endif
!>Take the first tile value now and put it over the other tiles
!These are not initial values but rather information that should be the same for al MOSAIC tiles
        if (nmtest > 1) then
            do m = 2,nmtest
                twarmm(i,m)=twarmm(i,1)
                tcoldm(i,m)=tcoldm(i,1)
                gdd5(i,m)=gdd5(i,1)
                aridity(i,m)=aridity(i,1)
                srplsmon(i,m)=srplsmon(i,1)
                defctmon(i,m)=defctmon(i,1)
                anndefct(i,m)=anndefct(i,1)
                annsrpls(i,m)=annsrpls(i,1)
                annpcp(i,m)=annpcp(i,1)
                dry_season_length(i,m) =dry_season_length(i,1)
                mlightng(i,m,:) = mlightng(i,1,:)
                extnprob(i,m) = extnprob(i,1)
                prbfrhuc(i,m) = prbfrhuc(i,1)
                stdaln(i,m) = stdaln(i,1)
            end do
        end if
      END DO !NLTEST


      !Place the initial conditions of the GRU across the different grids (NLTEST)
      !After this, each grid will have all of the data of each GRU/MOSAIC
        IF (NLTEST > 1) THEN
	    DO I=2,NLTEST
             DO M=1,NMTEST !Give grid I all GRUs/MOSAICs information that we read before

		DO J=1,icc
	 	  ailcminrow(i,m,j) = ailcminrow(1,m,j)
        	  ailcmaxrow(i,m,j) = ailcmaxrow(1,m,j)
        	  dvdfcanrow(i,m,j) = dvdfcanrow(1,m,j)
        	  gleafmasrow(i,m,j) = gleafmasrow(1,m,j)
        	  bleafmasrow(i,m,j) = bleafmasrow(1,m,j)
        	  stemmassrow(i,m,j) = stemmassrow(1,m,j)
        	  rootmassrow(i,m,j) = rootmassrow(1,m,j)
		  lfstatusrow(i,m,j) = lfstatusrow(1,m,j)
        	  pandaysrow(i,m,j) = pandaysrow(1,m,j)
		END DO
	
		DO J=1,iccp1
        	   litrmassrow(i,m,j) = litrmassrow(i,1,j)
        	   soilcmasrow(i,m,j) = soilcmasrow(i,1,j)
		END DO

       
          END DO !NMTEST
        END DO !NLTEST
     END IF !NLTEST > 1

      !Sanity check to make sure parameters and stuff is fine:


close(11)
    


!>Check that a competition or luc run has the correct number of mosaics. if it is not a start_bare run, then nmtest should equal nmos
      if (onetile_perPFT .and. (compete .or. lnduseon) .and. .not. start_bare) then
        if (nmtest .ne. nmos) then
           write(6,*)'compete or luc runs that do not start from bare'
           write(6,*)'ground need the number of mosaics to equal icc+1'
           write(6,*)'nmtest = ',nmtest,' nmos = ',nmos
            call xit('runclass36ctem', -2)
        endif
      endif
!>
!>if this run uses the competition or lnduseon parameterization and starts from bare ground, set up the model state here. this 
!>overwrites what was read in from the .ini and .ctm files. for composite runs (the composite set up is after this one for mosaics)
      if ((compete .or. lnduseon) .and. start_bare) then

       if (onetile_perPFT) then
!>
!!store the read-in crop fractions as we keep them even when we start bare. 
!!FLAG: this is setup assuming that crops are in mosaics 6 and 7. JM Apr 9 2014.
         do i=1,nltest
          crop_temp_frac(i,1)=FAREROT(i,6)
          crop_temp_frac(i,2)=FAREROT(i,7)
         end do

!>check the number of mosaics that came from the .ini file
        if (nmtest .ne. nmos) then

!Note that SAND,CLAY,ORGM and SDEP were commented out for now because they are not needed specifically here for MESH

!>we need to transfer some initial parameterization info to all mosaics, so set all values to that of the first mosaic.


!>we need to transfer some initial parameterization info to all mosaics, so set all values to that of the first mosaic.
         do i=1,nltest
          do m=nmtest+1,nmos

           do j=1,ican
             RSMNROT(i,m,j)=RSMNROT(i,1,j)
             QA50ROT(i,m,j)=QA50ROT(i,1,j)
             VPDAROT(i,m,j)=VPDAROT(i,1,j)
             VPDBROT(i,m,j)=VPDBROT(i,1,j)
             PSGAROT(i,m,j)=PSGAROT(i,1,j)
             PSGBROT(i,m,j)=PSGBROT(i,1,j)
           enddo

           DRNROT(i,m)=DRNROT(i,1)
           SDEPROT(i,m)=SDEPROT(i,1)
           FAREROT(i,m)=FAREROT(i,1)
           DDROT(i,m)  =DDROT(i,1)
           XSLPROT(i,m)=XSLPROT(i,1)
           GRKFROT(i,m)=GRKFROT(i,1)
           WFSFROT(i,m)=WFSFROT(i,1)
           WFCIROT(i,m)=WFCIROT(i,1)
           MIDROT(i,m)=MIDROT(i,1)

           do j=1,3
            SANDROT(i,m,j)=SANDROT(i,1,j)
            CLAYROT(i,m,j)=CLAYROT(i,1,j)
            ORGMROT(i,m,j)=ORGMROT(i,1,j)
            TBARROT(i,m,j)=TBARROT(i,1,j)
            THLQROT(i,m,j)=THLQROT(i,1,j)
            THICROT(i,m,j)=THICROT(i,1,j)
           enddo

           TCANROT(i,m)=TCANROT(i,1)
           TSNOROT(i,m)=TSNOROT(i,1)
           TPNDROT(i,m)=TPNDROT(i,1)
           ZPNDROT(i,m)=ZPNDROT(i,1)
           RCANROT(i,m)=RCANROT(i,1)
           SCANROT(i,m)=SCANROT(i,1)
           SNOROT(i,m)=SNOROT(i,1)
           ALBSROT(i,m)=ALBSROT(i,1)
           RHOSROT(i,m)=RHOSROT(i,1)
           GROROT(i,m)=GROROT(i,1)
           do j=1,icc
             lfstatusrow(i,m,j) = 4
           enddo !j

          enddo !m
         enddo !i

!>set the number of mosaics to icc+1        
        nmtest=nmos

        endif  !>if (nmtest .ne. nmos)

!>set the initial conditions for the pfts
!>(bah, this is such an inelegant way to do this, but oh well...)

!>initalize to zero
        FCANROT=0.0
        dvdfcanrow=0.0
        FAREROT=0.0

        do i=1,nltest
         do m=1,nmtest

!>set the seed amount for each pft in its mosaic
          if (compete .or. lnduseon) then
            if (m .lt. icc+1) then
             FAREROT(i,m)=seed
            else
             FAREROT(i,m)=1.0 - (real(icc) * seed)
            endif
          endif

          do j = 1,icc
            ailcminrow(i,m,j)=0.0
            ailcmaxrow(i,m,j)=0.0
            gleafmasrow(i,m,j)=0.0
            bleafmasrow(i,m,j)=0.0
            stemmassrow(i,m,j)=0.0
            rootmassrow(i,m,j)=0.0
            lfstatusrow(i,m,j)=4
            pandaysrow(i,m,j)=0
          enddo
  
          lfstatusrow(i,m,1)=2

          do j = 1,iccp1
            litrmassrow(i,m,j)=0. 
            soilcmasrow(i,m,j)=0. 
          enddo

!>initial conditions always required
          dvdfcanrow(i,m,1)=1.0  !ndl
          dvdfcanrow(i,m,3)=1.0  !bdl
          dvdfcanrow(i,m,6)=1.0  !crop
          dvdfcanrow(i,m,8)=1.0  !grasses

!>then adjusted below for the actual mosaic makeup
          if (m .le. 2) then                     !ndl
           FCANROT(i,m,1)=1.0
           if (m .eq. 2) then
             dvdfcanrow(i,m,1)=0.0
             dvdfcanrow(i,m,2)=1.0        
           endif
          elseif (m .ge. 3 .and. m .le. 5) then  !bdl
           FCANROT(i,m,2)=1.0
           if (m .eq. 4) then
             dvdfcanrow(i,m,3)=0.0
             dvdfcanrow(i,m,4)=1.0        
           endif
           if (m .eq. 5) then
             dvdfcanrow(i,m,3)=0.0
             dvdfcanrow(i,m,5)=1.0        
           endif
          elseif (m .eq. 6 .or. m .eq. 7) then  !crop
           FCANROT(i,m,3)=1.0
           if (m .eq. 7) then
             dvdfcanrow(i,m,6)=0.0
             dvdfcanrow(i,m,7)=1.0        
           endif
          elseif (m .eq. 8 .or. m .eq. 9) then  !grasses
           FCANROT(i,m,4)=1.0
           if (m .eq. 9) then
             dvdfcanrow(i,m,8)=0.0
             dvdfcanrow(i,m,9)=1.0        
           endif
          else                                  !bare/urban? 
           FCANROT(i,m,5)=1.0
           endif !mosaic adjustments
         enddo  !m
        enddo  !i


         do i=1,nltest
          FAREROT(i,6)=crop_temp_frac(i,1)
          FAREROT(i,7)=crop_temp_frac(i,2)
         end do

      else if (.not. onetile_perPFT) then  
!>set up for composite runs when start_bare is on and compete or landuseon

!>store the read-in crop fractions as we keep them even when we start bare. 
!!FLAG: this is setup assuming that crops are in pft number 6 and 7.
!!and the first tile contains the information for the grid cell (assumes we have crops in
!!every tile too! JM Apr 9 2014.
         do i=1,nltest
          crop_temp_frac(i,1)=FCANROT(i,1,3)*dvdfcanrow(i,1,6)
          crop_temp_frac(i,2)=FCANROT(i,1,3)*dvdfcanrow(i,1,7)
         end do
!>
!>initalize to zero, these will be filled in by the luc or competition subroutines.
       FCANROT=0.0
       dvdfcanrow=0.0

       ! Added this as start_bare runs were not properly assigning 
       ! a TCAN on the very first day since the FCANROT was 0. JM Jan 14 2014. 
!        do i=1,nltest
!         do m = 1,nmtest
!          do j=1,icp1
!            if (j .lt. icp1) then
!             FCANROT(i,m,j)=seed
!            else
!             FCANROT(i,m,j)=1.0 - (real(ican) * seed)
!            endif
!          end do
!         end do
!        end do

       do i=1,nltest
         do m = 1,nmtest

    !>initial conditions always required
            dvdfcanrow(i,m,1)=1.0  !ndl
            dvdfcanrow(i,m,3)=1.0  !bdl
            dvdfcanrow(i,m,6)=1.0  !crop
            dvdfcanrow(i,m,8)=1.0  !grasses

            do j = 1,icc
            ailcminrow(i,m,j)=0.0
            ailcmaxrow(i,m,j)=0.0
            gleafmasrow(i,m,j)=0.0
            bleafmasrow(i,m,j)=0.0
            stemmassrow(i,m,j)=0.0
            rootmassrow(i,m,j)=0.0
            lfstatusrow(i,m,j)=4
            pandaysrow(i,m,j)=0
            enddo

            lfstatusrow(i,m,1)=2

            do j = 1,iccp1
            litrmassrow(i,m,j)=0.0
            soilcmasrow(i,m,j)=0.0
            enddo
         end do ! nmtest
       enddo !nltest

         do i=1,nltest
            do m = 1,nmtest
                FCANROT(i,m,3) = crop_temp_frac(i,1) + crop_temp_frac(i,2)
                if (FCANROT(i,m,3) .gt. abszero) then
                dvdfcanrow(i,m,6) = crop_temp_frac(i,1) / FCANROT(i,m,3)
                dvdfcanrow(i,m,7) = crop_temp_frac(i,2) / FCANROT(i,m,3)
                else
                dvdfcanrow(i,m,6) = 1.0
                dvdfcanrow(i,m,7) = 0.0
                end if
            end do !nmtest
         end do !nltest

      end if ! mosaic / composite
      end if !if (compete/landuseon .and. start_bare) 


end subroutine read_from_ctm
!>@}
!==============================================================================================================

!>\ingroup io_driver_write_ctm_rs
!>@{

subroutine write_ctm_rs(nltest,nmtest,FCANROT,argbuff)

use ctem_params,        only : ican,l2max,modelpft,icc,nmos,nlat,icp1,iccp1
use ctem_statevars,     only : c_switch,vrot,vgat

implicit none

! arguments:
character(80), intent(in) :: argbuff
integer, intent(in) :: nltest
integer, intent(inout) :: nmtest
real, dimension(nlat,nmos,icp1), intent(inout) :: FCANROT

! pointers:
logical, pointer :: lnduseon
logical, pointer :: compete
logical, pointer :: dowetlands
character(80), pointer :: titlec1
character(80), pointer :: titlec2
character(80), pointer :: titlec3
integer, pointer, dimension(:,:) :: icountrow
real, pointer, dimension(:,:,:) :: dvdfcanrow           !
real, pointer, dimension(:,:,:) :: fcancmxrow
real, pointer, dimension(:,:,:) :: ailcminrow           !
real, pointer, dimension(:,:,:) :: ailcmaxrow           !
real, pointer, dimension(:,:,:) :: gleafmasrow          !
real, pointer, dimension(:,:,:) :: bleafmasrow          !
real, pointer, dimension(:,:,:) :: stemmassrow          !
real, pointer, dimension(:,:,:) :: rootmassrow          !
real, pointer, dimension(:,:,:) :: litrmassrow
real, pointer, dimension(:,:,:) :: soilcmasrow
integer, pointer, dimension(:,:,:) :: lfstatusrow
integer, pointer, dimension(:,:,:) :: pandaysrow
real, pointer, dimension(:,:) :: extnprob
real, pointer, dimension(:,:) :: prbfrhuc
real, pointer, dimension(:,:,:) :: mlightng
integer, pointer, dimension(:,:) :: stdaln
real, pointer, dimension(:,:) :: twarmm            !< temperature of the warmest month (c)
real, pointer, dimension(:,:) :: tcoldm            !< temperature of the coldest month (c)
real, pointer, dimension(:,:) :: gdd5              !< growing degree days above 5 c
real, pointer, dimension(:,:) :: aridity           !< aridity index, ratio of potential evaporation to precipitation
real, pointer, dimension(:,:) :: srplsmon          !< number of months in a year with surplus water i.e.precipitation more than potential evaporation
real, pointer, dimension(:,:) :: defctmon          !< number of months in a year with water deficit i.e.precipitation less than potential evaporation
real, pointer, dimension(:,:) :: anndefct          !< annual water deficit (mm)
real, pointer, dimension(:,:) :: annsrpls          !< annual water surplus (mm)
real, pointer, dimension(:,:) :: annpcp            !< annual precipitation (mm)
real, pointer, dimension(:,:) :: dry_season_length !< length of dry season (months)
real, pointer, dimension(:,:,:) :: slopefrac       !< Fraction flatter than the slope threshold

! local variables

integer :: i,m,j,strlen
integer :: k1c,k2c,n
real, dimension(icc) :: rnded_pft

! point pointers:
lnduseon          => c_switch%lnduseon
compete           => c_switch%compete
dowetlands        => c_switch%dowetlands
titlec1           => c_switch%titlec1
titlec2           => c_switch%titlec2
titlec3           => c_switch%titlec3
icountrow         => vrot%icount
dvdfcanrow        => vrot%dvdfcan
fcancmxrow        => vrot%fcancmx
ailcminrow        => vrot%ailcmin
ailcmaxrow        => vrot%ailcmax
gleafmasrow       => vrot%gleafmas
bleafmasrow       => vrot%bleafmas
stemmassrow       => vrot%stemmass
rootmassrow       => vrot%rootmass
litrmassrow       => vrot%litrmass
soilcmasrow       => vrot%soilcmas
lfstatusrow       => vrot%lfstatus
pandaysrow        => vrot%pandays
extnprob          => vrot%extnprob
prbfrhuc          => vrot%prbfrhuc
mlightng          => vrot%mlightng
stdaln            => vrot%stdaln
twarmm            => vrot%twarmm
tcoldm            => vrot%tcoldm
gdd5              => vrot%gdd5
aridity           => vrot%aridity
srplsmon          => vrot%srplsmon
defctmon          => vrot%defctmon
anndefct          => vrot%anndefct
annsrpls          => vrot%annsrpls
annpcp            => vrot%annpcp
dry_season_length => vrot%dry_season_length
slopefrac         => vrot%slopefrac
      
! -----------------      
! Begin


open(unit=101,file=argbuff(1:strlen(argbuff))//'.CTM_RS')

write(101,7010) titlec1
write(101,7010) titlec2
write(101,7010) titlec3

7010  FORMAT(A80)

!> if landuseon or competition, then we need to recreate the dvdfcanrow so do so now
if (lnduseon .or. compete ) then
  icountrow=0
  do j = 1, ican
    do i = 1,nltest
        do m = 1,nmtest 
        k1c = (j-1)*l2max + 1
        k2c = k1c + (l2max - 1)
    
        do n = k1c, k2c
          if (modelpft(n) .eq. 1) then
            icountrow(i,m) = icountrow(i,m) + 1
            if (FCANROT(i,m,j) .gt. 0.) then
              dvdfcanrow(i,m,icountrow(i,m)) = fcancmxrow(i,m,icountrow(i,m))/FCANROT(i,m,j)
            else
              dvdfcanrow(i,m,icountrow(i,m)) = 0.
            end if
          end if !modelpft
        end do !n
    
        !> check to ensure that the dvdfcanrow's add up to 1 across a class-level pft
        if (dvdfcanrow(i,m,1) .eq. 0. .and. dvdfcanrow(i,m,2) .eq. 0.) then
            dvdfcanrow(i,m,1)=1.0
        else if (dvdfcanrow(i,m,3) .eq. 0. .and. dvdfcanrow(i,m,4) .eq. 0. .and. dvdfcanrow(i,m,5) .eq. 0.) then
            dvdfcanrow(i,m,3)=1.0
        else if (dvdfcanrow(i,m,6) .eq. 0. .and. dvdfcanrow(i,m,7) .eq. 0.) then
            dvdfcanrow(i,m,6)=1.0
        else if (dvdfcanrow(i,m,8) .eq. 0. .and. dvdfcanrow(i,m,9) .eq. 0.) then
            dvdfcanrow(i,m,8)=1.0
        end if

        end do !m
    enddo !i 
  enddo !j

  do i=1,nltest
   do m=1,nmtest 
    do j = 1, icc
!>Lastly check if the different pfts accidently add up > 1.0 after rounding to the number of sig figs used in the output
!>this rounds to 3 decimal places. if you are found to be over or under, arbitrarily reduce one of the pfts. the amount of
!>the change will be inconsequential. 
       rnded_pft(j) =real(int(dvdfcanrow(i,m,j) * 1000.0))/ 1000.0
       dvdfcanrow(i,m,j) = rnded_pft(j)
    end do

    if (dvdfcanrow(i,m,1) + dvdfcanrow(i,m,2) .ne. 1.0) then
        dvdfcanrow(i,m,1) = 1.0 - rnded_pft(2)
        dvdfcanrow(i,m,2) = rnded_pft(2)
    end if 
    if (dvdfcanrow(i,m,3) + dvdfcanrow(i,m,4) +  dvdfcanrow(i,m,5) .ne. 1.0) then
        dvdfcanrow(i,m,3) = 1.0 - rnded_pft(4) - rnded_pft(5)
        dvdfcanrow(i,m,4) = rnded_pft(4)
        dvdfcanrow(i,m,5) = rnded_pft(5)
    end if 
    if (dvdfcanrow(i,m,6) + dvdfcanrow(i,m,7) .ne. 1.0) then
        dvdfcanrow(i,m,6) = 1.0 - rnded_pft(7)
        dvdfcanrow(i,m,7) = rnded_pft(7)
    end if 
    if (dvdfcanrow(i,m,8) + dvdfcanrow(i,m,9) .ne. 1.0) then
        dvdfcanrow(i,m,8) = 1.0 - rnded_pft(9)
        dvdfcanrow(i,m,9) = rnded_pft(9)
    end if
   enddo
  enddo

end if !lnuse/compete

do i=1,nltest
    do m=1,nmtest
        write(101,7011) (ailcminrow(i,m,j),j=1,icc)
        write(101,7011) (ailcmaxrow(i,m,j),j=1,icc)
        write(101,'(9f8.3)') (dvdfcanrow(i,m,j),j=1,icc)
        write(101,7011) (gleafmasrow(i,m,j),j=1,icc)
        write(101,7011) (bleafmasrow(i,m,j),j=1,icc)
        write(101,7011) (stemmassrow(i,m,j),j=1,icc)
        write(101,7011) (rootmassrow(i,m,j),j=1,icc)
        write(101,7013) (litrmassrow(i,m,j),j=1,iccp1)
        write(101,7013) (soilcmasrow(i,m,j),j=1,iccp1)
        write(101,7012) (lfstatusrow(i,m,j),j=1,icc)
        write(101,7012) (pandaysrow(i,m,j),j=1,icc)
    end do !nmtest

    write(101,"(6f8.3)") (mlightng(i,1,j),j=1,6)  !mean monthly lightning frequency
    write(101,"(6f8.3)") (mlightng(i,1,j),j=7,12) !flashes/km2.year, use the first tile since all the same.
    write(101,"(f8.2)") extnprob(i,1)
    write(101,"(f8.2)") prbfrhuc(i,1)
    write(101,"(i4)") stdaln(i,1)

    if (compete) then
    !>We can write out the first tile value since these are the same across an entire gridcell.
        write(101,7014)twarmm(i,1),tcoldm(i,1),gdd5(i,1),aridity(i,1),srplsmon(i,1)
        write(101,7014)defctmon(i,1),anndefct(i,1),annsrpls(i,1), annpcp(i,1),dry_season_length(i,1)
    end if

    if (dowetlands) then     
        !>Just write in the first tiles value since all tiles in a gridcell are the same.
        write(101,"(8f9.5)")(slopefrac(i,1,j),j=1,8)
    end if   

end do !nltest


close(101)

7011  format(9ES12.5)
7012  format(9i8)
7013  format(10ES12.5)
7014  format(5ES12.4)

end subroutine write_ctm_rs        
!>@}
!==============================================================================================================

!>\ingroup io_driver_create_outfiles
!>@{

subroutine create_outfiles(argbuff,title1, title2, title3, title4, title5, title6, name1, name2, name3, &
                           name4, name5, name6, place1 ,place2, place3, place4, place5, place6)

use ctem_statevars,     only : c_switch,vrot,vgat

implicit none

! arguments:
character(80), intent(in) :: argbuff
character(4), intent(in) :: title1, title2, title3, title4, &
                            title5, title6, name1, name2, name3, &
                            name4, name5, name6, place1 ,place2, &
                            place3, place4, place5, place6

! pointers:
logical, pointer :: dofire
logical, pointer :: ctem_on
logical, pointer :: compete
logical, pointer :: dowetlands
logical, pointer :: lnduseon
logical, pointer :: obswetf
logical, pointer :: parallelrun

! local variables:
integer :: strlen
character(80) :: titlec1, titlec2, titlec3

! point pointers:
dofire            => c_switch%dofire
ctem_on           => c_switch%ctem_on
compete           => c_switch%compete
dowetlands        => c_switch%dowetlands    
lnduseon          => c_switch%lnduseon
obswetf           => c_switch%obswetf
parallelrun       => c_switch%parallelrun

!-----     
!>begin:
!!
!!the ctem output file suffix naming convention is as follows: ".CT##{time}"
!!where the ## is a numerical identifier, {time} is any of H, D, M,
!!or Y for half hourly, daily, monthly, or yearly, respectively. 
!!
     
6001  FORMAT('#CLASS-CTEM TEST RUN:     ',6A4)
6002  FORMAT('#RESEARCHER:         ',6A4)
6003  FORMAT('#INSTITUTION:        ',6A4)

if (.not. parallelrun .and. ctem_on) then !>stand alone mode, includes half-hourly and daily output

    !>ctem half hourly output files
    open(unit=71, file=argbuff(1:strlen(argbuff))//'.CT01H_M')  
    open(unit=711,file=argbuff(1:strlen(argbuff))//'.CT01H_G')

    !>ctem daily output files
    open(unit=72,file=argbuff(1:strlen(argbuff))//'.CT01D')
    open(unit=73,file=argbuff(1:strlen(argbuff))//'.CT02D')
    open(unit=74,file=argbuff(1:strlen(argbuff))//'.CT03D')
    open(unit=75,file=argbuff(1:strlen(argbuff))//'.CT04D')
    !open(unit=76,file=argbuff(1:strlen(argbuff))//'.CT05D') !FLAG. this one is turned off for now. You can turn on but go through the vars to make sure all is ok.

    if (dofire .or. lnduseon) then
    open(unit=77,file=argbuff(1:strlen(argbuff))//'.CT06D') ! disturbance vars
    endif

    if (compete .or. lnduseon) then
        open(unit=78,file=argbuff(1:strlen(argbuff))//'.CT07D') ! competition
    end if

    if (dowetlands .or. obswetf) then
        open(unit=79,file=argbuff(1:strlen(argbuff))//'.CT08D') ! Methane(Wetland)
    endif 

endif ! parallelrun & ctem_on


!===========================
!
!     CTEM FILE TITLES
!
if (ctem_on .and. .not. parallelrun) then
    write(71,6001) title1,title2,title3,title4,title5,title6
    write(71,6002) name1,name2,name3,name4,name5,name6
    write(71,6003) place1,place2,place3,place4,place5,place6
    write(71,7020)
    write(71,7030)
    
    write(72,6001) title1,title2,title3,title4,title5,title6
    write(72,6002) name1,name2,name3,name4,name5,name6
    write(72,6003) place1,place2,place3,place4,place5,place6
    write(72,7020)
    write(72,7040)

    write(73,6001) title1,title2,title3,title4,title5,title6
    write(73,6002) name1,name2,name3,name4,name5,name6
    write(73,6003) place1,place2,place3,place4,place5,place6
    write(73,7020)
    write(73,7050)

    write(74,6001) title1,title2,title3,title4,title5,title6
    write(74,6002) name1,name2,name3,name4,name5,name6
    write(74,6003) place1,place2,place3,place4,place5,place6
    write(74,7020)
    write(74,7061)

    write(75,6001) title1,title2,title3,title4,title5,title6
    write(75,6002) name1,name2,name3,name4,name5,name6
    write(75,6003) place1,place2,place3,place4,place5,place6
    write(75,7020)
    write(75,7070)

    !write(76,6001) title1,title2,title3,title4,title5,title6
    !write(76,6002) name1,name2,name3,name4,name5,name6
    !write(76,6003) place1,place2,place3,place4,place5,place6
    !write(76,7020)
    !write(76,7080)

    if (dofire .or. lnduseon) then
        write(77,6001) title1,title2,title3,title4,title5,title6
        write(77,6002) name1,name2,name3,name4,name5,name6
        write(77,6003) place1,place2,place3,place4,place5,place6
        write(77,7021)
        write(77,7110)
        write(77,7111)
    end if

    write(711,6001) title1,title2,title3,title4,title5,title6
    write(711,6002) name1,name2,name3,name4,name5,name6
    write(711,6003) place1,place2,place3,place4,place5,place6
    write(711,7020)
    write(711,7030)

    if (compete .or. lnduseon) then
        write(78,6001) title1,title2,title3,title4,title5,title6
        write(78,6002) name1,name2,name3,name4,name5,name6
        write(78,6003) place1,place2,place3,place4,place5,place6
        write(78,7020)
        write(78,7075)
    end if

    ! methane(wetland) variables
    if (dowetlands .or. obswetf) then
        write(79,6001) title1,title2,title3,title4,title5,title6
        write(79,6002) name1,name2,name3,name4,name5,name6
        write(79,6003) place1,place2,place3,place4,place5,place6
        write(79,7020)
        write(79,7112)
        write(79,7113)
    end if

!A few formatting changes had to be made here in order to have the MESH makefile compatible,
!The lines were too long and had to add some "&"s and put some double lines

7010  FORMAT(A80)
7020  FORMAT('#CANADIAN TERRESTRIAL ECOSYSTEM MODEL (CTEM) DAILY RESULTS')
7021  FORMAT('#CANADIAN TERRESTRIAL ECOSYSTEM MODEL (CTEM) DAILY ',' DISTURBANCE RESULTS')
7030  FORMAT('HOUR MIN  DAY YEAR, An FOR 9 PFTs, RmL FOR 9 PFTs')
7040  FORMAT('  DAY YEAR       GPP       NPP       NEP       NBP', &
           '   AUTORES  HETRORES    LITRES    SOCRES  DSTCEMLS  LITRFALL', 'HUMIFTRS')            
7050  FORMAT('  DAY YEAR       RML       RMS       RMR        RG',&
           '  LEAFLITR  TLTRLEAF  TLTRSTEM  TLTRROOT ')
7060  FORMAT('  DAY YEAR  VGBIOMAS   GAVGLAI  GAVGLTMS  GAVGSCMS  ',&
           'TOTCMASS  GLEAFMAS   BLEAFMAS STEMMASS   ROOTMASS  LITRMASS ',' SOILCMAS')
7061  FORMAT('  DAY YEAR  VGBIOMAS   GAVGLAI  GLEAFMAS   BLEAFMAS ',&
    'STEMMASS   ROOTMASS  LITRMASS SOILCMAS')
7070  FORMAT('  DAY YEAR     AILCG     AILCB    RMATCTEM ',&
    'LAYER 1,2, & 3     VEGHGHT  ROOTDPTH  ROOTTEMP      SLAI')
7075  FORMAT('  DAY YEAR   FRAC #1   FRAC #2   FRAC #3   FRAC #4   ',&
    'FRAC #5   FRAC #6   FRAC #7   FRAC #8   FRAC #9  ','FRAC #10[%] SUMCHECK')
7080  FORMAT('  DAY YEAR   AFRLEAF   AFRSTEM   AFRROOT  TCANOACC','  LFSTATUS')
7110  FORMAT('  DAY YEAR   EMIT_CO2','    EMIT_CO   EMIT_CH4  EMIT_NMHC    EMIT_H2   EMIT_NOX',&
            '   EMIT_N2O  EMIT_PM25   EMIT_TPM    EMIT_TC    EMIT_OC',&
            '    EMIT_BC   BURNFRAC   SMFUNCVEG   LUCEMCOM   LUCLTRIN',&
            '   LUCSOCIN   GRCLAREA   BTERM   LTERM   MTERM')
7111  FORMAT('#               g/m2.D     g/m2.d','     g/m2.d     g/m2.d     g/m2.d     g/m2.d     g/m2.d',&
            '#     g/m2.d     g/m2.d     g/m2.d     g/m2.d     g/m2.d   ',&
    '       %  avgprob/d uMOL-CO2/M2.S KgC/M2.D','   KgC/M2.D      KM^2    prob/d       prob/d       prob/d')
7112  FORMAT(' DAY  YEAR   CH4WET1    CH4WET2    WETFDYN   CH4DYN1  CH4DYN2  SOILUP ')
7113  FORMAT('#          umolCH4/M2.S    umolCH4/M2.S          umolCH4/M2.S  umolCH4/M2.S  umolCH4/M2.S')

end if !>ctem_on & not parallelrun

!> CLASS MONTHLY FOR BOTH PARALLEL MODE AND STAND ALONE MODE

OPEN(UNIT=81,FILE=ARGBUFF(1:STRLEN(ARGBUFF))//'.OF1M')
WRITE(81,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
WRITE(81,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
WRITE(81,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
WRITE(81,6021)'MONTH','YEAR','SW','LW','QH','QE','SNOACC','WSNOACC','ROFACC','PCP',&
              'EVAP','TAIR','TRANSP','T/E','GROUNDEVAP','CANOPYEVAP','ALTOT'
WRITE(81,6021)'#','','W/m2','W/m2','W/m2','W/m2','kg/m2','kg/m2','mm.mon','mm.mon',&
              'mm.mon','degC','mm.mon','ratio','kg/m2/mon','kg/m2/mon',' '

OPEN(UNIT=82,FILE=ARGBUFF(1:STRLEN(ARGBUFF))//'.OF2M')
WRITE(82,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
WRITE(82,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
WRITE(82,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
WRITE(82,6022)'MONTH','YEAR','TG1','THL1','THI1','TG2','THL2','THI2','TG3','THL3','THI3'
WRITE(82,6022)'#','','deg','m3/m3','m3/m3','deg','m3/m3','m3/m3','deg','m3/m3','m3/m3'

!> CLASS YEARLY OUTPUT FILES

OPEN(UNIT=83,FILE=ARGBUFF(1:STRLEN(ARGBUFF))//'.OF1Y')
WRITE(83,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
WRITE(83,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
WRITE(83,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
WRITE(83,6023)'YEAR','SW','LW','QH','QE','ROFACC','PCP','EVAP','TRANSP','T/E','ALTOT'
WRITE(83,6023)'#','W/m2','W/m2','W/m2','W/m2','mm.yr','mm.yr','mm.yr','mm.yr','ratio',' '

if (ctem_on) then

    open(unit=84,file=argbuff(1:strlen(argbuff))//'.CT01M') !> CTEM monthly output files
    write(84,6001) title1,title2,title3,title4,title5,title6
    write(84,6002) name1,name2,name3,name4,name5,name6
    write(84,6003) place1,place2,place3,place4,place5,place6
    write(84,*)'#CANADIAN TERRESTRIAL ECOSYSTEM MODEL (CTEM) MONTHLY RESULTS'
    write(84,6124)'MONTH','YEAR','LAIMAXG','VGBIOMAS','LITTER','SOIL_C','NPP','GPP','NEP','NBP','HETRES',&
             'AUTORES','LITRES','SOILCRES','LITRFALL','HUMIFTRS'
    write(84,6124)'#','','m2/m2','Kg C/m2','Kg C/m2','Kg C/m2','gC/m2.mon','gC/m2.mon','gC/m2.mon',&
             'g/m2.mon','g/m2.mon','gC/m2.mon','gC/m2.mon','gC/m2.mon','gC/m2.mon','gC/m2.mon'
    
    if (dofire .or. lnduseon) then
        open(unit=85,file=argbuff(1:strlen(argbuff))//'.CT06M') !> Monthly disturbance
        write(85,6001) title1,title2,title3,title4,title5,title6
        write(85,6002) name1,name2,name3,name4,name5,name6
        write(85,6003) place1,place2,place3,place4,place5,place6
        write(85,*)'#CANADIAN TERRESTRIAL ECOSYSTEM MODEL (CTEM) MONTHLY RESULTS FOR DISTURBANCES'
        write(85,6125)'MONTH','YEAR','CO2','CO','CH4','NMHC','H2','NOX','N2O','PM25','TPM','TC','OC','BC',&
            'SMFUNCVEG','LUC_CO2_E','LUC_LTRIN','LUC_SOCIN','BURNFRAC','BTERM','LTERM','MTERM','WIND'
        write(85,6125)'#','','g/m2.mon','g/m2.mon','g/m2.mon','g/m2.mon','g/m2.mon','g/m2.mon','g/m2.mon', &
            'g/m2.mon','g/m2.mon','g/m2.mon','g/m2.mon','g/m2.mon','prob/mon','g C/m2','g C/m2','g C/m2', &
            '%','prob/mon','prob/mon','prob/mon','km/h'
    end if

    open(unit=86,file=argbuff(1:strlen(argbuff))//'.CT01Y') !> CTEM yearly output files
    write(86,6001) title1,title2,title3,title4,title5,title6
    write(86,6002) name1,name2,name3,name4,name5,name6
    write(86,6003) place1,place2,place3,place4,place5,place6
    write(86,*)'#CANADIAN TERRESTRIAL ECOSYSTEM MODEL (CTEM) YEARLY RESULTS'
    write(86,6126)'YEAR','LAIMAXG','VGBIOMAS','STEMMASS','ROOTMASS','LITRMASS','SOILCMAS','TOTCMASS', &
                  'ANNUALNPP','ANNUALGPP','ANNUALNEP','ANNUALNBP','ANNHETRSP','ANAUTORSP','ANNLITRES', &
                  'ANSOILCRES','VEGHGHT'
    write(86,6126)'#','m2/m2','Kg C/m2','Kg C/m2','Kg C/m2','Kg C/m2','Kg C/m2','Kg C/m2','gC/m2.yr',&
                  'gC/m2.yr','gC/m2.yr','gC/m2.yr','gC/m2.yr','gC/m2.yr','gC/m2.yr','gC/m2.yr','m'

    if (dofire .or. lnduseon) then
        open(unit=87,file=argbuff(1:strlen(argbuff))//'.CT06Y') !> Annual disturbance
        write(87,6001) title1,title2,title3,title4,title5,title6
        write(87,6002) name1,name2,name3,name4,name5,name6
        write(87,6003) place1,place2,place3,place4,place5,place6
        write(87,*)'#CANADIAN TERRESTRIAL ECOSYSTEM MODEL (CTEM) YEARLY RESULTS FOR DISTURBANCES'
        write(87,6127)'YEAR','ANNUALCO2','ANNUALCO','ANNUALCH4','ANN_NMHC','ANNUAL_H2','ANNUALNOX','ANNUALN2O',&
                      'ANN_PM25','ANNUALTPM','ANNUAL_TC','ANNUAL_OC','ANNUAL_BC','ASMFUNCVEG',&
                      'ANNLUCCO2','ANNLUCLTR','ANNLUCSOC','ABURNFRAC','ANNBTERM','ANNLTERM','ANNMTERM'
        write(87,6127)'#','g/m2.yr','g/m2.yr','g/m2.yr','g/m2.yr','g/m2.yr','g/m2.yr','g/m2.yr','g/m2.yr',&
                      'g/m2.yr','g/m2.yr','g/m2.yr','g/m2.yr','prob/yr ','  g/m2.yr','g/m2.yr','g/m2.yr',&
                      '%','prob/yr','prob/yr','prob/yr'
    end if

    if (compete .or. lnduseon) then

        open(unit=88,file=argbuff(1:strlen(argbuff))//'.CT07M')!> ctem pft fractions MONTHLY
        write(88,6001) title1,title2,title3,title4,title5,title6
        write(88,6002) name1,name2,name3,name4,name5,name6
        write(88,6003) place1,place2,place3,place4,place5,place6
        write(88,*)'#CANADIAN TERRESTRIAL ECOSYSTEM MODEL (CTEM) MONTHLY RESULTS'
        write(88,6128)'MONTH','YEAR','FRAC#1','FRAC#2','FRAC#3','FRAC#4','FRAC#5','FRAC#6','FRAC#7',&
                      'FRAC#8','FRAC#9','FRAC#10','SUMCHECK','PFT existence for each of the 9 pfts'
        write(88,6128)'#','','%','%','%','%','%','%','%','%','%','%','%'

        open(unit=89,file=argbuff(1:strlen(argbuff))//'.CT07Y')!> ctem pft fractions YEARLY
        write(89,6001) title1,title2,title3,title4,title5,title6
        write(89,6002) name1,name2,name3,name4,name5,name6
        write(89,6003) place1,place2,place3,place4,place5,place6
        write(89,*)'#CANADIAN TERRESTRIAL ECOSYSTEM MODEL (CTEM) YEARLY RESULTS'
        write(89,6129)'YEAR','FRAC#1','FRAC#2','FRAC#3','FRAC#4','FRAC#5','FRAC#6','FRAC#7',&
                      'FRAC#8','FRAC#9','FRAC#10','SUMCHECK','PFT existence for each of the 9 pfts'
        write(89,6129)'#','','%','%','%','%','%','%','%','%','%','%','%'

    end if !compete

    if (dowetlands .or. obswetf) then

        open(unit=91,file=argbuff(1:strlen(argbuff))//'.CT08M') !>Methane(wetland) MONTHLY
        write(91,6001) title1,title2,title3,title4,title5,title6
        write(91,6002) name1,name2,name3,name4,name5,name6
        write(91,6003) place1,place2,place3,place4,place5,place6
        write(91,*)'#CANADIAN TERRESTRIAL ECOSYSTEM MODEL (CTEM) MONTHLY RESULTS'
        write(91,6230)'MONTH','YEAR','CH4WET1','CH4WET2','WETFDYN','CH4DYN1','CH4DYN2','SOILUPTAKE'
        write(91,6230)'#','','gCH4/M2.MON','gCH4/M2.MON','fraction','gCH4/M2.MON','gCH4/M2.MON','gCH4/M2.MON'

        open(unit=92,file=argbuff(1:strlen(argbuff))//'.CT08Y')  !>Methane(wetland) YEARLY
        write(92,6001) title1,title2,title3,title4,title5,title6
        write(92,6002) name1,name2,name3,name4,name5,name6
        write(92,6003) place1,place2,place3,place4,place5,place6
        write(92,*)'#CANADIAN TERRESTRIAL ECOSYSTEM MODEL (CTEM) YEARLY RESULTS'
        write(92,6232)'YEAR','CH4WET1','CH4WET2','WETFDYN','CH4DYN1','CH4DYN2','SOILUPTAKE'
        write(92,6232)'#','gCH4/M2.YR','gCH4/M2.YR','fraction','gCH4/M2.YR','gCH4/M2.YR','gCH4/M2.YR'

    end if 
    
end if !>ctem_on & parallelrun

6021  FORMAT(A5,A5,5(A8,1X),A8,A12,8(A12,1X))
6022  FORMAT(A5,A5,3(A8,1X,2A6,1X))
6023  FORMAT(A5,4(A8,1X),A12,1X,5(A12,1X))
6124  FORMAT(A5,A5,14(A12,1X))
6125  FORMAT(A5,A5,21(A12,1X))
6126  FORMAT(1X,A5,16(A12,1X))
6127  FORMAT(1X,A5,20(A12,1X))
6128  FORMAT(A5,A5,11(A12,1X),45A)
6129  FORMAT(A5,11(A12,1X),45A)
6230  FORMAT(A5,A5,8(A12,1X))
6232  FORMAT(1X,A5,7(A12,1X))
 
end subroutine create_outfiles
!>@}
!==============================================================================================================
!>\ingroup io_driver_class_monthly_aw
!>@{
subroutine class_monthly_aw(IDAY,IYEAR,NCOUNT,NDAY,SBC,DELT,nltest,nmtest,&
                                  ALVSROT,FAREROT,FSVHROW,ALIRROT,FSIHROW,GTROT,FSSROW, &
                                  FDLROW,HFSROT,ROFROT,PREROW,QFSROT,QEVPROT,SNOROT, &
                                  TAROW,WSNOROT,TBARROT,THLQROT,THICROT,TFREZ,QFCROT, &
                                  QFGROT,QFNROT,QFCLROT,QFCFROT,FSGVROT,FSGSROT,FSGGROT)
                           
use ctem_statevars,     only : class_out,resetclassmon
use ctem_params, only : nmon, monthend, nlat, nmos, ignd

implicit none

! arguments
integer, intent(in) :: IDAY
integer, intent(in) :: IYEAR
integer, intent(in) :: NCOUNT
integer, intent(in) :: NDAY
integer, intent(in) :: nltest
integer, intent(in) :: nmtest
real, intent(in) :: SBC
real, intent(in) :: DELT
real, intent(in) :: TFREZ
real, dimension(nlat), intent(in) :: FSSROW
real, dimension(nlat), intent(in) :: FDLROW
real, dimension(nlat), intent(in) :: FSVHROW
real, dimension(nlat), intent(in) :: FSIHROW
real, dimension(nlat), intent(in) :: TAROW
real, dimension(nlat), intent(in) :: PREROW
real, dimension(nlat,nmos), intent(in) :: ALVSROT
real, dimension(nlat,nmos), intent(in) :: FAREROT
real, dimension(nlat,nmos), intent(in) :: ALIRROT
real, dimension(nlat,nmos), intent(in) :: GTROT
real, dimension(nlat,nmos), intent(in) :: HFSROT
real, dimension(nlat,nmos), intent(in) :: QEVPROT
real, dimension(nlat,nmos), intent(in) :: SNOROT
real, dimension(nlat,nmos), intent(in) :: WSNOROT
real, dimension(nlat,nmos), intent(in) :: ROFROT
real, dimension(nlat,nmos), intent(in) :: QFSROT
real, dimension(nlat,nmos,ignd), intent(in) :: TBARROT
real, dimension(nlat,nmos,ignd), intent(in) :: THLQROT
real, dimension(nlat,nmos,ignd), intent(in) :: THICROT
real, dimension(nlat,nmos,ignd), intent(in) :: QFCROT
real, dimension(nlat,nmos), intent(in) :: QFGROT
real, dimension(nlat,nmos), intent(in) :: QFNROT
real, dimension(nlat,nmos), intent(in) :: QFCLROT
real, dimension(nlat,nmos), intent(in) :: QFCFROT
real, dimension(nlat,nmos), intent(in) :: FSGVROT           !< Diagnosed net shortwave radiation on vegetation canopy
real, dimension(nlat,nmos), intent(in) :: FSGSROT           !< Diagnosed net shortwave radiation on ground snow surface
real, dimension(nlat,nmos), intent(in) :: FSGGROT           !< Diagnosed net shortwave radiation on ground surface

! pointers
real, pointer, dimension(:) :: ALVSACC_MO
real, pointer, dimension(:) :: ALIRACC_MO
real, pointer, dimension(:) :: FLUTACC_MO
real, pointer, dimension(:) :: FSINACC_MO
real, pointer, dimension(:) :: FLINACC_MO
real, pointer, dimension(:) :: HFSACC_MO
real, pointer, dimension(:) :: QEVPACC_MO
real, pointer, dimension(:) :: SNOACC_MO
real, pointer, dimension(:) :: WSNOACC_MO
real, pointer, dimension(:) :: ROFACC_MO
real, pointer, dimension(:) :: PREACC_MO
real, pointer, dimension(:) :: EVAPACC_MO
real, pointer, dimension(:) :: TRANSPACC_MO
real, pointer, dimension(:) :: TAACC_MO
real, pointer, dimension(:) :: ALTOTACC_MO
real, pointer, dimension(:) :: GROUNDEVAP
real, pointer, dimension(:) :: CANOPYEVAP
real, pointer :: FSSTAR_MO
real, pointer :: FLSTAR_MO
real, pointer :: QH_MO
real, pointer :: QE_MO
real, pointer, dimension(:,:) :: TBARACC_MO
real, pointer, dimension(:,:) :: THLQACC_MO
real, pointer, dimension(:,:) :: THICACC_MO
integer, pointer, dimension(:) :: altotcntr_m
   
! local

integer :: NT
integer :: NDMONTH
integer :: i,m,j
integer :: IMONTH
real :: tovere

! point pointers
ALVSACC_MO        => class_out%ALVSACC_MO  
ALIRACC_MO        => class_out%ALIRACC_MO
FLUTACC_MO        => class_out%FLUTACC_MO
FSINACC_MO        => class_out%FSINACC_MO
FLINACC_MO        => class_out%FLINACC_MO
HFSACC_MO         => class_out%HFSACC_MO
QEVPACC_MO        => class_out%QEVPACC_MO
SNOACC_MO         => class_out%SNOACC_MO
WSNOACC_MO        => class_out%WSNOACC_MO
ROFACC_MO         => class_out%ROFACC_MO
PREACC_MO         => class_out%PREACC_MO
EVAPACC_MO        => class_out%EVAPACC_MO
TRANSPACC_MO      => class_out%TRANSPACC_MO
TAACC_MO          => class_out%TAACC_MO
FSSTAR_MO         => class_out%FSSTAR_MO
FLSTAR_MO         => class_out%FLSTAR_MO
QH_MO             => class_out%QH_MO
QE_MO             => class_out%QE_MO  
TBARACC_MO        => class_out%TBARACC_MO
THLQACC_MO        => class_out%THLQACC_MO    
THICACC_MO        => class_out%THICACC_MO
GROUNDEVAP        => class_out%GROUNDEVAP
CANOPYEVAP        => class_out%CANOPYEVAP
ALTOTACC_MO       => class_out%ALTOTACC_MO
altotcntr_m       => class_out%altotcntr_m

! ------------

!> Accumulate output data for monthly averaged fields for class grid-mean.
!> for both parallel mode and stand alone mode

FSSTAR_MO   =0.0
FLSTAR_MO   =0.0
QH_MO       =0.0
QE_MO       =0.0

DO 820 I=1,NLTEST
DO 821 M=1,NMTEST

    ! These are presently not being outputted but the code is kept in place if the need arises.
!     ALVSACC_MO(I)=ALVSACC_MO(I)+ALVSROT(I,M)*FAREROT(I,M)*FSVHROW(I)
!     ALIRACC_MO(I)=ALIRACC_MO(I)+ALIRROT(I,M)*FAREROT(I,M)*FSIHROW(I)
    FLUTACC_MO(I)=FLUTACC_MO(I)+SBC*GTROT(I,M)**4*FAREROT(I,M)
    FSINACC_MO(I)=FSINACC_MO(I)+FSSROW(I)*FAREROT(I,M)
    FLINACC_MO(I)=FLINACC_MO(I)+FDLROW(I)*FAREROT(I,M)
    HFSACC_MO(I) =HFSACC_MO(I)+HFSROT(I,M)*FAREROT(I,M)
    QEVPACC_MO(I)=QEVPACC_MO(I)+QEVPROT(I,M)*FAREROT(I,M)
    SNOACC_MO(I) =SNOACC_MO(I)+SNOROT(I,M)*FAREROT(I,M)
    TAACC_MO(I)=TAACC_MO(I)+TAROW(I)*FAREROT(I,M)
    GROUNDEVAP(I)=GROUNDEVAP(I)+(QFGROT(I,M)+QFNROT(I,M))*FAREROT(I,M)*DELT !>ground evap includes both evap and sublimation from snow
    CANOPYEVAP(I)=CANOPYEVAP(I)+(QFCLROT(I,M)+QFCFROT(I,M))*FAREROT(I,M)*DELT !>canopy evap includes both evap and sublimation

    IF(SNOROT(I,M).GT.0.0) THEN
        WSNOACC_MO(I)=WSNOACC_MO(I)+WSNOROT(I,M)*FAREROT(I,M)
    ENDIF

    ROFACC_MO(I) =ROFACC_MO(I)+ROFROT(I,M)*FAREROT(I,M)*DELT
    PREACC_MO(I) =PREACC_MO(I)+PREROW(I)*FAREROT(I,M)*DELT
    EVAPACC_MO(I)=EVAPACC_MO(I)+QFSROT(I,M)*FAREROT(I,M)*DELT

    IF(FSSROW(I).GT.0.0) THEN
        ALTOTACC_MO(I)=ALTOTACC_MO(I) + ( (FSSROW(I)-(FSGVROT(I,M)+FSGSROT(I,M)+FSGGROT(I,M))) &
                        /FSSROW(I) )*FAREROT(I,M)
        altotcntr_m(i) = altotcntr_m(i) + 1
    ENDIF

    DO 823 J=1,IGND
        TBARACC_MO(I,J)=TBARACC_MO(I,J)+TBARROT(I,M,J)*FAREROT(I,M)
        THLQACC_MO(I,J)=THLQACC_MO(I,J)+THLQROT(I,M,J)*FAREROT(I,M)
        THICACC_MO(I,J)=THICACC_MO(I,J)+THICROT(I,M,J)*FAREROT(I,M)
        TRANSPACC_MO(I)=TRANSPACC_MO(I)+QFCROT(I,M,J)*FAREROT(I,M)*DELT
823 CONTINUE

821    CONTINUE
820   CONTINUE 

DO NT=1,NMON
    IF(IDAY.EQ.monthend(NT+1).AND.NCOUNT.EQ.NDAY)THEN
    
        IMONTH=NT
        NDMONTH=(monthend(NT+1)-monthend(NT))*NDAY

        DO 824 I=1,NLTEST

              ! These are presently not being outputted but the code is kept in place if the need arises.
!             IF(FSINACC_MO(I).GT.0.0) THEN
!                 ALVSACC_MO(I)=ALVSACC_MO(I)/(FSINACC_MO(I)*0.5)
!                 ALIRACC_MO(I)=ALIRACC_MO(I)/(FSINACC_MO(I)*0.5)
!             ELSE
!                 ALVSACC_MO(I)=0.0
!                 ALIRACC_MO(I)=0.0
!             ENDIF

            ! Albedo is only counted when sun is above horizon so it uses its own counter.\
            if (altotcntr_m(i) > 0) then
                ALTOTACC_MO(I) = ALTOTACC_MO(I)/REAL(altotcntr_m(i))
            else
                ALTOTACC_MO(I) = 0.
            end if

            FLUTACC_MO(I)=FLUTACC_MO(I)/REAL(NDMONTH)
            FSINACC_MO(I)=FSINACC_MO(I)/REAL(NDMONTH)
            FLINACC_MO(I)=FLINACC_MO(I)/REAL(NDMONTH)
            HFSACC_MO(I) =HFSACC_MO(I)/REAL(NDMONTH)
            QEVPACC_MO(I)=QEVPACC_MO(I)/REAL(NDMONTH)
            SNOACC_MO(I) =SNOACC_MO(I)/REAL(NDMONTH)
            WSNOACC_MO(I)=WSNOACC_MO(I)/REAL(NDMONTH)
            TAACC_MO(I)=TAACC_MO(I)/REAL(NDMONTH)
            ! The accumulated quantities don't change.
            !ROFACC_MO,PREACC_MO(I),EVAPACC_MO(I),TRANSPACC_MO(I),GROUNDEVAP,CANOPYEVAP
            DO J=1,IGND
                TBARACC_MO(I,J)=TBARACC_MO(I,J)/REAL(NDMONTH)
                THLQACC_MO(I,J)=THLQACC_MO(I,J)/REAL(NDMONTH)
                THICACC_MO(I,J)=THICACC_MO(I,J)/REAL(NDMONTH)
            ENDDO

            FSSTAR_MO=FSINACC_MO(I)*(1.-ALTOTACC_MO(I))
            FLSTAR_MO=FLINACC_MO(I)-FLUTACC_MO(I)
            QH_MO=HFSACC_MO(I)
            QE_MO=QEVPACC_MO(I)

            if (EVAPACC_MO(I) > 0.) then
                tovere = TRANSPACC_MO(I)/EVAPACC_MO(I)
            else
                tovere = 0.
            end if

            WRITE(81,8100)IMONTH,IYEAR,FSSTAR_MO,FLSTAR_MO,QH_MO, &
                         QE_MO,SNOACC_MO(I),WSNOACC_MO(I), &
                         ROFACC_MO(I),PREACC_MO(I),EVAPACC_MO(I), &
                         TAACC_MO(I)-TFREZ,TRANSPACC_MO(I),&
                         tovere,GROUNDEVAP(I),CANOPYEVAP(I),&
                         ALTOTACC_MO(I)
            IF (IGND.GT.3) THEN
            WRITE(82,8103)IMONTH,IYEAR,(TBARACC_MO(I,J)-TFREZ, &
                          THLQACC_MO(I,J),THICACC_MO(I,J),J=1,20) !, &
                        !,' TILE ',m
            ELSE
            WRITE(82,8102)IMONTH,IYEAR,(TBARACC_MO(I,J)-TFREZ, &
                          THLQACC_MO(I,J),THICACC_MO(I,J),J=1,3)! , &
                         ! ,' TILE ',m
            ENDIF   

          call resetclassmon(nltest)
          
 826      CONTINUE   

824     CONTINUE ! I
               
       END IF ! IF(IDAY.EQ.monthend(NT+1).AND.NCOUNT.EQ.NDAY)
      END DO ! NMON

8100  FORMAT(1X,I4,I5,5(F8.2,1X),F8.3,F12.4,8(E12.3,1X),2(A6,I2))
8101  FORMAT(1X,I4,I5,5(F7.2,1X,2F6.3,1X),2(A6,I2))
8103  FORMAT(1X,I4,I5,20(F7.2,1X,2F6.3,1X),2(A6,I2))
8102  FORMAT(1X,I4,I5,3(F8.2,1X,2F6.3,1X),2(A6,I2))


end subroutine class_monthly_aw
!>@}
!==============================================================================================================
!>\ingroup io_driver_class_annual_aw
!>@{
subroutine class_annual_aw(IDAY,IYEAR,NCOUNT,NDAY,SBC,DELT, &
                            nltest,nmtest,ALVSROT,FAREROT,FSVHROW, &
                            ALIRROT,FSIHROW,GTROT,FSSROW,FDLROW, &
                            HFSROT,ROFROT,PREROW,QFSROT,QEVPROT, &
                            TAROW,QFCROT,FSGVROT,FSGSROT,FSGGROT,&
                            leapnow)

use ctem_statevars,     only : class_out,resetclassyr
use ctem_params, only : nmon, monthend, nlat, nmos, ignd

implicit none

! arguments
integer, intent(in) :: IDAY
integer, intent(in) :: IYEAR
integer, intent(in) :: NCOUNT
integer, intent(in) :: NDAY
integer, intent(in) :: nltest
integer, intent(in) :: nmtest
logical, intent(in) :: leapnow                          !< true if this year is a leap year. Only used if the switch 'leap' is true.
real, intent(in) :: SBC
real, intent(in) :: DELT
real, dimension(nlat), intent(in) :: FSSROW
real, dimension(nlat), intent(in) :: FDLROW
real, dimension(nlat), intent(in) :: FSVHROW
real, dimension(nlat), intent(in) :: FSIHROW
real, dimension(nlat), intent(in) :: TAROW
real, dimension(nlat), intent(in) :: PREROW
real, dimension(nlat,nmos), intent(in) :: ALVSROT
real, dimension(nlat,nmos), intent(in) :: FAREROT
real, dimension(nlat,nmos), intent(in) :: ALIRROT
real, dimension(nlat,nmos), intent(in) :: GTROT
real, dimension(nlat,nmos), intent(in) :: HFSROT
real, dimension(nlat,nmos), intent(in) :: QEVPROT
real, dimension(nlat,nmos), intent(in) :: ROFROT
real, dimension(nlat,nmos), intent(in) :: QFSROT
real, dimension(nlat,nmos,ignd), intent(in) :: QFCROT
real, dimension(nlat,nmos), intent(in) :: FSGVROT           !< Diagnosed net shortwave radiation on vegetation canopy
real, dimension(nlat,nmos), intent(in) :: FSGSROT           !< Diagnosed net shortwave radiation on ground snow surface
real, dimension(nlat,nmos), intent(in) :: FSGGROT           !< Diagnosed net shortwave radiation on ground surface
integer, pointer, dimension(:) :: altotcntr_yr

! pointers
real, pointer, dimension(:) :: ALVSACC_YR
real, pointer, dimension(:) :: ALIRACC_YR
real, pointer, dimension(:) :: FLUTACC_YR
real, pointer, dimension(:) :: FSINACC_YR
real, pointer, dimension(:) :: FLINACC_YR
real, pointer, dimension(:) :: HFSACC_YR
real, pointer, dimension(:) :: QEVPACC_YR
real, pointer, dimension(:) :: ROFACC_YR
real, pointer, dimension(:) :: PREACC_YR
real, pointer, dimension(:) :: EVAPACC_YR
real, pointer, dimension(:) :: TRANSPACC_YR
real, pointer, dimension(:) :: TAACC_YR
real, pointer, dimension(:) :: ALTOTACC_YR
real, pointer :: FSSTAR_YR
real, pointer :: FLSTAR_YR
real, pointer :: QH_YR
real, pointer :: QE_YR

!local
integer :: i,m,j
real :: ALTOT_YR
real :: tovere
real :: daysinyr

!point pointers
ALVSACC_YR        => class_out%ALVSACC_YR
ALIRACC_YR        => class_out%ALIRACC_YR
FLUTACC_YR        => class_out%FLUTACC_YR
FSINACC_YR        => class_out%FSINACC_YR
FLINACC_YR        => class_out%FLINACC_YR
HFSACC_YR         => class_out%HFSACC_YR
QEVPACC_YR        => class_out%QEVPACC_YR
ROFACC_YR         => class_out%ROFACC_YR
PREACC_YR         => class_out%PREACC_YR
EVAPACC_YR        => class_out%EVAPACC_YR
TRANSPACC_YR      => class_out%TRANSPACC_YR
TAACC_YR          => class_out%TAACC_YR
FSSTAR_YR         => class_out%FSSTAR_YR
FLSTAR_YR         => class_out%FLSTAR_YR
QH_YR             => class_out%QH_YR
QE_YR             => class_out%QE_YR
ALTOTACC_YR       => class_out%ALTOTACC_YR
altotcntr_yr      => class_out%altotcntr_yr

!> Accumulate output data for yearly averaged fields for class grid-mean.
!> for both parallel mode and stand alone mode
FSSTAR_YR   =0.0
FLSTAR_YR   =0.0
QH_YR       =0.0
QE_YR       =0.0
ALTOT_YR    =0.0

DO 827 I=1,NLTEST
    DO 828 M=1,NMTEST

          ! These are presently not being outputted but the code is kept in place if the need arises.
!         ALVSACC_YR(I)=ALVSACC_YR(I)+ALVSROT(I,M)*FAREROT(I,M)*FSVHROW(I)
!         ALIRACC_YR(I)=ALIRACC_YR(I)+ALIRROT(I,M)*FAREROT(I,M)*FSIHROW(I)

        FLUTACC_YR(I)=FLUTACC_YR(I)+SBC*GTROT(I,M)**4*FAREROT(I,M)
        FSINACC_YR(I)=FSINACC_YR(I)+FSSROW(I)*FAREROT(I,M)
        FLINACC_YR(I)=FLINACC_YR(I)+FDLROW(I)*FAREROT(I,M)
        HFSACC_YR(I) =HFSACC_YR(I)+HFSROT(I,M)*FAREROT(I,M)
        QEVPACC_YR(I)=QEVPACC_YR(I)+QEVPROT(I,M)*FAREROT(I,M)
        TAACC_YR(I)=TAACC_YR(I)+TAROW(I)*FAREROT(I,M)
        ROFACC_YR(I) =ROFACC_YR(I)+ROFROT(I,M)*FAREROT(I,M)*DELT
        PREACC_YR(I) =PREACC_YR(I)+PREROW(I)*FAREROT(I,M)*DELT
        EVAPACC_YR(I)=EVAPACC_YR(I)+QFSROT(I,M)*FAREROT(I,M)*DELT
        DO J = 1,IGND
            TRANSPACC_YR(I)=TRANSPACC_YR(I)+QFCROT(I,M,J)*FAREROT(I,M)*DELT
        END DO
        IF(FSSROW(I).GT.0.0) THEN
           ALTOTACC_YR(I)=ALTOTACC_YR(I) + ((FSSROW(I)-(FSGVROT(I,M)+FSGSROT(I,M)+FSGGROT(I,M))) &
                        /FSSROW(I) )*FAREROT(I,M)
           altotcntr_yr(i) = altotcntr_yr(i) + 1
        ENDIF

828    CONTINUE
827   CONTINUE

IF ((.not.leapnow .AND.IDAY.EQ.365.AND.NCOUNT.EQ.NDAY) .OR. & 
    (leapnow .AND.IDAY.EQ.366.AND.NCOUNT.EQ.NDAY)) THEN 


    DO 829 I=1,NLTEST

            ! These are presently not being outputted but the code is kept in place if the need arises.
!             IF(FSINACC_YR(I).GT.0.0) THEN
!                 ALVSACC_YR(I)=ALVSACC_YR(I)/(FSINACC_YR(I)*0.5)
!                 ALIRACC_YR(I)=ALIRACC_YR(I)/(FSINACC_YR(I)*0.5)
!             ELSE
!                 ALVSACC_YR(I)=0.0
!                 ALIRACC_YR(I)=0.0
!             ENDIF

            if (leapnow) then
                daysinyr=366.
            else
                daysinyr=365.
            end if

            FLUTACC_YR(I)=FLUTACC_YR(I)/(REAL(NDAY)*daysinyr)
            FSINACC_YR(I)=FSINACC_YR(I)/(REAL(NDAY)*daysinyr)
            FLINACC_YR(I)=FLINACC_YR(I)/(REAL(NDAY)*daysinyr)
            HFSACC_YR(I) =HFSACC_YR(I)/(REAL(NDAY)*daysinyr)
            QEVPACC_YR(I)=QEVPACC_YR(I)/(REAL(NDAY)*daysinyr)
            ROFACC_YR(I) =ROFACC_YR(I)
            PREACC_YR(I) =PREACC_YR(I)
            EVAPACC_YR(I)=EVAPACC_YR(I)
            TRANSPACC_YR(I)=TRANSPACC_YR(I)
            TAACC_YR(I)=TAACC_YR(I)/(REAL(NDAY)*daysinyr)

            ! Albedo is only counted when sun is above horizon so it uses its own counter.
            if (altotcntr_yr(i) > 0) then
                ALTOTACC_YR(I)=ALTOTACC_YR(I)/(REAL(altotcntr_yr(i)))
            else
                ALTOTACC_YR(I)= 0.
            end if

            FSSTAR_YR=FSINACC_YR(I)*(1.-ALTOTACC_YR(I))
            FLSTAR_YR=FLINACC_YR(I)-FLUTACC_YR(I)
            QH_YR=HFSACC_YR(I)
            QE_YR=QEVPACC_YR(I)

            if (EVAPACC_YR(I) > 0.) then
                tovere = TRANSPACC_YR(I)/EVAPACC_YR(I)
            else
                tovere = 0.
            end if

            WRITE(83,8103)IYEAR,FSSTAR_YR,FLSTAR_YR,QH_YR,&
                          QE_YR,ROFACC_YR(I),PREACC_YR(I),&
                          EVAPACC_YR(I),TRANSPACC_YR(I),&
                          tovere,ALTOTACC_YR(I)

        !> ADD INITIALIZTION FOR YEARLY ACCUMULATED ARRAYS

        call resetclassyr(nltest)

829 CONTINUE ! I

ENDIF !> IDAY.EQ.365/366 .AND. NDAY

8103  FORMAT(1X,I5,4(F8.2,1X),F12.4,1X,5(F12.3,1X),2(A5,I1))

end subroutine class_annual_aw
!>@}
!==============================================================================================================
!>\ingroup io_driver_ctem_daily_aw
!>@{

subroutine ctem_daily_aw(nltest,nmtest,iday,FAREROT,iyear,jdstd,jdsty,jdendd,jdendy,grclarea,onetile_perPFT)

! J. Melton Feb 2016.

use ctem_statevars,     only : ctem_tile, vrot, c_switch, &
                               resetdaily, ctem_grd
use ctem_params, only : icc,ignd,nmos,iccp1,wtCH4,seed

implicit none

! arguments
integer, intent(in) :: nltest
integer, intent(in) :: nmtest
integer, intent(in) :: iday
real, intent(in), dimension(:,:) :: FAREROT
integer, intent(in) :: iyear
integer, intent(in) :: jdstd
integer, intent(in) :: jdsty
integer, intent(in) :: jdendd
integer, intent(in) :: jdendy
real, intent(in), dimension(:) :: grclarea
logical, intent(in) :: onetile_perPFT

! pointers

logical, pointer :: dofire
logical, pointer :: lnduseon
logical, pointer :: compete
logical, pointer :: dowetlands
logical, pointer :: obswetf
real, pointer, dimension(:,:,:) :: fcancmxrow
real, pointer, dimension(:,:,:) :: gppvegrow
real, pointer, dimension(:,:,:) :: nepvegrow
real, pointer, dimension(:,:,:) :: nbpvegrow
real, pointer, dimension(:,:,:) :: nppvegrow
real, pointer, dimension(:,:,:) :: hetroresvegrow
real, pointer, dimension(:,:,:) :: autoresvegrow
real, pointer, dimension(:,:,:) :: litresvegrow
real, pointer, dimension(:,:,:) :: soilcresvegrow
real, pointer, dimension(:,:,:) :: rmlvegaccrow
real, pointer, dimension(:,:,:) :: rmsvegrow
real, pointer, dimension(:,:,:) :: rmrvegrow
real, pointer, dimension(:,:,:) :: rgvegrow
real, pointer, dimension(:,:,:) :: ailcgrow
real, pointer, dimension(:,:,:) :: emit_co2row
real, pointer, dimension(:,:,:) :: emit_corow
real, pointer, dimension(:,:,:) :: emit_ch4row
real, pointer, dimension(:,:,:) :: emit_nmhcrow
real, pointer, dimension(:,:,:) :: emit_h2row
real, pointer, dimension(:,:,:) :: emit_noxrow
real, pointer, dimension(:,:,:) :: emit_n2orow
real, pointer, dimension(:,:,:) :: emit_pm25row
real, pointer, dimension(:,:,:) :: emit_tpmrow
real, pointer, dimension(:,:,:) :: emit_tcrow
real, pointer, dimension(:,:,:) :: emit_ocrow
real, pointer, dimension(:,:,:) :: emit_bcrow
real, pointer, dimension(:,:) :: burnfracrow
real, pointer, dimension(:,:,:) :: burnvegfrow
real, pointer, dimension(:,:,:) :: smfuncvegrow
real, pointer, dimension(:,:,:) :: btermrow
real, pointer, dimension(:,:) :: ltermrow
real, pointer, dimension(:,:,:) :: mtermrow
real, pointer, dimension(:,:) :: lucemcomrow
real, pointer, dimension(:,:) :: lucltrinrow
real, pointer, dimension(:,:) :: lucsocinrow
real, pointer, dimension(:,:) :: ch4wet1row
real, pointer, dimension(:,:) :: ch4wet2row
real, pointer, dimension(:,:) :: wetfdynrow
real, pointer, dimension(:,:) :: ch4dyn1row
real, pointer, dimension(:,:) :: ch4dyn2row
real, pointer, dimension(:,:) :: ch4soillsrow
real, pointer, dimension(:,:,:) :: litrmassrow
real, pointer, dimension(:,:,:) :: soilcmasrow
real, pointer, dimension(:,:,:) :: vgbiomas_vegrow
real, pointer, dimension(:,:,:) :: stemmassrow        
real, pointer, dimension(:,:,:) :: rootmassrow        
real, pointer, dimension(:,:,:) :: gleafmasrow        !
real, pointer, dimension(:,:,:) :: bleafmasrow        !
real, pointer, dimension(:,:) :: gavglairow
real, pointer, dimension(:,:,:) :: slairow
real, pointer, dimension(:,:,:) :: ailcbrow
real, pointer, dimension(:,:,:) :: flhrlossrow
real, pointer, dimension(:,:) :: dstcemls3row
integer, pointer, dimension(:,:,:) :: lfstatusrow
real, pointer, dimension(:,:) :: vgbiomasrow
real, pointer, dimension(:,:) :: gavgltmsrow
real, pointer, dimension(:,:) :: gavgscmsrow
      
real, pointer, dimension(:,:) :: leaflitr_t
real, pointer, dimension(:,:) :: tltrleaf_t
real, pointer, dimension(:,:) :: tltrstem_t
real, pointer, dimension(:,:) :: tltrroot_t
real, pointer, dimension(:,:) :: ailcg_t
real, pointer, dimension(:,:) :: ailcb_t
real, pointer, dimension(:,:,:) :: rmatctem_t
real, pointer, dimension(:,:) :: veghght_t
real, pointer, dimension(:,:) :: rootdpth_t
real, pointer, dimension(:,:) :: roottemp_t
real, pointer, dimension(:,:) :: slai_t
real, pointer, dimension(:,:) :: afrroot_t
real, pointer, dimension(:,:) :: afrleaf_t
real, pointer, dimension(:,:) :: afrstem_t
real, pointer, dimension(:,:) :: laimaxg_t
real, pointer, dimension(:,:) :: stemmass_t
real, pointer, dimension(:,:) :: rootmass_t
real, pointer, dimension(:,:) :: litrmass_t
real, pointer, dimension(:,:) :: gleafmas_t
real, pointer, dimension(:,:) :: bleafmas_t
real, pointer, dimension(:,:) :: soilcmas_t
real, pointer, dimension(:,:) :: emit_co2_t
real, pointer, dimension(:,:) :: emit_co_t
real, pointer, dimension(:,:) :: emit_ch4_t
real, pointer, dimension(:,:) :: emit_nmhc_t
real, pointer, dimension(:,:) :: emit_h2_t
real, pointer, dimension(:,:) :: emit_nox_t
real, pointer, dimension(:,:) :: emit_n2o_t
real, pointer, dimension(:,:) :: emit_pm25_t
real, pointer, dimension(:,:) :: emit_tpm_t
real, pointer, dimension(:,:) :: emit_tc_t
real, pointer, dimension(:,:) :: emit_oc_t
real, pointer, dimension(:,:) :: emit_bc_t
real, pointer, dimension(:,:) :: bterm_t
real, pointer, dimension(:,:) :: mterm_t
real, pointer, dimension(:,:) :: smfuncveg_t
real, pointer, dimension(:,:) :: tcanoaccrow_out

real, pointer, dimension(:,:) :: npprow
real, pointer, dimension(:,:) :: neprow
real, pointer, dimension(:,:) :: nbprow
real, pointer, dimension(:,:) :: gpprow
real, pointer, dimension(:,:) :: hetroresrow
real, pointer, dimension(:,:) :: autoresrow
real, pointer, dimension(:,:) :: soilcresprow
real, pointer, dimension(:,:) :: rgrow
real, pointer, dimension(:,:) :: litresrow
real, pointer, dimension(:,:) :: socresrow

real, pointer, dimension(:) :: gpp_g
real, pointer, dimension(:) :: npp_g
real, pointer, dimension(:) :: nbp_g
real, pointer, dimension(:) :: autores_g
real, pointer, dimension(:) :: socres_g
real, pointer, dimension(:) :: litres_g
real, pointer, dimension(:) :: dstcemls3_g
real, pointer, dimension(:) :: litrfall_g
real, pointer, dimension(:) :: rml_g
real, pointer, dimension(:) :: rms_g      
real, pointer, dimension(:) :: rg_g
real, pointer, dimension(:) :: leaflitr_g
real, pointer, dimension(:) :: tltrstem_g
real, pointer, dimension(:) :: tltrroot_g
real, pointer, dimension(:) :: nep_g
real, pointer, dimension(:) :: hetrores_g
real, pointer, dimension(:) :: dstcemls_g
real, pointer, dimension(:) :: humiftrs_g
real, pointer, dimension(:) :: rmr_g
real, pointer, dimension(:) :: tltrleaf_g
real, pointer, dimension(:) :: gavgltms_g
real, pointer, dimension(:) :: vgbiomas_g
real, pointer, dimension(:) :: gavglai_g
real, pointer, dimension(:) :: gavgscms_g
real, pointer, dimension(:) :: gleafmas_g
real, pointer, dimension(:) :: bleafmas_g
real, pointer, dimension(:) :: stemmass_g
real, pointer, dimension(:) :: rootmass_g
real, pointer, dimension(:) :: litrmass_g
real, pointer, dimension(:) :: soilcmas_g
real, pointer, dimension(:) :: slai_g
real, pointer, dimension(:) :: ailcg_g
real, pointer, dimension(:) :: ailcb_g
real, pointer, dimension(:) :: veghght_g
real, pointer, dimension(:) :: rootdpth_g
real, pointer, dimension(:) :: roottemp_g
real, pointer, dimension(:) :: totcmass_g
real, pointer, dimension(:) :: tcanoacc_out_g
real, pointer, dimension(:) :: burnfrac_g
real, pointer, dimension(:) :: smfuncveg_g
real, pointer, dimension(:) :: lucemcom_g
real, pointer, dimension(:) :: lucltrin_g
real, pointer, dimension(:) :: lucsocin_g
real, pointer, dimension(:) :: emit_co2_g
real, pointer, dimension(:) :: emit_co_g
real, pointer, dimension(:) :: emit_ch4_g
real, pointer, dimension(:) :: emit_nmhc_g
real, pointer, dimension(:) :: emit_h2_g
real, pointer, dimension(:) :: emit_nox_g
real, pointer, dimension(:) :: emit_n2o_g
real, pointer, dimension(:) :: emit_pm25_g
real, pointer, dimension(:) :: emit_tpm_g
real, pointer, dimension(:) :: emit_tc_g
real, pointer, dimension(:) :: emit_oc_g
real, pointer, dimension(:) :: emit_bc_g
real, pointer, dimension(:) :: bterm_g
real, pointer, dimension(:) :: lterm_g
real, pointer, dimension(:) :: mterm_g
real, pointer, dimension(:) :: ch4wet1_g
real, pointer, dimension(:) :: ch4wet2_g
real, pointer, dimension(:) :: wetfdyn_g
real, pointer, dimension(:) :: ch4dyn1_g
real, pointer, dimension(:) :: ch4dyn2_g
real, pointer, dimension(:) :: ch4soills_g
real, pointer, dimension(:,:) :: afrleaf_g
real, pointer, dimension(:,:) :: afrstem_g     
real, pointer, dimension(:,:) :: afrroot_g
real, pointer, dimension(:,:) :: lfstatus_g
real, pointer, dimension(:,:) :: rmlvegrow_g
real, pointer, dimension(:,:) :: anvegrow_g
real, pointer, dimension(:,:) :: rmatctem_g

real, pointer, dimension(:,:,:) :: bmasvegrow
real, pointer, dimension(:,:,:) :: cmasvegcrow
real, pointer, dimension(:,:,:) :: veghghtrow
real, pointer, dimension(:,:,:) :: rootdpthrow
real, pointer, dimension(:,:) :: rmlrow
real, pointer, dimension(:,:) :: rmsrow
real, pointer, dimension(:,:,:) :: tltrleafrow
real, pointer, dimension(:,:,:) :: tltrstemrow
real, pointer, dimension(:,:,:) :: tltrrootrow
real, pointer, dimension(:,:,:) :: leaflitrrow
real, pointer, dimension(:,:,:) :: roottemprow
real, pointer, dimension(:,:,:) :: afrleafrow
real, pointer, dimension(:,:,:) :: afrstemrow
real, pointer, dimension(:,:,:) :: afrrootrow
real, pointer, dimension(:,:,:) :: wtstatusrow
real, pointer, dimension(:,:,:) :: ltstatusrow
real, pointer, dimension(:,:) :: rmrrow

real, pointer, dimension(:,:,:,:) :: rmatctemrow
real, pointer, dimension(:,:) :: dstcemlsrow
real, pointer, dimension(:,:) :: litrfallrow
real, pointer, dimension(:,:) :: humiftrsrow
   
! local
integer :: i,m,j,nt,k
real :: barefrac
real :: sumfare

! point pointers

dofire                => c_switch%dofire
lnduseon              => c_switch%lnduseon
compete               => c_switch%compete
dowetlands            => c_switch%dowetlands
obswetf               => c_switch%obswetf

fcancmxrow        => vrot%fcancmx
gppvegrow         => vrot%gppveg
nepvegrow         => vrot%nepveg
nbpvegrow         => vrot%nbpveg
nppvegrow         => vrot%nppveg
hetroresvegrow    => vrot%hetroresveg
autoresvegrow     => vrot%autoresveg   
litresvegrow      => vrot%litresveg
soilcresvegrow    => vrot%soilcresveg
rmlvegaccrow      => vrot%rmlvegacc
rmsvegrow         => vrot%rmsveg
rmrvegrow         => vrot%rmrveg
rgvegrow          => vrot%rgveg
ailcgrow          => vrot%ailcg
emit_co2row       => vrot%emit_co2
emit_corow        => vrot%emit_co
emit_ch4row       => vrot%emit_ch4
emit_nmhcrow      => vrot%emit_nmhc
emit_h2row        => vrot%emit_h2
emit_noxrow       => vrot%emit_nox
emit_n2orow       => vrot%emit_n2o
emit_pm25row      => vrot%emit_pm25
emit_tpmrow       => vrot%emit_tpm
emit_tcrow        => vrot%emit_tc
emit_ocrow        => vrot%emit_oc
emit_bcrow        => vrot%emit_bc
burnfracrow       => vrot%burnfrac
burnvegfrow       => vrot%burnvegf
smfuncvegrow      => vrot%smfuncveg
btermrow          => vrot%bterm
ltermrow          => vrot%lterm
mtermrow          => vrot%mterm
lucemcomrow       => vrot%lucemcom
lucltrinrow       => vrot%lucltrin
lucsocinrow       => vrot%lucsocin
ch4wet1row        => vrot%ch4wet1
ch4wet2row        => vrot%ch4wet2
wetfdynrow        => vrot%wetfdyn
ch4dyn1row        => vrot%ch4dyn1
ch4dyn2row        => vrot%ch4dyn2
ch4soillsrow      => vrot%ch4_soills
litrmassrow       => vrot%litrmass
soilcmasrow       => vrot%soilcmas
vgbiomas_vegrow   => vrot%vgbiomas_veg
stemmassrow       => vrot%stemmass
rootmassrow       => vrot%rootmass
flhrlossrow       => vrot%flhrloss
dstcemls3row      => vrot%dstcemls3
lfstatusrow       => vrot%lfstatus
tcanoaccrow_out   => vrot%tcanoaccrow_out
npprow            => vrot%npp
neprow            => vrot%nep
nbprow            => vrot%nbp
gpprow            => vrot%gpp
hetroresrow       => vrot%hetrores
autoresrow        => vrot%autores
soilcresprow      => vrot%soilcresp
rgrow             => vrot%rg
litresrow         => vrot%litres
socresrow         => vrot%socres
vgbiomasrow       => vrot%vgbiomas
gavgltmsrow       => vrot%gavgltms
gavgscmsrow       => vrot%gavgscms
bmasvegrow        => vrot%bmasveg
cmasvegcrow       => vrot%cmasvegc
veghghtrow        => vrot%veghght
rootdpthrow       => vrot%rootdpth
rmlrow            => vrot%rml
rmsrow            => vrot%rms
tltrleafrow       => vrot%tltrleaf
tltrstemrow       => vrot%tltrstem
tltrrootrow       => vrot%tltrroot
leaflitrrow       => vrot%leaflitr
roottemprow       => vrot%roottemp
afrleafrow        => vrot%afrleaf
afrstemrow        => vrot%afrstem
afrrootrow        => vrot%afrroot
wtstatusrow       => vrot%wtstatus
ltstatusrow       => vrot%ltstatus
rmrrow            => vrot%rmr
gleafmasrow       => vrot%gleafmas
bleafmasrow       => vrot%bleafmas
gavglairow        => vrot%gavglai
slairow           => vrot%slai
ailcbrow          => vrot%ailcb
flhrlossrow       => vrot%flhrloss
rmatctemrow       => vrot%rmatctem
dstcemlsrow       => vrot%dstcemls
litrfallrow       => vrot%litrfall
humiftrsrow       => vrot%humiftrs

leaflitr_t        => ctem_tile%leaflitr_t
tltrleaf_t        => ctem_tile%tltrleaf_t
tltrstem_t        => ctem_tile%tltrstem_t
tltrroot_t        => ctem_tile%tltrroot_t
ailcg_t           => ctem_tile%ailcg_t
ailcb_t           => ctem_tile%ailcb_t
rmatctem_t        => ctem_tile%rmatctem_t
veghght_t         => ctem_tile%veghght_t
rootdpth_t        => ctem_tile%rootdpth_t
roottemp_t        => ctem_tile%roottemp_t
slai_t            => ctem_tile%slai_t
afrroot_t         => ctem_tile%afrroot_t
afrleaf_t         => ctem_tile%afrleaf_t
afrstem_t         => ctem_tile%afrstem_t
laimaxg_t         => ctem_tile%laimaxg_t
stemmass_t        => ctem_tile%stemmass_t
rootmass_t        => ctem_tile%rootmass_t
litrmass_t        => ctem_tile%litrmass_t
gleafmas_t        => ctem_tile%gleafmas_t
bleafmas_t        => ctem_tile%bleafmas_t
soilcmas_t        => ctem_tile%soilcmas_t
emit_co2_t        => ctem_tile%emit_co2_t
emit_co_t         => ctem_tile%emit_co_t
emit_ch4_t        => ctem_tile%emit_ch4_t
emit_nmhc_t       => ctem_tile%emit_nmhc_t
emit_h2_t         => ctem_tile%emit_h2_t
emit_nox_t        => ctem_tile%emit_nox_t
emit_n2o_t        => ctem_tile%emit_n2o_t
emit_pm25_t       => ctem_tile%emit_pm25_t
emit_tpm_t        => ctem_tile%emit_tpm_t
emit_tc_t         => ctem_tile%emit_tc_t
emit_oc_t         => ctem_tile%emit_oc_t
emit_bc_t         => ctem_tile%emit_bc_t
bterm_t           => ctem_tile%bterm_t
mterm_t           => ctem_tile%mterm_t
smfuncveg_t       => ctem_tile%smfuncveg_t

gpp_g             => ctem_grd%gpp_g
npp_g             => ctem_grd%npp_g
nbp_g             => ctem_grd%nbp_g
autores_g         => ctem_grd%autores_g
socres_g          => ctem_grd%socres_g
litres_g          => ctem_grd%litres_g
dstcemls3_g       => ctem_grd%dstcemls3_g 
litrfall_g        => ctem_grd%litrfall_g
rml_g             => ctem_grd%rml_g
rms_g             => ctem_grd%rms_g    
rg_g              => ctem_grd%rg_g    
leaflitr_g        => ctem_grd%leaflitr_g
tltrstem_g        => ctem_grd%tltrstem_g
tltrroot_g        => ctem_grd%tltrroot_g
nep_g             => ctem_grd%nep_g
hetrores_g        => ctem_grd%hetrores_g
dstcemls_g        => ctem_grd%dstcemls_g
humiftrs_g        => ctem_grd%humiftrs_g
rmr_g             => ctem_grd%rmr_g
tltrleaf_g        => ctem_grd%tltrleaf_g
gavgltms_g        => ctem_grd%gavgltms_g
vgbiomas_g        => ctem_grd%vgbiomas_g
gavglai_g         => ctem_grd%gavglai_g
gavgscms_g        => ctem_grd%gavgscms_g
gleafmas_g        => ctem_grd%gleafmas_g
bleafmas_g        => ctem_grd%bleafmas_g
stemmass_g        => ctem_grd%stemmass_g
rootmass_g        => ctem_grd%rootmass_g
litrmass_g        => ctem_grd%litrmass_g
soilcmas_g        => ctem_grd%soilcmas_g
slai_g            => ctem_grd%slai_g
ailcg_g           => ctem_grd%ailcg_g
ailcb_g           => ctem_grd%ailcb_g
veghght_g         => ctem_grd%veghght_g
rootdpth_g        => ctem_grd%rootdpth_g  
roottemp_g        => ctem_grd%roottemp_g
totcmass_g        => ctem_grd%totcmass_g
tcanoacc_out_g    => ctem_grd%tcanoacc_out_g
burnfrac_g        => ctem_grd%burnfrac_g
smfuncveg_g       => ctem_grd%smfuncveg_g
lucemcom_g        => ctem_grd%lucemcom_g
lucltrin_g        => ctem_grd%lucltrin_g
lucsocin_g        => ctem_grd%lucsocin_g
emit_co2_g        => ctem_grd%emit_co2_g
emit_co_g         => ctem_grd%emit_co_g
emit_ch4_g        => ctem_grd%emit_ch4_g
emit_nmhc_g       => ctem_grd%emit_nmhc_g
emit_h2_g         => ctem_grd%emit_h2_g
emit_nox_g        => ctem_grd%emit_nox_g
emit_n2o_g        => ctem_grd%emit_n2o_g
emit_pm25_g       => ctem_grd%emit_pm25_g
emit_tpm_g        => ctem_grd%emit_tpm_g
emit_tc_g         => ctem_grd%emit_tc_g
emit_oc_g         => ctem_grd%emit_oc_g
emit_bc_g         => ctem_grd%emit_bc_g
bterm_g           => ctem_grd%bterm_g
lterm_g           => ctem_grd%lterm_g
mterm_g           => ctem_grd%mterm_g
ch4wet1_g         => ctem_grd%ch4wet1_g
ch4wet2_g         => ctem_grd%ch4wet2_g
wetfdyn_g         => ctem_grd%wetfdyn_g
ch4dyn1_g         => ctem_grd%ch4dyn1_g
ch4dyn2_g         => ctem_grd%ch4dyn2_g
ch4soills_g       => ctem_grd%ch4_soills_g
afrleaf_g         => ctem_grd%afrleaf_g
afrstem_g         => ctem_grd%afrstem_g   
afrroot_g         => ctem_grd%afrroot_g
lfstatus_g        => ctem_grd%lfstatus_g
rmlvegrow_g       => ctem_grd%rmlvegrow_g
anvegrow_g        => ctem_grd%anvegrow_g
rmatctem_g        => ctem_grd%rmatctem_g    

!       ---------------------------------------------------------

!>write daily ctem results
if ((iyear .ge. jdsty).and.(iyear.le.jdendy))then
  if ((iday .ge. jdstd).and.(iday .le.jdendd))then

    !>Reset the grid and tile average variables.
    call resetdaily(nltest,nmtest)

    !>First some unit conversions:

    do 10 i = 1,1 !Set to 1 for now but must change once we have good .CTM file
      do 20 m = 1 , nmtest
        do 30 j=1,icc
            if (fcancmxrow(i,m,j) .gt.0.0) then

                gppvegrow(i,m,j)=gppvegrow(i,m,j)*1.0377 ! convert to gc/m2.day
                nppvegrow(i,m,j)=nppvegrow(i,m,j)*1.0377 ! convert to gc/m2.day
                nepvegrow(i,m,j)=nepvegrow(i,m,j)*1.0377 ! convert to gc/m2.day
                nbpvegrow(i,m,j)=nbpvegrow(i,m,j)*1.0377 ! convert to gc/m2.day
                hetroresvegrow(i,m,j)=hetroresvegrow(i,m,j)*1.0377 ! convert to gc/m2.day
                autoresvegrow(i,m,j)=autoresvegrow(i,m,j)*1.0377 ! convert to gc/m2.day
                litresvegrow(i,m,j)=litresvegrow(i,m,j)*1.0377 ! convert to gc/m2.day
                soilcresvegrow(i,m,j)=soilcresvegrow(i,m,j)*1.0377 ! convert to gc/m2.day

            end if

30      continue ! icc

        !>Now for the bare fraction of the grid cell.
        hetroresvegrow(i,m,iccp1)=hetroresvegrow(i,m,iccp1)*1.0377 ! convert to gc/m2.day
        litresvegrow(i,m,iccp1)=litresvegrow(i,m,iccp1)*1.0377 ! convert to gc/m2.day
        soilcresvegrow(i,m,iccp1)=soilcresvegrow(i,m,iccp1)*1.0377 ! convert to gc/m2.day
        nepvegrow(i,m,iccp1)=nepvegrow(i,m,iccp1)*1.0377 ! convert to gc/m2.day
        nbpvegrow(i,m,iccp1)=nbpvegrow(i,m,iccp1)*1.0377 ! convert to gc/m2.day

        npprow(i,m)     =npprow(i,m)*1.0377 ! convert to gc/m2.day
        gpprow(i,m)     =gpprow(i,m)*1.0377 ! convert to gc/m2.day
        neprow(i,m)     =neprow(i,m)*1.0377 ! convert to gc/m2.day
        nbprow(i,m)     =nbprow(i,m)*1.0377 ! convert to gc/m2.day
        lucemcomrow(i,m)=lucemcomrow(i,m)*1.0377 ! convert to gc/m2.day
        lucltrinrow(i,m)=lucltrinrow(i,m)*1.0377 ! convert to gc/m2.day
        lucsocinrow(i,m)=lucsocinrow(i,m)*1.0377 ! convert to gc/m2.day
        hetroresrow(i,m)=hetroresrow(i,m)*1.0377 ! convert to gc/m2.day
        autoresrow(i,m) =autoresrow(i,m)*1.0377  ! convert to gc/m2.day
        litresrow(i,m)  =litresrow(i,m)*1.0377   ! convert to gc/m2.day
        socresrow(i,m)  =socresrow(i,m)*1.0377   ! convert to gc/m2.day
        ch4wet1row(i,m) = ch4wet1row(i,m)*1.0377 * wtCH4 / 12. ! convert from umolch4/m2/s to gch4/m2.day
        ch4wet2row(i,m) = ch4wet2row(i,m)*1.0377 * wtCH4 / 12. ! convert from umolch4/m2/s to gch4/m2.day
        ch4dyn1row(i,m) = ch4dyn1row(i,m)*1.0377 * wtCH4 / 12. ! convert from umolch4/m2/s to gch4/m2.day
        ch4dyn2row(i,m) = ch4dyn2row(i,m)*1.0377 * wtCH4 / 12. ! convert from umolch4/m2/s to gch4/m2.day
        ch4soillsrow(i,m) = ch4soillsrow(i,m)*1.0377 * wtCH4 / 12. ! convert from umolch4/m2/s to gch4/m2.day

20   continue
10 continue


    !>Aggregate to the tile avg vars:
    do 60 i=1,nltest
      do 70 m=1,nmtest
        barefrac = 1.0
        do j=1,icc
            barefrac = barefrac - fcancmxrow(i,m,j)
            leaflitr_t(i,m)=leaflitr_t(i,m)+leaflitrrow(i,m,j)*fcancmxrow(i,m,j)
            tltrleaf_t(i,m)=tltrleaf_t(i,m)+tltrleafrow(i,m,j)*fcancmxrow(i,m,j)
            tltrstem_t(i,m)=tltrstem_t(i,m)+tltrstemrow(i,m,j)*fcancmxrow(i,m,j)
            tltrroot_t(i,m)=tltrroot_t(i,m)+tltrrootrow(i,m,j)*fcancmxrow(i,m,j)
            veghght_t(i,m)=veghght_t(i,m)+veghghtrow(i,m,j)*fcancmxrow(i,m,j)
            rootdpth_t(i,m)=rootdpth_t(i,m)+rootdpthrow(i,m,j)*fcancmxrow(i,m,j)
            roottemp_t(i,m)=roottemp_t(i,m)+roottemprow(i,m,j)*fcancmxrow(i,m,j)
            slai_t(i,m)=slai_t(i,m)+slairow(i,m,j)*fcancmxrow(i,m,j)
            afrleaf_t(i,m)=afrleaf_t(i,m)+afrleafrow(i,m,j)*fcancmxrow(i,m,j)
            afrstem_t(i,m)=afrstem_t(i,m)+afrstemrow(i,m,j)*fcancmxrow(i,m,j)
            afrroot_t(i,m)=afrroot_t(i,m)+afrrootrow(i,m,j)*fcancmxrow(i,m,j)
            ailcg_t(i,m)=ailcg_t(i,m)+ailcgrow(i,m,j)*fcancmxrow(i,m,j)
            ailcb_t(i,m)=ailcb_t(i,m)+ailcbrow(i,m,j)*fcancmxrow(i,m,j)
            gleafmas_t(i,m) = gleafmas_t(i,m) + gleafmasrow(i,m,j)*fcancmxrow(i,m,j)
            bleafmas_t(i,m) = bleafmas_t(i,m) + bleafmasrow(i,m,j)*fcancmxrow(i,m,j)
            stemmass_t(i,m) = stemmass_t(i,m) + stemmassrow(i,m,j)*fcancmxrow(i,m,j)
            rootmass_t(i,m) = rootmass_t(i,m) + rootmassrow(i,m,j)*fcancmxrow(i,m,j)
            litrmass_t(i,m) = litrmass_t(i,m) + litrmassrow(i,m,j)*fcancmxrow(i,m,j)
            soilcmas_t(i,m) = soilcmas_t(i,m) + soilcmasrow(i,m,j)*fcancmxrow(i,m,j)
            emit_co2_t(i,m) =emit_co2_t(i,m)+ emit_co2row(i,m,j)*fcancmxrow(i,m,j)
            emit_co_t(i,m)  =emit_co_t(i,m) + emit_corow(i,m,j)*fcancmxrow(i,m,j)
            emit_ch4_t(i,m) =emit_ch4_t(i,m)+ emit_ch4row(i,m,j)*fcancmxrow(i,m,j)
            emit_nmhc_t(i,m)=emit_nmhc_t(i,m)+emit_nmhcrow(i,m,j)*fcancmxrow(i,m,j)
            emit_h2_t(i,m)  =emit_h2_t(i,m) + emit_h2row(i,m,j)*fcancmxrow(i,m,j)
            emit_nox_t(i,m) =emit_nox_t(i,m)+ emit_noxrow(i,m,j)*fcancmxrow(i,m,j)
            emit_n2o_t(i,m) =emit_n2o_t(i,m)+ emit_n2orow(i,m,j)*fcancmxrow(i,m,j)
            emit_pm25_t(i,m)=emit_pm25_t(i,m)+emit_pm25row(i,m,j)*fcancmxrow(i,m,j)
            emit_tpm_t(i,m) =emit_tpm_t(i,m)+ emit_tpmrow(i,m,j)*fcancmxrow(i,m,j)
            emit_tc_t(i,m)  =emit_tc_t(i,m) + emit_tcrow(i,m,j)*fcancmxrow(i,m,j)
            emit_oc_t(i,m)  =emit_oc_t(i,m) + emit_ocrow(i,m,j)*fcancmxrow(i,m,j)
            emit_bc_t(i,m)  =emit_bc_t(i,m) + emit_bcrow(i,m,j)*fcancmxrow(i,m,j)
            bterm_t(i,m)  =bterm_t(i,m) + btermrow(i,m,j)*fcancmxrow(i,m,j)
            mterm_t(i,m)  =mterm_t(i,m) + mtermrow(i,m,j)*fcancmxrow(i,m,j)
            smfuncveg_t(i,m) = smfuncveg_t(i,m) + smfuncvegrow(i,m,j)*fcancmxrow(i,m,j)

            do k=1,ignd
            rmatctem_t(i,m,k)=rmatctem_t(i,m,k)+rmatctemrow(i,m,j,k)*fcancmxrow(i,m,j)
            enddo
        enddo !icc

        !>Do the bare ground also:
        litrmass_t(i,m) = litrmass_t(i,m) + litrmassrow(i,m,iccp1)*barefrac
        soilcmas_t(i,m) = soilcmas_t(i,m) + soilcmasrow(i,m,iccp1)*barefrac

    !>Calculation of grid averaged variables

        gpp_g(i) =gpp_g(i) + gpprow(i,m)*FAREROT(i,m)
        npp_g(i) =npp_g(i) + npprow(i,m)*FAREROT(i,m)
        nep_g(i) =nep_g(i) + neprow(i,m)*FAREROT(i,m)
        nbp_g(i) =nbp_g(i) + nbprow(i,m)*FAREROT(i,m)
        autores_g(i) =autores_g(i) +autoresrow(i,m)*FAREROT(i,m)
        hetrores_g(i)=hetrores_g(i)+hetroresrow(i,m)*FAREROT(i,m)
        litres_g(i) =litres_g(i) + litresrow(i,m)*FAREROT(i,m)
        socres_g(i) =socres_g(i) + socresrow(i,m)*FAREROT(i,m)
        dstcemls_g(i)=dstcemls_g(i)+dstcemlsrow(i,m)*FAREROT(i,m)
        dstcemls3_g(i)=dstcemls3_g(i)+dstcemls3row(i,m)*FAREROT(i,m)
        litrfall_g(i)=litrfall_g(i)+litrfallrow(i,m)*FAREROT(i,m)
        humiftrs_g(i)=humiftrs_g(i)+humiftrsrow(i,m)*FAREROT(i,m)
        rml_g(i) =rml_g(i) + rmlrow(i,m)*FAREROT(i,m)
        rms_g(i) =rms_g(i) + rmsrow(i,m)*FAREROT(i,m)
        rmr_g(i) =rmr_g(i) + rmrrow(i,m)*FAREROT(i,m)
        rg_g(i) =rg_g(i) + rgrow(i,m)*FAREROT(i,m)
        leaflitr_g(i) = leaflitr_g(i) + leaflitr_t(i,m)*FAREROT(i,m)
        tltrleaf_g(i) = tltrleaf_g(i) + tltrleaf_t(i,m)*FAREROT(i,m)
        tltrstem_g(i) = tltrstem_g(i) + tltrstem_t(i,m)*FAREROT(i,m)
        tltrroot_g(i) = tltrroot_g(i) + tltrroot_t(i,m)*FAREROT(i,m)
        slai_g(i) = slai_g(i) + slai_t(i,m)*FAREROT(i,m)
        ailcg_g(i)=ailcg_g(i)+ailcg_t(i,m)*FAREROT(i,m)
        ailcb_g(i)=ailcb_g(i)+ailcb_t(i,m)*FAREROT(i,m)
        vgbiomas_g(i) =vgbiomas_g(i) + vgbiomasrow(i,m)*FAREROT(i,m)
        veghght_g(i) = veghght_g(i) + veghght_t(i,m)*FAREROT(i,m)
        gavglai_g(i) =gavglai_g(i) + gavglairow(i,m)*FAREROT(i,m)
        gavgltms_g(i) =gavgltms_g(i) + gavgltmsrow(i,m)*FAREROT(i,m)
        gavgscms_g(i) =gavgscms_g(i) + gavgscmsrow(i,m)*FAREROT(i,m)
        tcanoacc_out_g(i) =tcanoacc_out_g(i)+tcanoaccrow_out(i,m)*FAREROT(i,m)
        totcmass_g(i) =vgbiomas_g(i) + gavgltms_g(i) + gavgscms_g(i)
        gleafmas_g(i) = gleafmas_g(i) + gleafmas_t(i,m)*FAREROT(i,m)
        bleafmas_g(i) = bleafmas_g(i) + bleafmas_t(i,m)*FAREROT(i,m)
        stemmass_g(i) = stemmass_g(i) + stemmass_t(i,m)*FAREROT(i,m)
        rootmass_g(i) = rootmass_g(i) + rootmass_t(i,m)*FAREROT(i,m)
        rootdpth_g(i) = rootdpth_g(i) + rootdpth_t(i,m)*FAREROT(i,m)
        roottemp_g(i) = roottemp_g(i) + roottemp_t(i,m)*FAREROT(i,m)
        litrmass_g(i) = litrmass_g(i) + litrmass_t(i,m)*FAREROT(i,m)
        soilcmas_g(i) = soilcmas_g(i) + soilcmas_t(i,m)*FAREROT(i,m)
        burnfrac_g(i) =burnfrac_g(i)+ burnfracrow(i,m)*FAREROT(i,m)
        smfuncveg_g(i) =smfuncveg_g(i)+smfuncveg_t(i,m)*FAREROT(i,m)
        lucemcom_g(i) =lucemcom_g(i)+lucemcomrow(i,m)*FAREROT(i,m)
        lucltrin_g(i) =lucltrin_g(i)+lucltrinrow(i,m)*FAREROT(i,m)
        lucsocin_g(i) =lucsocin_g(i)+lucsocinrow(i,m)*FAREROT(i,m)
        bterm_g(i)    =bterm_g(i)   +bterm_t(i,m)*FAREROT(i,m)
        lterm_g(i)    =lterm_g(i)   +ltermrow(i,m)*FAREROT(i,m)
        mterm_g(i)    =mterm_g(i)   +mterm_t(i,m)*FAREROT(i,m)
        ch4wet1_g(i) = ch4wet1_g(i) + ch4wet1row(i,m)*farerot(i,m)
        ch4wet2_g(i) = ch4wet2_g(i) + ch4wet2row(i,m)*farerot(i,m)
        wetfdyn_g(i) = wetfdyn_g(i) + wetfdynrow(i,m)*farerot(i,m)
        ch4dyn1_g(i) = ch4dyn1_g(i) + ch4dyn1row(i,m)*farerot(i,m)
        ch4dyn2_g(i) = ch4dyn2_g(i) + ch4dyn2row(i,m)*farerot(i,m)
        ch4soills_g(i) = ch4soills_g(i) + ch4soillsrow(i,m)*farerot(i,m)
        emit_co2_g(i) =emit_co2_g(i)+ emit_co2_t(i,m)*FAREROT(i,m)
        emit_co_g(i)  =emit_co_g(i) + emit_co_t(i,m)*FAREROT(i,m)
        emit_ch4_g(i) =emit_ch4_g(i)+ emit_ch4_t(i,m)*FAREROT(i,m)
        emit_nmhc_g(i)=emit_nmhc_g(i)+emit_nmhc_t(i,m)*FAREROT(i,m)
        emit_h2_g(i)  =emit_h2_g(i) + emit_h2_t(i,m)*FAREROT(i,m)
        emit_nox_g(i) =emit_nox_g(i)+ emit_nox_t(i,m)*FAREROT(i,m)
        emit_n2o_g(i) =emit_n2o_g(i)+ emit_n2o_t(i,m)*FAREROT(i,m)
        emit_pm25_g(i)=emit_pm25_g(i)+emit_pm25_t(i,m)*FAREROT(i,m)
        emit_tpm_g(i) =emit_tpm_g(i)+ emit_tpm_t(i,m)*FAREROT(i,m)
        emit_tc_g(i)  =emit_tc_g(i) + emit_tc_t(i,m)*FAREROT(i,m)
        emit_oc_g(i)  =emit_oc_g(i) + emit_oc_t(i,m)*FAREROT(i,m)
        emit_bc_g(i)  =emit_bc_g(i) + emit_bc_t(i,m)*FAREROT(i,m)

        do k=1,ignd
            rmatctem_g(i,k)=rmatctem_g(i,k)+rmatctem_t(i,m,k)*FAREROT(i,m)
        end do

70 continue !nmtest
60 continue !nltest

!>Write daily ctem results

do 80 i=1,nltest
   do 90 m=1,nmtest

        barefrac = 1.0

  !>First the per PFT values to file .CT01D
        do j=1,icc

            if (fcancmxrow(i,m,j) .gt. seed) then

                barefrac = barefrac - fcancmxrow(i,m,j)

                !>File: .CT01D
                write(72,8200)iday,iyear,gppvegrow(i,m,j),nppvegrow(i,m,j), &
                nepvegrow(i,m,j),nbpvegrow(i,m,j),autoresvegrow(i,m,j), &
                hetroresvegrow(i,m,j),litresvegrow(i,m,j),soilcresvegrow(i,m,j), &
                (dstcemlsrow(i,m)+dstcemls3row(i,m)), &   ! FLAG at present dstcemls are only per tile values
                litrfallrow(i,m),humiftrsrow(i,m), & ! same with litrfall and humiftrs.
                ' TILE ',m,' PFT ',j,' FRAC ',fcancmxrow(i,m,j)

                !>File .CT02D
                write(73,8300)iday,iyear,rmlvegaccrow(i,m,j), &
                rmsvegrow(i,m,j),rmrvegrow(i,m,j),rgvegrow(i,m,j), &
                leaflitrrow(i,m,j),tltrleafrow(i,m,j), &
                tltrstemrow(i,m,j),tltrrootrow(i,m,j), &
                ' TILE ',m,' PFT ',j,' FRAC ',fcancmxrow(i,m,j)

                !>File *.CT03D
                write(74,8401)iday,iyear,vgbiomas_vegrow(i,m,j), &
                ailcgrow(i,m,j),gleafmasrow(i,m,j), &
                bleafmasrow(i,m,j), stemmassrow(i,m,j), &
                rootmassrow(i,m,j), litrmassrow(i,m,j),  &
                soilcmasrow(i,m,j), &
                ' TILE ',m,' PFT ',j,' FRAC ',fcancmxrow(i,m,j)

                !>File .CT04D
                write(75,8500)iday,iyear, ailcgrow(i,m,j),  &
                ailcbrow(i,m,j),(rmatctemrow(i,m,j,k),k=1,3), &
                veghghtrow(i,m,j),rootdpthrow(i,m,j), &
                roottemprow(i,m,j),slairow(i,m,j), &
                ' TILE ',m,' PFT ',j,' FRAC ',fcancmxrow(i,m,j)

                ! File .CT05D
                !write(76,8600)iday,iyear, afrleafrow(i,m,j),  &
                !afrstemrow(i,m,j),afrrootrow(i,m,j),  &
                !tcanoaccrow_out(i,m), lfstatusrow(i,m,j), &
                !' TILE ',m,' PFT ',j,' FRAC ',fcancmxrow(i,m,j)

                !>File *.CT06D
                if (dofire .or. lnduseon) then
                    write(77,8800)iday,iyear, &
                    emit_co2row(i,m,j),emit_corow(i,m,j),emit_ch4row(i,m,j), &
                    emit_nmhcrow(i,m,j),emit_h2row(i,m,j),emit_noxrow(i,m,j), &
                    emit_n2orow(i,m,j),emit_pm25row(i,m,j), &
                    emit_tpmrow(i,m,j),emit_tcrow(i,m,j),emit_ocrow(i,m,j), &
                    emit_bcrow(i,m,j),burnvegfrow(i,m,j)*100., &
                    smfuncvegrow(i,m,j),lucemcom_g(i), &  !FLAG only per grid values for these last ones.
                    lucltrin_g(i), lucsocin_g(i), &
                    grclarea(i), btermrow(i,m,j), lterm_g(i), mtermrow(i,m,j), &
                    ' TILE ',m,' PFT ',j,' FRAC ',fcancmxrow(i,m,j)
                endif

            end if !fcancmx

        end do !icc

        !>Now write out the bare fraction values ( only needed if you have vars that are affected by barefrac values)
        if (barefrac .gt. seed) then

            !>File: .CT01D
            write(72,8200)iday,iyear,0.0,0.0, &
            nepvegrow(i,m,iccp1),nbpvegrow(i,m,iccp1),0.0, &
            hetroresvegrow(i,m,iccp1),litresvegrow(i,m,iccp1),soilcresvegrow(i,m,iccp1), &
            (dstcemlsrow(i,m)+dstcemls3row(i,m)), &   ! FLAG at present dstcemls are only per tile values
            litrfallrow(i,m),humiftrsrow(i,m), & ! same with litrfall and humiftrs.
            ' TILE ',m,' PFT ',iccp1,' FRAC ',barefrac

            !>File *.CT03D
            write(74,8401)iday,iyear,0.0, &
            0.0,0.0, &
            0.0, 0.0, &
            0.0, litrmassrow(i,m,iccp1),  &
            soilcmasrow(i,m,iccp1), &
            ' TILE ',m,' PFT ',iccp1,' FRAC ',barefrac

        end if

        !>Now write out the tile average values for each tile if the tile number
        !>is greater than 1 (nmtest > 1).
        if (nmtest > 1) then

            !>File: .CT01D
            write(72,8200)iday,iyear,gpprow(i,m),npprow(i,m), &
                neprow(i,m),nbprow(i,m),autoresrow(i,m), &
                hetroresrow(i,m),litresrow(i,m),socresrow(i,m), &
                (dstcemlsrow(i,m)+dstcemls3row(i,m)), &
                litrfallrow(i,m),humiftrsrow(i,m), &
                ' TILE ',m,' OF ',nmtest,' TFRAC ',FAREROT(i,m)

            !>File .CT02D
            write(73,8300)iday,iyear,rmlrow(i,m),rmsrow(i,m), &
                rmrrow(i,m),rgrow(i,m),leaflitr_t(i,m),tltrleaf_t(i,m), &
                tltrstem_t(i,m),tltrroot_t(i,m), &
                ' TILE ',m,' OF ',nmtest,' TFRAC ',FAREROT(i,m)

            !>File .CT03D
            write(74,8401)iday,iyear,vgbiomasrow(i,m), &
                ailcg_t(i,m), gleafmas_t(i,m), &
                bleafmas_t(i,m), stemmass_t(i,m), &
                rootmass_t(i,m), litrmass_t(i,m), &
                soilcmas_t(i,m),&
                ' TILE ',m,' OF ',nmtest,' TFRAC ',FAREROT(i,m)

            !>File .CT04D
            write(75,8500)iday,iyear,ailcg_t(i,m), &
                ailcb_t(i,m),(rmatctem_t(i,m,k),k=1,3), &
                veghght_t(i,m),rootdpth_t(i,m), &
                roottemp_t(i,m),slai_t(i,m), &
                ' TILE ',m,' OF ',nmtest,' TFRAC ',FAREROT(i,m)

            ! File .CT05D
            !write(76,8601)iday,iyear, afrleaf_t(i,m), &
            !    afrstem_t(i,m),afrroot_t(i,m),  &
            !    tcanoaccrow_out(i,m), -999,   & ! lfstatus is kinda meaningless grid avg so set to -999
            !    ' TILE ',m,' OF ',nmtest,' TFRAC ',FAREROT(i,m)

            if (dofire .or. lnduseon) then
                write(77,8800)iday,iyear,  &
                    emit_co2_t(i,m), emit_co_t(i,m), emit_ch4_t(i,m), &
                    emit_nmhc_t(i,m), emit_h2_t(i,m), emit_nox_t(i,m), &
                    emit_n2o_t(i,m), emit_pm25_t(i,m), emit_tpm_t(i,m), &
                    emit_tc_t(i,m), emit_oc_t(i,m), emit_bc_t(i,m), &
                    burnfrac_g(i)*100., smfuncveg_t(i,m),lucemcom_g(i), & !FLAG only per grid values for these last ones.
                    lucltrin_g(i), lucsocin_g(i), &
                    grclarea(i), bterm_t(i,m), lterm_g(i), mterm_t(i,m), &
                    ' TILE ',m,' OF ',nmtest,' TFRAC ',FAREROT(i,m)
            endif

        end if !nmtest>1

90  continue !nmtest

    !>Finally do the grid avg values:

    !>File: .CT01D
    write(72,8200)iday,iyear,gpp_g(i),npp_g(i), &
            nep_g(i),nbp_g(i),autores_g(i), &
            hetrores_g(i),litres_g(i),socres_g(i), &
            (dstcemls_g(i)+dstcemls3_g(i)), &
            litrfall_g(i),humiftrs_g(i),' GRDAV'

    !>File .CT02D
    write(73,8300)iday,iyear,rml_g(i),rms_g(i), &
            rmr_g(i),rg_g(i),leaflitr_g(i),tltrleaf_g(i), &
            tltrstem_g(i),tltrroot_g(i),' GRDAV'

    !>File .CT03D
    write(74,8401)iday,iyear,vgbiomas_g(i), &
            gavglai_g(i), &
            gleafmas_g(i), bleafmas_g(i), stemmass_g(i), &
            rootmass_g(i), litrmass_g(i), soilcmas_g(i),' GRDAV'

    !>File .CT04D
    write(75,8500)iday,iyear, ailcg_g(i),  &
            ailcb_g(i),(rmatctem_g(i,k),k=1,3), &
            veghght_g(i),rootdpth_g(i),roottemp_g(i),&
            slai_g(i),' GRDAV'

    ! File .CT05D
    !write(76,8601)iday,iyear, afrleaf_t(i,m), &
    !    afrstem_t(i,m),afrroot_t(i,m),  &
    !    tcanoaccrow_out(i,m), -999,   & ! lfstatus is kinda meaningless grid avg so set to -999
    !    ' GRDAV'

    !>File *.CT06D
    if (dofire .or. lnduseon) then
        write(77,8800)iday,iyear,  &
            emit_co2_g(i), emit_co_g(i), emit_ch4_g(i), &
            emit_nmhc_g(i), emit_h2_g(i), emit_nox_g(i), &
            emit_n2o_g(i), emit_pm25_g(i), emit_tpm_g(i), &
            emit_tc_g(i), emit_oc_g(i), emit_bc_g(i), &
            burnfrac_g(i)*100., smfuncveg_g(i),lucemcom_g(i), &
            lucltrin_g(i), lucsocin_g(i), &
            grclarea(i), bterm_g(i), lterm_g(i), mterm_g(i),' GRDAV'
    endif


    !>File .CT08D
    if (dowetlands .or. obswetf) then
    write(79,8810)iday,iyear, ch4wet1_g(i),  &
                 ch4wet2_g(i), wetfdyn_g(i),  &
                 ch4dyn1_g(i), ch4dyn2_g(i),  &
                 ch4soills_g(i),' GRDAV'
    endif

    if (compete .or. lnduseon) then
        sumfare=0.0
        if (onetile_perPFT) then
            do m=1,nmos
                sumfare=sumfare+FAREROT(i,m)
            enddo
            write(78,8200)iday,iyear,(FAREROT(i,m)*100.,m=1,nmos),sumfare
        else !composite
            do m=1,nmos
                sumfare=0.0
                do j=1,icc  !m = 1
                    sumfare=sumfare+fcancmxrow(i,m,j)
                enddo !j
                write(78,8200)iday,iyear,(fcancmxrow(i,m,j)*100.,j=1,icc),(1.0-sumfare)*100.,sumfare,' TILE ',m
            end do !m
        endif !mosaic/composite
    endif !compete/lnduseon

80 continue ! nltest

  end if !if write daily
endif !if write daily

8200       format(1x,i4,i5,11f10.5,2(a6,i2),a8,f5.3)
8201       format(1x,i4,i5,3f10.5,80x,2(a6,i2),a8,f5.3)
8300       format(1x,i4,i5,8f10.5,2(a6,i2),a8,f5.3)
8301       format(1x,i4,i5,4f10.5,40x,2(a6,i2),a8,f5.3)
!8400       format(1x,i4,i5,11f10.5,2(a6,i2),a6,f5.3)
8401       format(1x,i4,i5,2f10.5,6f10.5,2(a6,i2),a8,f5.3)
!8402       format(1x,i4,i5,10f10.5,2(a6,i2),a6,f5.3)
! 8402       format(1x,i4,i5,2f10.5,40x,2f10.5,2(a6,i2))
8500       format(1x,i4,i5,9f10.5,2(a6,i2),a8,f5.3)
8600       format(1x,i4,i5,4f10.5,i8,2(a6,i2),a8,f5.3)
8601       format(1x,i4,i5,4f10.5,8x,2(a6,i2),a8,f5.3)
8800       format(1x,i4,i5,20f11.4,2x,f9.2,2(a6,i2),a8,f5.3)
8810       format(1x,i4,i5,6f11.4,2(a6,i2),a8,f5.3)

end subroutine ctem_daily_aw
!>@}
!==============================================================================================================
!>\ingroup io_driver_ctem_monthly_aw
!>@{

subroutine ctem_monthly_aw(nltest,nmtest,iday,FAREROT,iyear,nday,onetile_perPFT)

! J. Melton Feb 2016.

use ctem_statevars,     only : ctem_tile_mo, vrot, ctem_grd_mo, c_switch, &
                                resetmonthend,ctem_mo
use ctem_params, only : icc,iccp1,nmon,mmday,monthend,monthdays,seed,nmos,nlat

implicit none

! arguments
integer, intent(in) :: nltest
integer, intent(in) :: nmtest
integer, intent(in) :: iday
real, intent(in), dimension(:,:) :: FAREROT
integer, intent(in) :: iyear
integer, intent(in) :: nday
logical, intent(in) :: onetile_perPFT

! pointers

logical, pointer :: dofire
logical, pointer :: lnduseon
logical, pointer :: compete
logical, pointer :: dowetlands
logical, pointer :: obswetf

real, pointer, dimension(:,:,:) :: fcancmxrow
real, pointer, dimension(:,:,:) :: laimaxg_mo
real, pointer, dimension(:,:,:) :: stemmass_mo
real, pointer, dimension(:,:,:) :: rootmass_mo
real, pointer, dimension(:,:,:) :: litrfallveg_mo
real, pointer, dimension(:,:,:) :: humiftrsveg_mo
real, pointer, dimension(:,:,:) :: npp_mo
real, pointer, dimension(:,:,:) :: gpp_mo
real, pointer, dimension(:,:,:) :: vgbiomas_mo
real, pointer, dimension(:,:,:) :: autores_mo
real, pointer, dimension(:,:,:) :: totcmass_mo
real, pointer, dimension(:,:,:) :: litrmass_mo
real, pointer, dimension(:,:,:) :: soilcmas_mo
real, pointer, dimension(:,:,:) :: nep_mo
real, pointer, dimension(:,:,:) :: litres_mo
real, pointer, dimension(:,:,:) :: soilcres_mo
real, pointer, dimension(:,:,:) :: hetrores_mo
real, pointer, dimension(:,:,:) :: nbp_mo
real, pointer, dimension(:,:,:) :: emit_co2_mo
real, pointer, dimension(:,:,:) :: emit_co_mo
real, pointer, dimension(:,:,:) :: emit_ch4_mo
real, pointer, dimension(:,:,:) :: emit_nmhc_mo
real, pointer, dimension(:,:,:) :: emit_h2_mo
real, pointer, dimension(:,:,:) :: emit_nox_mo
real, pointer, dimension(:,:,:) :: emit_n2o_mo
real, pointer, dimension(:,:,:) :: emit_pm25_mo
real, pointer, dimension(:,:,:) :: emit_tpm_mo
real, pointer, dimension(:,:,:) :: emit_tc_mo
real, pointer, dimension(:,:,:) :: emit_oc_mo
real, pointer, dimension(:,:,:) :: emit_bc_mo
real, pointer, dimension(:,:,:) :: bterm_mo
real, pointer, dimension(:,:,:) :: mterm_mo
real, pointer, dimension(:,:,:) :: burnfrac_mo
real, pointer, dimension(:,:,:) :: smfuncveg_mo

real, pointer, dimension(:,:) :: laimaxg_mo_t
real, pointer, dimension(:,:) :: stemmass_mo_t
real, pointer, dimension(:,:) :: rootmass_mo_t
real, pointer, dimension(:,:) :: litrfall_mo_t
real, pointer, dimension(:,:) :: humiftrs_mo_t
real, pointer, dimension(:,:) :: npp_mo_t
real, pointer, dimension(:,:) :: gpp_mo_t
real, pointer, dimension(:,:) :: vgbiomas_mo_t
real, pointer, dimension(:,:) :: autores_mo_t
real, pointer, dimension(:,:) :: totcmass_mo_t
real, pointer, dimension(:,:) :: litrmass_mo_t
real, pointer, dimension(:,:) :: soilcmas_mo_t
real, pointer, dimension(:,:) :: nep_mo_t
real, pointer, dimension(:,:) :: litres_mo_t
real, pointer, dimension(:,:) :: soilcres_mo_t
real, pointer, dimension(:,:) :: hetrores_mo_t
real, pointer, dimension(:,:) :: nbp_mo_t
real, pointer, dimension(:,:) :: emit_co2_mo_t
real, pointer, dimension(:,:) :: emit_co_mo_t
real, pointer, dimension(:,:) :: emit_ch4_mo_t
real, pointer, dimension(:,:) :: emit_nmhc_mo_t
real, pointer, dimension(:,:) :: emit_h2_mo_t
real, pointer, dimension(:,:) :: emit_nox_mo_t
real, pointer, dimension(:,:) :: emit_n2o_mo_t
real, pointer, dimension(:,:) :: emit_pm25_mo_t
real, pointer, dimension(:,:) :: emit_tpm_mo_t
real, pointer, dimension(:,:) :: emit_tc_mo_t
real, pointer, dimension(:,:) :: emit_oc_mo_t
real, pointer, dimension(:,:) :: emit_bc_mo_t
real, pointer, dimension(:,:) :: burnfrac_mo_t
real, pointer, dimension(:,:) :: smfuncveg_mo_t
real, pointer, dimension(:,:) :: bterm_mo_t
real, pointer, dimension(:,:) :: luc_emc_mo_t
real, pointer, dimension(:,:) :: lterm_mo_t
real, pointer, dimension(:,:) :: lucsocin_mo_t
real, pointer, dimension(:,:) :: mterm_mo_t
real, pointer, dimension(:,:) :: lucltrin_mo_t
real, pointer, dimension(:,:) :: ch4wet1_mo_t
real, pointer, dimension(:,:) :: ch4wet2_mo_t
real, pointer, dimension(:,:) :: wetfdyn_mo_t
real, pointer, dimension(:,:) :: ch4dyn1_mo_t
real, pointer, dimension(:,:) :: ch4dyn2_mo_t
real, pointer, dimension(:,:) :: ch4soills_mo_t
real, pointer, dimension(:,:) :: wind_mo_t

logical, pointer, dimension(:,:,:) :: pftexistrow
real, pointer, dimension(:,:,:) :: gppvegrow
real, pointer, dimension(:,:,:) :: nepvegrow
real, pointer, dimension(:,:,:) :: nbpvegrow
real, pointer, dimension(:,:,:) :: nppvegrow
real, pointer, dimension(:,:,:) :: hetroresvegrow
real, pointer, dimension(:,:,:) :: autoresvegrow
real, pointer, dimension(:,:,:) :: litresvegrow
real, pointer, dimension(:,:,:) :: soilcresvegrow
real, pointer, dimension(:,:,:) :: rmlvegaccrow
real, pointer, dimension(:,:,:) :: rmsvegrow
real, pointer, dimension(:,:,:) :: rmrvegrow
real, pointer, dimension(:,:,:) :: rgvegrow
real, pointer, dimension(:,:,:) :: ailcgrow
real, pointer, dimension(:,:,:) :: emit_co2row
real, pointer, dimension(:,:,:) :: emit_corow
real, pointer, dimension(:,:,:) :: emit_ch4row
real, pointer, dimension(:,:,:) :: emit_nmhcrow
real, pointer, dimension(:,:,:) :: emit_h2row
real, pointer, dimension(:,:,:) :: emit_noxrow
real, pointer, dimension(:,:,:) :: emit_n2orow
real, pointer, dimension(:,:,:) :: emit_pm25row
real, pointer, dimension(:,:,:) :: emit_tpmrow
real, pointer, dimension(:,:,:) :: emit_tcrow
real, pointer, dimension(:,:,:) :: emit_ocrow
real, pointer, dimension(:,:,:) :: emit_bcrow
real, pointer, dimension(:,:) :: burnfracrow
real, pointer, dimension(:,:,:) :: burnvegfrow
real, pointer, dimension(:,:,:) :: smfuncvegrow
real, pointer, dimension(:,:,:) :: btermrow
real, pointer, dimension(:,:) :: ltermrow
real, pointer, dimension(:,:,:) :: mtermrow
real, pointer, dimension(:,:) :: lucemcomrow
real, pointer, dimension(:,:) :: lucltrinrow
real, pointer, dimension(:,:) :: lucsocinrow
real, pointer, dimension(:,:) :: ch4wet1row
real, pointer, dimension(:,:) :: ch4wet2row
real, pointer, dimension(:,:) :: wetfdynrow
real, pointer, dimension(:,:) :: ch4dyn1row
real, pointer, dimension(:,:) :: ch4dyn2row
real, pointer, dimension(:,:) :: ch4soillsrow
real, pointer, dimension(:,:,:) :: litrmassrow
real, pointer, dimension(:,:,:) :: soilcmasrow
real, pointer, dimension(:,:,:) :: vgbiomas_vegrow
real, pointer, dimension(:,:,:) :: stemmassrow
real, pointer, dimension(:,:,:) :: rootmassrow
real, pointer, dimension(:,:,:) :: litrfallvegrow
real, pointer, dimension(:,:,:) :: humiftrsvegrow
real, pointer, dimension(:,:) ::uvaccrow_m
real, pointer, dimension(:,:) ::vvaccrow_m

real, pointer, dimension(:) :: laimaxg_mo_g
real, pointer, dimension(:) :: stemmass_mo_g
real, pointer, dimension(:) :: rootmass_mo_g
real, pointer, dimension(:) :: litrmass_mo_g
real, pointer, dimension(:) :: soilcmas_mo_g
real, pointer, dimension(:) :: litrfall_mo_g
real, pointer, dimension(:) :: humiftrs_mo_g
real, pointer, dimension(:) :: npp_mo_g
real, pointer, dimension(:) :: gpp_mo_g
real, pointer, dimension(:) :: nep_mo_g
real, pointer, dimension(:) :: nbp_mo_g
real, pointer, dimension(:) :: hetrores_mo_g
real, pointer, dimension(:) :: autores_mo_g
real, pointer, dimension(:) :: litres_mo_g
real, pointer, dimension(:) :: soilcres_mo_g
real, pointer, dimension(:) :: vgbiomas_mo_g
real, pointer, dimension(:) :: totcmass_mo_g
real, pointer, dimension(:) :: emit_co2_mo_g
real, pointer, dimension(:) :: emit_co_mo_g
real, pointer, dimension(:) :: emit_ch4_mo_g
real, pointer, dimension(:) :: emit_nmhc_mo_g
real, pointer, dimension(:) :: emit_h2_mo_g
real, pointer, dimension(:) :: emit_nox_mo_g
real, pointer, dimension(:) :: emit_n2o_mo_g
real, pointer, dimension(:) :: emit_pm25_mo_g
real, pointer, dimension(:) :: emit_tpm_mo_g
real, pointer, dimension(:) :: emit_tc_mo_g
real, pointer, dimension(:) :: emit_oc_mo_g
real, pointer, dimension(:) :: emit_bc_mo_g
real, pointer, dimension(:) :: smfuncveg_mo_g
real, pointer, dimension(:) :: luc_emc_mo_g
real, pointer, dimension(:) :: lucltrin_mo_g
real, pointer, dimension(:) :: lucsocin_mo_g
real, pointer, dimension(:) :: burnfrac_mo_g
real, pointer, dimension(:) :: bterm_mo_g
real, pointer, dimension(:) :: lterm_mo_g
real, pointer, dimension(:) :: mterm_mo_g
real, pointer, dimension(:) :: ch4wet1_mo_g
real, pointer, dimension(:) :: ch4wet2_mo_g
real, pointer, dimension(:) :: wetfdyn_mo_g
real, pointer, dimension(:) :: ch4dyn1_mo_g
real, pointer, dimension(:) :: ch4dyn2_mo_g
real, pointer, dimension(:) :: ch4soills_mo_g

! local
integer :: i,m,j,nt
real :: barefrac
real :: sumfare
integer :: NDMONTH
integer :: imonth

! point pointers

dofire                => c_switch%dofire
lnduseon              => c_switch%lnduseon
compete               => c_switch%compete
dowetlands            => c_switch%dowetlands
obswetf               => c_switch%obswetf
pftexistrow           => vrot%pftexist
fcancmxrow            => vrot%fcancmx
laimaxg_mo            =>ctem_mo%laimaxg_mo
stemmass_mo           =>ctem_mo%stemmass_mo
rootmass_mo           =>ctem_mo%rootmass_mo
litrfallveg_mo           =>ctem_mo%litrfallveg_mo
humiftrsveg_mo           =>ctem_mo%humiftrsveg_mo
npp_mo                =>ctem_mo%npp_mo
gpp_mo                =>ctem_mo%gpp_mo
vgbiomas_mo           =>ctem_mo%vgbiomas_mo
autores_mo            =>ctem_mo%autores_mo
totcmass_mo           =>ctem_mo%totcmass_mo
litrmass_mo           =>ctem_mo%litrmass_mo
soilcmas_mo           =>ctem_mo%soilcmas_mo
nep_mo                =>ctem_mo%nep_mo
litres_mo             =>ctem_mo%litres_mo
soilcres_mo           =>ctem_mo%soilcres_mo
hetrores_mo           =>ctem_mo%hetrores_mo
nbp_mo                =>ctem_mo%nbp_mo
emit_co2_mo           =>ctem_mo%emit_co2_mo
emit_co_mo            =>ctem_mo%emit_co_mo
emit_ch4_mo           =>ctem_mo%emit_ch4_mo
emit_nmhc_mo          =>ctem_mo%emit_nmhc_mo
emit_h2_mo            =>ctem_mo%emit_h2_mo
emit_nox_mo           =>ctem_mo%emit_nox_mo
emit_n2o_mo           =>ctem_mo%emit_n2o_mo
emit_pm25_mo          =>ctem_mo%emit_pm25_mo
emit_tpm_mo           =>ctem_mo%emit_tpm_mo
emit_tc_mo            =>ctem_mo%emit_tc_mo
emit_oc_mo            =>ctem_mo%emit_oc_mo
emit_bc_mo            =>ctem_mo%emit_bc_mo
bterm_mo              =>ctem_mo%bterm_mo
mterm_mo              =>ctem_mo%mterm_mo
burnfrac_mo           =>ctem_mo%burnfrac_mo
smfuncveg_mo          =>ctem_mo%smfuncveg_mo

laimaxg_mo_t          =>ctem_tile_mo%laimaxg_mo_t
stemmass_mo_t         =>ctem_tile_mo%stemmass_mo_t
rootmass_mo_t         =>ctem_tile_mo%rootmass_mo_t
litrfall_mo_t         =>ctem_tile_mo%litrfall_mo_t
humiftrs_mo_t         =>ctem_tile_mo%humiftrs_mo_t
npp_mo_t              =>ctem_tile_mo%npp_mo_t
gpp_mo_t              =>ctem_tile_mo%gpp_mo_t
vgbiomas_mo_t         =>ctem_tile_mo%vgbiomas_mo_t
autores_mo_t          =>ctem_tile_mo%autores_mo_t
totcmass_mo_t         =>ctem_tile_mo%totcmass_mo_t
litrmass_mo_t         =>ctem_tile_mo%litrmass_mo_t
soilcmas_mo_t         =>ctem_tile_mo%soilcmas_mo_t
nep_mo_t              =>ctem_tile_mo%nep_mo_t
litres_mo_t           =>ctem_tile_mo%litres_mo_t
soilcres_mo_t         =>ctem_tile_mo%soilcres_mo_t
hetrores_mo_t         =>ctem_tile_mo%hetrores_mo_t
nbp_mo_t              =>ctem_tile_mo%nbp_mo_t
emit_co2_mo_t         =>ctem_tile_mo%emit_co2_mo_t
emit_co_mo_t          =>ctem_tile_mo%emit_co_mo_t
emit_ch4_mo_t         =>ctem_tile_mo%emit_ch4_mo_t
emit_nmhc_mo_t        =>ctem_tile_mo%emit_nmhc_mo_t
emit_h2_mo_t          =>ctem_tile_mo%emit_h2_mo_t
emit_nox_mo_t         =>ctem_tile_mo%emit_nox_mo_t
emit_n2o_mo_t         =>ctem_tile_mo%emit_n2o_mo_t
emit_pm25_mo_t        =>ctem_tile_mo%emit_pm25_mo_t
emit_tpm_mo_t         =>ctem_tile_mo%emit_tpm_mo_t
emit_tc_mo_t          =>ctem_tile_mo%emit_tc_mo_t
emit_oc_mo_t          =>ctem_tile_mo%emit_oc_mo_t
emit_bc_mo_t          =>ctem_tile_mo%emit_bc_mo_t
burnfrac_mo_t         =>ctem_tile_mo%burnfrac_mo_t
smfuncveg_mo_t        =>ctem_tile_mo%smfuncveg_mo_t
bterm_mo_t            =>ctem_tile_mo%bterm_mo_t
luc_emc_mo_t          =>ctem_tile_mo%luc_emc_mo_t
lterm_mo_t            =>ctem_tile_mo%lterm_mo_t
lucsocin_mo_t         =>ctem_tile_mo%lucsocin_mo_t
mterm_mo_t            =>ctem_tile_mo%mterm_mo_t
lucltrin_mo_t         =>ctem_tile_mo%lucltrin_mo_t
ch4wet1_mo_t          =>ctem_tile_mo%ch4wet1_mo_t
ch4wet2_mo_t          =>ctem_tile_mo%ch4wet2_mo_t
wetfdyn_mo_t          =>ctem_tile_mo%wetfdyn_mo_t
ch4dyn1_mo_t          =>ctem_tile_mo%ch4dyn1_mo_t
ch4dyn2_mo_t          =>ctem_tile_mo%ch4dyn2_mo_t
ch4soills_mo_t        =>ctem_tile_mo%ch4soills_mo_t
wind_mo_t             =>ctem_tile_mo%wind_mo_t

gppvegrow         => vrot%gppveg
nepvegrow         => vrot%nepveg
nbpvegrow         => vrot%nbpveg
nppvegrow         => vrot%nppveg
hetroresvegrow    => vrot%hetroresveg
autoresvegrow     => vrot%autoresveg
litresvegrow      => vrot%litresveg
soilcresvegrow    => vrot%soilcresveg
rmlvegaccrow      => vrot%rmlvegacc
rmsvegrow         => vrot%rmsveg
rmrvegrow         => vrot%rmrveg
rgvegrow          => vrot%rgveg
ailcgrow          => vrot%ailcg
emit_co2row       => vrot%emit_co2
emit_corow        => vrot%emit_co
emit_ch4row       => vrot%emit_ch4
emit_nmhcrow      => vrot%emit_nmhc
emit_h2row        => vrot%emit_h2
emit_noxrow       => vrot%emit_nox
emit_n2orow       => vrot%emit_n2o
emit_pm25row      => vrot%emit_pm25
emit_tpmrow       => vrot%emit_tpm
emit_tcrow        => vrot%emit_tc
emit_ocrow        => vrot%emit_oc
emit_bcrow        => vrot%emit_bc
burnfracrow       => vrot%burnfrac
burnvegfrow       => vrot%burnvegf
smfuncvegrow      => vrot%smfuncveg
btermrow          => vrot%bterm
ltermrow          => vrot%lterm
mtermrow          => vrot%mterm
lucemcomrow       => vrot%lucemcom
lucltrinrow       => vrot%lucltrin
lucsocinrow       => vrot%lucsocin
ch4wet1row        => vrot%ch4wet1
ch4wet2row        => vrot%ch4wet2
wetfdynrow        => vrot%wetfdyn
ch4dyn1row        => vrot%ch4dyn1
ch4dyn2row        => vrot%ch4dyn2
ch4soillsrow      => vrot%ch4_soills
litrmassrow       => vrot%litrmass
soilcmasrow       => vrot%soilcmas
vgbiomas_vegrow   => vrot%vgbiomas_veg
stemmassrow       => vrot%stemmass
rootmassrow       => vrot%rootmass
uvaccrow_m        => vrot%uvaccrow_m
vvaccrow_m        => vrot%vvaccrow_m
litrfallvegrow    => vrot%litrfallveg
humiftrsvegrow    => vrot%humiftrsveg

laimaxg_mo_g        =>ctem_grd_mo%laimaxg_mo_g
stemmass_mo_g       =>ctem_grd_mo%stemmass_mo_g
rootmass_mo_g       =>ctem_grd_mo%rootmass_mo_g
litrmass_mo_g       =>ctem_grd_mo%litrmass_mo_g
soilcmas_mo_g       =>ctem_grd_mo%soilcmas_mo_g
litrfall_mo_g       =>ctem_grd_mo%litrfall_mo_g
humiftrs_mo_g       =>ctem_grd_mo%humiftrs_mo_g
npp_mo_g            =>ctem_grd_mo%npp_mo_g
gpp_mo_g            =>ctem_grd_mo%gpp_mo_g
nep_mo_g            =>ctem_grd_mo%nep_mo_g
nbp_mo_g            =>ctem_grd_mo%nbp_mo_g
hetrores_mo_g       =>ctem_grd_mo%hetrores_mo_g
autores_mo_g        =>ctem_grd_mo%autores_mo_g
litres_mo_g         =>ctem_grd_mo%litres_mo_g
soilcres_mo_g       =>ctem_grd_mo%soilcres_mo_g
vgbiomas_mo_g       =>ctem_grd_mo%vgbiomas_mo_g
totcmass_mo_g       =>ctem_grd_mo%totcmass_mo_g
emit_co2_mo_g       =>ctem_grd_mo%emit_co2_mo_g
emit_co_mo_g        =>ctem_grd_mo%emit_co_mo_g
emit_ch4_mo_g       =>ctem_grd_mo%emit_ch4_mo_g
emit_nmhc_mo_g      =>ctem_grd_mo%emit_nmhc_mo_g
emit_h2_mo_g        =>ctem_grd_mo%emit_h2_mo_g
emit_nox_mo_g       =>ctem_grd_mo%emit_nox_mo_g
emit_n2o_mo_g       =>ctem_grd_mo%emit_n2o_mo_g
emit_pm25_mo_g      =>ctem_grd_mo%emit_pm25_mo_g
emit_tpm_mo_g       =>ctem_grd_mo%emit_tpm_mo_g
emit_tc_mo_g        =>ctem_grd_mo%emit_tc_mo_g
emit_oc_mo_g        =>ctem_grd_mo%emit_oc_mo_g
emit_bc_mo_g        =>ctem_grd_mo%emit_bc_mo_g
smfuncveg_mo_g      =>ctem_grd_mo%smfuncveg_mo_g
luc_emc_mo_g        =>ctem_grd_mo%luc_emc_mo_g
lucltrin_mo_g       =>ctem_grd_mo%lucltrin_mo_g
lucsocin_mo_g       =>ctem_grd_mo%lucsocin_mo_g
burnfrac_mo_g       =>ctem_grd_mo%burnfrac_mo_g
bterm_mo_g          =>ctem_grd_mo%bterm_mo_g
lterm_mo_g          =>ctem_grd_mo%lterm_mo_g
mterm_mo_g          =>ctem_grd_mo%mterm_mo_g
ch4wet1_mo_g        =>ctem_grd_mo%ch4wet1_mo_g
ch4wet2_mo_g        =>ctem_grd_mo%ch4wet2_mo_g
wetfdyn_mo_g        =>ctem_grd_mo%wetfdyn_mo_g
ch4dyn1_mo_g        =>ctem_grd_mo%ch4dyn1_mo_g
ch4dyn2_mo_g        =>ctem_grd_mo%ch4dyn2_mo_g
ch4soills_mo_g      =>ctem_grd_mo%ch4soills_mo_g

!> ------------

!> Accumulate monthly outputs

do 862 i=1,nltest

    do 863 m = 1,nmtest
        do j=1,icc

           !> Accumulate monthly outputs at the per PFT level.
           if (ailcgrow(i,m,j) .gt. laimaxg_mo(i,m,j)) then
            laimaxg_mo(i,m,j)=ailcgrow(i,m,j)
           end if

           npp_mo(i,m,j)=npp_mo(i,m,j)+nppvegrow(i,m,j)
           gpp_mo(i,m,j)=gpp_mo(i,m,j)+gppvegrow(i,m,j)
           nep_mo(i,m,j)=nep_mo(i,m,j)+nepvegrow(i,m,j)
           nbp_mo(i,m,j)=nbp_mo(i,m,j)+nbpvegrow(i,m,j)
           hetrores_mo(i,m,j)=hetrores_mo(i,m,j)+hetroresvegrow(i,m,j)
           autores_mo(i,m,j) =autores_mo(i,m,j)+autoresvegrow(i,m,j)
           litres_mo(i,m,j)  =litres_mo(i,m,j) +litresvegrow(i,m,j)
           soilcres_mo(i,m,j) =soilcres_mo(i,m,j) +soilcresvegrow(i,m,j)
           emit_co2_mo(i,m,j)=emit_co2_mo(i,m,j)+emit_co2row(i,m,j)
           emit_co_mo(i,m,j) =emit_co_mo(i,m,j)+emit_corow(i,m,j)
           emit_ch4_mo(i,m,j) =emit_ch4_mo(i,m,j)+emit_ch4row(i,m,j)
           emit_nmhc_mo(i,m,j)=emit_nmhc_mo(i,m,j)+emit_nmhcrow(i,m,j)
           emit_h2_mo(i,m,j) =emit_h2_mo(i,m,j)+emit_h2row(i,m,j)
           emit_nox_mo(i,m,j) =emit_nox_mo(i,m,j)+emit_noxrow(i,m,j)
           emit_n2o_mo(i,m,j) =emit_n2o_mo(i,m,j)+emit_n2orow(i,m,j)
           emit_pm25_mo(i,m,j)=emit_pm25_mo(i,m,j)+emit_pm25row(i,m,j)
           emit_tpm_mo(i,m,j) =emit_tpm_mo(i,m,j)+emit_tpmrow(i,m,j)
           emit_tc_mo(i,m,j) =emit_tc_mo(i,m,j)+emit_tcrow(i,m,j)
           emit_oc_mo(i,m,j) =emit_oc_mo(i,m,j)+emit_ocrow(i,m,j)
           emit_bc_mo(i,m,j) =emit_bc_mo(i,m,j)+emit_bcrow(i,m,j)
           bterm_mo(i,m,j) = bterm_mo(i,m,j) + btermrow(i,m,j)
           mterm_mo(i,m,j) = mterm_mo(i,m,j) + mtermrow(i,m,j)
           burnfrac_mo(i,m,j) =burnfrac_mo(i,m,j)+burnvegfrow(i,m,j)
           smfuncveg_mo(i,m,j) =smfuncveg_mo(i,m,j) + smfuncvegrow(i,m,j)
           litrfallveg_mo(i,m,j) = litrfallveg_mo(i,m,j) + litrfallvegrow(i,m,j)
           humiftrsveg_mo(i,m,j) = humiftrsveg_mo(i,m,j) + humiftrsvegrow(i,m,j)

        end do !j

        !> Also do the bare ground
        nep_mo(i,m,iccp1)=nep_mo(i,m,iccp1)+nepvegrow(i,m,iccp1)
        nbp_mo(i,m,iccp1)=nbp_mo(i,m,iccp1)+nbpvegrow(i,m,iccp1)
        hetrores_mo(i,m,iccp1)=hetrores_mo(i,m,iccp1)+hetroresvegrow(i,m,iccp1)
        litres_mo(i,m,iccp1)  =litres_mo(i,m,iccp1)+litresvegrow(i,m,iccp1)
        soilcres_mo(i,m,iccp1) =soilcres_mo(i,m,iccp1) +soilcresvegrow(i,m,iccp1)

        !> Accumulate monthly outputs at the per tile level.
        luc_emc_mo_t(i,m) =luc_emc_mo_t(i,m)+lucemcomrow(i,m)
        lucsocin_mo_t(i,m) =lucsocin_mo_t(i,m)+lucsocinrow(i,m)
        lucltrin_mo_t(i,m) =lucltrin_mo_t(i,m)+lucltrinrow(i,m)
        ch4wet1_mo_t(i,m) = ch4wet1_mo_t(i,m) + ch4wet1row(i,m)
        ch4wet2_mo_t(i,m) = ch4wet2_mo_t(i,m) + ch4wet2row(i,m)
        wetfdyn_mo_t(i,m) = wetfdyn_mo_t(i,m) + wetfdynrow(i,m)
        ch4dyn1_mo_t(i,m) = ch4dyn1_mo_t(i,m) + ch4dyn1row(i,m)
        ch4dyn2_mo_t(i,m) = ch4dyn2_mo_t(i,m) + ch4dyn2row(i,m)
        ch4soills_mo_t(i,m) = ch4soills_mo_t(i,m) + ch4soillsrow(i,m)
        lterm_mo_t(i,m) = lterm_mo_t(i,m) + ltermrow(i,m)
        wind_mo_t(i,m) = wind_mo_t(i,m) + (sqrt(uvaccrow_m(i,m)**2.0 + vvaccrow_m(i,m)**2.0))*3.6 !>take mean wind speed and convert to km/h

863 continue ! m

    do 865 nt=1,nmon

        if(iday.eq.mmday(nt))then

        !> Do the mid-month variables (these are not accumulated, we just keep the mid month value for printing in the monthly file)

             do 866 m=1,nmtest

                do 867 j=1,icc

                  vgbiomas_mo(i,m,j)=vgbiomas_vegrow(i,m,j)
                  litrmass_mo(i,m,j)=litrmassrow(i,m,j)
                  soilcmas_mo(i,m,j)=soilcmasrow(i,m,j)
                  stemmass_mo(i,m,j)=stemmassrow(i,m,j)
                  rootmass_mo(i,m,j)=rootmassrow(i,m,j)
                  totcmass_mo(i,m,j)=vgbiomas_vegrow(i,m,j) + litrmassrow(i,m,j) + soilcmasrow(i,m,j)

867             continue

                !> Do the bare fraction too
                litrmass_mo(i,m,iccp1)=litrmassrow(i,m,iccp1)
                soilcmas_mo(i,m,iccp1)=soilcmasrow(i,m,iccp1)
                totcmass_mo(i,m,iccp1)=soilcmasrow(i,m,iccp1) + litrmassrow(i,m,iccp1)

                barefrac=1.0

               !> Now find the per tile values:
               do j=1,icc
                vgbiomas_mo_t(i,m)=vgbiomas_mo_t(i,m)+vgbiomas_mo(i,m,j)*fcancmxrow(i,m,j)
                litrmass_mo_t(i,m)=litrmass_mo_t(i,m)+litrmass_mo(i,m,j)*fcancmxrow(i,m,j)
                soilcmas_mo_t(i,m)=soilcmas_mo_t(i,m)+soilcmas_mo(i,m,j)*fcancmxrow(i,m,j)
                stemmass_mo_t(i,m)=stemmass_mo_t(i,m)+stemmass_mo(i,m,j)*fcancmxrow(i,m,j)
                rootmass_mo_t(i,m)=rootmass_mo_t(i,m)+rootmass_mo(i,m,j)*fcancmxrow(i,m,j)
                totcmass_mo_t(i,m)=totcmass_mo_t(i,m)+totcmass_mo(i,m,j)*fcancmxrow(i,m,j)
                barefrac=barefrac-fcancmxrow(i,m,j)
               end do

!>Also add in the bare fraction contributions.
                litrmass_mo_t(i,m)=litrmass_mo_t(i,m)+litrmass_mo(i,m,iccp1)*barefrac
                soilcmas_mo_t(i,m)=soilcmas_mo_t(i,m)+soilcmas_mo(i,m,iccp1)*barefrac
                totcmass_mo_t(i,m)=totcmass_mo_t(i,m)+(litrmass_mo(i,m,iccp1)+soilcmas_mo(i,m,iccp1))*barefrac

                !> Now find the gridcell level values:
                vgbiomas_mo_g(i)=vgbiomas_mo_g(i)+vgbiomas_mo_t(i,m)*FAREROT(i,m)
                litrmass_mo_g(i)=litrmass_mo_g(i)+litrmass_mo_t(i,m)*FAREROT(i,m)
                soilcmas_mo_g(i)=soilcmas_mo_g(i)+soilcmas_mo_t(i,m)*FAREROT(i,m)
                stemmass_mo_g(i)=stemmass_mo_g(i)+stemmass_mo_t(i,m)*FAREROT(i,m)
                rootmass_mo_g(i)=rootmass_mo_g(i)+rootmass_mo_t(i,m)*FAREROT(i,m)
                totcmass_mo_g(i)=totcmass_mo_g(i)+totcmass_mo_t(i,m)*FAREROT(i,m)

866         continue  !nmtest loop.

        endif ! mmday (mid-month instantaneous value)

        if(iday.eq.monthend(nt+1))then

            !> Do the end of month variables
            ndmonth=(monthend(nt+1)-monthend(nt))*nday

            do 900 m = 1,nmtest


                !> Convert some quantities into per day values
                wetfdyn_mo_t(i,m)=wetfdyn_mo_t(i,m)*(1./real(monthdays(nt)))
                lterm_mo_t(i,m)=lterm_mo_t(i,m)*(1./real(monthdays(nt)))
                wind_mo_t(i,m) = wind_mo_t(i,m)*(1./real(monthdays(nt)))
                do j = 1, icc
                    bterm_mo(i,m,j)=bterm_mo(i,m,j)*(1./real(monthdays(nt)))
                    mterm_mo(i,m,j)=mterm_mo(i,m,j)*(1./real(monthdays(nt)))
                    smfuncveg_mo(i,m,j) =smfuncveg_mo(i,m,j) *(1./real(monthdays(nt)))
                end do

                barefrac=1.0

                do j=1,icc

                    !> Find the monthly outputs at the per tile level from the outputs at the per PFT level
                    npp_mo_t(i,m)=npp_mo_t(i,m)+npp_mo(i,m,j)*fcancmxrow(i,m,j)
                    gpp_mo_t(i,m)=gpp_mo_t(i,m)+gpp_mo(i,m,j)*fcancmxrow(i,m,j)
                    nep_mo_t(i,m)=nep_mo_t(i,m)+nep_mo(i,m,j)*fcancmxrow(i,m,j)
                    nbp_mo_t(i,m)=nbp_mo_t(i,m)+nbp_mo(i,m,j)*fcancmxrow(i,m,j)
                    hetrores_mo_t(i,m)=hetrores_mo_t(i,m)+hetrores_mo(i,m,j)*fcancmxrow(i,m,j)
                    autores_mo_t(i,m) =autores_mo_t(i,m)+autores_mo(i,m,j)*fcancmxrow(i,m,j)
                    litres_mo_t(i,m)  =litres_mo_t(i,m) +litres_mo(i,m,j)*fcancmxrow(i,m,j)
                    soilcres_mo_t(i,m) =soilcres_mo_t(i,m) +soilcres_mo(i,m,j)*fcancmxrow(i,m,j)
                    emit_co2_mo_t(i,m)=emit_co2_mo_t(i,m)+emit_co2_mo(i,m,j)*fcancmxrow(i,m,j)
                    emit_co_mo_t(i,m) =emit_co_mo_t(i,m)+emit_co_mo(i,m,j)*fcancmxrow(i,m,j)
                    emit_ch4_mo_t(i,m) =emit_ch4_mo_t(i,m)+emit_ch4_mo(i,m,j)*fcancmxrow(i,m,j)
                    emit_nmhc_mo_t(i,m)=emit_nmhc_mo_t(i,m)+emit_nmhc_mo(i,m,j)*fcancmxrow(i,m,j)
                    emit_h2_mo_t(i,m) =emit_h2_mo_t(i,m)+emit_h2_mo(i,m,j)*fcancmxrow(i,m,j)
                    emit_nox_mo_t(i,m) =emit_nox_mo_t(i,m)+emit_nox_mo(i,m,j)*fcancmxrow(i,m,j)
                    emit_n2o_mo_t(i,m) =emit_n2o_mo_t(i,m)+emit_n2o_mo(i,m,j)*fcancmxrow(i,m,j)
                    emit_pm25_mo_t(i,m)=emit_pm25_mo_t(i,m)+emit_pm25_mo(i,m,j)*fcancmxrow(i,m,j)
                    emit_tpm_mo_t(i,m) =emit_tpm_mo_t(i,m)+emit_tpm_mo(i,m,j)*fcancmxrow(i,m,j)
                    emit_tc_mo_t(i,m) =emit_tc_mo_t(i,m)+emit_tc_mo(i,m,j)*fcancmxrow(i,m,j)
                    emit_oc_mo_t(i,m) =emit_oc_mo_t(i,m)+emit_oc_mo(i,m,j)*fcancmxrow(i,m,j)
                    emit_bc_mo_t(i,m) =emit_bc_mo_t(i,m)+emit_bc_mo(i,m,j)*fcancmxrow(i,m,j)
                    bterm_mo_t(i,m) =bterm_mo_t(i,m)+bterm_mo(i,m,j)*fcancmxrow(i,m,j)
                    mterm_mo_t(i,m) =mterm_mo_t(i,m)+mterm_mo(i,m,j)*fcancmxrow(i,m,j)
                    smfuncveg_mo_t(i,m) =smfuncveg_mo_t(i,m)+smfuncveg_mo(i,m,j)*fcancmxrow(i,m,j)
                    burnfrac_mo_t(i,m) =burnfrac_mo_t(i,m)+burnfrac_mo(i,m,j)*fcancmxrow(i,m,j)
                    laimaxg_mo_t(i,m)=laimaxg_mo_t(i,m)+laimaxg_mo(i,m,j)*fcancmxrow(i,m,j)
                    litrfall_mo_t(i,m)=litrfall_mo_t(i,m)+litrfallveg_mo(i,m,j)*fcancmxrow(i,m,j)
                    humiftrs_mo_t(i,m)=humiftrs_mo_t(i,m)+humiftrsveg_mo(i,m,j)*fcancmxrow(i,m,j)
                    barefrac=barefrac-fcancmxrow(i,m,j)

                end do !j

                nep_mo_t(i,m)=nep_mo_t(i,m)+nep_mo(i,m,iccp1)*barefrac
                nbp_mo_t(i,m)=nbp_mo_t(i,m)+nbp_mo(i,m,iccp1)*barefrac
                hetrores_mo_t(i,m)=hetrores_mo_t(i,m)+hetrores_mo(i,m,iccp1)*barefrac
                litres_mo_t(i,m)  =litres_mo_t(i,m) +litres_mo(i,m,iccp1)*barefrac
                soilcres_mo_t(i,m)=soilcres_mo_t(i,m)+soilcres_mo(i,m,iccp1)*barefrac
                humiftrs_mo_t(i,m)=humiftrs_mo_t(i,m)+humiftrsveg_mo(i,m,iccp1)*barefrac

                !> Find the monthly outputs at the per grid cell level from the outputs at the per tile level
                npp_mo_g(i)=npp_mo_g(i)+npp_mo_t(i,m)*FAREROT(i,m)
                gpp_mo_g(i)=gpp_mo_g(i)+gpp_mo_t(i,m)*FAREROT(i,m)
                nep_mo_g(i)=nep_mo_g(i)+nep_mo_t(i,m)*FAREROT(i,m)
                nbp_mo_g(i)=nbp_mo_g(i)+nbp_mo_t(i,m)*FAREROT(i,m)
                hetrores_mo_g(i)=hetrores_mo_g(i)+hetrores_mo_t(i,m)*FAREROT(i,m)
                autores_mo_g(i) =autores_mo_g(i) +autores_mo_t(i,m)*FAREROT(i,m)
                litres_mo_g(i)  =litres_mo_g(i) +litres_mo_t(i,m)*FAREROT(i,m)
                soilcres_mo_g(i) =soilcres_mo_g(i)+ soilcres_mo_t(i,m)*FAREROT(i,m)
                laimaxg_mo_g(i)=laimaxg_mo_g(i)+laimaxg_mo_t(i,m)*FAREROT(i,m)
                emit_co2_mo_g(i)=emit_co2_mo_g(i)+emit_co2_mo_t(i,m)*FAREROT(i,m)
                emit_co_mo_g(i) =emit_co_mo_g(i)+emit_co_mo_t(i,m)*FAREROT(i,m)
                emit_ch4_mo_g(i) =emit_ch4_mo_g(i)+emit_ch4_mo_t(i,m)*FAREROT(i,m)
                emit_nmhc_mo_g(i)=emit_nmhc_mo_g(i)+emit_nmhc_mo_t(i,m)*FAREROT(i,m)
                emit_h2_mo_g(i) =emit_h2_mo_g(i)+emit_h2_mo_t(i,m)*FAREROT(i,m)
                emit_nox_mo_g(i) =emit_nox_mo_g(i)+emit_nox_mo_t(i,m)*FAREROT(i,m)
                emit_n2o_mo_g(i) =emit_n2o_mo_g(i)+emit_n2o_mo_t(i,m)*FAREROT(i,m)
                emit_pm25_mo_g(i) =emit_pm25_mo_g(i)+emit_pm25_mo_t(i,m)*FAREROT(i,m)
                emit_tpm_mo_g(i) =emit_tpm_mo_g(i)+emit_tpm_mo_t(i,m)*FAREROT(i,m)
                emit_tc_mo_g(i) =emit_tc_mo_g(i)+emit_tc_mo_t(i,m)*FAREROT(i,m)
                emit_oc_mo_g(i) =emit_oc_mo_g(i)+emit_oc_mo_t(i,m)*FAREROT(i,m)
                emit_bc_mo_g(i) =emit_bc_mo_g(i)+emit_bc_mo_t(i,m)*FAREROT(i,m)
                burnfrac_mo_g(i)=burnfrac_mo_g(i)+burnfrac_mo_t(i,m)*FAREROT(i,m)
                luc_emc_mo_g(i) =luc_emc_mo_g(i)+luc_emc_mo_t(i,m)*FAREROT(i,m)
                lucsocin_mo_g(i) =lucsocin_mo_g(i)+lucsocin_mo_t(i,m)*FAREROT(i,m)
                lucltrin_mo_g(i) =lucltrin_mo_g(i)+lucltrin_mo_t(i,m)*FAREROT(i,m)
                ch4wet1_mo_g(i) = ch4wet1_mo_g(i) +ch4wet1_mo_t(i,m)*FAREROT(i,m)
                ch4wet2_mo_g(i) = ch4wet2_mo_g(i)+ch4wet2_mo_t(i,m)*FAREROT(i,m)
                wetfdyn_mo_g(i) = wetfdyn_mo_g(i)+wetfdyn_mo_t(i,m)*FAREROT(i,m)
                ch4dyn1_mo_g(i) = ch4dyn1_mo_g(i)+ch4dyn1_mo_t(i,m)*FAREROT(i,m)
                ch4dyn2_mo_g(i) = ch4dyn2_mo_g(i)+ch4dyn2_mo_t(i,m)*FAREROT(i,m)
                ch4soills_mo_g(i) = ch4soills_mo_g(i)+ch4soills_mo_t(i,m)*FAREROT(i,m)
                smfuncveg_mo_g(i)=smfuncveg_mo_g(i)+smfuncveg_mo_t(i,m)*FAREROT(i,m)
                bterm_mo_g(i) =bterm_mo_g(i)+bterm_mo_t(i,m)*FAREROT(i,m)
                lterm_mo_g(i) =lterm_mo_g(i)+lterm_mo_t(i,m)*FAREROT(i,m)
                mterm_mo_g(i) =mterm_mo_g(i)+mterm_mo_t(i,m)*FAREROT(i,m)
                litrfall_mo_g(i)=litrfall_mo_g(i)+litrfall_mo_t(i,m)*FAREROT(i,m)
                humiftrs_mo_g(i)=humiftrs_mo_g(i)+humiftrs_mo_t(i,m)*FAREROT(i,m)

900         continue

            imonth=nt

            !> Write to file .CT01M

            do m=1,nmtest

                barefrac=1.0

                !> First the per PFT values:
                do j=1,icc

                    barefrac=barefrac-fcancmxrow(i,m,j)

                    if (fcancmxrow(i,m,j) .gt. seed) then
                        write(84,8104)imonth,iyear,laimaxg_mo(i,m,j),&
                        vgbiomas_mo(i,m,j),litrmass_mo(i,m,j),&
                        soilcmas_mo(i,m,j),npp_mo(i,m,j),&
                        gpp_mo(i,m,j),nep_mo(i,m,j),&
                        nbp_mo(i,m,j),hetrores_mo(i,m,j),&
                        autores_mo(i,m,j),litres_mo(i,m,j),&
                        soilcres_mo(i,m,j),litrfallveg_mo(i,m,j),humiftrsveg_mo(i,m,j),&
                        ' TILE ',m,' PFT ',j,' FRAC ',fcancmxrow(i,m,j)
                    end if

                end do !icc

                !> Now write out the bare fraction values:
                if (barefrac .gt. seed) then
                    write(84,8104)imonth,iyear,0.0,  &
                        0.0,litrmass_mo(i,m,iccp1), &
                        soilcmas_mo(i,m,iccp1),0.0, &
                        0.0,nep_mo(i,m,iccp1), &
                        nbp_mo(i,m,iccp1),hetrores_mo(i,m,iccp1), &
                        0.0,litres_mo(i,m,iccp1), &
                        soilcres_mo(i,m,iccp1), &
                        0.0,humiftrsveg_mo(i,m,iccp1), &
                        ' TILE ',m,' PFT ',iccp1,' FRAC ',barefrac
                end if

                !> Now write out the tile average values for each tile if the tile number
                !> is greater than 1 (nmtest > 1).
                if (nmtest > 1) then
                    write(84,8104)imonth,iyear,laimaxg_mo_t(i,m),&
                        vgbiomas_mo_t(i,m),litrmass_mo_t(i,m),&
                        soilcmas_mo_t(i,m),npp_mo_t(i,m),&
                        gpp_mo_t(i,m),nep_mo_t(i,m),&
                        nbp_mo_t(i,m),hetrores_mo_t(i,m),&
                        autores_mo_t(i,m),litres_mo_t(i,m),&
                        soilcres_mo_t(i,m),litrfall_mo_t(i,m),humiftrs_mo_t(i,m),&
                        ' TILE ',m,' OF ',nmtest,' TFRAC ',FAREROT(i,m)
                end if

            end do !m

            write(84,8104)imonth,iyear,laimaxg_mo_g(i), &
                    vgbiomas_mo_g(i),litrmass_mo_g(i), &
                    soilcmas_mo_g(i),npp_mo_g(i), &
                    gpp_mo_g(i),nep_mo_g(i), &
                    nbp_mo_g(i),hetrores_mo_g(i),autores_mo_g(i), &
                    litres_mo_g(i),soilcres_mo_g(i),&
                    litrfall_mo_g(i),humiftrs_mo_g(i),' GRDAV'

            if (dofire .or. lnduseon) then

!>Write to file .CT06M

                do m=1,nmtest
                    !> First the per PFT values:
                    do j=1,icc
                        if (fcancmxrow(i,m,j) .gt. seed) then
                            write(85,8109)imonth,iyear,emit_co2_mo(i,m,j), &
                            emit_co_mo(i,m,j),emit_ch4_mo(i,m,j), &
                            emit_nmhc_mo(i,m,j),emit_h2_mo(i,m,j), &
                            emit_nox_mo(i,m,j),emit_n2o_mo(i,m,j), &
                            emit_pm25_mo(i,m,j),emit_tpm_mo(i,m,j), &
                            emit_tc_mo(i,m,j),emit_oc_mo(i,m,j), &
                            emit_bc_mo(i,m,j),smfuncveg_mo(i,m,j), &
                            luc_emc_mo_t(i,m),lucltrin_mo_t(i,m), &
                            lucsocin_mo_t(i,m),burnfrac_mo(i,m,j)*100., &
                            bterm_mo(i,m,j),lterm_mo_t(i,m),mterm_mo(i,m,j), &
                            wind_mo_t(i,m),' TILE ',m,' PFT ',j,' FRAC ',fcancmxrow(i,m,j)
                        end if
                    end do !j

                    !> Now write out the tile average values for each tile if the tile number
                    !> is greater than 1 (nmtest > 1).
                    if (nmtest > 1) then
                        write(85,8109)imonth,iyear,emit_co2_mo_t(i,m), &
                        emit_co_mo_t(i,m),emit_ch4_mo_t(i,m), &
                        emit_nmhc_mo_t(i,m),emit_h2_mo_t(i,m), &
                        emit_nox_mo_t(i,m),emit_n2o_mo_t(i,m), &
                        emit_pm25_mo_t(i,m),emit_tpm_mo_t(i,m), &
                        emit_tc_mo_t(i,m),emit_oc_mo_t(i,m), &
                        emit_bc_mo_t(i,m),smfuncveg_mo_t(i,m), &
                        luc_emc_mo_t(i,m),lucltrin_mo_t(i,m), &
                        lucsocin_mo_t(i,m),burnfrac_mo_t(i,m)*100., &
                        bterm_mo_t(i,m),lterm_mo_t(i,m),mterm_mo_t(i,m), &
                        wind_mo_t(i,m),' TILE ',m,' OF ',nmtest,' TFRAC ',FAREROT(i,m)
                    end if
                end do !m

                write(85,8109)imonth,iyear,emit_co2_mo_g(i), &
                    emit_co_mo_g(i),emit_ch4_mo_g(i),emit_nmhc_mo_g(i), &
                    emit_h2_mo_g(i),emit_nox_mo_g(i),emit_n2o_mo_g(i), &
                    emit_pm25_mo_g(i),emit_tpm_mo_g(i),emit_tc_mo_g(i), &
                    emit_oc_mo_g(i),emit_bc_mo_g(i), &
                    smfuncveg_mo_g(i),luc_emc_mo_g(i), &
                    lucltrin_mo_g(i),lucsocin_mo_g(i), &
                    burnfrac_mo_g(i)*100.,bterm_mo_g(i),lterm_mo_g(i), &
                    mterm_mo_g(i),wind_mo_t(i,m),' GRDAV '

            endif  !dofire/lnduseon

!>Add fraction of each pft and bare

            if (compete .or. lnduseon) then
                sumfare=0.0
                if (onetile_perPFT) then
                    do m=1,nmos
                        sumfare=sumfare+FAREROT(i,m)
                    enddo
                    write(88,8106)imonth,iyear,(FAREROT(i,m)*100.,m=1,nmos) &
                            ,sumfare,(pftexistrow(i,j,j),j=1,icc)
                else !composite or mosaic mode.
                    do m=1,nmtest
                        sumfare = 0.0
                        do j=1,icc
                            sumfare=sumfare+fcancmxrow(i,m,j)
                        end do !j
                        write(88,8106)imonth,iyear,(fcancmxrow(i,m,j)*100., &
                                j=1,icc),(1.0-sumfare)*100.,sumfare, &
                                    (pftexistrow(i,m,j),j=1,icc),' TILE ',m
                    end do !m
                endif !onetile_perPFT/composite
            endif !compete/lnduseon

            if (dowetlands .or. obswetf) then
                if (nmtest > 1) then
                    do m=1,nmtest
                        write(91,8111)imonth,iyear,ch4wet1_mo_t(i,m), &
                                ch4wet2_mo_t(i,m),wetfdyn_mo_t(i,m), &
                                ch4dyn1_mo_t(i,m),ch4dyn2_mo_t(i,m), &
                                ch4soills_mo_t(i,m),' TILE ',m
                    end do
                end if
                    write(91,8111)imonth,iyear,ch4wet1_mo_g(i), &
                                ch4wet2_mo_g(i),wetfdyn_mo_g(i), &
                                ch4dyn1_mo_g(i),ch4dyn2_mo_g(i), &
                                ch4soills_mo_g(i),' GRDAV '
            endif

    end if ! end of month

865   continue ! nmon
862  continue ! i

!> Prepare the monthly vars for the next month:
do nt=1,nmon
    if(iday.eq.monthend(nt+1))then

        !> Reset all end of month accumulated arrays
        call resetmonthend(nltest,nmtest)

    endif !> if(iday.eq.monthend(nt+1))
enddo !> nt=1,nmon


8104  FORMAT(1X,I4,I5,14(ES12.5,1X),2(A8,I2),A8,F8.2)
8106  FORMAT(1X,I4,I5,11(ES12.7,1X),9L5,2(A6,I2))
8109  FORMAT(1X,I4,I5,21(ES12.5,1X),2(A6,I2),A6,F8.2)
8111  FORMAT(1X,I4,I5,6(ES12.5,1X),2(A6,I2))

end subroutine ctem_monthly_aw
!>@}
!==============================================================================================================
!>\ingroup io_driver_ctem_annual_aw
!>@{
subroutine ctem_annual_aw(nltest,nmtest,iday,FAREROT,iyear,onetile_perPFT,leapnow)

use ctem_statevars,     only : ctem_tile_yr, vrot, ctem_grd_yr, c_switch, ctem_yr, &
                                resetyearend
use ctem_params, only : icc,iccp1,seed,nmos

implicit none

! arguments
integer, intent(in) :: nltest
integer, intent(in) :: nmtest
integer, intent(in) :: iday
real, intent(in), dimension(:,:) :: FAREROT
integer, intent(in) :: iyear
logical, intent(in) :: onetile_perPFT
logical, intent(in) :: leapnow                          !< true if this year is a leap year. Only used if the switch 'leap' is true.

! pointers

logical, pointer :: dofire
logical, pointer :: lnduseon
logical, pointer :: compete
logical, pointer :: dowetlands
logical, pointer :: obswetf

real, pointer, dimension(:,:,:) :: laimaxg_yr
real, pointer, dimension(:,:,:) :: stemmass_yr
real, pointer, dimension(:,:,:) :: rootmass_yr
real, pointer, dimension(:,:,:) :: npp_yr
real, pointer, dimension(:,:,:) :: gpp_yr
real, pointer, dimension(:,:,:) :: vgbiomas_yr
real, pointer, dimension(:,:,:) :: autores_yr
real, pointer, dimension(:,:,:) :: totcmass_yr
real, pointer, dimension(:,:,:) :: litrmass_yr
real, pointer, dimension(:,:,:) :: soilcmas_yr
real, pointer, dimension(:,:,:) :: nep_yr
real, pointer, dimension(:,:,:) :: litres_yr
real, pointer, dimension(:,:,:) :: soilcres_yr
real, pointer, dimension(:,:,:) :: hetrores_yr
real, pointer, dimension(:,:,:) :: nbp_yr
real, pointer, dimension(:,:,:) :: emit_co2_yr
real, pointer, dimension(:,:,:) :: emit_co_yr
real, pointer, dimension(:,:,:) :: emit_ch4_yr
real, pointer, dimension(:,:,:) :: emit_nmhc_yr
real, pointer, dimension(:,:,:) :: emit_h2_yr
real, pointer, dimension(:,:,:) :: emit_nox_yr
real, pointer, dimension(:,:,:) :: emit_n2o_yr
real, pointer, dimension(:,:,:) :: emit_pm25_yr
real, pointer, dimension(:,:,:) :: emit_tpm_yr
real, pointer, dimension(:,:,:) :: emit_tc_yr
real, pointer, dimension(:,:,:) :: emit_oc_yr
real, pointer, dimension(:,:,:) :: emit_bc_yr
real, pointer, dimension(:,:,:) :: bterm_yr
real, pointer, dimension(:,:,:) :: mterm_yr
real, pointer, dimension(:,:,:) :: smfuncveg_yr
real, pointer, dimension(:,:,:) :: burnfrac_yr
real, pointer, dimension(:,:,:) :: veghght_yr

real, pointer, dimension(:,:) :: laimaxg_yr_t
real, pointer, dimension(:,:) :: stemmass_yr_t
real, pointer, dimension(:,:) :: rootmass_yr_t
real, pointer, dimension(:,:) :: npp_yr_t
real, pointer, dimension(:,:) :: gpp_yr_t
real, pointer, dimension(:,:) :: vgbiomas_yr_t
real, pointer, dimension(:,:) :: autores_yr_t
real, pointer, dimension(:,:) :: totcmass_yr_t
real, pointer, dimension(:,:) :: litrmass_yr_t
real, pointer, dimension(:,:) :: soilcmas_yr_t
real, pointer, dimension(:,:) :: nep_yr_t
real, pointer, dimension(:,:) :: litres_yr_t
real, pointer, dimension(:,:) :: soilcres_yr_t
real, pointer, dimension(:,:) :: hetrores_yr_t
real, pointer, dimension(:,:) :: nbp_yr_t
real, pointer, dimension(:,:) :: emit_co2_yr_t
real, pointer, dimension(:,:) :: emit_co_yr_t
real, pointer, dimension(:,:) :: emit_ch4_yr_t
real, pointer, dimension(:,:) :: emit_nmhc_yr_t
real, pointer, dimension(:,:) :: emit_h2_yr_t
real, pointer, dimension(:,:) :: emit_nox_yr_t
real, pointer, dimension(:,:) :: emit_n2o_yr_t
real, pointer, dimension(:,:) :: emit_pm25_yr_t
real, pointer, dimension(:,:) :: emit_tpm_yr_t
real, pointer, dimension(:,:) :: emit_tc_yr_t
real, pointer, dimension(:,:) :: emit_oc_yr_t
real, pointer, dimension(:,:) :: emit_bc_yr_t
real, pointer, dimension(:,:) :: burnfrac_yr_t
real, pointer, dimension(:,:) :: smfuncveg_yr_t
real, pointer, dimension(:,:) :: bterm_yr_t
real, pointer, dimension(:,:) :: luc_emc_yr_t
real, pointer, dimension(:,:) :: lterm_yr_t
real, pointer, dimension(:,:) :: lucsocin_yr_t
real, pointer, dimension(:,:) :: mterm_yr_t
real, pointer, dimension(:,:) :: lucltrin_yr_t
real, pointer, dimension(:,:) :: ch4wet1_yr_t
real, pointer, dimension(:,:) :: ch4wet2_yr_t
real, pointer, dimension(:,:) :: wetfdyn_yr_t
real, pointer, dimension(:,:) :: ch4dyn1_yr_t
real, pointer, dimension(:,:) :: ch4dyn2_yr_t
real, pointer, dimension(:,:) :: ch4soills_yr_t
real, pointer, dimension(:,:) :: veghght_yr_t

logical, pointer, dimension(:,:,:) :: pftexistrow
real, pointer, dimension(:,:,:) :: gppvegrow
real, pointer, dimension(:,:,:) :: nepvegrow
real, pointer, dimension(:,:,:) :: nbpvegrow
real, pointer, dimension(:,:,:) :: nppvegrow
real, pointer, dimension(:,:,:) :: hetroresvegrow
real, pointer, dimension(:,:,:) :: autoresvegrow
real, pointer, dimension(:,:,:) :: litresvegrow
real, pointer, dimension(:,:,:) :: soilcresvegrow
real, pointer, dimension(:,:,:) :: rmlvegaccrow
real, pointer, dimension(:,:,:) :: rmsvegrow
real, pointer, dimension(:,:,:) :: rmrvegrow
real, pointer, dimension(:,:,:) :: rgvegrow
real, pointer, dimension(:,:,:) :: ailcgrow
real, pointer, dimension(:,:,:) :: emit_co2row
real, pointer, dimension(:,:,:) :: emit_corow
real, pointer, dimension(:,:,:) :: emit_ch4row
real, pointer, dimension(:,:,:) :: emit_nmhcrow
real, pointer, dimension(:,:,:) :: emit_h2row
real, pointer, dimension(:,:,:) :: emit_noxrow
real, pointer, dimension(:,:,:) :: emit_n2orow
real, pointer, dimension(:,:,:) :: emit_pm25row
real, pointer, dimension(:,:,:) :: emit_tpmrow
real, pointer, dimension(:,:,:) :: emit_tcrow
real, pointer, dimension(:,:,:) :: emit_ocrow
real, pointer, dimension(:,:,:) :: emit_bcrow
real, pointer, dimension(:,:) :: burnfracrow
real, pointer, dimension(:,:,:) :: burnvegfrow
real, pointer, dimension(:,:,:) :: smfuncvegrow
real, pointer, dimension(:,:,:) :: btermrow
real, pointer, dimension(:,:) :: ltermrow
real, pointer, dimension(:,:,:) :: mtermrow
real, pointer, dimension(:,:) :: lucemcomrow
real, pointer, dimension(:,:) :: lucltrinrow
real, pointer, dimension(:,:) :: lucsocinrow
real, pointer, dimension(:,:) :: ch4wet1row
real, pointer, dimension(:,:) :: ch4wet2row
real, pointer, dimension(:,:) :: wetfdynrow
real, pointer, dimension(:,:) :: ch4dyn1row
real, pointer, dimension(:,:) :: ch4dyn2row
real, pointer, dimension(:,:) :: ch4soillsrow
real, pointer, dimension(:,:,:) :: litrmassrow
real, pointer, dimension(:,:,:) :: soilcmasrow
real, pointer, dimension(:,:,:) :: vgbiomas_vegrow
real, pointer, dimension(:,:,:) :: stemmassrow
real, pointer, dimension(:,:,:) :: rootmassrow
real, pointer, dimension(:,:,:) :: fcancmxrow
real, pointer, dimension(:,:,:) :: veghghtrow

real, pointer, dimension(:) :: laimaxg_yr_g
real, pointer, dimension(:) :: stemmass_yr_g
real, pointer, dimension(:) :: rootmass_yr_g
real, pointer, dimension(:) :: litrmass_yr_g
real, pointer, dimension(:) :: soilcmas_yr_g
real, pointer, dimension(:) :: npp_yr_g
real, pointer, dimension(:) :: gpp_yr_g
real, pointer, dimension(:) :: nep_yr_g
real, pointer, dimension(:) :: nbp_yr_g
real, pointer, dimension(:) :: hetrores_yr_g
real, pointer, dimension(:) :: autores_yr_g
real, pointer, dimension(:) :: litres_yr_g
real, pointer, dimension(:) :: soilcres_yr_g
real, pointer, dimension(:) :: vgbiomas_yr_g
real, pointer, dimension(:) :: totcmass_yr_g
real, pointer, dimension(:) :: emit_co2_yr_g
real, pointer, dimension(:) :: emit_co_yr_g
real, pointer, dimension(:) :: emit_ch4_yr_g
real, pointer, dimension(:) :: emit_nmhc_yr_g
real, pointer, dimension(:) :: emit_h2_yr_g
real, pointer, dimension(:) :: emit_nox_yr_g
real, pointer, dimension(:) :: emit_n2o_yr_g
real, pointer, dimension(:) :: emit_pm25_yr_g
real, pointer, dimension(:) :: emit_tpm_yr_g
real, pointer, dimension(:) :: emit_tc_yr_g
real, pointer, dimension(:) :: emit_oc_yr_g
real, pointer, dimension(:) :: emit_bc_yr_g
real, pointer, dimension(:) :: smfuncveg_yr_g
real, pointer, dimension(:) :: luc_emc_yr_g
real, pointer, dimension(:) :: lucltrin_yr_g
real, pointer, dimension(:) :: lucsocin_yr_g
real, pointer, dimension(:) :: burnfrac_yr_g
real, pointer, dimension(:) :: bterm_yr_g
real, pointer, dimension(:) :: lterm_yr_g
real, pointer, dimension(:) :: mterm_yr_g
real, pointer, dimension(:) :: ch4wet1_yr_g
real, pointer, dimension(:) :: ch4wet2_yr_g
real, pointer, dimension(:) :: wetfdyn_yr_g
real, pointer, dimension(:) :: ch4dyn1_yr_g
real, pointer, dimension(:) :: ch4dyn2_yr_g
real, pointer, dimension(:) :: ch4soills_yr_g
real, pointer, dimension(:) :: veghght_yr_g

! local
integer :: i,m,j,nt
real :: barefrac
real :: sumfare
real :: daysinyr
integer :: NDMONTH
integer :: IMONTH

! point pointers

dofire                => c_switch%dofire
lnduseon              => c_switch%lnduseon
compete               => c_switch%compete
dowetlands            => c_switch%dowetlands
obswetf               => c_switch%obswetf

laimaxg_yr          =>ctem_yr%laimaxg_yr
stemmass_yr         =>ctem_yr%stemmass_yr
rootmass_yr         =>ctem_yr%rootmass_yr
npp_yr              =>ctem_yr%npp_yr
gpp_yr              =>ctem_yr%gpp_yr
vgbiomas_yr         =>ctem_yr%vgbiomas_yr
autores_yr          =>ctem_yr%autores_yr
totcmass_yr         =>ctem_yr%totcmass_yr
litrmass_yr         =>ctem_yr%litrmass_yr
soilcmas_yr         =>ctem_yr%soilcmas_yr
nep_yr              =>ctem_yr%nep_yr
litres_yr           =>ctem_yr%litres_yr
soilcres_yr         =>ctem_yr%soilcres_yr
hetrores_yr         =>ctem_yr%hetrores_yr
nbp_yr              =>ctem_yr%nbp_yr
emit_co2_yr         =>ctem_yr%emit_co2_yr
emit_co_yr          =>ctem_yr%emit_co_yr
emit_ch4_yr         =>ctem_yr%emit_ch4_yr
emit_nmhc_yr        =>ctem_yr%emit_nmhc_yr
emit_h2_yr          =>ctem_yr%emit_h2_yr
emit_nox_yr         =>ctem_yr%emit_nox_yr
emit_n2o_yr         =>ctem_yr%emit_n2o_yr
emit_pm25_yr        =>ctem_yr%emit_pm25_yr
emit_tpm_yr         =>ctem_yr%emit_tpm_yr
emit_tc_yr          =>ctem_yr%emit_tc_yr
emit_oc_yr          =>ctem_yr%emit_oc_yr
emit_bc_yr          =>ctem_yr%emit_bc_yr
bterm_yr            =>ctem_yr%bterm_yr
mterm_yr            =>ctem_yr%mterm_yr
burnfrac_yr         =>ctem_yr%burnfrac_yr
smfuncveg_yr        =>ctem_yr%smfuncveg_yr
veghght_yr          =>ctem_yr%veghght_yr

laimaxg_yr_t          =>ctem_tile_yr%laimaxg_yr_t
stemmass_yr_t         =>ctem_tile_yr%stemmass_yr_t
rootmass_yr_t         =>ctem_tile_yr%rootmass_yr_t
npp_yr_t              =>ctem_tile_yr%npp_yr_t
gpp_yr_t              =>ctem_tile_yr%gpp_yr_t
vgbiomas_yr_t         =>ctem_tile_yr%vgbiomas_yr_t
autores_yr_t          =>ctem_tile_yr%autores_yr_t
totcmass_yr_t         =>ctem_tile_yr%totcmass_yr_t
litrmass_yr_t         =>ctem_tile_yr%litrmass_yr_t
soilcmas_yr_t         =>ctem_tile_yr%soilcmas_yr_t
nep_yr_t              =>ctem_tile_yr%nep_yr_t
litres_yr_t           =>ctem_tile_yr%litres_yr_t
soilcres_yr_t         =>ctem_tile_yr%soilcres_yr_t
hetrores_yr_t         =>ctem_tile_yr%hetrores_yr_t
nbp_yr_t              =>ctem_tile_yr%nbp_yr_t
emit_co2_yr_t         =>ctem_tile_yr%emit_co2_yr_t
emit_co_yr_t          =>ctem_tile_yr%emit_co_yr_t
emit_ch4_yr_t         =>ctem_tile_yr%emit_ch4_yr_t
emit_nmhc_yr_t        =>ctem_tile_yr%emit_nmhc_yr_t
emit_h2_yr_t          =>ctem_tile_yr%emit_h2_yr_t
emit_nox_yr_t         =>ctem_tile_yr%emit_nox_yr_t
emit_n2o_yr_t         =>ctem_tile_yr%emit_n2o_yr_t
emit_pm25_yr_t        =>ctem_tile_yr%emit_pm25_yr_t
emit_tpm_yr_t         =>ctem_tile_yr%emit_tpm_yr_t
emit_tc_yr_t          =>ctem_tile_yr%emit_tc_yr_t
emit_oc_yr_t          =>ctem_tile_yr%emit_oc_yr_t
emit_bc_yr_t          =>ctem_tile_yr%emit_bc_yr_t
burnfrac_yr_t         =>ctem_tile_yr%burnfrac_yr_t
smfuncveg_yr_t        =>ctem_tile_yr%smfuncveg_yr_t
bterm_yr_t            =>ctem_tile_yr%bterm_yr_t
luc_emc_yr_t          =>ctem_tile_yr%luc_emc_yr_t
lterm_yr_t            =>ctem_tile_yr%lterm_yr_t
lucsocin_yr_t         =>ctem_tile_yr%lucsocin_yr_t
mterm_yr_t            =>ctem_tile_yr%mterm_yr_t
lucltrin_yr_t         =>ctem_tile_yr%lucltrin_yr_t
ch4wet1_yr_t          =>ctem_tile_yr%ch4wet1_yr_t
ch4wet2_yr_t          =>ctem_tile_yr%ch4wet2_yr_t
wetfdyn_yr_t          =>ctem_tile_yr%wetfdyn_yr_t
ch4dyn1_yr_t          =>ctem_tile_yr%ch4dyn1_yr_t
ch4dyn2_yr_t          =>ctem_tile_yr%ch4dyn2_yr_t
ch4soills_yr_t        =>ctem_tile_yr%ch4soills_yr_t
veghght_yr_t          =>ctem_tile_yr%veghght_yr_t

pftexistrow       => vrot%pftexist
gppvegrow         => vrot%gppveg
nepvegrow         => vrot%nepveg
nbpvegrow         => vrot%nbpveg
nppvegrow         => vrot%nppveg
hetroresvegrow    => vrot%hetroresveg
autoresvegrow     => vrot%autoresveg
litresvegrow      => vrot%litresveg
soilcresvegrow    => vrot%soilcresveg
rmlvegaccrow      => vrot%rmlvegacc
rmsvegrow         => vrot%rmsveg
rmrvegrow         => vrot%rmrveg
rgvegrow          => vrot%rgveg
ailcgrow          => vrot%ailcg
emit_co2row       => vrot%emit_co2
emit_corow        => vrot%emit_co
emit_ch4row       => vrot%emit_ch4
emit_nmhcrow      => vrot%emit_nmhc
emit_h2row        => vrot%emit_h2
emit_noxrow       => vrot%emit_nox
emit_n2orow       => vrot%emit_n2o
emit_pm25row      => vrot%emit_pm25
emit_tpmrow       => vrot%emit_tpm
emit_tcrow        => vrot%emit_tc
emit_ocrow        => vrot%emit_oc
emit_bcrow        => vrot%emit_bc
burnfracrow       => vrot%burnfrac
burnvegfrow       => vrot%burnvegf
smfuncvegrow      => vrot%smfuncveg
btermrow          => vrot%bterm
ltermrow          => vrot%lterm
mtermrow          => vrot%mterm
lucemcomrow       => vrot%lucemcom
lucltrinrow       => vrot%lucltrin
lucsocinrow       => vrot%lucsocin
ch4wet1row        => vrot%ch4wet1
ch4wet2row        => vrot%ch4wet2
wetfdynrow        => vrot%wetfdyn
ch4dyn1row        => vrot%ch4dyn1
ch4dyn2row        => vrot%ch4dyn2
ch4soillsrow      => vrot%ch4_soills
litrmassrow       => vrot%litrmass
soilcmasrow       => vrot%soilcmas
vgbiomas_vegrow   => vrot%vgbiomas_veg
stemmassrow       => vrot%stemmass
rootmassrow       => vrot%rootmass
fcancmxrow        => vrot%fcancmx
veghghtrow        => vrot%veghght

laimaxg_yr_g          =>ctem_grd_yr%laimaxg_yr_g
stemmass_yr_g         =>ctem_grd_yr%stemmass_yr_g
rootmass_yr_g         =>ctem_grd_yr%rootmass_yr_g
litrmass_yr_g         =>ctem_grd_yr%litrmass_yr_g
soilcmas_yr_g         =>ctem_grd_yr%soilcmas_yr_g
npp_yr_g              =>ctem_grd_yr%npp_yr_g
gpp_yr_g              =>ctem_grd_yr%gpp_yr_g
nep_yr_g              =>ctem_grd_yr%nep_yr_g
nbp_yr_g              =>ctem_grd_yr%nbp_yr_g
hetrores_yr_g         =>ctem_grd_yr%hetrores_yr_g
autores_yr_g          =>ctem_grd_yr%autores_yr_g
litres_yr_g           =>ctem_grd_yr%litres_yr_g
soilcres_yr_g         =>ctem_grd_yr%soilcres_yr_g
vgbiomas_yr_g         =>ctem_grd_yr%vgbiomas_yr_g
totcmass_yr_g         =>ctem_grd_yr%totcmass_yr_g
emit_co2_yr_g         =>ctem_grd_yr%emit_co2_yr_g
emit_co_yr_g          =>ctem_grd_yr%emit_co_yr_g
emit_ch4_yr_g         =>ctem_grd_yr%emit_ch4_yr_g
emit_nmhc_yr_g        =>ctem_grd_yr%emit_nmhc_yr_g
emit_h2_yr_g          =>ctem_grd_yr%emit_h2_yr_g
emit_nox_yr_g         =>ctem_grd_yr%emit_nox_yr_g
emit_n2o_yr_g         =>ctem_grd_yr%emit_n2o_yr_g
emit_pm25_yr_g        =>ctem_grd_yr%emit_pm25_yr_g
emit_tpm_yr_g         =>ctem_grd_yr%emit_tpm_yr_g
emit_tc_yr_g          =>ctem_grd_yr%emit_tc_yr_g
emit_oc_yr_g          =>ctem_grd_yr%emit_oc_yr_g
emit_bc_yr_g          =>ctem_grd_yr%emit_bc_yr_g
smfuncveg_yr_g        =>ctem_grd_yr%smfuncveg_yr_g
luc_emc_yr_g          =>ctem_grd_yr%luc_emc_yr_g
lucltrin_yr_g         =>ctem_grd_yr%lucltrin_yr_g
lucsocin_yr_g         =>ctem_grd_yr%lucsocin_yr_g
burnfrac_yr_g         =>ctem_grd_yr%burnfrac_yr_g
bterm_yr_g            =>ctem_grd_yr%bterm_yr_g
lterm_yr_g            =>ctem_grd_yr%lterm_yr_g
mterm_yr_g            =>ctem_grd_yr%mterm_yr_g
ch4wet1_yr_g          =>ctem_grd_yr%ch4wet1_yr_g
ch4wet2_yr_g          =>ctem_grd_yr%ch4wet2_yr_g
wetfdyn_yr_g          =>ctem_grd_yr%wetfdyn_yr_g
ch4dyn1_yr_g          =>ctem_grd_yr%ch4dyn1_yr_g
ch4dyn2_yr_g          =>ctem_grd_yr%ch4dyn2_yr_g
ch4soills_yr_g        =>ctem_grd_yr%ch4soills_yr_g
veghght_yr_g          =>ctem_grd_yr%veghght_yr_g
!>------------

!> Accumulate yearly outputs

do 882 i=1,nltest
    do 883 m=1,nmtest
        do 884 j=1,icc

            !> Accumulate the variables at the per PFT level

            if (ailcgrow(i,m,j).gt.laimaxg_yr(i,m,j)) then
            laimaxg_yr(i,m,j)=ailcgrow(i,m,j)
            end if

            npp_yr(i,m,j)=npp_yr(i,m,j)+nppvegrow(i,m,j)
            gpp_yr(i,m,j)=gpp_yr(i,m,j)+gppvegrow(i,m,j)
            nep_yr(i,m,j)=nep_yr(i,m,j)+nepvegrow(i,m,j)
            nbp_yr(i,m,j)=nbp_yr(i,m,j)+nbpvegrow(i,m,j)
            emit_co2_yr(i,m,j)=emit_co2_yr(i,m,j)+emit_co2row(i,m,j)
            emit_co_yr(i,m,j)=emit_co_yr(i,m,j)+emit_corow(i,m,j)
            emit_ch4_yr(i,m,j)=emit_ch4_yr(i,m,j)+emit_ch4row(i,m,j)
            emit_nmhc_yr(i,m,j)=emit_nmhc_yr(i,m,j)+emit_nmhcrow(i,m,j)
            emit_h2_yr(i,m,j)=emit_h2_yr(i,m,j)+emit_h2row(i,m,j)
            emit_nox_yr(i,m,j)=emit_nox_yr(i,m,j)+emit_noxrow(i,m,j)
            emit_n2o_yr(i,m,j)=emit_n2o_yr(i,m,j)+emit_n2orow(i,m,j)
            emit_pm25_yr(i,m,j)=emit_pm25_yr(i,m,j)+emit_pm25row(i,m,j)
            emit_tpm_yr(i,m,j)=emit_tpm_yr(i,m,j)+emit_tpmrow(i,m,j)
            emit_tc_yr(i,m,j)=emit_tc_yr(i,m,j)+emit_tcrow(i,m,j)
            emit_oc_yr(i,m,j)=emit_oc_yr(i,m,j)+emit_ocrow(i,m,j)
            emit_bc_yr(i,m,j)=emit_bc_yr(i,m,j)+emit_bcrow(i,m,j)

            if (leapnow) then
                daysinyr=366.
            else
                daysinyr=365.
            end if

            bterm_yr(i,m,j)=bterm_yr(i,m,j)+(btermrow(i,m,j)*(1./daysinyr))
            mterm_yr(i,m,j)=mterm_yr(i,m,j)+(mtermrow(i,m,j)*(1./daysinyr))
            smfuncveg_yr(i,m,j)=smfuncveg_yr(i,m,j)+(smfuncvegrow(i,m,j) * (1./365.))
            hetrores_yr(i,m,j)=hetrores_yr(i,m,j)+hetroresvegrow(i,m,j)
            autores_yr(i,m,j)=autores_yr(i,m,j)+autoresvegrow(i,m,j)
            litres_yr(i,m,j)=litres_yr(i,m,j)+litresvegrow(i,m,j)
            soilcres_yr(i,m,j)=soilcres_yr(i,m,j)+soilcresvegrow(i,m,j)
            burnfrac_yr(i,m,j)=burnfrac_yr(i,m,j)+burnvegfrow(i,m,j)

884     continue

    !>   Also do the bare fraction amounts
        hetrores_yr(i,m,iccp1)=hetrores_yr(i,m,iccp1)+hetroresvegrow(i,m,iccp1)
        litres_yr(i,m,iccp1)=litres_yr(i,m,iccp1)+litresvegrow(i,m,iccp1)
        soilcres_yr(i,m,iccp1)=soilcres_yr(i,m,iccp1)+soilcresvegrow(i,m,iccp1)
        nep_yr(i,m,iccp1)=nep_yr(i,m,iccp1)+nepvegrow(i,m,iccp1)
        nbp_yr(i,m,iccp1)=nbp_yr(i,m,iccp1)+nbpvegrow(i,m,iccp1)

        !> Accumulate the variables at the per tile level
        lterm_yr_t(i,m)=lterm_yr_t(i,m)+(ltermrow(i,m)*(1./daysinyr))
        wetfdyn_yr_t(i,m) = wetfdyn_yr_t(i,m)+(wetfdynrow(i,m)*(1./daysinyr))
        luc_emc_yr_t(i,m)=luc_emc_yr_t(i,m)+lucemcomrow(i,m)
        lucsocin_yr_t(i,m)=lucsocin_yr_t(i,m)+lucsocinrow(i,m)
        lucltrin_yr_t(i,m)=lucltrin_yr_t(i,m)+lucltrinrow(i,m)
        ch4wet1_yr_t(i,m) = ch4wet1_yr_t(i,m)+ch4wet1row(i,m)
        ch4wet2_yr_t(i,m) = ch4wet2_yr_t(i,m)+ch4wet2row(i,m)
        ch4dyn1_yr_t(i,m) = ch4dyn1_yr_t(i,m)+ch4dyn1row(i,m)
        ch4dyn2_yr_t(i,m) = ch4dyn2_yr_t(i,m)+ch4dyn2row(i,m)
        ch4soills_yr_t(i,m) = ch4soills_yr_t(i,m)+ch4soillsrow(i,m)

883 continue ! m

    if ((.not. leapnow .and.iday.eq.365) .or.(leapnow .and.iday.eq.366)) then


        do 900 m = 1, nmtest

            do 925 j=1,icc

                !> The pools are looked at just at the end of the year.
                stemmass_yr(i,m,j)=stemmassrow(i,m,j)
                rootmass_yr(i,m,j)=rootmassrow(i,m,j)
                litrmass_yr(i,m,j)=litrmassrow(i,m,j)
                soilcmas_yr(i,m,j)=soilcmasrow(i,m,j)
                vgbiomas_yr(i,m,j)=vgbiomas_vegrow(i,m,j)
                totcmass_yr(i,m,j)=vgbiomas_yr(i,m,j)+litrmass_yr(i,m,j)+soilcmas_yr(i,m,j)
                veghght_yr(i,m,j)=veghghtrow(i,m,j)

925         continue

            litrmass_yr(i,m,iccp1)=litrmassrow(i,m,iccp1)
            soilcmas_yr(i,m,iccp1)=soilcmasrow(i,m,iccp1)
            totcmass_yr(i,m,iccp1)=litrmassrow(i,m,iccp1) + soilcmasrow(i,m,iccp1)

            barefrac=1.0

            !> Add values to the per tile vars
            do j=1,icc

                laimaxg_yr_t(i,m)=laimaxg_yr_t(i,m)+ laimaxg_yr(i,m,j)*fcancmxrow(i,m,j)
                stemmass_yr_t(i,m)=stemmass_yr_t(i,m)+stemmass_yr(i,m,j)*fcancmxrow(i,m,j)
                rootmass_yr_t(i,m)=rootmass_yr_t(i,m)+rootmass_yr(i,m,j)*fcancmxrow(i,m,j)
                litrmass_yr_t(i,m)=litrmass_yr_t(i,m)+litrmass_yr(i,m,j)*fcancmxrow(i,m,j)
                soilcmas_yr_t(i,m)=soilcmas_yr_t(i,m)+soilcmas_yr(i,m,j)*fcancmxrow(i,m,j)
                vgbiomas_yr_t(i,m)=vgbiomas_yr_t(i,m)+vgbiomas_yr(i,m,j)*fcancmxrow(i,m,j)
                totcmass_yr_t(i,m)=totcmass_yr_t(i,m)+totcmass_yr(i,m,j)*fcancmxrow(i,m,j)
                npp_yr_t(i,m)=npp_yr_t(i,m)+npp_yr(i,m,j)*fcancmxrow(i,m,j)
                gpp_yr_t(i,m)=gpp_yr_t(i,m)+gpp_yr(i,m,j)*fcancmxrow(i,m,j)
                nep_yr_t(i,m)=nep_yr_t(i,m)+nep_yr(i,m,j)*fcancmxrow(i,m,j)
                nbp_yr_t(i,m)=nbp_yr_t(i,m)+nbp_yr(i,m,j)*fcancmxrow(i,m,j)
                emit_co2_yr_t(i,m)=emit_co2_yr_t(i,m)+emit_co2_yr(i,m,j)*fcancmxrow(i,m,j)
                emit_co_yr_t(i,m)=emit_co_yr_t(i,m)+emit_co_yr(i,m,j)*fcancmxrow(i,m,j)
                emit_ch4_yr_t(i,m)=emit_ch4_yr_t(i,m)+emit_ch4_yr(i,m,j)*fcancmxrow(i,m,j)
                emit_nmhc_yr_t(i,m)=emit_nmhc_yr_t(i,m)+emit_nmhc_yr(i,m,j)*fcancmxrow(i,m,j)
                emit_h2_yr_t(i,m)=emit_h2_yr_t(i,m)+emit_h2_yr(i,m,j)*fcancmxrow(i,m,j)
                emit_nox_yr_t(i,m)=emit_nox_yr_t(i,m)+emit_nox_yr(i,m,j)*fcancmxrow(i,m,j)
                emit_n2o_yr_t(i,m)=emit_n2o_yr_t(i,m)+emit_n2o_yr(i,m,j)*fcancmxrow(i,m,j)
                emit_pm25_yr_t(i,m)=emit_pm25_yr_t(i,m)+emit_pm25_yr(i,m,j)*fcancmxrow(i,m,j)
                emit_tpm_yr_t(i,m)=emit_tpm_yr_t(i,m)+emit_tpm_yr(i,m,j)*fcancmxrow(i,m,j)
                emit_tc_yr_t(i,m)=emit_tc_yr_t(i,m)+emit_tc_yr(i,m,j)*fcancmxrow(i,m,j)
                emit_oc_yr_t(i,m)=emit_oc_yr_t(i,m)+emit_oc_yr(i,m,j)*fcancmxrow(i,m,j)
                emit_bc_yr_t(i,m)=emit_bc_yr_t(i,m)+emit_bc_yr(i,m,j)*fcancmxrow(i,m,j)
                bterm_yr_t(i,m)=bterm_yr_t(i,m)+bterm_yr(i,m,j)*fcancmxrow(i,m,j)
                mterm_yr_t(i,m)=mterm_yr_t(i,m)+mterm_yr(i,m,j)*fcancmxrow(i,m,j)
                smfuncveg_yr_t(i,m)=smfuncveg_yr_t(i,m)+smfuncveg_yr(i,m,j)*fcancmxrow(i,m,j)
                hetrores_yr_t(i,m)=hetrores_yr_t(i,m)+hetrores_yr(i,m,j)*fcancmxrow(i,m,j)
                autores_yr_t(i,m) =autores_yr_t(i,m) +autores_yr(i,m,j)*fcancmxrow(i,m,j)
                litres_yr_t(i,m)  =litres_yr_t(i,m)  +litres_yr(i,m,j)*fcancmxrow(i,m,j)
                soilcres_yr_t(i,m) =soilcres_yr_t(i,m) +soilcres_yr(i,m,j)*fcancmxrow(i,m,j)
                burnfrac_yr_t(i,m)=burnfrac_yr_t(i,m)+burnfrac_yr(i,m,j)*fcancmxrow(i,m,j)
                veghght_yr_t(i,m) = veghght_yr_t(i,m)+veghght_yr(i,m,j)*fcancmxrow(i,m,j)

                barefrac=barefrac-fcancmxrow(i,m,j)

            end do !j

            litrmass_yr_t(i,m)=litrmass_yr_t(i,m)+litrmass_yr(i,m,iccp1)*barefrac
            soilcmas_yr_t(i,m)=soilcmas_yr_t(i,m)+soilcmas_yr(i,m,iccp1)*barefrac
            hetrores_yr_t(i,m)=hetrores_yr_t(i,m)+hetrores_yr(i,m,iccp1)*barefrac
            litres_yr_t(i,m)  =litres_yr_t(i,m)  +litres_yr(i,m,iccp1)*barefrac
            soilcres_yr_t(i,m)=soilcres_yr_t(i,m)+soilcres_yr(i,m,iccp1)*barefrac
            nep_yr_t(i,m)=nep_yr_t(i,m)+nep_yr(i,m,iccp1)*barefrac
            nbp_yr_t(i,m)=nbp_yr_t(i,m)+nbp_yr(i,m,iccp1)*barefrac
            totcmass_yr_t(i,m) = totcmass_yr_t(i,m)+(litrmass_yr(i,m,iccp1) + soilcmas_yr(i,m,iccp1))*barefrac

            !> Add values to the per gridcell vars
            laimaxg_yr_g(i)=laimaxg_yr_g(i)+ laimaxg_yr_t(i,m)*FAREROT(i,m)
            stemmass_yr_g(i)=stemmass_yr_g(i)+stemmass_yr_t(i,m)*FAREROT(i,m)
            rootmass_yr_g(i)=rootmass_yr_g(i)+rootmass_yr_t(i,m)*FAREROT(i,m)
            litrmass_yr_g(i)=litrmass_yr_g(i)+litrmass_yr_t(i,m)*FAREROT(i,m)
            soilcmas_yr_g(i)=soilcmas_yr_g(i)+soilcmas_yr_t(i,m)*FAREROT(i,m)
            vgbiomas_yr_g(i)=vgbiomas_yr_g(i)+vgbiomas_yr_t(i,m)*FAREROT(i,m)
            totcmass_yr_g(i)=totcmass_yr_g(i)+totcmass_yr_t(i,m)*FAREROT(i,m)
            npp_yr_g(i)=npp_yr_g(i)+npp_yr_t(i,m)*FAREROT(i,m)
            gpp_yr_g(i)=gpp_yr_g(i)+gpp_yr_t(i,m)*FAREROT(i,m)
            nep_yr_g(i)=nep_yr_g(i)+nep_yr_t(i,m)*FAREROT(i,m)
            nbp_yr_g(i)=nbp_yr_g(i)+nbp_yr_t(i,m)*FAREROT(i,m)
            emit_co2_yr_g(i)=emit_co2_yr_g(i)+emit_co2_yr_t(i,m)*FAREROT(i,m)
            emit_co_yr_g(i)=emit_co_yr_g(i)+emit_co_yr_t(i,m)*FAREROT(i,m)
            emit_ch4_yr_g(i)=emit_ch4_yr_g(i)+emit_ch4_yr_t(i,m)*FAREROT(i,m)
            emit_nmhc_yr_g(i)=emit_nmhc_yr_g(i)+emit_nmhc_yr_t(i,m)*FAREROT(i,m)
            emit_h2_yr_g(i)=emit_h2_yr_g(i)+emit_h2_yr_t(i,m)*FAREROT(i,m)
            emit_nox_yr_g(i)=emit_nox_yr_g(i)+emit_nox_yr_t(i,m)*FAREROT(i,m)
            emit_n2o_yr_g(i)=emit_n2o_yr_g(i)+emit_n2o_yr_t(i,m)*FAREROT(i,m)
            emit_pm25_yr_g(i)=emit_pm25_yr_g(i)+emit_pm25_yr_t(i,m)*FAREROT(i,m)
            emit_tpm_yr_g(i)=emit_tpm_yr_g(i)+emit_tpm_yr_t(i,m)*FAREROT(i,m)
            emit_tc_yr_g(i)=emit_tc_yr_g(i)+emit_tc_yr_t(i,m)*FAREROT(i,m)
            emit_oc_yr_g(i)=emit_oc_yr_g(i)+emit_oc_yr_t(i,m)*FAREROT(i,m)
            emit_bc_yr_g(i)=emit_bc_yr_g(i)+emit_bc_yr_t(i,m)*FAREROT(i,m)
            hetrores_yr_g(i)=hetrores_yr_g(i)+hetrores_yr_t(i,m)*FAREROT(i,m)
            autores_yr_g(i) =autores_yr_g(i) +autores_yr_t(i,m)*FAREROT(i,m)
            litres_yr_g(i)  =litres_yr_g(i)  +litres_yr_t(i,m)*FAREROT(i,m)
            soilcres_yr_g(i) =soilcres_yr_g(i) +soilcres_yr_t(i,m)*FAREROT(i,m)
            burnfrac_yr_g(i)=burnfrac_yr_g(i)+burnfrac_yr_t(i,m)*FAREROT(i,m)
            smfuncveg_yr_g(i)=smfuncveg_yr_g(i)+smfuncveg_yr_t(i,m)*FAREROT(i,m)
            bterm_yr_g(i)=bterm_yr_g(i)+bterm_yr_t(i,m)*FAREROT(i,m)
            lterm_yr_g(i)=lterm_yr_g(i)+lterm_yr_t(i,m)*FAREROT(i,m)
            mterm_yr_g(i)=mterm_yr_g(i)+mterm_yr_t(i,m)*FAREROT(i,m)
            luc_emc_yr_g(i)=luc_emc_yr_g(i)+luc_emc_yr_t(i,m)*FAREROT(i,m)
            lucsocin_yr_g(i)=lucsocin_yr_g(i)+lucsocin_yr_t(i,m)*FAREROT(i,m)
            lucltrin_yr_g(i)=lucltrin_yr_g(i)+lucltrin_yr_t(i,m)*FAREROT(i,m)
            ch4wet1_yr_g(i) = ch4wet1_yr_g(i)+ch4wet1_yr_t(i,m)*FAREROT(i,m)
            ch4wet2_yr_g(i) = ch4wet2_yr_g(i)+ch4wet2_yr_t(i,m)*FAREROT(i,m)
            wetfdyn_yr_g(i) = wetfdyn_yr_g(i)+wetfdyn_yr_t(i,m)*FAREROT(i,m)
            ch4dyn1_yr_g(i) = ch4dyn1_yr_g(i)+ch4dyn1_yr_t(i,m)*FAREROT(i,m)
            ch4dyn2_yr_g(i) = ch4dyn2_yr_g(i)+ch4dyn2_yr_t(i,m)*FAREROT(i,m)
            ch4soills_yr_g(i) = ch4soills_yr_g(i)+ch4soills_yr_t(i,m)*FAREROT(i,m)
            veghght_yr_g(i) = veghght_yr_g(i) + veghght_yr_t(i,m)*FAREROT(i,m)

900      continue !m

!>Write to annual output files:
!>
!>File .CT01Y

        do m=1,nmtest

            barefrac=1.0
            do j=1,icc
                barefrac=barefrac-fcancmxrow(i,m,j)
                if (fcancmxrow(i,m,j) .gt. seed) then
                    write(86,8105)iyear,laimaxg_yr(i,m,j), &
                    vgbiomas_yr(i,m,j),stemmass_yr(i,m,j), &
                    rootmass_yr(i,m,j),litrmass_yr(i,m,j), &
                    soilcmas_yr(i,m,j),totcmass_yr(i,m,j), &
                    npp_yr(i,m,j),gpp_yr(i,m,j),nep_yr(i,m,j), &
                    nbp_yr(i,m,j),hetrores_yr(i,m,j), &
                    autores_yr(i,m,j),litres_yr(i,m,j), &
                    soilcres_yr(i,m,j),veghght_yr(i,m,j),' TILE ',m,' PFT ',j,' FRAC ' &
                    ,fcancmxrow(i,m,j)
                end if
            end do !j

    !>Now do the bare fraction of the grid cell. Only soil c, hetres
    !>and litter are relevant so the rest are set to 0.

            if (barefrac .gt. seed) then
                write(86,8105)iyear,0.,  &
                    0.,  &
                    0.,0., &
                    litrmass_yr(i,m,iccp1),soilcmas_yr(i,m,iccp1), &
                    totcmass_yr(i,m,iccp1),0., &
                    0.,nep_yr(i,m,iccp1), &
                    nbp_yr(i,m,iccp1),hetrores_yr(i,m,iccp1), &
                    0.,litres_yr(i,m,iccp1),soilcres_yr(i,m,iccp1),0.0, &
                    ' TILE ',m,' PFT ',iccp1,' FRAC ',barefrac
            end if

            if (nmtest > 1) then

                !> Write out the per tile values
                write(86,8105)iyear,laimaxg_yr_t(i,m), &
                    vgbiomas_yr_t(i,m),stemmass_yr_t(i,m), &
                    rootmass_yr_t(i,m),litrmass_yr_t(i,m), &
                    soilcmas_yr_t(i,m),totcmass_yr_t(i,m), &
                    npp_yr_t(i,m),gpp_yr_t(i,m),nep_yr_t(i,m), &
                    nbp_yr_t(i,m),hetrores_yr_t(i,m), &
                    autores_yr_t(i,m),litres_yr_t(i,m), &
                    soilcres_yr_t(i,m),veghght_yr_t(i,m),' TILE ',m,' OF ' &
                    ,nmtest,' TFRAC ',FAREROT(i,m)
            end if
        end do !m

        !> Finally write out the per gridcell values
        write(86,8105)iyear,laimaxg_yr_g(i),vgbiomas_yr_g(i), &
                stemmass_yr_g(i),rootmass_yr_g(i),litrmass_yr_g(i), &
                soilcmas_yr_g(i),totcmass_yr_g(i),npp_yr_g(i), &
                gpp_yr_g(i),nep_yr_g(i), &
                nbp_yr_g(i),hetrores_yr_g(i),autores_yr_g(i), &
                litres_yr_g(i),soilcres_yr_g(i),veghght_yr_g(i),' GRDAV'

        if (dofire .or. lnduseon) then

            !> Write to file .CT06Y
            do m=1,nmtest
                do j=1,icc
                    if (fcancmxrow(i,m,j) .gt. seed) then
                        write(87,8108)iyear,emit_co2_yr(i,m,j), &
                        emit_co_yr(i,m,j),emit_ch4_yr(i,m,j), &
                        emit_nmhc_yr(i,m,j),emit_h2_yr(i,m,j), &
                        emit_nox_yr(i,m,j),emit_n2o_yr(i,m,j), &
                        emit_pm25_yr(i,m,j),emit_tpm_yr(i,m,j), &
                        emit_tc_yr(i,m,j),emit_oc_yr(i,m,j), &
                        emit_bc_yr(i,m,j),smfuncveg_yr(i,m,j), &
                        luc_emc_yr_t(i,m),lucltrin_yr_t(i,m), &
                        lucsocin_yr_t(i,m),burnfrac_yr(i,m,j)*100., &
                        bterm_yr(i,m,j),lterm_yr_t(i,m),mterm_yr(i,m,j), &
                        ' TILE ',m,' PFT ',j,' FRAC ' &
                        ,fcancmxrow(i,m,j)
                    end if
                end do !j

                if (nmtest > 1) then
                    !> Write out the per tile values
                    write(87,8108)iyear,emit_co2_yr_t(i,m), &
                        emit_co_yr_t(i,m),emit_ch4_yr_t(i,m), &
                        emit_nmhc_yr_t(i,m),emit_h2_yr_t(i,m), &
                        emit_nox_yr_t(i,m),emit_n2o_yr_t(i,m), &
                        emit_pm25_yr_t(i,m),emit_tpm_yr_t(i,m), &
                        emit_tc_yr_t(i,m),emit_oc_yr_t(i,m), &
                        emit_bc_yr_t(i,m),smfuncveg_yr_t(i,m), &
                        luc_emc_yr_t(i,m),lucltrin_yr_t(i,m), &
                        lucsocin_yr_t(i,m),burnfrac_yr_t(i,m)*100., &
                        bterm_yr_t(i,m),lterm_yr_t(i,m),mterm_yr_t(i,m), &
                        ' TILE ',m,' OF ',nmtest,' TFRAC ',FAREROT(i,m)
                end if
            end do !m

            !> Finally write out the per gridcell values
            write(87,8108)iyear,emit_co2_yr_g(i), &
                emit_co_yr_g(i),emit_ch4_yr_g(i),emit_nmhc_yr_g(i), &
                emit_h2_yr_g(i),emit_nox_yr_g(i),emit_n2o_yr_g(i), &
                emit_pm25_yr_g(i),emit_tpm_yr_g(i),emit_tc_yr_g(i), &
                emit_oc_yr_g(i),emit_bc_yr_g(i),smfuncveg_yr_g(i), &
                luc_emc_yr_g(i),lucltrin_yr_g(i), &
                lucsocin_yr_g(i),burnfrac_yr_g(i)*100.,bterm_yr_g(i), &
                lterm_yr_g(i),mterm_yr_g(i), ' GRDAV'

        endif !dofire,lnduseon

        !> Write fraction of each pft and bare

        if (compete .or. lnduseon) then
            sumfare=0.0
            if (onetile_perPFT) then
                do m=1,nmos
                    sumfare=sumfare+FAREROT(i,m)
                enddo
                write(89,8107)iyear,(FAREROT(i,m)*100.,m=1,nmos), &
                            sumfare,(pftexistrow(i,j,j),j=1,icc)
            else  !composite/mosaic (normal) runs
                do m=1,nmtest
                    sumfare=0.0
                    do j=1,icc
                        sumfare=sumfare+fcancmxrow(i,m,j)
                    enddo
                    write(89,8107)iyear,(fcancmxrow(i,m,j)*100., &
                        j=1,icc),(1.0-sumfare)*100.,sumfare, &
                        (pftexistrow(i,m,j),j=1,icc),' TILE ',m
                end do
            endif
        endif !compete/lnduseon

        if (dowetlands .or. obswetf) then
            !> Write out the per tile values
            if (nmtest > 1) then
                do m = 1,nmtest
                    write(92,8115)iyear,ch4wet1_yr_t(i,m), &
                        ch4wet2_yr_t(i,m),wetfdyn_yr_t(i,m), &
                        ch4dyn1_yr_t(i,m),ch4dyn2_yr_t(i,m), &
                        ch4soills_yr_t(i,m),' TILE ',m
                end do
            end if
        write(92,8115)iyear,ch4wet1_yr_g(i), &
                    ch4wet2_yr_g(i),wetfdyn_yr_g(i), &
                    ch4dyn1_yr_g(i),ch4dyn2_yr_g(i), &
                    ch4soills_yr_g(i),' GRDAV '
        endif

    endif ! if iday=365/366
882     continue ! i

if ((.not.leapnow .and.iday.eq.365).or.(leapnow .and.iday.eq.366)) then

!> Reset all annual vars in preparation:
    call resetyearend(nltest,nmtest)

end if


8105  FORMAT(1X,I5,16(ES12.5,1X),2(A6,I2),A6,F8.2)
8107  FORMAT(1X,I5,11(F12.7,1X),9L5,2(A6,I2))
8108  FORMAT(1X,I5,20(ES12.5,1X),2(A6,I2),A6,F8.2)
8115  FORMAT(1X,I5,6(ES12.5,1X),2(A6,I2))

end subroutine ctem_annual_aw
!>@}
!==============================================================================================================
!>\ingroup io_driver_close_outfiles
!>@{
subroutine close_outfiles()
                     
use ctem_statevars, only : c_switch

implicit none

! pointers

logical, pointer :: dofire
logical, pointer :: lnduseon
logical, pointer :: compete
logical, pointer :: dowetlands
logical, pointer :: obswetf
logical, pointer :: parallelrun

! point pointers

dofire                => c_switch%dofire
lnduseon              => c_switch%lnduseon
compete               => c_switch%compete
dowetlands            => c_switch%dowetlands
obswetf               => c_switch%obswetf
parallelrun           => c_switch%parallelrun

! -----------------------

      IF (.NOT. PARALLELRUN) THEN
        close(71)
        close(711)
         close(72)
         close(73)
         close(74)
         close(75)
         !close(76)
         if (dofire .or. lnduseon) then
          close(77)
         end if

        if (compete .or. lnduseon) then
          close(78)
        endif

        if (dowetlands .or. obswetf) then
        close(79)
        endif 

      endif ! if (.not. parallelrun) 

!>CLOSE CLASS OUTPUT FILES      
      CLOSE(81)
      CLOSE(82)
      CLOSE(83)
!>then the CTEM ones
      close(84)
      close(86)
      if (dofire .or. lnduseon) then
       close(85)
       close(87)
      endif
      if (compete .or. lnduseon) then
       close(88)
       close(89)
      endif
 
      if (dowetlands .or. obswetf) then 
       close(91)
       close(92)
      endif 


end subroutine close_outfiles
!@}

end module io_driver      
