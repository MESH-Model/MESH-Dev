!>\defgroup landuse_change_initialize_luc
!>Canadian Terrestrial Ecosystem Model (CTEM) 
!>LUC Initial Read-In Subroutine 
!------------------------------------------------------------------------------------
!>\defgroup landuse_change_readin_luc
!>Canadian Terrestrial Ecosystem Model (CTEM) 
!!LUC Annual Read-In Subroutine 
!------------------------------------------------------------------------------------
!>\defgroup landuse_change_luc
!>
!>Canadian Terrestrial Ecosystem Model (CTEM) 
!>Land Use Change Subroutine
!!
!!The land use change (LUC) module of CTEM is based on \cite Arora2010-416 and briefly
!! described here. When the area of crop PFTs changes, CTEM generates LUC emissions.
!! In the simulation where fractional coverage of PFTs is specified, the changes in
!! fractional coverage of crop PFTs are made consistent with changes in the
!! fractional coverage of natural non-crop PFTs. That is, an increase or decrease
!! in the area of crop PFTs is associated with a corresponding decrease or increase
!! in the area of non-crop PFTs. This approach is taken by \cite Wang2006-he, which
!! allows one to reconstruct historical land cover given a spatial data set of 
!!changes in crop area over the historical period and an estimate of potential
!! natural land cover for the pre-industrial period (as described in Sect. 
!!\ref{methods}). When competition between PFTs for space is allowed, only the
!! fractional coverage of crop PFTs is specified. Similar to a simulation with 
!!prescribed PFT fractions, when the area of crop PFTs increases, the fractional
!! coverage of non-crop PFTs is decreased in proportion to their existing coverage
!! \cite Wang2006-he. Alternatively, and in contrast to the simulation with prescribed
!! PFT fractions, when the area of crop PFTs decreases then the generated bare 
!!fraction is available for recolonization by non-crop PFTs.
!!
!!A decrease in the area of natural non-crop PFTs, associated with an increase in
!! area of crop PFTs, results in deforested biomass (while the term 
!!\f$\textit{deforested}\f$ implies clearing of forests, the same processes can
!! occur in grasslands as well and is meant here to imply removal of the biomass).
!! The deforested biomass is divided into three components: (i) the component that
!! is combusted or used for fuel wood immediately after natural vegetated is 
!!deforested and which contributes to atmospheric \f$CO_2\f$, (ii) the component
!! left as slash or used for pulp and paper products and (iii) the component that 
!!is used for long-lasting wood products. The fractions allocated to these three
!! components depend on whether the PFTs are woody or herbaceous and on their
!! aboveground vegetation biomass density (see Table 1 of \cite Arora2010-416). 
!!To account for the timescales involved, the fraction allocated to slash or pulp
!! and paper products is transferred to the model's litter pool and the fraction
!! allocated to long-lasting wood products is allocated to the model's soil carbon
!! pool. Land use change associated with a decrease in the area of natural
!! vegetation thus redistributes carbon from living vegetation to dead litter
!! and soil carbon pools and emits \f$CO_2\f$ to the atmosphere through direct
!! burning of the deforested biomass. The net result is positive LUC carbon 
!!emissions from land to the atmosphere.
!!
!!When croplands are abandoned, the area of natural PFTs increases. In simulations
!! with prescribed fractional coverage of PFTs this results in a decreased carbon
!! density for all model pools as the same amount of carbon is spread over a larger
!! fraction of the grid cell. This reduced density implies that natural vegetation 
!!is able to take up carbon as it comes into equilibrium with the driving climate
!! and atmospheric \f$CO_2\f$ concentration. This creates the carbon sink associated
!! with abandonment of croplands as natural vegetation grows in its place. In
!! simulations with competition between PFTs, the abandoned land is treated as
!! bare ground, which is subsequently available for recolonization, as mentioned
!! above. As natural vegetation expands into bare ground it takes up carbon, again
!! creating the carbon sink associated with abandonment of croplands. The net
!! result is negative LUC carbon emissions as carbon is taken from atmosphere to
!! grow vegetation over the area that was previously a cropland.
!!
!------------------------------------------------------------------------------------
!>\defgroup landuse_change_adjust_luc_fracs
!>
!>this subroutine adjusts the amount of each pft to ensure that the fraction of
!> gridcell bare ground is >0.
!------------------------------------------------------------------------------------
!>\defgroup landuse_change_adjust_fracs_comp
!>
!>This subroutine is used when compete = true. It adjusts the amount of each pft
!> to allow expansion of cropland.
!------------------------------------------------------------------------------------

!>\file
!!Central module for all land use change operations
module landuse_change

! J. Melton. Jan 11, 2013
use ctem_params

implicit none

! subroutines contained in this module:
public  :: initialize_luc
public  :: readin_luc
public  :: luc
private :: adjust_luc_fracs
private :: adjust_fracs_comp

contains

!-------------------------------------------------------------------------------------------------------------
!>\ingroup landuse_change_initialize_luc
!>@{

subroutine initialize_luc(iyear,lucdat,nmtest,nltest,&
                          nol2pfts,cyclemet,   &
                          cylucyr,lucyr,fcanrow,farerow,nfcancmxrow,  &
                          pfcancmxrow,fcancmxrow,reach_eof,start_bare,&
                          compete,onetile_perPFT)
!
!    11  Jul  2016  - Further bug fixes for competing for space within a tile
!     J. Melton
!
!     9  Mar  2016  - Adapt for tiling where we compete for space within a tile
!     J. Melton
!
!     3  Feb  2016  - Remove mosaic flag and replace it with the onetile_perPFT one.
!     J. Melton

!     9  Jan. 2013  - this subroutine takes in luc information from
!     J. Melton       a luc file and adapts them for runclassctem
!                     it is run once to set up the luc info before the 
!                     model timestepping begins.
!
!     7  Feb. 2014  - Adapt it to work with competition and start_bare
!     J. Melton 
!
!     28 Mar. 2014  - Add in check to ensure we don't make a negative bare
!     J. Melton       fraction when we add in seed fractions.
!

use ctem_params,        only : nmos,nlat,icc,ican,icp1,seed,crop,numcrops,minbare

implicit none

! arguments:

! inputs
integer, intent(in) :: iyear
character(80), intent(in) :: lucdat
integer, intent(in) :: nmtest
integer, intent(in) :: nltest
integer, dimension(ican), intent(in) :: nol2pfts
logical, intent(in) :: cyclemet
integer, intent(in) :: cylucyr 
logical, intent(in) :: start_bare
logical, intent(in) :: compete

logical, intent(in) :: onetile_perPFT !<if you are running with one tile per PFT in mosaic mode, set to true. Changes
                                !< how competition is run. Specifically it allows competition between tiles. This
                                !< is not recommended for any case where you don't have one PFT in each tile as it
                                !< has not been tested for that.

! updates
real, dimension(nlat,nmos,icp1), intent(inout) :: fcanrow
real, dimension(nlat,nmos),      intent(inout) :: farerow
logical, intent(inout) :: reach_eof

! outputs
real, dimension(nlat,nmos,icc), intent(out) :: pfcancmxrow
real, dimension(nlat,nmos,icc), intent(out) :: fcancmxrow
real, dimension(nlat,nmos,icc), intent(out) :: nfcancmxrow
integer, intent(out) :: lucyr

! local variables:
real, dimension(icc) :: temparray
real, dimension(nlat) :: barf
real, dimension(nlat,nmos) :: barfm
integer, dimension(2) :: bigpftc
integer, dimension(1) :: bigpft
real, dimension(nlat,nmos,icc-numcrops) :: pftarrays ! temp variable
integer, dimension(nlat,nmos,icc-numcrops) :: indexposj ! temp var
integer, dimension(nlat,nmos,icc-numcrops) :: indexposm ! temp var
real :: temp
integer :: j,m,i,n,k
integer :: k2,k1,strlen

!>-------------------------
!> Initialize barefraction to 1.0
barf=1.0
barfm = 1.0
pftarrays=0.

!>reset the composite fcanrow as it is appended on later in a loop
    if (.not. onetile_perPFT) fcanrow = 0.0

!>it is the first year, so prepare the luc data:

!>open the luc file

open(unit=15,file=lucdat(1:strlen(lucdat))//'.LUC')

!>Skip first three rows:
read(15,*)
read(15,*)
read(15,*)
!>
!>get first year of luc data
!>note we load the nfcancmx, not pfcancmx array. this is because this
!>nfcancmx value is passed to the pfcancmx array at the start of each simulation year
!>
do i = 1, nltest
    if (.not. onetile_perPFT) then  !composite
    read (15,*) lucyr,(nfcancmxrow(i,1,j),j=1,icc)
    if (nmtest > 1) then
        do m = 2, nmtest
        nfcancmxrow(i,m,:) = nfcancmxrow(i,1,:)
        end do
    end if
    else                    !onetile_perPFT
    read (15,*) lucyr,(temparray(j),j=1,icc)
    do m = 1, nmtest-1 !as nmtest-1 = icc
        j = m
        nfcancmxrow(i,m,j) = temparray(m)
    enddo !m loop
    endif
enddo !nltest

!>
!> next update our luc data if either of the following conditions are met:
!!
!! 1) we are cycling the met data and the luc year we just read in is less
!! than the year we want to cycle over (assuming it is not defaulted to
!! -9999) or,
!!
!! 2) we are not cycling over the met data so we just want to get the same
!! year of luc data as the met data we read in above in preparation for our
!! transient run.
!!
do while ((cyclemet .and. lucyr .lt. cylucyr             &
    .and. cylucyr .ne. -9999) .or. (.not. cyclemet .and.  &
    lucyr .lt. iyear))
!           get the luc data
    do i = 1, nltest
        if (.not. onetile_perPFT) then  !composite
        read (15,*,end=999) lucyr,(nfcancmxrow(i,1,j),j=1,icc)
        if (nmtest > 1) then
            do m = 2, nmtest
            nfcancmxrow(i,m,:) = nfcancmxrow(i,1,:)
            end do
        end if
        else                    !onetile_perPFT
        read (15,*,end=999) lucyr,(temparray(j),j=1,icc)
        do m = 1, nmtest-1 !nmtest-1 same as icc
        j = m
        nfcancmxrow(i,m,j) = temparray(m)
        enddo !m loop
        endif
    enddo !nltest
enddo  !while loop
!>
!! If you are running with start_bare on, take in only the
!! crop fractions, set rest to seed. If compete, but not start bare, then
!! just make sure you have at least seed for each pft.
!!
n=1
k=1
if (compete) then
    do i = 1, nltest
        do m = 1, nmtest
            do j = 1, icc
                if (.not. crop(j)) then
                    if (start_bare) then
                        nfcancmxrow(i,m,j)=seed
                    else !>not starting bare, but still make sure you have at least seed
                        nfcancmxrow(i,m,j)=max(seed,fcancmxrow(i,m,j))
                    end if
                    if (.not. onetile_perPFT) then
                        barfm(i,m) = barfm(i,m) - nfcancmxrow(i,m,j)
                    else
                        barf(i) = barf(i) - nfcancmxrow(i,m,j)
                    end if
                    !> Keep track of the non-crop nfcancmx for use in loop below.
                    !> pftarrays keeps track of the nfcancmxrow for all non-crops
                    !> indexposj and indexposm store the index values of the non-crops
                    !> in a continuous array for use later. n and k are then the indexes used by
                    !> these arrays.
                    if (.not. onetile_perPFT) n=m
                    pftarrays(i,n,k) = nfcancmxrow(i,m,j)
                    indexposj(i,n,k) = j
                    indexposm(i,n,k) = m
                    n = n+1
                    k = k+1
                    if (.not. onetile_perPFT .and. j == icc) k=1
                end if !crops
            end do !icc
        end do !nmtest
    end do !nltest
end if  ! compete

!> check that in making these seed fraction we haven't made our total fraction
!> more than 1.0.
do i=1,nltest
    if (onetile_perPFT) then
        if (barf(i) .lt. 0.) then
            !> Find out which of the non-crop PFTs covers the largest area.
            bigpftc=maxloc(pftarrays(i,:,:))

            !> j is then the nmos index and m is the icc index of the PFT with the largest area
            j = indexposj(i,bigpftc(1),bigpftc(2))
            m = indexposj(i,bigpftc(1),bigpftc(2))

            !> Reduce the most dominant PFT by barf and minbare. The extra
            !> amount is to ensure we don't have trouble later with an extremely
            !> small bare fraction. barf is a negative value.
            nfcancmxrow(i,m,j)=nfcancmxrow(i,m,j)+barf(i) - minbare

        end if
    else
        do m = 1,nmtest
            if (barfm(i,m) .lt. 0.) then

                !> Find out which of the non-crop PFTs covers the largest area.
                bigpft=maxloc(pftarrays(i,m,:))
                !> j is then the nmos index and m is the icc index of the PFT with the largest area
                j = indexposj(i,m,bigpft(1))
                !>
                !> Reduce the most dominant PFT by barf and minbare. The extra
                !! amount is to ensure we don't have trouble later with an extremely
                !! small bare fraction. barf is a negative value.
                !!
                nfcancmxrow(i,m,j)=nfcancmxrow(i,m,j)+barfm(i,j) - minbare

            end if

        end do !nmtest
    end if !onetile_perPFT
end do !nltest


!> get fcans for use by class using the nfcancmxs just read in
k1=0
do 997 j = 1, ican
    if(j.eq.1) then
    k1 = k1 + 1
    else
    k1 = k1 + nol2pfts(j-1)
    endif
    k2 = k1 + nol2pfts(j) - 1
    do 998 n = k1, k2
    do i = 1, nltest
        do m = 1, nmtest
        if (.not. onetile_perPFT) then !composite

        fcanrow(i,m,j)=fcanrow(i,m,j)+nfcancmxrow(i,m,n)

        else if (onetile_perPFT .and. nfcancmxrow(i,m,n) .gt. seed) then
!>this tile has some plants so overwrite the seed fraction with an actual fraction
!!
!!note: the seed fraction has already been assigned in runclassctem prior to entering this subroutine.
        farerow(i,m)=nfcancmxrow(i,m,n)

        endif
        enddo
    enddo
998       continue
997     continue

!> (re)find the bare fraction for farerow(i,iccp1)
if (onetile_perPFT) then
    do i = 1, nltest
        temp = 0.
        do m = 1, nmtest-1
            temp = temp + farerow(i,m)
        enddo
        farerow(i,nmtest) = 1.0 - temp
    enddo
endif
!>
!!check that the bare fraction is possible (>0) and if not then reduce the other pfts proportionally 
!!to make a non-negative bare ground fraction.
!!
do i = 1, nltest
    temp = 0.0
    if (onetile_perPFT) then
    !> competition requires a 'seed' fraction so make sure the bare ground is also that big.
    !> for prescribed runs you just need it to be possible (>0).
        if ((compete .and. farerow(i,nmtest) < seed) .or. (.not. compete .and. farerow(i,nmtest) < 0.)) then

            call adjust_luc_fracs(i,onetile_perPFT,nfcancmxrow,farerow(i,nmtest),compete)

            do m = 1, nmtest
            n = m
            farerow(i,m)=nfcancmxrow(i,m,n)
            temp = temp + farerow(i,m)
            enddo

            farerow(i,nmtest) = 1.0 - temp

        endif !farerow<seed
    endif !onetile_perPFT
enddo !nltest

!> assign the present pft fractions from those just read in
do j = 1, icc
    do i = 1, nltest
        do m = 1, nmtest
            if (.not. onetile_perPFT) then  !composite
                fcancmxrow(i,m,j)=nfcancmxrow(i,m,j)
                pfcancmxrow(i,m,j)=nfcancmxrow(i,m,j)
            else !onetile_perPFT
                ! I think this check below is not needed (JM Mar 2015)
                if (compete) then
        !              ensure that the fraction is >= seed
                    pfcancmxrow(i,m,j)=max(seed,nfcancmxrow(i,m,j))
                else !prescribed run
                    pfcancmxrow(i,m,j)=max(0.,nfcancmxrow(i,m,j))
                end if
            endif
        enddo
    enddo
enddo
!>
!> back up one year in the luc file
!! this is because we were setting things up here,
!! we will later call readin_luc so want the file to be
!! rewound prior to that to the proper start year.
!!
do i = 1, nltest
    backspace(15)
enddo

return

999    continue
  
!> end of the luc file is reached. close and tell main program to exit
close(15)
reach_eof = .true.

end subroutine initialize_luc
!>@}
!=======================================================================
!>\ingroup landuse_change_readin_luc
!>@{

subroutine readin_luc(iyear,nmtest,nltest,lucyr, &
                      nfcancmxrow,pfcancmxrow,reach_eof,compete,&
                      onetile_perPFT)
!     9  Mar  2016  - Adapt for tiling where we compete for space within a tile
!     J. Melton
!
!     3  Feb  2016  - Remove mosaic flag, replace with onetile_perPFT flag.
!     J. Melton

!     9  Jan. 2013  - this subroutine takes in luc information from
!     J. Melton       a luc file annually and adapts them for runclassctem
!	
!     7  Feb. 2014  - Adapt it to work with competition
!     J. Melton 
	      
use ctem_params,        only : nmos,nlat,icc,seed,crop

implicit none

! arguments

! inputs
integer, intent(in) :: iyear
integer, intent(in) :: nmtest
integer, intent(in) :: nltest
logical, intent(in) :: compete
logical, intent(in) :: onetile_perPFT !< if you are running with one tile per PFT in mosaic mode, set to true. Changes
                                !< how competition is run. Specifically it allows competition between tiles. This
                                !< is not recommended for any case where you don't have one PFT in each tile as it
                                !< has not been tested for that.

! updates
integer, intent(inout) :: lucyr
logical, intent(inout) :: reach_eof

! outputs
real, dimension(nlat,nmos,icc), intent(out) :: nfcancmxrow
real, dimension(nlat,nmos,icc), intent(in)  :: pfcancmxrow

! local variables
real, dimension(icc) :: temparray
real :: temp
integer :: j,m,i
integer :: k1,k2,n
real, dimension(nltest) :: bare_ground_frac

!>-------------------------

!>it is subsequent years so read in and adjust the luc file info.

         do while (lucyr < iyear) 
           do i = 1, nltest
            if (.not. onetile_perPFT) then  !composite
              read (15,*,end=999) lucyr,(nfcancmxrow(i,1,j),j=1,icc)
              if (nmtest > 1) then
                do m = 2, nmtest
                    nfcancmxrow(i,m,:) = nfcancmxrow(i,1,:)
                end do
              end if
            else                    !onetile_perPFT
              read (15,*,end=999) lucyr,(temparray(j),j=1,icc)
              do m = 1, nmtest-1    !nmtest-1 same as icc
               j = m
               if (compete) then
                  nfcancmxrow(i,m,j) = max(seed,temparray(m)) 
               else !prescribed run
                  nfcancmxrow(i,m,j) = max(0.,temparray(m)) 
               end if   
              enddo !m loop
            endif
           enddo !nltest
         enddo !lucyr<iyear
!>
!>If compete is on, then only take in the crop fraction. Set the other fractions
!>to the same as before. These will be adjusted in adjust_luc_fracs.
        if (compete) then
         do j = 1, icc
          if (.not. crop(j)) then
           do i = 1, nltest
             do m = 1,nmtest
                nfcancmxrow(i,m,j)=pfcancmxrow(i,m,j)
             end do
           end do
          end if
         end do
        end if


!>(re)find the bare fraction for farerow(i,iccp1)
          bare_ground_frac=0.
          do i = 1, nltest
          temp = 0.0
           do j = 1, icc
            if (onetile_perPFT) then !flag
              m = j
            else !composite
              m = 1
            end if 
            temp = temp + nfcancmxrow(i,m,j)
           enddo

            bare_ground_frac(i) = 1.0- temp

          enddo
          
          do i = 1, nltest
           if ((compete .and. bare_ground_frac(i) < seed) .or. (.not. compete .and. bare_ground_frac(i) < 0.)) then

             call adjust_luc_fracs(i,onetile_perPFT,nfcancmxrow,bare_ground_frac(i),compete)

           endif 
          enddo !nltest

return

999 continue

!>end of the luc file is reached. close and tell main program to exit
        close(15)
        reach_eof = .true.

end subroutine readin_luc
!>@}
!=======================================================================
!>\ingroup landuse_change_luc
!>@{

subroutine    luc(         il1,       il2,  nilg,      nol2pfts,    & !1    
                        grclarea, pfcancmx, nfcancmx,      iday,    & !2    
                         todfrac,  yesfrac, interpol,compete,  leapnow, & !3
!    ----------------------- inputs above this line -------------       
                         gleafmas, bleafmas, stemmass, rootmass,    & !4  
                         litrmass, soilcmas, vgbiomas, gavgltms,    & !5   
                         gavgscms,  fcancmx,   fcanmx,              & !6
!    ----------- updates above this line, outputs below ---------
                         lucemcom, lucltrin, lucsocin)                !7  
!
!     ----------------------------------------------------------------
!
!     31  Jan 2014  - Moved parameters to global file (ctem_params.f90)
!     J. Melton
!
!     18  Apr. 2013 - made it so that you will exit luc if the grid cell has
!     J. Melton       no actual luc in this timestep. removed some extraneous checks.
!
!     02  Jan. 2004 - this subroutine deals with the changes in the land
!     V. Arora        cover and estimates land use change (luc)
!                     related carbon emissions. based on whether there
!                     is conversion of forests/grassland to crop area,
!                     or croplands abandonment, this subroutine
!                     reallocates carbon from live vegetation to litter
!                     and soil c components. the decomposition from the
!                     litter and soil c pools thus implicitly models luc
!                     related carbon emissions. set of rules are
!                     followed to determine the fate of carbon that
!                     results from deforestation or replacement of
!                     grasslands by crops. 
!
!     ----------------------------------------------------------------
!     inputs
!
!     icc       - no of pfts for use by ctem, currently 9
!     ican        - no of pfts for use by class, currently 4
!     ----------------------------------------------------------------    
      use ctem_params,        only : icc, ican, zero, km2tom2, iccp1, &
                                     combust, paper, furniture, bmasthrs, &
                                     tolrnce1, tolrance, crop, numcrops, &
                                     minbare

      implicit none

      integer il1               !<il1=1
      integer il2               !<il2=nilg
      integer nilg              !<no. of grid cells in latitude circle(this is passed in as either ilg or nlat depending on comp/mos)
      integer i, j, k, m, n, k1, k2, q !FLAG q needed? JM
      integer iday              !<day of year
      integer nol2pfts(ican)    !<number of level 2 pfts
      integer fraciord(nilg,icc)!<fractional coverage increase or decrease increase +1, decrease -1
      integer treatind(nilg,icc)!<treatment index for combust, paper, & furniture
      integer bareiord(nilg)    !<bare fraction increases or decreases
      integer lrgstpft(1)       !<
      logical leapnow           !< true if this year is a leap year. Only used if the switch 'leap' is true.
      logical  interpol         !<if todfrac & yesfrac are provided then interpol must be set to false so that 
                                !<this subroutine doesn't do its own interpolation using pfcancmx and nfcancmx 
                                !<which are year end values
      logical  luctkplc(nilg)   !<
      logical  compete          !<true if the competition subroutine is on.

      real gleafmas(nilg,icc)   !<green or live leaf mass in kg c/m2, for the 9 pfts
      real bleafmas(nilg,icc)   !<brown or dead leaf mass in kg c/m2, for the 9 pfts
      real stemmass(nilg,icc)   !<stem biomass in kg c/m2, for the 9 pfts
      real rootmass(nilg,icc)   !<root biomass in kg c/m2, for the 9 pfts
      real fcancmx(nilg,icc)    !<max. fractional coverages of ctem's 9 pfts.
      real pfcancmx(nilg,icc)   !<previous max. fractional coverages of ctem's 9 pfts.
      real vgbiomas(nilg)       !<grid averaged vegetation biomass, kg c/m2
      real soilcmas(nilg,iccp1) !<soil c mass in kg c/m2, for the 9 pfts + bare
      real litrmass(nilg,iccp1) !<litter mass in kg c/m2, for the 9 pfts + bare
      real gavgltms(nilg)       !<grid averaged litter mass, kg c/m2
      real gavgscms(nilg)       !<grid averaged soil c mass, kg c/m2
      real nfcancmx(nilg,icc)   !<next max. fractional coverages of ctem's 9 pfts.
      real fcancmy(nilg,icc)    !<
      real todfrac(nilg,icc)    !<today's fractional coverage of all pfts
      real yesfrac(nilg,icc)    !<yesterday's fractional coverage of all pfts

      real fcanmx(nilg,ican)    !<fractional coverages of class 4 pfts (these are found based on new fcancmxs)
      real delfrac(nilg,icc)    !<
      real abvgmass(nilg,icc)   !<above-ground biomass
      real grclarea(nilg)       !<gcm grid cell area, km2
      real combustc(nilg,icc)   !<total carbon from deforestation- combustion
      real paperc(nilg,icc)     !<total carbon from deforestation- paper
      real furnturc(nilg,icc)   !<total carbon from deforestation- furniture
      real incrlitr(nilg,icc)   !<
      real incrsolc(nilg,icc)   !<
      real chopedbm(nilg)       !<chopped off biomass
      real compdelfrac(nilg,icc)!<with competition on, this is the change in pft frac per timestep   

      real redubmas1      !<
      real term           !<
      real barefrac(nilg) !<initialize bare fraction to 1.0
      real grsumcom(nilg) !<grid sum of combustion carbon for all pfts that are chopped
      real grsumpap(nilg) !<grid sum of paper carbon for all pfts that are chopped
      real grsumfur(nilg) !<grid sum of furniture carbon for all pfts that are chopped
      real grsumlit(nilg) !<grid sum of litter carbon for all pfts that are chopped
      real grsumsoc(nilg) !<grid sum of soil c carbon for all pfts that are chopped
      real pbarefra(nilg) !<initialize previous years's bare fraction to 1.0
      real grdencom(nilg) !<grid averaged densities for combustion carbon
      real grdenpap(nilg) !<grid averaged densities for paper carbon
      real grdenfur(nilg) !<grid averaged densities for furniture carbon
      real grdenlit(nilg) !<grid averaged densities for litter carbon
      real grdensoc(nilg) !<grid averaged densities for soil c carbon
      real totcmass(nilg) !<total c mass (live+dead)
      real totlmass(nilg) !<total c mass (live)
      real totdmas1(nilg) !<total c mass (dead) litter
      real ntotcmas(nilg) !<total c mass (live+dead) after luc treatment
      real ntotlmas(nilg) !<total c mass (live) after luc treatment
      real ntotdms1(nilg) !<total c mass (dead) litter after luc treatment
      real lucemcom(nilg) !<luc related carbon emission losses from combustion u-mol co2/m2.sec
      real pvgbioms(nilg) !<
      real pgavltms(nilg) !<
      real pgavscms(nilg) !<
      real redubmas2      !<
      real lucltrin(nilg) !<luc related input to litter pool, u-mol co2/m2.sec
      real lucsocin(nilg) !<luc related input to soil carbon pool, u-mol co2/m2.sec
      real totdmas2(nilg) !<total c mass (dead) soil c
      real ntotdms2(nilg) !<total c mass (dead) soil c after luc treatment

      real pftarrays(nilg,icc-numcrops) !<
      integer indexpos(nilg,icc-numcrops) !<

!>---------------------------------------------------------------
!>Constants and parameters are located in ctem_params.f90
!>---------------------------------------------------------------


      if(icc.ne.9)                               call xit('luc',-1)  
      if(ican.ne.4)                              call xit('luc',-2)  

!>------------------------------------------------------------------

!>find/use provided current and previous day's fractional coverage 
!>if competition is on, we will adjust these later.

      if(interpol) then !> perform interpolation 
       do 110 j = 1, icc
        do 111 i = il1, il2
          if (compete .and. .not. crop(j)) then !FLAG!! JM. ADDED if loop JUL 11 2016 TEST!!!
            nfcancmx(i,j)=yesfrac(i,j)
            pfcancmx(i,j)=yesfrac(i,j)
          end if
          delfrac(i,j)=nfcancmx(i,j)-pfcancmx(i,j) !change in fraction

          if (leapnow) then 
            delfrac(i,j)=delfrac(i,j)/366.0
          else 
            delfrac(i,j)=delfrac(i,j)/365.0
          endif

          fcancmx(i,j)=pfcancmx(i,j)+(real(iday)*delfrac(i,j)) !  current day
          fcancmy(i,j)=pfcancmx(i,j)+(real(iday-1)*delfrac(i,j)) ! previous day

          if( fcancmx(i,j).lt.0.0.and.abs(fcancmx(i,j)).lt.1.0e-05)then
            fcancmx(i,j)=0.0
          else if( fcancmx(i,j).lt.0.0.and. &
                abs(fcancmx(i,j)).ge.1.0e-05)then
            write(6,*)'fcancmx(',i,',',j,')=',fcancmx(i,j)
            write(6,*)'fractional coverage cannot be negative'
            call xit('luc',-4)
          endif

          if(fcancmy(i,j).lt.0.0.and.abs(fcancmy(i,j)).lt.1.0e-05)then    
            fcancmy(i,j)=0.0
          else if( fcancmy(i,j).lt.0.0.and. &
         abs(fcancmy(i,j)).ge.1.0e-05)then
            write(6,*)'fcancmy(',i,',',j,')=',fcancmy(i,j)
            write(6,*)'fractional coverage cannot be negative'
            call xit('luc',-5)
          endif

111     continue
110    continue
      else !> use provided values but still check
!>they are not -ve
       do 115 j = 1, icc
        do 116 i = il1, il2
          fcancmx(i,j) = todfrac(i,j)   
          fcancmy(i,j) = yesfrac(i,j)   

          if( fcancmx(i,j).lt.0.0.and.abs(fcancmx(i,j)).lt.1.0e-05)then
            fcancmx(i,j)=0.0
          else if( fcancmx(i,j).lt.0.0.and. &
         abs(fcancmx(i,j)).ge.1.0e-05)then
            write(6,*)'fcancmx(',i,',',j,')=',fcancmx(i,j)
            write(6,*)'fractional coverage cannot be negative'
            call xit('luc',-4)
          endif

          if(fcancmy(i,j).lt.0.0.and.abs(fcancmy(i,j)).lt.1.0e-05)then
            fcancmy(i,j)=0.0
          else if( fcancmy(i,j).lt.0.0.and. &
         abs(fcancmy(i,j)).ge.1.0e-05)then
            write(6,*)'fcancmy(',i,',',j,')=',fcancmy(i,j)
            write(6,*)'fractional coverage cannot be negative'
            call xit('luc',-5)
          endif

116     continue
115    continue
      endif
!>
!>If competition is on, we need to adjust the other fractions for the increase/decrease
!>in cropland as only the crops areas is now specified.
      if (compete) then

         call adjust_fracs_comp(il1,il2,nilg,iday,pfcancmx,yesfrac,delfrac,compdelfrac)

         do j = 1, icc
          do i = 1, il1, il2

            if (.not. crop(j)) then
             fcancmx(i,j)=yesfrac(i,j)+compdelfrac(i,j) !  current day
             fcancmy(i,j)=yesfrac(i,j) ! previous day
            end if
          end do

         end do
      end if
!>
!!check if this year's fractional coverages have changed or not for any pft
!!
      luctkplc(:)=.false.  ! did land use change take place for any pft in this grid cell
      do 200 j = 1, icc
        do 250 i = il1, il2
          if ( (abs(fcancmx(i,j)-fcancmy(i,j))).gt.zero ) then
            luctkplc(i)=.true. ! yes, luc did take place in this grid cell
          endif
250     continue
200   continue


!     only perform the rest of the subroutine if any luc is actually taking 
!     place, otherwise exit.
      do 255 i = il1, il2
        if (luctkplc(i)) then
!     -------------------------------------------------------------------
!     initialization

      do 260 j = 1, ican
            fcanmx(i,j)=0.0 ! fractional coverage of class' pfts
260   continue

      do 270 j = 1, icc
          fraciord(i,j)=0   !fractional coverage increase or decrease
!                           !increase +1, decrease -1
          abvgmass(i,j)=0.0 !above-ground biomass
          treatind(i,j)=0   !treatment index for combust, paper, & furniture
          combustc(i,j)=0.0 !total carbon from deforestation- combustion
          paperc(i,j)=0.0   !total carbon from deforestation- paper
          furnturc(i,j)=0.0 !total carbon from deforestation- furniture
270   continue

        pvgbioms(i)=vgbiomas(i)  ! store grid average quantities in
        pgavltms(i)=gavgltms(i)  ! temporary arrays
        pgavscms(i)=gavgscms(i)

        vgbiomas(i)=0.0
        gavgltms(i)=0.0
        gavgscms(i)=0.0


        barefrac(i)=1.0          
        pbarefra(i)=1.0           

        grsumcom(i)=0.0           
        grsumpap(i)=0.0           
        grsumfur(i)=0.0          
        grsumlit(i)=0.0          
        grsumsoc(i)=0.0          

        grdencom(i)=0.0          
        grdenpap(i)=0.0          
        grdenfur(i)=0.0           
        grdenlit(i)=0.0          
        grdensoc(i)=0.0          

        totcmass(i)=0.0           
        totlmass(i)=0.0           
        totdmas1(i)=0.0           
        totdmas2(i)=0.0           

        ntotcmas(i)=0.0           
        ntotlmas(i)=0.0           
        ntotdms1(i)=0.0           
        ntotdms2(i)=0.0           

        lucemcom(i)=0.0          
        lucltrin(i)=0.0           
        lucsocin(i)=0.0           

        bareiord(i)=0             
        chopedbm(i)=0.0           



!>initialization ends

!>-------------------------------------------------------------------

!>if land use change has taken place then get fcanmxs for use by class based on the new fcancmxs

      k1=0
      do 300 j = 1, ican
        if(j.eq.1) then
          k1 = k1 + 1
        else
          k1 = k1 + nol2pfts(j-1)
        endif
        k2 = k1 + nol2pfts(j) - 1
        do 301 m = k1, k2
            fcanmx(i,j)=fcanmx(i,j)+fcancmx(i,m)
            barefrac(i)=barefrac(i)-fcancmx(i,m)
301     continue
300   continue
!>
!>check if the interpol didn't mess up the barefrac. if so, take the
!!extra amount from the pft with the largest area. jm apr 24 2013.
!!but you can't take it from crops!
      pftarrays=0.

         if (barefrac(i).lt.0.0) then ! compete only needs minbare but it checks later.

           k=1
           do j = 1,icc
            if (.not. crop(j)) then
             indexpos(i,k) = j
             pftarrays(i,k)=fcancmx(i,j)
             k=k+1 
            end if          
           end do   
           
            lrgstpft = maxloc(pftarrays(i,:))
            j = indexpos(i,lrgstpft(1))  
             
            if (compete) then
               fcancmx(i,j) = fcancmx(i,j) + barefrac(i) - minbare 
               barefrac(i) = minbare 
            else
               fcancmx(i,j) = fcancmx(i,j) + barefrac(i)
               barefrac(i) = 0.0
            end if
         endif

!>find previous day's bare fraction using previous day's fcancmxs
      do 310 j = 1, icc
          pbarefra(i)=pbarefra(i)-fcancmy(i,j)
310   continue
!>
!>check if the interpol didn't mess up the pbarefra. if so, take the
!!extra amount from the pft with the largest area. jm apr 24 2013.
!!but you can't take it from crops!
         if (pbarefra(i).lt.0.0) then 
           k=1
           do j = 1,icc
            if (.not. crop(j)) then
             indexpos(i,k) = j
             pftarrays(i,k)=fcancmy(i,j)
             k=k+1
            end if
           end do

            lrgstpft = maxloc(pftarrays(i,:))
            j = indexpos(i,lrgstpft(1))

           if (compete) then
                fcancmy(i,j) = fcancmy(i,j) + pbarefra(i) - minbare
                pbarefra(i) = minbare
            else
                fcancmy(i,j) = fcancmy(i,j) + pbarefra(i)
                pbarefra(i) = 0.0
            end if

         endif
!>
!>based on sizes of 3 live pools and 2 dead pools we estimate the total amount of c in each grid cell.
!>
      do 320 j = 1, icc
          totlmass(i)=totlmass(i)+ &
                     (fcancmy(i,j)*(gleafmas(i,j)+bleafmas(i,j)+ &
                      stemmass(i,j)+rootmass(i,j))*grclarea(i) &
                      *km2tom2)
320   continue

      do 340 j = 1, iccp1
          if(j.lt.iccp1) then
            term = fcancmy(i,j)
          else if(j.eq.iccp1) then
            term = pbarefra(i)
          endif
          totdmas1(i)=totdmas1(i)+ &
                     (term*litrmass(i,j)*grclarea(i)*km2tom2)
          totdmas2(i)=totdmas2(i)+ & 
                     (term*soilcmas(i,j)*grclarea(i)*km2tom2)
340   continue

        totcmass(i)=totlmass(i)+totdmas1(i)+totdmas2(i)
!     bare fractions cannot be negative
        if( pbarefra(i).lt.0.0.and.abs(pbarefra(i)).lt.1.0e-05 )then
          pbarefra(i)=0.0
        else if(pbarefra(i).lt.0.0.and.abs(pbarefra(i)).ge.1.0e-05 )then
          write(6,*)'bare fractions cannot be negative'
          write(6,*)'prev. bare fraction(',i,')  =',pbarefra(i)
          call xit('luc',-7)
        endif

        if( barefrac(i).lt.0.0.and.abs(barefrac(i)).lt.1.0e-05 )then
          write(*,*)'setting bare to zero',barefrac(i)
          barefrac(i)=0.0
        else if(barefrac(i).lt.0.0.and.abs(barefrac(i)).ge.1.0e-05 )then
          write(6,*)'bare fractions cannot be negative'
          write(6,*)'bare fraction(',i,')  =',barefrac(i)
          call xit('luc',-8)
        endif
!>     find above ground biomass and treatment index for combust, paper,
!>     and furniture
      k1=0
      do 500 j = 1, ican
        if(j.eq.1) then
          k1 = k1 + 1
        else
          k1 = k1 + nol2pfts(j-1)
        endif
        k2 = k1 + nol2pfts(j) - 1
        do 510 m = k1, k2
            abvgmass(i,m)=gleafmas(i,m)+bleafmas(i,m)+stemmass(i,m)
            if(j.eq.1.or.j.eq.2) then  ! trees
              if(abvgmass(i,m).ge.bmasthrs(1)) then !forest
                treatind(i,m)=1
              else if (abvgmass(i,m).le.bmasthrs(2)) then !bush 
                treatind(i,m)=3
              else  !shrubland
                treatind(i,m)=2
              endif
            else                       !crops and grasses
              treatind(i,m)=3
            endif
510     continue
500   continue

!>check if a pft's fractional cover is increasing or decreasing

      do 550 j = 1, icc 
          if( ( fcancmx(i,j).gt.fcancmy(i,j)) .and. &
             (abs(fcancmy(i,j)-fcancmx(i,j)).gt.zero) ) then
              fraciord(i,j)=1  ! increasing
          else if( ( fcancmx(i,j).lt.fcancmy(i,j)) .and. &
                  (abs(fcancmy(i,j)-fcancmx(i,j)).gt.zero) ) then
              fraciord(i,j)=-1 ! decreasing
          endif
550   continue

!>check if bare fraction increases of decreases

        if( ( barefrac(i).gt.pbarefra(i)) .and. &
           (abs(pbarefra(i)-barefrac(i)).gt.zero) ) then
              bareiord(i)=1  !increasing
        else if ( ( barefrac(i).lt.pbarefra(i)) .and. &
                 (abs(pbarefra(i)-barefrac(i)).gt.zero) ) then
              bareiord(i)=-1 !decreasing
        endif

!>        
!>if the fractional coverage of pfts increases then spread their live & dead biomass uniformly 
!!over the new fraction. this effectively reduces their per m2 c density. 
!!
      do 570 j = 1, icc
          if(fraciord(i,j).eq.1)then
            term = fcancmy(i,j)/fcancmx(i,j)
            gleafmas(i,j)=gleafmas(i,j)*term
            bleafmas(i,j)=bleafmas(i,j)*term
            stemmass(i,j)=stemmass(i,j)*term
            rootmass(i,j)=rootmass(i,j)*term
            litrmass(i,j)=litrmass(i,j)*term
            soilcmas(i,j)=soilcmas(i,j)*term
          endif 
570   continue
!>
!>if bare fraction increases then spread its litter and soil c uniformly over the increased fraction
!>
        if(bareiord(i).eq.1)then
          term = pbarefra(i)/barefrac(i)
          litrmass(i,iccp1)=litrmass(i,iccp1)*term
          soilcmas(i,iccp1)=soilcmas(i,iccp1)*term
        endif
!>
!>if any of the pfts fractional coverage decreases, then we chop the
!!aboveground biomass and treat it according to our rules (burn it,
!!and convert it into paper and furniture). the below ground live
!!biomass and litter of this pfts gets assimilated into litter of
!!all pfts (uniformly spread over the whole grid cell), and soil c
!!from the chopped off fraction of this pft, gets assimilated into
!!soil c of all existing pfts as well.
!!
      k1=0
      do 600 j = 1, ican
        if(j.eq.1) then
          k1 = k1 + 1
        else
          k1 = k1 + nol2pfts(j-1)
        endif
        k2 = k1 + nol2pfts(j) - 1
        do 610 m = k1, k2
            if(fraciord(i,m).eq.-1)then

!>chop off above ground biomass 
              redubmas1=(fcancmy(i,m)-fcancmx(i,m))*grclarea(i) &
                       *abvgmass(i,m)*km2tom2

              if(redubmas1.lt.0.0)then
                write(6,*)'redubmas1 less than zero'
                write(6,*)'fcancmy-fcancmx = ',  &
                          fcancmy(i,m)-fcancmx(i,m)
                write(6,*)'grid cell = ',i,' pft = ',m
                call xit('luc',-10)
              endif
!>
!>rootmass needs to be chopped as well and all of it goes to the litter/paper pool
!>
              redubmas2=(fcancmy(i,m)-fcancmx(i,m))*grclarea(i)  &
                       *rootmass(i,m)*km2tom2

!>keep adding chopped off biomass for each pft to get the total for a grid cell for diagnostics

              chopedbm(i)=chopedbm(i) + redubmas1 + redubmas2 

!>find what's burnt, and what's converted to paper & furniture
              combustc(i,m)=combust(treatind(i,m))*redubmas1
              paperc(i,m)=paper(treatind(i,m))*redubmas1 + redubmas2
              furnturc(i,m)=furniture(treatind(i,m))*redubmas1

!>keep adding all this for a given grid cell
              grsumcom(i)=grsumcom(i)+combustc(i,m)
              grsumpap(i)=grsumpap(i)+paperc(i,m)
              grsumfur(i)=grsumfur(i)+furnturc(i,m)
!>
!>litter from the chopped off fraction of the chopped
!>off pft needs to be assimilated, and so does soil c from
!>the chopped off fraction of the chopped pft
!>
              redubmas1=(fcancmy(i,m)-fcancmx(i,m))*grclarea(i) &
                       *litrmass(i,m)*km2tom2
              incrlitr(i,m)=redubmas1

              redubmas1=(fcancmy(i,m)-fcancmx(i,m))*grclarea(i) &
                       *soilcmas(i,m)*km2tom2
              incrsolc(i,m)=redubmas1

              grsumlit(i)=grsumlit(i)+incrlitr(i,m)
              grsumsoc(i)=grsumsoc(i)+incrsolc(i,m)
            endif

610     continue
600   continue

!>if bare fraction decreases then chop off the litter and soil c
!!from the decreased fraction and add it to grsumlit & grsumsoc for spreading over the whole grid cell
!!
        if(bareiord(i).eq.-1)then

          redubmas1=(pbarefra(i)-barefrac(i))*grclarea(i) &
                   *litrmass(i,iccp1)*km2tom2

          redubmas2=(pbarefra(i)-barefrac(i))*grclarea(i) &
                   *soilcmas(i,iccp1)*km2tom2

          grsumlit(i)=grsumlit(i)+redubmas1
          grsumsoc(i)=grsumsoc(i)+redubmas2
        endif

!>calculate if the chopped off biomass equals the sum of grsumcom(i), grsumpap(i) & grsumfur(i)

       if( abs(chopedbm(i)-grsumcom(i)-grsumpap(i)-grsumfur(i)).gt. &
          tolrnce1 ) then
           write(6,*)'at grid cell = ',i
           write(6,*)'chopped biomass does not equals sum of total'   
           write(6,*)'luc related emissions'
           write(6,*)'chopedbm(i) = ',chopedbm(i)
           write(6,*)'grsumcom(i) = ',grsumcom(i)
           write(6,*)'grsumpap(i) = ',grsumpap(i)
           write(6,*)'grsumfur(i) = ',grsumfur(i)
           write(6,*)'sum of grsumcom, grsumpap, grsumfur(i) = ', &
            grsumcom(i)+grsumpap(i)+grsumfur(i) 
           call xit('luc',-11)
       endif
!>
!!spread chopped off stuff uniformly over the litter and soil c pools of all existing pfts, including the bare fraction.
!!
!!convert the available c into density 
!!
        grdencom(i)=grsumcom(i)/(grclarea(i)*km2tom2)
        grdenpap(i)=grsumpap(i)/(grclarea(i)*km2tom2)
        grdenfur(i)=grsumfur(i)/(grclarea(i)*km2tom2)
        grdenlit(i)=grsumlit(i)/(grclarea(i)*km2tom2)
        grdensoc(i)=grsumsoc(i)/(grclarea(i)*km2tom2)


      do 650 j = 1, icc
          if(fcancmx(i,j).gt.zero)then
            litrmass(i,j)=litrmass(i,j)+grdenpap(i)+grdenlit(i)
            soilcmas(i,j)=soilcmas(i,j)+grdenfur(i)+grdensoc(i)
          else
            gleafmas(i,j)=0.0
            bleafmas(i,j)=0.0
            stemmass(i,j)=0.0
            rootmass(i,j)=0.0
            litrmass(i,j)=0.0
            soilcmas(i,j)=0.0
          endif
650   continue
 
        if(barefrac(i).gt.zero)then
          litrmass(i,iccp1)=litrmass(i,iccp1)+grdenpap(i)+grdenlit(i)
          soilcmas(i,iccp1)=soilcmas(i,iccp1)+grdenfur(i)+grdensoc(i)
        else
          litrmass(i,iccp1)=0.0
          soilcmas(i,iccp1)=0.0
        endif

!>the combusted c is used to find the c flux that we can release into the atmosphere.

        lucemcom(i)=grdencom(i)     
!>this is flux in kg c/m2.day that will be emitted 
        lucltrin(i)=grdenpap(i) ! flux in kg c/m2.day
        lucsocin(i)=grdenfur(i) ! flux in kg c/m2.day

!>convert all land use change fluxes to u-mol co2-c/m2.sec
        lucemcom(i)=lucemcom(i)*963.62
        lucltrin(i)=lucltrin(i)*963.62
        lucsocin(i)=lucsocin(i)*963.62

!>and finally we see if the total amount of carbon is conserved

      do 700 j = 1, icc
          ntotlmas(i)=ntotlmas(i)+ &
                     (fcancmx(i,j)*(gleafmas(i,j)+bleafmas(i,j)+&
                     stemmass(i,j)+rootmass(i,j))*grclarea(i) &
                     *km2tom2)
700   continue

      do 710 j = 1, iccp1
          if(j.lt.iccp1) then
            term = fcancmx(i,j)
          else if(j.eq.iccp1) then
            term = barefrac(i)
          endif
          ntotdms1(i)=ntotdms1(i)+ &
                     (term*litrmass(i,j)*grclarea(i)*km2tom2) 
          ntotdms2(i)=ntotdms2(i)+ & 
                     (term*soilcmas(i,j)*grclarea(i)*km2tom2)
710   continue

        ntotcmas(i)=ntotlmas(i)+ntotdms1(i)+ntotdms2(i)
!>
!>total litter mass (before + input from chopped off biomass) and after must be same
!>
        if( abs(totdmas1(i)+grsumpap(i)-ntotdms1(i)).gt.tolrnce1 )then   
           write(6,*)'at grid cell = ',i
           write(6,*)'total litter carbon does not balance after luc'
           write(6,*)'totdmas1(i) = ',totdmas1(i)
           write(6,*)'grsumpap(i) = ',grsumpap(i)
           write(6,*)'totdmas1(i) + grsumpap(i) = ', &
                     totdmas1(i) + grsumpap(i)
           write(6,*)'ntotdms1(i) = ',ntotdms1(i)
           call xit('luc',-12)
        endif
!>
!>for conservation totcmass(i) must be equal to ntotcmas(i) plus combustion carbon losses
!>
        if( abs(totcmass(i)-ntotcmas(i)-grsumcom(i)).gt.tolrnce1)then
           write(6,*)'at grid cell = ',i
           write(6,*)'total carbon does not balance after luc'
           write(6,*)'totcmass(i) = ',totcmass(i)
           write(6,*)'ntotcmas(i) = ',ntotcmas(i)
           write(6,*)'grsumcom(i) = ',grsumcom(i)
           call xit('luc',-13)
        endif
!>
!>update grid averaged vegetation biomass, and litter and soil c densities
!>
      do 750 j = 1, icc
          vgbiomas(i)=vgbiomas(i)+fcancmx(i,j)*(gleafmas(i,j)+ &
                     bleafmas(i,j)+stemmass(i,j)+rootmass(i,j))
          gavgltms(i)=gavgltms(i)+fcancmx(i,j)*litrmass(i,j)
          gavgscms(i)=gavgscms(i)+fcancmx(i,j)*soilcmas(i,j)
750   continue

        gavgltms(i)=gavgltms(i)+( barefrac(i)*litrmass(i,iccp1) )
        gavgscms(i)=gavgscms(i)+( barefrac(i)*soilcmas(i,iccp1) )
!>
!>just like total amount of carbon must balance, the grid averagred densities must also balance
!>
       if( abs(pvgbioms(i)+pgavltms(i)+pgavscms(i)- &
              vgbiomas(i)-gavgltms(i)-gavgscms(i)- &
              grdencom(i)).gt.tolrance ) then
           write(6,*)'iday = ',iday
           write(6,*)'at grid cell = ',i
           write(6,*)'pbarefra(i) = ',pbarefra(i)
           write(6,*)'barefrac(i) = ',barefrac(i)
           write(6,*)'pfcancmx(i,j) = ',(pfcancmx(i,j),j=1,icc)
           write(6,*)'nfcancmx(i,j) = ',(nfcancmx(i,j),j=1,icc)
           write(6,*)'total carbon density does not balance after luc'
           write(6,*)'pvgbioms(i) = ',pvgbioms(i)
           write(6,*)'pgavltms(i) = ',pgavltms(i)
           write(6,*)'pgavscms(i) = ',pgavscms(i)
           write(6,*)'vgbiomas(i) = ',vgbiomas(i)
           write(6,*)'gavgltms(i) = ',gavgltms(i)
           write(6,*)'gavgscms(i) = ',gavgscms(i)
           write(6,*)'grdencom(i) = ',grdencom(i)
           write(6,*)'pvgbioms + pgavltms + pgavscms = ', &
                   (pvgbioms(i)+pgavltms(i)+pgavscms(i))
           write(6,*)'vgbiomas + gavgltms + gavgscms + grdencom = ', &
         (vgbiomas(i)+gavgltms(i)+gavgscms(i)+ grdencom(i))
         write(6,*)'diff = ',abs((pvgbioms(i)+pgavltms(i)+pgavscms(i)) &
          -(vgbiomas(i)+gavgltms(i)+gavgscms(i)+ grdencom(i)))
           write(6,*)'tolrance = ',tolrance
           call xit('luc',-14)
       endif

      do 800 j = 1, icc
          if((.not. leapnow .and.iday.eq.365) .or. & 
             (leapnow.and.iday.eq.366)) then
            pfcancmx(i,j)=nfcancmx(i,j)
          endif
800   continue

       endif  ! loop to check if any luc took place. 
255   continue

      return 

end subroutine luc
!>@}
!=======================================================================
!>\ingroup landuse_change_adjust_luc_fracs
!>@{

subroutine adjust_luc_fracs(i,onetile_perPFT,nfcancmxrow, &
                          bare_ground_frac, compete)
! J. Melton, Jan 11 2013

use ctem_params,        only : nlat,nmos,icc,seed

implicit none

! arguments:
integer, intent(in) :: i
real, dimension(nlat,nmos,icc), intent(inout) :: nfcancmxrow
real, intent(in) :: bare_ground_frac
logical, intent(in) :: onetile_perPFT
logical, intent(in) :: compete

real, dimension(nlat,nmos,icc) :: outnfcrow

! local variables:
integer :: m, j
real, dimension(icc) :: frac_abv_min_val
real :: needed_bare,reduce_per_pft,tot
real :: min_val

!-------------------------
tot = 0.0

if (compete) then
    min_val = seed
else
    min_val = 0.
end if        

!> find the amount of needed space in the other pfts
!> need a minimum bare area of min_val.
needed_bare=(-bare_ground_frac) + min_val

!> get the proportionate amounts of each pft above the min_val lower limit
do j = 1,icc
  if (onetile_perPFT) then
    m = j
  else
    m = 1
  end if

  frac_abv_min_val(j)=max(0.0,nfcancmxrow(i,m,j)-min_val)
  tot = tot + frac_abv_min_val(j)

enddo

!> add in the bare ground min fraction of min_val.
  tot = tot + min_val

!> now reduce the pfts proportional to their fractional area to make
!> the bare ground fraction be the min_val amount and no other pft be less than min_val
do j = 1,icc
  if (onetile_perPFT) then
    m = j
  else
    m = 1
  end if
  outnfcrow(i,m,j)=max(min_val,nfcancmxrow(i,m,j) - needed_bare * &
                       frac_abv_min_val(j) / tot)
  nfcancmxrow(i,m,j) = outnfcrow(i,m,j)
enddo

end subroutine adjust_luc_fracs
!>@}
!=======================================================================
!>\ingroup landuse_change_adjust_fracs_comp
!>@{

subroutine adjust_fracs_comp(il1,il2,nilg,iday,pfcancmx,yesfrac,delfrac,outdelfrac)
! J. Melton, Feb 13 2014

use ctem_params,        only : icc,crop,zero,seed

implicit none

! arguments:
integer, intent(in) :: il1
integer, intent(in) :: il2
integer, intent(in) :: nilg
integer, intent(in) :: iday
real, dimension(nilg,icc), intent(in) :: pfcancmx
real, dimension(nilg,icc), intent(in) :: yesfrac
real, dimension(nilg,icc), intent(in) :: delfrac

real, dimension(nilg,icc), intent(out) :: outdelfrac

! local variables:
integer :: i, j
real, dimension(nilg) :: chgcrop, cropfrac
real, dimension(nilg,icc) :: adjus_fracs,fmx,fmy
real, dimension(nilg) :: barefrac
real, dimension(nilg,1,icc) :: tmpfcancmx

real, parameter :: smallnumber = 1.0e-12

!>-------------------------

!> Some initializations
chgcrop = 0.
cropfrac = 0.
adjus_fracs = 0.
outdelfrac = 0.
barefrac=1.0

!> Find how much the crop area changes this timestep. We only care about the total
!> change, not the per crop PFT change.
do i = il1, il2
  do j = 1, icc
     if (crop(j)) then
          fmx(i,j)=pfcancmx(i,j)+(real(iday)*delfrac(i,j)) !  current day
          fmy(i,j)=pfcancmx(i,j)+(real(iday-1)*delfrac(i,j)) ! previous day
          chgcrop(i) = chgcrop(i) + (fmx(i,j) - fmy(i,j))
          cropfrac(i) = cropfrac(i) + fmx(i,j)
     else 
        !> add the seed fracs to the cropfrac for use below since we can't take
        !> area from a pft that only has a seed fraction.
        if (yesfrac(i,j) .eq. seed) then 
          cropfrac(i) = cropfrac(i) + seed
        end if 

     end if   
  end do
end do 
!>
!> If the crop area changed we have to reduce the other PFT proportional to their 
!> area (if we gained crop area). We don't presently assume anything like grasslands
!> are converted first. We assume that on the scale of our gridcells, area 
!> is simply converted proportional to the total.
do i = il1, il2

  if (chgcrop(i) .gt. smallnumber) then

    !> Adjust the non-crop PFT fractions to find their proportional fraction that does not include crops.
    do j = 1, icc
     if (.not. crop(j) .and. yesfrac(i,j) .gt. seed) then
     
       adjus_fracs(i,j) = yesfrac(i,j) / (1. - cropfrac(i))
       outdelfrac(i,j) = chgcrop(i) * adjus_fracs(i,j)

     end if

     if (.not. crop(j)) then
       barefrac(i) = barefrac(i) - (yesfrac(i,j) + outdelfrac(i,j))
       tmpfcancmx(i,1,j) = yesfrac(i,j) + outdelfrac(i,j)
     else  ! crops
       barefrac(i) = barefrac(i) - fmx(i,j) 
       tmpfcancmx(i,1,j) = fmx(i,j)
     end if
    end do

    if (barefrac(i) .lt. 0.0) then
       !> tmpfcancmx has a nilg,1,icc dimension as adjust_luc_fracs expects
       !> a 'row' structure of the array.
       call adjust_luc_fracs(i,.false.,tmpfcancmx,barefrac(i),.true.)
       do j = 1, icc
          outdelfrac(i,j) = tmpfcancmx(i,1,j) - yesfrac(i,j)
       end do
    end if

  end if

end do

end subroutine adjust_fracs_comp
!>@}




end module
