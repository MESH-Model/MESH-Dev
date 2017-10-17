module svs_configs

  !use sfc_options

  implicit none


  ! Number of soil ("GROUND") layers for SVS land scheme
!VV DEBUT MODIFICATION POUR MESH
!  integer, save :: nl_svs
!  integer, parameter :: nl_svs = 7
  integer , parameter :: nl_svs = 7

  ! Depth of deep soil layers in METERS   
!  real, allocatable, save :: dl_svs(:)
  !=  (/ 0.05, 0.1, 0.2, 0.4, 1.0, 2.0, 3.0 /) 
  real, parameter, dimension(nl_svs) :: dl_svs =  (/ 0.05, 0.1, 0.2, 0.4, 1.0, 2.0, 3.0 /) 

  ! Number of last active (permeable) layer
  !integer, parameter :: kdp = 7
  integer, parameter :: kdp = 7
!VV FIN MODIFICATION POUR MESH

  ! Thicknesses of layers in METERS
  real,  allocatable, save :: delz(:) !nl_svs

  !---------------------------------------------------------------
  ! SOIL TEXTURE OPTIONS: 
  !  1:   "CLASSIC"  --  3 layers of sand, clay info... use the mean to initialize all variables
  !  2:   "GSDE"      --  8 layers of soil texture from CHINESE DATASET !!!
  !  3:   "SLC"      --  5 layers of soil texture:  SOIL LANDSCAPE of CANADA
  ! ENTRY  bus number of levels for clay & sand variables
  integer,  save :: nl_ste
  ! PERMANENT PHYSICS bus number of levels for clay & sand variables
  integer, save :: nl_stp

  ! GSDE NUMBER OF LAYERS 
  integer,  parameter :: nl_gsde = 8 
  ! GSDE SOIL DEPTH in METERS... 
  real, parameter , dimension(nl_gsde):: dl_gsde =  (/ 0.045, 0.091, 0.166, 0.289, 0.493, 0.829, 1.38, 2.229 /) 
  

   ! SLC NUMBER OF LAYERS 
  integer,  parameter :: nl_slc = 6
  ! SLC SOIL DEPTH in METERS... ( 0-5cm, 15-30cm, 30-60cm, 60-100cm, 100-200cm ) 
  real, parameter , dimension(nl_slc):: dl_slc =  (/ 0.05, 0.15, 0.3, 0.6, 1.0, 2.0 /) 
  
  !  WEIGHTS TO MAP soil parameters calculated on GSDE or SLC  layers unto model soil_layers
  real, allocatable, save :: weights(:,:)       !(max_nl_svs,nl_soil_texture)


  !------------------------------------------------------------------
  ! NUMERICAL METHOD HYDRO
  ! Numerical method used to solve Richards' equations in hydro_svs
  ! hydro_svs_method=0: Euler, forward, 1st order
  ! hydro_svs_method=1: Runge-Kutta 4th order (RK4)
  integer, parameter :: hydro_svs_method = 1

  !-----------------------------------------------------------------
  ! VEG HIGH/LOW SPLIT 
  !
  ! number and "type" of low veg
  integer, parameter :: ntypel = 13
  integer, parameter, dimension(ntypel) :: vl_type = (/ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 22 , 23  /)
  ! number and "type" of high veg
  integer, parameter :: ntypeh = 8
  integer, parameter, dimension(ntypeh) :: vh_type = (/ 4, 5, 6, 7, 8, 9, 25, 26 /)
  !  WATCH OUT.... DECIDUOUS AND EVERGREEN fraction still hard-coded in veglowhih.F90

  ! Urban Class: 21, split between high and low veg
  ! fraction of urban in low veg (+ fraction of urban in high veg = 1.0)
  real, parameter :: furb_vl = 0.5
  

  !---------------------------------------------------------------  
  ! other constants...
  ! M_TO_MM : CONVERT METERS TO MM
  real, parameter :: M_TO_MM = 1000.00
  ! FRACTION OF URBAN CLASS CONSIDERED IMPERVIOUS FOR RUNOFF CALC. IN HYDRO...
  REAL, PARAMETER :: IMP_URB = 0.33
  ! TRACE AMOUNT OF SNOW MASS [kg/m3] (measured=0.1 ... but in model allow smaller threshold)
  REAL, PARAMETER :: CRITSNOWMASS_E=0.5  ! Entry snow mass criteria... need to be more stringent...
  REAL, PARAMETER :: CRITSNOWMASS=0.01 ! model integration snow mass criteria
  REAL, PARAMETER :: LAI0 = 1.0  ! DEFAULT LAI VALUE
  private :: weights_soil_texture


contains


  subroutine init_soil_text_levels()

    use sfc_options
    implicit none

    nl_ste = 3
    nl_stp = 3

    ! number of levels of entry and bus clay, sand variables
    if ( soiltext == "CLASSIC" ) then

       ! CLASSIC:

       ! ENTRY  bus number of levels for clay & sand variables
       nl_ste = 3
       ! PERMANENT PHYSICS bus number of levels for clay & sand variables
       nl_stp = nl_svs

    else if ( soiltext == "GSDE" ) then

       ! GSDE:

       ! ENTRY  bus number of levels for clay & sand variables
       nl_ste = nl_gsde
       ! PERMANENT PHYSICS bus number of levels for clay & sand variables
       nl_stp = nl_gsde

       call weights_soil_texture()

    else  if ( soiltext == "SLC" ) then
    	  ! ENTRY  bus number of levels for clay & sand variables
       	  nl_ste = nl_slc
       	  ! PERMANENT PHYSICS bus number of levels for clay & sand variables
       	  nl_stp = nl_slc
       call weights_soil_texture()	

    endif

    return
  end subroutine INIT_SOIL_TEXT_LEVELS


  subroutine weights_soil_texture()


!VVI DEBUT MODIFICATION POUR MESH
!#include <msg.h>
!VVI FIN MODIFICATION POUR MESH
 	 use sfc_options
    implicit none
    integer :: unout
!VVI DEBUT MODIFICATION POUR MESH
!    integer, external :: msg_getUnit
!VVI FIN MODIFICATION POUR MESH
    integer k, kk
    real d_svs(nl_svs+1), d_soil_texture(nl_stp+1), dl_soil_texture(nl_stp)

  
    if ( .not. allocated(weights) ) allocate( weights(nl_svs,nl_stp) )

    ! defined array of depth with value starting at 0m, to make looping easier
    ! for same reason , set maximum depth of SOIL_TEXTURE equal to that of MODEL layers... i.e., assume
    ! texture constant past maximum depth of SOIL_TEXTURE texture
    d_svs(1) = 0
    do k=2,nl_svs+1
       d_svs(k)=dl_svs(k-1)
    enddo

    ! first depth set to 0
    d_soil_texture(1) = 0
    if ( soiltext == "GSDE" ) then
    
       do k=2,nl_stp
          d_soil_texture(k)=dl_gsde(k-1)
       enddo
       ! last depth of soil texture database set to max depth of SVS 
       ! i.e, deepest soil texture measured extends to the bottom of last SVS layer
       d_soil_texture(nl_stp+1)=max( dl_svs(nl_svs) , dl_gsde(nl_stp) )
    else  if ( soiltext == "SLC" ) then
         
       do k=2,nl_stp
          d_soil_texture(k)=dl_slc(k-1)
       enddo
       ! last depth of soil texture database set to max depth of SVS 
       ! i.e, deepest soil texture measured extends to the bottom of last SVS layer
       d_soil_texture(nl_stp+1)=max( dl_svs(nl_svs) , dl_slc(nl_stp) )
    endif
    
    
    ! calculate weights

    do k = 1,nl_svs ! model layers
       do kk = 1, nl_stp ! database layers

          weights(k,kk) = max( min(d_svs(k+1),d_soil_texture(kk+1)) - max(d_svs(k),d_soil_texture(kk) ) , 0.0 ) &
               / (d_svs(k+1) - d_svs(k)) 

       end do
    end do


    ! PRINT INFO in LISTING
!VVI DEBUT MODIFICATION POUR MESH
!    unout         = msg_getUnit(MSG_INFO)
     unout         = 6 !output_unit
!VVI FIN MODIFICATION POUR MESH
    if (unout > 0) then
       if ( soiltext == "GSDE" ) then
          write(unout, *) ' ****** GSDE SOIL TEXTURE ******* '
       else if (soiltext == "SLC" ) then
          write(unout, *) ' ****** SLC SOIL TEXTURE ******* '
       endif
          
       write(unout, *) ' ****** SOIL MAPPING WEIGHTS [METERS] ******* '
       do k = 1,nl_svs ! model layers

          write(unout, *) '--- for SVS layer k=', k, ' depth=', dl_svs(k), ' ---'

          if ( soiltext == "GSDE" ) then
             do kk = 1, nl_stp ! database layers
                write(unout, *) 'for GSDE layer kk=', kk,' depth=', dl_gsde(kk),' weight=', weights(k,kk)
             enddo
          else if (soiltext == "SLC" ) then
             do kk = 1, nl_stp ! database layers
                write(unout, *) 'for SLC layer kk=', kk,' depth=', dl_slc(kk),' weight=', weights(k,kk)
             enddo
          endif
       enddo
    endif

    return
  end subroutine weights_soil_texture

  subroutine layer_thickness()
    implicit none

    integer k
    if ( .not. allocated(delz) ) allocate( delz(nl_svs) )
    DELZ(1)=DL_SVS(1)            
    DO K=2,NL_SVS        
       DELZ(K)=DL_SVS(K)-DL_SVS(K-1)
    ENDDO

  end subroutine layer_thickness

  real function AG(FVH,FVL,FSNVH,FSN,BG,VG,SNO,SNV)
    !     FUNCTION TO AGGREGATE SURFACE VARIABLES OVER SVS SURFACE TYPES
    !     (Bare Ground, Vegetation, low-veg-and-bare-ground snow, under-high-veg snow)

    !          - Input -
    ! FVH      Fraction of grid cell covered by HIGH vegetation
    ! FVL      Fraction of grid cell covered by LOW  vegetation
    ! FSNVH    Fraction of HIGH vegetation covered by snow (from ground or atm)
    ! FSN      Fraction of LOW  vegetation or bare ground covered by snow
    ! BG       Bare Ground Variable 
    ! VG       Vegetation  Variable
    ! SNO      Snow over low veg & bare ground Variable 
    ! SNV      Snow-under-vegetation variable
    !
    !         - Output -
    ! AG       Aggregated Variable
    REAL, intent(in) :: FVH,FVL,FSNVH,FSN,BG,VG,SNO,SNV
     
    AG =  (1. - FVH - FVL ) * (1. - FSN)             * BG   &
         + (FVH * (1. - FSNVH)  + FVL * (1. - FSN))   * VG   &
         + (1. - FVH) * FSN                           * SNO  &
         +  FVH * FSNVH                               * SNV  
    
    return
  end function AG


end module svs_configs
  
