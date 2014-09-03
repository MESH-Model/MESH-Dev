SUBROUTINE RDCMEMMESHSTATIC(NA,NTYPE,ILG,NMTEST,IGND,ICP1,ZSNOW, RHOSGAT, &
							THLQGAT, TBARGAT, TAGAT,TSFSGAT, FCANGAT, SANDGAT, CLAYGAT)
                                   
! Purpose :
! ----------
!	Read static input variables from MESH for cmem
!
! Kurt C. Kornelsen 03 Aug 2014
! Based on RDCMEMASCII by P. de Rosnay

USE PARKIND1, ONLY : JPIM, JPRM
USE YOMLUN, ONLY : NULOUT, NULTMP
USE YOMCMEMPAR, ONLY : CIDVEG, CITVEG, CIATM,  LGPRINT,CNAMEID, LOFIELDEXP, nobs_atm,PPG, &
                     LOMASK_OCEAN, LOMASK_AUTO,nlay_soil_ls
USE YOMCMEMFIELDS, ONLY : N_CMEM, JJ, nbpt &
                    &  ,fTVL, fTVH,  fs_laiL, ftfrac, fsnowd, frsnow,ftl_lsm, ftair,ftveg & ! SOME OF THESE VARIABLES NEED TO GET MOVED
                    &  ,ftskin,fwc_lsm,fsand,fclay,fwater,mask 
USE YOMCMEMATM, ONLY : fZ, fs_tatm,fs_spres,fs_R,z_atm,r_atm 
USE YOMCMEMSOIL, ONLY : sal_sea 
USE MESH_INPUT_MODULE
USE MESH_CMEM_MODULE, ONLY : ECHGRUGAT, ECLGRUGAT, AIL, WATERGAT,ELEVGAT

IMPLICIT NONE

! MESH DIMENSIONS
INTEGER :: NA, NTYPE, ILG, NMTEST, IGND, ICP1


REAL(KIND=JPRM) :: rvcov(0:20)
REAL(KIND=JPRM) :: rvlai(0:20)
REAL(KIND=JPRM) :: b(0:7)
REAL(KIND=JPRM) :: b1(0:7)
REAL(KIND=JPRM) :: b2(0:7)
REAL(KIND=JPRM) :: b3(0:7)
REAL(KIND=JPRM) :: VWC(0:7)
REAL(KIND=JPRM) :: ZNrh(0:7)
REAL(KIND=JPRM) :: ZNrv(0:7)
REAL(KIND=JPRM) :: Ztth(0:7)
REAL(KIND=JPRM) :: Zttv(0:7)
REAL(KIND=JPRM) :: Zhr(0:7)
REAL(KIND=JPRM) :: Zw_eff(0:7,2)
INTEGER(KIND=JPIM) :: RVTV(0:20)

! TODO THESE VALUES ARE ACTUALLY ALLOCATED AND DEALLOCATED
! INSTANTLY IN CMEM RD SUBROUTINE. SO WE NEED TO FIND A 
! WAY TO KEEP THEM AROUND ARE WE NEED TO REDO THIS SECTION
! AT EACH AND EVERY MESH RUN. I DON"T SEE A WAY AROUND THIS
! ALSO THE ABOVE VARIABLES ARE READ IN VEGTABLE AND PRINTED ONCE
! THEREFORE WE NEED TO RECOPY THE ZSAND-CLAY REDUNDANTLY.
REAL(KIND=JPRM),ALLOCATABLE :: cvegl(:), cvegh(:)
REAL(KIND=JPRM),ALLOCATABLE :: fbare(:)  
REAL(KIND=JPRM),ALLOCATABLE :: fvegl(:)  
REAL(KIND=JPRM),ALLOCATABLE :: fvegh(:)    
REAL(KIND=JPRM),ALLOCATABLE :: sncov(:) 




REAL(KIND=JPRM) ::  zsand,zclay, zfZ,zfTVL,zfTVH,zfvegl,zfvegh,zfwater,zlaiH 

!INTEGER(KIND=JPIM), DIMENSION(ILG) :: ECHGRUGAT, ECLGRUGAT
REAL, DIMENSION(ILG) :: TAGAT, RHOSGAT, ZSNOW
REAL, DIMENSION(ILG,IGND):: THLQGAT, TBARGAT
REAL, DIMENSION(ILG,4):: TSFSGAT
REAL, DIMENSION(ILG,ICP1):: FCANGAT
REAL, DIMENSION(ILG,IGND) :: CLAYGAT, SANDGAT



!LOCAL COUNTERS
INTEGER :: I, J, D

!
!------------------------------------------------------------------------------
!
!
!Define default mask
!
mask(:) = 1_JPIM
!
!-------------------------------------------------------------------------------



! Read the soil moisture, skin temperature, soil temperature, snow depth and snow density
! All units are the same in MESH and CMEM so no conversion is necessary
DO JJ = 1,N_CMEM
	! For each soil layer
	DO D = 1,nlay_soil_ls
		fwc_lsm(JJ,D) = THLQGAT(JJ,D)
		ftl_lsm(JJ,D) = TBARGAT(JJ,D)
	END DO

	
	fsnowd(JJ)=ZSNOW(JJ)
	frsnow(JJ) = RHOSGAT(JJ)
	ftskin(JJ) = 0.0
	! Average over MESH sub-grid surface temperatures
	DO D = 1,4
		ftskin(JJ) = ftskin(JJ) + TSFSGAT(JJ,D) ! Add up sub-grid areas
	END DO
	
	ftskin(JJ) = ftskin(JJ) / 4.0 ! Take the average
	
END DO

fsnowd(:) = MAX(1.e-2_JPRM,fsnowd(:))
WHERE(fsnowd == 1.e-2_JPRM) fsnowd(:) = 0.0

! Update the mask to eliminate unrealistic points
   SELECT CASE (LOMASK_AUTO)
   CASE (.True.)
     do i = 1,nlay_soil_ls
      WHERE (ftl_lsm(:,i) <100.0_JPRM .or. ftskin(:) < 100.0_JPRM) mask(:) = 0_JPIM  ! unrealistic temperature
      WHERE ( fwc_lsm(:,i) < 0.0_JPRM ) mask(:) = 0_JPIM  ! Unrealistic SM
     enddo
   END SELECT

! in any case ensure sm is positive
     fwc_lsm(:,:) = MAX(0.0_JPRM, fwc_lsm(:,:))

!> ----------------------------------------------------------------------
!> Read soil and vegetation inputs
!>--------------------------------------------------------------------
!> 
!> 2.1 Read data texture and vegetation types
ALLOCATE (fbare(N_CMEM))
ALLOCATE (fvegl(N_CMEM))
ALLOCATE (fvegh(N_CMEM))
ALLOCATE (cvegl(N_CMEM))
ALLOCATE (cvegh(N_CMEM))
ALLOCATE (sncov(N_CMEM))

! Default vegetation cover is 100% on vegetated tiles
cvegh(:) = 1.0_JPRM
cvegl(:) = 1.0_JPRM





SELECT CASE (LOMASK_AUTO)
  CASE (.True.)
  WHERE (fZ > 9.0_JPRM .or. fZ < -1.0_JPRM) mask = 0_JPIM
END SELECT

!> Convert ECOCLIMAP vegetation types based on the GRU

DO JJ = 1,N_CMEM
fTVL(JJ) = ECLGRUGAT(JJ)!Low veg type
fTVH(JJ) = ECHGRUGAT(JJ)!High veg type

END DO

! Read fwater from MESH_drainage_database

DO JJ = 1, N_CMEM

	fvegl(JJ) = FCANGAT(JJ,3) + FCANGAT(JJ,4) ! ADD GRASS AND CROPS
	fvegh(JJ) = FCANGAT(JJ,1) + FCANGAT(JJ,2) ! ADD BROAD AND NEEDLE LEAF
	fsand(JJ) = SANDGAT(JJ,1)/100
	fclay(JJ) = CLAYGAT(JJ,1)/100 ! Assume all the same as surface
	
	IF(WATERGAT(JJ)==NMTEST-1) THEN ! SET WATER FRACTION TO 100% FOR SECOND
	fwater(JJ) = 1.0     ! LAST GRU ONLY
	ELSE
	fwater(JJ) = 0.0
	END IF
	


END DO

WHERE ( fTVL > 7_JPIM .and. fwater(:) <  0.5_JPRM ) fTVL = 4_JPIM  ! Defaults low vegetation is grass
WHERE ( fTVL > 7_JPIM .and. fwater(:) >= 0.5_JPRM ) fTVL = 0_JPIM  ! No vegetation on  water pixel
WHERE ( fTVH > 7_JPIM .and. fwater(:) <  0.5_JPRM ) fTVH = 1_JPIM  ! Defaults low vegetation is grass
WHERE ( fTVH > 7_JPIM .and. fwater(:) >= 0.5_JPRM ) fTVH = 0_JPIM  ! No vegetation on  water pixel

! 2.2 Read LAI from MESH
SELECT CASE (CIDVEG)

	CASE ('Ecoclimap')
	DO JJ = 1,N_CMEM 
	IF(fTVL(JJ)==4 .OR. fTVL(JJ)==5) THEN 
	  fs_laiL(JJ) = AIL(JJ, 4) ! GRASS
	ELSE
	  fs_laiL(JJ) = AIL(JJ,3) ! CROPS
	ENDIF

	IF(fTVH(JJ) == 1) THEN
	  zlaiH = AIL(JJ,2) ! BROADLEAF
	ELSEIF(fTVH(JJ) == 3) THEN
	  zlaiH = AIL(JJ,2) ! TROPICAL BROADLEAF
	ELSE
	  zlaiH =AIL(JJ,1)  !NEEDLEAF
	ENDIF
	
	END DO

END SELECT

! 2.3  Calculate tile fractions
!------------------------------

    fvegl(:) =  fvegl(:) * cvegl(:)      
    fvegh(:) =  fvegh(:) * cvegh(:)
    fbare(:) = 1. - fvegl(:) - fvegh(:)  

    ! Snow cover on land tiles

    WHERE (  fsnowd(:) > 0.0_JPRM) sncov(:) = 1.0_JPRM  
    WHERE (  fsnowd(:) <= 0.0_JPRM) sncov(:) = 0.0_JPRM  


    ftfrac(:,1) = fbare(:) * (1. - fwater(:)) * (1.-sncov(:)) ! bare soil 
    ftfrac(:,2) = fbare(:) * (1. - fwater(:)) * sncov(:)      ! bare soil with snow   
    ftfrac(:,3) = fvegl(:) * (1. - fwater(:)) * (1.-sncov(:)) ! low veg
    ftfrac(:,4) = fvegl(:) * (1. - fwater(:)) * sncov(:)      ! low veg with snow
    ftfrac(:,5) = fvegh(:) * (1. - fwater(:)) * (1.-sncov(:)) ! high veg
    ftfrac(:,6) = fvegh(:) * (1. - fwater(:)) * sncov(:)      ! high veg with snow
    ftfrac(:,7) = fwater(:)                                   ! water

	

   SELECT CASE (LOMASK_OCEAN)
       CASE (.True.)
        WHERE ( fwater(:) >= 0.5 )  mask(:) = 0.0_JPRM
    END SELECT



  DEALLOCATE (fbare)
  DEALLOCATE (fvegl)
  DEALLOCATE (fvegh)
  DEALLOCATE (cvegl)
  DEALLOCATE (cvegh)
  DEALLOCATE (sncov)

SELECT CASE (CIATM)
	CASE ('Liebe')
	WRITE(*,*) 'The Liebe option for CIATM is currently not implmented with MESH'
	STOP
END SELECT
!-----------------------------------------------------------------------------
! 5. Read atmospheric temperature input

SELECT CASE (CITVEG)

CASE ('Tair')
! Replace with MESH INPUT KCK
    DO JJ = 1, N_CMEM
    ftair(JJ) = TAGAT(JJ)
	END DO

    ftveg(:) = ftair(:)

!    Update the mask to eliminate unrealistic points
     SELECT CASE (LOMASK_AUTO)
       CASE (.True.)
       WHERE (ftveg < 100.0_JPRM ) mask = 0_JPIM  ! Unrealistic Temperature
     END SELECT
!

   CASE ('Tsurf')

     ftveg(:) = ftl_lsm(:,1)

END SELECT

SELECT CASE (CIATM)

   CASE ('Pellarin', 'Ulaby')

     DO JJ = 1, N_CMEM
      ftair(JJ) = TAGAT(JJ)
	 ENDDO
     
!    Update the mask to eliminate unrealistic points
      SELECT CASE (LOMASK_AUTO)
       CASE (.True.)
       WHERE (ftair < 100.0_JPRM ) mask = 0_JPIM  ! Unrealistic Temperature
      END SELECT
!


END SELECT

  
!>---------------------------------------------------------------
!> Read SSS
!>---------------------------------------------------------------


sal_sea(:) = 32.5_JPRM

END SUBROUTINE RDCMEMMESHSTATIC