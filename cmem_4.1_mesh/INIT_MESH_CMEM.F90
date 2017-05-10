SUBROUTINE INIT_MESH_CMEM(ILG,NML,NA,NTYPE,NMTEST,IGND,ICAN,ILMOS,JLMOS,ZBOT,ELEV)

USE PARKIND1, ONLY : JPIM, JPRM
USE YOMLUN, ONLY : NULOUT, NULTMP
USE YOMCMEMPAR
USE YOMCMEMFIELDS
USE YOMCMEMSOIL
USE YOMCMEMVEG, ONLY : wc_veg, tb_veg, w_eff, bj, tauN, &
					& tb_veg, tau_veg, t_veg, a_geo, a_geoL, a_geoH, tth, ttv
USE YOMCMEMATM, ONLY : tau_atm,tb_au,tb_ad,tb_toa,tb_tov,fZ,tair &
					&, fs_tatm, fs_spres, fs_R, t_atm, r_atm, p_atm, z_atm, rh_atm, ah
USE MESH_CMEM_MODULE

IMPLICIT NONE

INTEGER :: ILG, NML, NA, NTYPE, IGND, NMTEST, ICAN
INTEGER :: ILMOS(ILG), JLMOS(ILG)
REAL :: ZBOT(IGND)
REAL :: ELEV(NA)

INTEGER :: I, M, IOS

!> ALLOCATE AND INITIALIZE READING VARIABLES

! AIL IS ALLOCATED TO BE READ BY CLASSA
ALLOCATE(AIL(ILG,ICAN))
DO I = 1,ILG
	DO M = 1,ICAN
	AIL(I,M) = 0.
	END DO
END DO

ALLOCATE(TBH(ILG,CMEMFLAG), TBV(ILG,CMEMFLAG), TAUH(ILG,CMEMFLAG), TAUV(ILG,CMEMFLAG))
DO I = 1,ILG
  DO M = 1,CMEMFLAG
  TBH(I,M) = 0.0
  TBV(I,M) = 0.0
  TAUH(I,M) = 0.0
  TAUV(I,M) = 0.0
  END DO
END DO

CALL CMEM_SETUP


!> CHECK TO SEE IF THE SOIL LEVELS SPECIFIED IN MESH ARE CONSISTENT WITH
!> THE DEFAULTS FOR CMEM
IF (ZBOT(1) .ne. 0.07 .OR. ZBOT(2) .ne. 0.28 .OR. ZBOT(3) .ne. 1.0) THEN
	WRITE(*,*) "WARNING: The depth of soil layers specified in MESH_input_soil_levels", &
	& " is not consistent with default CMEM values. The default CMEM soil depths", &
	& "for MESH_input_soil_levels are 0.07m, 0.21m and 0.72m."
	WRITE(*,*) "The default CMEM soil depths are 0.07. 0.28m and 1.0m"
	
END IF 

IF(ZBOT(1) .GT. 0.10001) THEN
	WRITE(*,*) "ERROR: The depth of the first soil layer is greater", &
	& " than 0.1m. This is beyond microwave penetration depths. ",&
	& " Change the soil depths in MESH_input_soil_levels.txt or set",&
	& " CMEMFLAG to 0"
	STOP
END IF

!> Get information on the input files and allocate the variables
!> In CMEM this is where RDCMEM*INFO would go to check the consistenty
!> of the length of the input variables and set N. This is not required
!> in the MESH coupling. 

!> SET CMEM NUMBER OF GRID CELLS
N_CMEM = NML ! NML is the useful cells in GAT variables

! 1.2 Allocate CMEM variables according to the size of input 
!
ALLOCATE (ftau_atm(N_CMEM))
ALLOCATE (ftau_veg(N_CMEM,2))
ALLOCATE (ftb_au(N_CMEM))
ALLOCATE (ftb_ad(N_CMEM))
ALLOCATE (ftair(N_CMEM))
ALLOCATE (fsnowd(N_CMEM))          
ALLOCATE (frsnow(N_CMEM))  
ALLOCATE (fwater(N_CMEM))
ALLOCATE (mask(N_CMEM))
ALLOCATE (fTVL(N_CMEM))
ALLOCATE (fTVH(N_CMEM))
ALLOCATE (fs_laiL(N_CMEM))
ALLOCATE (ftfrac(N_CMEM,JPCMEMTILE))
ALLOCATE (ftskin(N_CMEM))
ALLOCATE (ftl_lsm(N_CMEM,nlay_soil_ls))
ALLOCATE (fwc_lsm(N_CMEM,nlay_soil_ls))
ALLOCATE (fsand(N_CMEM))
ALLOCATE (fclay(N_CMEM))
ALLOCATE (frho_b(N_CMEM))
ALLOCATE (fp(N_CMEM))
ALLOCATE (fWP(N_CMEM))
ALLOCATE (falpha(N_CMEM))
ALLOCATE (fsal_wat(N_CMEM))
ALLOCATE (fh(N_CMEM))
ALLOCATE (ftveg(N_CMEM))
ALLOCATE (fwc_veg(N_CMEM,2))
ALLOCATE (fb(N_CMEM,2))
ALLOCATE (ftauN(N_CMEM,2))
ALLOCATE (fw_effL(N_CMEM,2))
ALLOCATE (fw_effH(N_CMEM,2))
ALLOCATE (tsoil(nlay_soil_mw))  
ALLOCATE (fNrh_L(N_CMEM))
ALLOCATE (fNrh_H(N_CMEM))
ALLOCATE (fNrv_L(N_CMEM))
ALLOCATE (fNrv_H(N_CMEM))
ALLOCATE(fhrmodel(N_CMEM,2))
ALLOCATE (ftth(N_CMEM,2))
ALLOCATE (fttv(N_CMEM,2))
ALLOCATE (fZ(N_CMEM))
ALLOCATE (sal_sea(N_CMEM))
ALLOCATE(ECHGRUROW(NA,NMTEST))
ALLOCATE(ECLGRUROW(NA,NMTEST))
ALLOCATE(WATERROW(NA,NMTEST))
ALLOCATE(ELEVROW(NA,NMTEST))
ALLOCATE(ECLGRUGAT(ILG))
ALLOCATE(ECHGRUGAT(ILG))
ALLOCATE(WATERGAT(ILG))
ALLOCATE(ELEVGAT(ILG))
!
!
SELECT CASE (CIATM)
    CASE ( 'Liebe' )
	WRITE(*,*) 'ERROR:The CMEM CIATM Liebe option is currently not'
	WRITE(*,*) 'available with MESH. Please select another '
	WRITE(*,*) 'option for CIATM.'
	STOP
    !ALLOCATE (fs_spres(N_CMEM))
    !ALLOCATE (fs_R(N_CMEM,nobs_atm))
    !ALLOCATE (fs_tatm(N_CMEM,nobs_atm))
    !ALLOCATE (r_atm(nobs_atm))
    !ALLOCATE (p_atm(nobs_atm))
    !ALLOCATE (z_atm(nobs_atm))
    !ALLOCATE (t_atm(nobs_atm))
    !ALLOCATE (rh_atm(nobs_atm))
    !ALLOCATE (ah(nobs_atm))
END SELECT
!
!  allocate output
!  ----------------
!
ALLOCATE ( ftb_soil(N_CMEM,2) )
ALLOCATE ( ftb_toa(N_CMEM,2) )
ALLOCATE ( fsurf_emis(N_CMEM,2) )
ALLOCATE ( fteffC(N_CMEM,3) )  

!> READ IN THE ECOCLIMAP DOMINANT HIGH AND LOW VEGETATION TYPE FOR
!> EACH GRU. NOTE THERE IS CURRENTLY A LIMITATION IN THIS CODE WHERE
!> EACH GRU CAN HAVE ONLY ONE DOMINANT HIGH AND LOW VEGETATION TYPE
!> IF BOTH BROADLEAF AND NEEDLE LEAF EXIST IN THE SAME GRU ONE WILL 
!> NEED TO BE CHOSEN

!LGPRINT = .FALSE. !> Turn off CMEM printing

OPEN(NULTMP,FILE='MESH_cmem_ecoclimap.ini',STATUS='OLD',IOSTAT=IOS)
  IF(IOS .NE. 0) THEN
  WRITE(*,*) 'The file MESH_cmem_ecoclimap.ini is missing'
  WRITE(*,*) 'This file must be included'
  STOP
  END IF
  READ(NULTMP,*,IOSTAT=IOS) !Skip header
  DO I = 1,NMTEST
	READ(NULTMP,*,IOSTAT=IOS) ECHGRUROW(1,I), ECLGRUROW(1,I)
	
	IF(IOS < 0) THEN
	WRITE(*,*) 'ERROR: Reached the end of MESH_cmem_ecoclimap.ini'
	WRITE(*,*) 'before the number of GRUs. There should be'
	WRITE(*,*) NMTEST, 'rows with two variables'
	STOP
	END IF
   END DO
CLOSE(NULTMP)

! GET WATER FOR TRACKING WATERBODIES IN MESH
DO M = 1,NMTEST
 WATERROW(1,M) = M
END DO

! Read and distribute the data to all cells in the same way that the 
! ROW values are distributed

DO I = 2,NA
	DO M = 1,NMTEST
	ECHGRUROW(I,M) = ECHGRUROW(1,M)
	ECLGRUROW(I,M) = ECLGRUROW(1,M)
	END DO
	
END DO

DO I = 2,NA
	DO M = 1,NMTEST
	WATERROW(I,M) = WATERROW(1,M)
	ELEVROW(I,M) = ELEV(I)
	END DO
END DO

!> DISTRIBUTE TO GAT STYLE ARRAYS
DO M = 1,NML
    ECHGRUGAT(M) = ECHGRUROW(ILMOS(M),JLMOS(M))
	ECLGRUGAT(M) = ECLGRUROW(ILMOS(M),JLMOS(M))
	WATERGAT(M) = WATERROW(ILMOS(M), JLMOS(M))
	ELEVGAT(M) = ELEVROW(ILMOS(M),JLMOS(M))
END DO

!> Read geopotential fractions (aka height in km)


DO JJ = 1,N_CMEM ! LOOP THROUGH THE GRID CELLS
		fZ(JJ) = ELEVGAT(NA)/1000._JPRM !> CONVERT TO KM
		
END DO

PRINT*, ' '
PRINT*, 'RUNNING MESH WITH CMEM 4.1 '
PRINT*, ' '

DEALLOCATE(ECHGRUROW)
DEALLOCATE(ECLGRUROW)
DEALLOCATE(WATERROW)
DEALLOCATE(ELEVROW)
DEALLOCATE(ELEVGAT)

END SUBROUTINE INIT_MESH_CMEM