SUBROUTINE RUN_MESHCMEM(M)

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

INTEGER :: M


THETA = CMEMTHETA(M) 
COSTHETA = COS(THETA*PI_CMEM/180._JPRM)
SINTHETA = SIN(THETA*PI_CMEM/180._JPRM)

IF (CIRGHR == 'Wegmeuller' .AND. THETA >= 70. ) THEN
CALL ABOR1('CMEM_RUN: CIRGHR == Wegmueller AND theta >=70 ')
END IF


CALL CMEM_INIT

FIELD: DO JJ = 1, N_CMEM 
!
! 3.1 Initialize
!---------------
!
tb_soil(:) = (/0.,0./)
tb_veg(:) = (/0.,0./)
tb_toa(:) = (/0.,0./)
fsurf_emis(JJ,:) = (/0.,0./) 
ftb_soil(JJ,:) = (/0.,0./) 
ftau_veg(JJ,:) = (/0.,0./) 
!  
SELECT CASE (MASK(JJ))  
!
  CASE ( 0_JPIM )    ! points on which TB is not computed (merge MASK_AUTO and MASK_OCEAN)
!
    CYCLE
!
  CASE ( 1_JPIM )    ! points on which TB is computed
!
! Cell values
  tfrac(:) = (ftfrac(JJ,:))
  tau_atm = ftau_atm(JJ) / costheta
  tb_au = ftb_au(JJ)
  tb_ad = ftb_ad(JJ)
  t_veg = ftveg(JJ)
  tair = ftair(JJ)

  DO JCMEMTILE = 1,JPCMEMTILE
! 3.2 Compute surface emissivity    
!
    SELECT CASE (JCMEMTILE)  !1
      CASE ( 1,2 )
        Nrh = fNrh_L(JJ)
        Nrv = fNrv_L(JJ)
        hrmodel = fhrmodel(JJ,1)
        tsoildeep = ftl_lsm(JJ,nlay_soil_ls)
        CALL CMEM_SOIL
        IF (LGPRINT) WRITE(NULOUT,*) '--- Mean TEFF bf',&
          & ftfrac(JJ,JCMEMTILE),fteffC(JJ,1),t_eff(1),fteffC(JJ,2),t_eff(2),&
          & fteffC(JJ,3),t_eff(3)
        fteffC(JJ,:) = fteffC(JJ,:) + ftfrac(JJ,JCMEMTILE) * t_eff(:)
        IF (LGPRINT) WRITE(NULOUT,*) '--- Mean TEFF b',fteffC(JJ,:)
      CASE ( 6 )
        Nrh = fNrh_H(JJ)
        Nrv = fNrv_H(JJ)
        hrmodel = fhrmodel(JJ,2)
        tsoildeep = ftl_lsm(JJ,1)
        CALL CMEM_SOIL
        IF (LGPRINT) WRITE(NULOUT,*) '--- Mean TEFF bf',&
          & ftfrac(JJ,JCMEMTILE),fteffC(JJ,1),t_eff(1),fteffC(JJ,2),t_eff(2),&
          & fteffC(JJ,3),t_eff(3)
        fteffC(JJ,:) = fteffC(JJ,:) + ftfrac(JJ,JCMEMTILE) * t_eff(:)
        IF (LGPRINT) WRITE(NULOUT,*) '--- Mean TEFF c',fteffC(JJ,:)
      CASE (  3, 4 )
        Nrh = fNrh_L(JJ)
        Nrv = fNrv_L(JJ)
        hrmodel = fhrmodel(JJ,1)
        tsoildeep = ftl_lsm(JJ,nlay_soil_ls)
        CALL CMEM_SOIL
        IF (LGPRINT) WRITE(NULOUT,*) '--- Mean TEFF bf',&
          & ftfrac(JJ,JCMEMTILE),fteffC(JJ,1),t_eff(1),fteffC(JJ,2),t_eff(2),&
          & fteffC(JJ,3),t_eff(3)
        fteffC(JJ,:) = fteffC(JJ,:) + ftfrac(JJ,JCMEMTILE) * t_eff(:)
        IF (LGPRINT) WRITE(NULOUT,*) '--- Mean TEFF d',fteffC(JJ,:)
      CASE ( 5 )
        Nrh = fNrh_H(JJ)
        Nrv = fNrv_H(JJ)
        hrmodel = fhrmodel(JJ,2)
        tsoildeep = ftl_lsm(JJ,nlay_soil_ls)
        CALL CMEM_SOIL
        IF (LGPRINT) WRITE(NULOUT,*) '--- Mean TEFF bf',&
          & ftfrac(JJ,JCMEMTILE),fteffC(JJ,1),t_eff(1),fteffC(JJ,2),t_eff(2),&
          & fteffC(JJ,3),t_eff(3)
        fteffC(JJ,:) = fteffC(JJ,:) + ftfrac(JJ,JCMEMTILE) * t_eff(:)
        IF (LGPRINT) WRITE(NULOUT,*) '--- Mean TEFF e',fteffC(JJ,:)
      CASE ( 7 )
        Nrh = 0.0_JPRM
        Nrv = 0.0_JPRM
        hrmodel = 0.0_JPRM
        tsoildeep = ftl_lsm(JJ,nlay_soil_ls)
        CALL CMEM_SOIL
        IF (LGPRINT) WRITE(NULOUT,*) '--- Mean TEFF bf',&
          & ftfrac(JJ,JCMEMTILE),fteffC(JJ,1),t_eff(1),fteffC(JJ,2),t_eff(2),&
          & fteffC(JJ,3),t_eff(3)
        !fteffC(JJ,1) = fteffC(JJ,1) + ftfrac(JJ,JCMEMTILE) * t_eff(1)
        fteffC(JJ,2) = fteffC(JJ,2) + ftfrac(JJ,JCMEMTILE) * t_eff(2)
        fteffC(JJ,3) = fteffC(JJ,3) + ftfrac(JJ,JCMEMTILE) * t_eff(3)
        IF (LGPRINT) WRITE(NULOUT,*) '--- Mean TEFF f',fteffC(JJ,:)
    END SELECT !JCMEMTILE 1
!          
! 3.3 Compute vegetation emissivity if present          
!
!
    SELECT CASE (JCMEMTILE)  !2
      CASE ( 3, 4 )  ! Low vegetation
        w_eff(:) = fw_effL(JJ,:) 
        a_geo = a_geoL 
        wc_veg =  fwc_veg(JJ,1)
        bj =  fb(JJ,1)
        tauN = ftauN(JJ,1)
        tth = ftth(JJ,1)
        ttv = fttv(JJ,1)
        IF (tb_veg(1) == 0.)  CALL CMEM_VEG   
     CASE ( 5, 6)  ! High vegetation
        w_eff(:) = fw_effH(JJ,:)
        a_geo = a_geoH 
        wc_veg = fwc_veg(JJ,2)
        bj =  fb(JJ,2)
        tauN = ftauN(JJ,2)
        tth = ftth(JJ,2)
        ttv = fttv(JJ,2)
        CALL CMEM_VEG         
     CASE ( 1, 2, 7 )
        tau_veg(:) = (/0.,0./)
        tb_veg(:) = (/0.,0./)
    END SELECT     !JCMEMTILE2
!
!
! 3.4 Compute top-of-vegetation brightness temperature
!
    CALL CMEM_RTM
!     
!  3.5 Add snow layer: tb_tov-->tb top-of-snow
!
    SELECT CASE (JCMEMTILE)  !3
      CASE ( 2_JPIM, 4_JPIM, 6_JPIM )
        XMV = 0.1 ! Snow Moisture, should be from data field
        DO JJPOL = 1,2    ! h- and v-polarization
          RSN(JJPOL)=1._JPRM  - tb_tov(JJPOL) / ftl_lsm(JJ,1)
        ENDDO
        CALL CMEM_SNOW(RSN,frsnow(JJ)/1000.,fsnowd(JJ),ESN)
    END SELECT    !JCMEMTILE 3
!    
!
! 3.6 Add vegetation layer for High vegetation over snow
!
    SELECT CASE (JCMEMTILE)  !4
      CASE ( 6_JPIM )  ! High vegetation, snow
        ! soil
        DO JJPOL = 1,2    ! h- and v-polarization
          r_r(JJPOL)=1. - ESN(JJPOL)
        ENDDO
        tb_soil = tb_tov
        ! vegetation
        w_eff(:) = fw_effH(JJ,:) 
        a_geo = a_geoH 
        wc_veg = fwc_veg(JJ,2)
        bj =  fb(JJ,2)
        CALL CMEM_VEG   
        ! Tb top-of-vegetation
        CALL CMEM_RTM
    END SELECT   !JCMEMTILE 4
!           
! 3.7 Compute contribution to top-of-atmosphere brightness temperature
!
     tb_toa(:) = tb_toa(:) + ftfrac(JJ,JCMEMTILE) * tb_tov(:)
     fsurf_emis(JJ,:) = fsurf_emis(JJ,:) + ftfrac(JJ,JCMEMTILE) * surf_emis(:)
     ftb_soil(JJ,:) = ftb_soil(JJ,:) + ftfrac(JJ,JCMEMTILE) * tb_soil(:)
!    Diagnostic Tau_veg
     ftau_veg(JJ,:) = ftau_veg(JJ,:) + ftfrac(JJ,JCMEMTILE) * tau_veg(:)   
!    Diagnostic mean TEFF
     IF (LGPRINT) WRITE(NULOUT,*) 'end of cmem tile loop',JCMEMTILE
     IF (LGPRINT) WRITE(NULOUT,*) '                 TB TOV',tb_tov(:)
     IF (LGPRINT) WRITE(NULOUT,*) '                 tb_soil',tb_soil(:)
     
!
 ENDDO !end of JCMEMTILE
!
!
!   3.7 Top-of-Atmosphere brightness temperature
!
  IF (LGPRINT) WRITE(NULOUT,*) '--- TB no atm:',tb_toa(:)
  ftb_toa(JJ,:) = tb_toa(:) * exp(-tau_atm) + ftb_au(JJ)   
  IF (LGPRINT) WRITE(NULOUT,*) '--- TBTOA:',ftb_toa(JJ,:)
!
!
END SELECT  ! end of mask selection
!
END DO FIELD







END SUBROUTINE RUN_MESHCMEM