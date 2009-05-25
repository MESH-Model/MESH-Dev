      SUBROUTINE READ_SOIL_INI(NMTEST, IGND, NTYPE, NA, sv, SOILINIFLAG)
      USE MESH_INPUT_MODULE
!> local variables
      INTEGER :: SOIL_IOS, M, I, J
!> read in variables
      INTEGER :: NMTEST, IGND, SOILINIFLAG
      INTEGER*4 :: NA, NTYPE
      TYPE(SoilValues) :: sv

!> *********************************************************************
!>  Open and read in values from soil.ini file
!>  Bruce Davison, August 13, 2004
!>  Changes to the soil parameters so that they're read-in directly.
!>  Read in the soil parameters that used to be calculated from %sand, %clay
!> *********************************************************************

      IF (SOILINIFLAG == 0) THEN
        RETURN
      ELSE IF (SOILINIFLAG /= 1) THEN
        PRINT *, "Error: The SOILINIFLAG has been set to an invalid ",
     *           "value", SOILINIFLAG, ". Correct values are 0 (",
     *           "do not read in soil.ini) and 1 (read in soil.ini)."
        STOP
      END IF

      OPEN(UNIT=23,file='soil.ini',status='old',iostat=SOIL_IOS)
!> CHECK TO SEE IF THERE IS A new_soil.ini FILE

!todo - change this so it's an option in one of the ini files
!> when SOIL_IOS is 0, the file opened successfully.
      IF(soil_ios/=0)THEN 
        PRINT *, "The soil.ini file was not found when the SOILINIFLAG",
     *           " has been set to 1. Please either change the ",
     *           "SOILINIFLAG to be 0 or create the soil.ini file."
      ELSE
        PRINT*,'The soil.ini file was found'
        PRINT*,'CLASSBHYD.f will be used'
        PRINT*,'-----------------------------------'
        READ (23,*)
        READ (23,*) (sv%wc_thpor (1,m,1),m=1,NMTEST)
        READ (23,*)
        READ (23,*) (sv%wc_thpor (1,m,2),m=1,NMTEST)
        READ (23,*)
        READ (23,*) (sv%wc_thpor (1,m,3),m=1,NMTEST)
        READ (23,*)
        READ (23,*) (sv%wc_thlret(1,m,1),m=1,NMTEST)
        READ (23,*)
        READ (23,*) (sv%wc_thlret(1,m,2),m=1,NMTEST)
        READ (23,*)
        READ (23,*) (sv%wc_thlret(1,m,3),m=1,NMTEST)
        READ (23,*)
        READ (23,*) (sv%wc_thlmin(1,m,1),m=1,NMTEST)
        READ (23,*)
        READ (23,*) (sv%wc_thlmin(1,m,2),m=1,NMTEST)
        READ (23,*)
        READ (23,*) (sv%wc_thlmin(1,m,3),m=1,NMTEST)
        READ (23,*)
        READ (23,*) (sv%wc_bi    (1,m,1),m=1,NMTEST)
        READ (23,*)
        READ (23,*) (sv%wc_bi    (1,m,2),m=1,NMTEST)
        READ (23,*)
        READ (23,*) (sv%wc_bi    (1,m,3),m=1,NMTEST)
        READ (23,*)
        READ (23,*) (sv%wc_psisat(1,m,1),m=1,NMTEST)
        READ (23,*)
        READ (23,*) (sv%wc_psisat(1,m,2),m=1,NMTEST)
        READ (23,*)
        READ (23,*) (sv%wc_psisat(1,m,3),m=1,NMTEST)
        READ (23,*)
        READ (23,*) (sv%wc_grksat(1,m,1),m=1,NMTEST)
        READ (23,*)
        READ (23,*) (sv%wc_grksat(1,m,2),m=1,NMTEST)
        READ (23,*)
        READ (23,*) (sv%wc_grksat(1,m,3),m=1,NMTEST)
        READ (23,*)
        READ (23,*) (sv%wc_hcps  (1,m,1),m=1,NMTEST)
        READ (23,*)
        READ (23,*) (sv%wc_hcps  (1,m,2),m=1,NMTEST)
        READ (23,*)
        READ (23,*) (sv%wc_hcps  (1,m,3),m=1,NMTEST)
        READ (23,*)
        READ (23,*) (sv%wc_tcs   (1,m,1),m=1,NMTEST)
        READ (23,*)
        READ (23,*) (sv%wc_tcs   (1,m,2),m=1,NMTEST)
        READ (23,*)
        READ (23,*) (sv%wc_tcs   (1,m,3),m=1,NMTEST)
        READ (23,*)
        READ (23,*) (sv%wc_algwet(1,m),m=1,NMTEST)
        READ (23,*)
        READ (23,*) (sv%wc_algdry(1,m),m=1,NMTEST)

        DO I=2,NA
          DO M=1,NMTEST
            DO J=1,3
              sv%wc_thpor(I,M,J) = sv%wc_thpor(1,M,J)
              sv%wc_thlret(I,M,J) = sv%wc_thlret(1,M,J)
              sv%wc_thlmin(I,M,J) = sv%wc_thlmin(1,M,J)
              sv%wc_bi(I,M,J) = sv%wc_bi(1,M,J)
              sv%wc_psisat(I,M,J) = sv%wc_psisat(1,M,J)
              sv%wc_grksat(I,M,J) = sv%wc_grksat(1,M,J)
              sv%wc_hcps(I,M,J) = sv%wc_hcps(1,M,J)
              sv%wc_tcs(I,M,J) = sv%wc_tcs(1,M,J)
            END DO
            sv%wc_algwet(I,M) = sv%wc_algwet(1,M)
            sv%wc_algdry(I,M) = sv%wc_algdry(1,M)
          END DO
        END DO
      ENDIF

      CLOSE(unit=23)

      RETURN
      END SUBROUTINE READ_SOIL_INI