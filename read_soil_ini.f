      SUBROUTINE READ_SOIL_INI(NMTEST, IGND, NTYPE, NA, sv)
      USE MESH_INPUT_MODULE
	  USE FLAGS
!> local variables
      INTEGER :: SOIL_IOS, M, I, J
!> read in variables
      INTEGER :: NMTEST, IGND
      INTEGER*4 :: NA, NTYPE
      TYPE(SoilValues) :: sv

!> *********************************************************************
!>  Open and read in values from soil.ini file
!>  Bruce Davison, August 13, 2004
!>  Changes to the soil parameters so that they're read-in directly.
!>  Read in the soil parameters that used to be calculated from %sand, %clay
!> *********************************************************************

      IF (SOILINIFLAG /= 5) RETURN

      OPEN(UNIT=23,file='soil.ini',status='old',iostat=SOIL_IOS)
!> CHECK TO SEE IF THERE IS A new_soil.ini FILE

!todo - change this so it's an option in one of the ini files
!> when SOIL_IOS is 0, the file opened successfully.
      IF(soil_ios/=0)THEN 
        PRINT*,"ERROR: The soil.ini file was not found."
        PRINT*,"You can set SOILINIFLAG to ",
     *   	   "values less than 5 and MESH will use soil percentages ",
     *	       "from MESH_parameters_CLASS.ini file."
        PRINT*,"Below is what MESH will do if the sum of soil ",
     *         "percentages is greater than 100%:"
		PRINT*,"For SOILINIFLAG set to 1 - ",
     1  		"MESH will use the soil percentages as specified"
		PRINT*,"For SOILINIFLAG set to 2 - ",
     2	        "MESH will adjust soil percentages in favor of sand"
		PRINT*,"For SOILINIFLAG set to 3 - ",
     3	        "MESH will adjust soil percentages in favor of clay"
		PRINT*,"For SOILINIFLAG set to 4 - ",
     4	        "MESH will proportionally adjust the soil percentages"
		STOP
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

        DO I=1,NA
          DO M=1,NMTEST
            DO J=1,3
              sv%wc_thpor (I,M,J) = sv%wc_thpor(1,M,J)
              sv%wc_thlret(I,M,J) = sv%wc_thlret(1,M,J)
              sv%wc_thlmin(I,M,J) = sv%wc_thlmin(1,M,J)
              sv%wc_bi    (I,M,J) = sv%wc_bi(1,M,J)
              sv%wc_psisat(I,M,J) = sv%wc_psisat(1,M,J)
              sv%wc_grksat(I,M,J) = sv%wc_grksat(1,M,J)
              sv%wc_hcps  (I,M,J) = sv%wc_hcps(1,M,J)
              sv%wc_tcs   (I,M,J) = sv%wc_tcs(1,M,J)
            END DO
            DO J=4,IGND
              sv%wc_thpor (I,M,J) = sv%wc_thpor(I,M,3)
              sv%wc_thlret(I,M,J) = sv%wc_thlret(I,M,3)
              sv%wc_thlmin(I,M,J) = sv%wc_thlmin(I,M,3)
              sv%wc_bi    (I,M,J) = sv%wc_bi(I,M,3)
              sv%wc_psisat(I,M,J) = sv%wc_psisat(I,M,3)
              sv%wc_grksat(I,M,J) = sv%wc_grksat(I,M,3)
              sv%wc_hcps  (I,M,J) = sv%wc_hcps(I,M,3) 
              sv%wc_tcs   (I,M,J) = sv%wc_tcs(I,M,3)
            END DO
          END DO
        END DO
      ENDIF

      CLOSE(unit=23)

      RETURN
      END SUBROUTINE READ_SOIL_INI