      program connect_dds_MESH
C     ------------------------------------------------------------------------------------
C     BASED ON DDS VARIABLE VALUES, THIS PROGRAM UPDATES THE FOLLOWING INPUT FILES:
C     1. MESH_parameters_CLASS.ini
C     2. MESH_parameters_hydrology.ini
C
C     ------------------------------------------------------------------------------------
C     ORIGINALLY WRITTEN BY F.SEGLENIEKS
C     MODIFIED BY M.A.MEKONNEN/B.DAVISON/M.MACDONALD - MARCH 12, 2010
C     ------------------------------------------------------------------------------------

C     DEFINITIONS - MESH_parameters_hydrology.ini
C     SOIL_POR_MAX    - Maximum soil porosity
C     SOIL_DEPTH      - Depth from surface to bottom of rooting zone for maximum water holding capacity, m
C     S0              - Surface saturations [0.75 - 1.0]
C     T_ICE_LENS      - Overnight minimum to cause ice lens after major melt [-50 - 0.0 ºC]
C     FZRCROW         - Coefficient for the frozen soil infiltration parametric equation [1 - 3]


C     DEFINITIONS - MESH_parameters_CLASS.ini
C 
C     TITLE           - 24 CHARACTERS FOR TITLE OF MODEL RUN
C     NAME            - 24 CHARACTERS FOR THE NAMES OF THE RESEARCHERS
C     PLACE           - 24 CHARACTERS FOR CORRESPONDING PLACES OF RESEARCHERS
C     DEGLAT          - LATITUDE OF STUDY AREA IN DEGREES (POSITIVE NORTHWARD, NEGATIVE SOUTHWARD)
C     DEGLON          - LONGITUDE OF STUDY AREA IN DEGREES MEASURED EAST OF GREENWICH
C     ZRFMGRD         - REFERENCE HEIGHT (MEASUREMENT HEIGHT) FOR MOMENTUM (WIND SPEED)
C     ZRFHGRD         - REFERENCE HEIGHT (MEASUREMENT HEIGHT) FOR HEAT (TEMPERATURE AND HUMIDITY)
C     ZBLDGRD         - BLENDING HEIGHT FOR AGGREGATING SURFACE ROUGHNESS (50 m IS REASONABLE)
C     GCGRD           - THE GCM SURFACE DESCRIPTOR VARIABLE. FOR LAND SURFACES (INCLUDING INLAND WATER) IT HAS A
C                       VALUE OF -1.
C     NLTEST          - NUMBER OF GRID CELLS BEING RUN
C     NMTEST          - NUMBER OF MOSAIC TILES BEING USED
C     FCANROW         - FRACTION OF THE GRID CELL OCCUPIED BY LAND COVER CATEGORIES
C     LAMXROW         - MAXIMUM LEAF AREA INDEX FOR THE VEGETATION TYPES
C     LNZ0ROW         - NATURAL LOGARITHM OF THE ROUGHNESS LENGTH FOR LAND COVER CATEGORIES
C     LAMNROW         - MINIMUM LEAF AREA INDEX FOR THE VEGETATION TYPES
C     ALVCROW         - VISIBLE ALBEDO FOR LAND COVER CATEGORIES
C     LAMNROW         - STANDING BIOMASS DENSITY (KG/M2) FOR THE VEGETATION TYPES
C     ALICROW         - NEAR INFRARED ALBEDO FOR LAND COVER CATEGORIES
C     ROOTROW         - ROOTING DEPTH FOR THE 4 VEGETATION TYPES
C     RSMNROW         - MINIMUM STOMATAL RESISTANCE FOR THE VEGETATION TYPES
C     QA50ROW         - COEFFICIENT GOVERNING THE RESPONSE OF STOMATES TO LIGHT
C                       (COMMON VALUES - 30 TO 50 W/M2)
C     VPDAROW         - COEFFICIENT GOVERNING THE RESPONSE OF STOMATES TO VAPOR PRESSURE DEFICIT
C                       (COMMON VALUES - 0.5)
C     VPDBROW         - COEFFICIENT GOVERNING THE RESPONSE OF STOMATES TO VAPOR PRESSURE DEFICIT
C                       (COMMON VALUES - 1.0)
C     PSGAROW         - COEFFICIENT GOVERNING THE RESPONSE OF STOMATES TO SOIL WATER SUCTION
C                       FOR THE 4 VEGETATION TYPES (COMMON VALUES - 100)
C     PSGBROW         - COEFFICIENT GOVERNING THE RESPONSE OF STOMATES TO SOIL WATER SUCTION
C                       (COMMON VALUES - 5)
C     DRNROW          - DRAINAGE INDEX, SET TO 1 TO ALLOW SOIL PHYSICS TO MODEL DRAINAGE, 
C                       AND TO A VALUE BETWEEN 0 AND 1 TO SIMULATE IMPEDDED DRAINAGE. THE 
C                       CALCULATED DRAINAGE IS MULTIPLIED BY THIS VALUE.
C     SDEPROW         - THE PERMEABLE DEPTH OF THE SOIL COLUMN
C     FAREROW         - WHEN RUNNING A MOSAIC, THE FRACTIONAL AREA THAT THIS 
C                       TILE REPRESENTS IN A GRID CELL
C     DDENROW         - THE DRAINAGE DENSITY OF THE GRU IN KM/KM2
C     XSLPROW         - AVERAGE OVERLAND SLOPE OF A GIVEN GRU
C     GRKFROW         - THE FRACTION XDRAINH = K/KS, WHERE K IS THE HORIZONTAL 
C                       CONDUCTIVITY OF A SATURATED SOIL AT A DEPTH OF h0 (USUALLY 1m) AND 
C                       KS IS THE HORIZONTAL CONDUCTIVITY OF A SATURATED SOIL AT THE 
C                       SURFACE
C     MANNROW         - MANNING ROUGHNESS COEFFICIENT
C     WFCIROW         - HORIZONTAL CONDUCTIVITY OF A SATURATED SOIL AT THE 
C                       SURFACE (KS)
C     MIDROW          - MOSAIC TILE IDENTIFIER, WHICH HAS A VALUE OF 1 FOR LAND
C     SANDROW         - PERCENTAGES OF SAND CONTENT OF THE SOIL LAYERS 
C     CLAYROW         - PERCENTAGES OF CLAY CONTENT OF THE SOIL LAYERS 
C     ORGMROW         - PERCENTAGES OF ORGANIC MATTER OF THE SOIL LAYERS 
C     TBARROW         - TEMPERATURE OF THE SOIL LAYERS (IN DEGREES CENTIGRADE)
C     TCANROW         - CANOPY TEMPERATURE (IN DEGREES CENTIGRADE)
C     TSNOROW         - TEMPERATURE OF THE SNOW PACK (IN DEGREES CENTIGRADE)
C     TPNDROW         - TEMPERATURE OF PONDED WATER ON THE SURFACE (IN DEGREES CENTIGRADE)
C     THLQROW         - FRACTIONAL VOLUME OF LIQUID WATER IN THE SOIL LAYERS
C     THICROW         - FRACTIONAL VOLUME OF FROZEN WATER IN THE SOIL LAYERS
C     ZPNDROW         - DEPTH OF WATER PONDED ON THE SURFACE
C     RCANROW         - LIQUID WATER HELD ON THE VEGETATION CANOPY (KG/M2)
C     SCANROW         - FROZEN WATER HELD ON THE VEGETATION CANOPY (KG/M2)
C     SNOROW          - SNOW MASS PRESENT ON THE GROUND (KG/M2)
C     ALBSROW         - ALBEDO OF THE SNOW
C     RHOSROW         - DENSITY OF THE SNOW
C     GROROW          - VEGETATION GROWTH INDEX. SET TO 0 BEFORE LEAF-OUT, 
C                       1 WHEN IN FULL LEAF, AND ESTIMATE IN BETWEEN

	IMPLICIT NONE

	INTEGER,PARAMETER :: NNM   = 6     ! NUMBER OF NAMES OF THE RESEARCHERS
	INTEGER,PARAMETER :: NPL   = 6     ! NAMES OF CORRESPONDING PLACES
	INTEGER,PARAMETER :: NVGT  = 4     ! 4 VEGETATION TYPES
	INTEGER,PARAMETER :: NLCR  = 5     ! 4 VEGETATION TYPES + URBAN AREAS
	INTEGER,PARAMETER :: NSL   = 3     ! NUMBER OF SOIL LAYERS
	INTEGER,PARAMETER :: NRC   = 5     ! NUMBER OF RIVER CLASSES
	INTEGER,PARAMETER :: NVAR  = 200   ! MAXIMUM NUMBER OF OPTIMIZED VARIABLES
      INTEGER,PARAMETER :: NROW  = 100   ! MAXIMUM NUMBER OF ROWS	
	
      CHARACTER*24 TITLE
      CHARACTER*4  NAME(NNM),PLACE(NPL),DEPPARLABEL(10)
	CHARACTER    HEAD(30)*85
		
	CHARACTER,DIMENSION(:,:), ALLOCATABLE :: LABEL*40
	
	INTEGER,DIMENSION(:,:), ALLOCATABLE   :: INOUT
	INTEGER,DIMENSION(:), ALLOCATABLE     :: MIDROW
      
      REAL,DIMENSION(:,:), ALLOCATABLE :: TOTAL,PERCENT,
     +     FCANROW,LAMXROW,LNZ0ROW,LAMNROW,ALVCROW,CMASROW,ALICROW,
     +     ROOTROW,RSMNROW,QA50ROW,VPDAROW,VPDBROW,PSGAROW,PSGBROW,
     +     THLQROW,THICROW,SANDROW,CLAYROW,ORGMROW,TBARROW,MINLIMIT,
     +     MAXLIMIT,VARORG,VARNEW

      REAL,DIMENSION(:),ALLOCATABLE :: DRNROW,SDEPROW,FAREROW,DDENROW,
     +     XSLPROW,GRKFROW,WFSFROW,WFCIROW,TCANROW,TSNOROW,
     +     TPNDROW,ZPNDROW,RCANROW,SCANROW,SNOROW,ALBSROW,
     +     RHOSROW,GROROW,ZSNLROW,ZPLSROW,ZPLGROW,FZRCROW,INDEPPARVAL
    
      REAL  WF_R2(NRC),THEXTRA,ICE_INDEX,DEGLAT,DEGLON,ZRFMGRD(1),
     +      ZRFHGRD(1),ZBLDGRD(1),GCGRD(1),VAR(NVAR),junk,
     +      PRECADJ,TEMPADJ,GWSCALE,PAR1,PAR2,
     +      SOIL_POR_MAX,SOIL_DEPTH,S0,T_ICE_LENS

	INTEGER I,J,IB,IB1,IB2,IB3,IE,IR,IW,INL,I4,I5,INS,LN,
     +        NLTEST,NMTEST,ILW,LAND,DAY1,DAY2,DAY3,DAY4,DAY5,DAY6,
     +        YEAR1,YEAR2,YEAR3,YEAR4,YEAR5,YEAR6,
     +        MINUTE,HOUR,DAY,YEAR,
     +        OPTFLAGS, INDEPPAR, DEPPAR, NOGRUS,COUNT,
     +        INVARD,INVARI,INLAND,INCOUNT

! ******************************************
!     READ MESH_parameters_CLASS.ini FILE          
! ******************************************
	open(unit=50,file='MESH_parameters_CLASS.ini',status='old')
	read(50,5010) TITLE
	read(50,5010) (NAME(I), I = 1,NNM)
	read(50,5010) (PLACE(I),I = 1,NPL)
	read(50,5020) DEGLAT,DEGLON,ZRFMGRD(1),ZRFHGRD(1),ZBLDGRD(1),
     +              GCGRD(1),ILW,NLTEST,NMTEST

      ALLOCATE (LABEL (NMTEST,19),TOTAL(NSL,NMTEST),PERCENT(NSL,NMTEST),
     +   FCANROW(NLCR,NMTEST),LAMXROW(NVGT,NMTEST),LNZ0ROW(NLCR,NMTEST),
     +   LAMNROW(NVGT,NMTEST),ALVCROW(NLCR,NMTEST),CMASROW(NVGT,NMTEST),
     +   ALICROW(NLCR,NMTEST),ROOTROW(NVGT,NMTEST),RSMNROW(NVGT,NMTEST),
     +   QA50ROW(NVGT,NMTEST),VPDAROW(NVGT,NMTEST),VPDBROW(NVGT,NMTEST),
     +   PSGAROW(NVGT,NMTEST),PSGBROW(NVGT,NMTEST),SANDROW(NSL,NMTEST),
     +   CLAYROW(NSL,NMTEST),ORGMROW(NSL,NMTEST),TBARROW(NSL,NMTEST),
     +   THLQROW(NSL,NMTEST),THICROW(NSL,NMTEST),DRNROW (NMTEST),
     +   SDEPROW(NMTEST),FAREROW(NMTEST),DDENROW(NMTEST),
     +   XSLPROW(NMTEST),GRKFROW(NMTEST),WFSFROW(NMTEST),
     +   WFCIROW(NMTEST),MIDROW (NMTEST),TCANROW(NMTEST),
     +   TSNOROW(NMTEST),TPNDROW(NMTEST),ZPNDROW(NMTEST),
     +   RCANROW(NMTEST),SCANROW(NMTEST),SNOROW (NMTEST),
     +   ALBSROW(NMTEST),RHOSROW(NMTEST),GROROW (NMTEST),
     +   ZSNLROW(NMTEST),ZPLSROW(NMTEST),ZPLGROW(NMTEST),
     +   MINLIMIT(NROW,NMTEST),MAXLIMIT(NROW,NMTEST),FZRCROW(NMTEST),
     +   VARORG(NROW,NMTEST),VARNEW(NROW,NMTEST),INOUT(NROW,NMTEST))
     
	DO LAND = 1,NMTEST

C-----------LINE 5-----------------------------------------------------
      LN = 5
	read(50,5040) (FCANROW(I,LAND), I = 1,NLCR),
     +              (LAMXROW(I,LAND), I = 1,NVGT),
     +               LABEL(LAND,LN)

C-----------LINE 6-----------------------------------------------------
      LN = 6
	read(50,5040) (LNZ0ROW(I,LAND), I = 1,NLCR),
     +              (LAMNROW(I,LAND), I = 1,NVGT), 
     +               LABEL(LAND,LN)

C-----------LINE 7-----------------------------------------------------
      LN = 7
	read(50,5040) (ALVCROW(I,LAND), I = 1,NLCR),
     +              (CMASROW(I,LAND), I = 1,NVGT),
     +               LABEL(LAND,LN)

C-----------LINE 8-----------------------------------------------------
      LN = 8
	read(50,5040) (ALICROW(I,LAND), I = 1,NLCR),
     +              (ROOTROW(I,LAND), I = 1,NVGT),
     +               LABEL(LAND,LN)

C-----------LINE 9-----------------------------------------------------
      LN = 9
      read(50,5030) (RSMNROW(I,LAND), I = 1,NVGT),
     +              (QA50ROW(I,LAND), I = 1,NVGT),
     +               LABEL(LAND,LN)

C-----------LINE 10----------------------------------------------------
      LN = 10
	read(50,5030) (VPDAROW(I,LAND), I = 1,NVGT),
     +              (VPDBROW(I,LAND), I = 1,NVGT),
     +               LABEL(LAND,LN)

C-----------LINE 11----------------------------------------------------
      LN = 11
	read(50,5030) (PSGAROW(I,LAND), I = 1,NVGT),
     +              (PSGBROW(I,LAND), I = 1,NVGT),
     +               LABEL(LAND,LN)

C-----------LINE 12----------------------------------------------------
      LN = 12
      read(50,5100)  DRNROW(LAND),SDEPROW(LAND),FAREROW(LAND),
     +               DDENROW(LAND),
     +               LABEL(LAND,LN)

C-----------LINE 13----------------------------------------------------
      LN = 13
      read(50,5090)  XSLPROW(LAND),GRKFROW(LAND),WFSFROW(LAND),
     +               WFCIROW(LAND),MIDROW(LAND),
     +               LABEL(LAND,LN)

C-----------LINE 14----------------------------------------------------
      LN = 14
	read(50,5080) (SANDROW(I,LAND),I=1,NSL),
     +               LABEL(LAND,LN)

C-----------LINE 15----------------------------------------------------
      LN = 15
	read(50,5080) (CLAYROW(I,LAND),I=1,NSL),
     +               LABEL(LAND,LN)

C-----------LINE 16----------------------------------------------------
      LN = 16
	read(50,5080) (ORGMROW(I,LAND),I=1,NSL),
     +               LABEL(LAND,LN)

C-----------LINE 17----------------------------------------------------
      LN = 17
	read(50,5050) (TBARROW(I,LAND),I=1,NSL),
     +               TCANROW(LAND),TSNOROW(LAND),TPNDROW(LAND),
     +               LABEL(LAND,LN)

C-----------LINE 18----------------------------------------------------
      LN = 18
	read(50,5060) (THLQROW(I,LAND),I=1,NSL),
     +              (THICROW(I,LAND),I=1,NSL),
     +               ZPNDROW(LAND),
     +               LABEL(LAND,LN)

C-----------LINE 19----------------------------------------------------
      LN = 19
	read(50,5070)  RCANROW(LAND),SCANROW(LAND),SNOROW(LAND),
     +               ALBSROW(LAND),RHOSROW(LAND),GROROW(LAND),
     +               LABEL(LAND,LN)
      enddo

C-----------LINE 20----------------------------------------------------
	read(50,"(6I10)")DAY1,DAY2,DAY3,DAY4,DAY5,DAY6

C-----------LINE 21----------------------------------------------------
	read(50,"(6I10)")YEAR1,YEAR2,YEAR3,YEAR4,YEAR5,YEAR6

C-----------LINE 22----------------------------------------------------
	read(50,"(4I10)")MINUTE,HOUR,DAY,YEAR

	CLOSE(UNIT=50)

C ******************************************
C     READ MESH_parameters_hydrology.ini FILE
C ******************************************
C      open(unit=23,file='../MESH_parameters_hydrology.ini',status='old')
      open(unit=23,file='MESH_parameters_hydrology.ini',status='old')
      read(23,2310) HEAD(1)
      read(23,2310) HEAD(2)
      read(23,2310) HEAD(3)
      READ(23,"(I5)") OPTFLAGS
      IF(OPTFLAGS.GT.0) THEN
        DO I=1,OPTFLAGS
           read(23,2310) HEAD(3+I)
        ENDDO
      ENDIF
      DO I=1,2
         read(23,2310) HEAD(3+OPTFLAGS+I)
      ENDDO
      READ (23,"(5F6.3)") (WF_R2(i),I=1,5)
      DO I=1,2
         read(23,2310) HEAD(5+OPTFLAGS+I)
      ENDDO
      READ(23,"(I8)") INDEPPAR
      ALLOCATE(INDEPPARVAL(INDEPPAR))
      DO I=1,INDEPPAR
         READ(23,*) INDEPPARVAL(I)
      ENDDO
      IF(INDEPPAR.GE.4) THEN
       SOIL_POR_MAX = INDEPPARVAL(1)
       SOIL_DEPTH   = INDEPPARVAL(2)
       S0           = INDEPPARVAL(3)
       T_ICE_LENS   = INDEPPARVAL(4)
      ENDIF
      DO I=1,2
         read(23,2310) HEAD(7+OPTFLAGS+INDEPPAR+I)
      ENDDO
      READ(23,"(I8)") NOGRUS
	IF(NOGRUS.NE.NMTEST) THEN
      PRINT *, 'Number of GRUs in hydrology file: ',NOGRUS
      PRINT *, 'Number of GRUs in drainage database: ',NMTEST
      PRINT *, 'Please adjust these values.'
      STOP
	ENDIF
      READ(23,"(I8)") DEPPAR
      read(23,2310) HEAD(10+OPTFLAGS+INDEPPAR)
      IF(DEPPAR.GT.0) THEN
      READ(23,"(10F10.2)") (ZSNLROW(I),I=1,NMTEST)
      READ(23,"(10F10.2)") (ZPLSROW(I),I=1,NMTEST)
      READ(23,"(10F10.2)") (ZPLGROW(I),I=1,NMTEST)
      ENDIF
      IF(DEPPAR >= 4)THEN
         READ(23,"(10F10.2)") (FZRCROW(I),I=1,NMTEST)
      ENDIF
	CLOSE(UNIT=23)

C ******************************************
C     READ VALUES FROM DDS_init.txt
C ******************************************
      open(unit=9, file='DDS_init.txt',status='old')

C-----------PASS OVER TOP 15 LINES--------------------------------------
      do i = 1, 15
	   read(9,*)
	enddo

C-----------READ NUMBER OF LAND CLASS INDEPENDENT VARIABLES-------------
      read(9,*) invari
      if(invari .ne. 12)then
        print*,'number of land class independent variables should be 12'
        pause
        stop
      endif

C-----------READ NUMBER OF LAND CLASS DEPENDENT VARIABLES---------------
      read(9,*) invard
      if(invard .ne. 76)then
        print*,'number of land class independent variables should be 76'
        pause
        stop
      endif

C-----------READ NUMBER OF LAND CLASSES---------------------------------
      read(9,*) inland

C-----------PASS OVER A COMMENT LINE------------------------------------
      read(9,*) 

C-----------CHECK IF NUMBER OF LAND CLASSES MATCHES UP------------------
      if(inland.ne.NMTEST) then
	   print *, 'Number of GRUs in CLASS.INI: ',NMTEST
	   print *, 'Number of GRUs in DDS_init.txt: ',inland
         print *, 'These numbers must match to run DDS'
	   pause
	   stop
	endif

C-----------READ MINIMUM LIMIT, MAXIMUM LIMIT AND OPTIMIZATION FLAG-----
	DO I=1,INVARI
         READ(9,*) MINLIMIT(I,1)
         READ(9,*) MAXLIMIT(I,1)
         READ(9,*) INOUT(I,1)
      ENDDO
      
      DO I=INVARI+1,INVARI+INVARD
         READ(9,*) (MINLIMIT(I,J),J=1,NMTEST)
         READ(9,*) (MAXLIMIT(I,J),J=1,NMTEST)
         READ(9,*) (INOUT(I,J),   J=1,NMTEST)
	ENDDO
	CLOSE(UNIT=9)

C-----------FIGURE OUT NUMBER OF VARIABLES BEING OPTIMIZED--------------
      INCOUNT=0
	DO I=1,INVARI+INVARD
         DO J=1,NMTEST
            IF(INOUT(I,J).EQ.1)INCOUNT=INCOUNT+1
	   ENDDO
	ENDDO

      IF(INCOUNT.EQ.0) THEN
         PRINT *, 'NO VARIABLES WERE CHOSEN TO BE OPTIMZED IN 
     +             DDS_INIT.TXT'
	   PRINT *, 'DDS PROGRAM CANNOT RUN.'
	   PAUSE
	   STOP
	ELSE
         call check_dds_bounds(inout,minlimit,maxlimit,nrow,nmtest,
     +                         invari,invard)
	ENDIF

C-----------CHECK TO MAKE SURE THAT BOTH THE SAND AND CLAY PARAMETERS 
C           ARE EITHER ON OR OFF. IN ITS PRESENT FORM OF THE DDS_INIT 
C           FILE THE BEGINNING OF SOIL FRACTIONS DATA CORRESPONDS TO 19.
      IB = 21 !19
      IE = IB + NSL * (3 - 1)
      DO I = IB, IE, NSL !rows 21 and 22; rows 24 and 25; rows 27 and 28
         DO J=1,NMTEST
	      IF(INOUT(I,J).EQ.1.OR.INOUT(I+1,J).EQ.1) THEN
	         IF(INOUT(I,J).EQ.1.AND.INOUT(I+1,J).EQ.1) THEN
                  CONTINUE
	         ELSE
       PRINT *, 'BOTH SAND AND CLAY VARIABLES MUST BE EITHER ON OR OFF.'
	 PRINT *, 'ROW',I,': SAND ',INOUT(I,J),' CLAY ',INOUT(I+1,J)
                  PAUSE
               ENDIF
            ENDIF
         ENDDO
      ENDDO
         
      DO I=1,NSL
         DO J=1,NMTEST
C        CHECK TO MAKE SURE WE WON'T GET A DIVIDE BY ZERO ERROR
            IF(ORGMROW(I,J).GE.100.0.OR.
     +        (ORGMROW(I,J)-SANDROW(I,J)).GE.100.0)THEN
               PRINT *, 'EITHER ORGANIC PERCENTAGES OR ORGANIC 
     +                   PLUS SAND ARE'
	         PRINT *, '100% IN ROW ',I,' PLEASE ADJUST'
               PAUSE
	      ENDIF
	   ENDDO
      ENDDO
      
      VARORG = -999
      IR = 1   
      VARORG(IR,1) = THEXTRA
      IR = IR + 1
      VARORG(IR,1) = ICE_INDEX
      IR = IR + 1
      VARORG(IR,1) = GWSCALE
      DO I = 1,NRC
         IR = IR + 1
         VARORG(IR,1) = WF_R2(I)
      ENDDO
      
      IF(INDEPPAR.GE.4) THEN
          IR = IR + 1
          VARORG(IR,1) = SOIL_POR_MAX          
          IR = IR + 1
          VARORG(IR,1) = SOIL_DEPTH
          IR = IR + 1
          VARORG(IR,1) = S0
          IR = IR + 1
          VARORG(IR,1) = T_ICE_LENS
      ELSE !PREVIOUS FORMAT WITHOUT FROZEN SOIL INFILTRATION PARAMETERS
          IR = IR + 1
          VARORG(IR,1) = PAR1          
          IR = IR + 1
          VARORG(IR,1) = PAR2       
      ENDIF
      
      DO J = 1, NMTEST
         VARORG(IR+1,J) = DRNROW (J)
         VARORG(IR+2,J) = SDEPROW(J)   
         VARORG(IR+3,J) = FAREROW(J)   
         VARORG(IR+4,J) = DDENROW(J)   
         VARORG(IR+5,J) = XSLPROW(J)   
         VARORG(IR+6,J) = GRKFROW(J)   
         VARORG(IR+7,J) = WFSFROW(J)   
         VARORG(IR+8,J) = WFCIROW(J)   
      ENDDO
      IB1 = IR + 8
      DO I = 1,NSL
         IR   = IB1 + (I-1)*3
         DO J = 1,NMTEST
c        adjust sand and clay
            TOTAL(I,J)   = SANDROW(I,j)/(100.0-ORGMROW(I,j))*100.0
            PERCENT(I,J) = CLAYROW(I,j)/(100.0-ORGMROW(I,j)
     +                       -SANDROW(I,j))*100.0
c           check that we didn't calculated any bad numbers during the soil calculation
            if(TOTAL(I,J).gt.100.0.or.PERCENT(I,J).gt.100.0) then
               print *, 'One of the soil parameters are greater than'
	         print *, '100% in row ',IR,' please adjust'
               pause
               stop
            endif
            VARORG(IR+1,J) = TOTAL(I,J)
            VARORG(IR+2,J) = PERCENT(I,J)
            VARORG(IR+3,J) = ORGMROW(I,j)
         ENDDO
      ENDDO
      IR = IR + NSL
      
      IR = IR + 1
      VARORG(IR,1:NMTEST) = ZSNLROW(1:NMTEST)   
      IR = IR + 1
      VARORG(IR,1:NMTEST) = ZPLSROW(1:NMTEST)   
      IR = IR + 1
      VARORG(IR,1:NMTEST) = ZPLGROW(1:NMTEST)   
      IF(DEPPAR >= 4)THEN
         IR = IR + 1
         VARORG(IR,1:NMTEST) = FZRCROW(1:NMTEST)   
      ENDIF
      
      IB2 = IR
      DO I5 = 1,5
         IR = IB2 + (I5-1)*6
         DO J = 1,NMTEST
            VARORG(IR+1,J)    = LNZ0ROW(I5,J)   
            VARORG(IR+2,J)    = ALVCROW(I5,J)   
            VARORG(IR+3,J)    = ALICROW(I5,J)
            IF(I5 .LT. 5)THEN   !URBAN AREAS
               VARORG(IR+4,J) = RSMNROW(I5,J)   
               VARORG(IR+5,J) = VPDAROW(I5,J)   
               VARORG(IR+6,J) = PSGAROW(I5,J)
            ENDIF
         ENDDO
      ENDDO
      IB3 = IR + 3 
      DO I4 = 1,4
         IR = IB3 + (I4-1)*7
         DO J = 1,NMTEST
            VARORG(IR+1,J) = LAMXROW(I4,J)   
            VARORG(IR+2,J) = LAMNROW(I4,J)   
            VARORG(IR+3,J) = CMASROW(I4,J)   
            VARORG(IR+4,J) = ROOTROW(I4,J)   
            VARORG(IR+5,J) = QA50ROW(I4,J)
            VARORG(IR+6,J) = VPDBROW(I4,J)
            VARORG(IR+7,J) = PSGBROW(I4,J)
         ENDDO
      ENDDO
      IR = IR + 7

C ******************************************
C     READ VALUES FROM variables_in.txt
C ******************************************
      open(unit=9, file='variables_in.txt',status='old')
      read(9,*) junk
      if(junk.lt.-990) then
         rewind 9

C        *********************************************************
C        Update variables from class.ini and watflow.ini files
C        *********************************************************
         print *, 'A value of -999.9 found in variables_in.txt file'
	   print *, 'Assume this is the start of the optimzation'

         CLOSE(9)
	ELSE
 
C        ***********************************************
C        Update variables from variables_in.txt file
C        ***********************************************
         rewind 9
         count = 1 
 100     read (9,*,end=200) var(count)
         count = count+1
	   goto 100
 200     close(9)

c check if the number of variables being optimized matches
         if(incount.ne.count-1) then
	      print *, 'Number of variables from DDS_ini file: ',incount
	      print *, 'Number of variables from variables_in file: ',count-1
           print *, 'The program cannot be run with this inconsistency.'
           pause
           STOP
	   endif
	   
         VARNEW = -999
         count  = 1
	   do i=1,invari
	      if(inout(i,1).eq.1) then
               VARNEW(I,1) = var(count)
	         count       = count+1
	      else
               VARNEW(I,1) = VARORG(i,1)
	      endif
	       
         ENDDO

         do i = invari+1,invari+invard
            DO J = 1, NMTEST
               if(inout(i,J).eq.1) then
                  VARNEW(I,J) = var(count)
	            count       = count+1
	         else
                  VARNEW(I,J) = VARORG(I,J)
	         endif
	      ENDDO
	   ENDDO

** Start reading new land class independent variables
         IW = 1
         THEXTRA   = VARNEW(IW,1)
         IW = IW + 1
         ICE_INDEX = VARNEW(IW,1)
         IW = IW + 1 
         GWSCALE   = VARNEW(IW,1)
         DO I = 1,5
            IW = IW + 1
            WF_R2(I) = VARNEW(IW,1)
         ENDDO
         IF(INDEPPAR.GE.4) THEN
            IW = IW + 1
            SOIL_POR_MAX = VARNEW(IW,1)          
            IW = IW + 1
            SOIL_DEPTH = VARNEW(IW,1) 
            IW = IW + 1
            S0 = VARNEW(IW,1)
            IW = IW + 1
            T_ICE_LENS = VARNEW(IW,1) 
        ELSE !PREVIOUS FORMAT WITHOUT FROZEN SOIL INFILTRATION PARAMETERS
            IW = IW + 1
            PAR1 = VARNEW(IW,1)         
            IW = IW + 1
            PAR2 = VARNEW(IW,1)       
        ENDIF

** Start reading new land class dependent variables
         DO J = 1,NMTEST
            DRNROW (J) = VARNEW(IW + 1,J)
            SDEPROW(J) = VARNEW(IW + 2,J)   
            FAREROW(J) = VARNEW(IW + 3,J)   
            DDENROW(J) = VARNEW(IW + 4,J)   
            XSLPROW(J) = VARNEW(IW + 5,J)   
            GRKFROW(J) = VARNEW(IW + 6,J)   
            WFSFROW(J) = VARNEW(IW + 7,J)   
            WFCIROW(J) = VARNEW(IW + 8,J)   
         ENDDO
         IB = 19
         DO I = 1,NSL
            IW = IB1 + (I-1)*3
            DO J = 1,NMTEST
               TOTAL  (I,J) = VARNEW(IW+1,J)
               PERCENT(I,J) = VARNEW(IW+2,J)
               ORGMROW(I,J) = VARNEW(IW+3,J)
               IF((100.0-ORGMROW(I,j)).eq.0.0) THEN
                   SANDROW(I,j)=0.0
               ELSE
                  SANDROW(I,j)=(TOTAL(I,j)/100.0)*
     +                           (100.0-ORGMROW(I,j))
               ENDIF
               IF((100.0-ORGMROW(I,j)-SANDROW(I,j)).eq.0.0) THEN
                  CLAYROW(I,j)=0.0
               ELSE
                  CLAYROW(I,j)=(PERCENT(I,j)/100.0)*
     +                           (100.0-ORGMROW(I,j)-SANDROW(I,j))
               ENDIF
            ENDDO
         ENDDO
         IW = IW + NSL
         DO J = 1,NMTEST
            ZSNLROW(J) = VARNEW(IW + 1,J)   
            ZPLSROW(J) = VARNEW(IW + 2,J)   
            ZPLGROW(J) = VARNEW(IW + 3,J)
         ENDDO
         IF(DEPPAR >= 4)THEN
            FZRCROW(1:NMTEST) = VARNEW(IW+4,1:NMTEST)  
         ENDIF
      
         DO I5 = 1,5
            IW = IB2 + (I5-1)*6
            DO J = 1,NMTEST
               LNZ0ROW(I5,J) = VARNEW(IW + 1,J)   
               ALVCROW(I5,J) = VARNEW(IW + 2,J)   
               ALICROW(I5,J) = VARNEW(IW + 3,J)
               IF(I5 .LT. 5)THEN   !URBAN AREAS
                  RSMNROW(I5,J) = VARNEW(IW + 4,J)   
                  VPDAROW(I5,J) = VARNEW(IW + 5,J)   
                  PSGAROW(I5,J) = VARNEW(IW + 6,J)
               ENDIF
            ENDDO
         ENDDO

         DO I4 = 1,4
            IW = IB3 + (I4-1)*7
            DO J = 1,NMTEST
               LAMXROW(I4,J) = VARNEW(IW + 1,J)   
               LAMNROW(I4,J) = VARNEW(IW + 2,J)   
               CMASROW(I4,J) = VARNEW(IW + 3,J)  
               ROOTROW(I4,J) = VARNEW(IW + 4,J)   
               QA50ROW(I4,J) = VARNEW(IW + 5,J)
               VPDBROW(I4,J) = VARNEW(IW + 6,J)
               PSGBROW(I4,J) = VARNEW(IW + 7,J) 
            ENDDO
         ENDDO
  	ENDIF

C ******************************************
C     rewrite class.ini file
C ******************************************
C	open (unit=50,file='../MESH_parameters_CLASS.INI')
	open (unit=50,file='MESH_parameters_CLASS.ini')
	write(50,5010) TITLE
	write(50,5010) (NAME(I),  I = 1,NNM)
	write(50,5010) (PLACE(I), I = 1,NPL)
	write(50,5020) DEGLAT,DEGLON,ZRFMGRD(1),ZRFHGRD(1),ZBLDGRD(1),
     +              GCGRD(1),ILW,NLTEST,NMTEST
	DO LAND=1,NMTEST
      LN = 5
	write(50,5005)(FCANROW(I,LAND), I = 1,NLCR),
     +              (LAMXROW(I,LAND), I = 1,NVGT),
     +               LABEL(LAND,LN)
      LN = 6
	write(50,5006)(LNZ0ROW(I,LAND), I = 1,NLCR),
     +              (LAMNROW(I,LAND), I = 1,NVGT), 
     +               LABEL(LAND,LN)
      LN = 7
	write(50,5007)(ALVCROW(I,LAND), I = 1,NLCR),
     +              (CMASROW(I,LAND), I = 1,NVGT),
     +               LABEL(LAND,LN)
      LN = 8
	write(50,5008)(ALICROW(I,LAND), I = 1,NLCR),
     +              (ROOTROW(I,LAND), I = 1,NVGT),
     +               LABEL(LAND,LN)
      LN = 9
      write(50,5009)(RSMNROW(I,LAND), I = 1,NVGT),
     +              (QA50ROW(I,LAND), I = 1,NVGT),
     +               LABEL(LAND,LN)
      LN = 10
	write(50,6010)(VPDAROW(I,LAND), I = 1,NVGT),
     +              (VPDBROW(I,LAND), I = 1,NVGT),
     +               LABEL(LAND,LN)
      LN = 11
	write(50,6011)(PSGAROW(I,LAND), I = 1,NVGT),
     +              (PSGBROW(I,LAND), I = 1,NVGT),
     +               LABEL(LAND,LN)
      LN = 12
      write(50,5112)DRNROW(LAND),SDEPROW(LAND),FAREROW(LAND),
     +               DDENROW(LAND),
     +               LABEL(LAND,LN)
      LN = 13
      write(50,5113)XSLPROW(LAND),GRKFROW(LAND),WFSFROW(LAND),
     +               WFCIROW(LAND),MIDROW(LAND),
     +               LABEL(LAND,LN)
      LN = 14
	write(50,5114)(SANDROW(I,LAND),I=1,NSL),
     +               LABEL(LAND,LN)
      LN = 15
	write(50,5115)(CLAYROW(I,LAND),I=1,NSL),
     +               LABEL(LAND,LN)
      LN = 16
	write(50,5116)(ORGMROW(I,LAND),I=1,NSL),
     +               LABEL(LAND,LN)
      LN = 17
	write(50,5017)(TBARROW(I,LAND),I=1,NSL),
     +               TCANROW(LAND),TSNOROW(LAND),TPNDROW(LAND),
     +               LABEL(LAND,LN)
      LN = 18
	write(50,6018)(THLQROW(I,LAND),I=1,NSL),
     +              (THICROW(I,LAND),I=1,NSL),
     +               ZPNDROW(LAND),
     +               LABEL(LAND,LN)
      LN = 19
	write(50,5019) RCANROW(LAND),SCANROW(LAND),SNOROW(LAND),
     +               ALBSROW(LAND),RHOSROW(LAND),GROROW(LAND),
     +               LABEL(LAND,LN)
      enddo

C-----------LINE 20----------------------------------------------------
	write(50,"(6I10,14X,'20')")DAY1,DAY2,DAY3,DAY4,DAY5,DAY6

C-----------LINE 21----------------------------------------------------
	write(50,"(6I10,14X,'21')")YEAR1,YEAR2,YEAR3,YEAR4,YEAR5,YEAR6

C-----------LINE 22----------------------------------------------------
	write(50,"(4I10,34X,'22')")MINUTE,HOUR,DAY,YEAR

C-----------ENDING LINE------------------------------------------------
	write(50,"(A83)")'123456789*123456789*123456789*123456789
     +*123456789*123456789*123456789*123   ver 30d'

	CLOSE(UNIT=50)

C ******************************************
Crewrite watflow.ini
C ******************************************

      open(unit=23,file='MESH_parameters_hydrology.ini')
      WRITE(23,2310) HEAD(1)
      WRITE(23,2310) HEAD(2)
      WRITE(23,2310) HEAD(3)
      WRITE(23,"(I5,' # Number of option flags')") OPTFLAGS
      IF(OPTFLAGS.GT.0) THEN
        DO I=1,OPTFLAGS
           WRITE(23,2310) HEAD(3+I)
        ENDDO
      ENDIF

      DO I=1,2
         WRITE(23,2310) HEAD(3+OPTFLAGS+I)
      ENDDO

      WRITE (23,"(5F6.3)") (WF_R2(i),I=1,5)

      DO I=1,2
         WRITE(23,2310) HEAD(5+OPTFLAGS+I)
      ENDDO

      WRITE(23,"(I8,
     +' # Number of GRU independent hydrologic parameters')") INDEPPAR

      IF(INDEPPAR.GE.4) THEN
         INDEPPARVAL(1) = SOIL_POR_MAX
         INDEPPARVAL(2) = SOIL_DEPTH
         INDEPPARVAL(3) = S0
         INDEPPARVAL(4) = T_ICE_LENS
      ENDIF      
      IF(INDEPPAR.GT.0) THEN
        DO I=1,INDEPPAR
           WRITE(23,"(F10.3)") INDEPPARVAL(I)
        ENDDO
      ENDIF
      
      DO I=1,2
         WRITE(23,2310) HEAD(7+OPTFLAGS+INDEPPAR+I)
      ENDDO

      WRITE(23,"(I8,' #Number of GRUs (must match number ',
     +'in mesh_parameters_class.ini file)')") NOGRUS
	IF(NOGRUS.NE.NMTEST) THEN
      PRINT *, 'Number of GRUs in hydrology file: ',NOGRUS
      PRINT *, 'Number of GRUs in drainage database: ',NMTEST
      PRINT *, 'Please adjust these values.'
      STOP
	ENDIF

      WRITE(23,"(I8,
     +' #Number of GRU dependent hydrologic parameters')") DEPPAR

      WRITE(23,2310) HEAD(10+OPTFLAGS+INDEPPAR)
      IF(DEPPAR.GT.0) THEN
      WRITE(23,"(100F10.2)") (ZSNLROW(I),I=1,NMTEST)
      WRITE(23,"(100F10.2)") (ZPLSROW(I),I=1,NMTEST)
      WRITE(23,"(100F10.2)") (ZPLGROW(I),I=1,NMTEST)
      ENDIF

      IF(DEPPAR.GE.4) THEN
      WRITE(23,"(100F10.2)") (FZRCROW(I),I=1,NMTEST)
      ENDIF

	close(unit=23)

2310  FORMAT(A85)
5010  FORMAT(2X,6A4)
5015  FORMAT(5(A10,3X))
5020  FORMAT(F10.2,4F10.2,F7.1,3I5)
5040  FORMAT(9F8.3,4X,A40)
5030  FORMAT(4F8.3,8X,4F8.3,4X,A40)
5100  FORMAT(4F8.3,44X,A40) !???????
5090  FORMAT(F8.4,F8.3,F8.3,F8.5,I8,36X,A40)
5080  FORMAT(3F10.1,46X,A40)
5050  FORMAT(6F10.2,16X,A40)
5060  FORMAT(7F10.3,6X,A40)
5070  FORMAT(2F10.4,F10.2,F10.3,F10.4,F10.3,16X,A40)
5005  FORMAT(9F8.3,2X,'05',A40)
5006  FORMAT(9F8.3,2X,'06',A40)
5007  FORMAT(9F8.3,2X,'07',A40)
5008  FORMAT(9F8.3,2X,'08',A40)
5009  FORMAT(4F8.3,8X,4F8.3,2X,'09',A40)
6010  FORMAT(4F8.3,8X,4F8.3,2X,'10',A40)
6011  FORMAT(4F8.3,8X,4F8.3,2X,'11',A40)
5017  FORMAT(6F10.2,14X,'17',A40)
6018  FORMAT(7F10.3,4X,'18',A40)
5112  FORMAT(3F8.3,F8.4,42X,'12',A40)
5113  FORMAT(F8.4,F8.3,F8.3,F8.5,I8,34X,'13',A40)
5114  FORMAT(3F10.2,44X,'14',A40)
5115  FORMAT(3F10.2,44X,'15',A40)
5116  FORMAT(3F10.2,44X,'16',A40)
5019  FORMAT(2F10.4,F10.2,F10.3,F10.3,F10.3,14X,'19',A40)
	stop
	end
	
