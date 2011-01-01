SUBROUTINE READ_FORCING_DATA(YCOUNT,XCOUNT,NTYPE,NA,NML,ILG,ILMOS,JLMOS,YYY,XXX,ENDDATA,FAREA, &
                             FSDOWN,FSVHGRD,FSIHGRD,FDLGRD,PREGRD,TAGRD,ULGRD,PRESGRD,QAGRD, &
                             FSVHGAT, FSIHGAT, FDLGAT, PREGAT, TAGAT, ULGAT, PRESGAT, QAGAT)

!> *********************************************************************
!> MAM - Read in Meteorological forcing data
!> *********************************************************************
!>
!>*******************************************************************
!* R4SHRTGRID2D: VISIBLE SHORTWAVE RADIATION [W m-2]
!* R4LONGGRID2D: DOWNWELLING LONGWAVE RADIATION [W m-2]
!* R4RAINGRID2D: PRECIPITATION [kg m-2 s-1]
!* R4TEMPGRID2D: AMBIENT AIR TEMPERATURE [dC]
!* R4WINDGRID2D: WIND SPEED AT REFERENCE HEIGHT [m s-1]
!* R4PRESGRID2D: AIR PRESSURE AT SURFACE [Pa]
!* R4HUMDGRID2D: SPECIFIC HUMIDITY AT REFERENCE HEIGHT [kg kg-1]
!> THESE HAVE TO BE REAL*4 IN ORDER TO READ IN THE MET DATA
!> CORRECTLY.
!>*******************************************************************

USE FLAGS

IMPLICIT NONE

INTEGER YCOUNT,XCOUNT,NTYPE,NA,NML,ILG
LOGICAL ENDDATA

REAL*4, DIMENSION(YCOUNT, XCOUNT) :: R4SHRTGRID2D, R4LONGGRID2D, R4RAINGRID2D, R4TEMPGRID2D, &
                                     R4WINDGRID2D, R4PRESGRID2D, R4HUMDGRID2D
REAL*4, DIMENSION(NA,NTYPE)       :: FAREA
REAL*4, DIMENSION(NTYPE)          :: R4SHRTGRU, R4LONGGRU, R4RAINGRU, R4TEMPGRU, R4WINDGRU, &
                                     R4PRESGRU, R4HUMDGRU
REAL*4, DIMENSION(NA)             :: FSDOWN, FSVHGRD, FSIHGRD, FDLGRD, PREGRD, TAGRD, ULGRD, &
                                     PRESGRD, QAGRD
REAL*4, DIMENSION(ILG)            :: FSVHGAT, FSIHGAT, FDLGAT, PREGAT, TAGAT, ULGAT, & 
                                     PRESGAT, QAGAT
REAL*4                            :: JUNK
INTEGER*4,DIMENSION(NA)           :: YYY,XXX
INTEGER*4,DIMENSION(ILG)          :: ILMOS,JLMOS
INTEGER                           :: I,J,K,CURGRU,ICOUNT

!Initialize counting number of r2c and csv files
ICOUNT = 0

!> *********************************************************************
!> Read shortwave radiation data
!> *********************************************************************

!> *********************************************************************
!> basin_shortwave.bin
!> *********************************************************************
  IF(BASINSHORTWAVEFLAG==0)THEN
    READ(51,END=999) ((R4SHRTGRID2D(I,J),J=1,XCOUNT),I=1,YCOUNT)
    DO I=1,NA
      FSDOWN(I)=R4SHRTGRID2D(YYY(I),XXX(I))
    ENDDO
    FSVHGRD=0.5*FSDOWN
    FSIHGRD=FSVHGRD
    CALL GATHER(NA,NML,ILG,ILMOS,FSVHGRD,FSVHGAT)
    FSIHGAT=FSVHGAT
   
!> *********************************************************************
!> basin_shortwave.r2c
!> *********************************************************************
  ELSEIF (BASINSHORTWAVEFLAG == 1) THEN
    READ (90, *, END=999) !:Frame line
    DO I=1,YCOUNT
      READ (90, *, END=999) (R4SHRTGRID2D(I,J),J=1,XCOUNT)
    END DO
    READ (90, *, END=999) !:EndFrame line
    DO I=1,NA
      FSDOWN(I)=R4SHRTGRID2D(YYY(I),XXX(I))
    ENDDO
    FSVHGRD=0.5*FSDOWN
    FSIHGRD=FSVHGRD
    CALL GATHER(NA,NML,ILG,ILMOS,FSVHGRD,FSVHGAT)
    FSIHGAT=FSVHGAT
    ICOUNT=ICOUNT+1
!> *********************************************************************
!> basin_shortwave.csv
!> *********************************************************************
  ELSEIF (BASINSHORTWAVEFLAG == 2) THEN
    READ (90, *, END=999) (R4SHRTGRU(I),I=1,NTYPE)
    DO I=1,NML
      CURGRU     = JLMOS(I)
      FSVHGAT(I) = 0.5*R4SHRTGRU(CURGRU)
    ENDDO
    FSIHGAT=FSVHGAT
    CALL SCATTER(NTYPE,NA,FAREA,FSVHGRD,R4SHRTGRU)
    FSDOWN=FSVHGRD
    FSIHGRD=FSVHGRD
    ICOUNT=ICOUNT+1
    IF(TESTCSVFLAG==1)then
        print*,"Shortwave radiation"
        CALL TEST_CSV(NTYPE,NML,NA,ILMOS,R4SHRTGRU,FSVHGAT+FSIHGAT)
    ENDIF
  ELSE
    PRINT*,'BASINSHORTWAVEFLAG SHOULD BE EITHER 0, 1 0R 2'
    PAUSE
    STOP
  ENDIF

!> *********************************************************************
!> Read longwave radiation data
!> *********************************************************************

!> *********************************************************************
!> basin_longwave.bin
!> *********************************************************************
  IF(BASINLONGWAVEFLAG==0)THEN
    !Skip the forcing data that is read from r2c and csv files
    DO K = 1,ICOUNT
       READ(51,END=999) ((JUNK,J=1,XCOUNT),I=1,YCOUNT)
    ENDDO
    READ(51,END=999) ((R4LONGGRID2D(I,J),J=1,XCOUNT),I=1,YCOUNT)
    DO I=1,NA
      FDLGRD(I)=R4LONGGRID2D(YYY(I),XXX(I))
    ENDDO
    CALL GATHER(NA,NML,ILG,ILMOS,FDLGRD,FDLGAT)

!> *********************************************************************
!> basin_longwave.r2c
!> *********************************************************************
  ELSEIF (BASINLONGWAVEFLAG == 1) THEN
    READ (91, *, END=999) !:Frame line
    DO I=1,YCOUNT
      READ (91, *, END=999) (R4LONGGRID2D(I,J),J=1,XCOUNT)
    END DO
    READ (91, *, END=999) !:EndFrame line
    DO I=1,NA
      FDLGRD(I)=R4LONGGRID2D(YYY(I),XXX(I))
    ENDDO
    CALL GATHER(NA,NML,ILG,ILMOS,FDLGRD,FDLGAT)
    ICOUNT=ICOUNT+1
!> *********************************************************************
!> basin_longwave.csv
!> *********************************************************************
  ELSEIF (BASINLONGWAVEFLAG == 2) THEN
    READ (91, *, END=999) (R4LONGGRU(I),I=1,NTYPE)
    DO I=1,NML
      CURGRU    = JLMOS(I)
      FDLGAT(I) = R4LONGGRU(CURGRU)
    ENDDO
    CALL SCATTER(NTYPE,NA,FAREA,FDLGRD,R4LONGGRU)
    ICOUNT=ICOUNT+1
    IF(TESTCSVFLAG==1)then
        print*,"Longwave radiation"
        CALL TEST_CSV(NTYPE,NML,NA,ILMOS,R4LONGGRU,FDLGAT)
    ENDIF
  ELSE
    PRINT*,'BASINLONGWAVEFLAG SHOULD BE EITHER 0, 1 0R 2'
    PAUSE
    STOP
  ENDIF

!> *********************************************************************
!> Read precipitation data
!> *********************************************************************

!> *********************************************************************
!> basin_rain.bin
!> *********************************************************************
  IF(BASINRAINFLAG==0)THEN
    !Skip the forcing data that is read from r2c and csv files
    DO K = 1,ICOUNT
       READ(51,END=999) ((JUNK,J=1,XCOUNT),I=1,YCOUNT)
    ENDDO
    READ(51,END=999) ((R4RAINGRID2D(I,J),J=1,XCOUNT),I=1,YCOUNT)
    DO I=1,NA
	   PREGRD(I)=R4RAINGRID2D(YYY(I),XXX(I))
    ENDDO	   
    CALL GATHER(NA,NML,ILG,ILMOS,PREGRD,PREGAT)

!> *********************************************************************
!> basin_rain.r2c
!> *********************************************************************
  ELSEIF (BASINRAINFLAG == 1) THEN
    READ (92, *, END=999) !:Frame line
    DO I=1,YCOUNT
      READ (92, *, END=999) (R4RAINGRID2D(I,J),J=1,XCOUNT)
    END DO
    READ (92, *, END=999) !:EndFrame line
    DO I=1,NA
      PREGRD(I)=R4RAINGRID2D(YYY(I),XXX(I))
    ENDDO
    CALL GATHER(NA,NML,ILG,ILMOS,PREGRD,PREGAT)
    ICOUNT=ICOUNT+1
!> *********************************************************************
!> basin_rain.csv
!> *********************************************************************
  ELSEIF (BASINRAINFLAG == 2) THEN
    READ (92, *, END=999) (R4RAINGRU(I),I=1,NTYPE)
    DO I=1,NML
      CURGRU    = JLMOS(I)
      PREGAT(I) = R4RAINGRU(CURGRU)
    ENDDO
    CALL SCATTER(NTYPE,NA,FAREA,PREGRD,R4RAINGRU)
    ICOUNT=ICOUNT+1
    IF(TESTCSVFLAG==1)then
        print*,"Precipitation"
        CALL TEST_CSV(NTYPE,NML,NA,ILMOS,R4RAINGRU,PREGAT)
    ENDIF
  ELSE
    PRINT*,'BASINRAINFLAG SHOULD BE EITHER 0, 1 0R 2'
    PAUSE
    STOP
  ENDIF

!> *********************************************************************
!> Read temperature data
!> *********************************************************************

!> *********************************************************************
!> basin_temperature.bin
!> *********************************************************************
  IF(BASINTEMPERATUREFLAG==0)THEN
    !Skip the forcing data that is read from r2c and csv files
    DO K = 1,ICOUNT
       READ(51,END=999) ((JUNK,J=1,XCOUNT),I=1,YCOUNT)
    ENDDO
    READ(51,END=999) ((R4TEMPGRID2D(I,J),J=1,XCOUNT),I=1,YCOUNT)
    DO I=1,NA
      TAGRD(I)=R4TEMPGRID2D(YYY(I),XXX(I))
    ENDDO
    CALL GATHER(NA,NML,ILG,ILMOS,TAGRD,TAGAT)

!> *********************************************************************
!> basin_temperature.r2c
!> *********************************************************************
  ELSEIF (BASINTEMPERATUREFLAG == 1) THEN
    READ (93, *, END=999) !:Frame line
    DO I = 1,YCOUNT
      READ (93, *, END=999) (R4TEMPGRID2D(I,J),J=1,XCOUNT)
    END DO
    READ (93, *, END=999) !:EndFrame line
    DO I=1,NA
      TAGRD(I)=R4TEMPGRID2D(YYY(I),XXX(I))
    ENDDO
    CALL GATHER(NA,NML,ILG,ILMOS,TAGRD,TAGAT)
    ICOUNT=ICOUNT+1
!> *********************************************************************
!>  basin_temperature.csv
!> *********************************************************************
  ELSEIF (BASINTEMPERATUREFLAG == 2) THEN
    READ (93, *, END=999) (R4TEMPGRU(I),I=1,NTYPE)
    DO I=1,NML
      CURGRU    = JLMOS(I)
      TAGAT(I)  = R4TEMPGRU(CURGRU)
    ENDDO
    CALL SCATTER(NTYPE,NA,FAREA,TAGRD,R4TEMPGRU)
    ICOUNT=ICOUNT+1
    IF(TESTCSVFLAG==1)then
        print*,"Temperature"
        CALL TEST_CSV(NTYPE,NML,NA,ILMOS,R4TEMPGRU,TAGAT)
    ENDIF
  ELSE
    PRINT*,'BASINTEMPERATUREFLAG SHOULD BE EITHER 0, 1 0R 2'
    PAUSE
    STOP
  ENDIF

!> *********************************************************************
!> Read wind data
!> *********************************************************************

!> *********************************************************************
!> basin_wind.bin
!> *********************************************************************
  IF(BASINWINDFLAG==0)THEN !use the forcing bin
    !Skip the forcing data that is read from r2c and csv files
    DO K = 1,ICOUNT
       READ(51,END=999) ((JUNK,J=1,XCOUNT),I=1,YCOUNT)
    ENDDO
    READ(51,END=999) ((R4WINDGRID2D(I,J),J=1,XCOUNT),I=1,YCOUNT)
    DO I=1,NA
      ULGRD(I)=R4WINDGRID2D(YYY(I),XXX(I))
    ENDDO
    !VLGRD=0.0
    !VLGAT=0.0
    !UVGRD=MAX(VMIN,ULGRD)
    CALL GATHER(NA,NML,ILG,ILMOS,ULGRD,ULGAT)

!> *********************************************************************
!> basin_wind.r2c
!> *********************************************************************
  ELSEIF (BASINWINDFLAG == 1) THEN
    READ (94, *, END=999) !:Frame line
    DO I = 1,YCOUNT
      READ (94, *, END=999) (R4WINDGRID2D(I,J),J=1,XCOUNT)
    END DO
    READ (94, *, END=999) !:EndFrame line
    DO I=1,NA
      ULGRD(I)=R4WINDGRID2D(YYY(I),XXX(I))
    ENDDO
    !VLGRD=0.0
    !VLGAT=0.0
    !UVGRD=MAX(VMIN,ULGRD)
    CALL GATHER(NA,NML,ILG,ILMOS,ULGRD,ULGAT)
    ICOUNT=ICOUNT+1
!> *********************************************************************
!> basin_wind.csv
!> *********************************************************************
  ELSEIF (BASINWINDFLAG == 2) THEN
    READ (94, *, END=999) (R4WINDGRU(I),I=1,NTYPE)
    DO I=1,NML
      CURGRU    = JLMOS(I)
      ULGAT(I)  = R4WINDGRU(CURGRU)
    ENDDO
    !VLGRD=0.0
    !VLGAT=0.0
    CALL SCATTER(NTYPE,NA,FAREA,ULGRD,R4WINDGRU)
    ICOUNT=ICOUNT+1
    IF(TESTCSVFLAG==1)then
        print*,"Wind"
        CALL TEST_CSV(NTYPE,NML,NA,ILMOS,R4WINDGRU,ULGAT)
    ENDIF
  ELSE
    PRINT*,'BASINWINDFLAG SHOULD BE EITHER 0, 1 0R 2'
    PAUSE
    STOP
  ENDIF

!> *********************************************************************
!> read pressure data
!> *********************************************************************

!> *********************************************************************
!> basin_pres.bin
!> *********************************************************************
  IF(BASINPRESFLAG==0)THEN
    !Skip the forcing data that is read from r2c and csv files
    DO K = 1,ICOUNT
       READ(51,END=999) ((JUNK,J=1,XCOUNT),I=1,YCOUNT)
    ENDDO
    READ(51,END=999) ((R4PRESGRID2D(I,J),J=1,XCOUNT),I=1,YCOUNT)
    DO I=1,NA
      PRESGRD(I)=R4PRESGRID2D(YYY(I),XXX(I))
    ENDDO
    CALL GATHER(NA,NML,ILG,ILMOS,PRESGRD,PRESGAT)

!> *********************************************************************
!> basin_pres.r2c
!> *********************************************************************
  ELSEIF (BASINPRESFLAG == 1) THEN
    READ (95, *, END=999) !:Frame line
    DO I =1,YCOUNT
      READ (95, *, END=999) (R4PRESGRID2D(I,J),J=1,XCOUNT)
    END DO
    READ (95, *, END=999) !:EndFrame line
    DO I=1,NA
      PRESGRD(I)=R4PRESGRID2D(YYY(I),XXX(I))*100.0
    ENDDO
    CALL GATHER(NA,NML,ILG,ILMOS,PRESGRD,PRESGAT)
    ICOUNT=ICOUNT+1
!> *********************************************************************
!> basin_pres.csv
!> *********************************************************************
  ELSEIF (BASINPRESFLAG == 2) THEN
    READ (95, *, END=999) (R4PRESGRU(I),I=1,NTYPE)
    DO I=1,NML
      CURGRU    = JLMOS(I)
      PRESGAT(I) = R4PRESGRU(CURGRU)
    ENDDO
    CALL SCATTER(NTYPE,NA,FAREA,PRESGRD,R4PRESGRU)
    ICOUNT=ICOUNT+1
    IF(TESTCSVFLAG==1)then
        print*,"Pressure"
        CALL TEST_CSV(NTYPE,NML,NA,ILMOS,R4PRESGRU,PRESGAT)
    ENDIF
  ELSE
    PRINT*,'BASINPRESSUREFLAG SHOULD BE EITHER 0, 1 0R 2'
    PAUSE
    STOP
  ENDIF

!> *********************************************************************
!> read humidity data
!> *********************************************************************

!> *********************************************************************
!> basin_humidity.bin
!> *********************************************************************
  IF(BASINHUMIDITYFLAG==0)THEN
    !Skip the forcing data that is read from r2c and csv files
    DO K = 1,ICOUNT
       READ(51,END=999) ((JUNK,J=1,XCOUNT),I=1,YCOUNT)
    ENDDO
    READ(51,END=999) ((R4HUMDGRID2D(I,J),J=1,XCOUNT),I=1,YCOUNT)
    DO I=1,NA
      QAGRD(I)=R4HUMDGRID2D(YYY(I),XXX(I))
    ENDDO
    CALL GATHER(NA,NML,ILG,ILMOS,QAGRD,QAGAT)

!> *********************************************************************
!> basin_humidity.r2c
!> *********************************************************************
  ELSEIF (BASINHUMIDITYFLAG == 1) THEN
    READ (96, *, END=999) !:Frame line
    DO I = 1,YCOUNT
      READ (96, *, END=999) (R4HUMDGRID2D(I,J),J=1,XCOUNT)
    END DO
    READ (96, *, END=999) !:EndFrame line
    DO I=1,NA
      QAGRD(I)=R4HUMDGRID2D(YYY(I),XXX(I))
    ENDDO
    CALL GATHER(NA,NML,ILG,ILMOS,QAGRD,QAGAT)
    ICOUNT=ICOUNT+1
!> *********************************************************************
!> basin_humidity.csv
!> *********************************************************************
  ELSEIF (BASINHUMIDITYFLAG == 2) THEN
    READ (96, *, END=999) (R4HUMDGRU(I),I=1,NTYPE)
    DO I=1,NML
      CURGRU    = JLMOS(I)
      QAGAT(I)  = R4HUMDGRU(CURGRU)
    ENDDO
    CALL SCATTER(NTYPE,NA,FAREA,QAGRD,R4HUMDGRU)
    ICOUNT=ICOUNT+1
    IF(TESTCSVFLAG==1)then
        print*,"Humidity"
        CALL TEST_CSV(NTYPE,NML,NA,ILMOS,R4HUMDGRU,QAGAT)
    ENDIF
  ELSE
    PRINT*,'BASINHUMIDITYFLAG SHOULD BE EITHER 0, 1 0R 2'
    PAUSE
    STOP
  ENDIF

RETURN

999 ENDDATA = .TRUE.

END
