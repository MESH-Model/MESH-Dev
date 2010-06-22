SUBROUTINE READ_FORCING_DATA(IMIN,VMIN,YCOUNT,XCOUNT,NTYPE,NA,NML,ILG,JLMOS,YYY,XXX,ENDDATA, &
                             FSDOWN, FSVHGRD, FSIHGRD, FDLGRD, PREGRD, &
                             TAGRD, ULGRD, VLGRD, UVGRD, PRESGRD, QAGRD)

!> *********************************************************************
!> Read in Meteorological forcing data
!> *********************************************************************

!> READ IN METEOROLOGICAL FORCING DATA FOR CURRENT TIME STEP;
!> CALCULATE SOLAR ZENITH ANGLE AND COMPONENTS OF INCOMING SHORT-
!> WAVE RADIATION FLUX; ESTIMATE FLUX PARTITIONS IF NECESSARY.
!> READ IN METEOROLOGICAL FORCING DATA FOR CURRENT TIME STEP;
!> CALCULATE SOLAR ZENITH ANGLE AND COMPONENTS OF INCOMING SHORT-
!> WAVE RADIATION FLUX; ESTIMATE FLUX PARTITIONS IF NECESSARY.

USE FLAGS

IMPLICIT NONE

!>
!>*******************************************************************
!>
!> MET. FORCING DATA (FORCING.BIN):
!> THESE HAVE TO BE REAL*4 IN ORDER TO READ IN THE MET DATA
!> CORRECTLY.
!* R4SHRTGRID2D: VISIBLE SHORTWAVE RADIATION [W m-2]
!* R4LONGGRID2D: DOWNWELLING LONGWAVE RADIATION [W m-2]
!* R4RAINGRID2D: PRECIPITATION [kg m-2 s-1]
!* R4TEMPGRID2D: AMBIENT AIR TEMPERATURE [dC]
!* R4WINDGRID2D: WIND SPEED AT REFERENCE HEIGHT [m s-1]
!* R4PRESGRID2D: AIR PRESSURE AT SURFACE [Pa]
!* R4HUMDGRID2D: SPECIFIC HUMIDITY AT REFERENCE HEIGHT [kg kg-1]

INTEGER IMIN,YCOUNT,XCOUNT,NTYPE,NA,NML,ILG,ENDDATA
REAL    VMIN

REAL*4, DIMENSION(YCOUNT, XCOUNT) :: R4SHRTGRID2D, R4LONGGRID2D, R4RAINGRID2D, R4TEMPGRID2D, &
                                     R4WINDGRID2D, R4PRESGRID2D, R4HUMDGRID2D

REAL*4, DIMENSION(NTYPE)          :: R4SHRTGRU, R4LONGGRU, R4RAINGRU, R4TEMPGRU, R4WINDGRU, &
                                     R4PRESGRU, R4HUMDGRU

REAL*4, DIMENSION(NA)             :: FSDOWN, FSVHGRD, FSIHGRD, FDLGRD, PREGRD, TAGRD, ULGRD,&
                                     VLGRD, UVGRD, PRESGRD, QAGRD

INTEGER*4,DIMENSION(NA)           :: YYY,XXX

INTEGER*4,DIMENSION(ILG)           :: JLMOS

INTEGER I,J,CURGRU

IF(IMIN==0 .OR. HOURLYFLAG == 30)THEN

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
      FSVHGRD(I)=0.5*R4SHRTGRID2D(YYY(I),XXX(I))
      FSIHGRD(I)=FSVHGRD(I)
    ENDDO

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
      FSVHGRD(I)=0.5*R4SHRTGRID2D(YYY(I),XXX(I))
      FSIHGRD(I)=FSVHGRD(I)
    ENDDO

!> *********************************************************************
!> basin_shortwave.csv
!> *********************************************************************
  ELSEIF (BASINSHORTWAVEFLAG == 2) THEN
    READ (90, *, END=999) (R4SHRTGRU(I),I=1,NTYPE)
    DO I=1,NML
      CURGRU    = JLMOS(I)
      FSDOWN(I) = R4SHRTGRU(CURGRU)
      FSVHGRD(I)= 0.5*R4SHRTGRU(CURGRU)
      FSIHGRD(I)= FSVHGRD(I)
    ENDDO
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
    READ(51,END=999) ((R4LONGGRID2D(I,J),J=1,XCOUNT),I=1,YCOUNT)
    DO I=1,NA
      FDLGRD(I)=R4LONGGRID2D(YYY(I),XXX(I))
    ENDDO

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

!> *********************************************************************
!> basin_longwave.csv
!> *********************************************************************
  ELSEIF (BASINLONGWAVEFLAG == 2) THEN
    READ (91, *, END=999) (R4LONGGRU(I),I=1,NTYPE)
    DO I=1,NML
      CURGRU    = JLMOS(I)
      FDLGRD(I) = R4LONGGRU(CURGRU)
    ENDDO
  ELSE
    PRINT*,'BASINLONGWAVEFLAG SHOULD BE EITHER 0, 1 0R 2'
    PAUSE
    STOP
  ENDIF

!> *********************************************************************
!> Read rain data
!> *********************************************************************

!> *********************************************************************
!> basin_rain.bin
!> *********************************************************************
  IF(BASINRAINFLAG==0)THEN
    READ(51,END=999) ((R4RAINGRID2D(I,J),J=1,XCOUNT),I=1,YCOUNT)
    DO I=1,NA
	   PREGRD(I)=R4RAINGRID2D(YYY(I),XXX(I))
    ENDDO	   

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

!> *********************************************************************
!> basin_rain.csv
!> *********************************************************************
  ELSEIF (BASINRAINFLAG == 2) THEN
    READ (92, *, END=999) (R4RAINGRU(I),I=1,NTYPE)
    DO I=1,NML
      CURGRU    = JLMOS(I)
      PREGRD(I) = R4RAINGRU(CURGRU)
    ENDDO
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
    READ(51,END=999) ((R4TEMPGRID2D(I,J),J=1,XCOUNT),I=1,YCOUNT)
    DO I=1,NA
      TAGRD(I)=R4TEMPGRID2D(YYY(I),XXX(I))
    ENDDO

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

!> *********************************************************************
!>  basin_temperature.csv
!> *********************************************************************
  ELSEIF (BASINTEMPERATUREFLAG == 2) THEN
    READ (93, *, END=999) (R4TEMPGRU(I),I=1,NTYPE)
    DO I=1,NML
      CURGRU    = JLMOS(I)
      TAGRD(I)  = R4TEMPGRU(CURGRU)
    ENDDO
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
    READ(51,END=999) ((R4WINDGRID2D(I,J),J=1,XCOUNT),I=1,YCOUNT)
    DO I=1,NA
      ULGRD(I)=R4WINDGRID2D(YYY(I),XXX(I))
      VLGRD(I)=0.0
      UVGRD(I)=MAX(VMIN,ULGRD(I))
    ENDDO

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
      VLGRD(I)=0.0
      UVGRD(I)=MAX(VMIN,ULGRD(I))
    ENDDO
 
!> *********************************************************************
!> basin_wind.csv
!> *********************************************************************
  ELSEIF (BASINWINDFLAG == 2) THEN
    READ (94, *, END=999) (R4WINDGRU(I),I=1,NTYPE)
    DO I=1,NML
      CURGRU    = JLMOS(I)
      ULGRD(I)  = R4WINDGRU(CURGRU)
      VLGRD(I)  = 0.0
      UVGRD(I)  = MAX(VMIN,ULGRD(I))
    ENDDO
  ELSE
    PRINT*,'BASINWINDFLAG SHOULD BE EITHER 0, 1 0R 2'
    PAUSE
    STOP
  ENDIF

!> *********************************************************************
!> read pressure data
!> *********************************************************************

!> *********************************************************************
!> basin_pres.r2c
!> *********************************************************************
  IF(BASINPRESFLAG==0)THEN
    READ(51,END=999) ((R4PRESGRID2D(I,J),J=1,XCOUNT),I=1,YCOUNT)
    DO I=1,NA
      PRESGRD(I)=R4PRESGRID2D(YYY(I),XXX(I))
    ENDDO

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
      PRESGRD(I)=R4PRESGRID2D(YYY(I),XXX(I))
    ENDDO

!> *********************************************************************
!> basin_pres.csv
!> *********************************************************************
  ELSEIF (BASINPRESFLAG == 2) THEN
    READ (95, *, END=999) (R4PRESGRU(I),I=1,NTYPE)
    DO I=1,NML
      CURGRU    = JLMOS(I)
      PRESGRD(I) = R4PRESGRU(CURGRU)
    ENDDO
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
    READ(51,END=999) ((R4HUMDGRID2D(I,J),J=1,XCOUNT),I=1,YCOUNT)
    DO I=1,NA
      QAGRD(I)=R4HUMDGRID2D(YYY(I),XXX(I))
    ENDDO

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

!> *********************************************************************
!> basin_humidity.csv
!> *********************************************************************
  ELSEIF (BASINHUMIDITYFLAG == 2) THEN
    READ (96, *, END=999) (R4HUMDGRU(I),I=1,NTYPE)
    DO I=1,NML
      CURGRU    = JLMOS(I)
      QAGRD(I) = R4HUMDGRU(CURGRU)
    ENDDO
  ELSE
    PRINT*,'BASINHUMIDITYFLAG SHOULD BE EITHER 0, 1 0R 2'
    PAUSE
    STOP
  ENDIF
ENDIF
ENDDATA = 0

RETURN

999 ENDDATA = 1

END