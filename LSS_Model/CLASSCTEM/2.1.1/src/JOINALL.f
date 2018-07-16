      PROGRAM JOINALL
C     
C     Writes daily averages from monthly files to full time series file
C
      IMPLICIT NONE
C
      INTEGER NCOUNT,NIN,NOUT
      INTEGER IBUF(8),IDAT(10088)
      INTEGER NC4TO8,INTEFLT,ME32O64
      INTEGER MAXX,MLON,MLAT,IDUMMY,MACHINE,INTSIZE,NTEST
      REAL F(10088)
      LOGICAL OK
C
      REAL  DLATGRD(10088),  DLONGRD(10088),  SNOACC(10088).   
     1      SNOMAX(10088),   SNOAVG(10088)
C
      REAL
     1     TBARACC1(10088), THLQACC1(10088), THICACC1(10088),
     2     TBARACC2(10088), THLQACC2(10088), THICACC2(10088),
     3     TBARACC3(10088), THLQACC3(10088), THICACC3(10088)
C
      COMMON/ICOM/IBUF,IDAT
      COMMON/MACHTYP/MACHINE,INTSIZE
C
      MAXX=10088
      MLON=97
      MLAT=104
      NIN=6201
      NOUT=6300
C
      OPEN(UNIT=6201,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199106.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=6202,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199107.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=6203,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199108.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=6204,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199109.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=6205,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199110.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=6206,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199111.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=6207,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199112.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=6208,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199201.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=6209,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199202.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=6210,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199203.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=6211,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199204.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=6212,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199205.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=6213,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199206.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=6214,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199207.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=6215,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199306.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=6276,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199709.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=6277,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199710.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=6278,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199711.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=6279,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199712.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=6280,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199801.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=6281,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199802.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=6282,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199803.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=6283,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199804.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=6284,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199805.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=6285,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199806.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=6286,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_199807.of2.38lev.ccrn',FORM='UNFORMATTED',STATUS='OLD')
C
      OPEN(UNIT=6300,FILE='/data12/acrnrdv/class_ipy_output/CLASS_IPY_Qu
     1ebec_1tile_allyears.of2.38lev.ccrn',FORM='UNFORMATTED')
      OPEN(UNIT=66,FILE='run_report')
C
      DO 100 I=1,NLAT
          SNOMAX(I)=0.0
          SNOAVG(I)=0.0
100   CONTINUE
C
C     * LAUNCH RUN.

      NCOUNT=1
      NMNTH=1
      CALL GETFLD2(NIN,DLATGRD,-1,-1,-1,-1,IBUF,MAXX,OK)
      CALL GETFLD2(NIN,DLONGRD,-1,-1,-1,-1,IBUF,MAXX,OK)
      CALL STORVAR(NOUT,DLATGRD,0,' LAT',1,MLON,MLAT,MAXX)
      CALL STORVAR(NOUT,DLONGRD,0,' LON',1,MLON,MLAT,MAXX)

200   CONTINUE
C
      CALL GETFLD2(NIN,TBARACC1,-1,-1,-1,-1,IBUF,MAXX,OK)
      IF(.NOT.OK)THEN
          WRITE(66,6010) NSTEPS
          WRITE(66,6011) NMNTH
          NMNTH=NMNTH+1
          IF(NMNTH.EQ.87) THEN
              WRITE(66,6699) 
6699          FORMAT(2X,'end run')
              CALL                        XIT('RUNCLASS',0)
          ENDIF
          NSTEPS=0
          NIN=NIN+1
          CALL GETFLD2(NIN,DLATGRD,-1,-1,-1,-1,IBUF,MAXX,OK)
          CALL GETFLD2(NIN,DLONGRD,-1,-1,-1,-1,IBUF,MAXX,OK)
          CALL GETFLD2(NIN,TBARACC1,-1,-1,-1,-1,IBUF,MAXX,OK)
      ENDIF

      CALL GETFLD2(NIN,THLQACC1,-1,-1,-1,-1,IBUF,MAXX,OK)
      CALL GETFLD2(NIN,THICACC1,-1,-1,-1,-1,IBUF,MAXX,OK)
      CALL GETFLD2(NIN,TBARACC2,-1,-1,-1,-1,IBUF,MAXX,OK)
      CALL GETFLD2(NIN,THLQACC2,-1,-1,-1,-1,IBUF,MAXX,OK)
      CALL GETFLD2(NIN,THICACC2,-1,-1,-1,-1,IBUF,MAXX,OK)
      CALL GETFLD2(NIN,TBARACC3,-1,-1,-1,-1,IBUF,MAXX,OK)
      CALL GETFLD2(NIN,THLQACC3,-1,-1,-1,-1,IBUF,MAXX,OK)
      CALL GETFLD2(NIN,THICACC3,-1,-1,-1,-1,IBUF,MAXX,OK)
      CALL GETFLD2(NIN,SNOACC,-1,-1,-1,-1,IBUF,MAXX,OK)
      NCOUNT=NCOUNT+1
 
      CALL STORVAR(NOUT,TBARACC1,IBUF(2),'TBAR',1,MLON,MLAT,MAXX)
      CALL STORVAR(NOUT,THLQACC1,IBUF(2),'THLQ',1,MLON,MLAT,MAXX)
      CALL STORVAR(NOUT,THICACC1,IBUF(2),'THIC',1,MLON,MLAT,MAXX)
      CALL STORVAR(NOUT,TBARACC2,IBUF(2),'TBAR',2,MLON,MLAT,MAXX)
      CALL STORVAR(NOUT,THLQACC2,IBUF(2),'THLQ',2,MLON,MLAT,MAXX)
      CALL STORVAR(NOUT,THICACC2,IBUF(2),'THIC',2,MLON,MLAT,MAXX)
      CALL STORVAR(NOUT,TBARACC3,IBUF(2),'TBAR',3,MLON,MLAT,MAXX)
      CALL STORVAR(NOUT,THLQACC3,IBUF(2),'THLQ',3,MLON,MLAT,MAXX)
      CALL STORVAR(NOUT,THICACC3,IBUF(2),'THIC',3,MLON,MLAT,MAXX)
      CALL STORVAR(NOUT,SNOACC,IBUF(2),' SNO',1,MLON,MLAT,MAXX)

      DO 300 I=1,NLAT
          SNOAVG(I)=SNOAVG(I)+SNOACC(I)
          SNOMAX(I)=MAX(SNOMAX(I),SNOACC(I))
300   CONTINUE
  
      IF(IBUF(2).EQ.38016) THEN
          DO 310 I=1,NLAT
              SNOAVG(I)=SNOAVG(I)/NCOUNT
310       CONTINUE
          CALL STORVAR(NOUT,SNOMAX,1,'SNMX',1,MLON,MLAT,MAXX)
          CALL STORVAR(NOUT,SNOAVG,1,'SNAV',1,MLON,MLAT,MAXX)
          DO 320 I=1,NLAT
              SNOAVG(I)=0.0
              SNOMAX(I)=0.0
320       CONTINUE
          NCOUNT=1
      ENDIF

      IF(IBUF(2).EQ.73056) THEN
          DO 330 I=1,NLAT
              SNOAVG(I)=SNOAVG(I)/NCOUNT
330       CONTINUE
          CALL STORVAR(NOUT,SNOMAX,2,'SNMX',1,MLON,MLAT,MAXX)
          CALL STORVAR(NOUT,SNOAVG,2,'SNAV',1,MLON,MLAT,MAXX)
          DO 340 I=1,NLAT
              SNOAVG(I)=0.0
              SNOMAX(I)=0.0
340       CONTINUE
          NCOUNT=1
      ENDIF
      
      IF(IBUF(2).EQ.108096) THEN
          DO 350 I=1,NLAT
              SNOAVG(I)=SNOAVG(I)/NCOUNT
350       CONTINUE
          CALL STORVAR(NOUT,SNOMAX,3,'SNMX',1,MLON,MLAT,MAXX)
          CALL STORVAR(NOUT,SNOAVG,3,'SNAV',1,MLON,MLAT,MAXX)
          DO 360 I=1,NLAT
              SNOAVG(I)=0.0
              SNOMAX(I)=0.0
360       CONTINUE
          NCOUNT=1
      ENDIF
 
      IF(IBUF(2).EQ.143136) THEN
          DO 370 I=1,NLAT
              SNOAVG(I)=SNOAVG(I)/NCOUNT
370       CONTINUE
          CALL STORVAR(NOUT,SNOMAX,4,'SNMX',1,MLON,MLAT,MAXX)
          CALL STORVAR(NOUT,SNOAVG,4,'SNAV',1,MLON,MLAT,MAXX)
          DO 380 I=1,NLAT
              SNOAVG(I)=0.0
              SNOMAX(I)=0.0
380       CONTINUE
          NCOUNT=1
      ENDIF
      
      IF(IBUF(2).EQ.178272) THEN
          DO 390 I=1,NLAT
              SNOAVG(I)=SNOAVG(I)/NCOUNT
390       CONTINUE
          CALL STORVAR(NOUT,SNOMAX,5,'SNMX',1,MLON,MLAT,MAXX)
          CALL STORVAR(NOUT,SNOAVG,5,'SNAV',1,MLON,MLAT,MAXX)
          DO 400 I=1,NLAT
              SNOAVG(I)=0.0
              SNOMAX(I)=0.0
400       CONTINUE
          NCOUNT=1
      ENDIF

      IF(IBUF(2).EQ.213312) THEN
          DO 410 I=1,NLAT
              SNOAVG(I)=SNOAVG(I)/NCOUNT
410       CONTINUE
          CALL STORVAR(NOUT,SNOMAX,6,'SNMX',1,MLON,MLAT,MAXX)
          CALL STORVAR(NOUT,SNOAVG,6,'SNAV',1,MLON,MLAT,MAXX)
          DO 420 I=1,NLAT
              SNOAVG(I)=0.0
              SNOMAX(I)=0.0
420       CONTINUE
          NCOUNT=1
      ENDIF
      
      IF(IBUF(2).EQ.248352) THEN
          DO 430 I=1,NLAT
              SNOAVG(I)=SNOAVG(I)/NCOUNT
430       CONTINUE
          CALL STORVAR(NOUT,SNOMAX,7,'SNMX',1,MLON,MLAT,MAXX)
          CALL STORVAR(NOUT,SNOAVG,7,'SNAV',1,MLON,MLAT,MAXX)
          DO 440 I=1,NLAT
              SNOAVG(I)=0.0
              SNOMAX(I)=0.0
440       CONTINUE
          NCOUNT=1
      ENDIF

      GO TO 200

      CALL EXIT
      END
