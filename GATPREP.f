      SUBROUTINE GATPREP(ILMOS,JLMOS,IWMOS,JWMOS,IWAT,IICE,
     1                   NML,NMW,NWAT,NICE,GCGRD,FAREA,MOSID,
     2                   NLAT,NMOS,ILG,IL1,IL2,IM)
C
C     * JUN 12/06 - E.CHAN.  DIMENSION IWAT AND IICE BY NLAT.
C     * NOV 03/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * AUG 09/02 - D.VERSEGHY/M.LAZARE. DETERMINE INDICES FOR
C     *                        GATHER-SCATTER OPERATIONS ON
C     *                        CURRENT LATITUDE LOOP.
C     
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER   NML,NMW,NWAT,NICE,NLAT,NMOS,ILG,IL1,IL2,IM,I,J
C
C     * OUTPUT FIELDS.
C
      INTEGER  ILMOS  (ILG),  JLMOS  (ILG),  IWMOS  (ILG),
     1         JWMOS  (ILG)  
C
      INTEGER  IWAT  (NLAT),  IICE  (NLAT)
C
C     * INPUT FIELDS.
C 
      REAL     GCGRD  (NLAT), FAREA (NLAT,NMOS)
C
      INTEGER  MOSID  (NLAT,NMOS)
     1         
C
C---------------------------------------------------------------------
      NML=0
      NMW=0
      NWAT=0
      NICE=0

      DO 200 I=IL1,IL2
          IF(GCGRD(I).LE.-0.5)                               THEN
              DO 100 J=1,IM
                  IF(FAREA(I,J).GT.0.0)              THEN
                      IF(MOSID(I,J).GT.0)    THEN
                          NML=NML+1
                          ILMOS(NML)=I
                          JLMOS(NML)=J
                      ELSE
                          NMW=NMW+1
                          IWMOS(NMW)=I
                          JWMOS(NMW)=J
                      ENDIF
                  ENDIF
  100         CONTINUE
          ELSEIF(GCGRD(I).GT.0.5)                            THEN
              NICE=NICE+1
              IICE(NICE)=I
          ELSE
              NWAT=NWAT+1
              IWAT(NWAT)=I
          ENDIF
  200 CONTINUE
C
      RETURN
      END
