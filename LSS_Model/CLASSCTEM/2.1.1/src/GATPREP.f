!>\file
!!Purpose: Assign values to pointer vectors relating the location of elements on the "gathered" variable
!!vectors to elements on the original two-dimensional arrays (latitude circle x mosaic tiles) for land grid
!!cells.
!!

      SUBROUTINE GATPREP(ILMOS,JLMOS,IWMOS,JWMOS,
     1                   NML,NMW,GCROW,FAREA,MOSID,
     2                   NL,NM,ILG,IL1,IL2,IM)
C
C     * DEC 28/11 - D.VERSEGHY. CHANGE ILGM BACK TO ILG AND
C     *                         ILG TO NL FOR CONSISTENCY WITH
C     *                         BOTH STAND-ALONE AND GCM
C     *                         CONVENTIONS.
C     * OCT 22/11 - M.LAZARE. REMOVE OCEAN/ICE CODE (NOW DONE
C     *                       IN COISS).
C     * OCT 21/11 - M.LAZARE. COSMETIC: ILG->ILGM AND NLAT->ILG,
C     *                       TO BE CONSISTENT WITH MODEL
C     *                       CONVENTION. ALSO GCGRD->GCROW.
C     * JUN 12/06 - E.CHAN.  DIMENSION IWAT AND IICE BY ILG.
C     * NOV 03/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * AUG 09/02 - D.VERSEGHY/M.LAZARE. DETERMINE INDICES FOR
C     *                        GATHER-SCATTER OPERATIONS ON
C     *                        CURRENT LATITUDE LOOP.
C     
      IMPLICIT NONE

C
C     * INTEGER CONSTANTS.
C
      INTEGER  NML  !<Total number of mosaic tiles in land surface gather vectors
      INTEGER  NMW  !<Total number of mosaic tiles in inland water gather vectors
      INTEGER  NL   !<Hard-coded maximum number of grid cells
      INTEGER  NM   !<Hard-coded maximum number of mosaic tiles
      INTEGER  ILG  !<Hard-coded maximum number of elements in the gathered vectors
      INTEGER  IL1  !<
      INTEGER  IL2  !<
      INTEGER  IM   !<Maximum number of mosaic tiles within the grid cells in the array under consideration
      INTEGER  I    !<
      INTEGER  J    !<
C
C     * OUTPUT FIELDS.
C
      INTEGER  ILMOS  (ILG) !<Index of grid cell corresponding to current element
                            !<of gathered vector of land surface variables [ ]
      INTEGER  JLMOS  (ILG) !<Index of mosaic tile corresponding to current element
                            !<of gathered vector of land surface variables [ ]
      INTEGER  IWMOS  (ILG) !<Index of grid cell corresponding to current element of gathered vector
                            !<of inland water body variables [ ]
      INTEGER  JWMOS  (ILG) !<Index of mosaic tile corresponding to current element of gathered vector
                            !<of inland water body variables [ ]
C
C     * INPUT FIELDS.
C 
      REAL     GCROW (NL)    !<Real number identifier indicating whether the grid cell
                             !<is land (-1.0), sea ice (+1.0), or ocean (0.0)
      REAL     FAREA (NL,NM) !<Fractional coverage of mosaic tile on grid cell [ ]
C
      INTEGER  MOSID (NL,NM) !<Mosaic tile type identifier (1 for land, 0 for inland water)
C---------------------------------------------------------------------
      NML=0
      NMW=0
!>
!!A looping operation is performed over the latitude circle, or array of grid cells, under consideration. If
!!the grid cell is a land one (GCROW = -1.0), an additional internal loop is performed over all the mosaic
!!tiles present. For each mosaic tile, if its fractional coverage is greater than zero, then if the mosaic type
!!identifier MOSID is equal to 1 (indicating land), the counter of total mosaic tiles in the land surface gather
!!vectors, NML, is incremented by one, and the elements of the vectors ILMOS and JLMOS corresponding
!!to NML are set to the indices of the current grid cell and mosaic tile respectively. If MOSID is equal to
!!zero (indicating inland water), the counter of total mosaic tiles in the inland water gather vectors, NMW,
!!is incremented by one, and the elements of the vectors IWMOS and JWMOS corresponding to NMW are
!!set to the indices of the current grid cell and mosaic tile respectively.
!!


      DO 200 I=IL1,IL2
	
          IF(GCROW(I).LE.-0.5)                               THEN
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
          ENDIF
  200 CONTINUE
C	

      RETURN
      END
