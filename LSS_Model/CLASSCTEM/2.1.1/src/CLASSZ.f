!>\file
C!Purpose: Check for energy and water balance closure over modelled 
C!area. 
!!
      SUBROUTINE CLASSZ(ISTEP,  CTVSTP, CTSSTP,
     1                  CTNSTP, WTVSTP, WTSSTP, WTGSTP,
     2                  FSGV,   FLGV,   HFSC,   HEVC,   HMFC,   HTCC,
     3                  FSGS,   FLGS,   HFSS,   HEVS,   HMFN,   HTCS,
     4                  FSGG,   FLGG,   HFSG,   HEVG,   HMFG,   HTC,
     5                  PCFC,   PCLC,   QFCF,   QFCL,   ROFC,   WTRC,
     6                  PCPN,   QFN,    ROFN,   WTRS,   PCPG,   QFG,
     7                  QFC,    ROF,    WTRG,   CMAI,   RCAN,   SCAN,   
     8                  TCAN,   SNO,    WSNOW,  TSNOW,  THLIQ,  THICE,  
     9                  HCPS,   THPOR,  DELZW,  TBAR,   ZPOND,  TPOND,  
     A                  DELZ,   FCS,    FGS,    FC,     FG,
     B                  IL1,    IL2,    ILG,    IG,     N    )

C     * May 29/19 - S.Sauer     Changed the calculation of internal energy variables
C                               Instead of 3 variables for each soil layer, there
C                               is now a 2D variable using the variable IG (# of layers)
C                               CLASSZ calls are much simpler now, should fix later
C     * SEP 04/12 - J.MELTON    REMOVED 'STOP', THEY ARE DEPRECATED, 
C                               REPLACE WITH CALL EXIT
C     * JAN 06/09 - D.VERSEGHY. MORE VARIABLES IN PRINT STATEMENTS;
C     *                         SLIGHTLY INCREASED ACCURACY LIMITS.
C     * NOV 10/06 - D.VERSEGHY. CHECK THAT SUMS OF ENERGY AND WATER
C     *                         FLUXES FOR CANOPY, SNOW AND SOIL MATCH
C     *                         CHANGES IN HEAT AND WATER STORAGE OVER
C     *                         CURRENT TIMESTEP.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER  ISTEP    !<Flag indicating position at beginning or end of 
                        !<time step
      INTEGER  IL1,IL2,ILG,IG,N,I,J
C
C     * DIAGNOSTIC ARRAYS USED FOR CHECKING ENERGY AND WATER 
C     * BALANCES.
C
      REAL CTVSTP(ILG)  !<Change in internal energy of vegetation over 
                        !<current time step \f$[W m^{-2}] \f$
      REAL CTSSTP(ILG)  !<Change in internal energy of snow pack over 
                        !<current time step \f$[W m^{-2}] \f$                    
      REAL CTNSTP(ILG,IG)! Change in internal energy of layer n
                        !<over current time step \f$[W m^{-2}] \f$
      REAL WTVSTP(ILG)  !<Change in vegetation mass over current time step \f$[kg m^{-2}]\f$
      REAL WTSSTP(ILG)  !<Change in snow mass over current time step \f$[kg m^{-2}]\f$
      REAL WTGSTP(ILG)  !<Change in soil water storage over current time step \f$[kg m^{-2}]\f$
C
      REAL QSUMV,QSUMS,QSUM1,QSUM2,QSUM3,WSUMV,WSUMS,WSUMG
C
C     * INPUT ARRAYS.
C
      REAL QSUMN (IG)       !Sum of, one entry for every soil layer

      REAL FSGV  (ILG)      !<Diagnosed net shortwave radiation on 
                            !<vegetation canopy \f$[W m^{-2}] (K_{*,c})\f$
      REAL FLGV  (ILG)      !<Diagnosed net longwave radiation on 
                            !<vegetation canopy \f$[W m^{-2}] (L_{*,c})\f$
      REAL HFSC  (ILG)      !<Diagnosed sensible heat flux on vegetation 
                            !<canopy \f$[W m^{-2}] (Q_{H,c})\f$
      REAL HEVC  (ILG)      !<Diagnosed latent heat flux on vegetation 
                            !<canopy \f$[W m^{-2}] (Q_{E,c})\f$
      REAL HMFC  (ILG)      !<Diagnosed energy associated with phase 
                            !<change of water on vegetation \f$[W m^{-2}] (Q_{M,c})\f$
      REAL HTCC  (ILG)      !<Diagnosed internal energy change of 
                            !<vegetation canopy due to conduction and/or 
                            !<change in mass \f$[W m^{-2}] (Q_{I,c})\f$
      REAL FSGS  (ILG)      !<Diagnosed net shortwave radiation at snow 
                            !<surface \f$[W m^{-2}] (K_{*,s})\f$
      REAL FLGS  (ILG)      !<Diagnosed net longwave radiation at snow 
                            !<surface \f$[W m^{-2}] (L_{*,s})\f$
      REAL HFSS  (ILG)      !<Diagnosed sensible heat flux at snow 
                            !<surface \f$[W m^{-2}] (Q_{H,s})\f$
      REAL HEVS  (ILG)      !<Diagnosed latent heat flux at snow surface 
                            !<\f$[W m^{-2}] (Q_{E,s})\f$
      REAL HMFN  (ILG)      !<Diagnosed energy associated with phase 
                            !<change of water in snow pack \f$[W m^{-2}] (Q_{M,s})\f$
      REAL HTCS  (ILG)      !<Diagnosed internal energy change of snow 
                            !<pack due to conduction and/or change in 
                            !<mass \f$[W m^{-2}] (Q_{I,s})\f$
      REAL FSGG  (ILG)      !<Diagnosed net shortwave radiation at soil 
                            !<surface \f$[W m^{-2}] (K_{*,g})\f$
      REAL FLGG  (ILG)      !<Diagnosed net longwave radiation at soil 
                            !<surface \f$[W m^{-2}] (L_{*,g})\f$
      REAL HFSG  (ILG)      !<Diagnosed sensible heat flux at soil 
                            !<surface \f$[W m^{-2}] (Q_{H,g})\f$
      REAL HEVG  (ILG)      !<Diagnosed latent heat flux at soil surface 
                            !<\f$[W m^{-2}] (Q_{E,g})\f$
      REAL HMFG  (ILG,IG)   !<Diagnosed energy associated with phase 
                            !<change of water in soil layers \f$[W m^{-2}] (Q_{M,g})\f$
      REAL HTC   (ILG,IG)   !<Diagnosed internal energy change of soil 
                            !<layer due to conduction and/or change in 
                            !<mass \f$[W m^{-2}] (Q_{I,g})\f$
      REAL PCFC  (ILG)      !<Diagnosed frozen precipitation intercepted 
                            !<by vegetation \f$[kg m^{-2} s^{-1}] (P_{f,c})\f$
      REAL PCLC  (ILG)      !<Diagnosed liquid precipitation intercepted 
                            !<by vegetation \f$[kg m^{-2} s^{-1}] (P_{l,c})\f$
      REAL QFCF  (ILG)      !<Diagnosed vapour flux from frozen water on 
                            !<vegetation \f$[kg m^{-2} s^{-1}] (E_{f,c})\f$
      REAL QFCL  (ILG)      !<Diagnosed vapour flux from liquid water on 
                            !<vegetation \f$[kg m^{-2} s^{-1}] (E_{l,c})\f$
      REAL ROFC  (ILG)      !<Liquid/frozen water runoff from vegetation 
                            !<\f$[kg m^{-2} s^{-1}] (R_c)\f$
      REAL WTRC  (ILG)      !<Diagnosed water transferred off the 
                            !<vegetation canopy \f$[kg m^{-2} s^{-1}] (A_c)\f$
      REAL PCPN  (ILG)      !<Diagnosed precipitation incident on snow 
                            !<pack \f$[kg m^{-2} s^{-1}] (P_s)\f$
      REAL QFN   (ILG)      !<Diagnosed water vapour flux from snow pack 
                            !<\f$[kg m^{-2} s^{-1}] (E_s)\f$
      REAL ROFN  (ILG)      !<Liquid water runoff from snow pack 
                            !<\f$[kg m^{-2} s^{-1}] (R_s)\f$
      REAL WTRS  (ILG)      !<Diagnosed water transferred into or out of 
                            !<the snow pack \f$[kg m^{-2} s^{-1}] (A_s)\f$
      REAL PCPG  (ILG)      !<Diagnosed precipitation incident on ground 
                            !<\f$[kg m^{-2} s^{-1}] (P_g)\f$
      REAL QFG   (ILG)      !<Diagnosed water vapour flux from ground 
                            !<surface \f$[kg m^{-2} s^{-1}] (E_g)\f$
      REAL QFC   (ILG,IG)   !<Diagnosed vapour flux from transpiration 
                            !<over modelled area \f$[W m^{-2}] (E_c)\f$
      REAL ROF   (ILG)      !<Total runoff from soil \f$[kg m^{-2} s^{-1}] (R_g)\f$
      REAL WTRG  (ILG)      !<Diagnosed water transferred into or out of 
                            !<the soil \f$[kg m^{-2} s^{-1}] (A_g)\f$
      REAL CMAI  (ILG)      !<Current mass of vegetation canopy \f$[kg m^{-2}] (W_c)\f$
      REAL RCAN  (ILG)      !<Intercepted liquid water stored on canopy 
                            !<\f$[kg m^{-2}] (W_{l,c})\f$
      REAL SCAN  (ILG)      !<Intercepted frozen water stored on canopy 
                            !<\f$[kg m^{-2}] (W_{f,c})\f$
      REAL TCAN  (ILG)      !<Vegetation canopy temperature \f$[K] (T_c)\f$
      REAL SNO   (ILG)      !<Mass of snow pack \f$[kg m^{-2}] (W_s)\f$
      REAL WSNOW (ILG)      !<Liquid water content of snow pack \f$[kg m^{-2}] (W_{l,s})\f$
      REAL TSNOW (ILG)      !<Snowpack temperature \f$[K] (T_s)\f$
      REAL THLIQ (ILG,IG)   !<Volumetric liquid water content of soil 
                            !<layers \f$[m^3 m^{-3}] (\theta_l)\f$
      REAL THICE (ILG,IG)   !<Volumetric frozen water content of soil 
                            !<layers \f$[m^3 m^{-3}] (\theta_f)\f$
      REAL HCPS  (ILG,IG)   !<Volumetric heat capacity of soil particles 
                            !<\f$[J m^{-3}] (C_g)\f$
      REAL THPOR (ILG,IG)   !<Pore volume in soil layer \f$[m^3 m^{-3}]\f$
      REAL DELZW (ILG,IG)   !<Permeable thickness of soil layer \f$[m] (\Delta_zw)\f$
      REAL DELZ  (IG)       !<Total thickness of soil layer \f$[m] (\Delta_z)\f$
      REAL TBAR  (ILG,IG)   !<Temperature of soil layers \f$[K] (T_g)\f$
      REAL ZPOND (ILG)      !<Depth of ponded water on surface \f$[m] (z_p)\f$
      REAL TPOND (ILG)      !<Total thickness of soil layer \f$[m] (\Delta_z)\f$
      REAL FCS   (ILG)      !<Fractional coverage of vegetation over snow on modelled area [ ]
      REAL FGS   (ILG)      !<Fractional coverage of snow over bare ground on modelled area [ ]
      REAL FC    (ILG)      !<Fractional coverage of vegetation over bare ground on modelled area [ ]
      REAL FG    (ILG)      !<Fractional coverage of bare ground on modelled area [ ]
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT     !<Time step [s]
      REAL TFREZ    !<Freezing point of water [K]
      REAL HCPW     !<Volumetric heat capacity of water \f$(4.187 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPICE   !<Volumetric heat capacity of ice \f$(1.9257 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPSOL   !<Volumetric heat capacity of mineral matter 
                    !<\f$(2.25 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPOM    !<Volumetric heat capacity of organic matter 
                    !<\f$(2.50 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPSND   !<Volumetric heat capacity of sand particles 
                    !<\f$(2.13 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPCLY   !<Volumetric heat capacity of fine mineral particles 
                    !<\f$(2.38 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL SPHW     !<Specific heat of water \f$(4.186 * 10^3) [J kg^{-1} K^{-1}]\f$
      REAL SPHICE   !<Specific heat of ice \f$(2.10 * 10^3) [J kg^{-1} K^{-1}]\f$
      REAL SPHVEG   !<Specific heat of vegetation matter \f$(2.70 * 10^3) [J kg^{-1} K^{-1}]\f$
      REAL SPHAIR   !<Specific heat of air \f$[J kg^{-1} K^{-1}]\f$
      REAL RHOW     !<Density of water \f$(1.0 * 10^3) [kg m^{-3}]\f$
      REAL RHOICE   !<Density of ice \f$(0.917 * 10^3) [kg m^{-3}]\f$
      REAL TCGLAC   !<Thermal conductivity of ice sheets \f$(2.24) [W m^{-1} K^{-1}]\f$
      REAL CLHMLT   !<Latent heat of freezing of water \f$(0.334 * 10^6) [J kg^{-1}]\f$
      REAL CLHVAP   !<Latent heat of vaporization of water \f$(2.501 * 10^6) [J kg^{-1}]\f$
C
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
C
C =================================================================
C
      !>
      !!In this subroutine, checks are carried out to ensure that the 
      !!change in energy storage in each of the components of the 
      !!modelled area (canopy, snow and soil) is equal to the sum of the 
      !!energy fluxes into and out of them; and that the change in 
      !!moisture storage in each of the components is equal to the sum of 
      !!the water fluxes into and out of them. The subroutine is called 
      !!twice, once at the beginning (ISTEP=0) and once at the end 
      !!(ISTEP=1) of each time step. At the beginning, the instantaneous 
      !!energy and moisture storage terms are evaluated, and at the end 
      !!the differences over the time step are calculated:
      !!
      !!Change in canopy energy storage = 
      !!     \f$\Delta [(c_c W_c + c_w W_{l,c} + c_i W_{f,c} )T_c ] / \Delta t\f$
      !!
      !!Change in snow energy storage = 
      !!     \f$\Delta [(C_i W_s /\rho_i + C_w W_{l,s} / \rho_w )T_s ]/ \Delta t \f$
      !!
      !!Change in soil layer energy storage =
      !!     \f$\Delta {[(C_w \theta_l + C_i \theta_f + C_g \theta_g ) \Delta z_w + C_b (\Delta z – \Delta z_w )]T_j }/ \Delta t\f$
      !!(For the first soil layer, the numerator contains the additional 
      !!term \f$C_w z_p T_p\f$.)
      !!
      !!Change in canopy moisture storage = \f$\Delta[W_{l,c} + W_{f,c}]\f$
      !!
      !!Change in snow moisture storage = \f$\Delta[W_s + W_{l,s}]\f$
      !!
      !!Change in soil moisture storage = 
      !!     \f$\Delta [(\theta_l \rho_w + \theta_f \rho_i ) \Delta z_w + z_p \rho_w ]\f$
      !!
      !!The net energy and moisture fluxes are also evaluated at the end 
      !!of the time step:
      !!
      !!Net energy flux for canopy = 
      !!     \f$K_{*,c} + L_{*,c} – Q_{H,c} – Q_{E,c} – Q_{M,c} + Q_{I,c}\f$
      !!
      !!Net energy flux for snow = 
      !!     \f$K_{*,s} + L_{*,s} – Q_{H,s} – Q_{E,s} – Q_{M,s} + Q_{I,s}\f$
      !!
      !!Net energy flux for first soil layer = 
      !!     \f$K_{*,g} + L_{*,g} – Q_{H,g} – Q_{E,g} – Q_{M,1} + Q_{I,1}\f$
      !!
      !!Net energy flux for other soil layers = \f$- Q_{M,j} + Q_{I,j}\f$
      !!
      !!Net moisture flux for canopy = 
      !!     \f$P_{l,c} + P_{f,c} – E_{l,c} – E_{f,c} – R_c + A_c\f$
      !!
      !!Net moisture flux for snow = \f$P_s – E_s – R_s + A_s\f$
      !!
      !!Net moisture flux for soil = \f$P_g – E_g – R_g + A_g - E_c\f$
      !!
      !!In these equations the \f$K_*\f$ terms refer to net shortwave radiation, 
      !!the \f$L_*\f$ terms to net longwave radiation, the \f$Q_H\f$ terms to sensible 
      !!heat flux, the \f$Q_E\f$ terms to latent heat flux, the \f$Q_M\f$ terms to heat 
      !!associated with melting or freezing of water, and the \f$Q_I\f$ terms to 
      !!changes in heat storage caused by conduction or redistribution of 
      !!water. The P terms refer to precipitation, the E terms to 
      !!evaporation, the R terms to runoff and the A terms to water 
      !!transferred between different components of the landscape. 
      !!The subscript 1 refers to the first soil layer, and j to a 
      !!generalized other layer.
      !!
      !!Finally, each change in energy or moisture storage is compared in 
      !!turn with the corresponding net flux of energy or moisture, and 
      !!if the difference is greater than a selected threshold value, an 
      !!error message is printed out and the run is stopped.
      !!


      IF(ISTEP.EQ.0) THEN
C
C     * SET BALANCE CHECK VARIABLES FOR START OF CURRENT TIME STEP.

      DO 100 I=IL1,IL2

          WTGSTP(I)=0.0
          CTVSTP(I)=-(CMAI(I)*SPHVEG+RCAN(I)*SPHW+
     1             SCAN(I)*SPHICE)*TCAN(I)
          CTSSTP(I)=-TSNOW(I)*(HCPICE*SNO(I)/RHOICE+
     1             HCPW*WSNOW(I)/RHOW)

        DO J=1,IG
           IF (J.eq.1) THEN !Include ZPOND and TPOND for the first layer
                CTNSTP(I,J) = -((HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)
     1             +HCPS(I,J)*(1.0-THPOR(I,J)))*DELZW(I,J)+
     2             HCPSND*(DELZ(J)-DELZW(I,J)))*TBAR(I,J)-
     3             HCPW*ZPOND(I)*TPOND(I)


 
           ELSE !All other layers

               CTNSTP(I,J)=-((HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)
     1             +HCPS(I,J)*(1.0-THPOR(I,J)))*DELZW(I,J)+
     2             HCPSND*(DELZ(J)-DELZW(I,J)))*TBAR(I,J)
           ENDIF
       ENDDO

            WTVSTP(I)=-(RCAN(I)+SCAN(I))
            WTSSTP(I)=-SNO(I)-WSNOW(I)
            DO 50 J=1,IG
              WTGSTP(I)=WTGSTP(I)-
     1             (THLIQ(I,J)*RHOW+THICE(I,J)*RHOICE)*
     2             DELZW(I,J)
 50       CONTINUE
          WTGSTP(I)=WTGSTP(I)-ZPOND(I)*RHOW
100   CONTINUE
C
      ENDIF
C
      IF(ISTEP.EQ.1) THEN
C
C     * CHECK ENERGY AND WATER BALANCES OVER THE CURRENT TIME STEP.
                 
C
      DO 200 I=IL1,IL2

          CTVSTP(I)=CTVSTP(I)+(CMAI(I)*SPHVEG+RCAN(I)*SPHW+
     1             SCAN(I)*SPHICE)*TCAN(I)
          CTSSTP(I)=CTSSTP(I)+TSNOW(I)*(HCPICE*SNO(I)/RHOICE+
     1             HCPW*WSNOW(I)/RHOW)

         DO J=1,IG
           IF (J.eq.1) THEN !Include ZPOND and TPOND for the first layer
            CTNSTP(I,J)=CTNSTP(I,J)+((HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)
     1             +HCPS(I,J)*(1.0-THPOR(I,J)))*DELZW(I,J)+
     2             HCPSND*(DELZ(J)-DELZW(I,J)))*TBAR(I,J)+
     3             HCPW*ZPOND(I)*TPOND(I)
           ELSE !All other layers
            CTNSTP(I,J)=CTNSTP(I,J)+((HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)
     1             +HCPS(I,J)*(1.0-THPOR(I,J)))*DELZW(I,J)+
     2             HCPSND*(DELZ(J)-DELZW(I,J)))*TBAR(I,J)
           ENDIF
         ENDDO

        DO J=1,IG !For every soil layer
            CTNSTP(I,J)=CTNSTP(I,J)/DELT
        ENDDO
          CTVSTP(I)=CTVSTP(I)/DELT
          CTSSTP(I)=CTSSTP(I)/DELT
          WTVSTP(I)=WTVSTP(I)+RCAN(I)+SCAN(I)
          WTSSTP(I)=WTSSTP(I)+SNO(I)+WSNOW(I)
          DO 150 J=1,IG
              WTGSTP(I)=WTGSTP(I)+
     1             (THLIQ(I,J)*RHOW+THICE(I,J)*RHOICE)*
     2             DELZW(I,J)
150       CONTINUE
          WTGSTP(I)=WTGSTP(I)+ZPOND(I)*RHOW
200   CONTINUE


C
      DO 400 I=IL1,IL2
          QSUMV=FSGV(I)+FLGV(I)-HFSC(I)-HEVC(I)-HMFC(I)+HTCC(I)      
          QSUMS=FSGS(I)+FLGS(I)-HFSS(I)-HEVS(I)-HMFN(I)+HTCS(I)   

          DO J=1,IG !For every soil layer
             IF (J.eq.1) THEN !Include every surface variables for first layer
           QSUMN(J)=FSGG(I)+FLGG(I)-HFSG(I)-
     1             HEVG(I)-HMFG(I,J)+HTC(I,J)
 
           ELSE !All other layers
           QSUMN(J)= -HMFG(I,J)+HTC(I,J)             
             ENDIF
          ENDDO

          WSUMV=(PCFC(I)+PCLC(I)-
     1          QFCF(I)-QFCL(I)-ROFC(I)+
     2              WTRC(I))*DELT
          WSUMS=(PCPN(I)-QFN(I)-
     1              ROFN(I)+WTRS(I))*DELT
          WSUMG=(PCPG(I)-QFG(I)-
     1              ROF(I)+WTRG(I))*DELT
          DO 250 J=1,IG
              WSUMG=WSUMG-QFC(I,J)*DELT
250       CONTINUE
C
           IF(ABS(CTVSTP(I)-QSUMV).GT.1.0) THEN
              WRITE(6,6441) N,CTVSTP(I),QSUMV
6441          FORMAT(2X,'CANOPY ENERGY BALANCE  ',I8,2F20.8)
              WRITE(6,6450) FSGV(I),FLGV(I),HFSC(I),
     1             HEVC(I),HMFC(I),HTCC(I)
              WRITE(6,6450) RCAN(I),SCAN(I),TCAN(I)
              CALL EXIT
          ENDIF
          IF(ABS(CTSSTP(I)-QSUMS).GT.7.0) THEN
              WRITE(6,6442) N,I,CTSSTP(I),QSUMS
6442          FORMAT(2X,'SNOW ENERGY BALANCE  ',2I8,2F20.8)
              WRITE(6,6450) FSGS(I),FLGS(I),HFSS(I),
     1            HEVS(I),HMFN(I),HTCS(I)
              WRITE(6,6450) TSNOW(I),SNO(I),WSNOW(I)
              WRITE(6,6451) FCS(I),FGS(I),FC(I),FG(I)
              CALL EXIT
          ENDIF


!The below check was simplied and no longer gives numbers or messages
!The previous version was unique for each layer so this should be included at some point
! S.Sauer May/2019
          DO J =1,IG !Go over every soil layer
              IF(ABS(CTNSTP(I,J)-QSUMN(J)).GT.5.0) THEN
                  WRITE(6,6443) I,J,CTNSTP(I,J),QSUMN(J)
6451              FORMAT(2X,7E20.6)
6443              FORMAT(2X,'Soil Layer ENERGY BALANCE  ',2I8,2F20.8)
                  CALL EXIT !Exit
              ENDIF
          ENDDO
            
          IF(ABS(WTVSTP(I)-WSUMV).GT.1.0E-3) THEN
              WRITE(6,6446) N,WTVSTP(I),WSUMV
6446          FORMAT(2X,'CANOPY WATER BALANCE  ',I8,2F20.8)
              CALL EXIT
          ENDIF
          IF(ABS(WTSSTP(I)-WSUMS).GT.1.0E-2) THEN
              WRITE(6,6447) N,I,WTSSTP(I),WSUMS
6447          FORMAT(2X,'SNOW WATER BALANCE  ',2I8,2F20.8)
              WRITE(6,6450) PCPN(I)*DELT,QFN(I)*DELT,
     1            ROFN(I)*DELT,WTRS(I)*DELT
              WRITE(6,6450) SNO(I),WSNOW(I),TSNOW(I)-TFREZ
              WRITE(6,6451) FCS(I),FGS(I),FC(I),FG(I)
              CALL EXIT
          ENDIF

!Message below should be updated for better representation of more layers

          IF(ABS(WTGSTP(I)-WSUMG).GT.1.0E-1) THEN
              WRITE(6,6448) N,I,WTGSTP(I),WSUMG
6448          FORMAT(2X,'GROUND WATER BALANCE  ',2I8,2F20.8)
              WRITE(6,6450) PCPG(I)*DELT,QFG(I)*DELT,
     1            QFC(I,1)*DELT,QFC(I,2)*DELT,
     2            QFC(I,3)*DELT,ROF(I)*DELT,
     3            WTRG(I)*DELT
              DO 390 J=1,IG
                  WRITE(6,6450) THLIQ(I,J)*RHOW*DELZW(I,J),
     *                THICE(I,J)*RHOICE*DELZW(I,J),
     *                DELZW(I,J)
390           CONTINUE
              WRITE(6,6450) ZPOND(I)*RHOW
6450          FORMAT(2X,7F15.6)
              WRITE(6,6451) FCS(I),FGS(I),FC(I),FG(I)
              CALL EXIT
          ENDIF
400   CONTINUE
C
      ENDIF
C
      RETURN
      END

