      SUBROUTINE SNOW_ALBVAL(albdif, ! OUTPUT                                     SNOW_ALBVAL.2  
     1                       albdir,                                              SNOW_ALBVAL.3  
     2                       smu,    ! INPUT                                      SNOW_ALBVAL.4  
     3                       salb,                                                SNOW_ALBVAL.5  
     4                       bc_conc,                                             SNOW_ALBVAL.6  
     5                       snow_reff,                                           SNOW_ALBVAL.7  
     6                       swe,                                                 SNOW_ALBVAL.8  
     7                       c_ind,                                               SNOW_ALBVAL.9  
     8                       il1,                                                 SNOW_ALBVAL.10 
     9                       il2,                                                 SNOW_ALBVAL.11 
     1                       ilg,                                                 SNOW_ALBVAL.12 
     2                       nbnd     )                                           SNOW_ALBVAL.13 
!                                                                                 SNOW_ALBVAL.14 
!     * JAN 24, 2013 - J. COLE                                                    SNOW_ALBVAL.15 
!                    - COMPUTES THE DIRECT AND DIFFUSE SNOW ALBEDO                SNOW_ALBVAL.16 
!                      USING LOOKUP TABLE AND CURRENT SNOW CONDITIONS.            SNOW_ALBVAL.17 
!                                                                                 SNOW_ALBVAL.18 
                                                                                  SNOW_ALBVAL.19 
      IMPLICIT NONE                                                               SNOW_ALBVAL.20 
                                                                                  SNOW_ALBVAL.21 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~             SNOW_ALBVAL.22 
! THIS SUBROUTINE COMPUTE THE DIRECT AND DIFFUSE SNOW ALBEDO USING A              SNOW_ALBVAL.23 
! LOOKUP TABLE AND INFORMATION ABOUT THE CURRENT SNOW PACK STATE.                 SNOW_ALBVAL.24 
! ALBEDOS ARE COMPUTED FOR EACH SOLAR RADIATION WAVELENGTH INTERVALS              SNOW_ALBVAL.25 
! SO A TOTAL OF 8 ALBEDOS WILL BE RETURNED.  THESE ALBEDOS CAN THEN BE            SNOW_ALBVAL.26 
! USED TO COMPUTE THE TOTAL SNOW ALBEDO BASED ON THE BY WEIGHTING                 SNOW_ALBVAL.27 
! THE RESULTS BY THE DIRECT BEAM FRACTION OF THE INCIDENT SOLAR RADIATION.        SNOW_ALBVAL.28 
!                                                                                 SNOW_ALBVAL.29 
! INPUTS                                                                          SNOW_ALBVAL.30 
! SMU:       COSINE OF THE SOLAR ZENITH ANGLE [UNITLESS]                          SNOW_ALBVAL.31 
! SALB :     ALBEDO OF THE UNDERLYING SURFACE [UNITLESS]                          SNOW_ALBVAL.32 
! BC_CONC:   CONCENTRATION OF BLACK CARBON IN THE SNOW PACK [NG (BC)/KG (SNOW)]   SNOW_ALBVAL.33 
! SNOW_REFF: EFFECTIVE RADIUS OF THE SNOW GRAIN [MICRONS]                         SNOW_ALBVAL.34 
! SWE:       SNOW WATER EQUIVALENT (SNOWPACK DENSITY*SNOW PACK DEPTH) [KG/M^2]    SNOW_ALBVAL.35 
! C_IND:     INDICATOR THAT A CALCULATION SHOULD BE PERFORMED FOR THIS POINT      SNOW_ALBVAL.36 
!            1-YES, 0-NO                                                          SNOW_ALBVAL.37 
! IL1:       STARTING POINT FOR ALBEDO CALCULATIONS                               SNOW_ALBVAL.38 
! IL2:       ENDING POINT FOR ALBEDO CALCULATIONS                                 SNOW_ALBVAL.39 
! ILG:      NUMBER OF POINTS FOR WHICH TO COMPUTE ALBEDOS                         SNOW_ALBVAL.40 
! NBND:      NUMBER OF WAVELENGTH INTERVALS FOR WHICH TO COMPUTE THE ALBEDOS      SNOW_ALBVAL.41 
!                                                                                 SNOW_ALBVAL.42 
! OUTPUTS                                                                         SNOW_ALBVAL.43 
! ALBDIF: DIFFUSE SNOW ALBEDO (AKA WHITE SKY ALBEDO)                              SNOW_ALBVAL.44 
! ALBDIR: DIRECT BEAM SNOW ALBEDO (AKA BLACK SKY ALBEDO)                          SNOW_ALBVAL.45 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~             SNOW_ALBVAL.46 
                                                                                  SNOW_ALBVAL.47 
!                                                                                 SNOW_ALBVAL.48 
! INPUT                                                                           SNOW_ALBVAL.49 
!                                                                                 SNOW_ALBVAL.50 
      REAL, INTENT(IN), DIMENSION(ILG) ::                                         SNOW_ALBVAL.51 
     1 smu,                                                                       SNOW_ALBVAL.52 
     2 bc_conc,                                                                   SNOW_ALBVAL.53 
     3 snow_reff,                                                                 SNOW_ALBVAL.54 
     4 swe                                                                        SNOW_ALBVAL.55 
                                                                                  SNOW_ALBVAL.56 
      REAL, INTENT(IN), DIMENSION(ILG,NBND) ::                                    SNOW_ALBVAL.57 
     1 salb                                                                       SNOW_ALBVAL.58 
                                                                                  SNOW_ALBVAL.59 
      INTEGER, INTENT(IN), DIMENSION(ILG) ::                                      SNOW_ALBVAL.60 
     1 c_ind                                                                      SNOW_ALBVAL.61 
                                                                                  SNOW_ALBVAL.62 
      INTEGER, INTENT(IN) ::                                                      SNOW_ALBVAL.63 
     1 il1,                                                                       SNOW_ALBVAL.64 
     2 il2,                                                                       SNOW_ALBVAL.65 
     3 ilg,                                                                       SNOW_ALBVAL.66 
     4 nbnd                                                                       SNOW_ALBVAL.67 
                                                                                  SNOW_ALBVAL.68 
!                                                                                 SNOW_ALBVAL.69 
! OUTPUT                                                                          SNOW_ALBVAL.70 
!                                                                                 SNOW_ALBVAL.71 
      REAL, INTENT(OUT), DIMENSION(ILG,NBND) ::                                   SNOW_ALBVAL.72 
     1 albdif,                                                                    SNOW_ALBVAL.73 
     2 albdir                                                                     SNOW_ALBVAL.74 
                                                                                  SNOW_ALBVAL.75 
!                                                                                 SNOW_ALBVAL.76 
! LOCAL                                                                           SNOW_ALBVAL.77 
!                                                                                 SNOW_ALBVAL.78 
      REAL, DIMENSION(ILG,2) ::                                                   SNOW_ALBVAL.79 
     1 wsmu,                                                                      SNOW_ALBVAL.80 
     2 wbc,                                                                       SNOW_ALBVAL.81 
     3 wreff,                                                                     SNOW_ALBVAL.82 
     4 wswe                                                                       SNOW_ALBVAL.83 
                                                                                  SNOW_ALBVAL.84 
      REAL ::                                                                     SNOW_ALBVAL.85 
     1 wsalb(2)                                                                   SNOW_ALBVAL.86 
                                                                                  SNOW_ALBVAL.87 
      REAL ::                                                                     SNOW_ALBVAL.88 
     1 wtt,                                                                       SNOW_ALBVAL.89 
     2 snow_reff_l                                                                SNOW_ALBVAL.90 
                                                                                  SNOW_ALBVAL.91 
      INTEGER, DIMENSION(ILG) ::                                                  SNOW_ALBVAL.92 
     1 ismu,                                                                      SNOW_ALBVAL.93 
     2 ibc,                                                                       SNOW_ALBVAL.94 
     3 ireff,                                                                     SNOW_ALBVAL.95 
     4 iswe                                                                       SNOW_ALBVAL.96 
                                                                                  SNOW_ALBVAL.97 
      INTEGER ::                                                                  SNOW_ALBVAL.98 
     1 ib,                                                                        SNOW_ALBVAL.99 
     2 i,                                                                         SNOW_ALBVAL.100
     3 isalb                                                                      SNOW_ALBVAL.101
                                                                                  SNOW_ALBVAL.102
      INTEGER ::                                                                  SNOW_ALBVAL.103
     1 iismu,                                                                     SNOW_ALBVAL.104
     2 iisalb,                                                                    SNOW_ALBVAL.105
     3 iibc,                                                                      SNOW_ALBVAL.106
     4 iireff,                                                                    SNOW_ALBVAL.107
     5 iiswe                                                                      SNOW_ALBVAL.108
                                                                                  SNOW_ALBVAL.109
      INTEGER ::                                                                  SNOW_ALBVAL.110
     1 mvidx                                                                      SNOW_ALBVAL.111
                                                                                  SNOW_ALBVAL.112
!                                                                                 SNOW_ALBVAL.113
! CONSTANTS                                                                       SNOW_ALBVAL.114
!                                                                                 SNOW_ALBVAL.115
      INTEGER, PARAMETER ::                                                       SNOW_ALBVAL.116
     1 nsmu     = 10,                                                             SNOW_ALBVAL.117
     2 nsalb    = 11,                                                             SNOW_ALBVAL.118
     3 nbc      = 20,                                                             new_snow_lut.2 
     4 nreff    = 10,                                                             SNOW_ALBVAL.120
     5 nswe     = 11,                                                             SNOW_ALBVAL.121
     6 nbnd_lut = 4                                                               SNOW_ALBVAL.122
                                                                                  SNOW_ALBVAL.123
      REAL, PARAMETER :: ! STATE VALUES FOR LUT                                   SNOW_ALBVAL.124
     1 LSALB(NSALB)    =                                                          SNOW_ALBVAL.125
     2                  (/0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0/),          SNOW_ALBVAL.126
     3 LSMU(NSMU)        =                                                        SNOW_ALBVAL.127
     4                      (/0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0/),          SNOW_ALBVAL.128
     5 LSNOW_REFF(NREFF) =                                                        SNOW_ALBVAL.129
     6   (/50.0,75.0,100.0,150.0,200.0,275.0,375.0,500.0,700.0,1000.0/),          SNOW_ALBVAL.130
     7 LSWE(NSWE)        =                                                        SNOW_ALBVAL.131
     8      (/0.1,0.25,0.65,1.7,4.4,12.0,30.0,75.0,200.0,500.0,5000.0/),          SNOW_ALBVAL.132
     9 LBC_CONC(NBC)     = (/0.0,1.0,                                             new_snow_lut.3 
     1                       5.0,10.0,                                            new_snow_lut.4 
     2                       50.0,100.0,                                          new_snow_lut.5 
     3                       500.0,1000.0,                                        new_snow_lut.6 
     4                       5000.0,10000.0,                                      new_snow_lut.7 
     5                       50000.0,100000.0,                                    new_snow_lut.8 
     6                       250000.0,500000.0,750000.0,1000000.0,                new_snow_lut.9 
     7                       2500000.0,5000000.0,7500000.0,10000000.0/)           new_snow_lut.10
                                                                                  SNOW_ALBVAL.135
      REAL, DIMENSION(NBC,NSWE,NREFF,NSMU,NSALB,NBND_LUT) ::                      SNOW_ALBVAL.136
     1 albdif_lut,                                                                SNOW_ALBVAL.137
     2 albdir_lut                                                                 SNOW_ALBVAL.138
                                                                                  SNOW_ALBVAL.139
      INTEGER ::                                                                  SNOW_ALBVAL.140
     1 snow_alb_lut_init                                                          SNOW_ALBVAL.141
                                                                                  SNOW_ALBVAL.142
       COMMON /SNOWALBLUT/ albdif_lut,albdir_lut,snow_alb_lut_init                SNOW_ALBVAL.143
                                                                                  SNOW_ALBVAL.144
! ABORT IF THE LUT HAS NOT BEEN READ IN                                           SNOW_ALBVAL.145
      IF (snow_alb_lut_init .NE. 1) THEN                                          SNOW_ALBVAL.146
         WRITE(6,*) 'SNOW ALBEDO LUT HAS NOT BEEN INITIALIZED',                   SNOW_ALBVAL.147
     1              snow_alb_lut_init                                             SNOW_ALBVAL.148
         CALL XIT('SNOW_ALBVAL',-1)                                               SNOW_ALBVAL.149
      END IF                                                                      SNOW_ALBVAL.150
                                                                                  SNOW_ALBVAL.151
! ABORT IF THE NUMBER OF BANDS IN THE LUT DOES NOT MATCH SIZE PASSED IN           SNOW_ALBVAL.152
      IF (nbnd_lut .NE. nbnd) THEN                                                SNOW_ALBVAL.153
         WRITE(6,*) 'MISMATCH IN NUMBER OF WAVELENGTH INTERVALS'                  SNOW_ALBVAL.154
         CALL XIT('SNOW_ALBVAL',-2)                                               SNOW_ALBVAL.155
      END IF                                                                      SNOW_ALBVAL.156
                                                                                  SNOW_ALBVAL.157
! COMPUTE THE ALBEDOS USING LINEAR INTERPOLATION                                  SNOW_ALBVAL.158
                                                                                  SNOW_ALBVAL.159
! COMPUTE THE INTERPOLATION WEIGHTS AND POINTS ONCE AND REUSE FOR                 SNOW_ALBVAL.160
! ALBEDO INTERPOLATION FOR EACH BAND.                                             SNOW_ALBVAL.161
! HAVE A CHECK TO SET THE WEIGHTS DEPENDING IF THE INPUT IS                       SNOW_ALBVAL.162
! OUTSIDE OR INSIDE THE LOOKUP TABLE RANGE                                        SNOW_ALBVAL.163
                                                                                  SNOW_ALBVAL.164
      DO i = il1, il2                                                             SNOW_ALBVAL.165
         IF (c_ind(i) .EQ. 1) THEN                                                SNOW_ALBVAL.166
            snow_reff_l = snow_reff(i)                                            SNOW_ALBVAL.167
            ismu(i)     = mvidx(LSMU,       nsmu,  smu(i))                        SNOW_ALBVAL.168
            ibc(i)      = mvidx(LBC_CONC,   nbc,   bc_conc(i))                    SNOW_ALBVAL.169
            ireff(i)    = mvidx(LSNOW_REFF, nreff, snow_reff(i))                  SNOW_ALBVAL.170
            iswe(i)     = mvidx(LSWE,       nswe,  swe(i))                        SNOW_ALBVAL.171
                                                                                  SNOW_ALBVAL.172
            IF (smu(i) .LE. LSMU(1)) THEN                                         SNOW_ALBVAL.173
               wsmu(i,2) = 0.0                                                    SNOW_ALBVAL.174
               wsmu(i,1) = 1.0-wsmu(i,2)                                          SNOW_ALBVAL.175
            ELSEIF (smu(i) .GT. LSMU(NSMU)) THEN                                  SNOW_ALBVAL.176
               wsmu(i,2) = 1.0                                                    SNOW_ALBVAL.177
               wsmu(i,1) = 1.0-wsmu(i,2)                                          SNOW_ALBVAL.178
            ELSE                                                                  SNOW_ALBVAL.179
               wsmu(i,2) = (smu(i)-LSMU(ismu(i)))                                 SNOW_ALBVAL.180
     1                   / (LSMU(ismu(i)+1)-LSMU(ismu(i)))                        SNOW_ALBVAL.181
               wsmu(i,1) = 1.0-wsmu(i,2)                                          SNOW_ALBVAL.182
            END IF                                                                SNOW_ALBVAL.183
                                                                                  SNOW_ALBVAL.184
            IF (bc_conc(i) .LE. LBC_CONC(1)) THEN                                 SNOW_ALBVAL.185
               wbc(i,2) = 0.0                                                     SNOW_ALBVAL.186
               wbc(i,1) = 1.0-wbc(i,2)                                            SNOW_ALBVAL.187
            ELSEIF (bc_conc(i) .GT. LBC_CONC(NBC)) THEN                           SNOW_ALBVAL.188
               wbc(i,2) = 1.0                                                     SNOW_ALBVAL.189
               wbc(i,1) = 1.0-wbc(i,2)                                            SNOW_ALBVAL.190
            ELSE                                                                  SNOW_ALBVAL.191
               wbc(i,2) = (bc_conc(i)-LBC_CONC(ibc(i)))                           SNOW_ALBVAL.192
     1                  / (LBC_CONC(ibc(i)+1)-LBC_CONC(ibc(i)))                   SNOW_ALBVAL.193
               wbc(i,1) = 1.0-wbc(i,2)                                            SNOW_ALBVAL.194
            END IF                                                                SNOW_ALBVAL.195
                                                                                  SNOW_ALBVAL.196
            IF (snow_reff_l .LE. LSNOW_REFF(1)) THEN                              SNOW_ALBVAL.197
               wreff(i,2) = 0.0                                                   SNOW_ALBVAL.198
               wreff(i,1) = 1.0-wreff(i,2)                                        SNOW_ALBVAL.199
            ELSEIF (snow_reff_l .GT. LSNOW_REFF(NREFF)) THEN                      SNOW_ALBVAL.200
               wreff(i,2) = 1.0                                                   SNOW_ALBVAL.201
               wreff(i,1) = 1.0-wreff(i,2)                                        SNOW_ALBVAL.202
            ELSE                                                                  SNOW_ALBVAL.203
               wreff(i,2) = (snow_reff_l-LSNOW_REFF(ireff(i)))                    SNOW_ALBVAL.204
     1                    / (LSNOW_REFF(ireff(i)+1)                               SNOW_ALBVAL.205
     2                                   -LSNOW_REFF(ireff(i)))                   SNOW_ALBVAL.206
               wreff(i,1) = 1.0-wreff(i,2)                                        SNOW_ALBVAL.207
            END IF                                                                SNOW_ALBVAL.208
                                                                                  SNOW_ALBVAL.209
            IF (swe(i) .LE. LSWE(1)) THEN                                         SNOW_ALBVAL.210
               wswe(i,2) = 0.0                                                    SNOW_ALBVAL.211
               wswe(i,1) = 1.0-wswe(i,2)                                          SNOW_ALBVAL.212
            ELSEIF (swe(i) .GT. LSWE(NSWE)) THEN                                  SNOW_ALBVAL.213
               wswe(i,2) = 1.0                                                    SNOW_ALBVAL.214
               wswe(i,1) = 1.0-wswe(i,2)                                          SNOW_ALBVAL.215
            ELSE                                                                  SNOW_ALBVAL.216
               wswe(i,2) = (swe(i)-LSWE(iswe(i)))                                 SNOW_ALBVAL.217
     1                   / (LSWE(iswe(i)+1)-LSWE(iswe(i)))                        SNOW_ALBVAL.218
               wswe(i,1) = 1.0-wswe(i,2)                                          SNOW_ALBVAL.219
            END IF                                                                SNOW_ALBVAL.220
         END IF                                                                   SNOW_ALBVAL.221
      END DO ! i                                                                  SNOW_ALBVAL.222
                                                                                  SNOW_ALBVAL.223
      DO ib = 1, nbnd                                                             SNOW_ALBVAL.224
         DO i = il1, il2                                                          SNOW_ALBVAL.225
            IF (c_ind(i) .EQ. 1) THEN                                             SNOW_ALBVAL.226
                                                                                  SNOW_ALBVAL.227
               isalb = mvidx(LSALB,    nsalb, salb(i,ib))                         SNOW_ALBVAL.228
                                                                                  SNOW_ALBVAL.229
               IF (salb(i,ib) .LE. LSALB(1)) THEN                                 SNOW_ALBVAL.230
                  wsalb(2) = 0.0                                                  SNOW_ALBVAL.231
                  wsalb(1) = 1.0-wsalb(2)                                         SNOW_ALBVAL.232
               ELSEIF (salb(i,ib) .GT. LSALB(NSALB)) THEN                         SNOW_ALBVAL.233
                  wsalb(2) = 1.0                                                  SNOW_ALBVAL.234
                  wsalb(1) = 1.0-wsalb(2)                                         SNOW_ALBVAL.235
               ELSE                                                               SNOW_ALBVAL.236
                  wsalb(2) = (salb(i,ib)-LSALB(isalb))                            SNOW_ALBVAL.237
     1                     / (LSALB(isalb+1)-LSALB(isalb))                        SNOW_ALBVAL.238
                  wsalb(1) = 1.0-wsalb(2)                                         SNOW_ALBVAL.239
               END IF                                                             SNOW_ALBVAL.240
                                                                                  SNOW_ALBVAL.241
               albdir(i,ib) = 0.0                                                 SNOW_ALBVAL.242
               albdif(i,ib) = 0.0                                                 SNOW_ALBVAL.243
                                                                                  SNOW_ALBVAL.244
               DO iisalb = isalb,isalb+1                                          SNOW_ALBVAL.245
                  DO iismu = ismu(i),ismu(i)+1                                    SNOW_ALBVAL.246
                     DO iireff = ireff(i),ireff(i)+1                              SNOW_ALBVAL.247
                        DO iiswe = iswe(i), iswe(i)+1                             SNOW_ALBVAL.248
                           DO iibc = ibc(i), ibc(i)+1                             SNOW_ALBVAL.249
                                                                                  SNOW_ALBVAL.250
                              wtt = wsmu(i,iismu-ismu(i)+1)                       SNOW_ALBVAL.251
     +                            * wreff(i,iireff-ireff(i)+1)                    SNOW_ALBVAL.252
     +                            * wswe(i,iiswe-iswe(i)+1)                       SNOW_ALBVAL.253
     +                            * wbc(i,iibc-ibc(i)+1)                          SNOW_ALBVAL.254
     +                            * wsalb(iisalb-isalb+1)                              SNOW_ALBVAL.255
                                                                                  SNOW_ALBVAL.256
                              albdif(i,ib) = albdif(i,ib) + wtt                   SNOW_ALBVAL.257
     +                    *albdif_lut(iibc,iiswe,iireff,iismu,iisalb,ib)          SNOW_ALBVAL.258
                              albdir(i,ib) = albdir(i,ib) + wtt                   SNOW_ALBVAL.259
     +                    *albdir_lut(iibc,iiswe,iireff,iismu,iisalb,ib)          SNOW_ALBVAL.260
                                                                                  SNOW_ALBVAL.261
                           END DO ! iibc                                          SNOW_ALBVAL.262
                        END DO  ! iiswe                                           SNOW_ALBVAL.263
                     END DO     ! iireff                                          SNOW_ALBVAL.264
                  END DO        ! iismu                                           SNOW_ALBVAL.265
               END DO           ! iisalb                                          SNOW_ALBVAL.266
                                                                                  SNOW_ALBVAL.267
               IF(albdif(i,ib) .GT. 1.0 .OR. albdif(i,ib) .LT. 0.0) THEN          SNOW_ALBVAL.268
                  WRITE(6,*) 'Bad albdif ',i,ib,smu(i),bc_conc(i),                SNOW_ALBVAL.269
     1                     snow_reff(i),swe(i),salb(i,ib),albdif(i,ib)            SNOW_ALBVAL.270
                  WRITE(6,*) i,ib,ismu(i),ibc(i),ireff(i),iswe(i),isalb           SNOW_ALBVAL.271
                  CALL XIT('SNOW_ALBVAL',-3)                                      SNOW_ALBVAL.272
               END IF                                                             SNOW_ALBVAL.273
               IF(albdir(i,ib) .GT. 1.0 .OR. albdir(i,ib) .LT. 0.0) THEN          SNOW_ALBVAL.274
                  WRITE(6,*) 'Bad albdir ',i,ib,smu(i),bc_conc(i),                SNOW_ALBVAL.275
     1                       snow_reff(i),swe(i),salb(i,ib),albdir(i,ib)          SNOW_ALBVAL.276
                  WRITE(6,*) i,ib,ismu(i),ibc(i),ireff(i),iswe(i),isalb           SNOW_ALBVAL.277
                  CALL XIT('SNOW_ALBVAL',-3)                                      SNOW_ALBVAL.278
               END IF                                                             SNOW_ALBVAL.279
            ELSE                                                                  SNOW_ALBVAL.280
               albdif(i,ib) = -999.0                                              SNOW_ALBVAL.281
               albdir(i,ib) = -999.0                                              SNOW_ALBVAL.282
            END IF                                                                SNOW_ALBVAL.283
         END DO                 ! i                                               SNOW_ALBVAL.284
      END DO                    ! ib                                              SNOW_ALBVAL.285
                                                                                  SNOW_ALBVAL.286
      RETURN                                                                      SNOW_ALBVAL.287
      END                                                                         SNOW_ALBVAL.288
