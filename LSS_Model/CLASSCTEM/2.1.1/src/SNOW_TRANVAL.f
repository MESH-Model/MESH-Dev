      SUBROUTINE SNOW_TRANVAL(trandif, ! OUTPUT                                   SNOW_TRANVAL.2 
     1                        trandir,                                            SNOW_TRANVAL.3 
     2                        smu,     ! INPUT                                    SNOW_TRANVAL.4 
     3                        salb,                                               SNOW_TRANVAL.5 
     4                        bc_conc,                                            SNOW_TRANVAL.6 
     5                        snow_reff,                                          SNOW_TRANVAL.7 
     6                        swe,                                                SNOW_TRANVAL.8 
     7                        c_ind,                                              SNOW_TRANVAL.9 
     8                        il1,                                                SNOW_TRANVAL.10
     9                        il2,                                                SNOW_TRANVAL.11
     1                        ilg,                                                SNOW_TRANVAL.12
     2                        nbnd     )                                          SNOW_TRANVAL.13
!                                                                                 SNOW_TRANVAL.14
!     * JAN 24, 2013 - J. COLE                                                    SNOW_TRANVAL.15
!                    - COMPUTES THE DIRECT AND DIFFUSE SNOW TRANSMISSION          SNOW_TRANVAL.16
!                      USING LOOKUP TABLE AND CURRENT SNOW CONDITIONS.            SNOW_TRANVAL.17
!                                                                                 SNOW_TRANVAL.18
                                                                                  SNOW_TRANVAL.19
      IMPLICIT NONE                                                               SNOW_TRANVAL.20
                                                                                  SNOW_TRANVAL.21
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~             SNOW_TRANVAL.22
! THIS SUBROUTINE COMPUTE THE DIRECT AND DIFFUSE SNOW TRANSMISSION USING A        SNOW_TRANVAL.23
! LOOKUP TABLE AND INFORMATION ABOUT THE CURRENT SNOW PACK STATE.                 SNOW_TRANVAL.24
! TRANSMISSION ARE COMPUTED FOR EACH SOLAR RADIATION WAVELENGTH INTERVALS         SNOW_TRANVAL.25
! SO A TOTAL OF 8 ALBEDOS WILL BE RETURNED.  THESE TRANSMISSIONS CAN THEN BE      SNOW_TRANVAL.26
! USED TO COMPUTE THE TOTAL SNOW TRAMISSION BY WEIGHTING                          SNOW_TRANVAL.27
! THE RESULTS BY THE DIRECT BEAM FRACTION OF THE INCIDENT SOLAR RADIATION.        SNOW_TRANVAL.28
!                                                                                 SNOW_TRANVAL.29
! INPUTS                                                                          SNOW_TRANVAL.30
! SMU:       COSINE OF THE SOLAR ZENITH ANGLE [UNITLESS]                          SNOW_TRANVAL.31
! SALB :     ALBEDO OF THE UNDERLYING SURFACE [UNITLESS]                          SNOW_TRANVAL.32
! BC_CONC:   CONCENTRATION OF BLACK CARBON IN THE SNOW PACK [NG (BC)/KG (SNOW)]   SNOW_TRANVAL.33
! SNOW_REFF: EFFECTIVE RADIUS OF THE SNOW GRAIN [MICRONS]                         SNOW_TRANVAL.34
! SWE:       SNOW WATER EQUIVALENT (SNOWPACK DENSITY*SNOW PACK DEPTH) [KG/M^2]    SNOW_TRANVAL.35
! C_IND:     INDICATOR THAT A CALCULATION SHOULD BE PERFORMED FOR THIS POINT      SNOW_TRANVAL.36
!            1-YES, 0-NO                                                          SNOW_TRANVAL.37
! IL1:       STARTING POINT FOR TRANSMISSION CALCULATIONS                         SNOW_TRANVAL.38
! IL2:       ENDING POINT FOR TRANSMISSION CALCULATIONS                           SNOW_TRANVAL.39
! ILG:       NUMBER OF POINTS FOR WHICH TO COMPUTE TRANSMISSIONS                  SNOW_TRANVAL.40
! NBND:      NUMBER OF WAVELENGTH INTERVALS FOR WHICH TO COMPUTE THE TRANSMISSIONSSNOW_TRANVAL.41
!                                                                                 SNOW_TRANVAL.42
! OUTPUTS                                                                         SNOW_TRANVAL.43
! TRANDIF: DIFFUSE SNOW TRANSMISSION (AKA WHITE SKY TRANSMISSION)                 SNOW_TRANVAL.44
! TRANDIR: DIRECT BEAM SNOW TRANSMISSION (AKA BLACK SKY TRANSMISSION)             SNOW_TRANVAL.45
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~             SNOW_TRANVAL.46
                                                                                  SNOW_TRANVAL.47
!                                                                                 SNOW_TRANVAL.48
! INPUT                                                                           SNOW_TRANVAL.49
!                                                                                 SNOW_TRANVAL.50
      REAL, INTENT(IN), DIMENSION(ILG) ::                                         SNOW_TRANVAL.51
     1 smu,                                                                       SNOW_TRANVAL.52
     2 bc_conc,                                                                   SNOW_TRANVAL.53
     3 snow_reff,                                                                 SNOW_TRANVAL.54
     4 swe                                                                        SNOW_TRANVAL.55
                                                                                  SNOW_TRANVAL.56
      REAL, INTENT(IN), DIMENSION(ILG,NBND) ::                                    SNOW_TRANVAL.57
     1 salb                                                                       SNOW_TRANVAL.58
                                                                                  SNOW_TRANVAL.59
      INTEGER, INTENT(IN), DIMENSION(ILG) ::                                      SNOW_TRANVAL.60
     1 c_ind                                                                      SNOW_TRANVAL.61
                                                                                  SNOW_TRANVAL.62
      INTEGER, INTENT(IN) ::                                                      SNOW_TRANVAL.63
     1 il1,                                                                       SNOW_TRANVAL.64
     2 il2,                                                                       SNOW_TRANVAL.65
     3 ilg,                                                                       SNOW_TRANVAL.66
     4 nbnd                                                                       SNOW_TRANVAL.67
                                                                                  SNOW_TRANVAL.68
!                                                                                 SNOW_TRANVAL.69
! OUTPUT                                                                          SNOW_TRANVAL.70
!                                                                                 SNOW_TRANVAL.71
      REAL, INTENT(OUT), DIMENSION(ILG,NBND) ::                                   SNOW_TRANVAL.72
     1 trandif,                                                                   SNOW_TRANVAL.73
     2 trandir                                                                    SNOW_TRANVAL.74
                                                                                  SNOW_TRANVAL.75
!                                                                                 SNOW_TRANVAL.76
! LOCAL                                                                           SNOW_TRANVAL.77
!                                                                                 SNOW_TRANVAL.78
      REAL, DIMENSION(ILG,2) ::                                                   SNOW_TRANVAL.79
     1 wsmu,                                                                      SNOW_TRANVAL.80
     2 wbc,                                                                       SNOW_TRANVAL.81
     3 wreff,                                                                     SNOW_TRANVAL.82
     4 wswe                                                                       SNOW_TRANVAL.83
                                                                                  SNOW_TRANVAL.84
      REAL ::                                                                     SNOW_TRANVAL.85
     1 wsalb(2)                                                                   SNOW_TRANVAL.86
                                                                                  SNOW_TRANVAL.87
      REAL ::                                                                     SNOW_TRANVAL.88
     1 wtt                                                                        SNOW_TRANVAL.89
                                                                                  SNOW_TRANVAL.90
      INTEGER, DIMENSION(ILG) ::                                                  SNOW_TRANVAL.91
     1 ismu,                                                                      SNOW_TRANVAL.92
     2 ibc,                                                                       SNOW_TRANVAL.93
     3 ireff,                                                                     SNOW_TRANVAL.94
     4 iswe                                                                       SNOW_TRANVAL.95
                                                                                  SNOW_TRANVAL.96
      INTEGER ::                                                                  SNOW_TRANVAL.97
     1 ib,                                                                        SNOW_TRANVAL.98
     2 i,                                                                         SNOW_TRANVAL.99
     3 isalb                                                                      SNOW_TRANVAL.100
                                                                                  SNOW_TRANVAL.101
      INTEGER ::                                                                  SNOW_TRANVAL.102
     1 iismu,                                                                     SNOW_TRANVAL.103
     2 iisalb,                                                                    SNOW_TRANVAL.104
     3 iibc,                                                                      SNOW_TRANVAL.105
     4 iireff,                                                                    SNOW_TRANVAL.106
     5 iiswe                                                                      SNOW_TRANVAL.107
                                                                                  SNOW_TRANVAL.108
      INTEGER ::                                                                  SNOW_TRANVAL.109
     1 mvidx                                                                      SNOW_TRANVAL.110
                                                                                  SNOW_TRANVAL.111
!                                                                                 SNOW_TRANVAL.112
! CONSTANTS                                                                       SNOW_TRANVAL.113
!                                                                                 SNOW_TRANVAL.114
      INTEGER, PARAMETER ::                                                       SNOW_TRANVAL.115
     1 nsmu     = 10,                                                             SNOW_TRANVAL.116
     2 nsalb    = 11,                                                             SNOW_TRANVAL.117
     3 nbc      = 20,                                                             new_snow_lut.11
     4 nreff    = 10,                                                             SNOW_TRANVAL.119
     5 nswe     = 11,                                                             SNOW_TRANVAL.120
     6 nbnd_lut = 4                                                               SNOW_TRANVAL.121
                                                                                  SNOW_TRANVAL.122
      REAL, PARAMETER :: ! STATE VALUES FOR LUT                                   SNOW_TRANVAL.123
     1 LSALB(NSALB)    =                                                          SNOW_TRANVAL.124
     2                  (/0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0/),          SNOW_TRANVAL.125
     3 LSMU(NSMU)        =                                                        SNOW_TRANVAL.126
     4                      (/0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0/),          SNOW_TRANVAL.127
     5 LSNOW_REFF(NREFF) =                                                        SNOW_TRANVAL.128
     6   (/50.0,75.0,100.0,150.0,200.0,275.0,375.0,500.0,700.0,1000.0/),          SNOW_TRANVAL.129
     7 LSWE(NSWE)        =                                                        SNOW_TRANVAL.130
     8      (/0.1,0.25,0.65,1.7,4.4,12.0,30.0,75.0,200.0,500.0,5000.0/),          SNOW_TRANVAL.131
     9 LBC_CONC(NBC)     = (/0.0,1.0,                                             new_snow_lut.12
     1                       5.0,10.0,                                            new_snow_lut.13
     2                       50.0,100.0,                                          new_snow_lut.14
     3                       500.0,1000.0,                                        new_snow_lut.15
     4                       5000.0,10000.0,                                      new_snow_lut.16
     5                       50000.0,100000.0,                                    new_snow_lut.17
     6                       250000.0,500000.0,750000.0,1000000.0,                new_snow_lut.18
     7                       2500000.0,5000000.0,7500000.0,10000000.0/)           new_snow_lut.19
                                                                                  SNOW_TRANVAL.134
      REAL, DIMENSION(NBC,NSWE,NREFF,NSMU,NSALB,NBND_LUT) ::                      SNOW_TRANVAL.135
     1 trandif_lut,                                                               SNOW_TRANVAL.136
     2 trandir_lut                                                                SNOW_TRANVAL.137
                                                                                  SNOW_TRANVAL.138
      INTEGER ::                                                                  SNOW_TRANVAL.139
     1 snow_tran_lut_init                                                         SNOW_TRANVAL.140
                                                                                  SNOW_TRANVAL.141
       COMMON /SNOWTRANLUT/ trandif_lut,trandir_lut,snow_tran_lut_init            SNOW_TRANVAL.142
                                                                                  SNOW_TRANVAL.143
! ABORT IF THE LUT HAS NOT BEEN READ IN                                           SNOW_TRANVAL.144
      IF (snow_tran_lut_init .NE. 1) THEN                                         SNOW_TRANVAL.145
         WRITE(6,*) 'SNOW TRANSMISSION LUT HAS NOT BEEN INITIALIZED',             SNOW_TRANVAL.146
     1              snow_tran_lut_init                                            SNOW_TRANVAL.147
         CALL XIT('SNOW_TRANVAL',-1)                                              SNOW_TRANVAL.148
      END IF                                                                      SNOW_TRANVAL.149
                                                                                  SNOW_TRANVAL.150
! ABORT IF THE NUMBER OF BANDS IN THE LUT DOES NOT MATCH SIZE PASSED IN           SNOW_TRANVAL.151
      IF (nbnd_lut .NE. nbnd) THEN                                                SNOW_TRANVAL.152
         WRITE(6,*) 'MISMATCH IN NUMBER OF WAVELENGTH INTERVALS'                  SNOW_TRANVAL.153
         CALL XIT('SNOW_TRANVAL',-2)                                              SNOW_TRANVAL.154
      END IF                                                                      SNOW_TRANVAL.155
                                                                                  SNOW_TRANVAL.156
! COMPUTE THE TRANSMISSIONS USING LINEAR INTERPOLATION                            SNOW_TRANVAL.157
                                                                                  SNOW_TRANVAL.158
! COMPUTE THE INTERPOLATION WEIGHTS AND POINTS ONCE AND REUSE FOR                 SNOW_TRANVAL.159
! TRANSMISSION INTERPOLATION FOR EACH BAND.                                       SNOW_TRANVAL.160
! HAVE A CHECK TO SET THE WEIGHTS DEPENDING IF THE INPUT IS                       SNOW_TRANVAL.161
! OUTSIDE OR INSIDE THE LOOKUP TABLE RANGE                                        SNOW_TRANVAL.162
                                                                                  SNOW_TRANVAL.163
      DO i = il1, il2                                                             SNOW_TRANVAL.164
         IF (c_ind(i) .EQ. 1) THEN                                                SNOW_TRANVAL.165
            ismu(i)  = mvidx(LSMU,       nsmu,  smu(i))                           SNOW_TRANVAL.166
            ibc(i)   = mvidx(LBC_CONC,   nbc,   bc_conc(i))                       SNOW_TRANVAL.167
            ireff(i) = mvidx(LSNOW_REFF, nreff, snow_reff(i))                     SNOW_TRANVAL.168
            iswe(i)  = mvidx(LSWE,       nswe,  swe(i))                           SNOW_TRANVAL.169
                                                                                  SNOW_TRANVAL.170
            IF (smu(i) .LE. LSMU(1)) THEN                                         SNOW_TRANVAL.171
               wsmu(i,2) = 0.0                                                    SNOW_TRANVAL.172
               wsmu(i,1) = 1.0-wsmu(i,2)                                          SNOW_TRANVAL.173
            ELSEIF (smu(i) .GT. LSMU(NSMU)) THEN                                  SNOW_TRANVAL.174
               wsmu(i,2) = 1.0                                                    SNOW_TRANVAL.175
               wsmu(i,1) = 1.0-wsmu(i,2)                                          SNOW_TRANVAL.176
            ELSE                                                                  SNOW_TRANVAL.177
               wsmu(i,2) = (smu(i)-LSMU(ismu(i)))                                 SNOW_TRANVAL.178
     1                   / (LSMU(ismu(i)+1)-LSMU(ismu(i)))                        SNOW_TRANVAL.179
               wsmu(i,1) = 1.0-wsmu(i,2)                                          SNOW_TRANVAL.180
            END IF                                                                SNOW_TRANVAL.181
                                                                                  SNOW_TRANVAL.182
            IF (bc_conc(i) .LE. LBC_CONC(1)) THEN                                 SNOW_TRANVAL.183
               wbc(i,2) = 0.0                                                     SNOW_TRANVAL.184
               wbc(i,1) = 1.0-wbc(i,2)                                            SNOW_TRANVAL.185
            ELSEIF (bc_conc(i) .GT. LBC_CONC(NBC)) THEN                           SNOW_TRANVAL.186
               wbc(i,2) = 1.0                                                     SNOW_TRANVAL.187
               wbc(i,1) = 1.0-wbc(i,2)                                            SNOW_TRANVAL.188
            ELSE                                                                  SNOW_TRANVAL.189
               wbc(i,2) = (bc_conc(i)-LBC_CONC(ibc(i)))                           SNOW_TRANVAL.190
     1                  / (LBC_CONC(ibc(i)+1)-LBC_CONC(ibc(i)))                   SNOW_TRANVAL.191
               wbc(i,1) = 1.0-wbc(i,2)                                            SNOW_TRANVAL.192
            END IF                                                                SNOW_TRANVAL.193
                                                                                  SNOW_TRANVAL.194
            IF (snow_reff(i) .LE. LSNOW_REFF(1)) THEN                             SNOW_TRANVAL.195
               wreff(i,2) = 0.0                                                   SNOW_TRANVAL.196
               wreff(i,1) = 1.0-wreff(i,2)                                        SNOW_TRANVAL.197
            ELSEIF (snow_reff(i) .GT. LSNOW_REFF(NREFF)) THEN                     SNOW_TRANVAL.198
               wreff(i,2) = 1.0                                                   SNOW_TRANVAL.199
               wreff(i,1) = 1.0-wreff(i,2)                                        SNOW_TRANVAL.200
            ELSE                                                                  SNOW_TRANVAL.201
               wreff(i,2) = (snow_reff(i)-LSNOW_REFF(ireff(i)))                   SNOW_TRANVAL.202
     1                    / (LSNOW_REFF(ireff(i)+1)                               SNOW_TRANVAL.203
     2                                   -LSNOW_REFF(ireff(i)))                   SNOW_TRANVAL.204
               wreff(i,1) = 1.0-wreff(i,2)                                        SNOW_TRANVAL.205
            END IF                                                                SNOW_TRANVAL.206
                                                                                  SNOW_TRANVAL.207
            IF (swe(i) .LE. LSWE(1)) THEN                                         SNOW_TRANVAL.208
               wswe(i,2) = 0.0                                                    SNOW_TRANVAL.209
               wswe(i,1) = 1.0-wswe(i,2)                                          SNOW_TRANVAL.210
            ELSEIF (swe(i) .GT. LSWE(NSWE)) THEN                                  SNOW_TRANVAL.211
               wswe(i,2) = 1.0                                                    SNOW_TRANVAL.212
               wswe(i,1) = 1.0-wswe(i,2)                                          SNOW_TRANVAL.213
            ELSE                                                                  SNOW_TRANVAL.214
               wswe(i,2) = (swe(i)-LSWE(iswe(i)))                                 SNOW_TRANVAL.215
     1                   / (LSWE(iswe(i)+1)-LSWE(iswe(i)))                        SNOW_TRANVAL.216
               wswe(i,1) = 1.0-wswe(i,2)                                          SNOW_TRANVAL.217
            END IF                                                                SNOW_TRANVAL.218
         END IF                                                                   SNOW_TRANVAL.219
      END DO ! i                                                                  SNOW_TRANVAL.220
                                                                                  SNOW_TRANVAL.221
      DO ib = 1, nbnd                                                             SNOW_TRANVAL.222
         DO i = il1, il2                                                          SNOW_TRANVAL.223
            IF (c_ind(i) .EQ. 1) THEN                                             SNOW_TRANVAL.224
                                                                                  SNOW_TRANVAL.225
               isalb = mvidx(LSALB,    nsalb, salb(i,ib))                         SNOW_TRANVAL.226
                                                                                  SNOW_TRANVAL.227
               IF (salb(i,ib) .LE. LSALB(1)) THEN                                 SNOW_TRANVAL.228
                  wsalb(2) = 0.0                                                  SNOW_TRANVAL.229
                  wsalb(1) = 1.0-wsalb(2)                                         SNOW_TRANVAL.230
               ELSEIF (salb(i,ib) .GT. LSALB(NSALB)) THEN                         SNOW_TRANVAL.231
                  wsalb(2) = 1.0                                                  SNOW_TRANVAL.232
                  wsalb(1) = 1.0-wsalb(2)                                         SNOW_TRANVAL.233
               ELSE                                                               SNOW_TRANVAL.234
                  wsalb(2) = (salb(i,ib)-LSALB(isalb))                            SNOW_TRANVAL.235
     1                     / (LSALB(isalb+1)-LSALB(isalb))                        SNOW_TRANVAL.236
                  wsalb(1) = 1.0-wsalb(2)                                         SNOW_TRANVAL.237
               END IF                                                             SNOW_TRANVAL.238
                                                                                  SNOW_TRANVAL.239
               trandir(i,ib) = 0.0                                                SNOW_TRANVAL.240
               trandif(i,ib) = 0.0                                                SNOW_TRANVAL.241
                                                                                  SNOW_TRANVAL.242
               DO iisalb = isalb,isalb+1                                          SNOW_TRANVAL.243
                  DO iismu = ismu(i),ismu(i)+1                                    SNOW_TRANVAL.244
                     DO iireff = ireff(i),ireff(i)+1                              SNOW_TRANVAL.245
                        DO iiswe = iswe(i), iswe(i)+1                             SNOW_TRANVAL.246
                           DO iibc = ibc(i), ibc(i)+1                             SNOW_TRANVAL.247
                                                                                  SNOW_TRANVAL.248
                              wtt = wsmu(i,iismu-ismu(i)+1)                       SNOW_TRANVAL.249
     +                            * wreff(i,iireff-ireff(i)+1)                    SNOW_TRANVAL.250
     +                            * wswe(i,iiswe-iswe(i)+1)                       SNOW_TRANVAL.251
     +                            * wbc(i,iibc-ibc(i)+1)                          SNOW_TRANVAL.252
     +                            * wsalb(iisalb-isalb+1)                              SNOW_TRANVAL.253
                                                                                  SNOW_TRANVAL.254
                              trandif(i,ib) = trandif(i,ib) + wtt                 SNOW_TRANVAL.255
     +                   *trandif_lut(iibc,iiswe,iireff,iismu,iisalb,ib)          SNOW_TRANVAL.256
                              trandir(i,ib) = trandir(i,ib) + wtt                 SNOW_TRANVAL.257
     +                   *trandir_lut(iibc,iiswe,iireff,iismu,iisalb,ib)          SNOW_TRANVAL.258
                                                                                  SNOW_TRANVAL.259
                           END DO ! iibc                                          SNOW_TRANVAL.260
                        END DO  ! iiswe                                           SNOW_TRANVAL.261
                     END DO     ! iireff                                          SNOW_TRANVAL.262
                  END DO        ! iismu                                           SNOW_TRANVAL.263
               END DO           ! iisalb                                          SNOW_TRANVAL.264
                                                                                  SNOW_TRANVAL.265
               IF(trandif(i,ib) .GT. 1.3 .OR.                                     SNOW_TRANVAL.266
     1                                     trandif(i,ib) .LT. 0.0) THEN           SNOW_TRANVAL.267
                  WRITE(6,*) 'Bad trandif ',i,ib,smu(i),bc_conc(i),               SNOW_TRANVAL.268
     1                     snow_reff(i),swe(i),salb(i,ib),trandif(i,ib)           SNOW_TRANVAL.269
                  WRITE(6,*) i,ib,ismu(i),ibc(i),ireff(i),iswe(i),isalb           SNOW_TRANVAL.270
                  CALL XIT('SNOW_TRANVAL',-3)                                     SNOW_TRANVAL.271
               END IF                                                             SNOW_TRANVAL.272
               IF(trandir(i,ib) .GT. 1.3 .OR.                                     SNOW_TRANVAL.273
     1                                     trandir(i,ib) .LT. 0.0) THEN           SNOW_TRANVAL.274
                  WRITE(6,*) 'Bad trandir ',i,ib,smu(i),bc_conc(i),               SNOW_TRANVAL.275
     1                      snow_reff(i),swe(i),salb(i,ib),trandir(i,ib)          SNOW_TRANVAL.276
                  WRITE(6,*) i,ib,ismu(i),ibc(i),ireff(i),iswe(i),isalb           SNOW_TRANVAL.277
                  CALL XIT('SNOW_TRANVAL',-3)                                     SNOW_TRANVAL.278
               END IF                                                             SNOW_TRANVAL.279
            ELSE                                                                  SNOW_TRANVAL.280
               trandif(i,ib) = -999.0                                             SNOW_TRANVAL.281
               trandir(i,ib) = -999.0                                             SNOW_TRANVAL.282
            END IF                                                                SNOW_TRANVAL.283
         END DO                 ! i                                               SNOW_TRANVAL.284
      END DO                    ! ib                                              SNOW_TRANVAL.285
                                                                                  SNOW_TRANVAL.286
      RETURN                                                                      SNOW_TRANVAL.287
      END                                                                         SNOW_TRANVAL.288
