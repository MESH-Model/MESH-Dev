SUBROUTINE WRCMEMASCII

! Purpose :
! -------
!  WRITE CMEM OUTPUT DATA IN ASC FORMAT
   
! Interface :
! ---------

! Method :
! ------

! Externals :
! ---------

! Internal variables
! ------------------

! Author :
!     Patricia de Rosnay  October 2007

!------------------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM 
USE YOMLUN   , ONLY : NULOUT,NULTMP

USE YOMCMEMPAR, ONLY : JPHISTLEV,CNAMEID,CFREQ,CANGLE
USE YOMCMEMFIELDS, ONLY: N_CMEM,JJ,doy,ndate,ntime, nbpt,CLNAME &
                     & ,fwc_veg,fb,ftfrac,fh,fWP,falpha,fsal_wat,ftau_atm,ftb_au &
                     & ,ftb_toa,fteffC,fsurf_emis,ftau_veg


IMPLICIT NONE

!------------------------------------------------------------------------------


! LEVEL 1 outputs
!----------------

  CLNAME='out_tbtoat_'//CNAMEID//'_'//cfreq//'_'//cangle//'.asc'

  OPEN(NULTMP,FILE=CLNAME,status='replace')
  WRITE(NULTMP,'(a65)') '#   nbpt   date        hour     doy    TBH(K)  TBV(K)   TEFF_H(K)'
  DO JJ = 1, N_CMEM
  WRITE(NULTMP,'(3I9,f9.2,4(f9.3))') nbpt(JJ), ndate(JJ),ntime(JJ), doy(JJ), ftb_toa(JJ,1),ftb_toa(JJ,2),fteffC(JJ,2)
  ENDDO
  CLOSE(NULTMP)




! LEVEL 2 Outputs:
!------------------

  SELECT CASE (JPHISTLEV)

  CASE ( 2_JPIM, 3_JPIM )

  CLNAME='out_tauveg_'//CNAMEID//'_'//cfreq//'_'//cangle//'.asc'
  OPEN(NULTMP,FILE=CLNAME,status='replace')
   WRITE(NULTMP,'(a62)') '#   nbpt   date        hour     doy   Tau_veg (H,V)'
   DO JJ = 1, N_CMEM
   WRITE(NULTMP,'(3I9,f9.2,2f9.3)') nbpt(JJ), ndate(JJ),ntime(JJ), doy(JJ),ftau_veg(JJ,1),ftau_veg(JJ,2)
   ENDDO
  CLOSE(NULTMP)


  CLNAME='out_frbare_'//CNAMEID//'_'//cfreq//'_'//cangle//'.asc'
  OPEN(NULTMP,FILE=CLNAME,status='replace')
   WRITE(NULTMP,'(a62)') '#   nbpt   date        hour     doy   Total bare soil fraction'
   DO JJ = 1, N_CMEM
   WRITE(NULTMP,'(3I9,f9.2,f9.3)') nbpt(JJ), ndate(JJ),ntime(JJ), doy(JJ),ftfrac(JJ,1)+ftfrac(JJ,2)
   ENDDO
  CLOSE(NULTMP)
   
 
  CLNAME='out_VWCsum_'//CNAMEID//'_'//cfreq//'_'//cangle//'.asc'
  OPEN(NULTMP,FILE=CLNAME,status='replace')
   WRITE(NULTMP,'(a76)') '#   nbpt   date        hour     doy   Total vegetation water content [kg/m2]'
   DO JJ = 1, N_CMEM
   WRITE(NULTMP,'(3I9,f9.2,f9.3)')  nbpt(JJ), ndate(JJ),ntime(JJ), doy(JJ), &
                          & fwc_veg(JJ,1)*(ftfrac(JJ,3)+ftfrac(JJ,4)) +  fwc_veg(JJ,2)*(ftfrac(JJ,5)+ftfrac(JJ,6))
   ENDDO
  CLOSE(NULTMP)
 
 
  CLNAME='out_tau_at_'//CNAMEID//'_'//cfreq//'_'//cangle//'.asc'
  OPEN(NULTMP,FILE=CLNAME,status='replace')
   WRITE(NULTMP,'(a63)') '#   nbpt   date        hour     doy   Atmospheric optical depth'
   DO JJ = 1, N_CMEM
   WRITE(NULTMP,'(3I9,f9.2,f9.3)')  nbpt(JJ), ndate(JJ),ntime(JJ), doy(JJ), ftau_atm(JJ)
   ENDDO
  CLOSE(NULTMP)


  CLNAME='out_tb_aup_'//CNAMEID//'_'//cfreq//'_'//cangle//'.asc'
  OPEN(NULTMP,FILE=CLNAME,status='replace')
   WRITE(NULTMP,'(a62)') '#   nbpt   date        hour     doy   TB atmosphere Upward (K)'
   DO JJ = 1, N_CMEM
   WRITE(NULTMP,'(3I9,f9.2,f9.3)') nbpt(JJ), ndate(JJ),ntime(JJ), doy(JJ), ftb_au(JJ) 
   ENDDO
  CLOSE(NULTMP)
 
 
  END SELECT


  SELECT CASE (JPHISTLEV)


! LEVLE 3 outputs:
!------------------

   CASE ( 3_JPIM )
 
  CLNAME='out_ffrac1_'//CNAMEID//'_'//cfreq//'_'//cangle//'.asc'
  OPEN(NULTMP,FILE=CLNAME,status='replace')
   WRITE(NULTMP,'(a71)') '#   nbpt   date        hour     doy   Pure (no snow) bare soil fraction'
   DO JJ = 1, N_CMEM
   WRITE(NULTMP,'(3I9,f9.2,f9.3)') nbpt(JJ), ndate(JJ),ntime(JJ), doy(JJ), ftfrac(JJ,1) 
   ENDDO
  CLOSE(NULTMP)


  CLNAME='out_ffrac3_'//CNAMEID//'_'//cfreq//'_'//cangle//'.asc'
  OPEN(NULTMP,FILE=CLNAME,status='replace')
   WRITE(NULTMP,'(a76)') '#   nbpt   date        hour     doy   Pure (no snow) low vegetation fraction'
   DO JJ = 1, N_CMEM
   WRITE(NULTMP,'(3I9,f9.2,f9.3)') nbpt(JJ), ndate(JJ),ntime(JJ), doy(JJ), ftfrac(JJ,3)
   ENDDO
  CLOSE(NULTMP)

 
  CLNAME='out_VWC_LO_'//CNAMEID//'_'//cfreq//'_'//cangle//'.asc'
  OPEN(NULTMP,FILE=CLNAME,status='replace')
   WRITE(NULTMP,'(a74)') '#   nbpt   date        hour     doy   Low vegetation water content [kg/m2]'
   DO JJ = 1, N_CMEM
   WRITE(NULTMP,'(3I9,f9.2,f9.3)')  nbpt(JJ), ndate(JJ),ntime(JJ), doy(JJ), fwc_veg(JJ,1)
   ENDDO
  CLOSE(NULTMP)


  CLNAME='out_VWC_HI_'//CNAMEID//'_'//cfreq//'_'//cangle//'.asc'
  OPEN(NULTMP,FILE=CLNAME,status='replace')
   WRITE(NULTMP,'(a75)') '#   nbpt   date        hour     doy   High vegetation water content [kg/m2]'
   DO JJ = 1, N_CMEM
   WRITE(NULTMP,'(3I9,f9.2,f9.3)')  nbpt(JJ), ndate(JJ),ntime(JJ), doy(JJ), fwc_veg(JJ,2)
   ENDDO
  CLOSE(NULTMP)

  CLNAME='out_Bpa_LO_'//CNAMEID//'_'//cfreq//'_'//cangle//'.asc'
  OPEN(NULTMP,FILE=CLNAME,status='replace')
   WRITE(NULTMP,'(a64)') '#   nbpt   date        hour     doy   Low vegetation b parameter'
   DO JJ = 1, N_CMEM
   WRITE(NULTMP,'(3I9,f9.2,f9.3)')  nbpt(JJ), ndate(JJ),ntime(JJ), doy(JJ), fb(JJ,1)
   ENDDO
  CLOSE(NULTMP)

  CLNAME='out_Bpa_HI_'//CNAMEID//'_'//cfreq//'_'//cangle//'.asc'
  OPEN(NULTMP,FILE=CLNAME,status='replace')
   WRITE(NULTMP,'(a65)') '#   nbpt   date        hour     doy   High vegetation b parameter'
   DO JJ = 1, N_CMEM
   WRITE(NULTMP,'(3I9,f9.2,f9.3)')  nbpt(JJ), ndate(JJ),ntime(JJ), doy(JJ), fb(JJ,2)
   ENDDO
  CLOSE(NULTMP)

  CLNAME='out_fh_par_'//CNAMEID//'_'//cfreq//'_'//cangle//'.asc'
  OPEN(NULTMP,FILE=CLNAME,status='replace')
   WRITE(NULTMP,'(a59)') '#   nbpt   date        hour     doy   Roughness parameter h'
   DO JJ = 1, N_CMEM
   WRITE(NULTMP,'(3I9,f9.2,f9.3)')  nbpt(JJ), ndate(JJ),ntime(JJ), doy(JJ), fh(JJ)
   ENDDO
  CLOSE(NULTMP)

  CLNAME='out_WP_par_'//CNAMEID//'_'//cfreq//'_'//cangle//'.asc'
  OPEN(NULTMP,FILE=CLNAME,status='replace')
   WRITE(NULTMP,'(a70)') '#   nbpt   date        hour     doy    Wilting point soil moisture (%)'
   DO JJ = 1, N_CMEM
   WRITE(NULTMP,'(3I9,f9.2,f9.3)')  nbpt(JJ), ndate(JJ),ntime(JJ), doy(JJ), fWP(JJ)
   ENDDO
  CLOSE(NULTMP)

  CLNAME='out_emissH_'//CNAMEID//'_'//cfreq//'_'//cangle//'.asc'
  OPEN(NULTMP,FILE=CLNAME,status='replace')
   WRITE(NULTMP,'(a60)') '#   nbpt   date        hour     doy   Horizontal emissivity '
   DO JJ = 1, N_CMEM
   WRITE(NULTMP,'(3I9,f9.2,f9.3)')  nbpt(JJ), ndate(JJ),ntime(JJ), doy(JJ), fsurf_emis(JJ,1)
   ENDDO
  CLOSE(NULTMP)

  CLNAME='out_emissV_'//CNAMEID//'_'//cfreq//'_'//cangle//'.asc'
  OPEN(NULTMP,FILE=CLNAME,status='replace')
   WRITE(NULTMP,'(a58)') '#   nbpt   date        hour     doy   Vertical emissivity '
   DO JJ = 1, N_CMEM
   WRITE(NULTMP,'(3I9,f9.2,f9.3)')  nbpt(JJ), ndate(JJ),ntime(JJ), doy(JJ), fsurf_emis(JJ,2)
   ENDDO
  CLOSE(NULTMP)

  CLNAME='out_teff_C_'//CNAMEID//'_'//cfreq//'_'//cangle//'.asc'  ! C parameter
  OPEN(NULTMP,FILE=CLNAME,status='replace')
   WRITE(NULTMP,'(a81)') '#   nbpt   date        hour     doy   C parameter used for effective temperature '
   DO JJ = 1, N_CMEM
   WRITE(NULTMP,'(3I9,f9.2,f9.3)')  nbpt(JJ), ndate(JJ),ntime(JJ), doy(JJ), fteffC(JJ,1)
   ENDDO
  CLOSE(NULTMP)

ENDSELECT

       
END SUBROUTINE WRCMEMASCII

