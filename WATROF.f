C    Combination of Ric's interflow and Ric/Bruce's overland flow 20070126
      SUBROUTINE WATROF(IVEG,THLIQ,THICE,ZPOND,TPOND,OVRFLW,TOVRFL,
     1                  SUBFLW,TSUBFL,BASFLW,TBASFL,RUNOFF,TRUNOF,FI,
     2                  ZPLIM,XSLOPE,GRKFAC,MANNING_N,DD,WFCINT,
     3                  ZFAV,LZFAV,THLINV,TBARW,DELZW,ZBOTW,THPOR,
     4                  THLMIN,BI,THFC,DODRN,DOVER,DIDRN,ISAND,IWF,IG,
     5                  ILG,IL1,IL2,JL,IGP1)


C     * SEP 16/06 - R.SOULIS/F.SEGLENIEKS/A.PIETRONIRO/B.DAVISON.
C     *             MODIFICATIONS TO OVERLAND FLOW.
C     * SEP 15/05 - D.VERSEGHY. REMOVE HARD CODING OF IG=3.
C     * MAR 30/05 - D.VERSEGHY. ADDITIONAL FIELDS.
C     * NOV 03/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * AUG 02/02 - R.SOULIS/D.VERSEGHY. UPDATES DEVELOPED AT 
C     *             WATERLOO.
C     * DEC 10/01 - R.SOULIS/K.SNELGROVE/T.WHIDDEN/D.VERSEGHY 
C     *             WATFLOOD ROUTINE TO CALCULATE OVERLAND FLOW AND
C     *             INTERFLOW COMPONENTS OF SURFACE RUNOFF.
C
      IMPLICIT NONE
C
      INTEGER IWF,IG,ILG,IL1,IL2,I,J,IGP1
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL  THLIQ (ILG,IG),  THICE (ILG,IG)
C
      REAL  ZPOND (ILG),     TPOND (ILG),     OVRFLW(ILG),   
     1      TOVRFL(ILG),     SUBFLW(ILG),     
     2      RUNOFF(ILG),     TRUNOF(ILG)
C
C     * INPUT ARRAYS.
C
      REAL  FI    (ILG),     ZPLIM (ILG),     XSLOPE(ILG),
     1      GRKFAC(ILG),     WFCINT(ILG),
     2      TBARW (ILG,IG)
C 
C     * SOIL INFORMATION ARRAYS.
C
      REAL  DELZW (ILG,IG),   
     1      THPOR (ILG,IG),  THLMIN(ILG,IG),
     2      BI    (ILG,IG),  THFC  (ILG,IG)

      INTEGER                ISAND (ILG,IG)
C
C     * WORK ARRAYS.
C
      REAL  DODRN (ILG),     DOVER (ILG),
     1      DIDRN (ILG,IG)
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ
C
      COMMON /CLASS1/ DELT,TFREZ
      
C Additions due to AUG 8, 2006 changes to WATROF
C     * INPUT/OUTPUT ARRAYS.
C
      REAL  basflw(ilg), zf(ilg)

C     * SOIL INFORMATION ARRAYS.
C
      REAL  thlinf(ilg,igp1), recharge(ilg),
     1      trecharge(ilg),  delcharge(ilg)

      REAL HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1     SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2     TCGLAC,CLHMLT,CLHVAP

C      * DECLARATIONS NEEDED BECAUSE OF "IMPLICIT NONE"
      REAL ZSNOW(ILG),thliqt,dlinf,delzf,
     1     delzx,satfc,
     2     thporx,satx,xdrainv,qflow_avg,qsint_layer,depth,
     3     xdrain_all,xlztop,di_max,davail,dtot,
     4     TSUBFL(ILG),TBASFL(ILG),ZFAV(ILG),THLINV(ILG),ZBOTW (ILG,IG)

      INTEGER jj,lzf(ILG),lzfx,jwf,IVEG,LZFAV(ILG),JL

      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP

	INTEGER lzfflg,extflg,iwfice,IMIN,IHOUR,IDAY,IYEAR,errflg
      REAL viceflg,psi_limit,hiceflg

      COMMON /WATFLGS/ viceflg,psi_limit,hiceflg,lzfflg,extflg,iwfice,
     +errflg,IMIN,IHOUR,IDAY,IYEAR

C     * DECLARATIONS NEEDED DUE TO REMOVAL OF FVISC AND FICE FUNCTIONS
c      REAL FICE(ILG,IG),FRAC_ICE(ILG,IG),THPOR_EFF(ILG,IG),
c     1     FVISC(ILG,IG),visct,viscr
     
C     * INTERNAL SCALARS AND VECTORS
      REAL VEL_T0(ILG),NUC_DOVER(ILG),MANNING_N(ILG),DD(ILG)

c     Input variables to wat_drain
      REAL asat_t0,cc,grk_eff,H,xdrainh,H0,ztop

c	Output variables from wat_drain
      REAL asat_t1,bflow_t0

c     Declarations resulting from trying to deal with the code fork of October 14, 2006
      REAL trunoff(ILG),thlinf0,thliq_eff,
     +     thliq_avail,thpor_avail,
     +     qsatxx_t0,thliqxx_t0,xiceflg,thlret(ILG,IG),
     +     qsatxx_t1,recharge0,bsat_t0,fvisc,fice

C-----------------------------------------------------------------------
C
c     skip if using flat class 
      if(iwf.eq.0)return
c-----------------------------------------------------------------------------
c
c     save basflw but restore liquid water to third layer to give interflow a chance
c
c-----------------------------------------------------------------------------
c

      do i=il1,il2
      IF(FI(I).GT.0.0) THEN
	  if(runoff(i).lt.1.0E-08)then
          recharge(i)=0.0
	    trecharge(i)=0.0
          runoff(i)=0.0
          delcharge(i)=0.0
	  else
          recharge(i)=runoff(i)
	    trecharge(i)=trunoff(i)
          runoff(i)=0.0
          delcharge(i)=0.0
	    call watmix(thliq(i,ig),tbarw(i,ig),
     +	  recharge(i)/delzw(i,ig),trecharge(i))
	  endif
      endif
	enddo

C
C     * PART 1 - OVERLAND FLOW
C     * (MODELLED USING MANNINGS EQUATION).  
C     * CALCULATED USING THREE PARAMETERS "XSLOPE, MANNING_N AND DD"
C     * XSLOPE = AVERAGE SLOPE OF GRU
C     * MANNING_N = MANNING'S 'N'
C     * DD = DRAINAGE DENSITY
C     * TWO OPTIONS ARE AVAILABLE TO CONSTRAIN THE FLOW

      DO 100 I=IL1,IL2
          IF(FI(I).GT.0.0) THEN 
            IF(ZPOND(I).GT.ZPLIM(I))THEN
C             Calculate the depth of water available for overland flow. Units: L
              DOVER(I)=ZPOND(I)-ZPLIM(I)

C             Calculate the flow velocity at the beginning of the timestep 
C             (based on kinematic wave velocity) Units: LT-1
              VEL_T0(I)=DOVER(I)**(2./3.)*SQRT(XSLOPE(I))/(MANNING_N(I))
C             Eqn (1) in spec doc

C             Calculate a normalized unconstrained overland flow to avoid numerical 
C             problems with a division of small DOVER(I) values. 
              NUC_DOVER(I) = -2*DD(I)*VEL_T0(I)*DELT
C             Eqn (29) in spec doc
C             Constrained Overland Flow - Limited by physically possible flow
              DODRN(I)=DOVER(I)*(1.0-1./((1.0-(2./3.)*NUC_DOVER(I))
     +	             **(3./2.)))
C             Eqn (30) in spec doc

              IF(RUNOFF(I).GT.1.0E-08) THEN
                 TRUNOF(I)=(TRUNOF(I)*RUNOFF(I)+(TPOND(I)+TFREZ)*
     1                     DODRN(I))/(RUNOFF(I)+DODRN(I))
              ENDIF
              RUNOFF(I)=RUNOFF(I)+DODRN(I)
              IF(DODRN(I).GT.0.0) 
     1            TOVRFL(I)=(TOVRFL(I)*OVRFLW(I)+(TPOND(I)+TFREZ)*
     2                FI(I)*DODRN(I))/(OVRFLW(I)+FI(I)*DODRN(I))
              OVRFLW(I)=OVRFLW(I)+FI(I)*DODRN(I)
              ZPOND(I)=ZPOND(I)-DODRN(I)
            ENDIF
          ENDIF  
 100  CONTINUE
c-----------------------------------------------------------------------------
c
C     * PART 2 - INTERFLOW - MODELLED AS FLOW ALONG A SHALLOW SLOPING AQUIFER
C     *     CALCULATED FROM THE RELATIVE DEPTH OF THE CONTRIBUTING
C     *     PORTION OF THE SOIL (HARAT), THE CRITICAL SOIL MOISTURE (THETA_C)
C     *     IS CONTENT AT WHICH UNSATURATED FLOW BEGINS TO TAKE PLACE.
C     *     THETA_A IS CONTENT AT WHICH UNSATURATED FLOW ENDS
C     * 
C
c-----------------------------------------------------------------------------
c

      DO 250 I=IL1,IL2
	DO 200 j=1,ig
          IF(FI(I).GT.0.0 .AND. ISAND(I,J).GE.-2)         THEN 
c
c-----------------------------------------------------------------------------
c
c         determine geometry of next layer or sub-layer
c
c-----------------------------------------------------------------------------
c

c         find top of current layer
          ztop = 0.0
          if(j.ge.2)then
	      do jj=2,j
	        ztop = ztop + delzw(i,jj-1)
            enddo
          endif

c
c-----------------------------------------------------------------------------
c
c         determine available liquidwater in layer
c
c-----------------------------------------------------------------------------
c
c
         thliq_avail = max(0.0,thliq(i,j)-thlmin(i,j))
         thpor_avail = max(thliq(i,j),thlmin(i,j),thpor(i,j)-thice(i,j))
         if(thliq_avail.gt.0.0 .and. delzw(i,j).gt.0.0) then
c
c           preparation of parameters
            asat_t0 = thliq_avail/thpor_avail
            xdrainh = grkfac(i)

c           qsint_max is saturated conveyance/unit area (ksat*slope/length of valley slope/porosity)
c           adjust for temperature effects and ice
            grk_eff = XSLOPE(i)/(1+XSLOPE(i)**2)*
     +		 WFCINT(i)*(2*DD(i))/(THPOR_AVAIL) 
     +        *fvisc(tbarw(i,j),iwfice)
     +        *fice(thliq(i,j),thice(i,j),thpor(i,j),thlmin(i,j),
     +         iwfice,hiceflg)

c           find average interflow for timestep

c

		  cc=2.*BI(I,j)+3
            H = delzw(i,j)
		  H0 = 1.0
c
c      operational call to wat_drain
            call WAT_DRAIN(2,H,H0,ztop,xdrainh,cc,grk_eff,
     +        bsat_t0,asat_t0,asat_t1)

            didrn(i,j) = (asat_t0-asat_t1)*thpor_avail*H

c           share bottom layer, below wetting front with drainage
            if(j.eq.ig)then
	        davail  = (thliq(i,j)-thlmin(i,j))*delzw(i,j)
              dtot = didrn(i,j)+recharge(i)	       
              if(davail.le.0.0)then
	          didrn(i,j) = 0.0
                delcharge(i)= recharge(i)
	          recharge(i)=0.0
	        elseif(dtot.gt.davail)then
	          didrn(i,j) = didrn(i,j)*(davail/dtot)
                recharge0 = recharge(i)
	          recharge(i) = recharge(i)*(davail/dtot)
	          delcharge(i) = recharge0-recharge(i)
              endif
            endif
c

	davail=max(0.0,thliq(i,j)-thlmin(i,j))*delzw(i,j)
      didrn(i,j)=max(0.0,min(davail,didrn(i,j)))

      if(didrn(i,j).gt.1.0e-8)then
	  call watmix(runoff(i),trunoff(i),didrn(i,j),tbarw(i,j))
        THLIQ(I,J) = THLIQ(I,J)-didrn(i,j)/delzw(i,j)
        SUBFLW(I)=SUBFLW(I)+FI(I)*didrn(i,j)
      endif
c

      ENDIF !if(thliq_avail.gt.0.0 .and. delzw(i,j).gt.0.0) then

      ENDIF !!IF(FI(I).GT.0.0 .AND. ISAND(I,J).GE.-2)

200   CONTINUE
250   CONTINUE

c
c     restore base flow if water left after interflow
      do i=il1,il2
        IF(FI(I).GT.0.0) THEN
          thliq(i,ig)=thliq(i,ig)-recharge(i)/delzw(i,ig)
	    call watmix(runoff(i),trunoff(i),recharge(i),trecharge(i))
	    basflw(i)=basflw(i)-fi(i)*delcharge(i)
	  endif
	enddo


      RETURN

      END

c******************************************************************************
c
	subroutine watmix(quan1,prop1,quan2,prop2)

      prop2t = max(0.0,prop2)
c
c	check for temperature mixes
c      if(abs(prop1-prop2).gt.100.)then
c	print *, prop1,prop2
c     continue
c	endif
c      if(prop1.lt.-100.0 .or.prop2.lt.0.0)then
c	print *, prop1,prop2
c      continue
c	endif
c
c
c	adds two quantities and blends their intrinsic properties
c
c	output overwrites quan1 and prop1
c

	if(quan2 .le. 0.0)then
	  return
	elseif(quan1 .le. 0.0)then
	  quan1=quan2
	  prop1=prop2t
	  return
	else
	  quan = quan1 + quan2
	  prop1 = (quan1*prop1 + quan2*prop2t)/quan
	  quan1 = quan
        return
	endif

	return

      END	


c******************************************************************************
	
	real function fvisc(tbar,iwfice)
c
c     corrects conductivity for temperature effects (ref:Dingman pg 545 B-12)

c     reference tempeature is 0.0C

      real tbar, tbart, visct, viscr
c
c	check function turned on
	if(iwfice.ne.2 .and. iwfice.ne.3)then
	  fvisc = 1.0
	  return
	endif

c     check function receiving celsius temperature
      tbart=max(0.00001,min(100.,tbar))
      if(tbar.gt.tbart)then
	  fvisc = 1.0
c	  print *,'tbar',tbar,' tbart',tbart
c	  pause
	  return
	endif

      visct=2.0319d-04+1.5883d-03*exp(-((tbart**0.9)/22.))
      viscr=1.7915d-03
      fvisc=viscr/visct

	return

	end

******************************************************************************

	real function fice(thliq,thice,thpor,thlmin,iwfice,iceflg)
c
c     corrects conductivity for ice content (ref:Gray,Toth,et al)

c
      REAL thliq,thice,thpor,thlmin,thliq_eff,thpor_eff,frac_ice,iceflg

c	check function turned on
	if(iwfice.ne.1 .and. iwfice.ne.3)then
	  fice = 1.0
	  return
	endif
c
c     allow pore space to expand to allow for "frost heave"
c      thpor_eff = max(0.0,thice,thpor-thlmin)
       thpor_eff = thpor

      if(thpor_eff .le. 0.0)then
        frac_ice = 0.0
      else 
        frac_ice=max(0.0,min(0.9999,thice/thpor_eff))
 	endif
c 
c	Gray's impedance proposal
c
c     fice=fzero*exp(-beta*frac_ice)
c
c     our implementation

      fice=(1.0-frac_ice)**(iceflg)

	return

	end	
C
c******************************************************************************

	real function fhsat(thliq,thice,thpor,thlmin,iwfice)
c
c     calculates hydraulic curve effective saturation
c     corrects for ice content (ref:Gray,Toth,et al)
c     and for minimum water content
c
c     iwfice set to 1 to include ice

      integer iwfice
	real thliq,thice,thpor,thlmin,thliq_eff,thpor_eff,thpor_avail

c      thliq_eff = max(0.0,thliq-thlmin)
c      thpor_eff = max(0.0,thpor-thlmin)

      thliq_eff = thliq
      thpor_eff = thpor


      if(iwfice.eq.1.or.iwfice.eq.3)then
	  thpor_eff = max(thliq,thlmin,thpor_eff-thice)
	else
	  thpor_eff = max(thliq,thlmin,thpor_eff)
	endif

      if(thpor_eff .le. 1.0e-4)then
        if(thliq_eff .le. thpor_eff)then
	    fhsat = 0.0
	  else
	    fhsat = 1.0
	  endif
      else 
        fhsat = thliq_eff/thpor_eff
	endif
    
c     limit smallest value to prevent dispersion
      if(fhsat .lt. 1.0e-4)fhsat=1.0e-4

	return

	end	
C
c******************************************************************************

	real function frsat(thliq,thice,thpor,thlmin,iwfice)
c
c     calculates retention curve effective saturation
      integer iwfice
      real thliq,thice,thpor,thlmin,thliq_eff,thpor_eff,thpor_avail

c      thliq_eff = max(0.0,thliq-thlmin)
c      thpor_eff = max(0.0,thpor-thlmin)

      thliq_eff = thliq
      thpor_eff = thpor

      if(iwfice.eq.1.or.iwfice.eq.3)then
	  thpor_eff = max(thliq,thlmin,thpor_eff-thice)
	else
	  thpor_eff = max(thliq,thlmin,thpor_eff)
	endif

      if(thpor_eff .le. 1.0e-8)then
        if(thliq_eff .le. thpor_eff)then
	    frsat = 0.0
	  else
	    frsat = 1.0
	  endif
      else 
        frsat = thliq_eff/thpor_eff
	endif

c     limit smallest value to prevent dispersion
      if(frsat .lt. 1.0e-4)frsat=1.0e-4
	      
	return

	end	
c
c******************************************************************************

