!>\file
C!               Canadian Terrestrial Ecosystem Model (CTEM) 
C!                    Mapping For Competition Subroutine
!!
      subroutine competition_map(nml, ilmos, jlmos, grclarea,
     2                           faregat,  fcancmx,  nppveg,geremort,
     3                           intrmort,gleafmas,bleafmas,stemmass,
     4                           rootmass,litrmass,soilcmas,
     5                           pftexist,  lambda, bmasveg,burnvegf,
     6                           add2allo,      cc,      mm,  fcanmx,
     7                           vgbiomas,gavgltms,gavgscms,
     8                           ta,precip, netrad,   tcurm,srpcuryr,
     9                           dftcuryr,  tmonth,anpcpcur, anpecur, 
     1                            gdd5cur,surmncur,defmncur,srplscur,  
     2                           defctcur,  twarmm,  tcoldm,    gdd5, 
     3                            aridity,srplsmon,defctmon,anndefct,
     4                           annsrpls,  annpcp,
     &                           dry_season_length,
     5                           lucemcom,  lucltrin, lucsocin,
     6                           pfcancmx,  nfcancmx, pstemmass,
     7                           pgleafmass,
c    ------------------- inputs above this line ---------------------
     a                             netradrow,
c    ------------------- intermediate and to save above this line ---
     a                           fare_cmp,  nppveg_cmp,geremort_cmp,
     b                          intrmort_cmp,gleafmas_cmp,bleafmas_cmp,
     c                          stemmass_cmp,rootmass_cmp,litrmass_cmp,
     d                          soilcmas_cmp,pftexist_cmp,  lambda_cmp,
     e                         bmasveg_cmp,burnvegf_cmp,add2allo_cmp,
     f                                cc_cmp,      mm_cmp,  fcanmx_cmp,
     g                          vgbiomas_cmp, grclarea_cmp,
     h                          gavgltms_cmp,gavgscms_cmp,
     i                                ta_cmp,  precip_cmp,  netrad_cmp, 
     j                             tcurm_cmp,srpcuryr_cmp,dftcuryr_cmp,
     k                            tmonth_cmp,anpcpcur_cmp, anpecur_cmp, 
     l                           gdd5cur_cmp,surmncur_cmp,defmncur_cmp,
     m                          srplscur_cmp,defctcur_cmp,  twarmm_cmp, 
     n                            tcoldm_cmp,    gdd5_cmp, aridity_cmp,
     o                          srplsmon_cmp,defctmon_cmp,anndefct_cmp,
     p                          annsrpls_cmp,  annpcp_cmp,
     &                         dry_season_length_cmp,
     q                         lucemcom_cmp, lucltrin_cmp, lucsocin_cmp,
     r                           pfcancmx_cmp,  nfcancmx_cmp,
     s                        pstemmass_cmp,  pgleafmass_cmp )      
c    ------------------- outputs above this line----------------------   
c
c
c    23  Jul 2013 - Add in module for parameters
C    J. Melton

c    28 Aug 2012 - this subroutine prepares for the competition 
c    Y. Peng       calculation by mapping the only pft in each 
c                  mosaic to pft fractions in each grid cell
c
c                  input:        array(ilg,icc)
c                    | 
c                    | scattering      
c                    v
c                  intermediate: arrayrow(nlat,nmos,icc)
c                    | 
c                    | mapping
c                    v
c                  output:       array_cmp(nlat,icc)
c
c                  array_cmp: mapped array prepared for competition
c
c    -----------------------------------------------------------------
c
c    indices
c
c     nlat    - max. number of grid cells in the latitude circle, which
c               is prescribed in runclass36ctem.f
c     nmos    - max. number of mosaic tiles in each latitudinal grid cell, 
c               which is prescribed in runclass35ctem.f
c     ilg     - ilg=nlat*nmos
c     icc     - number of pfts for use by ctem, currently 10
c     ican      - number of pfts for use by class, currently 4
c
c

!>\param pstemmass test pstemmass doc
!!

      use ctem_params,        only : nlat,nmos,icc, ilg, ican,iccp1

      implicit none
c

      integer i,m,l,k,mn
      integer nml        !<\param nml total number of mosaic tiles with pft fractions larger than 1, see gatprep.f
      integer ilmos(ilg) !<indices for scattering, see gatprep.f
      integer jlmos(ilg) !<indices for scattering, see gatprep.f
c
c--------input arrays for mapping------------------------------------------
c
      real fcancmx(ilg,icc)   !<fractional coverage of ctem's 10 pfts in each mosaic
      real faregat(ilg)       !<fractional coverage of each ctem pft in each mosaic tile
      real grclarea(ilg)      !<area of the grid cell, \f$km^2\f$
      real nppveg(ilg,icc)    !<npp for each pft type /m2 of vegetated area [u-mol co2-c/m2.sec]
      real geremort(ilg,icc)  !<growth related mortality (1/day)
      real intrmort(ilg,icc)  !<intrinsic (age related) mortality (1/day)
      real gleafmas(ilg,icc)  !<green leaf mass for each of the 10 ctem pfts, \f$kg c/m^2\f$
      real bleafmas(ilg,icc)  !<brown leaf mass for each of the 10 ctem pfts, \f$kg c/m^2\f$
      real stemmass(ilg,icc)  !<stem mass for each of the 10 ctem pfts, \f$kg c/m^2\f$
      real rootmass(ilg,icc)  !<root mass for each of the 10 ctem pfts, \f$kg c/m^2\f$
      real litrmass(ilg,iccp1)!<litter mass for each of the 10 ctem pfts + bare, \f$kg c/m^2\f$
      real soilcmas(ilg,iccp1)!<soil carbon mass for each of the 10 ctem pfts + bare, \f$kg c/m^2\f$
      real lambda(ilg,icc)    !<fraction of npp that is used for horizontal expansion
      real todfrac(ilg,icc)   !<max. fractional coverage of ctem's 9 pfts by the end of the day,
                              !<for use by land use subroutine
      real bmasveg(ilg,icc)   !<total (gleaf + stem + root) biomass for each ctem pft, \f$kg c/m^2\f$
      real burnvegf(ilg,icc)  !<fractional areas burned, for ctem pfts
      real add2allo(ilg,icc)  !<npp \f$kg c/m^2\f$.day that is used for expansion and subsequently allocated
                              !<to leaves, stem, and root via  the allocation part of the model.
      real cc(ilg,icc)        !<colonization rate & mortality rate
      real mm(ilg,icc)        !<colonization rate & mortality rate
      real fcanmx(ilg,ican)   !<fractional coverage of class' 4 pfts
      real vgbiomas(ilg)      !<grid averaged vegetation biomass, \f$kg c/m^2\f$
      real gavgltms(ilg)      !<grid averaged litter mass, \f$kg c/m^2\f$
      real gavgscms(ilg)      !<grid averaged soil c mass, \f$kg c/m^2\f$
      real lucemcom(ilg)      !<land use change (luc) related combustion emission losses, u-mol co2/m2.sec 
      real lucltrin(ilg)      !<luc related inputs to litter pool, u-mol co2/m2.sec
      real lucsocin(ilg)      !<luc related inputs to soil c pool, u-mol co2/m2.sec
      real pfcancmx(ilg,icc)  !<previous year's fractional coverages of pfts
      real nfcancmx(ilg,icc)  !<next year's fractional coverages of pfts
      real pstemmass(ilg,icc)
      real pgleafmass(ilg,icc)
c
      logical pftexist(ilg,icc) !<logical array indicating pfts exist (t) or not (f)
c
      real ta(ilg)       !<mean daily temperature, k
      real precip(ilg)   !<daily precipitation (mm/day)
      real netrad(ilg)   !<daily net radiation (w/m2)
      real tcurm(ilg)    !<temperature of the current month (c)
      real srpcuryr(ilg) !<water surplus for the current year
      real dftcuryr(ilg) !<water deficit for the current year
      real tmonth(12,ilg)!<monthly temperatures
      real anpcpcur(ilg) !<annual precipitation for current year (mm)
      real anpecur(ilg)  !<annual potential evaporation for current year (mm)
      real gdd5cur(ilg)  !<growing degree days above 5 c for current year
      real surmncur(ilg) !<number of months with surplus water for current year
      real defmncur(ilg) !<number of months with water deficit for current year
      real srplscur(ilg) !<water surplus for the current month
      real defctcur(ilg) !<water deficit for the current month
      real twarmm(ilg)   !<temperature of the warmest month (c)
      real tcoldm(ilg)   !<temperature of the coldest month (c)
      real gdd5(ilg)     !<growing degree days above 5 c
      real aridity(ilg)  !<aridity index, ratio of potential evaporation to precipitation
      real srplsmon(ilg) !<number of months in a year with surplus water i.e. precipitation more than potential evaporation
      real defctmon(ilg) !<number of months in a year with water deficit i.e. precipitation less than potential evaporation
      real anndefct(ilg) !<annual water deficit (mm)
      real annsrpls(ilg) !<annual water surplus (mm)
      real annpcp(ilg)   !<annual precipitation (mm)
      real dry_season_length(ilg) !<length of dry season (months)
 
c
c--------intermediate arrays for mapping-----------------------------------
c
      real  fcancmxrow(nlat,nmos,icc),   nppvegrow(nlat,nmos,icc),
     1      geremortrow(nlat,nmos,icc),  intrmortrow(nlat,nmos,icc),
     2      gleafmasrow(nlat,nmos,icc),  bleafmasrow(nlat,nmos,icc),
     3      stemmassrow(nlat,nmos,icc),  rootmassrow(nlat,nmos,icc),
     4      litrmassrow(nlat,nmos,iccp1),soilcmasrow(nlat,nmos,iccp1),
     5      lambdarow(nlat,nmos,icc),    todfracrow(nlat,nmos,icc),  
     6      bmasvegrow(nlat,nmos,icc),   burnvegfrow(nlat,nmos,icc),
     7      add2allorow(nlat,nmos,icc),  ccrow(nlat,nmos,icc),
     8      mmrow(nlat,nmos,icc),        fcanmxrow(nlat,nmos,ican),
     9      farerow(nlat,nmos),          grclarearow(nlat,nmos),
     1      vgbiomasrow(nlat,nmos),    
     2      gavgltmsrow(nlat,nmos),      gavgscmsrow(nlat,nmos),
     3      lucemcomrow(nlat,nmos),      lucltrinrow(nlat,nmos),
     4      lucsocinrow(nlat,nmos),
     5      pfcancmxrow(nlat,nmos,icc), nfcancmxrow(nlat,nmos,icc),
     6      pstemmassrow(nlat,nmos,icc), pgleafmassrow(nlat,nmos,icc)
c
      logical pftexistrow(nlat,nmos,icc)
c
c--------these intermediate arrays will be saved for unmapping------------\\
c
      real  netradrow(nlat,nmos)
c
c-------------------------------------------------------------------------//
c
      real  tarow(nlat,nmos),       preciprow(nlat,nmos),
     1      tcurmrow(nlat,nmos),
     2      srpcuryrrow(nlat,nmos), dftcuryrrow(nlat,nmos),
     3      tmonthrow(12,nlat,nmos),anpcpcurrow(nlat,nmos), 
     4      anpecurrow(nlat,nmos),  gdd5currow(nlat,nmos), 
     5      surmncurrow(nlat,nmos), defmncurrow(nlat,nmos),
     6      srplscurrow(nlat,nmos), defctcurrow(nlat,nmos), 
     7      twarmmrow(nlat,nmos),   tcoldmrow(nlat,nmos),  
     8      gdd5row(nlat,nmos),     aridityrow(nlat,nmos),
     9      srplsmonrow(nlat,nmos), defctmonrow(nlat,nmos), 
     1      anndefctrow(nlat,nmos), annsrplsrow(nlat,nmos),
     2      annpcprow(nlat,nmos),
     3      dry_season_lengthrow(nlat,nmos) 
c
c--------output arrays after mapping---------------------------------------
c
      real fare_cmp(nlat,icc)      !<fractional coverage of ctem's 10 pfts in each latitudinal grid cell
      real nppveg_cmp(nlat,icc)    !<npp for each pft type of vegetated area in each latitudinal grid cell
      real geremort_cmp(nlat,icc)  !<growth related mortality in each latitudinal grid cell
      real intrmort_cmp(nlat,icc)  !<intrinsic (age related) mortality in each latitudinal grid cell
      real gleafmas_cmp(nlat,icc)  !<green leaf mass for each of the 10 ctem pfts in each latitudinal grid cell
      real bleafmas_cmp(nlat,icc)  !<brown leaf mass for each of the 10 ctem pfts in each latitudinal grid cell
      real stemmass_cmp(nlat,icc)  !<stem mass for each of the 10 ctem pfts in each latitudinal grid cell
      real rootmass_cmp(nlat,icc)  !<root mass for each of the 10 ctem pfts in each latitudinal grid cell
      real litrmass_cmp(nlat,iccp1)!<litter mass for each of the 10 ctem pfts + bare, in each latitudinal grid cell
      real soilcmas_cmp(nlat,iccp1)!<soil carbon mass for each of the 10 ctem pfts + bare, in each latitudinal grid cell
      real lambda_cmp(nlat,icc)    !<fraction of npp that is used for horizontal expansion in each latitudinal grid cell
      real todfrac_cmp(nlat,icc)   !<max. fractional coverage of ctem's 9 pfts by the end of the dayin
                                   !<each latitudinal grid cell, for use by land use subroutine
      real bmasveg_cmp(nlat,icc)   !<total (gleaf + stem + root) biomass for each ctem pft, \f$kg c/m^2\f$ in each latitudinal grid cell
      real burnvegf_cmp(nlat,icc)  !<fractional areas burned, for ctem pfts in each latitudinal grid cell
      real add2allo_cmp(nlat,icc)  !<npp \f$kg c/m^2\f$.day in each latitudinal grid cell that is used for expansion
                                   !<and subsequently allocated to leaves, stem, and root via the allocation part of the model.
      real cc_cmp(nlat,icc)        !<colonization rate & mortality rate in each latitudinal grid cell
      real mm_cmp(nlat,icc)        !<colonization rate & mortality rate in each latitudinal grid cell
      real fcanmx_cmp(nlat,ican)   !<fractional coverage of class' 4 pfts in each latitudinal grid cell
      real grclarea_cmp(nlat)      !<area of the grid cell, \f$km^2\f$
      real vgbiomas_cmp(nlat)      !<grid averaged vegetation biomass, \f$kg c/m^2\f$
      real gavgltms_cmp(nlat)      !<grid averaged litter mass, \f$kg c/m^2\f$
      real gavgscms_cmp(nlat)      !<grid averaged soil c mass, \f$kg c/m^2\f$
      real lucemcom_cmp(nlat)      !<land use change (luc) related combustion emission losses
                                   !<in each latitudional grid cell, u-mol co2/m2.sec
      real lucltrin_cmp(nlat)      !<luc related inputs to litter pool, in each latitudional 
                                   !<grid cell, u-mol co2/m2.sec
      real lucsocin_cmp(nlat)      !<luc related inputs to soil c pool, in each latitudional 
                                   !<grid cell, u-mol co2/m2.sec
      real pfcancmx_cmp(nlat,icc)  !<previous year's fractional coverages of pfts in each latitudinal grid cell
      real nfcancmx_cmp(nlat,icc)  !<next year's fractional coverages of pfts in each latitudinal grid cell
      real pstemmass_cmp(nlat,icc) !<stem mass from previous timestep, is value before fire. used by burntobare subroutine
      real pgleafmass_cmp(nlat,icc)!<root mass from previous timestep, is value before fire. used by burntobare subroutine

      logical pftexist_cmp(nlat,icc) !<logical array indicating pfts exist (t) or not (f) in each latitudinal grid cell 

      real ta_cmp(nlat)       !<mean daily temperature (k) in each latitudinal grid cell
      real precip_cmp(nlat)   !<daily precipitation (mm/day) in each latitudinal grid cell
      real netrad_cmp(nlat)   !<daily net radiation \f$(w/m^2)\f$ in each latitudinal grid cell
      real tcurm_cmp(nlat)    !<temperature of the current month (c) in each latitudinal grid cell
      real srpcuryr_cmp(nlat) !<water surplus for the current year in each latitudinal grid cell
      real dftcuryr_cmp(nlat) !<water deficit for the current year in each latitudinal grid cell
      real tmonth_cmp(12,nlat)!<monthly temperatures in each latitudinal grid cell
      real anpcpcur_cmp(nlat) !<annual precipitation for current year (mm) in each latitudinal grid cell
      real anpecur_cmp(nlat)  !<annual potential evaporation for current year (mm) in each latitudinal grid cell
      real gdd5cur_cmp(nlat)  !<growing degree days above 5 c for current year in each latitudinal grid cell
      real surmncur_cmp(nlat) !<number of months with surplus water for current year in each latitudinal grid cell
      real defmncur_cmp(nlat) !<number of months with water deficit for current year in each latitudinal grid cell
      real srplscur_cmp(nlat) !<water surplus for the current month in each latitudinal grid cell
      real defctcur_cmp(nlat) !<water deficit for the current month in each latitudinal grid cell
      real twarmm_cmp(nlat)   !<temperature of the warmest month (c) in each latitudinal grid cell
      real tcoldm_cmp(nlat)   !<temperature of the coldest month (c) in each latitudinal grid cell
      real gdd5_cmp(nlat)     !<growing degree days above 5 c in each latitudinal grid cell
      real aridity_cmp(nlat)  !<aridity index, ratio of potential evaporation to precipitation in
                              !<each latitudinal grid cell
      real srplsmon_cmp(nlat) !<number of months in a year with surplus water i.e. precipitation more
                              !<than potential evaporation in each latitudinal grid cell
      real defctmon_cmp(nlat) !<number of months in a year with water deficit i.e. precipitation less
                              !<than potential evaporation in each latitudinal grid cell
      real anndefct_cmp(nlat) !<annual water deficit (mm) in each latitudinal grid cell
      real annsrpls_cmp(nlat) !<annual water surplus (mm) in each latitudinal grid cell
      real annpcp_cmp(nlat)   !<annual precipitation (mm) in each latitudinal grid cell
      real dry_season_length_cmp(nlat) !<length of dry season (months) in each latitudinal grid cell
!>
!!------------------------------------------------------------------
!!parameters used 
!!
!!note the structure of parameter vectors which clearly shows the
!!class pfts (along rows) and ctem sub-pfts (along columns)
!!
!!\f[
!!\begin{tabular} { | l | c | c | c | }
!!\hline
!!needle leaf &  evg1 &    evg2 &      dcd \\ \hline
!!broad leaf  &  evg  & dcd-cld &  dcd-dry \\ \hline
!!crops       &   c3  &      c4 &      --- \\ \hline
!!grasses     &   c3  &      c4 &      --- \\ \hline
!!\end{tabular}
!!\f]
!!---------------------------------------------------------------
!!
      if(icc.ne.9)                    call xit('compete_unmap',-1)
      if(ican.ne.4)                     call xit('compete_unmap',-2)
c
c     initialization
c
      do 90 i = 1, nlat
       do 91 m = 1, nmos
c
        do l=1,icc
         fcancmxrow(i,m,l) = 0.0
         nppvegrow(i,m,l)  = 0.0
         geremortrow(i,m,l)= 0.0
         intrmortrow(i,m,l)= 0.0
         gleafmasrow(i,m,l)= 0.0
         bleafmasrow(i,m,l)= 0.0
         stemmassrow(i,m,l)= 0.0
         rootmassrow(i,m,l)= 0.0
         pftexistrow(i,m,l)= .false.
         lambdarow(i,m,l)  = 0.0
         todfracrow(i,m,l) = 0.0
         pfcancmxrow(i,m,l)= 0.0
         nfcancmxrow(i,m,l)= 0.0
         pstemmassrow(i,m,l)= 0.0
         pgleafmassrow(i,m,l)= 0.0
         bmasvegrow(i,m,l) = 0.0
         burnvegfrow(i,m,l) = 0.0
         add2allorow(i,m,l)= 0.0
         ccrow(i,m,l)      = 0.0
         mmrow(i,m,l)      = 0.0
        enddo       
c
        do l=1,iccp1
         litrmassrow(i,m,l)= 0.0
         soilcmasrow(i,m,l)= 0.0
        enddo
c
        do l=1,ican
         fcanmxrow(i,m,l)  = 0.0            
        enddo
c
         farerow(i,m)      = 0.0
         vgbiomasrow(i,m)  = 0.0
         gavgltmsrow(i,m)  = 0.0
         gavgscmsrow(i,m)  = 0.0 
         lucemcomrow(i,m)  = 0.0
         lucltrinrow(i,m)  = 0.0
         lucsocinrow(i,m)  = 0.0 
         tarow(i,m)        = 0.0  
         preciprow(i,m)    = 0.0  
         netradrow(i,m)    = 0.0  
         tcurmrow(i,m)     = 0.0  
         srpcuryrrow(i,m)  = 0.0  
         dftcuryrrow(i,m)  = 0.0  
c
         do mn=1,12
          tmonthrow(mn,i,m) = 0.0
         enddo
c
         anpcpcurrow(i,m)  = 0.0  
         anpecurrow(i,m)   = 0.0  
         gdd5currow(i,m)   = 0.0  
         surmncurrow(i,m)  = 0.0  
         defmncurrow(i,m)  = 0.0  
         srplscurrow(i,m)  = 0.0    
         defctcurrow(i,m)  = 0.0  
         twarmmrow(i,m)    = 0.0  
         tcoldmrow(i,m)    = 0.0  
         gdd5row(i,m)      = 0.0   
         aridityrow(i,m)   = 0.0  
         srplsmonrow(i,m)  = 0.0  
         defctmonrow(i,m)  = 0.0  
         anndefctrow(i,m)  = 0.0  
         annsrplsrow(i,m)  = 0.0  
         annpcprow(i,m)    = 0.0  
         dry_season_lengthrow(i,m) = 0.0
91     continue
c
       do l=1,icc
        fare_cmp(i,l) = 0.0
        nppveg_cmp(i,l)  = 0.0
        geremort_cmp(i,l)= 0.0
        intrmort_cmp(i,l)= 0.0
        gleafmas_cmp(i,l)= 0.0
        bleafmas_cmp(i,l)= 0.0
        stemmass_cmp(i,l)= 0.0
        rootmass_cmp(i,l)= 0.0
        pftexist_cmp(i,l)= .false.
        lambda_cmp(i,l)  = 0.0
        todfrac_cmp(i,l) = 0.0
        pfcancmx_cmp(i,l)= 0.0
        nfcancmx_cmp(i,l)= 0.0
        pstemmass_cmp(i,l)= 0.0
        pgleafmass_cmp(i,l)= 0.0
        bmasveg_cmp(i,l) = 0.0
        burnvegf_cmp(i,l) = 0.0
        add2allo_cmp(i,l)= 0.0
        cc_cmp(i,l)      = 0.0
        mm_cmp(i,l)      = 0.0
       enddo
c
       do l=1,iccp1
        litrmass_cmp(i,l)= 0.0
        soilcmas_cmp(i,l)= 0.0
       enddo
c
       do l=1,ican
        fcanmx_cmp(i,l)  = 0.0  
       enddo
c
        vgbiomas_cmp(i)  = 0.0
        gavgltms_cmp(i)  = 0.0
        gavgscms_cmp(i)  = 0.0 
        lucemcom_cmp(i)  = 0.0 
        lucltrin_cmp(i)  = 0.0
        lucsocin_cmp(i)  = 0.0
        ta_cmp(i)        = 0.0  
        precip_cmp(i)    = 0.0  
        netrad_cmp(i)    = 0.0  
        tcurm_cmp(i)     = 0.0  
        srpcuryr_cmp(i)  = 0.0  
        dftcuryr_cmp(i)  = 0.0  
c
        do mn=1,12
         tmonth_cmp(mn,i) = 0.0
        enddo
c
        anpcpcur_cmp(i)  = 0.0  
        anpecur_cmp(i)   = 0.0  
        gdd5cur_cmp(i)   = 0.0  
        surmncur_cmp(i)  = 0.0  
        defmncur_cmp(i)  = 0.0  
        srplscur_cmp(i)  = 0.0    
        defctcur_cmp(i)  = 0.0  
        twarmm_cmp(i)    = 0.0  
        tcoldm_cmp(i)    = 0.0  
        gdd5_cmp(i)      = 0.0   
        aridity_cmp(i)   = 0.0  
        srplsmon_cmp(i)  = 0.0  
        defctmon_cmp(i)  = 0.0  
        anndefct_cmp(i)  = 0.0  
        annsrpls_cmp(i)  = 0.0  
        annpcp_cmp(i)    = 0.0  
        dry_season_length_cmp(i) = 0.0  
        grclarea_cmp(i)  = 0.0
90    continue 
!>
!!scattering the pft index in each mosaic (fcancmx) to 
!!pft index in each mosaic of each grid cell (fcancmxrow)
!!nml, ilmos and jlmos are referring to gatprep.f
!!
      do 100 l=1,icc
       do 100 k=1,nml
         fcancmxrow(ilmos(k),jlmos(k),l)  = fcancmx(k,l)
         nppvegrow(ilmos(k),jlmos(k),l)   = nppveg(k,l)
         geremortrow(ilmos(k),jlmos(k),l) = geremort(k,l)
         intrmortrow(ilmos(k),jlmos(k),l) = intrmort(k,l)
         gleafmasrow(ilmos(k),jlmos(k),l) = gleafmas(k,l)
         bleafmasrow(ilmos(k),jlmos(k),l) = bleafmas(k,l)
         stemmassrow(ilmos(k),jlmos(k),l) = stemmass(k,l)
         rootmassrow(ilmos(k),jlmos(k),l) = rootmass(k,l)
         pftexistrow(ilmos(k),jlmos(k),l) = pftexist(k,l)
         lambdarow(ilmos(k),jlmos(k),l)   = lambda(k,l)
         todfracrow(ilmos(k),jlmos(k),l)  = todfrac(k,l)
         pfcancmxrow(ilmos(k),jlmos(k),l) = pfcancmx(k,l)
         nfcancmxrow(ilmos(k),jlmos(k),l) = nfcancmx(k,l)
         pstemmassrow(ilmos(k),jlmos(k),l) = pstemmass(k,l)
         pgleafmassrow(ilmos(k),jlmos(k),l) = pgleafmass(k,l)
         bmasvegrow(ilmos(k),jlmos(k),l)  = bmasveg(k,l)
         burnvegfrow(ilmos(k),jlmos(k),l)  = burnvegf(k,l)
         add2allorow(ilmos(k),jlmos(k),l) = add2allo(k,l)
         ccrow(ilmos(k),jlmos(k),l)       = cc(k,l)
         mmrow(ilmos(k),jlmos(k),l)       = mm(k,l)   
 100  continue
c
      do 110 l=1,iccp1
       do 110 k=1,nml
         litrmassrow(ilmos(k),jlmos(k),l) = litrmass(k,l)
         soilcmasrow(ilmos(k),jlmos(k),l) = soilcmas(k,l)
 110  continue
c
      do 120 l=1,ican
       do 120 k=1,nml
         fcanmxrow(ilmos(k),jlmos(k),l) = fcanmx(k,l)
 120  continue
c
      do 130 k=1,nml
         farerow(ilmos(k),jlmos(k))     = faregat(k)
         vgbiomasrow(ilmos(k),jlmos(k)) = vgbiomas(k)
         gavgltmsrow(ilmos(k),jlmos(k)) = gavgltms(k)
         gavgscmsrow(ilmos(k),jlmos(k)) = gavgscms(k) 
         lucemcomrow(ilmos(k),jlmos(k)) = lucemcom(k)
         lucltrinrow(ilmos(k),jlmos(k)) = lucltrin(k) 
         lucsocinrow(ilmos(k),jlmos(k)) = lucsocin(k)  
         tarow(ilmos(k),jlmos(k))       = ta(k)
         preciprow(ilmos(k),jlmos(k))   = precip(k)
         netradrow(ilmos(k),jlmos(k))   = netrad(k)
         tcurmrow(ilmos(k),jlmos(k))    = tcurm(k)
         srpcuryrrow(ilmos(k),jlmos(k)) = srpcuryr(k)
         dftcuryrrow(ilmos(k),jlmos(k)) = dftcuryr(k)
         grclarearow(ilmos(k),jlmos(k)) = grclarea(k)

         do mn=1,12
          tmonthrow(mn,ilmos(k),jlmos(k)) = tmonth(mn,k)
         enddo
c
         anpcpcurrow(ilmos(k),jlmos(k)) = anpcpcur(k)
         anpecurrow(ilmos(k),jlmos(k))  = anpecur(k)  
         gdd5currow(ilmos(k),jlmos(k))  = gdd5cur(k)  
         surmncurrow(ilmos(k),jlmos(k)) = surmncur(k)  
         defmncurrow(ilmos(k),jlmos(k)) = defmncur(k)  
         srplscurrow(ilmos(k),jlmos(k)) = srplscur(k)    
         defctcurrow(ilmos(k),jlmos(k)) = defctcur(k)  
         twarmmrow(ilmos(k),jlmos(k))   = twarmm(k) 
         tcoldmrow(ilmos(k),jlmos(k))   = tcoldm(k)  
         gdd5row(ilmos(k),jlmos(k))     = gdd5(k)  
         aridityrow(ilmos(k),jlmos(k))  = aridity(k)  
         srplsmonrow(ilmos(k),jlmos(k)) = srplsmon(k)  
         defctmonrow(ilmos(k),jlmos(k)) = defctmon(k)  
         anndefctrow(ilmos(k),jlmos(k)) = anndefct(k) 
         annsrplsrow(ilmos(k),jlmos(k)) = annsrpls(k)   
         annpcprow(ilmos(k),jlmos(k))   = annpcp(k) 
         dry_season_lengthrow(ilmos(k),jlmos(k)) = dry_season_length(k)
 130  continue
!>
!!mapping the pft areal fraction in each mosaic of each
!!grid cell (farerow) to pft fraction in each grid cell (fare_cmp)
!!
      do 200 i=1,nlat
      do 210 m=1,nmos
c
       do l=1,icc
        if (fcancmxrow(i,m,l) .eq. 1.0) then
         fare_cmp(i,l)    = farerow(i,m)
         nppveg_cmp(i,l)  = nppvegrow(i,m,l)
         geremort_cmp(i,l)= geremortrow(i,m,l)
         intrmort_cmp(i,l)= intrmortrow(i,m,l)
         gleafmas_cmp(i,l)= gleafmasrow(i,m,l)
         bleafmas_cmp(i,l)= bleafmasrow(i,m,l)
         stemmass_cmp(i,l)= stemmassrow(i,m,l)
         rootmass_cmp(i,l)= rootmassrow(i,m,l)
         pftexist_cmp(i,l)= pftexistrow(i,m,l)
         lambda_cmp(i,l)  = lambdarow(i,m,l)
         todfrac_cmp(i,l) = todfracrow(i,m,l)
         pfcancmx_cmp(i,l)= pfcancmxrow(i,m,l)
         nfcancmx_cmp(i,l)= nfcancmxrow(i,m,l)
         pstemmass_cmp(i,l)= pstemmassrow(i,m,l)
         pgleafmass_cmp(i,l)= pgleafmassrow(i,m,l)
         bmasveg_cmp(i,l) = bmasvegrow(i,m,l)
         burnvegf_cmp(i,l) = burnvegfrow(i,m,l)
         add2allo_cmp(i,l)= add2allorow(i,m,l)
         cc_cmp(i,l)      = ccrow(i,m,l)
         mm_cmp(i,l)      = mmrow(i,m,l)

        endif
       enddo
c
       do l=1,iccp1
        if (litrmassrow(i,m,l) .gt. 0.) then
         litrmass_cmp(i,l) = litrmassrow(i,m,l)
         soilcmas_cmp(i,l) = soilcmasrow(i,m,l)
        endif
       enddo 
c
       do l=1,ican
        if (fcanmxrow(i,m,l) .eq. 1.0) then
         fcanmx_cmp(i,l)  = fcanmxrow(i,m,l)
        endif
       enddo     
c
210   continue
c
       do m=1,nmos                                    
        vgbiomas_cmp(i) = vgbiomas_cmp(i)+vgbiomasrow(i,m)*farerow(i,m)
        gavgltms_cmp(i) = gavgltms_cmp(i)+gavgltmsrow(i,m)*farerow(i,m)
        gavgscms_cmp(i) = gavgscms_cmp(i)+gavgscmsrow(i,m)*farerow(i,m) 
        lucemcom_cmp(i) = lucemcom_cmp(i)+lucemcomrow(i,m)*farerow(i,m)
        lucltrin_cmp(i) = lucltrin_cmp(i)+lucltrinrow(i,m)*farerow(i,m)
        lucsocin_cmp(i) = lucsocin_cmp(i)+lucsocinrow(i,m)*farerow(i,m)

        ta_cmp(i)       = tarow(i,m)
        precip_cmp(i)   = preciprow(i,m)
        netrad_cmp(i)   = netrad_cmp(i)+netradrow(i,m)*farerow(i,m)
        tcurm_cmp(i)    = tcurmrow(i,m)
        srpcuryr_cmp(i) = srpcuryrrow(i,m)
        dftcuryr_cmp(i) = dftcuryrrow(i,m)
c
        do mn=1,12
         tmonth_cmp(mn,i) = tmonthrow(mn,i,m)
        enddo
c
        anpcpcur_cmp(i) = anpcpcurrow(i,m)
        anpecur_cmp(i)  = anpecurrow(i,m)  
        gdd5cur_cmp(i)  = gdd5currow(i,m)  
        surmncur_cmp(i) = surmncurrow(i,m)  
        defmncur_cmp(i) = defmncurrow(i,m)  
        srplscur_cmp(i) = srplscurrow(i,m)    
        defctcur_cmp(i) = defctcurrow(i,m)  
        twarmm_cmp(i)   = twarmmrow(i,m) 
        tcoldm_cmp(i)   = tcoldmrow(i,m)  
        gdd5_cmp(i)     = gdd5row(i,m)  
        aridity_cmp(i)  = aridityrow(i,m)  
        srplsmon_cmp(i) = srplsmonrow(i,m)  
        defctmon_cmp(i) = defctmonrow(i,m)  
        anndefct_cmp(i) = anndefctrow(i,m) 
        annsrpls_cmp(i) = annsrplsrow(i,m)   
        annpcp_cmp(i)   = annpcprow(i,m) 
        dry_season_length_cmp(i) = dry_season_lengthrow(i,m)
        grclarea_cmp(i) = grclarearow(i,m)
       enddo 
c
200   continue
c
      return
      end

