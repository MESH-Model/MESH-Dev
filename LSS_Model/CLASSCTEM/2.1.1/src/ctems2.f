!>\file
!!              Canadian Terrestrial Ecosystem Model (CTEM)
!!
      subroutine ctems2 (fcancmxrow,rmatcrow,zolncrow,paicrow,
     1      ailcrow,     ailcgrow,    cmasvegcrow,  slaicrow,
     2      ailcgsrow,   fcancsrow,   fcancrow,     rmatctemrow,
     3      co2concrow,  co2i1cgrow,  co2i1csrow,   co2i2cgrow,
     4      co2i2csrow,  xdiffus,     slairow,      cfluxcgrow,
     5      cfluxcsrow,  ancsvegrow,  ancgvegrow,   rmlcsvegrow,
     6      rmlcgvegrow, canresrow,   sdeprow,      ch4concrow,
     7      sandrow,     clayrow,     orgmrow,
     8      anvegrow,    rmlvegrow,   tcanoaccrow_m,tbaraccrow_m,
     9      uvaccrow_m,  vvaccrow_m,  prbfrhucgrd,
     a      extnprobgrd, pfcancmxrow,  nfcancmxrow,
     b      stemmassrow, rootmassrow, litrmassrow,  gleafmasrow,
     c      bleafmasrow, soilcmasrow, ailcbrow,     flhrlossrow,
     d      pandaysrow,  lfstatusrow, grwtheffrow,  lystmmasrow,
     e      lyrotmasrow, tymaxlairow, vgbiomasrow,  gavgltmsrow,
     f      stmhrlosrow, bmasvegrow,  colddaysrow,  rothrlosrow,
     g      alvsctmrow,  alirctmrow,  gavglairow,   npprow,
     h      neprow,      hetroresrow, autoresrow,   soilresprow,
     i      rmrow,       rgrow,       nbprow,       litresrow,
     j      socresrow,   gpprow,      dstcemlsrow,  litrfallrow,
     k      humiftrsrow, veghghtrow,  rootdpthrow,  rmlrow,
     1      litrfallvegrow, humiftrsvegrow,
     l      rmsrow,      rmrrow,      tltrleafrow,  tltrstemrow,
     m      tltrrootrow, leaflitrrow, roottemprow,  afrleafrow,
     n      afrstemrow,  afrrootrow,  wtstatusrow,  ltstatusrow,
     o      burnfracrow, smfuncvegrow, lucemcomrow,  lucltrinrow,
     p      lucsocinrow, nppvegrow,   dstcemls3row,
     q      farerow,     gavgscmsrow, tcanoaccrow_out,
     &      rmlvegaccrow, rmsvegrow,  rmrvegrow,    rgvegrow,
     &      vgbiomas_vegrow,gppvegrow,nepvegrow,ailcminrow,ailcmaxrow,
     &      fcanrow,      pftexistrow,
     &      emit_co2row,  emit_corow, emit_ch4row,  emit_nmhcrow,
     &      emit_h2row,   emit_noxrow,emit_n2orow,  emit_pm25row,
     &      emit_tpmrow,  emit_tcrow, emit_ocrow,   emit_bcrow,
     &      btermrow,     ltermrow,   mtermrow,
     &      nbpvegrow,   hetroresvegrow, autoresvegrow,litresvegrow,
     &      soilcresvegrow, burnvegfrow, pstemmassrow, pgleafmassrow,
     &      ch4wet1row, ch4wet2row, wetfdynrow, ch4dyn1row,
     &      ch4dyn2row,ch4soillsrow,
     &      twarmmrow,    tcoldmrow,     gdd5row,
     1      aridityrow, srplsmonrow,  defctmonrow, anndefctrow,
     2      annsrplsrow,   annpcprow,  dry_season_lengthrow,
c --
     r      ilmos,       jlmos,       iwmos,        jwmos,
     s      nml,   fcancmxgat,  rmatcgat,    zolncgat,     paicgat,
     v      ailcgat,     ailcggat,    cmasvegcgat,  slaicgat,
     w      ailcgsgat,   fcancsgat,   fcancgat,     rmatctemgat,
     x      co2concgat,  co2i1cggat,  co2i1csgat,   co2i2cggat, 
     y      co2i2csgat,  xdiffusgat,  slaigat,      cfluxcggat, 
     z      cfluxcsgat,  ancsveggat,  ancgveggat,   rmlcsveggat,
     1      rmlcgveggat, canresgat,   sdepgat,      ch4concgat,
     2      sandgat,     claygat,     orgmgat,
     3      anveggat,    rmlveggat,   tcanoaccgat_m,tbaraccgat_m,
     4      uvaccgat_m,  vvaccgat_m,  prbfrhucgat,
     5      extnprobgat, pfcancmxgat,  nfcancmxgat,
     6      stemmassgat, rootmassgat, litrmassgat,  gleafmasgat,
     7      bleafmasgat, soilcmasgat, ailcbgat,     flhrlossgat,
     8      pandaysgat,  lfstatusgat, grwtheffgat,  lystmmasgat,
     9      lyrotmasgat, tymaxlaigat, vgbiomasgat,  gavgltmsgat,
     a      stmhrlosgat, bmasveggat,  colddaysgat,  rothrlosgat,
     b      alvsctmgat,  alirctmgat,  gavglaigat,   nppgat,
     c      nepgat,      hetroresgat, autoresgat,   soilrespgat,
     d      rmgat,       rggat,       nbpgat,       litresgat,
     e      socresgat,   gppgat,      dstcemlsgat,  litrfallgat,
     f      humiftrsgat, veghghtgat,  rootdpthgat,  rmlgat,
     1      litrfallveggat, humiftrsveggat,
     g      rmsgat,      rmrgat,      tltrleafgat,  tltrstemgat,
     h      tltrrootgat, leaflitrgat, roottempgat,  afrleafgat,
     i      afrstemgat,  afrrootgat,  wtstatusgat,  ltstatusgat,
     j      burnfracgat, smfuncveggat, lucemcomgat,  lucltringat,
     k      lucsocingat, nppveggat,   dstcemls3gat,
     l      faregat,     gavgscmsgat, tcanoaccgat_out,
     &      rmlvegaccgat, rmsveggat,  rmrveggat,    rgveggat,
     &      vgbiomas_veggat,gppveggat,nepveggat, ailcmingat,ailcmaxgat,
     &      fcangat,      pftexistgat,
     &      emit_co2gat,  emit_cogat, emit_ch4gat,  emit_nmhcgat,
     &      emit_h2gat,   emit_noxgat,emit_n2ogat,  emit_pm25gat,
     &      emit_tpmgat,  emit_tcgat, emit_ocgat,   emit_bcgat,
     &      btermgat,     ltermgat,   mtermgat,
     &      nbpveggat, hetroresveggat, autoresveggat,litresveggat,
     &      soilcresveggat, burnvegfgat, pstemmassgat, pgleafmassgat,
     &      ch4wet1gat, ch4wet2gat, wetfdyngat, ch4dyn1gat,
     &      ch4dyn2gat, ch4soillsgat,
     &      twarmmgat,    tcoldmgat,     gdd5gat,
     1      ariditygat, srplsmongat,  defctmongat, anndefctgat,
     2      annsrplsgat,   annpcpgat,  dry_season_lengthgat)
c
C     July 12 2013    Bring in the ctem params use statement
c     J. Melton

c      August 4, 2009 scatter operation on CTEM variables.
c      Rong Li
c 
      use ctem_params,        only : nlat, nmos, ilg, ignd, ican, icp1,
     1                               icc,iccp1

      implicit none
c
c     integer constants.
c
      integer  nml, k, l, m
c
c
c     gather-scatter index arrays.
c
      integer  ilmos (ilg),  jlmos  (ilg),  iwmos  (ilg),  jwmos (ilg)
c
c
      real  fcancmxrow(nlat,nmos,icc),  rmatcrow(nlat,nmos,ican,ignd),
     1      zolncrow(nlat,nmos,ican),   paicrow(nlat,nmos,ican),
     2      ailcrow(nlat,nmos,ican),    ailcgrow(nlat,nmos,icc),
     3      cmasvegcrow(nlat,nmos,ican),slaicrow(nlat,nmos,ican),
     4      ailcgsrow(nlat,nmos,icc),   fcancsrow(nlat,nmos,icc),
     5      fcancrow(nlat,nmos,icc),    rmatctemrow(nlat,nmos,icc,ignd),
     6      co2concrow(nlat,nmos),      co2i1cgrow(nlat,nmos,icc),
     7      co2i1csrow(nlat,nmos,icc),  co2i2cgrow(nlat,nmos,icc),
     8      co2i2csrow(nlat,nmos,icc),  xdiffus(nlat),
     9      slairow(nlat,nmos,icc),     cfluxcgrow(nlat,nmos),
     a      cfluxcsrow(nlat,nmos),      ancsvegrow(nlat,nmos,icc),
     b      ancgvegrow(nlat,nmos,icc),  rmlcsvegrow(nlat,nmos,icc),
     c      rmlcgvegrow(nlat,nmos,icc), canresrow(nlat,nmos),
     d      sdeprow(nlat,nmos),         fcanrow(nlat,nmos,icp1),
     e      ch4concrow(nlat,nmos)
c
      real    sandrow(nlat,nmos,ignd),  clayrow(nlat,nmos,ignd), 
     1        orgmrow(nlat,nmos,ignd)
c
      real  anvegrow(nlat,nmos,icc),    rmlvegrow(nlat,nmos,icc)
c
      real  tcanoaccrow_m(nlat,nmos),
     1      uvaccrow_m(nlat,nmos),      vvaccrow_m(nlat,nmos)
c
      real  prbfrhucgrd(nlat),       extnprobgrd(nlat),
     1      tbaraccrow_m(nlat,nmos,ignd),
     2      pfcancmxrow(nlat,nmos,icc), nfcancmxrow(nlat,nmos,icc),
     3      stemmassrow(nlat,nmos,icc), rootmassrow(nlat,nmos,icc),
     &      pstemmassrow(nlat,nmos,icc), pgleafmassrow(nlat,nmos,icc),
     4      litrmassrow(nlat,nmos,iccp1),gleafmasrow(nlat,nmos,icc),
     5      bleafmasrow(nlat,nmos,icc), soilcmasrow(nlat,nmos,iccp1),
     6      ailcbrow(nlat,nmos,icc),    flhrlossrow(nlat,nmos,icc)
c
      integer  pandaysrow(nlat,nmos,icc), lfstatusrow(nlat,nmos,icc),
     1         colddaysrow(nlat,nmos,2)

      logical pftexistrow(nlat,nmos,icc)
c
      real  grwtheffrow(nlat,nmos,icc),  lystmmasrow(nlat,nmos,icc),
     9      lyrotmasrow(nlat,nmos,icc),  tymaxlairow(nlat,nmos,icc),
     a      vgbiomasrow(nlat,nmos),      gavgltmsrow(nlat,nmos),
     b      stmhrlosrow(nlat,nmos,icc),  bmasvegrow(nlat,nmos,icc),
     c      rothrlosrow(nlat,nmos,icc),
     d      alvsctmrow(nlat,nmos,ican),  alirctmrow(nlat,nmos,ican),
     e      gavglairow(nlat,nmos)
c
      real  npprow(nlat,nmos),            neprow(nlat,nmos),
     1      hetroresrow(nlat,nmos),       autoresrow(nlat,nmos),
     2      soilresprow(nlat,nmos),   rmrow(nlat,nmos),rgrow(nlat,nmos),
     3      nbprow(nlat,nmos),            litresrow(nlat,nmos),
     4      socresrow(nlat,nmos),         gpprow(nlat,nmos),
     5      dstcemlsrow(nlat,nmos),       litrfallrow(nlat,nmos),
     6      humiftrsrow(nlat,nmos),       veghghtrow(nlat,nmos,icc),
     7      litrfallvegrow(nlat,nmos,icc),
     &      humiftrsvegrow(nlat,nmos,iccp1),
     7      rootdpthrow(nlat,nmos,icc),   rmlrow(nlat,nmos),
     8      rmsrow(nlat,nmos),            rmrrow(nlat,nmos),
     9      tltrleafrow(nlat,nmos,icc),   tltrstemrow(nlat,nmos,icc),
     a      tltrrootrow(nlat,nmos,icc),   leaflitrrow(nlat,nmos,icc),
     b      roottemprow(nlat,nmos,icc),   afrleafrow(nlat,nmos,icc),
     c      afrstemrow(nlat,nmos,icc),    afrrootrow(nlat,nmos,icc),
     d      wtstatusrow(nlat,nmos,icc),   ltstatusrow(nlat,nmos,icc),
     e      burnfracrow(nlat,nmos),      smfuncvegrow(nlat,nmos,icc),
     f      lucemcomrow(nlat,nmos),       lucltrinrow(nlat,nmos),
     g      lucsocinrow(nlat,nmos),       nppvegrow(nlat,nmos,icc),
     h      dstcemls3row(nlat,nmos)
c
c     fire variables
c
      real emit_co2row(nlat,nmos,icc),    emit_corow(nlat,nmos,icc), 
     1     emit_ch4row(nlat,nmos,icc),    emit_nmhcrow(nlat,nmos,icc),
     2     emit_h2row(nlat,nmos,icc),     emit_noxrow(nlat,nmos,icc),
     3     emit_n2orow(nlat,nmos,icc),    emit_pm25row(nlat,nmos,icc),
     4     emit_tpmrow(nlat,nmos,icc),    emit_tcrow(nlat,nmos,icc),
     5     emit_ocrow(nlat,nmos,icc),     emit_bcrow(nlat,nmos,icc),
     6     burnvegfrow(nlat,nmos,icc),    btermrow(nlat,nmos,icc),
     7     ltermrow(nlat,nmos),           mtermrow(nlat,nmos,icc)

      real  farerow(nlat,nmos)
      real  gavgscmsrow(nlat,nmos)
      real  tcanoaccrow_out(nlat,nmos)
c
      real rmlvegaccrow(nlat,nmos,icc),  rmsvegrow(nlat,nmos,icc),
     1      rmrvegrow(nlat,nmos,icc),    rgvegrow(nlat,nmos,icc),
     2      ailcminrow(nlat,nmos,icc),   ailcmaxrow(nlat,nmos,icc)
c
      real vgbiomas_vegrow(nlat,nmos,icc)
c
      real gppvegrow(nlat,nmos,icc),    nepvegrow(nlat,nmos,iccp1),
     1      nbpvegrow(nlat,nmos,iccp1),hetroresvegrow(nlat,nmos,iccp1),
     2      autoresvegrow(nlat,nmos,icc),litresvegrow(nlat,nmos,iccp1),
     3      soilcresvegrow(nlat,nmos,iccp1)
c
      real  fcancmxgat(ilg,icc),        rmatcgat(ilg,ican,ignd),
     1      zolncgat(ilg,ican),         paicgat(ilg,ican),
     2      ailcgat(ilg,ican),          ailcggat(ilg,icc),
     3      cmasvegcgat(ilg,ican),      slaicgat(ilg,ican),
     4      ailcgsgat(ilg,icc),         fcancsgat(ilg,icc),
     5      fcancgat(ilg,icc),          rmatctemgat(ilg,icc,ignd),
     6      co2concgat(ilg),            co2i1cggat(ilg,icc),
     7      co2i1csgat(ilg,icc),        co2i2cggat(ilg,icc),
     8      co2i2csgat(ilg,icc),        xdiffusgat(ilg),
     9      slaigat(ilg,icc),           cfluxcggat(ilg),
     a      cfluxcsgat(ilg),            ancsveggat(ilg,icc),
     b      ancgveggat(ilg,icc),        rmlcsveggat(ilg,icc),
     c      rmlcgveggat(ilg,icc),       canresgat(ilg),
     d      sdepgat(ilg),               fcangat(ilg,icp1),
     e      ch4concgat(ilg)
c
      real    sandgat(ilg,ignd),        claygat(ilg,ignd), 
     1        orgmgat(ilg,ignd)
c
      real  anveggat(ilg,icc),          rmlveggat(ilg,icc)
c
      real  tcanoaccgat_m(ilg),         uvaccgat_m(ilg),
     1      vvaccgat_m(ilg)
c
      real  prbfrhucgat(ilg),           extnprobgat(ilg),
     1      tbaraccgat_m(ilg,ignd),
     2      pfcancmxgat(ilg,icc),       nfcancmxgat(ilg,icc),
     3      stemmassgat(ilg,icc),       rootmassgat(ilg,icc),  
     &      pstemmassgat(ilg,icc),      pgleafmassgat(ilg,icc),  
     4      litrmassgat(ilg,iccp1),     gleafmasgat(ilg,icc),
     5      bleafmasgat(ilg,icc),       soilcmasgat(ilg,iccp1),
     6      ailcbgat(ilg,icc),          flhrlossgat(ilg,icc)
c
      integer pandaysgat(ilg,icc),      lfstatusgat(ilg,icc),
     1        colddaysgat(ilg,2)

      logical pftexistgat(ilg,icc)
c
      real  grwtheffgat(ilg,icc),       lystmmasgat(ilg,icc),
     9      lyrotmasgat(ilg,icc),       tymaxlaigat(ilg,icc),
     a      vgbiomasgat(ilg),           gavgltmsgat(ilg),
     b      stmhrlosgat(ilg,icc),       bmasveggat(ilg,icc),
     c      rothrlosgat(ilg,icc),       alvsctmgat(ilg,ican),
     d      alirctmgat(ilg,ican),       gavglaigat(ilg)
c
      real  nppgat(ilg),               nepgat(ilg),
     1      hetroresgat(ilg),          autoresgat(ilg),
     2      soilrespgat(ilg), rmgat(ilg),    rggat(ilg),
     3      nbpgat(ilg),               litresgat(ilg),
     4      socresgat(ilg),            gppgat(ilg),
     5      dstcemlsgat(ilg),          litrfallgat(ilg),
     6      humiftrsgat(ilg),          veghghtgat(ilg,icc),
     &      litrfallveggat(ilg,icc), humiftrsveggat(ilg,iccp1),
     7      rootdpthgat(ilg,icc),      rmlgat(ilg),
     8      rmsgat(ilg),               rmrgat(ilg),
     9      tltrleafgat(ilg,icc),      tltrstemgat(ilg,icc),
     a      tltrrootgat(ilg,icc),      leaflitrgat(ilg,icc),
     b      roottempgat(ilg,icc),      afrleafgat(ilg,icc),
     c      afrstemgat(ilg,icc),       afrrootgat(ilg,icc),
     d      wtstatusgat(ilg,icc),      ltstatusgat(ilg,icc),
     e      burnfracgat(ilg),          smfuncveggat(ilg,icc),
     f      lucemcomgat(ilg),          lucltringat(ilg),
     g      lucsocingat(ilg),          nppveggat(ilg,icc),
     h      dstcemls3gat(ilg)
c
c      fire variables
       real emit_co2gat(ilg,icc),      emit_cogat(ilg,icc), 
     1      emit_ch4gat(ilg,icc),      emit_nmhcgat(ilg,icc),
     2      emit_h2gat(ilg,icc),       emit_noxgat(ilg,icc),
     3      emit_n2ogat(ilg,icc),      emit_pm25gat(ilg,icc),
     4      emit_tpmgat(ilg,icc),      emit_tcgat(ilg,icc),
     5      emit_ocgat(ilg,icc),       emit_bcgat(ilg,icc),
     6      burnvegfgat(ilg,icc),      btermgat(ilg,icc),
     7      ltermgat(ilg),             mtermgat(ilg,icc)

c
      real  faregat(ilg) 
      real  gavgscmsgat(ilg)  
      real  tcanoaccgat_out(ilg)
c
      real rmlvegaccgat(ilg,icc),     rmsveggat(ilg,icc),
     1      rmrveggat(ilg,icc),       rgveggat(ilg,icc),
     2      ailcmingat(ilg,icc),     ailcmaxgat(ilg,icc)
c
      real vgbiomas_veggat(ilg,icc)
c
      real gppveggat(ilg,icc),        nepveggat(ilg,iccp1),
     1     nbpveggat(ilg,iccp1), hetroresveggat(ilg,iccp1),
     2      autoresveggat(ilg,icc),litresveggat(ilg,iccp1),
     3      soilcresveggat(ilg,iccp1)

c   Methane related variables
       real  ch4wet1row(nlat,nmos),         ch4wet1gat(ilg),
     3       ch4wet2row(nlat,nmos),         ch4wet2gat(ilg),
     4       wetfdynrow(nlat,nmos),         wetfdyngat(ilg),
     5       ch4dyn1row(nlat,nmos),         ch4dyn1gat(ilg),
     6       ch4dyn2row(nlat,nmos),         ch4dyn2gat(ilg),
     7       ch4soillsrow(nlat,nmos),      ch4soillsgat(ilg)

       real twarmmrow(nlat,nmos),             twarmmgat(ilg),
     1       tcoldmrow(nlat,nmos),            tcoldmgat(ilg),
     2       gdd5row(nlat,nmos),              gdd5gat(ilg),
     3       aridityrow(nlat,nmos),           ariditygat(ilg),
     4       srplsmonrow(nlat,nmos),          srplsmongat(ilg),
     5       defctmonrow(nlat,nmos),          defctmongat(ilg),
     6       anndefctrow(nlat,nmos),          anndefctgat(ilg),
     7       annsrplsrow(nlat,nmos),          annsrplsgat(ilg),
     8       annpcprow(nlat,nmos),            annpcpgat(ilg),
     9       dry_season_lengthrow(nlat,nmos),
     +       dry_season_lengthgat(ilg)


c----------------------------------------------------------------------
      do 100 k=1,nml
          sdeprow(ilmos(k),jlmos(k))        = sdepgat(k)
          co2concrow(ilmos(k),jlmos(k))     = co2concgat(k)
          ch4concrow(ilmos(k),jlmos(k))     = ch4concgat(k)
          cfluxcgrow(ilmos(k),jlmos(k))     = cfluxcggat(k)
          cfluxcsrow(ilmos(k),jlmos(k))     = cfluxcsgat(k)
          canresrow(ilmos(k),jlmos(k))      = canresgat(k)
          xdiffus(ilmos(k))                 = xdiffusgat(k) 
          prbfrhucgrd(ilmos(k))             = prbfrhucgat(k)
          extnprobgrd(ilmos(k))             = extnprobgat(k)
          tcanoaccrow_m(ilmos(k),jlmos(k))  = tcanoaccgat_m(k) 
          uvaccrow_m(ilmos(k),jlmos(k))     = uvaccgat_m(k) 
          vvaccrow_m(ilmos(k),jlmos(k))     = vvaccgat_m(k) 
          vgbiomasrow(ilmos(k),jlmos(k))    = vgbiomasgat(k)
          gavgltmsrow(ilmos(k),jlmos(k))    = gavgltmsgat(k)
          gavglairow(ilmos(k),jlmos(k))     = gavglaigat(k)
          npprow(ilmos(k),jlmos(k))         = nppgat(k)
          neprow(ilmos(k),jlmos(k))         = nepgat(k)
          hetroresrow(ilmos(k),jlmos(k))    = hetroresgat(k)
          autoresrow(ilmos(k),jlmos(k))     = autoresgat(k)
          soilresprow(ilmos(k),jlmos(k))    = soilrespgat(k)
          rmrow(ilmos(k),jlmos(k))          = rmgat(k)
          rgrow(ilmos(k),jlmos(k))          = rggat(k)
          nbprow(ilmos(k),jlmos(k))         = nbpgat(k)
          litresrow(ilmos(k),jlmos(k))      = litresgat(k)
          socresrow(ilmos(k),jlmos(k))      = socresgat(k)
          gpprow(ilmos(k),jlmos(k))         = gppgat(k)
          dstcemlsrow(ilmos(k),jlmos(k))    = dstcemlsgat(k)
          litrfallrow(ilmos(k),jlmos(k))    = litrfallgat(k)
          humiftrsrow(ilmos(k),jlmos(k))    = humiftrsgat(k)
          rmlrow(ilmos(k),jlmos(k))         = rmlgat(k)
          rmsrow(ilmos(k),jlmos(k))         = rmsgat(k)
          rmrrow(ilmos(k),jlmos(k))         = rmrgat(k)
          burnfracrow(ilmos(k),jlmos(k))    = burnfracgat(k)
          ltermrow(ilmos(k),jlmos(k))       = ltermgat(k)
          lucemcomrow(ilmos(k),jlmos(k))    = lucemcomgat(k)
          lucltrinrow(ilmos(k),jlmos(k))    = lucltringat(k)
          lucsocinrow(ilmos(k),jlmos(k))    = lucsocingat(k)
          dstcemls3row(ilmos(k),jlmos(k))   = dstcemls3gat(k)
          farerow(ilmos(k),jlmos(k))        = faregat(k)
          gavgscmsrow(ilmos(k),jlmos(k))    = gavgscmsgat(k)
          tcanoaccrow_out(ilmos(k),jlmos(k))= tcanoaccgat_out(k)
!          wetfracrow(ilmos(k))              = wetfracgat(k)
!          slopefrac_row(ilmos(k))            = slopefrac_gat(k)
          ch4wet1row(ilmos(k),jlmos(k))     = ch4wet1gat(k)
          ch4wet2row(ilmos(k),jlmos(k))     = ch4wet2gat(k)
          wetfdynrow(ilmos(k),jlmos(k))     = wetfdyngat(k)
          ch4dyn1row(ilmos(k),jlmos(k))     = ch4dyn1gat(k)
          ch4dyn2row(ilmos(k),jlmos(k))     = ch4dyn2gat(k)
          ch4soillsrow(ilmos(k),jlmos(k))   = ch4soillsgat(k)

          twarmmrow(ilmos(k),jlmos(k)) = twarmmgat(k)
          tcoldmrow(ilmos(k),jlmos(k)) = tcoldmgat(k)
          gdd5row(ilmos(k),jlmos(k)) = gdd5gat(k)
          aridityrow(ilmos(k),jlmos(k)) = ariditygat(k)
          srplsmonrow(ilmos(k),jlmos(k)) = srplsmongat(k)
          defctmonrow(ilmos(k),jlmos(k)) = defctmongat(k)
          anndefctrow(ilmos(k),jlmos(k)) = anndefctgat(k)
          annsrplsrow(ilmos(k),jlmos(k)) = annsrplsgat(k)
          annpcprow(ilmos(k),jlmos(k)) = annpcpgat(k)
          dry_season_lengthrow(ilmos(k),jlmos(k)) =
     1                           dry_season_lengthgat(k)

c
100   continue
c
      do 101 l=1,icc
       do 101 k=1,nml
          smfuncvegrow(ilmos(k),jlmos(k),l) = smfuncveggat(k,l)
          mtermrow(ilmos(k),jlmos(k),l)     = mtermgat(k,l)
          btermrow(ilmos(k),jlmos(k),l)     = btermgat(k,l)
          ailcgrow(ilmos(k),jlmos(k),l)     = ailcggat(k,l)   
          ailcgsrow(ilmos(k),jlmos(k),l)    = ailcgsgat(k,l)
          co2i1cgrow(ilmos(k),jlmos(k),l)   = co2i1cggat(k,l) 
          co2i1csrow(ilmos(k),jlmos(k),l)   = co2i1csgat(k,l) 
          co2i2cgrow(ilmos(k),jlmos(k),l)   = co2i2cggat(k,l) 
          co2i2csrow(ilmos(k),jlmos(k),l)   = co2i2csgat(k,l) 
          slairow(ilmos(k),jlmos(k),l)      = slaigat(k,l) 
          anvegrow(ilmos(k),jlmos(k),l)     = anveggat(k,l) 
          rmlvegrow(ilmos(k),jlmos(k),l)    = rmlveggat(k,l) 
          pfcancmxrow(ilmos(k),jlmos(k),l)  = pfcancmxgat(k,l)
          nfcancmxrow(ilmos(k),jlmos(k),l)  = nfcancmxgat(k,l)
          fcancmxrow(ilmos(k),jlmos(k),l)   = fcancmxgat(k,l)
          stemmassrow(ilmos(k),jlmos(k),l)  = stemmassgat(k,l)
          rootmassrow(ilmos(k),jlmos(k),l)  = rootmassgat(k,l)
          pstemmassrow(ilmos(k),jlmos(k),l)  = pstemmassgat(k,l)
          pgleafmassrow(ilmos(k),jlmos(k),l)  = pgleafmassgat(k,l)
          gleafmasrow(ilmos(k),jlmos(k),l)  = gleafmasgat(k,l)
          bleafmasrow(ilmos(k),jlmos(k),l)  = bleafmasgat(k,l)
          ailcbrow(ilmos(k),jlmos(k),l)     = ailcbgat(k,l)   
          flhrlossrow(ilmos(k),jlmos(k),l)  = flhrlossgat(k,l)
          pandaysrow(ilmos(k),jlmos(k),l)   = pandaysgat(k,l)
          lfstatusrow(ilmos(k),jlmos(k),l)  = lfstatusgat(k,l)
          grwtheffrow(ilmos(k),jlmos(k),l)  = grwtheffgat(k,l)
          lystmmasrow(ilmos(k),jlmos(k),l)  = lystmmasgat(k,l)
          lyrotmasrow(ilmos(k),jlmos(k),l)  = lyrotmasgat(k,l)
          tymaxlairow(ilmos(k),jlmos(k),l)  = tymaxlaigat(k,l)
          stmhrlosrow(ilmos(k),jlmos(k),l)  = stmhrlosgat(k,l)
          bmasvegrow(ilmos(k),jlmos(k),l)   = bmasveggat(k,l)
          rothrlosrow(ilmos(k),jlmos(k),l)  = rothrlosgat(k,l)
          veghghtrow(ilmos(k),jlmos(k),l)   = veghghtgat(k,l)
          rootdpthrow(ilmos(k),jlmos(k),l)  = rootdpthgat(k,l)
          tltrleafrow(ilmos(k),jlmos(k),l)  = tltrleafgat(k,l)
          tltrstemrow(ilmos(k),jlmos(k),l)  = tltrstemgat(k,l)
          tltrrootrow(ilmos(k),jlmos(k),l)  = tltrrootgat(k,l)
          leaflitrrow(ilmos(k),jlmos(k),l)  = leaflitrgat(k,l)
          roottemprow(ilmos(k),jlmos(k),l)  = roottempgat(k,l)
          afrleafrow(ilmos(k),jlmos(k),l)   = afrleafgat(k,l)
          afrstemrow(ilmos(k),jlmos(k),l)   = afrstemgat(k,l)
          afrrootrow(ilmos(k),jlmos(k),l)   = afrrootgat(k,l)
          wtstatusrow(ilmos(k),jlmos(k),l)  = wtstatusgat(k,l)
          ltstatusrow(ilmos(k),jlmos(k),l)  = ltstatusgat(k,l)
          nppvegrow(ilmos(k),jlmos(k),l)    = nppveggat(k,l)
          rmlvegaccrow(ilmos(k),jlmos(k),l) = rmlvegaccgat(k,l)
          rmsvegrow(ilmos(k),jlmos(k),l)    = rmsveggat(k,l)
          rmrvegrow(ilmos(k),jlmos(k),l)    = rmrveggat(k,l)
          rgvegrow(ilmos(k),jlmos(k),l)     = rgveggat(k,l)
          gppvegrow(ilmos(k),jlmos(k),l)    = gppveggat(k,l)
          vgbiomas_vegrow(ilmos(k),jlmos(k),l)=vgbiomas_veggat(k,l)
          autoresvegrow(ilmos(k),jlmos(k),l) = autoresveggat(k,l)
          ailcminrow(ilmos(k),jlmos(k),l)=ailcmingat(k,l)
          ailcmaxrow(ilmos(k),jlmos(k),l)=ailcmaxgat(k,l)
          pftexistrow(ilmos(k),jlmos(k),l)=pftexistgat(k,l)

          ancsvegrow(ilmos(k),jlmos(k),l)=ancsveggat(k,l)
          ancgvegrow(ilmos(k),jlmos(k),l)=ancgveggat(k,l)
          rmlcsvegrow(ilmos(k),jlmos(k),l)=rmlcsveggat(k,l)
          rmlcgvegrow(ilmos(k),jlmos(k),l)=rmlcgveggat(k,l)
          litrfallvegrow(ilmos(k),jlmos(k),l) = litrfallveggat(k,l)

c         fire variables
          emit_co2row(ilmos(k),jlmos(k),l)    = emit_co2gat(k,l)
          emit_corow(ilmos(k),jlmos(k),l)     = emit_cogat(k,l) 
          emit_ch4row(ilmos(k),jlmos(k),l)    = emit_ch4gat(k,l)
          emit_nmhcrow(ilmos(k),jlmos(k),l)   = emit_nmhcgat(k,l)
          emit_h2row(ilmos(k),jlmos(k),l)     = emit_h2gat(k,l)
          emit_noxrow(ilmos(k),jlmos(k),l)    = emit_noxgat(k,l)
          emit_n2orow(ilmos(k),jlmos(k),l)    = emit_n2ogat(k,l)
          emit_pm25row(ilmos(k),jlmos(k),l)   = emit_pm25gat(k,l)
          emit_tpmrow(ilmos(k),jlmos(k),l)    = emit_tpmgat(k,l)
          emit_tcrow(ilmos(k),jlmos(k),l)     = emit_tcgat(k,l)
          emit_ocrow(ilmos(k),jlmos(k),l)     = emit_ocgat(k,l)
          emit_bcrow(ilmos(k),jlmos(k),l)     = emit_bcgat(k,l)
          burnvegfrow(ilmos(k),jlmos(k),l)    = burnvegfgat(k,l)
c
101   continue
c
      do 102 l=1,iccp1
       do 102 k=1,nml
          litrmassrow(ilmos(k),jlmos(k),l) = litrmassgat(k,l)
          soilcmasrow(ilmos(k),jlmos(k),l) = soilcmasgat(k,l)
          hetroresvegrow(ilmos(k),jlmos(k),l) = hetroresveggat(k,l)
          litresvegrow(ilmos(k),jlmos(k),l) = litresveggat(k,l)
          soilcresvegrow(ilmos(k),jlmos(k),l) = soilcresveggat(k,l)
          nepvegrow(ilmos(k),jlmos(k),l)    = nepveggat(k,l)
          nbpvegrow(ilmos(k),jlmos(k),l)    = nbpveggat(k,l)
          humiftrsvegrow(ilmos(k),jlmos(k),l) = humiftrsveggat(k,l)

102   continue
c
      do 106 l=1,2     !2 pfts (ndl dcd & crops)
       do 106 k=1,nml
          colddaysrow(ilmos(k),jlmos(k),l)=colddaysgat(k,l)
106   continue
c
      do 201 l=1,ican
       do 201 k=1,nml
          ailcrow(ilmos(k),jlmos(k),l)    = ailcgat(k,l)
          zolncrow(ilmos(k),jlmos(k),l)   = zolncgat(k,l)
          cmasvegcrow(ilmos(k),jlmos(k),l)= cmasvegcgat(k,l)
          paicrow(ilmos(k),jlmos(k),l)    = paicgat(k,l)
          slaicrow(ilmos(k),jlmos(k),l)   = slaicgat(k,l)
          alvsctmrow(ilmos(k),jlmos(k),l) = alvsctmgat(k,l)
          alirctmrow(ilmos(k),jlmos(k),l) = alirctmgat(k,l)
201   continue
c
      do 250 l=1,ignd
       do 250 k=1,nml
          sandrow(ilmos(k),jlmos(k),l)     = sandgat(k,l)
          clayrow(ilmos(k),jlmos(k),l)     = claygat(k,l)
          orgmrow(ilmos(k),jlmos(k),l)     = orgmgat(k,l)
          tbaraccrow_m(ilmos(k),jlmos(k),l)= tbaraccgat_m(k,l)
250   continue
c
      do 280 l=1,icc
       do 280 m=1,ignd
        do 280 k=1,nml
          rmatctemrow(ilmos(k),jlmos(k),l,m) = rmatctemgat(k,l,m)
280   continue
c
      do 290 l=1,ican
       do 290 m=1,ignd
        do 290 k=1,nml
          rmatcrow(ilmos(k),jlmos(k),l,m) = rmatcgat(k,l,m)
290   continue
c
 
c     this class variable is scattered here, but it gathered in classg,
c     not in ctemg2. jm jan 8 2013.
      do 300 l=1,icp1
       do 300 k=1,nml
          fcanrow(ilmos(k),jlmos(k),l)     = fcangat(k,l)  
300   continue

      return
      end
