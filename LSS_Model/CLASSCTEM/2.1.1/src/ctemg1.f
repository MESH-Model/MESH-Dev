!>\file
C!              Canadian Terrestrial Ecosystem Model (CTEM) 
!!
      subroutine ctemg1(  gleafmasgat,  bleafmasgat,  stemmassgat,
     a                    rootmassgat,   fcancmxgat,      zbtwgat,
     b                        dlzwgat,      sdepgat,     ailcggat,
     c           ailcbgat,    ailcgat,     zolncgat,     rmatcgat,
     d        rmatctemgat,    slaigat,   bmasveggat,  cmasvegcgat,
     e         veghghtgat,rootdpthgat,   alvsctmgat,   alirctmgat,
     f            paicgat,   slaicgat,   faregat,
     g              ilmos,      jlmos,       iwmos,         jwmos,
     h         nml, 
     i        gleafmasrow,bleafmasrow, stemmassrow,   rootmassrow,
     j         fcancmxrow,    zbtwrow,     dlzwrow,       sdeprow,
     k           ailcgrow,   ailcbrow,     ailcrow,      zolncrow,
     l           rmatcrow,rmatctemrow,     slairow,    bmasvegrow,
     m        cmasvegcrow, veghghtrow, rootdpthrow,    alvsctmrow,
     n         alirctmrow,    paicrow,    slaicrow,    FAREROT )
c
c     22  Jul 2013  - Add in module for parameters
C     J. Melton
c
c     July 5 2009   - gather operation on ctem variables for consistency
c                   with class' tiled version
c     Rong Li
c

      use ctem_params,        only : nlat, nmos, ilg, ignd, ican, icp1, 
     1                               icc

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

      real  gleafmasgat(ilg,icc),  bleafmasgat(ilg,icc),  
     1      stemmassgat(ilg,icc),  rootmassgat(ilg,icc),  
     2       fcancmxgat(ilg,icc),     zbtwgat(ilg,ignd), 
     3         dlzwgat(ilg,ignd),          sdepgat(ilg),      
     4         ailcggat(ilg,icc),     ailcbgat(ilg,icc),
     5         ailcgat(ilg,ican),     zolncgat(ilg,ican),
     6   rmatcgat(ilg,ican,ignd),     faregat(ilg),
     7 rmatctemgat(ilg,icc,ignd),
     8          slaigat(ilg,icc),    bmasveggat(ilg,icc),
     9     cmasvegcgat(ilg,ican),    veghghtgat(ilg,icc),
     a      rootdpthgat(ilg,icc),
     b      alvsctmgat(ilg,ican),   alirctmgat(ilg,ican),
     c         paicgat(ilg,ican),     slaicgat(ilg,ican)
c
      real  gleafmasrow(nlat,nmos,icc),     bleafmasrow(nlat,nmos,icc),
     1      stemmassrow(nlat,nmos,icc),     rootmassrow(nlat,nmos,icc),
     2       fcancmxrow(nlat,nmos,icc),        zbtwrow(nlat,nmos,ignd),
     3         dlzwrow(nlat,nmos,ignd),             sdeprow(nlat,nmos),
     4         ailcgrow(nlat,nmos,icc),        ailcbrow(nlat,nmos,icc),
     5         ailcrow(nlat,nmos,ican),       zolncrow(nlat,nmos,ican),
     6   rmatcrow(nlat,nmos,ican,ignd),       FAREROT(nlat, nmos),
     7 rmatctemrow(nlat,nmos,icc,ignd),
     8          slairow(nlat,nmos,icc),      bmasvegrow(nlat,nmos,icc),
     9     cmasvegcrow(nlat,nmos,ican),      veghghtrow(nlat,nmos,icc),
     a      rootdpthrow(nlat,nmos,icc),
     b      alvsctmrow(nlat,nmos,ican),     alirctmrow(nlat,nmos,ican),
     c         paicrow(nlat,nmos,ican),       slaicrow(nlat,nmos,ican)
c
c----------------------------------------------------------------------


      do 100 k=1,nml
          sdepgat(k)=sdeprow(ilmos(k),jlmos(k))
          faregat(k) = FAREROT(ilmos(k), jlmos(k))
100   continue
c


      do 101 l=1,icc !Loop through each CTEM pft
       do 101 k=1,nml !Loop through each Mosaic/GRU tile
          gleafmasgat(k,l) = gleafmasrow(ilmos(k),jlmos(k),l)
          bleafmasgat(k,l) = bleafmasrow(ilmos(k),jlmos(k),l)
          stemmassgat(k,l) = stemmassrow(ilmos(k),jlmos(k),l)
          rootmassgat(k,l) = rootmassrow(ilmos(k),jlmos(k),l)
          fcancmxgat(k,l)  = fcancmxrow(ilmos(k),jlmos(k),l)
          ailcggat(k,l)    = ailcgrow(ilmos(k),jlmos(k),l)
          ailcbgat(k,l)    = ailcbrow(ilmos(k),jlmos(k),l)
          slaigat(k,l)     = slairow(ilmos(k),jlmos(k),l)
          bmasveggat(k,l)  = bmasvegrow(ilmos(k),jlmos(k),l)
          veghghtgat(k,l)  = veghghtrow(ilmos(k),jlmos(k),l)
          rootdpthgat(k,l) = rootdpthrow(ilmos(k),jlmos(k),l)


101   continue





c
      do 201 l=1,ican
       do 201 k=1,nml
          ailcgat(k,l)     = ailcrow(ilmos(k),jlmos(k),l)
          zolncgat(k,l)    = zolncrow(ilmos(k),jlmos(k),l)
          cmasvegcgat(k,l) = cmasvegcrow(ilmos(k),jlmos(k),l)
          alvsctmgat(k,l)  = alvsctmrow(ilmos(k),jlmos(k),l)
          alirctmgat(k,l)  = alirctmrow(ilmos(k),jlmos(k),l)
          paicgat(k,l)     = paicrow(ilmos(k),jlmos(k),l)
          slaicgat(k,l)    = slaicrow(ilmos(k),jlmos(k),l)
201   continue
c
      do 250 l=1,ignd
       do 250 k=1,nml
          zbtwgat(k,l) = zbtwrow(ilmos(k),jlmos(k),l)
          dlzwgat(k,l) = dlzwrow(ilmos(k),jlmos(k),l)
250   continue
c
      do 280 l=1,icc
       do 280 m=1,ignd
        do 280 k=1,nml
          rmatctemgat(k,l,m) = rmatctemrow(ilmos(k),jlmos(k),l,m)
280   continue
c
      do 290 l=1,ican
       do 290 m=1,ignd
        do 290 k=1,nml
           rmatcgat(k,l,m)=rmatcrow(ilmos(k),jlmos(k),l,m)
290   continue
c
      return
      end
