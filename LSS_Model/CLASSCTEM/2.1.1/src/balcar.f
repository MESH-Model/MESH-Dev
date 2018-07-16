!>\file
!!               Canadian Terrestrial Ecosystem Model (CTEM)
!!                          Carbon Balance Subroutine
!!
!!unless mentioned all pools are in kg c/m2
!!unless mentioned all fluxes are in units of u-mol co2/m2.sec
!!
       subroutine  balcar (gleafmas, stemmass, rootmass, bleafmas,      
     1                     litrmass, soilcmas, ntchlveg, ntchsveg,
     2                     ntchrveg, tltrleaf, tltrstem, tltrroot,
     3                     glcaemls, blcaemls, stcaemls, rtcaemls,
     4                     ltrcemls, ltresveg, scresveg, humtrsvg,
     5                     pglfmass, pblfmass, pstemass, protmass,
     6                     plitmass, psocmass, vgbiomas, repro_cost,
     7                     pvgbioms, gavgltms, pgavltms, gavgscms,
     8                     pgavscms, galtcels, repro_cost_g,
     9                          npp,  autores, hetrores,      gpp,
     a                          nep,   litres,   socres, dstcemls,
     b                          nbp, litrfall, humiftrs,
     c                          il1,      il2)    
c     -----------------------------------------------------------------      
c
c     22  Nov 2012  - calling this version 1.1 since a fair bit of ctem
c     V. Arora        subroutines were changed for compatibility with class
c                     version 3.6 including the capability to run ctem in
c                     mosaic/tile version along with class.
c
c     24  Sep 2012  - add in checks to prevent calculation of non-present
c     J. Melton       pfts
c
c     27  May 2003  - this subroutine checks if the various c fluxes
c     V. Arora        between the different pools balance properly to
c                     make sure that conservation of mass is achieved 
c                     with in a specified tolerance.
c
c     inputs 
c                 grid averaged fluxes
c
c     expnbaln  - amount of c related to spatial expansion
c
c                 other variables
c
c     deltat    - ctem's time step
c     icc       - no. of ctem plant function types, currently 8
c     ilg       - no. of grid cells in latitude circle
c     fcancmx   - max. fractional coverage of ctem's 9 pfts, but this can be
c                modified by land-use change, and competition between pfts

      use ctem_params,        only : tolrance, icc, ilg, deltat
c
      implicit none
c
      integer il1 !<other variables: il1=1
      integer il2 !<other variables: il2=ilg
      integer i, j, k
c
      real stemmass(ilg,icc)  !<pools (after being updated): stem mass for each of the 9 ctem pfts
      real rootmass(ilg,icc)  !<pools (after being updated): root mass for each of the 9 ctem pfts
      real gleafmas(ilg,icc)  !<pools (after being updated): green leaf mass for each of the 9 ctem pfts
      real bleafmas(ilg,icc)  !<pools (after being updated): brown leaf mass for each of the 9 ctem pfts
      real litrmass(ilg,icc+1)!<pools (after being updated): litter mass over the 9 pfts and the bare fraction of the grid cell
      real soilcmas(ilg,icc+1)!<pools (after being updated): soil carbon mass over the 9 pfts and the bare fraction of the grid cell
      real ntchlveg(ilg,icc)  !<fluxes for each pft: net change in leaf biomass
      real ntchsveg(ilg,icc)  !<fluxes for each pft: net change in stem biomass
      real ntchrveg(ilg,icc)  !<fluxes for each pft: net change in root biomass the net change is the difference
                              !<between allocation and autotrophic respiratory fluxes 
      real tltrleaf(ilg,icc)  !<fluxes for each pft: total leaf litter falling rate
      real tltrstem(ilg,icc)  !<fluxes for each pft: total stem litter falling rate
      real tltrroot(ilg,icc)  !<fluxes for each pft: total root litter falling rate
      real glcaemls(ilg,icc)  !<fluxes for each pft: carbon emission losses mainly due to fire: green leaf carbon emission losses 
      real blcaemls(ilg,icc)  !<fluxes for each pft: carbon emission losses mainly due to fire: brown leaf carbon emission losses 
      real stcaemls(ilg,icc)  !<fluxes for each pft: carbon emission losses mainly due to fire: stem carbon emission losses 
      real rtcaemls(ilg,icc)  !<fluxes for each pft: carbon emission losses mainly due to fire: root carbon emission losses 
      real ltrcemls(ilg,icc)  !<fluxes for each pft: carbon emission losses mainly due to fire: litter carbon emission losses
      real ltresveg(ilg,icc+1)!<fluxes for each pft: litter respiration for each pft + bare fraction 
      real scresveg(ilg,icc+1)!<fluxes for each pft: soil c respiration for each pft + bare fraction
      real humtrsvg(ilg,icc+1)!<fluxes for each pft: humification for each pft + bare fraction
      real pglfmass(ilg,icc)  !<pools (before being updated): previous green leaf mass
      real pblfmass(ilg,icc)  !<pools (before being updated): previous brown leaf mass
      real pstemass(ilg,icc)  !<pools (before being updated): previous stem mass
      real protmass(ilg,icc)  !<pools (before being updated): previous root mass
      real plitmass(ilg,icc+1)!<pools (before being updated): previous litter mass
      real psocmass(ilg,icc+1)!<pools (before being updated): previous soil c mass
      real npp(ilg)           !<grid averaged flux: net primary productivity 
      real vgbiomas(ilg)      !<pools (after being updated): grid averaged pools: vegetation biomass
      real pvgbioms(ilg)      !<pools (before being updated): grid average pools: previous vegetation biomass
      real gavgltms(ilg)      !<pools (after being updated): grid averaged pools: litter mass
      real pgavltms(ilg)      !<pools (before being updated): grid average pools: previous litter mass
      real gavgscms(ilg)      !<pools (after being updated): grid averaged pools: soil carbon mass
      real pgavscms(ilg)      !<pools (before being updated): grid average pools: previous soil c mass
      real autores(ilg)       !<grid averaged flux: autotrophic respiration
      real hetrores(ilg)      !<grid averaged flux: heterotrophic respiration
      real gpp(ilg)           !<grid averaged flux: gross primary productivity
      real nep(ilg)           !<grid averaged flux: net primary productivity 
      real litres(ilg)        !<grid averaged flux: litter respiration
      real socres(ilg)        !<grid averaged flux: soil carbon respiration
      real dstcemls(ilg)      !<grid averaged flux: carbon emission losses due to disturbance, mainly fire
      real nbp(ilg)           !<grid averaged flux: net biome productivity
      real litrfall(ilg)      !<grid averaged flux: combined (leaves, stem, and root) total litter fall rate
      real humiftrs(ilg)      !<grid averaged flux: humification
      real repro_cost(ilg,icc)!<pools (after being updated): amount of C transferred to litter due to reproductive tissues
          
!     d         galtcels(ilg),       expnbaln(ilg),   

      real galtcels(ilg)      !<grid averaged flux: carbon emission losses from litter
      real repro_cost_g(ilg)  !<grid averaged flux: amount of C used to generate reproductive tissues
c
      real diff1  !<
      real diff2  !<
c
c
c     -----------------------------------------------------------------  
c
      if(icc.ne.9)                            call xit('balcar',-1)
!>
!!to check c budget we go through each pool for each vegetation type.
!!
!!green and brown leaves
!!
      do 100 j = 1, icc
        do 110 i = il1, il2
          diff1=(gleafmas(i,j)+bleafmas(i,j)- pglfmass(i,j)-
     &     pblfmass(i,j))
          diff2=(ntchlveg(i,j)- tltrleaf(i,j)- glcaemls(i,j)- 
     &     blcaemls(i,j))*(deltat/963.62)
          if((abs(diff1-diff2)).gt.tolrance)then
            write(6,2000)i,j,abs(diff1-diff2),tolrance
2000        format('at (i)= (',i3,'), pft=',i2,', ',f12.6,' is greater    
     & than our tolerance of ',f12.6,' for leaves')  
            call xit('balcar',-2)
          endif
c         endif
110     continue
100   continue     
!>
!!stem
!!
      do 150 j = 1, icc
        do 160 i = il1, il2
          diff1=stemmass(i,j) - pstemass(i,j)
          diff2=(ntchsveg(i,j)- tltrstem(i,j)- 
     &     stcaemls(i,j))*(deltat/963.62)
          if((abs(diff1-diff2)).gt.tolrance)then
            write(6,2001)i,j,abs(diff1-diff2),tolrance
2001        format('at (i)= (',i3,'), pft=',i2,', ',f12.6,' is greater
     & than our tolerance of ',f12.6,' for stem')
            call xit('balcar',-3)
          endif
c         endif
160     continue
150   continue    
!>
!!root
!!
      do 200 j = 1, icc
        do 210 i = il1, il2
          diff1=rootmass(i,j) - protmass(i,j)
          diff2=(ntchrveg(i,j)- tltrroot(i,j)- 
     &     rtcaemls(i,j))*(deltat/963.62)
          if((abs(diff1-diff2)).gt.tolrance)then
            write(6,2002)i,j,abs(diff1-diff2),tolrance
2002        format('at (i)= (',i3,'), pft=',i2,', ',f12.6,' is greater
     & than our tolerance of ',f12.6,' for root')
            call xit('balcar',-4)
          endif
c         endif
210     continue
200   continue    
!>
!!litter over all pfts
!!
      do 250 j = 1, icc
        do 260 i = il1, il2
          diff1=litrmass(i,j) - plitmass(i,j)
          diff2=( tltrleaf(i,j)+tltrstem(i,j)+tltrroot(i,j)-
     &      ltresveg(i,j)-humtrsvg(i,j)-ltrcemls(i,j)
     &      + repro_cost(i,j))*(deltat/963.62)   
          if((abs(diff1-diff2)).gt.tolrance)then
            write(6,2003)i,j,abs(diff1-diff2),tolrance
2003        format('at (i)= (',i3,'), pft=',i2,', ',f12.6,' is greater
     & than our tolerance of ',f12.6,' for litter')
            call xit('balcar',-5)
          endif
c         endif
260     continue
250   continue    
!>
!!litter over the bare fraction
!!
        do 280 i = il1, il2
          diff1=litrmass(i,icc+1) - plitmass(i,icc+1)
          diff2=( -ltresveg(i,icc+1)-humtrsvg(i,icc+1))*
     &          ( deltat/963.62 )  
          if((abs(diff1-diff2)).gt.tolrance)then
            write(6,2003)i,icc+1,abs(diff1-diff2),tolrance
            call xit('balcar',-6)
          endif
280     continue
!>
!!soil carbon over the bare fraction
!!
      do 300 j = 1, icc+1
        do 310 i = il1, il2
          diff1=soilcmas(i,j) - psocmass(i,j)
          diff2=( humtrsvg(i,j)-scresveg(i,j) )*(deltat/963.62)  
          if((abs(diff1-diff2)).gt.tolrance)then
            write(6,2004)i,j,abs(diff1-diff2),tolrance
2004        format('at (i)= (',i3,'), pft=',i2,', ',f12.6,' is greater
     & than our tolerance of ',f12.6,' for soil c')
            call xit('balcar',-7)
          endif
310     continue
300   continue   
c
c     -----------------------------------------------------------------
!>
!!grid averaged fluxes must also balance
!!
!!vegetation biomass
!!
      do 350 i = il1, il2
        diff1=vgbiomas(i)-pvgbioms(i)
        diff2=(gpp(i)-autores(i)-litrfall(i)-
     &   dstcemls(i)-repro_cost_g(i))*(deltat/963.62)
        if((abs(diff1-diff2)).gt.tolrance)then
          write(6,3001)'vgbiomas(',i,')=',vgbiomas(i)
          write(6,3001)'pvgbioms(',i,')=',pvgbioms(i)
          write(6,3001)'     gpp(',i,')=',gpp(i)
          write(6,3001)' autores(',i,')=',autores(i)
          write(6,3001)'litrfall(',i,')=',litrfall(i)
          write(6,3001)'dstcemls(',i,')=',dstcemls(i)
          write(6,3001)'repro_cost_g(',i,')=',repro_cost_g(i)
3001      format(a9,i2,a2,f14.9) 
          write(6,2005)i,abs(diff1-diff2),tolrance
2005      format('at (i)= (',i3,'),',f12.6,' is greater
     & than our tolerance of ',f12.6,' for vegetation biomass')
          call xit('balcar',-8)
        endif
350   continue
!>
!!litter
!!
      do 380 i = il1, il2
        diff1=gavgltms(i)-pgavltms(i)
        diff2=(litrfall(i)-litres(i)-humiftrs(i)-galtcels(i)
     &   +repro_cost_g(i))*
     &   (deltat/963.62)
        if((abs(diff1-diff2)).gt.tolrance)then
          write(6,3001)'pgavltms(',i,')=',pgavltms(i)
          write(6,3001)'gavgltms(',i,')=',gavgltms(i)
          write(6,3001)'litrfall(',i,')=',litrfall(i)
          write(6,3001)'  litres(',i,')=',litres(i)
          write(6,3001)'humiftrs(',i,')=',humiftrs(i)
          write(6,3001)'galtcels(',i,')=',galtcels(i)
          write(6,2006)i,abs(diff1-diff2),tolrance
          write(*,*)i,abs(diff1-diff2),tolrance
2006      format('at (i)= (',i3,'),',f12.6,' is greater
     & than our tolerance of ',f12.6,' for litter mass')
          call xit('balcar',-9)
        endif
380   continue
!>
!!soil carbon
!!
      do 390 i = il1, il2
        diff1=gavgscms(i)-pgavscms(i)
        diff2=(humiftrs(i)-socres(i))*(deltat/963.62)
        if((abs(diff1-diff2)).gt.tolrance)then
          write(6,3001)'pgavscms(',i,')=',pgavscms(i)
          write(6,3001)'gavgscms(',i,')=',gavgscms(i)
          write(6,3001)'humiftrs(',i,')=',humiftrs(i)
          write(6,3001)'  socres(',i,')=',socres(i)
          write(6,2007)i,abs(diff1-diff2),tolrance
2007      format('at (i)= (',i3,'),',f12.6,' is greater
     & than our tolerance of ',f12.6,' for soil c mass')
          call xit('balcar',-10)
        endif
390   continue
c
      return
      end
