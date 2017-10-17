!-------------------------------------- LICENCE BEGIN ------------------------------------
!Environment Canada - Atmospheric Science and Technology License/Disclaimer,
!                     version 3; Last Modified: May 7, 2008.
!This is free but copyrighted software; you can use/redistribute/modify it under the terms
!of the Environment Canada - Atmospheric Science and Technology License/Disclaimer
!version 3 or (at your option) any later version that should be found at:
!http://collaboration.cmc.ec.gc.ca/science/rpn.comm/license.html
!
!This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
!without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!See the above mentioned License/Disclaimer for more details.
!You should have received a copy of the License/Disclaimer along with this software;
!if not, you can write to: EC-RPN COMM Group, 2121 TransCanada, suite 500, Dorval (Quebec),
!CANADA, H9P 1J3; or send e-mail to service.rpn@ec.gc.ca
!-------------------------------------- LICENCE END --------------------------------------

subroutine inisoili_svs_slc(ni, trnch)
   use sfcbus_mod
   use svs_configs
   implicit none
#include <arch_specific.hf>

   integer ni, trnch

   !@Author Stephane Belair (February 1999)
   !@Object Initialize the soil properties from the sand and clay
   !         fraction for 5 layers of the soil
   !@Arguments
   !             - Input -
   ! NI          longueur d'une tranche horizontale

   integer :: i, k, kk
   real b, usb, fb, crit1_wfcint, crit2_wfcint
   
   real, dimension(ni,nl_slc) :: wsat_slc, wwilt_slc, wfc_slc, b_slc, psisat_slc, &
           ksat_slc, wfcint_slc, fb_slc, &
           quartz_slc, rhosoil_slc, condsld_slc, conddry_slc

   real, pointer, dimension(:) :: zcgsat, zgrkef, zdraindens, zslop

   real, pointer, dimension(:,:) :: zbcoef, zclay, zcondsld, zconddry, zfbcof, zksat, zpsisat, zquartz, zrhosoil, zsand, zwfc, zwfcint, zwsat, zwwilt 

  
#define MKPTR1D(NAME1,NAME2) nullify(NAME1); if (vd%NAME2%i > 0 .and. associated(busptr(vd%NAME2%i)%ptr)) NAME1(1:ni) => busptr(vd%NAME2%i)%ptr(:,trnch)
#define MKPTR2D(NAME1,NAME2) nullify(NAME1); if (vd%NAME2%i > 0 .and. associated(busptr(vd%NAME2%i)%ptr)) NAME1(1:ni,1:vd%NAME2%mul*vd%NAME2%niveaux) => busptr(vd%NAME2%i)%ptr(:,trnch)

   MKPTR1D(zcgsat, cgsat)
   MKPTR1D(zdraindens, draindens)
   MKPTR1D(zgrkef, grkef)
   MKPTR1D(zslop, slop)

   MKPTR2D(zbcoef, bcoef)
   MKPTR2D(zclay, clay)
   MKPTR2D(zcondsld,condsld)
   MKPTR2D(zconddry,conddry)
   MKPTR2D(zfbcof, fbcof)
   MKPTR2D(zksat, ksat)
   MKPTR2D(zpsisat, psisat)
   MKPTR2D(zquartz, quartz)
   MKPTR2D(zrhosoil,rhosoil)
   MKPTR2D(zsand, sand)
   MKPTR2D(zwfc, wfc)
   MKPTR2D(zwfcint, wfcint)
   MKPTR2D(zwsat, wsat)
   MKPTR2D(zwwilt , wwilt)
   
      !call subroutine to compute layer thicknesses
      call layer_thickness()


      ! calculate soil parameters on native SLC layers, and then map them unto model layers. 



      ! calculate weights to be used in phybusinit.... because here... we are
      ! re-doing the calculation for each row of domain...
      ! but the weights are the same !
     

!     Computer soil properties for SLC layers
   do i=1,ni
      do k=1,nl_slc
         wsat_slc  (i,k)  =  -0.00126   * zsand(i,k) + 0.489
         wwilt_slc (i,k)  =  37.1342e-3 * sqrt(max(1.,zclay(i,k)))
         wfc_slc   (i,k)  =  89.0467e-3 * max(1.,zclay(i,k))**0.3496
         psisat_slc(i,k)  =  0.01 * ( 10.0**(-0.0131 * zsand(i,k) + 1.88) )
         ksat_slc  (i,k)  =  ( 10.0**(0.0153 * zsand(i,k) - 0.884) ) * 7.0556E-6

         b                 =  0.137 * zclay(i,k)  + 3.501
         b_slc     (i,k)  =  b
         usb               =  1./b
         fb                =  b**usb/(b-1.) * ((3.*b+2.)**(1.-usb)-(2.*b+2.)**(1.-usb))
         fb_slc(i,k)      =  fb
         
         ! quartz content (ref : NL95 & PL98)): 
         quartz_slc(i,k)  =  0.038 + 0.0095*zsand(i,k)

         ! Compute water content at field capacity along sloping aquifer based on Soulis et al. 2012
         ! Ensure that wc at fc stays between wilting point and saturation

        crit1_wfcint   = 2.*zdraindens(i)*psisat_slc(i,k)*(wsat_slc(i,k)/wwilt_slc(i,k)*fb)**b
        crit2_wfcint   = 2.*zdraindens(i)*psisat_slc(i,k)*fb**b

        if (abs(zslop(i)).gt.crit1_wfcint) then
           wfcint_slc(i,k) = wwilt_slc(i,k)        
        elseif (abs(zslop(i)).lt.crit2_wfcint) then
           wfcint_slc(i,k) = wsat_slc(i,k) 
        elseif (zslop(i).ne.0.0) then
           wfcint_slc(i,k) = wsat_slc(i,k) * fb * &
                ( psisat_slc(i,k)/ABS(zslop(i)) *2. * zdraindens(i) )**usb
        else
           wfcint_slc(i,k) = wfc_slc(i,k)
        endif

        ! Compute soil thermal properties for heat diffusion

        ! Soil dry density (PL98)
        !    XDRYWGHT  = 2700.0 ! kg/m3 -->Soil solids dry weight mettre dans cdk
        !    ZGAMMAD(:,:)   = (1.0-WSAT(:,:))*XDRYWGHT  
        rhosoil_slc(i,k) = (1.0-wsat_slc(i,k))*2700.

        ! Soil solids conductivity
        if (quartz_slc(i,k).gt.0.20) then
           condsld_slc(i,k) = (7.7**quartz_slc(i,k)) * (2.0**(1.0-quartz_slc(i,k)))  
        endif
        if (quartz_slc(i,k).le.0.20) then
           condsld_slc(i,k) = (7.7**quartz_slc(i,k)) * (3.0**(1.0-quartz_slc(i,k)))  
        endif
        
        ! Soil dry conductivity
        conddry_slc(i,k) = (0.135*rhosoil_slc(i,k) + 64.7) /  &
                            (2700. - 0.947*rhosoil_slc(i,k))  
        
     enddo
  enddo

   ! "Map" SLC soil properties unto model soil layers
   Do i = 1 , ni
         Do k = 1, nl_svs
            do kk = 1 , nl_slc
                             
               zwsat   (i,k)  = zwsat   (i,k) + wsat_slc  (i,k)  * weights( k , kk)
               zwwilt  (i,k)  = zwwilt  (i,k) + wwilt_slc (i,k)  * weights( k , kk)
             
               zwfc    (i,k)  = zwfc    (i,k) + wfc_slc   (i,k)  * weights( k , kk)
               zbcoef  (i,k)  = zbcoef  (i,k) + b_slc     (i,k)  * weights( k , kk)
               zfbcof  (i,k)  = zfbcof  (i,k) + fb_slc    (i,k)  * weights( k , kk)
               zpsisat (i,k)  = zpsisat (i,k) + psisat_slc(i,k)  * weights( k , kk)
               zksat   (i,k)  = zksat   (i,k) + ksat_slc  (i,k)  * weights( k , kk)
               zwfcint (i,k)  = zwfcint (i,k) + wfcint_slc(i,k)  * weights( k , kk)
               
               zquartz (i,k)  = zquartz (i,k) + quartz_slc (i,k) * weights( k , kk)
               zrhosoil(i,k)  = zrhosoil(i,k) + rhosoil_slc(i,k) * weights( k , kk)
               zcondsld(i,k)  = zcondsld(i,k) + condsld_slc(i,k) * weights( k , kk) 
               zconddry(i,k)  = zconddry(i,k) + conddry_slc(i,k) * weights( k , kk)

            enddo
         enddo

         ! compute thermal coeff. 
         ! for 1st model layer only --- here simply use 1st SLC soil texture !!! Do not map !
         zcgsat (i)  = ( -1.557e-2 * zsand(i,1) &
              -  1.441e-2 * zclay(i,1) + 4.7021 ) * 1.E-6 
         
         ! Compute effective parameter for watdrain
         zgrkef(i)   = 2.* zdraindens(i) * zslop(i)

      enddo

   return
 end subroutine inisoili_svs_slc
