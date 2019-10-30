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

!VV DEBUT MODIFICATION POUR MESH
!subroutine inisoili_svs(ni, trnch)
subroutine inisoili_svs(f, fsiz, ni)
!VV FIN MODIFICATION POUR MESH


!VV DEBUT MODIFICATION POUR MESH
!   use sfcbus_mod
#ifdef RUNSVS
USE runsvs_mod
#endif
!VV FIN MODIFICATION POUR MESH

   use svs_configs
   use sfc_options

   implicit none
#include <arch_specific.hf>

!VV DEBUT MODIFICATION POUR MESH
   integer ni,fsiz,n2d
   real,target :: f(fsiz)
!   integer ni, trnch
!VV FIN MODIFICATION POUR MESH

   !@Author Stephane Belair (February 1999)
   !@Object Initialize the soil properties from the sand and clay
   !         fraction for 5 layers of the soil
   !@Arguments
   !             - Input -
   ! NI          longueur d'une tranche horizontale

   integer :: i, k, kk
   REAL b, usb, fb, crit1_wfcint, crit2_wfcint
   
   REAL, dimension(ni,nl_gsde) :: wsat_gsde, wwilt_gsde, wfc_gsde, b_gsde, psisat_gsde, &
           ksat_gsde, wfcint_gsde, fb_gsde
   real, pointer, dimension(:) :: zcgsat, zgrkef, zdraindens, zslop

   real, pointer, dimension(:,:) :: zbcoef, zclay, zfbcof, zksat, zpsisat, zsand, zwfc, zwfcint, zwsat, zwwilt 

  
!VV DEBUT MODIFICATION POUR MESH
!#define MKPTR1D(NAME1,NAME2) nullify(NAME1); if (vd%NAME2%i > 0 .and. associated(busptr(vd%NAME2%i)%ptr)) NAME1(1:ni) => busptr(vd%NAME2%i)%ptr(:,trnch)
!#define MKPTR2D(NAME1,NAME2) nullify(NAME1); if (vd%NAME2%i > 0 .and. associated(busptr(vd%NAME2%i)%ptr)) NAME1(1:ni,1:vd%NAME2%mul*vd%NAME2%niveaux) => busptr(vd%NAME2%i)%ptr(:,trnch)

  ! MKPTR1D(zcgsat, cgsat)
!   MKPTR1D(zdraindens, draindens)
!   MKPTR1D(zgrkef, grkef)
!   MKPTR1D(zslop, slop)
!
!   MKPTR2D(zbcoef, bcoef)
!   MKPTR2D(zclay, clay)
!   MKPTR2D(zfbcof, fbcof)
!   MKPTR2D(zksat, ksat)
!   MKPTR2D(zpsisat , psisat)
!   MKPTR2D(zsand, sand)
!   MKPTR2D(zwfc, wfc)
!   MKPTR2D(zwfcint, wfcint)
!   MKPTR2D(zwsat, wsat)
!   MKPTR2D(zwwilt , wwilt)

! assign pointers
      zcgsat      (1:ni) => f(cgsat:(cgsat+ni-1))
      zdraindens  (1:ni) => f(draindens:(draindens+ni-1))
      zgrkef      (1:ni) => f(grkef:(grkef+ni-1))
      zslop       (1:ni) => f(slop:(slop+ni-1))

      n2d =  ni*nl_gsde
      zbcoef      (1:ni,1:nl_gsde) => f(bcoef:(bcoef+n2d-1))
      zfbcof      (1:ni,1:nl_gsde) => f(fbcof:(fbcof+n2d-1))
      zclay      (1:ni,1:nl_gsde) => f(clay:(clay+n2d-1))
      zksat      (1:ni,1:nl_gsde) => f(ksat:(ksat+n2d-1))
      zpsisat      (1:ni,1:nl_gsde) => f(psisat:(psisat+n2d-1))
      zsand      (1:ni,1:nl_gsde) => f(sand:(sand+n2d-1))
      zwfc      (1:ni,1:nl_gsde) => f(wfc:(wfc+n2d-1))
      zwfcint      (1:ni,1:nl_gsde) => f(wfcint:(wfcint+n2d-1))
      zwsat      (1:ni,1:nl_gsde) => f(wsat:(wsat+n2d-1))
      zwwilt      (1:ni,1:nl_gsde) => f(wwilt:(wwilt+n2d-1))
!VV FIN MODIFICATION POUR MESH
   
      !call subroutine to compute layer thicknesses
      call layer_thickness()


      ! calculate soil parameters on native GSDE layers, and then map them unto model layers. 



      ! calculate weights to be used in phybusinit.... because here... we are
      ! re-doing the calculation for each row of domain...
      ! but the weights are the same !
     

!     Computer soil properties for GSDE layers
   do i=1,ni
      do k=1,nl_gsde
         wsat_gsde  (i,k)  =  -0.00126   * zsand(i,k) + 0.489
         wwilt_gsde (i,k)  =  37.1342e-3 * sqrt(max(1.,zclay(i,k)))
         wfc_gsde   (i,k)  =  89.0467e-3 * max(1.,zclay(i,k))**0.3496
         psisat_gsde(i,k)  =  0.01 * ( 10.0**(-0.0131 * zsand(i,k) + 1.88) )
         ksat_gsde  (i,k)  =  ( 10.0**(0.0153 * zsand(i,k) - 0.884) ) * 7.0556E-6

         b                 =  0.137 * zclay(i,k)  + 3.501
         b_gsde     (i,k)  =  b
         usb               =  1./b
         fb                =  b**usb/(b-1.) * ((3.*b+2.)**(1.-usb)-(2.*b+2.)**(1.-usb))
         fb_gsde(i,k)      =  fb
         ! Compute water content at field capacity along sloping aquifer based on Soulis et al. 2012
         ! Ensure that wc at fc stays between wilting point and saturation

        crit1_wfcint   = 2.*zdraindens(i)*psisat_gsde(i,k)*(wsat_gsde(i,k)/wwilt_gsde(i,k)*fb)**b
        crit2_wfcint   = 2.*zdraindens(i)*psisat_gsde(i,k)*fb**b

        if (abs(zslop(i)).gt.crit1_wfcint) then
           wfcint_gsde(i,k) = wwilt_gsde(i,k)        
        elseif (abs(zslop(i)).lt.crit2_wfcint) then
           wfcint_gsde(i,k) = wsat_gsde(i,k) 
        elseif (zslop(i).ne.0.0) then
           wfcint_gsde(i,k) = wsat_gsde(i,k) * fb * &
                ( psisat_gsde(i,k)/ABS(zslop(i)) *2. * zdraindens(i) )**usb
        else
           wfcint_gsde(i,k) = wfc_gsde(i,k)
        endif

      enddo
   enddo
   ! "Map" GSDE soil properties unto model soil layers
   Do i = 1 , ni
         Do k = 1, nl_svs
            do kk = 1 , nl_gsde
                             
               zwsat  (i,k)  = zwsat  (i,k) + wsat_gsde  (i,kk)  * weights( k , kk)
               zwwilt (i,k)  = zwwilt (i,k) + wwilt_gsde (i,kk)  * weights( k , kk)
             
               zwfc   (i,k)  = zwfc   (i,k) + wfc_gsde   (i,kk)  * weights( k , kk)
               zbcoef (i,k)  = zbcoef (i,k) + b_gsde     (i,kk)  * weights( k , kk)
               zfbcof (i,k)  = zfbcof (i,k) + fb_gsde    (i,kk)  * weights( k , kk)
               zpsisat(i,k)  = zpsisat(i,k) + psisat_gsde(i,kk)  * weights( k , kk)
               zksat  (i,k)  = zksat  (i,k) + ksat_gsde  (i,kk)  * weights( k , kk)
               zwfcint(i,k)  = zwfcint(i,k) + wfcint_gsde(i,kk)  * weights( k , kk)
               
            enddo
         enddo
         ! compute thermal coeff. 
         ! for 1st model layer only --- here simply use 1st GSDE soil texture !!! Do not map !
         zcgsat (i)  = ( -1.557e-2 * zsand(i,1) &
              -  1.441e-2 * zclay(i,1) + 4.7021 ) * 1.E-6 
         
         ! Compute effective parameter for watdrain
         zgrkef(i)   = 2.* zdraindens(i) * zslop(i)

      enddo

   return
 end subroutine !inisoili_svs_gsde
