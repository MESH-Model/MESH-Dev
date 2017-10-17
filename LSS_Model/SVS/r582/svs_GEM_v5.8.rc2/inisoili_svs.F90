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

   implicit none
#include <arch_specific.hf>

!VV DEBUT MODIFICATION POUR MESH
   integer ni,fsiz
   real,target :: f(fsiz)
!   integer ni, trnch
!VV FIN MODIFICATION POUR MESH

   !@Author Stephane Belair (February 1999)
   !@Object Initialize the soil properties from the sand and clay
   !         fraction for 5 layers of the soil
   !@Arguments
   !             - Input -
   ! NI          longueur d'une tranche horizontale

   integer :: i, k , n2d
   REAL b, usb, fb, crit1_wfcint, crit2_wfcint

   real, pointer, dimension(:) :: zcgsat, zgrkef, zdraindens, zslop

   real, pointer, dimension(:,:) :: zbcoef, zclay, zksat, zpsisat, zsand, zwfc, zwfcint, zwsat, zwwilt 

  
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
!   MKPTR2D(zksat, ksat)
!   MKPTR2D(zpsisat , psisat)
!   MKPTR2D(zsand, sand)
!   MKPTR2D(zwfc, wfc)
!   MKPTR2D(zwfcint, wfcint)
!   MKPTR2D(zwsat, wsat)
!   MKPTR2D(zwwilt , wwilt)

write(*,*)  'cgsat',cgsat,fsiz

! assign pointers
      zcgsat      (1:ni) => f(cgsat:(cgsat+ni-1))
      zdraindens  (1:ni) => f(draindens:(draindens+ni-1))
      zgrkef      (1:ni) => f(grkef:(grkef+ni-1))
      zslop       (1:ni) => f(slop:(slop+ni-1))

      n2d =  ni*nl_svs
      zbcoef      (1:ni,1:nl_svs) => f(bcoef:(bcoef+n2d-1))
      zclay      (1:ni,1:nl_svs) => f(clay:(clay+n2d-1))
      zksat      (1:ni,1:nl_svs) => f(ksat:(ksat+n2d-1))
      zpsisat      (1:ni,1:nl_svs) => f(psisat:(psisat+n2d-1))
      zsand      (1:ni,1:nl_svs) => f(sand:(sand+n2d-1))
      zwfc      (1:ni,1:nl_svs) => f(wfc:(wfc+n2d-1))
      zwfcint      (1:ni,1:nl_svs) => f(wfcint:(wfcint+n2d-1))
      zwsat      (1:ni,1:nl_svs) => f(wsat:(wsat+n2d-1))
      zwwilt      (1:ni,1:nl_svs) => f(wwilt:(wwilt+n2d-1))
!VV FIN MODIFICATION POUR MESH

      !call subroutine to compute layer thicknesses
      call layer_thickness()

!print *, '% SAND:           ', bus(sand), bus(sand + ni)

   do i=1,ni
      do k=1,nl_svs
!fixed values 
        !zwsat  (i,k)  =  0.45
        !zwwilt (i,k)  =  0.1
        !zwfc   (i,k)  =  0.35

!calculate based on soil texture  data
        zwsat  (i,k)  =  -0.00126   * zsand(i,k) + 0.489
        zwwilt (i,k)  =  37.1342e-3 * sqrt(max(1.,zclay(i,k)))
        zwfc   (i,k)  =  89.0467e-3 * max(1.,zclay(i,k))**0.3496

        b             =  0.137 * zclay(i,k)  + 3.501
        usb           = 1./b
        zbcoef (i,k)  = b
        
        fb            = b**usb/(b-1.) * ((3.*b+2.)**(1.-usb)-(2.*b+2.)**(1.-usb))
 
        zpsisat(i,k)  = 0.01 * ( 10.0**(-0.0131 * zsand(i,k) + 1.88) )
        zksat(i,k)    = ( 10.0**(0.0153 * zsand(i,k) - 0.884) ) * 7.0556E-6

! Compute water content at field capacity along sloping aquifer based on Soulis et al. 2012
! Ensure that wc at fc stays between wilting point and saturation

        crit1_wfcint   = 2.*zdraindens(i)*zpsisat(i,k)*(zwsat(i,k)/zwwilt(i,k)*fb)**b
        crit2_wfcint   = 2.*zdraindens(i)*zpsisat(i,k)*fb**b

        if (abs(zslop(i)).gt.crit1_wfcint) then
           zwfcint(i,k) = zwwilt(i,k)        
        elseif (abs(zslop(i)).lt.crit2_wfcint) then
           zwfcint(i,k) = zwsat(i,k) 
        elseif (zslop(i).ne.0.0) then
           zwfcint(i,k) = zwsat(i,k) * fb * &
                ( zpsisat(i,k)/ABS(zslop(i)) *2. * zdraindens(i) )**usb
        else
           zwfcint(i,k) = zwfc(i,k)
        endif

      enddo
! Compute effective parameter for watdrain
      zgrkef(i)   = 2.* zdraindens(i) * zslop(i)


      zcgsat (i)  = ( -1.557e-2 * zsand(i,1) &
           -  1.441e-2 * zclay(i,1) + 4.7021 ) * 1.E-6 

   enddo


   return
 end subroutine inisoili_svs
