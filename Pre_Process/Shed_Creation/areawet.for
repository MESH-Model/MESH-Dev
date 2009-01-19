      MODULE areawet


      real*4, dimension(:), allocatable :: wetwid,chawid,chadep,qswrain,
     *             wstore1,wstore2,wcap,flowxa,
     *             chaxa,satxa,wetxa,qin,hcha1,
     *             hcha2,hwet1,hwet2,qiwet1,
     *             qiwet2,qowet1,qowet2,qswevp,
     *             kcond,theta,widep,chaarea,wetarea,wsat,wetfrac
		
      
      REAL :: isowetwt,isowetold,oldisow,isooutwet
      REAL :: isowetold1,isowetold2,isowetold3,isowetold4,isowetold5,
     *        isowetold6,oldisow1,oldisow2,oldisow3,oldisow4,oldisow5,
     *        oldisow6,isooutwet1,isooutwet2,isooutwet3,isooutwet4,
     *        isooutwet5,isooutwet6

	real*4, dimension(:,:), allocatable :: isoin1wet,isoin2wet,
     *             isoout1wet,isoout2wet,isoconcwet,isowstore1,
     *             isowstore2
	real*4, dimension(:,:), allocatable :: isoin1SWwet,isoin2SWwet,
     *             isoout1SWwet,isoout2SWwet,isoconcSWwet,isowstore1SW,
     *             isowstore2SW,isoin1IFwet,isoin2IFwet,
     *             isoout1IFwet,isoout2IFwet,isoconcIFwet,isowstore1IF,
     *             isowstore2IF
	real*4, dimension(:,:), allocatable :: isoin1fswet,isoin2fswet,
     *             isoout1fswet,isoout2fswet,isoconcfswet,isowstore1fs,
     *             isowstore2fs,isoin1SWfswet,isoin2SWfswet,
     *      isoout1SWfswet,isoout2SWfswet,isoconcSWfswet,isowstore1SWfs,
     *      isowstore2SWfs,isoin1IFfswet,isoin2IFfswet,
     *      isoout1IFfswet,isoout2IFfswet,isoconcIFfswet,isowstore1IFfs,
     *      isowstore2IFfs



      END MODULE areawet
  

! TS - Nov 20/03: Added isotope tracer parameters to wetland module
! TS - Jan 30/04: Added more isotope tracer parameters to wetland module
! TS - Apr 25/05: Added more isotope tracer parameters (for snowmelt)





! PARAMETER LIST:
!
! wetwid(n)  - width of wetland coverage to one side of stream channel
! chanwid(n) - width of the stream channel
! chandep(n) - depth of the full stream channel
! wstore1(n) - wetland storage at beginning of time step
! wstore2(n) - wetland storage at end of time step
! wcap(n)    - wetland capacity (maximum storage)
! flowxa(n)  - cross-sectional area of the depth of flow in the channel
! chanxa(n)  - cross-sectional area of the channel
! satxa(n)   - cross-sectional area of the saturation depth in the wetland
! wetxa(n)   - cross-sectional area of the wetland
! hcha(n)    - height of water in the stream channel
! hwet(n)    - height of water in the wetland
! wetarea(n) - plan area of wetlands in m^2
! chaarea(n) - plan area of channel in m^2
! widep      - width-to-depth ratio (a11 in PAR file)
! theta      - wetland soil porosity (a9 in PAR file)
! kcond      - soil conductivity (a10 in PAR file)




