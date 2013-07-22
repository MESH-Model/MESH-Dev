      MODULE area1

!     entrees rearranged in alphabetical order.  nk  Mar. 7/07

      character*10         :: program_name
      integer*4      ::       maxr
      real*4         ::       por,sqlz,slzinflw,leakage,dacheck

      character*1, dimension(:),allocatable :: glacier_flag

      integer*4,dimension(:), allocatable   :: next,ibn,ichnl,irough,
     *                        ireach,nreach,res,xxx,yyy
     *                                       

      real*4, dimension(:),   allocatable :: 
     *                        att,bnkfll,bin_precip,ch_length,da,d2,cap,
     *                        fake,fakefs,frac,gridarea,lzs,netflow,
     *                        over,pot,potfs,psmear,punused,
     *                        qi1,qi2,qo1,qo2,qda,qr,qlz,qbase,qmax,
     *                        qstream,qstrm,qdrng,qdrngfs,
     *                        qdrng2,qdrngfs2,
     *                        rechrg,rl,
     *                        sl1,slope,sl2,sq1,sq1fs,sqint,sqintfs,
     *                        store1,store2,storinit,sr,strloss,
     *                        sdrng,sdrngfs,sexcess,
     *                        sump,sumrff,sumqint,sumqintfs,
     *                        sumq1,sumq1fs,sumrechrg,
     *                        totd1,totuzs,totsnw,totchnl,totgrid,
     *                        x4,x5
                   

!     note:  frac is to be replaced by gridarea


!     rev. 9.1.30  Nov.  08/02  - added q1, qint & drng to the wfo file
      real*4, dimension(:,:),  allocatable::
     *                         aclass,d1,d1fs,effpor,drng,drngfs,
     *                         df,dffs,q1,q1fs,qint,qintfs,
     *                         snow,sumf,sumffs,r,v,uzs,uzsfs
!    *                                       intcap

!     rev. 9.4.02  Apr.  18/07  - NK: moved rf, rffs from areawq to area1
      real*4, dimension(:,:),  allocatable::
     *                         rf,rffs

      END MODULE area1


! NOTES:
!
! NOV. 2000, TS: Added QLZ variable during WATFLOOD swamp routing changes 
!                b/c needed to access it from routea (not just runof5a).
! AUG 27/03  TS: Added sumq1,sumq1fs,sumqint,and sumqintfs,qdrngfs arrays and made
!                qdrng and qdrngfs arrays.  FOR TRACER S/R
   





