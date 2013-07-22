      MODULE area4

      real*4,    dimension(:), allocatable ::  r1,r4,ds,dsfs,chnl,
     *                         r2,r3,r3fs,rec,ak,akfs,r2low,r3low,
     *                         r3fslow,reclow,aklow,akfslow,ak2fslow,
     *                         r2hgh,r3hgh,r3fshgh,rechgh,akhgh,akfshgh,
     *                         ak2fshgh,r2dlt,r3dlt,r3fsdlt,recdlt,
     *                         akdlt,akfsdlt,ak2fsdlt,retn,ak2,flz,flz2,
     *                         pwr,pwr2,retnlow,ak2low,flzlow,pwrlow,
     *                         retnhgh,ak2hgh,flzhgh,pwrhgh,retndlt,
     *                         ak2dlt,flzdlt,pwrdlt,retfs, ak2fs,fpet,
     *                         fpetdlt,fpetlow,fpethgh,ftall,ftalldlt,
     *                         ftalllow,ftallhgh,mndr,aa2,aa3,aa4,
     *            thetadlt,thetalow,thetahgh,widepdlt,wideplow,widephgh,
     *            kconddlt,kcondlow,kcondhgh,
     *            flz_o,pwr_o,r1n_o,r2n_o,mndr_o,aa2_o,aa3_o,aa4_o,
     *            theta_o,widep_o,kcond_o

!     rev. 9.2.11  Sep.  11/05  - NK: added Manning's n  r1n & r2n
      real*4,    dimension(:), allocatable :: r1n,r2n, 
     *                           r2nlow,r2nhgh,r2ndlt

      real*4,    dimension(:,:), allocatable :: h,fpetmo 

      integer*2,   dimension(:), allocatable :: iiclass

      character*10, dimension(:), allocatable :: nclass,rivtype

      real*4    :: a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,type1

      END MODULE area4

