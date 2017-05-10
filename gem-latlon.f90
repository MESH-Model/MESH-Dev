program gemlatlon
  implicit none
  
  integer nigem, njgem
  
  integer ezqkdef, ezuvint, ezdefset, gdll, ezwdint
  
  character * 28   cle(1)
  character * 128 def(1), val(1)
  real a
  
  real, dimension(:,:), allocatable :: gemlat, gemlon
  
  integer gzkey,vvkey
  integer nigem, njgem, nkgem, gdin
  
  
  integer  ier, nrecs
  integer  fnom, fstouv, fclos, fstfrm, fstinf, fstluk, fstprm
  external fnom, fstouv, fclos, fstfrm, fstinf, fstluk, fstprm
  
  character*2 nomvar
  character*1 typvar,grtyp
  character*8 etiket 
  
  character *1 grsrc
  
  integer ip1,ip2,ip3
  integer extra1, extra2, extra3, deet, npas, nbits, datyp, dateo
  integer ig1src, ig2src, ig3src, ig4src, date
  integer swa, lng, dltf, ubc
  integer npts,nnpas,idt,ipos
  
  
  !     
  data cle /'IMFLDS.'/
  data def /'SCRAP'/
  data val /'SCRAP'/
  
  !------------------------------------------------------------------------------------------------

!  call ccard(cle,def,val, 1, ipos)
  
!  print *, val(1)
  ier = fnom(1,'pr_an_2009053118_','STD+RND+OLD+R/O',0)
  nrecs = fstouv(1, 'RND')
  
  gzkey=fstinf(1,nigem,njgem,nkgem,-1,'        ',-1,-1,-1,'A','PR')
  
  ier=fstprm(gzkey, dateo, deet, npas, nigem, njgem, nkgem, nbits, datyp, &
       ip1, ip2, ip3, typvar, nomvar, etiket, &
       grsrc, ig1src, ig2src, ig3src,&
       ig4src, swa, lng, dltf, ubc, extra1, extra2, extra3)
  print*,nigem,njgem
  allocate(gemlat(nigem,njgem))
  allocate(gemlon(nigem,njgem))
  
  print *, ip1, ip2, ip3, typvar, nomvar, etiket, grsrc, ig1src, ig2src, ig3src, ig4src
  
  !------------------------------------------------------------------------------------------------
  gdin    = ezqkdef(nigem,njgem,grsrc,ig1src,ig2src,ig3src,ig4src,1)
  ier     = gdll(gdin, gemlat, gemlon)
  
  call statfld('LA','P',ip1,ip2,ip3,date,etiket,gemlat,nigem, njgem, nkgem) 
  call statfld('LO','P',ip1,ip2,ip3,date,etiket,gemlon,nigem, njgem, nkgem) 

  !------------------------------------------------------------------------------------------------
  stop
end program gemlatlon
subroutine statfld(nomvar,typvar,ip1,ip2,ip3,date,etiket,f,ni,nj,nk) 
  implicit none
  character*2 nomvar
  character*1 typvar
  integer ip1,ip2,ip3,date
  character*8 etiket
  ! 
  integer ni,nj,nk
  real f(ni,nj,nk) 
  !
  !OBJECT
  !     calcule et imprime: la moyenne    (moy)
  !                         la variance   (var)
  !                         le minimum et le maximum
  !     du champ f   
  ! 
  !     arguments:
  !         - f       - champ sur lequel on veut faire des statistiques
  !         - n       - dimensions du champ f
  !         - champ   - identification du champ
  !         - no      - compteur 
  !         - from    - identification du module d'ou on fait l'appel 
  !
  !METHOD
  !
  !EXTERNALS
  !
  !AUTHOR   Michel Desgagne                   Nov   1992
  !
  !HISTORY
  !
  !*
  integer i,j,k
  real sum,moy,var,rmin,rmax
  integer imin,jmin,kmin,imax,jmax,kmax
  !--------------------------------------------------------------------
  !
  ! ** On calcule la moyenne.
  !
  sum = 0.0
  do k=1,nk
     do j=1,nj
        do i=1,ni
           sum = sum + f(i,j,k)
        enddo
     enddo
  enddo
  moy = sum / float(ni*nj*nk)
  !
  ! ** On calcule la variance
  !
  sum = 0.0
  do k=1,nk
     do j=1,nj
        do i=1,ni
           sum = sum + ((f(i,j,k) - moy)*(f(i,j,k) - moy))
        enddo
     enddo
  enddo


  var = sqrt (sum / float(ni*nj*nk))
  !
  ! ** On identifie le minimum et le maximum.
  !
  imin = 1
  jmin = 1
  kmin = 1
  imax = 1
  jmax = 1
  kmax = 1
  rmax = f(1,1,1)
  rmin = f(1,1,1)
  !
  do k=1,nk
     do j=1,nj
        do i=1,ni
           if (f(i,j,k) .gt. rmax) then
              rmax  = f(i,j,k)
              imax = i
              jmax = j
              kmax = k
           endif
           if (f(i,j,k) .lt. rmin) then
              rmin  = f(i,j,k)
              imin = i
              jmin = j
              kmin = k
           endif
        enddo
     enddo
  enddo

  !       
  ! ** On imprime
  ! 
  write(6,10) nomvar,typvar,ip1,ip2,ip3,date,etiket, moy,var,imin,jmin+(kmin-1)*nj,rmin,&
       imax,jmax+(kmax-1)*nj,rmax
10 format (' ',a2,1x,a1,1x,i5,1x,i4,1x,i3,1x,i9,1x,a8,1x,&
       ' Mean:',e12.6,'  Var:',e12.6,&
       '  Min:[(',i3,',',i3,'):',&
       e10.4,']',' Max:[(',i3,',',i3,'):',&
       e10.4,']')
  !
  !----------------------------------------------------------------
  return
end subroutine statfld

