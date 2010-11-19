program zlatlon
  implicit none
  
  ! This program demonstrates the creation of a 'Z' grid mapped on a latlon grid.
  !
  ! The size of the grid, southwest corner (in latlon coords) and grid length are 
  ! defined by the calling arguments
  !
  ! If no arguments are given, then a 401x201 grid, starting at (40N,270W) and with a grid length
  ! of 0.1 deg in each dimension is defined.
  !
  ! The program afterwards presents the grid to the ezscint package, from which it extracts
  ! the latitudes and longitudes of the grid and appends them to the the standard file containing the
  ! positional records
  !
  ! Yves Chartier - RPN - November 2000


  integer ni, nj, nk
  
  integer ezqkdef, gdll
  
  real, dimension(:,:), allocatable :: zlat, zlon
  real, dimension(:), allocatable:: xaxis, yaxis
  
  integer  ier, nrecs, i, j, npak, iun
  integer  fnom, fstouv, fclos, fstfrm, fstecr, newdate
  external fnom, fstouv, fclos, fstfrm, fstecr, newdate
  
  character*2 nomvar
  character*1 typvar,grtyp,grref
  character*8 etiket 
  
  integer ip1,ip2,ip3,gdin
  integer ip1z,ip2z,ip3z
  integer deet, npas, nbits, datyp
  integer yyyymmdd,hhmmssss,dateo
  integer ig1, ig2, ig3, ig4, date
  real swlat, swlon, dlat, dlon
  real swlatref, swlonref, dlatref, dlonref
  integer npts,nnpas,idt,ipos
  logical rewrit
    
  character * 8   cle(7)
  character * 128 def(7), val(7)
  
  ! Definition of defaults values if no argument is given
  !
  data cle /'fst.',  'ni',  'nj', 'swlat', 'swlon', 'dlat', 'dlon'/
  data def /'SCRAP', '401', '201', '40.',  '270.',  '0.1',  '0.1'/
  data val /'SCRAP', '401', '201', '40.',  '270.',  '0.1',  '0.1'/
  
  !------------------------------------------------------------------------------------------------
  call ccard(cle,def,val, 7, ipos)
  
  read (val(2),*) ni
  read (val(3),*) nj
  read (val(4),*) swlat
  read (val(5),*) swlon
  read (val(6),*) dlat
  read (val(7),*) dlon
  
  iun = 1
  nk  = 1

  ! creates the standard file

  ier = fnom(1, val(iun),'STD+RND',0)
  nrecs = fstouv(1, 'RND')
  
  allocate(zlat(ni,nj))
  allocate(zlon(ni,nj))
  allocate(xaxis(ni))
  allocate(yaxis(nj))

  ! Initialisation of the axes values - pretty straight forward calculation

  do i=1,ni
     xaxis(i) = swlon + dlon * (i-1)
  enddo
  
  do j=1,nj
     yaxis(j) = swlat + dlat * (j-1)
  enddo

  !- computation of ig parameters for defining a z-grid in a latlon space
  !  The values are such that the lat-lon can be encoded "as is" in the navigational records
  !  without supplemental conversion

  swlatref = 0.0
  swlonref = 0.0
  dlonref = 1.0
  dlatref = 1.0

  call cxgaig('L', ig1, ig2, ig3, ig4, swlatref, swlonref, dlatref, dlonref)

  ! Initialization of the necessary standard file parameters required to write a record

  npak = -24
  deet = 0
  npas = 0
  ip1z  = 2000
  ip2z  = 2001
  ip3z  = 2002
  dateo = 0
  typvar = 'P'
  grref  = 'L'
  etiket = 'ZLATLON'
  datyp  = 1
  rewrit = .false.

  ! definition of dateo

  yyyymmdd = 20001102
  hhmmssss = 00000000
  ier = newdate(dateo,yyyymmdd,hhmmssss,3)

  ! Write the navigational records into the standard file

  nomvar = '>>'
  ier=fstecr(xaxis, xaxis, npak, iun, dateo, deet, npas, ni, 1, nk, &
       ip1z, ip2z, ip3z, typvar, nomvar, etiket, grref, ig1, ig2, ig3, ig4, datyp, rewrit)
  
  nomvar = '^^'
  ier=fstecr(yaxis, yaxis, npak, iun, dateo, deet, npas, 1, nj, nk, &
       ip1z, ip2z, ip3z, typvar, nomvar, etiket, grref, ig1, ig2, ig3, ig4, datyp, rewrit)

  ! This program could stop here if all we wanted to do is write the navigational records
  !------------------------------------------------------------------------------------------------
  !

  !  Now we initialize the ezscint package to get the lat-lon of the grid we just defined
  !  For more info on ezqkdef and gdll
  !      consult http://iweb.cmc.ec.gc.ca/rpn/eng/si/rmnlib/ezscint/index.html
  !
  
  ! define the grid

  grtyp   = 'Z'
  gdin    = ezqkdef(ni,nj,grtyp, ip1z, ip2z, ip3z, 0, iun)

  ! get the latlon of the grid we just defined

  ier     = gdll(gdin, zlat, zlon)
  

  ! Write the contents of 'LA' and 'LO' in the standard file.

  ip1 = 0
  ip2 = 0
  ip3 = 0

  nomvar = 'LA'
  ier=fstecr(zlat, zlat, npak, iun, dateo, deet, npas, ni, nj, nk, &
       ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ip1z, ip2z, ip3z, 0, datyp, rewrit)

  nomvar = 'LO'
  ier=fstecr(zlon, zlon, npak, iun, dateo, deet, npas, ni, nj, nk, &
       ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ip1z, ip2z, ip3z, 0, datyp, rewrit)
  
  ! get statistics about the data just written

  call statfld('LA','P',ip1,ip2,ip3,date,etiket,zlat,ni, nj, nk) 
  call statfld('LO','P',ip1,ip2,ip3,date,etiket,zlon,ni, nj, nk) 

  ! close the file and all is done.

  ier = fstfrm(iun)
  ier = fclos(iun)

  !------------------------------------------------------------------------------------------------
  stop
end program zlatlon
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
