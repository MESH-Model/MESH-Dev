      subroutine weight(ng,xsta,ysta,ixr,iyr,w)

c - this subroutine calculates weights for each grid square for
c   each rain gauge.

      dimension d(ng)
      real xsta(ng),ysta(ng)
      real*8 w(iyr,ixr,ng)
c      dimension d(500),xsta(500),ysta(500)

c	nw is the distance weighting factor
c	nw=4    now in area16.for

	if(nw.lt.1.or.nw.gt.10)then
         nw=2
          print*, ' distance weighting exponent not entered'
          print*, ' or outside reasonable range (1-8)'
          print*, ' exponent set to default = 2'
          print*, ' '
          pause ' Hit enter to continue'
	endif

c initialize weights

             do 120 is=1,ng
                do 120 i=1,iyr
                   do 120 j=1,ixr
                      w(i,j,is)=0.0
120          continue

c if there is only one station all weights = 1.0

      if(ng.eq.1)then
        do 200 i=1,iyr
          do 200 j=1,ixr
             w(i,j,1)=1.0
200     continue
        return
      endif

c loop through grid squares

      do 1000 i=1,iyr
         do 1000 j=1,ixr
c loop through rain gauges
          ii=0
          sum=0.0 
          do 300 is=1,ng
              x=xsta(is)-float(j)
              y=ysta(is)-float(i)
              d(is)=sqrt(x*x+y*y)
              if(d(is).le.0.99)then
                ii=ii+1
              else
                sum=sum+1.0/d(is)**nw
              endif
300       continue

         do 700 is=1,ng
              if(d(is).le.0.99)then
                w(i,j,is)=1.0/float(ii)
              else
                if(ii.lt.1)then
c		      there is no station in this element
                  w(i,j,is)=1.0/(d(is)**nw)/sum
                endif
              endif
700       continue
1000  continue

      return
      end
