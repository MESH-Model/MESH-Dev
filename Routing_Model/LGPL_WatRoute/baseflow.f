C    This file is part of WATROUTE.
C
C    WATROUTE is free software: you can redistribute it and/or modify
C    it under the terms of the GNU Lesser General Public License as published by
C    the Free Software Foundation, either version 3 of the License, or
C    (at your option) any later version.
C
C    WATROUTE is distributed in the hope that it will be useful,
C    but WITHOUT ANY WARRANTY; without even the implied warranty of
C    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C    GNU Lesser General Public License for more details.
C
C    You should have received a copy of the GNU Lesser General Public License
C    along with WATROUTE.  If not, see <http://www.gnu.org/licenses/>.

      subroutine baseflow(n,dlz,sdlz,tdum)


!***********************************************************************
!       copyright (c) by Nick Kouwen 1987-2007
!***********************************************************************

!     s/r created May 5/03 NK
!     copyright (c) nick kouwen 2003-

!     rev. 9.4.08  May.  29/07  - NK: changed baseflow argument list

!     called in RUNOF6 and ROUTE

      USE area_watflood
	implicit none

      integer    :: n
      real*4     :: dlz,sdlz,tdum

!     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
!       GROUNDWATER OUTFLOW:
!     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

!       DLZ = LOWER ZONE OUTFLOW IN MM

        dlz=flz2(n)*lzs(n)**pwr(n)

        if(dlz.gt.lzs(n))then
          dlz=lzs(n)
          lzs(n)=0.0
        else
          lzs(n)=lzs(n)-dlz
        endif

c	if(iopt.eq.2)print*,' checkpoint 5a in baseflow'

          qlz(n)=dlz*tdum*frac(n)

	  leakage=leakage+qlz(n)
!        qdrng=qdrng+rechrg(n)*tdum*frac(n)

        if(n.eq.nnprint)then
!       CALC LZ OUTFLOW
          sdlz=sdlz+dlz
        endif

!     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

      return

      end subroutine baseflow