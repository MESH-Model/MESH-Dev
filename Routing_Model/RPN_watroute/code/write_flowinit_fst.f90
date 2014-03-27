! csubich: write_flowinit_fst is a subroutine to output the new set
! of initial conditions to a file of '.fst' format, creating the file
! if necessary.  It is designed as a straight-up replacement for the
! existing write_flowinit subroutine, which outputs to a text-based
! r2c file format -- this has obvious scalability problems for large
! grids.

! By watroute's design, the arrays written out to disk are global
! variables, structured as very long, 1D arrays.  This code will
! re-two-dimensionalize them to the global array 'outarray', which
! is in turn used by write_fst for disk I/O.

module write_flowinit_fst

contains 

subroutine write_flowinit_fst(unitNum, flname, iyear, imonth, iday, ihour)

   use area_watflood
   use write_fst

   integer :: unitNum, & ! Unit number
              iyear, imonth, iday, ihour, & ! Date variables
              jj ! Loop index

   character :: flname*(*) ! Filename


   ! Initial grid inflow: qi1

   forall (jj=1:ubound(qi1,1)) outarray(yyy(jj),xxx(jj))=qi1(jj)

   call write_fst(unitNum,flname,'qi1 ',iyear,imonth,iday,ihour)

   ! Initial grid outflow: qo1

   forall (jj=1:ubound(qo1,1)) outarray(yyy(jj),xxx(jj))=qo1(jj)
   call write_fst(unitNum,flname,'qo1 ',iyear,imonth,iday,ihour)

   ! Grid channel storage: store1 -> 'stor'

   forall (jj=1:ubound(store1,1)) outarray(yyy(jj),xxx(jj))=store1(jj)
   call write_fst(unitNum,flname,'stor',iyear,imonth,iday,ihour)

   ! Overbank storage: over
   forall (jj=1:ubound(over,1)) outarray(yyy(jj),xxx(jj))=over(jj)
   call write_fst(unitNum,flname,'over',iyear,imonth,iday,ihour)

   ! Lower zone storage: lzs
   forall (jj=1:ubound(lzs,1)) outarray(yyy(jj),xxx(jj))=lzs(jj)
   call write_fst(unitNum,flname,'lzs ',iyear,imonth,iday,ihour)
end subroutine

end module write_flowinit_fst
