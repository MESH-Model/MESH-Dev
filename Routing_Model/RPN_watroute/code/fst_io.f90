! Dummy file to lump all of the RPN-standard I/O utilities in one
! single module for inclusion.  This way, a separate 'stub' module
! implementing the same interface can be used on systems that do
! not have rmnlib available. 

module fst_io

   use read_fst
   use write_fst
   use read_shed_fst
   use read_flowinit_fst
   use write_flowinit_fst

end module fst_io
