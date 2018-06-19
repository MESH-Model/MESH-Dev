!> Description:
!>  Stop the program with non-zero exit.
subroutine program_abort()

    !> 'mpi_module': For call to 'MPI_Finalize', 'ipid', and 'inp'.
    !> 'print_routines: For print routines.
    use mpi_module
    use print_routines

    implicit none

    !> Local variables.
    integer ierrcode, ierr

    !> Print message.
    call print_message('Abnormal exit.')

    !> Stop with non-zero exit.
    stop -1

end subroutine
