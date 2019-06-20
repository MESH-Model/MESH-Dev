!> Description:
!>  Stop the program with normal exit.
subroutine program_end()

    !> 'mpi_module': For call to 'MPI_Finalize' and 'inp'.
    !> 'print_routines: For print routines.
    use mpi_module
    use print_routines

    implicit none

    !> Local variables.
    integer ierr, istat

    !> Finalize MPI processes.
    if (inp > 1) then
        call MPI_Finalize(ierr)
        if (ierr /= MPI_SUCCESS) call print_warning('MPI exchange failed to exit with normal status.')
    end if

    !> Stop the program.
    stop

end subroutine
