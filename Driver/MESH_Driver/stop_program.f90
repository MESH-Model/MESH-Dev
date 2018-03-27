!> Description:
!>  Stop the program.
subroutine stop_program()

    !> 'mpi_module': Required for call to 'MPI_Finalize'.
    use mpi_module

    !> Local variables.
    integer ierr

    !> Finalize MPI processes.
    call MPI_Finalize(ierr)
    if (ierr /= 0) print 1000, 'MPI exchange failed to exit with normal status.'

    !> Stop the program.
    stop

1000    format(1x, 'WARNING :', (a))

end subroutine
