!> Description:
!>  Stop the program.
subroutine stop_program()

    !> Local variables.
    integer ierr

    !> Finalize MPI processes.
    call mpi_finalize(ierr)
    if (ierr /= 0) print 1000, 'MPI exchange failed to exit with normal status.'

    !> Stop the program.
    stop

1000    format(1x, 'WARNING :', (a))

end subroutine
