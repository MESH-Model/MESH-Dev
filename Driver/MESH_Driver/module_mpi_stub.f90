!>
!> Stub for MPI subroutines if an MPI compiler cannot be used.
!>
module module_mpi

    implicit none

    integer :: &
        MPI_COMM_WORLD = 0, MPI_SUCCESS = 0, MPI_REQUEST_NULL = 0, MPI_STATUS_SIZE = 0, &
        MPI_REAL = 0, MPI_INTEGER = 0

    contains

    subroutine MPI_Comm_size(comm, size, ierr)
        integer comm, size, ierr
        size = 1
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Comm_rank(comm, rank, ierr)
        integer comm, rank, ierr
        rank = 0
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Init(ierr)
        integer ierr
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Finalize(ierr)
        integer ierr
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Barrier(comm, ierr)
        integer comm, ierr
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Isend(buf, count, datatype, dest, tag, comm, request, ierr)
        real buf(:)
        integer count, datatype, dest, tag, comm, request, ierr
        request = MPI_REQUEST_NULL
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Testall(count, array_of_requests, flag, array_of_statuses, ierr)
        integer count, array_of_requests(:), array_of_statuses(:, :), ierr
        logical flag
        array_of_statuses = MPI_SUCCESS
        flag = .true.
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Irecv(buf, count, datatype, source, tag, comm, request, ierr)
        real buf(:)
        integer count, datatype, source, tag, comm, request, ierr
        request = MPI_REQUEST_NULL
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Abort(comm, errorcode, ierr)
        integer comm, errorcode, ierr
        ierr = MPI_SUCCESS
    end subroutine

end module !module_mpi
