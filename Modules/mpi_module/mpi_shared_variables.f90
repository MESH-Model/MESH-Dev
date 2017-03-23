!>
!> MPI-related variables.
!>
module mpi_shared_variables

    implicit none

    !> Information about the node(s).
    !*  inp: Number of nodes (including head node).
    !*  ipid: Zero-based index of active/current/this node (default: 0 = head node).
    integer :: inp = 1, ipid = 0

    !> Indices.
    !*  izero: If to include the head node in tile iterations when multiple nodes are used
    !       (default: 0 = reserve head node for between grid processes and book-keeping).
    !*  il1: First index to be used in tile iterations on this node.
    !*  il2: Last index to be used in tile iterations on this node.
    !*  ilen: Total number of indices active on this node.
    integer izero, il1, il2, ilen

end module
