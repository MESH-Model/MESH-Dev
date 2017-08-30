!>
!> MPI-related variables.
!>
module mpi_shared_variables

    implicit none

    !> Information about the node(s).
    !*  inp: Number of nodes (including head node).
    !*  ipid: Zero-based index of active/current/this node (default: 0 = head node).
    integer :: inp = 1, ipid = 0

    !*  izero: If to include the head node in tile iterations when multiple nodes are used
    !       (default: 0 = reserve head node for between grid processes and book-keeping).
    integer izero

    !* Grid-based indices.
    !*  i1: First index to be used in grid iterations on this node.
    !*  i2: Last index to be used in grid iterations on this node.
    integer i1, i2

    !> Land tile-based indices.
    !*  il1: First index to be used in tile iterations on this node.
    !*  il2: Last index to be used in tile iterations on this node.
    integer il1, il2

    !> Water tile-based indices.
    !*  iw1: First index to be used in tile iterations on this node.
    !*  iw2: Last index to be used in tile iterations on this node.
    integer iw1, iw2

end module
