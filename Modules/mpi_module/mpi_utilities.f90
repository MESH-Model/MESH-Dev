!>
!> MPI-related utilities.
!>
module mpi_utilities

    implicit none

    contains

    !> Description:
    !>  Determine il1:il2 indices based on the number of active nodes
    !>  and tiles in the setup for the given node. The subroutine will nudge
    !>  il1 and il2 so that no grid is split between separate nodes.
    !>
    !> Variables:
    !*  inp: Number of nodes (including head node).
    !*  izero: If to include the head node in tile iterations when multiple nodes are used
    !       (default: 0 = reserve head node for between grid processes and book-keeping).
    !*  ipid: Zero-based index of active/current/this node (default: 0 = head node).
    !*  NML: Number of active land tiles in the setup.
    !*  ILMOS: NML-to-Grid lookup table.
    !>
    !> Returns:
    !*  il1: First index to be used in land tile based iterations on this node.
    !*  il2: Last index to be used in land tile based iterations on this node.
    subroutine mpi_split_nml(inp, izero, ipid, &
                             NML, ILMOS, &
                             il1, il2)

        !> Input variables.
        integer, intent(in) :: inp, izero, ipid
        integer, intent(in) :: NML
        integer, intent(in), dimension(:) :: ILMOS

        !> Output variables
        integer, intent(out) :: il1, il2

        !> Calculate an initial lower index.
        il1 = max(min(ceiling(NML/real(inp - izero))*(ipid - izero) + 1, NML), 0)

        !> On succeeding nodes, bump the index to begin at the next grid in
        !> the sequence if otherwise the GRUs and/or tiles of the grid would
        !> be broken across nodes.
        if (ipid > (0 + izero)) then
            do while (ILMOS(il1 - 1) == ILMOS(il1))
                il1 = il1 + 1
            end do
        end if

        !> Calculate an initial upper index.
        il2 = max(min(ceiling(NML/real(inp - izero))*((ipid - izero) + 1), NML), il1)

        !> Bump the index to include the entire grid so that the GRUs and/or
        !> tiles of the grid are not broken across nodes.
        if (ipid < (inp - 1) .and. ipid /= 0) then
            do while (ILMOS(il2) == ILMOS(il2 + 1) .and. il2 < NML)
                il2 = il2 + 1
            end do
        end if

        !> Override for head node so that variables for bookkeeping that are
        !> allocated from il1:il2 are properly allocated 1:NML.
        if (ipid == 0) then
            il1 = 1
            il2 = NML
        end if

    end subroutine

    !> Description:
    !>  Determine iw1:iw2 indices based on the number of active nodes
    !>  and tiles in the setup for the given node. The subroutine will base
    !>  the start and stop indices based on il1 and il2 so that no grid is split
    !>  between separate nodes.
    !>
    !> Variables:
    !*  inp: Number of nodes (including head node).
    !*  izero: If to include the head node in tile iterations when multiple nodes are used
    !       (default: 0 = reserve head node for between grid processes and book-keeping).
    !*  ipid: Zero-based index of active/current/this node (default: 0 = head node).
    !*  NML: Number of active land tiles in the setup.
    !*  NMW: Number of active water tiles in the setup.
    !*  ILMOS: NML-to-Grid lookup table.
    !*  il1: First index to be used in land tile based iterations on this node.
    !*  il2: Last index to be used in land tile based iterations on this node.
    !*  IWMOS: NMW-to-Grid lookup table.
    !>
    !> Returns:
    !*  iw1: First index to be used in water tile based iterations on this node.
    !*  iw2: Last index to be used in water tile based iterations on this node.
    subroutine mpi_split_nmw(inp, izero, ipid, &
                             NML, NMW, ILMOS, il1, il2, IWMOS, &
                             iw1, iw2)

        !> Input variables.
        integer, intent(in) :: NML, NMW, il1, il2, inp, izero, ipid
        integer, intent(in), dimension(:) :: ILMOS, IWMOS

        !> Output variables
        integer, intent(out) :: iw1, iw2

        !> Local variables.
        integer i1, i2, i

        !> Return if no land or water tiles are active.
        if (NMW == 0 .or. NML == 0) then
            iw1 = 0
            iw2 = 0
            return
        end if

        !> Get the grid indices of the land tile based indices.
        i1 = ILMOS(il1)
        i2 = ILMOS(il2)

        !> Get the start and stop indices for water tiles.
        iw1 = 0
        iw2 = 0
        do i = 1, NMW
            if (IWMOS(i) < i1) cycle
            if ((IWMOS(i) >= i1 .and. IWMOS(i) <= i2) .and. iw1 == 0) iw1 = i
            if (IWMOS(i) <= i2) iw2 = i
            if (IWMOS(i) > i2) exit
        end do
        if (iw2 == 0) iw2 = iw1

    end subroutine

end module
