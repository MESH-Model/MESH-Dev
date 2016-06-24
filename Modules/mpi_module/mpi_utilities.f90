module mpi_utilities

    contains

    subroutine mpi_split_nml(inp, izero, ipid, &
                         NML, ILMOS, &
                         il1, il2, ilen)

        integer, intent(in) :: inp, izero, ipid
        integer, intent(in) :: NML
        integer, intent(in), dimension(:) :: ILMOS

        integer, intent(out) :: il1, il2, ilen

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
        if (ipid < (inp - 1)) then
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

        !> Calculate the total number of active elements in the sequence.
        ilen = (il2 - il1) + 1

    end subroutine !mpi_split_nml

end module !mpi_utilities