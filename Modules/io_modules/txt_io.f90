!>
!> Description:
!>  Module that contains functions and subroutines pertaining to reading
!>  and writing fields from space or comma delimited text files.
!>
module txt_io

    implicit none

    !> Interface: read_records_txt
    !*  read_records_txt_N: Can read multiple lines of record; data saved
    !*                      from only the last line read.
    !*  read_records_txt_1: Reads a single line of record.
    !>
    !> Description:
    !>  Function to read comma or space delimited records from a text
    !>  file, provided the unit. Stores the last read record in the array
    !>  provided. Returns the status of the read statement.
    !>
    !> Input/output:
    !>  iun: Unit of the file.
    !>  nf: Number of columns to read.
    !>  vals: Values read from file (dimension: nf).
    !>  nr: Number of records to read, if to skip lines (optional).
    !>  ierr: Status of the read statement.
    !>
    interface read_records_txt
        module procedure read_records_txt_N
        module procedure read_records_txt_1
    end interface

    contains

    integer function read_records_txt_N(iun, vals, nr) result(ierr)

        implicit none

        !> Input/output variables.
        integer, intent(in) :: iun, nr
        real vals(:)

        !> Local variables.
        integer i

        !> Skip records if 'nr' is provided.
        do i = 1, max(0, nr - 1)
            read(iun, *, iostat = ierr)
            if (ierr /= 0) exit
        end do

        !> Read record to array using free format.
        read(iun, *, iostat = ierr) (vals(i), i = 1, size(vals))

    end function

    integer function read_records_txt_1(iun, vals) result(ierr)

        implicit none

        !> Input/output variables.
        integer, intent(in) :: iun
        real vals(:)

        !> Call main subroutine to read a single record.
        ierr = read_records_txt_N(iun, vals, 1)

    end function

end module
