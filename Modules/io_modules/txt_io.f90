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
        module procedure read_records_txt_multi_line_double
        module procedure read_records_txt_multi_line_float
        module procedure read_records_txt_single_line_double
        module procedure read_records_txt_single_line_float
    end interface

    contains

    integer function read_records_txt_multi_line_double(iun, dvals, nrows) result(ierr)

        implicit none

        !> Input/output variables.
        integer, intent(in) :: iun, nrows
        real(kind = 8) dvals(:)

        !> Local variables.
        integer i

        !> Initialize return variable.
        ierr = 0

        !> Read 'nrows' number of rows.
        do i = 1, max(0, nrows - 1)
            read(iun, *, iostat = ierr)
            if (ierr /= 0) exit
        end do

        !> Read record to array using free format.
        read(iun, *, iostat = ierr) (dvals(i), i = 1, size(dvals))

    end function

    integer function read_records_txt_multi_line_float(iun, fvals, nrows) result(ierr)

        implicit none

        !> Input/output variables.
        integer, intent(in) :: iun, nrows
        real(kind = 4) fvals(:)

        !> Local variables.
        real(kind = 8), allocatable :: dvals(:)

        !> Allocate and initialize local variable.
        allocate(dvals(size(fvals)))
        dvals = 0.0

        !> Call main subroutine to read records.
        ierr = read_records_txt_multi_line_double(iun, dvals, 1)

        !> Convert field.
        if (ierr == 0) then
            fvals = real(dvals, kind = 4)
        end if

    end function

    integer function read_records_txt_single_line_double(iun, dvals) result(ierr)

        implicit none

        !> Input/output variables.
        integer, intent(in) :: iun
        real(kind = 8) dvals(:)

        !> Call main subroutine to read a single record.
        ierr = read_records_txt_multi_line_double(iun, dvals, 1)

    end function

    integer function read_records_txt_single_line_float(iun, fvals) result(ierr)

        implicit none

        !> Input/output variables.
        integer, intent(in) :: iun
        real(kind = 4) fvals(:)

        !> Local variables.
        real(kind = 8), allocatable :: dvals(:)

        !> Allocate and initialize local variable.
        allocate(dvals(size(fvals)))
        dvals = 0.0

        !> Call main subroutine to read a single record.
        ierr = read_records_txt_multi_line_double(iun, dvals, 1)

        !> Convert field.
        if (ierr == 0) then
            fvals = real(dvals, kind = 4)
        end if

    end function

end module
