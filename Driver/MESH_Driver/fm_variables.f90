!>
!> Description:
!>  Contains variable types for structures or 'forms' in the model, such
!>  as landmark locations like streamflow gauge, irrigation demand,
!>  lake, and reservoir locations.
!>
!> Instances of these types are accessible by the
!> 'sa_mesh_shared_variables' module: fms%
!>
!> Types:
!*  outlet_location: Location of an outlet location.
!>
module fm_variables

    !> Type: outlet_location
    !>  Attributes of an outlet location.
    !>
    !> Attributes:
    !*  iun: File unit (default: 100).
    !*  fname: File name (default: '' to avoid random characters).
    !*  lopen: Status of file opened, .true. if the file is open and attached 'iun'; .false. otherwise.
    type fm_config_file
        integer :: iun = 100
        character(len = 50) :: fname = ''
        logical :: lopen = .false.
    end type

    !> Type: outlet_location
    !>  Attributes of an outlet location.
    !>
    !> Attributes:
    !*  name: ID printed to output files.
    !*  y: Y-coordinate of outlet location.
    !*  x: X-coordinate of outlet location.
    !*  iy: Vertical index of the grid-cell containing the location.
    !*  jx: Horizontal index of the grid-cell containing the location.
    !*  n: Rank or index of the grid-cell containing the location.
    type outlet_location
        character(len = 8), dimension(:), allocatable :: name
        real(kind = 4), dimension(:), allocatable :: y, x
        integer(kind = 4), dimension(:), allocatable :: iy, jx, rnk
    end type

    !> Type: time_series
    !>  Attributes of a time-series (e.g., observed/measured flow at a location)
    !>
    !> Attributes:
    !*  val: Time-series of data at the location.
    !*  dts: Time-step of the series. [s].
    !*  iyear: Year of the start date of data.
    !*  ijday: Day in the year of the start date of data.
    !*  imonth: Month of the start date of data.
    !*  iday: Day in the month of the start date of data.
    !*  ihour: Hour in the day of the start date of data.
    !*  imins: Minutes in the hour of the start date of data.
    type time_series
        type(fm_config_file) fls
        real(kind = 4), dimension(:), allocatable :: val
        integer :: dts, iyear, ijday, imonth, iday, ihour, imins
    end type

    !> Type: release_outlet
    !>  Attibutes of outlets with release coefficients.
    !>
    !> Attributes:
    !*  cfn: Type of release curve function.
    !*  b: Coefficients.
    type release_outlet
        integer(kind = 4), dimension(:), allocatable :: cfn
        real(kind = 4), dimension(:), allocatable :: b1, b2, b3, b4, b5, lvlz0, area
    end type

    !> Type: streamflow_gauge_location
    !>  Attributes of streamflow gauge locations.
    !>
    !> Indices:
    !*  n: Number of locations dimensioned.
    !>
    !> Attributes:
    !*  DA: Drainage area.
    type streamflow_gauge_location
        integer :: n = 0
        type(outlet_location) meta
        real(kind = 4), dimension(:), allocatable :: da
        type(time_series) qomeas
    end type

    !> Type: reservoir_outlet_location
    !>  Attributes of lake and reservoir outlet locations.
    !>
    !> Indices:
    !*  n: Number of locations dimensioned.
    !>
    !> Attributes:
    type reservoir_outlet_location
        integer :: n = 0
        type(outlet_location) meta
        type(release_outlet) rls
        type(time_series) qorls
    end type

    !> Type: abstraction_point_location
    !>  Attributes of an abstraction point location for reservoir demand.
    !>
    !> Indices:
    !*  n: Number of locations dimensioned.
    !>
    !> Attributes:
    !*  rr: Index of the reservoir for abstraction.
    type abstraction_point_location
        integer :: n = 0
        type(outlet_location) meta
        integer(kind = 4), dimension(:), allocatable :: rr
    end type

    contains

    !> Subroutines to allocate base types.

    subroutine allocate_outlet_location(meta, n, ierr)
        type(outlet_location) meta
        integer n, ierr
        allocate(meta%name(n), meta%y(n), meta%x(n), meta%iy(n), meta%jx(n), meta%rnk(n), stat = ierr)
        if (ierr == 0) then
            meta%name(n) = ''; meta%y(n) = 0.0; meta%x(n) = 0.0; meta%iy(n) = 0; meta%jx(n) = 0; meta%rnk(n) = 0
        end if
    end subroutine

    subroutine allocate_time_series(ts, n, ierr)
        type(time_series) ts
        integer n, ierr
        allocate(ts%val(n), stat = ierr)
        if (ierr == 0) ts%val = 0.0
    end subroutine

    subroutine allocate_release_outlet(rls, n, ierr)
        type(release_outlet) rls
        integer n, ierr
        allocate(rls%cfn(n), rls%b1(n), rls%b2(n), rls%b3(n), rls%b4(n), rls%b5(n), rls%lvlz0(n), rls%area(n), stat = ierr)
        if (ierr == 0) then
            rls%cfn = 0; rls%b1 = 0.0; rls%b2 = 0.0; rls%b3 = 0.0; rls%b4 = 0.0; rls%b5 = 0.0; rls%lvlz0 = 0.0; rls%area = 0.0
        end if
    end subroutine

    !> Subroutines to allocate locations types.

    subroutine allocate_streamflow_gauge_location(stmg, n, ierr)
        type(streamflow_gauge_location) stmg
        integer n, ierr
        stmg%n = n
        call allocate_outlet_location(stmg%meta, n, ierr)
        if (ierr /= 0) return
        allocate(stmg%DA(n), stat = ierr)
        if (ierr /= 0) return
        stmg%DA = 0.0
        call allocate_time_series(stmg%qomeas, n, ierr)
    end subroutine

    subroutine allocate_reservoir_outlet_location(rsvr, n, ierr)
        type(reservoir_outlet_location) rsvr
        integer n, ierr
        rsvr%n = n
        call allocate_outlet_location(rsvr%meta, n, ierr)
        if (ierr /= 0) return
        call allocate_release_outlet(rsvr%rls, n, ierr)
        if (ierr /= 0) return
        call allocate_time_series(rsvr%qorls, n, ierr)
    end subroutine

    subroutine allocate_abstraction_point_location(absp, n, ierr)
        type(abstraction_point_location) absp
        integer n, ierr
        absp%n = n
        call allocate_outlet_location(absp%meta, n, ierr)
        if (ierr /= 0) return
        allocate(absp%rr(n), stat = ierr)
        if (ierr == 0) absp%rr = 0
    end subroutine

end module
