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
    !> Indices:
    !*  n: Number of elements dimensioned.
    !>
    !> Attributes:
    !*  name: ID printed to output files.
    !*  y: Y-coordinate of outlet location.
    !*  x: X-coordinate of outlet location.
    !*  iy: Vertical index of the grid-cell containing the location.
    !*  jx: Horizontal index of the grid-cell containing the location.
    !*  n: Rank or index of the grid-cell containing the location.
    type outlet_location
        integer :: n = 0
        character(len=8), dimension(:), allocatable :: name
        real(kind = 4), dimension(:), allocatable :: y, x
        integer(kind = 4), dimension(:), allocatable :: iy, jx, rnk
    end type

    !> Type: streamflow_gauge (extends: outlet_location)
    !>  Attributes of streamflow gauge locations.
    !>
    !> Attributes:
    !*  DA: Drainage area.
    type, extends(outlet_location) :: streamflow_gauge
        real(kind = 4), dimension(:), allocatable :: DA
    end type

    !> Type: lake_outlet (extends: outlet_location)
    !>  Attibutes of lakes.
    !>
    !> Attributes:
    !*  cfn: Type of release curve function.
    type, extends(outlet_location) :: lake_outlet
        integer(kind = 4), dimension(:), allocatable :: cfn
    end type

    !> Type: reservoir_outlet (extends: lake_outlet)
    !>  Attributes of reservoir locations.
    !>
    !> Attributes:
    type, extends(lake_outlet) :: reservoir_outlet

    end type

    !> Type: abstraction_point (extends: outlet_location)
    !>  Attributes of an abstraction point for reservoir demand.
    !>
    !> Attributes:
    !*  rr: Index of the reservoir for abstraction.
    type, extends(outlet_location) :: abstraction_point
        integer(kind = 4), dimension(:), allocatable :: rr
    end type

end module
