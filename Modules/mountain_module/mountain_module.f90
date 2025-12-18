!> Description:
!>  Calculate theoretical shortwave radiation using method proposed by
!>  Garnier and Ohmura (1970) to adjust for slope and aspect.
!>  Calls "calc_rsrd_adjusted".
!>
!> Author: Zelalem Tesemma (of Solar_Adjust.m; last updated Jan 30, 2018)
!>
!> Notes:
!>  - 2018/02/01: Converted to Fortran (exact copy)
!>  - 2018/02/02: Fortran code optimized/consolidated
!>      ('program' component replaced by 'solar_adjust_module')
!>  - 2019/10/10: Upgraded into Mountain MESH (renamed 'mountain_module')
module mountain_module

    implicit none

    !> Description:
    !> Type for parameters (options).
    !>
    !> Variables:
    !*  Curveweight: wind model curvature weight
    !*  CalcFreq: Iterations per day (must divide the day into equal increments of minutes). [--].
    !*  ilapse: Flag to specify lapse rate table. [--].
    !>      0: None.
    !>      1: Use mean annual lapse rate derived from 2.5km GEM (Oct, 2016 to Sept, 2019). (default).
    !>      2: Use monthly lapse rate derived from 2.5km GEM (Oct, 2016 to Sept, 2019).
    !>      3: Use table based on literature values.
    !*  ipre: Flag to specify precipitation adjustment method. [--].
    !>      0: None.
    !>      1: Adjustment based on Thornton, 1997. (default).
    !>      2: Lapse-rate adjustment.
    !*  itemp: Flag to specify temperature adjustment method. [--].
    !>      0: None.
    !>      1: Lapse-rate adjustment. (default).
    !*  ipres: Flag to specify pressure adjustment method. [--].
    !>      0: None.
    !>      1: Elevation adjustment. (default).
    !*  ihumd: Flag to specify specific humidity adjustment method. [--].
    !>      0: None.
    !>      1: Adjustment based on Murray, 1967. (default).
    !>      2: Adjustment based on Kunkel, 1989.
    !*  irlds: Flag to specify longwave radiation adjustment method. [--].
    !>      0: None.
    !>      1: Lapse-rate adjustment. (default).
    !>      2: Adjustment based on Abramowitz et al., 2012.
    !*  iwind: Flag to specify wind speed adjustment method. [--].
    !>      0: None.
    !>      1: Adjustment based on Liston and Sturm, 1998 (requires wind direction, winddir). (default).
    !>      2: Lapse-rate adjustment.
    !*  iphase: Flag to specify precipitation phase partitioning method. [--].
    !>      0: Partioning to 0.0 degrees C.
    !>      1: Partitioning based on Harder and Pomeroy, 2013. (default).
    !*  irsrd: Flag to specify shortwave radiation adjustment method. [--].
    !>      0: None.
    !>      1: Adjustment based on Garnier and Ohmura, 1970. (default).
    !*  iconsmm: Flag to specify run of conservative Mountain MESH (Conserve the Grid/Subbasin forcing field ). [--].
    !>      0: No conservation.
    !>      1: Conservation of the forcing field.
    !*  elev: Weighted average elevation of GRUs. [m].
    !*  delta: Weighted average elevation difference between high (MESH) and low (GEM) resolution elevation. [m].
    !*  slope: Weighted average slope of surface (1: grid; 2: GRU). [degrees].
    !*  aspect: Weighted average aspect of surface (1: grid; 2: GRU). [degrees].
    !*  curvature: Weighted average curvature of the surface. [--].
    !*  skyviewfactor: Weighted average skyviewfactor of the surface. [--].
    !*  gru_frac: Fraction GRUs in the modelling subbasin / grid.
    !*  tlapse: Table of lapse rate values for temperature. [--].
    !*  dtlapse: Table of vapor pressure coefficient [km**-1].
    !*  plapse: Table of lapse rate values for precipitation. [--].
    !*  lwlapse: Table of lapse rate values for longwave radiation. [--].
    !*  wlapse: Table of lapse rate values for wind speed. [--].
    type mountain_parameters
        real :: CurveWeight = 0.50
        integer :: CalcFreq = 288
        integer :: ilapse = 1
        integer :: ipre = 1
        integer :: itemp = 1
        integer :: ipres = 1
        integer :: ihumd = 1
        integer :: irlds = 1
        integer :: iwind = 1
        integer :: iphase = 1
        integer :: irsrd = 1
        integer :: iconsmm = 1
        real, dimension(:, :), allocatable :: elev, slope, aspect, delta, curvature, skyviewfactor, gru_frac
        real, dimension(1440) :: tlapse = (/ &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
        real, dimension(288) :: dtlapse = (/ &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
        real, dimension(12) :: plapse = (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
        real, dimension(12) :: lwlapse = (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
        real, dimension(12) :: wlapse = (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
    end type

    !> Description:
    !> Type for variables/constants.
    !>
    !> Variables:
    !*  xlng: Longitude. [degrees].
    !*  ylat: Latitude. [degrees].
    !*  elev: Weighted average elevation. [m].
    !*  slope: Weighted average slope of the surface. [degrees].
    !*  aspect: Weighted average aspect of the surface. [degrees].
    !*  delta: Weighted average elevation difference between high (MESH) and low (GEM) resolution elevation. [m].
    !*  curvature: Weighted average curvature of the surface. [--].
    !*  skyviewfactor: Weighted average skyviewfactor of the surface. [--].
    type mountain_variables
        real, dimension(:), allocatable :: &
            elev, xlng, ylat, slope, aspect, delta, curvature, skyviewfactor, gru_frac
    end type

    !> Description:
    !> Type for 'mountain' parameters and variables.
    !>
    !> Variables:
    !*  pm: Parameters and options.
    !*  vs: Variables.
    !*  PROCESS_ACTIVE: .true. to enable 'MOUNTAINMESH'; .false. otherwise (default: .false.).
    type mountain_container
        type(mountain_parameters) pm
        type(mountain_variables) vs
        logical :: PROCESS_ACTIVE = .false.
        character(len = 1000) :: RUNOPTIONSFLAG = ''
    end type

    !* fsadj: Instance of 'mountain' parameters and variables.
    type(mountain_container), save :: mountain_mesh

    contains

    subroutine mountain_extract_value(arg, ierr)

        !> Required for the 'parse', 'lowercase', and 'value' functions.
        use strings

        !> Input variables.
        character(len = *), intent(in) :: arg

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer nargs, z
        character(len = len(arg)) args(50)

        !> Status.
        ierr = 0

        !> Return argument contains no '=' (designates option).
        if (scan(arg, '=') == 0) return

        !> Parse the option (using '=').
        !> Return if there is no value assigned to the option.
        call parse(arg, '=', args, nargs)
        if (nargs <= 1) return

        !> Assign the value.
        z = 0
        select case (lowercase(args(1)))
            case ('curveweight')
                call value(args(2), mountain_mesh%pm%CurveWeight, z)
            case ('calcfreq')
                call value(args(2), mountain_mesh%pm%CalcFreq, z)
            case ('ilapse')
                call value(args(2), mountain_mesh%pm%ilapse, z)
            case ('ipre')
                call value(args(2), mountain_mesh%pm%ipre, z)
            case ('itemp')
                call value(args(2), mountain_mesh%pm%itemp, z)
            case ('ipres')
                call value(args(2), mountain_mesh%pm%ipres, z)
            case ('ihumd')
                call value(args(2), mountain_mesh%pm%ihumd, z)
            case ('irlds')
                call value(args(2), mountain_mesh%pm%irlds, z)
            case ('iwind')
                call value(args(2), mountain_mesh%pm%iwind, z)
            case ('iphase')
                call value(args(2), mountain_mesh%pm%iphase, z)
            case ('irsrd')
                call value(args(2), mountain_mesh%pm%irsrd, z)
            case ('iconsmm')
                call value(args(2), mountain_mesh%pm%iconsmm, z)
            case default
                ierr = 2
        end select

        !> Check for conversion error.
        if (z /= 0) ierr = 1

    end subroutine

    subroutine mountain_parse_options(flg, ierr)

        !> Required for MESH 'print_warning' routine.
        use sa_mesh_common

        !> Required for the 'parse', 'lowercase', and 'uppercase' functions.
        use strings

        !> Input variables.
        character(len = *), intent(in) :: flg

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer nargs, i, z
        character(len = 20) args(20)

        !> Initialize the error variable.
        ierr = 0

        !> Assume if the flag is populated that the routine is enabled.
        !> Disabled if the 'off' keyword provided.
        if (len_trim(flg) > 0) then
            mountain_mesh%PROCESS_ACTIVE = .true.
        else
            return
        end if

        !> Parse the flag.
        call parse(flg, ' ', args, nargs)

        !> Check the keywords.
        do i = 2, nargs

            !> Exit if any of the keywords have disabled the routine.
            if (.not. mountain_mesh%PROCESS_ACTIVE) return

            !> Reset the error variable.
            z = 0

            !> Specific options.
            select case (lowercase(args(i)))
                case ('off', '0')

                    !> 'off' or '0' disables the routine.
                    mountain_mesh%PROCESS_ACTIVE = .false.
                    exit
                case ('none')

                    !> Disable all options (to simplify enabling a subset).
                    mountain_mesh%pm%ilapse = 0
                    mountain_mesh%pm%ipre = 0
                    mountain_mesh%pm%itemp = 0
                    mountain_mesh%pm%ipres = 0
                    mountain_mesh%pm%ihumd = 0
                    mountain_mesh%pm%irlds = 0
                    mountain_mesh%pm%iwind = 0
                    mountain_mesh%pm%iphase = 0
                    mountain_mesh%pm%irsrd = 0
                    mountain_mesh%pm%iconsmm = 0
                case default

                    !> Other options.
                    call mountain_extract_value(args(i), z)
                    if (z == 2) then
                        call print_warning("Unrecognized option on '" // uppercase(trim(args(1))) // "': " // trim(args(i)))
                    else if (z == 1) then
                        call print_warning( &
                            "An error occurred parsing the '" // trim(args(i)) // "' option on '" // &
                            uppercase(trim(args(1))) // "'.")
                        ierr = z
                    end if
            end select
        end do

    end subroutine

    subroutine mountain_init(fls, shd)

        !> Required for MESH variables and options.
        use model_files_variables
        use sa_mesh_common

        !> Required for 'il1', 'il2', and 'iln'.
        !> Because 'il1' and 'il2' are not passed to the subroutine, the
        !> subset of the stride is used here instead.
        use mpi_module

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd

        !> Local variables.
        integer k, ierr
        character(len = DEFAULT_LINE_LENGTH) line
        character(len = DEFAULT_FIELD_LENGTH) val

        !> Initialize the error variable.
        ierr = 0

        !> Parse options.
        call mountain_parse_options(mountain_mesh%RUNOPTIONSFLAG, ierr)

        !> Return if module is not enabled.
        if (.not. mountain_mesh%PROCESS_ACTIVE) then
            if (allocated(mountain_mesh%pm%slope)) deallocate(mountain_mesh%pm%slope)
            if (allocated(mountain_mesh%pm%aspect)) deallocate(mountain_mesh%pm%aspect)
            if (allocated(mountain_mesh%pm%delta)) deallocate(mountain_mesh%pm%delta)
            if (allocated(mountain_mesh%pm%curvature)) deallocate(mountain_mesh%pm%curvature)
            if (allocated(mountain_mesh%pm%skyviewfactor)) deallocate(mountain_mesh%pm%skyviewfactor)
            if (allocated(mountain_mesh%pm%gru_frac)) deallocate(mountain_mesh%pm%gru_frac)
            return
        end if

        !> Allocate variables.
        allocate( &
            mountain_mesh%vs%elev(il1:il2), mountain_mesh%vs%xlng(il1:il2), &
            mountain_mesh%vs%ylat(il1:il2), mountain_mesh%vs%slope(il1:il2), &
            mountain_mesh%vs%aspect(il1:il2), mountain_mesh%vs%delta(il1:il2), &
            mountain_mesh%vs%curvature(il1:il2), mountain_mesh%vs%skyviewfactor(il1:il2), &
            mountain_mesh%vs%gru_frac(il1:il2))
            mountain_mesh%vs%slope = 0.0
            mountain_mesh%vs%aspect = 0.0
            mountain_mesh%vs%delta = 0.0
            mountain_mesh%vs%curvature = 0.0
            mountain_mesh%vs%skyviewfactor = 0.0
            mountain_mesh%vs%gru_frac = 0.0

        !> Assign values.
        do k = il1, il2

            !> Pull generic values from drainage_database.r2c
            mountain_mesh%vs%elev(k) = shd%ELEV(shd%lc%ILMOS(k))
            mountain_mesh%vs%xlng(k) = shd%XLNG(shd%lc%ILMOS(k))
            mountain_mesh%vs%ylat(k) = shd%YLAT(shd%lc%ILMOS(k))
            mountain_mesh%vs%gru_frac(k) = shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))

            !> Overwrite with values provided by GRU (e.g., parameters.r2c).
            if (allocated(mountain_mesh%pm%elev)) then
                mountain_mesh%vs%elev(k) = mountain_mesh%pm%elev(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (allocated(mountain_mesh%pm%slope)) then
                mountain_mesh%vs%slope(k) = mountain_mesh%pm%slope(shd%lc%ILMOS(k), shd%lc%JLMOS(k))

                !> Overwrite estimated average slope of the GRU.
                pm%tile%xslp(k) = tan(mountain_mesh%pm%slope(shd%lc%ILMOS(k), shd%lc%JLMOS(k))*3.14159265/180.0)
            end if
            if (allocated(mountain_mesh%pm%aspect)) then
                mountain_mesh%vs%aspect(k) = mountain_mesh%pm%aspect(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (allocated(mountain_mesh%pm%delta)) then
                mountain_mesh%vs%delta(k) = mountain_mesh%pm%delta(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (allocated(mountain_mesh%pm%curvature)) then
                mountain_mesh%vs%curvature(k) = mountain_mesh%pm%curvature(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (allocated(mountain_mesh%pm%skyviewfactor)) then
                mountain_mesh%vs%skyviewfactor(k) = mountain_mesh%pm%skyviewfactor(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
        end do

        !> De-allocate 'ROW' based fields (from parameters file).
        if (allocated(mountain_mesh%pm%slope)) deallocate(mountain_mesh%pm%slope)
        if (allocated(mountain_mesh%pm%aspect)) deallocate(mountain_mesh%pm%aspect)
        if (allocated(mountain_mesh%pm%delta)) deallocate(mountain_mesh%pm%delta)
        if (allocated(mountain_mesh%pm%curvature)) deallocate(mountain_mesh%pm%curvature)
        if (allocated(mountain_mesh%pm%skyviewfactor)) deallocate(mountain_mesh%pm%skyviewfactor)
        if (allocated(mountain_mesh%pm%gru_frac)) deallocate(mountain_mesh%pm%gru_frac)

        !> Print summary and remark that the process is active.
        call print_new_section('MOUNTAINMESH is active.')
        call increase_tab()

        !> Print configuration information to file if 'DIAGNOSEMODE' is active.
        if (DIAGNOSEMODE) then
            line = 'MOUNTAINMESH on'
            write(val, FMT_GEN) mountain_mesh%pm%CurveWeight
            line = trim(line) // ' CurveWeight=' // trim(adjustl(val))
            write(val, FMT_GEN) mountain_mesh%pm%CalcFreq
            line = trim(line) // ' CalcFreq=' // trim(adjustl(val))
            write(val, FMT_GEN) mountain_mesh%pm%ilapse
            line = trim(line) // ' ilapse=' // trim(adjustl(val))
            write(val, FMT_GEN) mountain_mesh%pm%ipre
            line = trim(line) // ' ipre=' // trim(adjustl(val))
            write(val, FMT_GEN) mountain_mesh%pm%itemp
            line = trim(line) // ' itemp=' // trim(adjustl(val))
            write(val, FMT_GEN) mountain_mesh%pm%ipres
            line = trim(line) // ' ipres=' // trim(adjustl(val))
            write(val, FMT_GEN) mountain_mesh%pm%ihumd
            line = trim(line) // ' ihumd=' // trim(adjustl(val))
            write(val, FMT_GEN) mountain_mesh%pm%irlds
            line = trim(line) // ' irlds=' // trim(adjustl(val))
            write(val, FMT_GEN) mountain_mesh%pm%iwind
            line = trim(line) // ' iwind=' // trim(adjustl(val))
            write(val, FMT_GEN) mountain_mesh%pm%iphase
            line = trim(line) // ' iphase=' // trim(adjustl(val))
            write(val, FMT_GEN) mountain_mesh%pm%irsrd
            line = trim(line) // ' irsrd=' // trim(adjustl(val))
            write(val, FMT_GEN) mountain_mesh%pm%iconsmm
            line = trim(line) // ' iconsmm=' // trim(adjustl(val))
            call print_message(line)
        end if

        !> Check values, print error messages for invalid values.
        !> The check is of 'GAT'-based variables, for which all tiles should have valid values.
        if (mod(24*60, mountain_mesh%pm%CalcFreq) /= 0) then
            write(line, FMT_GEN) mountain_mesh%pm%CalcFreq
            call print_error("'CalcFreq' must evenly divide into minutes in the day. 1440 mod " // trim(adjustl(line)) // " /= 0.")
            ierr = 1
        end if
        if (any(mountain_mesh%vs%elev < 0.0)) then
            call print_error("Values of 'elevation' are less than zero.")
            ierr = 1
        end if
        if (any(mountain_mesh%vs%slope < 0.0)) then
            call print_error("Values of 'slope' are less than zero.")
            ierr = 1
        end if
        if (any(mountain_mesh%vs%aspect < 0.0)) then
            call print_error("Values of 'aspect' are less than zero.")
            ierr = 1
        end if
        if (any(mountain_mesh%vs%skyviewfactor < 0.0)) then
            call print_error("Values of 'skyviewfactor' are less than zero.")
            ierr = 1
        end if
        if (mountain_mesh%pm%iwind > 0 .and. .not. allocated(vs%tile%wdir)) then
            call print_error("'iwind' option 1 requires wind direction, but the driving variable is not active.")
            ierr = 1
        end if

        !> Data tables.
        select case (mountain_mesh%pm%ilapse)
            case (1)

                !> Option 1:
                !>  Tables of hour month hour lapse rate derived from the high
                !>  resolution (2.5km by 2.5km) climate model or derived from point observation data.
                mountain_mesh%pm%tlapse = (/ &
                    -12.12, -14.06, -11.17, -8.17, -4.56, -2.82, -1.73, -2.99, -5.90, -8.26, -11.19, -12.11, &
                    -12.14, -14.12, -11.30, -8.32, -4.75, -3.03, -1.92, -3.21, -6.11, -8.35, -11.24, -12.15, &
                    -12.10, -14.16, -11.40, -8.51, -4.97, -3.28, -2.10, -3.41, -6.29, -8.42, -11.27, -12.22, &
                    -12.13, -14.22, -11.50, -8.65, -5.22, -3.46, -2.23, -3.60, -6.43, -8.52, -11.33, -12.18, &
                    -12.20, -14.22, -11.61, -8.80, -5.44, -3.65, -2.46, -3.81, -6.57, -8.64, -11.36, -12.23, &
                    -12.29, -14.24, -11.70, -8.98, -5.63, -3.59, -2.58, -4.03, -6.74, -8.74, -11.36, -12.27, &
                    -12.34, -14.27, -11.78, -9.04, -4.95, -2.53, -1.81, -3.92, -6.90, -8.76, -11.38, -12.21, &
                    -12.36, -14.34, -11.73, -8.12, -3.61, -1.44, -0.77, -2.72, -6.56, -8.88, -11.33, -12.17, &
                    -12.46, -14.23, -10.93, -6.97, -3.07, -1.13, -0.54, -1.97, -5.37, -8.37, -11.27, -12.19, &
                    -12.34, -13.65, -10.13, -6.62, -2.74, -0.91, -0.29, -1.59, -4.69, -7.09, -10.75, -12.09, &
                    -11.71, -12.81, -10.00, -6.26, -2.49, -0.74, -0.12, -1.36, -4.19, -6.32, -10.02, -11.59, &
                    -11.21, -12.26, -9.76, -6.05, -2.45, -0.72, 0.00, -1.29, -3.89, -6.12, -9.75, -11.01, &
                    -10.95, -11.95, -9.59, -5.95, -2.56, -0.77, -0.02, -1.31, -3.78, -5.91, -9.72, -10.87, &
                    -10.95, -11.83, -9.53, -5.97, -2.76, -0.92, -0.21, -1.44, -3.76, -5.90, -9.66, -10.97, &
                    -10.91, -11.78, -9.64, -6.00, -2.96, -1.14, -0.45, -1.55, -3.84, -5.88, -9.73, -11.06, &
                    -10.96, -11.87, -9.63, -5.97, -3.12, -1.27, -0.52, -1.58, -3.93, -6.00, -9.95, -11.32, &
                    -11.26, -12.09, -9.71, -5.95, -3.30, -1.40, -0.71, -1.73, -3.91, -6.26, -10.37, -11.83, &
                    -9.74, -10.83, -7.40, -3.39, -0.62, 1.41, 1.72, 0.69, -1.28, -3.45, -8.15, -9.95, &
                    -11.33, -13.37, -10.70, -6.54, -3.51, -1.19, -0.85, -1.96, -4.24, -6.68, -10.34, -11.48, &
                    -11.58, -13.76, -11.21, -7.50, -3.88, -1.60, -1.20, -2.56, -5.29, -7.18, -10.59, -11.66, &
                    -11.83, -13.89, -11.06, -7.78, -4.22, -2.04, -1.63, -3.13, -5.36, -7.50, -10.86, -11.88, &
                    -12.00, -14.08, -11.03, -7.80, -4.38, -2.56, -1.94, -2.96, -5.43, -7.79, -11.00, -11.97, &
                    -12.05, -14.03, -11.04, -7.87, -4.32, -2.61, -1.76, -2.86, -5.57, -7.97, -11.07, -12.08, &
                    -12.09, -14.06, -11.10, -7.97, -4.39, -2.70, -1.69, -2.85, -5.67, -8.12, -11.07, -12.08, &
                    -6.64, -7.97, -7.10, -6.75, -5.54, -4.42, -3.28, -3.85, -5.17, -6.29, -6.97, -6.99, &
                    -6.62, -7.98, -7.11, -6.69, -5.48, -4.42, -3.28, -3.87, -5.19, -6.25, -6.97, -7.00, &
                    -6.59, -7.98, -7.10, -6.67, -5.45, -4.46, -3.29, -3.89, -5.21, -6.20, -6.98, -7.03, &
                    -6.60, -7.99, -7.10, -6.64, -5.46, -4.48, -3.29, -3.92, -5.22, -6.19, -7.00, -6.99, &
                    -6.63, -7.98, -7.11, -6.64, -5.47, -4.51, -3.35, -3.96, -5.24, -6.19, -7.00, -7.00, &
                    -6.67, -7.98, -7.12, -6.66, -5.50, -4.49, -3.41, -4.02, -5.28, -6.20, -6.99, -7.01, &
                    -6.69, -7.99, -7.12, -6.68, -5.34, -4.13, -3.22, -4.01, -5.33, -6.16, -6.98, -6.99, &
                    -6.69, -8.01, -7.09, -6.47, -4.90, -3.66, -2.79, -3.66, -5.26, -6.19, -6.95, -6.97, &
                    -6.74, -7.98, -6.85, -6.19, -4.66, -3.52, -2.49, -3.29, -4.93, -6.08, -6.92, -6.97, &
                    -6.70, -7.79, -6.66, -6.15, -4.61, -3.50, -2.38, -3.06, -4.68, -5.77, -6.76, -6.94, &
                    -6.49, -7.56, -6.69, -6.17, -4.64, -3.52, -2.40, -3.07, -4.52, -5.57, -6.57, -6.81, &
                    -6.40, -7.38, -6.70, -6.30, -4.78, -3.62, -2.47, -3.18, -4.54, -5.64, -6.58, -6.66, &
                    -6.36, -7.34, -6.80, -6.44, -5.00, -3.75, -2.62, -3.34, -4.66, -5.72, -6.67, -6.65, &
                    -6.45, -7.41, -6.95, -6.60, -5.24, -3.90, -2.85, -3.57, -4.78, -5.87, -6.76, -6.75, &
                    -6.52, -7.50, -7.16, -6.72, -5.46, -4.06, -3.07, -3.75, -4.94, -5.97, -6.89, -6.84, &
                    -6.59, -7.62, -7.27, -6.80, -5.64, -4.16, -3.16, -3.87, -5.06, -6.12, -7.04, -6.98, &
                    -6.74, -7.76, -7.37, -6.85, -5.80, -4.25, -3.30, -4.01, -5.11, -6.30, -7.19, -7.18, &
                    -5.37, -6.42, -5.41, -4.81, -4.03, -2.83, -1.82, -2.27, -3.29, -4.27, -5.45, -5.85, &
                    -6.36, -7.90, -7.41, -6.94, -6.10, -4.23, -3.37, -4.02, -5.07, -6.11, -6.80, -6.80, &
                    -6.45, -8.05, -7.61, -7.38, -6.31, -4.48, -3.60, -4.26, -5.44, -6.31, -6.88, -6.86, &
                    -6.56, -8.07, -7.43, -7.28, -6.21, -4.56, -3.66, -4.33, -5.37, -6.36, -6.96, -6.95, &
                    -6.63, -8.09, -7.30, -7.08, -6.02, -4.60, -3.67, -4.16, -5.29, -6.37, -6.98, -6.97, &
                    -6.64, -8.02, -7.20, -6.92, -5.79, -4.53, -3.52, -4.01, -5.23, -6.35, -6.97, -7.00, &
                    -6.64, -8.00, -7.14, -6.80, -5.64, -4.46, -3.38, -3.89, -5.16, -6.32, -6.93, -6.98, &
                    -6.55, -7.77, -7.26, -7.13, -6.43, -5.65, -4.91, -5.29, -6.05, -6.66, -6.91, -6.96, &
                    -6.51, -7.76, -7.24, -7.07, -6.33, -5.60, -4.86, -5.29, -6.02, -6.60, -6.90, -6.95, &
                    -6.47, -7.74, -7.22, -7.03, -6.26, -5.57, -4.82, -5.28, -6.00, -6.54, -6.90, -6.96, &
                    -6.45, -7.73, -7.20, -7.00, -6.22, -5.54, -4.78, -5.26, -5.97, -6.51, -6.90, -6.92, &
                    -6.45, -7.71, -7.18, -6.98, -6.20, -5.51, -4.79, -5.26, -5.95, -6.49, -6.88, -6.91, &
                    -6.46, -7.69, -7.16, -6.98, -6.20, -5.46, -4.80, -5.28, -5.95, -6.47, -6.86, -6.90, &
                    -6.46, -7.69, -7.14, -6.98, -6.10, -5.19, -4.62, -5.23, -5.96, -6.41, -6.84, -6.87, &
                    -6.46, -7.69, -7.10, -6.90, -5.94, -4.85, -4.17, -4.92, -5.89, -6.41, -6.80, -6.85, &
                    -6.49, -7.66, -6.98, -6.90, -5.90, -4.83, -3.85, -4.48, -5.69, -6.35, -6.77, -6.85, &
                    -6.47, -7.57, -7.04, -6.96, -6.01, -4.95, -3.87, -4.37, -5.52, -6.26, -6.69, -6.83, &
                    -6.39, -7.51, -7.18, -7.03, -6.18, -5.09, -4.03, -4.50, -5.50, -6.18, -6.70, -6.79, &
                    -6.45, -7.45, -7.21, -7.17, -6.41, -5.27, -4.24, -4.71, -5.62, -6.30, -6.84, -6.80, &
                    -6.51, -7.44, -7.31, -7.32, -6.67, -5.45, -4.49, -4.97, -5.79, -6.43, -6.96, -6.88, &
                    -6.62, -7.52, -7.44, -7.46, -6.89, -5.60, -4.74, -5.24, -5.96, -6.60, -7.05, -6.96, &
                    -6.70, -7.61, -7.60, -7.57, -7.08, -5.73, -4.95, -5.45, -6.13, -6.73, -7.18, -7.05, &
                    -6.78, -7.73, -7.69, -7.61, -7.21, -5.80, -5.05, -5.58, -6.26, -6.87, -7.30, -7.19, &
                    -6.90, -7.85, -7.75, -7.62, -7.31, -5.85, -5.15, -5.69, -6.32, -7.01, -7.42, -7.35, &
                    -5.60, -6.45, -5.95, -5.83, -5.86, -5.29, -4.63, -4.55, -4.85, -5.26, -5.76, -5.98, &
                    -6.62, -7.94, -7.69, -7.55, -7.54, -5.98, -5.14, -5.56, -6.25, -6.83, -7.03, -7.03, &
                    -6.60, -7.98, -7.78, -7.85, -7.58, -6.08, -5.26, -5.68, -6.52, -6.90, -6.99, -7.00, &
                    -6.62, -7.94, -7.57, -7.71, -7.39, -6.03, -5.21, -5.68, -6.43, -6.85, -6.99, -7.02, &
                    -6.63, -7.92, -7.45, -7.49, -7.09, -5.97, -5.22, -5.58, -6.30, -6.80, -6.97, -7.01, &
                    -6.60, -7.84, -7.35, -7.32, -6.79, -5.85, -5.15, -5.46, -6.19, -6.75, -6.94, -7.01, &
                    -6.57, -7.81, -7.30, -7.20, -6.58, -5.74, -5.02, -5.35, -6.09, -6.70, -6.90, -6.98, &
                    -6.55, -7.77, -7.26, -7.13, -6.43, -5.65, -4.91, -5.29, -6.05, -6.66, -6.91, -6.96, &
                    -6.51, -7.76, -7.24, -7.07, -6.33, -5.60, -4.86, -5.29, -6.02, -6.60, -6.90, -6.95, &
                    -6.47, -7.74, -7.22, -7.03, -6.26, -5.57, -4.82, -5.28, -6.00, -6.54, -6.90, -6.96, &
                    -6.45, -7.73, -7.20, -7.00, -6.22, -5.54, -4.78, -5.26, -5.97, -6.51, -6.90, -6.92, &
                    -6.45, -7.71, -7.18, -6.98, -6.20, -5.51, -4.79, -5.26, -5.95, -6.49, -6.88, -6.91, &
                    -6.46, -7.69, -7.16, -6.98, -6.20, -5.46, -4.80, -5.28, -5.95, -6.47, -6.86, -6.90, &
                    -6.46, -7.69, -7.14, -6.98, -6.10, -5.19, -4.62, -5.23, -5.96, -6.41, -6.84, -6.87, &
                    -6.46, -7.69, -7.10, -6.90, -5.94, -4.85, -4.17, -4.92, -5.89, -6.41, -6.80, -6.85, &
                    -6.49, -7.66, -6.98, -6.90, -5.90, -4.83, -3.85, -4.48, -5.69, -6.35, -6.77, -6.85, &
                    -6.47, -7.57, -7.04, -6.96, -6.01, -4.95, -3.87, -4.37, -5.52, -6.26, -6.69, -6.83, &
                    -6.39, -7.51, -7.18, -7.03, -6.18, -5.09, -4.03, -4.50, -5.50, -6.18, -6.70, -6.79, &
                    -6.45, -7.45, -7.21, -7.17, -6.41, -5.27, -4.24, -4.71, -5.62, -6.30, -6.84, -6.80, &
                    -6.51, -7.44, -7.31, -7.32, -6.67, -5.45, -4.49, -4.97, -5.79, -6.43, -6.96, -6.88, &
                    -6.62, -7.52, -7.44, -7.46, -6.89, -5.60, -4.74, -5.24, -5.96, -6.60, -7.05, -6.96, &
                    -6.70, -7.61, -7.60, -7.57, -7.08, -5.73, -4.95, -5.45, -6.13, -6.73, -7.18, -7.05, &
                    -6.78, -7.73, -7.69, -7.61, -7.21, -5.80, -5.05, -5.58, -6.26, -6.87, -7.30, -7.19, &
                    -6.90, -7.85, -7.75, -7.62, -7.31, -5.85, -5.15, -5.69, -6.32, -7.01, -7.42, -7.35, &
                    -5.60, -6.45, -5.95, -5.83, -5.86, -5.29, -4.63, -4.55, -4.85, -5.26, -5.76, -5.98, &
                    -6.62, -7.94, -7.69, -7.55, -7.54, -5.98, -5.14, -5.56, -6.25, -6.83, -7.03, -7.03, &
                    -6.60, -7.98, -7.78, -7.85, -7.58, -6.08, -5.26, -5.68, -6.52, -6.90, -6.99, -7.00, &
                    -6.62, -7.94, -7.57, -7.71, -7.39, -6.03, -5.21, -5.68, -6.43, -6.85, -6.99, -7.02, &
                    -6.63, -7.92, -7.45, -7.49, -7.09, -5.97, -5.22, -5.58, -6.30, -6.80, -6.97, -7.01, &
                    -6.60, -7.84, -7.35, -7.32, -6.79, -5.85, -5.15, -5.46, -6.19, -6.75, -6.94, -7.01, &
                    -6.57, -7.81, -7.30, -7.20, -6.58, -5.74, -5.02, -5.35, -6.09, -6.70, -6.90, -6.98, &
                    -6.55, -7.77, -7.26, -7.13, -6.43, -5.65, -4.91, -5.29, -6.05, -6.66, -6.91, -6.96, &
                    -6.51, -7.76, -7.24, -7.07, -6.33, -5.60, -4.86, -5.29, -6.02, -6.60, -6.90, -6.95, &
                    -6.47, -7.74, -7.22, -7.03, -6.26, -5.57, -4.82, -5.28, -6.00, -6.54, -6.90, -6.96, &
                    -6.45, -7.73, -7.20, -7.00, -6.22, -5.54, -4.78, -5.26, -5.97, -6.51, -6.90, -6.92, &
                    -6.45, -7.71, -7.18, -6.98, -6.20, -5.51, -4.79, -5.26, -5.95, -6.49, -6.88, -6.91, &
                    -6.46, -7.69, -7.16, -6.98, -6.20, -5.46, -4.80, -5.28, -5.95, -6.47, -6.86, -6.90, &
                    -6.46, -7.69, -7.14, -6.98, -6.10, -5.19, -4.62, -5.23, -5.96, -6.41, -6.84, -6.87, &
                    -6.46, -7.69, -7.10, -6.90, -5.94, -4.85, -4.17, -4.92, -5.89, -6.41, -6.80, -6.85, &
                    -6.49, -7.66, -6.98, -6.90, -5.90, -4.83, -3.85, -4.48, -5.69, -6.35, -6.77, -6.85, &
                    -6.47, -7.57, -7.04, -6.96, -6.01, -4.95, -3.87, -4.37, -5.52, -6.26, -6.69, -6.83, &
                    -6.39, -7.51, -7.18, -7.03, -6.18, -5.09, -4.03, -4.50, -5.50, -6.18, -6.70, -6.79, &
                    -6.45, -7.45, -7.21, -7.17, -6.41, -5.27, -4.24, -4.71, -5.62, -6.30, -6.84, -6.80, &
                    -6.51, -7.44, -7.31, -7.32, -6.67, -5.45, -4.49, -4.97, -5.79, -6.43, -6.96, -6.88, &
                    -6.62, -7.52, -7.44, -7.46, -6.89, -5.60, -4.74, -5.24, -5.96, -6.60, -7.05, -6.96, &
                    -6.70, -7.61, -7.60, -7.57, -7.08, -5.73, -4.95, -5.45, -6.13, -6.73, -7.18, -7.05, &
                    -6.78, -7.73, -7.69, -7.61, -7.21, -5.80, -5.05, -5.58, -6.26, -6.87, -7.30, -7.19, &
                    -6.90, -7.85, -7.75, -7.62, -7.31, -5.85, -5.15, -5.69, -6.32, -7.01, -7.42, -7.35, &
                    -5.60, -6.45, -5.95, -5.83, -5.86, -5.29, -4.63, -4.55, -4.85, -5.26, -5.76, -5.98, &
                    -6.62, -7.94, -7.69, -7.55, -7.54, -5.98, -5.14, -5.56, -6.25, -6.83, -7.03, -7.03, &
                    -6.60, -7.98, -7.78, -7.85, -7.58, -6.08, -5.26, -5.68, -6.52, -6.90, -6.99, -7.00, &
                    -6.62, -7.94, -7.57, -7.71, -7.39, -6.03, -5.21, -5.68, -6.43, -6.85, -6.99, -7.02, &
                    -6.63, -7.92, -7.45, -7.49, -7.09, -5.97, -5.22, -5.58, -6.30, -6.80, -6.97, -7.01, &
                    -6.60, -7.84, -7.35, -7.32, -6.79, -5.85, -5.15, -5.46, -6.19, -6.75, -6.94, -7.01, &
                    -6.57, -7.81, -7.30, -7.20, -6.58, -5.74, -5.02, -5.35, -6.09, -6.70, -6.90, -6.98 /)
                mountain_mesh%pm%dtlapse = (/ &
                    0.52, 0.63, 0.52, 0.50, 0.43, 0.34, 0.34, 0.38, 0.41, 0.50, 0.52, 0.54, &
                    0.52, 0.63, 0.52, 0.50, 0.43, 0.33, 0.33, 0.37, 0.41, 0.50, 0.53, 0.55, &
                    0.52, 0.63, 0.53, 0.50, 0.42, 0.33, 0.33, 0.36, 0.41, 0.50, 0.53, 0.55, &
                    0.52, 0.64, 0.54, 0.50, 0.42, 0.33, 0.32, 0.36, 0.40, 0.50, 0.53, 0.55, &
                    0.53, 0.64, 0.54, 0.50, 0.42, 0.33, 0.32, 0.35, 0.40, 0.50, 0.53, 0.55, &
                    0.53, 0.64, 0.54, 0.50, 0.41, 0.32, 0.31, 0.35, 0.40, 0.50, 0.53, 0.55, &
                    0.53, 0.64, 0.55, 0.50, 0.40, 0.32, 0.30, 0.34, 0.40, 0.50, 0.53, 0.55, &
                    0.54, 0.64, 0.55, 0.49, 0.39, 0.32, 0.31, 0.34, 0.40, 0.50, 0.52, 0.56, &
                    0.54, 0.64, 0.53, 0.48, 0.39, 0.33, 0.31, 0.36, 0.40, 0.49, 0.52, 0.56, &
                    0.54, 0.63, 0.51, 0.47, 0.39, 0.33, 0.31, 0.35, 0.41, 0.48, 0.51, 0.56, &
                    0.53, 0.61, 0.49, 0.46, 0.38, 0.33, 0.30, 0.35, 0.41, 0.49, 0.50, 0.55, &
                    0.52, 0.59, 0.47, 0.46, 0.39, 0.33, 0.31, 0.35, 0.40, 0.49, 0.51, 0.54, &
                    0.52, 0.58, 0.45, 0.45, 0.39, 0.34, 0.32, 0.36, 0.40, 0.49, 0.51, 0.23, &
                    0.52, 0.57, 0.44, 0.44, 0.40, 0.34, 0.32, 0.38, 0.41, 0.48, 0.51, 0.55, &
                    0.51, 0.56, 0.44, 0.44, 0.40, 0.35, 0.34, 0.39, 0.42, 0.49, 0.51, 0.55, &
                    0.51, 0.56, 0.43, 0.44, 0.40, 0.35, 0.35, 0.40, 0.43, 0.49, 0.51, 0.56, &
                    0.52, 0.57, 0.43, 0.44, 0.41, 0.35, 0.36, 0.42, 0.43, 0.50, 0.52, 0.57, &
                    0.47, 0.53, 0.42, 0.39, 0.32, 0.25, 0.28, 0.32, 0.35, 0.42, 0.42, 0.49, &
                    0.51, 0.59, 0.48, 0.48, 0.44, 0.37, 0.37, 0.42, 0.46, 0.50, 0.50, 0.54, &
                    0.52, 0.60, 0.50, 0.50, 0.44, 0.38, 0.37, 0.42, 0.44, 0.50, 0.51, 0.54, &
                    0.51, 0.61, 0.51, 0.49, 0.45, 0.38, 0.37, 0.42, 0.42, 0.51, 0.51, 0.54, &
                    0.51, 0.61, 0.51, 0.49, 0.44, 0.36, 0.35, 0.40, 0.42, 0.51, 0.52, 0.54, &
                    0.51, 0.62, 0.52, 0.50, 0.44, 0.35, 0.34, 0.40, 0.42, 0.51, 0.52, 0.54, &
                    0.52, 0.63, 0.52, 0.50, 0.43, 0.35, 0.34, 0.39, 0.42, 0.50, 0.52, 0.54 /)
                mountain_mesh%pm%plapse = (/ 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30 /)
                mountain_mesh%pm%lwlapse = &
                    (/ -18.35, -18.35, -18.35, -18.35, -18.35, -18.35, -18.35, -18.35, -18.35, -18.35, -18.35, -18.35 /)
                mountain_mesh%pm%wlapse = (/ 0.21, 0.21, 0.21, 0.21, 0.21, 0.21, 0.21, 0.21, 0.21, 0.21, 0.21, 0.21 /)
            case default
                if (mountain_mesh%pm%ipre /= 0) then
                    call print_error("'ipre' is active but cannot be used without any ilapse option to define 'plapse'.")
                    ierr = 1
                end if
                if (mountain_mesh%pm%itemp /= 0) then
                    call print_error("'itemp' is active but cannot be used without any ilapse option to define 'tlapse'.")
                    ierr = 1
                end if
                if (mountain_mesh%pm%ihumd /= 0) then
                    call print_error("'ihumd' is active but cannot be used without any ilapse option to define 'dtlapse'.")
                    ierr = 1
                end if
                if (mountain_mesh%pm%irlds /= 0) then
                    call print_error("'irlds' is active but cannot be used without any ilapse option to define 'lwlapse'.")
                    ierr = 1
                end if
                if (mountain_mesh%pm%iwind > 0) then
                    call print_error("'iwind' is active but cannot be used without any ilapse option to define 'wlapse'.")
                    ierr = 1
                end if
        end select

        !> Check for errors.
        if (ierr /= 0) then
            call print_error("Errors occurred during the initialization of 'MOUNTAINMESH'.")
            call program_abort()
        else
            call reset_tab()
        end if

        !> Check for required variables.
        if (mountain_mesh%pm%iwind > 0 .and. .not. allocated(vs%tile%uv)) then
            call print_error( &
                "'IWIND' is active but the driving variable '" // VN_UV // "' is not active or not associated with an input file.")
            ierr = 1
            if (mountain_mesh%pm%iwind > 0 .and. .not. allocated(vs%tile%wdir)) then
                call print_error( &
                    "'IWIND 1' is active but the driving variable '" // VN_WDIR // &
                    "' is not active or not associated with an input file.")
                ierr = 1
            end if
        end if
        if (mountain_mesh%pm%itemp > 0 .and. .not. allocated(vs%tile%ta)) then
            call print_error( &
                "'ITEMP' is active but the driving variable '" // VN_TA // "' is not active or not associated with an input file.")
            ierr = 1
        end if
        if (mountain_mesh%pm%ipres > 0 .and. .not. allocated(vs%tile%pres)) then
            call print_error( &
                "'IPRES' is active but the driving variable '" // VN_PRES // &
                "' is not active or not associated with an input file.")
            ierr = 1
            if (.not. allocated(vs%tile%ta)) then
                call print_error( &
                    "'IPRES' is active but the driving variable '" // VN_TA // &
                    "' is not active or not associated with an input file.")
                ierr = 1
            end if
        end if
        if (mountain_mesh%pm%ihumd > 0 .and. .not. allocated(vs%tile%qa)) then
            call print_error( &
                "'IHUMD' is active but the driving variable '" // VN_QA // "' is not active or not associated with an input file.")
            ierr = 1
            if (.not. allocated(vs%tile%ta)) then
                call print_error( &
                    "'IHUMD' is active but the driving variable '" // VN_TA // &
                    "' is not active or not associated with an input file.")
                ierr = 1
            end if
            if (.not. allocated(vs%tile%pres)) then
                call print_error( &
                    "'IHUMD' is active but the driving variable '" // VN_PRES // &
                    "' is not active or not associated with an input file.")
                ierr = 1
            end if
        end if
        if (mountain_mesh%pm%irlds > 0 .and. .not. allocated(vs%tile%flin)) then
            call print_error( &
                "'IRLDS' is active but the driving variable '" // VN_FLIN // &
                "' is not active or not associated with an input file.")
            ierr = 1
            if (.not. allocated(vs%tile%ta)) then
                call print_error( &
                    "'IRLDS' is active but the driving variable '" // VN_TA // &
                    "' is not active or not associated with an input file.")
                ierr = 1
            end if
            if (.not. allocated(vs%tile%qa)) then
                call print_error( &
                    "'IRLDS' is active but the driving variable '" // VN_PRES // &
                    "' is not active or not associated with an input file.")
                ierr = 1
            end if
        end if
        if (mountain_mesh%pm%ipre > 0 .and. .not. allocated(vs%tile%pre)) then
            call print_error( &
                "'IPRE' is active but the driving variable '" // VN_PRE // "' is not active or not associated with an input file.")
            ierr = 1
        end if
        if (mountain_mesh%pm%iphase > 0) then
            if (.not. allocated(vs%tile%pre)) then
                call print_error( &
                    "'IPHASE' is active but the driving variable '" // VN_PRE // &
                    "' is not active or not associated with an input file.")
                ierr = 1
            end if
            if (.not. allocated(vs%tile%ta)) then
                call print_error( &
                    "'IPRE' is active with 'IPHASE' but the driving variable '" // VN_TA // &
                    "' is not active or not associated with an input file.")
                ierr = 1
            end if
            if (ierr == 0) then
                if (.not. allocated(vs%tile%prern)) allocate(vs%tile%prern(vs%tile%dim_length))
                if (.not. allocated(vs%tile%presno)) allocate(vs%tile%presno(vs%tile%dim_length))
                if (associated(vs%grid)) then
                    if (.not. allocated(vs%grid%prern)) allocate(vs%grid%prern(vs%grid%dim_length))
                    if (.not. allocated(vs%grid%presno)) allocate(vs%grid%presno(vs%grid%dim_length))
                end if
            end if
        end if
        if (mountain_mesh%pm%irsrd > 0 .and. .not. allocated(vs%tile%fsin)) then
            call print_error( &
                "'IRSRD' is active but the driving variable '" // VN_FSIN // &
                "' is not active or not associated with an input file.")
            ierr = 1
            if (mountain_mesh%pm%irsrd > 0 .and. .not. allocated(vs%tile%pres)) then
                call print_error( &
                    "'IRSRD 1' is active but the driving variable '" // VN_PRES // &
                    "' is not active or not associated with an input file.")
                ierr = 1
            end if
        end if
        if (ierr /= 0) then
            call reset_tab()
            call print_error( &
                "The variables required to drive the model are not active or have not been associated with an input file.")
            call program_abort()
        end if

    end subroutine

    subroutine mountain_within_tile(fls, shd)

        !> Required for MESH variables.
        use model_files_variables
        use sa_mesh_common
        use model_dates

        !> Required for 'il1', 'il2', and 'iln'.
        !> Because 'il1' and 'il2' are not passed to subroutine, the
        !> subset of the stride is used here instead.
        use mpi_module

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd

        !> Local variables.
        integer k
        real, dimension(il1:il2) :: rsrd_adjusted, rsds_in
        real, dimension(il1:il2) :: rlds_adjusted, rlds_in
        real, dimension(il1:il2) :: temp_adjusted, temp_in
        real, dimension(il1:il2) :: pres_adjusted, pres_in
        real, dimension(il1:il2) :: humd_adjusted, humd_in
        real, dimension(il1:il2) :: rain_adjusted, rain_in
        real, dimension(il1:il2) :: rain_phased_adjusted
        real, dimension(il1:il2) :: snow_phased_adjusted
        real, dimension(il1:il2) :: wind_adjusted, wind_in, winddir_in

        !> Return if module is not enabled.
        if (.not. mountain_mesh%PROCESS_ACTIVE) return

        !> Transfer variables.
        if (allocated(vs%tile%fsin)) rsds_in(il1:il2) = vs%tile%fsin(il1:il2)
        if (allocated(vs%tile%flin)) rlds_in(il1:il2) = vs%tile%flin(il1:il2)
        if (allocated(vs%tile%ta)) temp_in(il1:il2) = vs%tile%ta(il1:il2)
        if (allocated(vs%tile%pres)) pres_in(il1:il2) = vs%tile%pres(il1:il2)
        if (allocated(vs%tile%qa)) humd_in(il1:il2) = vs%tile%qa(il1:il2)
        if (allocated(vs%tile%pre)) rain_in(il1:il2) = vs%tile%pre(il1:il2)
        if (allocated(vs%tile%uv)) wind_in(il1:il2) = vs%tile%uv(il1:il2)
        if (allocated(vs%tile%wdir)) winddir_in(il1:il2) = vs%tile%wdir(il1:il2)

        !> Call routine to calculate adjusted radiation value.
        call forcing_adjust( &
            mountain_mesh%vs%elev(il1:il2), mountain_mesh%vs%xlng(il1:il2), mountain_mesh%vs%ylat(il1:il2), &
            mountain_mesh%vs%slope(il1:il2), mountain_mesh%vs%aspect(il1:il2), &
            mountain_mesh%vs%delta(il1:il2), mountain_mesh%vs%curvature(il1:il2), mountain_mesh%vs%skyviewfactor(il1:il2), &
            iln, shd%lc%ILMOS(il1:il2), i1, i2, &
            mountain_mesh%pm%CurveWeight, mountain_mesh%pm%CalcFreq, &
            mountain_mesh%pm%ipre, mountain_mesh%pm%itemp, mountain_mesh%pm%ipres, &
            mountain_mesh%pm%ihumd, mountain_mesh%pm%irlds, mountain_mesh%pm%iwind, &
            mountain_mesh%pm%iphase, mountain_mesh%pm%irsrd, mountain_mesh%pm%iconsmm, &
            mountain_mesh%pm%tlapse, mountain_mesh%pm%plapse, mountain_mesh%pm%dtlapse, &
            mountain_mesh%pm%lwlapse, mountain_mesh%pm%wlapse, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            rsds_in(il1:il2), &
            rlds_in(il1:il2), &
            temp_in(il1:il2), &
            pres_in(il1:il2), &
            humd_in(il1:il2), &
            rain_in(il1:il2), &
            wind_in(il1:il2), &
            winddir_in(il1:il2), &
            rsrd_adjusted(il1:il2), &
            rlds_adjusted(il1:il2), &
            temp_adjusted(il1:il2), &
            pres_adjusted(il1:il2), &
            humd_adjusted(il1:il2), &
            rain_adjusted(il1:il2), &
            rain_phased_adjusted(il1:il2), &
            snow_phased_adjusted(il1:il2), &
            wind_adjusted(il1:il2), &
            ic%now%year, ic%now%month, ic%now%jday, ic%now%hour, ic%now%mins, ic%dtmins, &
            mountain_mesh%vs%gru_frac(il1:il2))

        !> Update radiation.
        !> Must update 'GRD' separately for output (e.g., energy_balance.csv).
        if (allocated(vs%tile%fsin)) vs%tile%fsin(il1:il2) = rsrd_adjusted(il1:il2)
        if (allocated(vs%tile%flin)) vs%tile%flin(il1:il2) = rlds_adjusted(il1:il2)
        if (allocated(vs%tile%ta)) vs%tile%ta(il1:il2) = temp_adjusted(il1:il2)
        if (allocated(vs%tile%pres)) vs%tile%pres(il1:il2) = pres_adjusted(il1:il2)
        if (allocated(vs%tile%qa)) vs%tile%qa(il1:il2) = humd_adjusted(il1:il2)
        if (allocated(vs%tile%pre)) vs%tile%pre(il1:il2) = rain_adjusted(il1:il2)
        if (allocated(vs%tile%prern)) vs%tile%prern(il1:il2) = rain_phased_adjusted(il1:il2)
        if (allocated(vs%tile%presno)) vs%tile%presno(il1:il2) = snow_phased_adjusted(il1:il2)
        if (allocated(vs%tile%uv)) vs%tile%uv(il1:il2) = wind_adjusted(il1:il2)
        if (allocated(vs%grid%fsin)) vs%grid%fsin = 0.0
        if (allocated(vs%grid%flin)) vs%grid%flin = 0.0
        if (allocated(vs%grid%ta)) vs%grid%ta = 0.0
        if (allocated(vs%grid%pres)) vs%grid%pres = 0.0
        if (allocated(vs%grid%qa)) vs%grid%qa = 0.0
        if (allocated(vs%grid%pre)) vs%grid%pre = 0.0
        if (allocated(vs%grid%prern)) vs%grid%prern = 0.0
        if (allocated(vs%grid%presno)) vs%grid%presno = 0.0
        if (allocated(vs%grid%uv)) vs%grid%uv = 0.0
        do k = il1, il2
            if (allocated(vs%grid%fsin)) then
                vs%grid%fsin(shd%lc%ILMOS(k)) = vs%grid%fsin(shd%lc%ILMOS(k)) + &
                    rsrd_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (allocated(vs%grid%flin)) then
                vs%grid%flin(shd%lc%ILMOS(k)) = vs%grid%flin(shd%lc%ILMOS(k)) + &
                    rlds_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (allocated(vs%grid%ta)) then
                vs%grid%ta(shd%lc%ILMOS(k)) = vs%grid%ta(shd%lc%ILMOS(k)) + &
                    temp_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (allocated(vs%grid%pres)) then
                vs%grid%pres(shd%lc%ILMOS(k)) = vs%grid%pres(shd%lc%ILMOS(k)) + &
                    pres_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (allocated(vs%grid%qa)) then
                vs%grid%qa(shd%lc%ILMOS(k)) = vs%grid%qa(shd%lc%ILMOS(k)) + &
                    humd_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (allocated(vs%grid%pre)) then
                vs%grid%pre(shd%lc%ILMOS(k)) = vs%grid%pre(shd%lc%ILMOS(k)) + &
                    rain_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (allocated(vs%grid%prern)) then
                vs%grid%prern(shd%lc%ILMOS(k)) = vs%grid%prern(shd%lc%ILMOS(k)) + &
                    rain_phased_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (allocated(vs%grid%presno)) then
                vs%grid%presno(shd%lc%ILMOS(k)) = vs%grid%presno(shd%lc%ILMOS(k)) + &
                    snow_phased_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (allocated(vs%grid%uv)) then
                vs%grid%uv(shd%lc%ILMOS(k)) = vs%grid%uv(shd%lc%ILMOS(k)) + &
                    wind_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
        end do

    end subroutine

end module
