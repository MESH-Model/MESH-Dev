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
    !*  Time_Zone: The time zone of the study area.
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
    !*  idecl: Flag to specify calculation to use for declination. [--].
    !>      0: Based on Dingman, 2015 and Iqbal, 1983.
    !>      1: Alternate approach. (default).
    !*  elev: Weighted average elevation of GRUs. [m].
    !*  delta: Weighted average elevation difference between high (MESH) and low (GEM) resolution elevation. [m].
    !*  slope: Weighted average slope of surface (1: grid; 2: GRU). [degrees].
    !*  aspect: Weighted average aspect of surface (1: grid; 2: GRU). [degrees].
    !*  curvature: Weighted average curvature of the surface. [--].
    !*  tlapse: Table of lapse rate values for temperature. [--].
    !*  plapse: Table of lapse rate values for precipitation. [--].
    !*  dtlapse: Table of lapse rate values for dew point temperature. [--].
    !*  lwlapse: Table of lapse rate values for longwave radiation. [--].
    !*  wlapse: Table of lapse rate values for wind speed. [--].
    type mountain_parameters
        real :: Time_Zone = -6.0
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
        integer :: idecl = 1
        real, dimension(:, :), allocatable :: elev, slope, aspect, delta, curvature
        real, dimension(12) :: tlapse = (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
        real, dimension(12) :: plapse = (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
        real, dimension(12) :: dtlapse = (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
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
    type mountain_variables
        real, dimension(:), allocatable :: elev, xlng, ylat, slope, aspect, delta, curvature
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
            case ('time_zone')
                call value(args(2), mountain_mesh%pm%Time_Zone, z)
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
            case ('idecl')
                call value(args(2), mountain_mesh%pm%idecl, z)
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
                    mountain_mesh%pm%idecl = 0
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

    subroutine mountain_init(fls, shd, cm)

        !> Required for MESH variables and options.
        use model_files_variables
        use sa_mesh_common
        use climate_forcing

        !> Required for 'il1', 'il2', and 'iln'.
        !> Because 'il1' and 'il2' are not passed to the subroutine, the
        !> subset of the stride is used here instead.
        use mpi_module

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(clim_info), intent(in) :: cm

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
            return
        end if

        !> Allocate variables.
        allocate( &
            mountain_mesh%vs%elev(il1:il2), mountain_mesh%vs%xlng(il1:il2), &
            mountain_mesh%vs%ylat(il1:il2), mountain_mesh%vs%slope(il1:il2), &
            mountain_mesh%vs%aspect(il1:il2), mountain_mesh%vs%delta(il1:il2), &
            mountain_mesh%vs%curvature(il1:il2))
            mountain_mesh%vs%slope = 0.0
            mountain_mesh%vs%aspect = 0.0
            mountain_mesh%vs%delta = 0.0
            mountain_mesh%vs%curvature = 0.0

        !> Assign values.
        do k = il1, il2

            !> Pull generic values from drainage_database.r2c
            mountain_mesh%vs%elev(k) = shd%ELEV(shd%lc%ILMOS(k))
            mountain_mesh%vs%xlng(k) = shd%XLNG(shd%lc%ILMOS(k))
            mountain_mesh%vs%ylat(k) = shd%YLAT(shd%lc%ILMOS(k))

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
        end do

        !> De-allocate 'ROW' based fields (from parameters file).
        if (allocated(mountain_mesh%pm%slope)) deallocate(mountain_mesh%pm%slope)
        if (allocated(mountain_mesh%pm%aspect)) deallocate(mountain_mesh%pm%aspect)
        if (allocated(mountain_mesh%pm%delta)) deallocate(mountain_mesh%pm%delta)
        if (allocated(mountain_mesh%pm%curvature)) deallocate(mountain_mesh%pm%curvature)

        !> Print summary and remark that the process is active.
        call reset_tab()
        call print_message('MOUNTAINMESH is ACTIVE.')
        call increase_tab()

        !> Print configuration information to file if 'DIAGNOSEMODE' is active.
        if (DIAGNOSEMODE) then
            line = 'MOUNTAINMESH on'
            write(val, FMT_GEN) mountain_mesh%pm%Time_Zone
            line = trim(line) // ' Time_Zone=' // trim(adjustl(val))
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
            write(val, FMT_GEN) mountain_mesh%pm%idecl
            line = trim(line) // ' idecl=' // trim(adjustl(val))
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
        if (mountain_mesh%pm%iwind == 1 .and. .not. cm%dat(ck%WD)%factive) then
            call print_error("'iwind' option 1 requires wind direction, but the driving variable is not active.")
            ierr = 1
        end if

        !> Data tables.
        select case (mountain_mesh%pm%ilapse)
            case (1)

                !> Option 1:
                !>  Tables of mean annual lapse rate derived from the high
                !>  resolution (2.5km by 2.5km) GEM run for the period Oct, 2016
                !>  to Sept, 2019.
                mountain_mesh%pm%plapse = (/ 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30 /)
                mountain_mesh%pm%tlapse = (/ 6.60, 6.60, 6.60, 6.60, 6.60, 6.60, 6.60, 6.60, 6.60, 6.60, 6.60, 6.60 /)
                mountain_mesh%pm%dtlapse = (/ 2.92, 2.92, 2.92, 2.92, 2.92, 2.92, 2.92, 2.92, 2.92, 2.92, 2.92, 2.92 /)
                mountain_mesh%pm%lwlapse = (/ 18.35, 18.35, 18.35, 18.35, 18.35, 18.35, 18.35, 18.35, 18.35, 18.35, 18.35, 18.35 /)
                mountain_mesh%pm%wlapse = (/ 0.21, 0.21, 0.21, 0.21, 0.21, 0.21, 0.21, 0.21, 0.21, 0.21, 0.21, 0.21 /)
            case (2)

                !> Option 2:
                !>  Tables of monthly lapse rate derived from the high
                !>  resolution (2.5km by 2.5km) GEM run for the period Oct, 2016
                !>  to Sept, 2019.
                mountain_mesh%pm%plapse = (/ 0.516, 0.306, 0.420, 0.263, 0.084, 0.164, 0.158, 0.219, 0.206, 0.461, 0.528, 0.342 /)
                mountain_mesh%pm%tlapse = (/ 5.479, 5.830, 5.683, 6.991, 8.107, 7.940, 6.700, 6.648, 6.177, 6.729, 7.080, 5.791 /)
                mountain_mesh%pm%dtlapse = (/ 1.986, 2.567, 1.941, 2.892, 2.747, 3.267, 4.834, 3.802, 3.257, 3.145, 2.505, 2.126 /)
                mountain_mesh%pm%lwlapse = &
                    (/ 6.832, 8.647, 4.803, 17.993, 31.842, 28.492, 36.013, 33.234, 25.388, 12.674, 0.288, 13.972 /)
                mountain_mesh%pm%wlapse = (/ 0.26, 0.32, 0.16, 0.22, 0.23, 0.26, 0.22, 0.12, 0.16, 0.13, 0.14, 0.20 /)
            case (3)

                !> Option 3:
                !>  Tables of temperature lapse rate, vapor pressure coefficient
                !>  (Kunkel et al., 1989), and precipitation-elevation
                !>  adjustment factors (Thornton et al., 1997) for each month
                !>  for the Northern Hemisphere. Incoming long wave radiation
                !>  lapse rate of 29 W/m^2/1000 m (Marty et al., 2002).
                mountain_mesh%pm%plapse = (/ 0.35, 0.35, 0.35, 0.30, 0.25, 0.20, 0.20, 0.20, 0.20, 0.25, 0.30, 0.35 /)
                mountain_mesh%pm%tlapse = (/ 4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70 /)
                mountain_mesh%pm%dtlapse = (/ 5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51 /)
                mountain_mesh%pm%lwlapse = (/ 29.0, 29.0, 29.0, 29.0, 29.0, 29.0, 29.0, 29.0, 29.0, 29.0, 29.0, 29.0 /)

                !> Check against 'iwind' (no 'wlapse').
                if (mountain_mesh%pm%iwind == 2) then
                    call print_error( &
                        "'iwind' option 2 cannot be used with ilapse option 3, as the option does not provide 'wlapse'.")
                    ierr = 1
                end if
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
                if (mountain_mesh%pm%iwind == 2) then
                    call print_error("'iwind' option 2 cannot be used without any ilapse option to define 'wlapse'.")
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

    end subroutine

    subroutine mountain_within_tile(fls, shd, cm)

        !> Required for MESH variables.
        use model_files_variables
        use sa_mesh_common
        use climate_forcing

        !> Required for 'il1', 'il2', and 'iln'.
        !> Because 'il1' and 'il2' are not passed to subroutine, the
        !> subset of the stride is used here instead.
        use mpi_module

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd

        !> Input/output variables.
        type(clim_info) :: cm

        !> Local variables.
        integer k
        real, dimension(il1:il2) :: rsrd_adjusted
        real, dimension(il1:il2) :: rlds_adjusted
        real, dimension(il1:il2) :: temp_adjusted
        real, dimension(il1:il2) :: pres_adjusted
        real, dimension(il1:il2) :: humd_adjusted
        real, dimension(il1:il2) :: rain_adjusted
        real, dimension(il1:il2) :: rain_phased_adjusted
        real, dimension(il1:il2) :: snow_phased_adjusted
        real, dimension(il1:il2) :: wind_adjusted

        !> Return if module is not enabled.
        if (.not. mountain_mesh%PROCESS_ACTIVE) return

        !> Call routine to calculate adjusted radiation value.
        call forcing_adjust( &
            mountain_mesh%vs%elev(il1:il2), mountain_mesh%vs%xlng(il1:il2), &
            mountain_mesh%vs%ylat(il1:il2), mountain_mesh%vs%slope(il1:il2), &
            mountain_mesh%vs%aspect(il1:il2), mountain_mesh%vs%delta(il1:il2), &
            mountain_mesh%vs%curvature(il1:il2), iln, &
            shd%lc%ILMOS(il1:il2), i1, i2, &
            mountain_mesh%pm%Time_Zone, &
            mountain_mesh%pm%CalcFreq, &
            mountain_mesh%pm%ipre, mountain_mesh%pm%itemp, mountain_mesh%pm%ipres, &
            mountain_mesh%pm%ihumd, mountain_mesh%pm%irlds, mountain_mesh%pm%iwind, &
            mountain_mesh%pm%iphase, mountain_mesh%pm%irsrd, mountain_mesh%pm%idecl, &
            mountain_mesh%pm%tlapse, mountain_mesh%pm%plapse, mountain_mesh%pm%dtlapse, &
            mountain_mesh%pm%lwlapse, mountain_mesh%pm%wlapse, &
            cm%dat(ck%FB)%hf, &
            cm%dat(ck%FI)%hf, &
            cm%dat(ck%TT)%hf, &
            cm%dat(ck%P0)%hf, &
            cm%dat(ck%HU)%hf, &
            cm%dat(ck%RT)%hf, &
            cm%dat(ck%UV)%hf, &
            cm%dat(ck%WD)%hf, &
            cm%dat(ck%FB)%GAT(il1:il2), &
            cm%dat(ck%FI)%GAT(il1:il2), &
            cm%dat(ck%TT)%GAT(il1:il2), &
            cm%dat(ck%P0)%GAT(il1:il2), &
            cm%dat(ck%HU)%GAT(il1:il2), &
            cm%dat(ck%RT)%GAT(il1:il2), &
            cm%dat(ck%UV)%GAT(il1:il2), &
            cm%dat(ck%WD)%GAT(il1:il2), &
            rsrd_adjusted(il1:il2), &
            rlds_adjusted(il1:il2), &
            temp_adjusted(il1:il2), &
            pres_adjusted(il1:il2), &
            humd_adjusted(il1:il2), &
            rain_adjusted(il1:il2), &
            rain_phased_adjusted(il1:il2), &
            snow_phased_adjusted(il1:il2), &
            wind_adjusted(il1:il2), &
            ic%now%year, ic%now%month, ic%now%jday, &
            ic%now%hour, ic%now%mins, ic%dtmins)

        !> Update radiation.
        !> Must update 'GRD' separately for output (e.g., energy_balance.csv).
        cm%dat(ck%FB)%GAT(il1:il2) = rsrd_adjusted(il1:il2)
        cm%dat(ck%FI)%GAT(il1:il2) = rlds_adjusted(il1:il2)
        cm%dat(ck%TT)%GAT(il1:il2) = temp_adjusted(il1:il2)
        cm%dat(ck%P0)%GAT(il1:il2) = pres_adjusted(il1:il2)
        cm%dat(ck%HU)%GAT(il1:il2) = humd_adjusted(il1:il2)
        cm%dat(ck%RT)%GAT(il1:il2) = rain_adjusted(il1:il2)
        vs%tile%prern(il1:il2) = rain_phased_adjusted(il1:il2)
        vs%tile%presno(il1:il2) = snow_phased_adjusted(il1:il2)
        cm%dat(ck%UV)%GAT(il1:il2) = wind_adjusted(il1:il2)
        cm%dat(ck%FB)%GRD = 0.0
        cm%dat(ck%FI)%GRD = 0.0
        cm%dat(ck%TT)%GRD = 0.0
        cm%dat(ck%P0)%GRD = 0.0
        cm%dat(ck%HU)%GRD = 0.0
        cm%dat(ck%RT)%GRD = 0.0
        vs%grid%prern = 0.0
        vs%grid%presno = 0.0
        cm%dat(ck%UV)%GRD = 0.0
        do k = il1, il2
            cm%dat(ck%FB)%GRD(shd%lc%ILMOS(k)) = cm%dat(ck%FB)%GRD(shd%lc%ILMOS(k)) + &
                rsrd_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            cm%dat(ck%FI)%GRD(shd%lc%ILMOS(k)) = cm%dat(ck%FI)%GRD(shd%lc%ILMOS(k)) + &
                rlds_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            cm%dat(ck%TT)%GRD(shd%lc%ILMOS(k)) = cm%dat(ck%TT)%GRD(shd%lc%ILMOS(k)) + &
                temp_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            cm%dat(ck%P0)%GRD(shd%lc%ILMOS(k)) = cm%dat(ck%P0)%GRD(shd%lc%ILMOS(k)) + &
                pres_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            cm%dat(ck%HU)%GRD(shd%lc%ILMOS(k)) = cm%dat(ck%HU)%GRD(shd%lc%ILMOS(k)) + &
                humd_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            cm%dat(ck%RT)%GRD(shd%lc%ILMOS(k)) = cm%dat(ck%RT)%GRD(shd%lc%ILMOS(k)) + &
                rain_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            vs%grid%prern(shd%lc%ILMOS(k)) = vs%grid%prern(shd%lc%ILMOS(k)) + &
                rain_phased_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            vs%grid%presno(shd%lc%ILMOS(k)) = vs%grid%presno(shd%lc%ILMOS(k)) + &
                snow_phased_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            cm%dat(ck%UV)%GRD(shd%lc%ILMOS(k)) = cm%dat(ck%UV)%GRD(shd%lc%ILMOS(k)) + &
                wind_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
        end do

    end subroutine

end module
