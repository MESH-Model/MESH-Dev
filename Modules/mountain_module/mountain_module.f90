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
    !*  elev: Weighted average elevation of GRUs. [m].
    !*  delta: Weighted average elevation difference between high (MESH) and low (GEM) resolution elevation. [m].
    !*  slope: Weighted average slope of surface (1: grid; 2: GRU). [degrees].
    !*  aspect: Weighted average aspect of surface (1: grid; 2: GRU). [degrees].
    !*  curvature: Weighted average curvature of the surface. [--].
    type mountain_parameters
        real :: Time_Zone = -6.0
        integer :: CalcFreq = 288
        real, dimension(:, :), allocatable :: elev, slope, aspect, delta, curvature
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
        integer nargs
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
        select case (lowercase(args(1)))
            case ('time_zone')
                call value(args(2), mountain_mesh%pm%Time_Zone, ierr)
            case ('calcfreq')
                call value(args(2), mountain_mesh%pm%CalcFreq, ierr)
        end select

    end subroutine

    subroutine mountain_parse_options(flg, ierr)

        !> Required for the 'parse' and 'lowercase' functions.
        use strings

        !> Input variables.
        character(len = *), intent(in) :: flg

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer i, nargs
        character(len = 20) args(20)

        !> Status.
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
        do i = 1, nargs

            !> Exit if any of the keywords have disabled the routine.
            if (.not. mountain_mesh%PROCESS_ACTIVE) return

            !> Specific options.
            select case (lowercase(args(i)))
                case ('off', '0')

                    !> 'off' or '0' disables the routine.
                    mountain_mesh%PROCESS_ACTIVE = .false.
                    exit
                case default

                    !> Other options.
                    call mountain_extract_value(args(i), ierr)
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
                pm%tile%xslp(k) = tan(mountain_mesh%pm%slope(shd%lc%ILMOS(k), shd%lc%JLMOS(k)))*180.0/3.14159265
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
        call print_message('MOUNTAINMESH is ACTIVE.')

        !> Print configuration information to file if 'DIAGNOSEMODE' is active.
        if (DIAGNOSEMODE) then
            write(line, FMT_GEN) 'Time_Zone', mountain_mesh%pm%Time_Zone
            call print_message_detail(line)
            write(line, FMT_GEN) 'CalcFreq', mountain_mesh%pm%CalcFreq
            call print_message_detail(line)
        end if

        !> Check values, print error messages for invalid values.
        !> The check is of 'GAT'-based variables, for which all tiles should have valid values.
        ierr = 0
        if (mod(24*60, mountain_mesh%pm%CalcFreq) /= 0) then
            write(line, FMT_GEN) mountain_mesh%pm%CalcFreq
            call print_message_detail( &
                'ERROR: CalcFreq must evenly divide into minutes in the day. 1440 mod ' // trim(adjustl(line)) // ' /= 0.')
            ierr = 1
        end if
        if (any(mountain_mesh%vs%elev < 0.0)) then
            call print_message_detail('ERROR: Values of ELEVATION are less than zero.')
            ierr = 1
        end if
        if (any(mountain_mesh%vs%slope < 0.0)) then
            call print_message_detail('ERROR: Values of SLOPE are less than zero.')
            ierr = 1
        end if
        if (any(mountain_mesh%vs%aspect < 0.0)) then
            call print_message_detail('ERROR: Values of ASPECT are less than zero.')
            ierr = 1
        end if
        if (ierr /= 0) call program_abort()

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
            mountain_mesh%pm%CalcFreq, cm%dat(ck%FB)%hf, &
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
        cm%dat(ck%UV)%GAT(il1:il2) = wind_adjusted(il1:il2)
        cm%dat(ck%FB)%GRD = 0.0
        cm%dat(ck%FI)%GRD = 0.0
        cm%dat(ck%TT)%GRD = 0.0
        cm%dat(ck%P0)%GRD = 0.0
        cm%dat(ck%HU)%GRD = 0.0
        cm%dat(ck%RT)%GRD = 0.0
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
            cm%dat(ck%UV)%GRD(shd%lc%ILMOS(k)) = cm%dat(ck%UV)%GRD(shd%lc%ILMOS(k)) + &
                wind_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
        end do

    end subroutine

end module
