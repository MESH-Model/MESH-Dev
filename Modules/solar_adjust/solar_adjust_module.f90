!> Description:
!>  Calculate theoretical shortwave radiation using method proposed by
!>  Garnier and Ohmura (1970) to adjust for slope and aspect.
!>  Calls "calc_rsrd_adjusted".
!>
!> Author: Zelalem Tesemma (of Solar_Adjust.m; last updated Jan 30, 2018)
!> Converted to Fortran: Feb 1, 2018 (exact)
!> Fortran code optimized/consolidated; subroutine isolated
!>  ('program' component replaced by 'solar_adjust_module';
!>      'solar_adjust_module' coupled with MESH): Feb 2, 2018.
module solar_adjust_module

    implicit none

    !> Description:
    !>  Type for parameters (options).
    !>
    !> Variables:
    !*  Time_Zone: The time zone of the study area.
    !*  CalcFreq: Iterations per day (must divide the day into equal increments of minutes). [--].
    !*  gru_elev: Weighted average elevation of GRUs. [m].
    !*  gru_delta: Weighted average elevation difference between high (MESH) and low (GEM) resolution elevation. [m].
    !*  gru_slope: Weighted average slope of surface (1: grid; 2: GRU). [degrees].
    !*  gru_aspect: Weighted average aspect of surface (1: grid; 2: GRU). [degrees].
    type solar_adjust_parameters
        real :: Time_Zone = -6.0
        integer :: CalcFreq = 288
        real, dimension(:, :), allocatable :: elev, slope, aspect, delta
    end type

    !> Description:
    !> Type for variables/constants.
    !>
    !> Variables:
    !*  xlng: Longitude. [degrees].
    !*  ylat: Latitude. [degrees].
    !*  gru_elev: Weighted average elevation. [m].
    !*  gru_slope: Weighted average slope of the surface. [degrees].
    !*  gru_aspect: Weighted average aspect of the surface. [degrees].
    type solar_adjust_variables
        real, dimension(:), allocatable :: elev, xlng, ylat, slope, aspect, delta
    end type

    !> Description:
    !> Type for 'solar_adjust' parameters and variables.
    !>
    !> Variables:
    !*  pm: Parameters and options.
    !*  vs: Variables.
    !*  PROCESS_ACTIVE: .true. to enable 'Solar_Adjust; .false. otherwise (default: .false.).
    type solar_adjust_container
        type(solar_adjust_parameters) pm
        type(solar_adjust_variables) vs
        logical :: PROCESS_ACTIVE = .false.
        character(len = 1000) :: RUNOPTIONSFLAG = ''
    end type

    !* fsadj: Instance of 'solar_adjust' parameters and variables.
    type(solar_adjust_container), save :: rsrd_adj

    contains

    subroutine solar_adjust_extract_value(arg, ierr)

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
                call value(args(2), rsrd_adj%pm%Time_Zone, ierr)
            case ('calcfreq')
                call value(args(2), rsrd_adj%pm%CalcFreq, ierr)
        end select

    end subroutine

    subroutine solar_adjust_parse_options(flg, ierr)

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
            rsrd_adj%PROCESS_ACTIVE = .true.
        else
            return
        end if

        !> Parse and check the keywords.
        call parse(flg, ' ', args, nargs)
        do i = 1, nargs

            !> Exit if any of the keywords have disabled the routine.
            if (.not. rsrd_adj%PROCESS_ACTIVE) return
            select case (lowercase(args(i)))

                !> 'off' disables the routine.
                case ('off')
                    rsrd_adj%PROCESS_ACTIVE = .false.
                    exit

            !> Options.
            case default
                call solar_adjust_extract_value(args(i), ierr)
            end select
        end do

    end subroutine

    subroutine solar_adjust_init(fls, shd, cm)

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
        call solar_adjust_parse_options(rsrd_adj%RUNOPTIONSFLAG, ierr)

        !> Return if module is not enabled.
        if (.not. rsrd_adj%PROCESS_ACTIVE) then
            if (allocated(rsrd_adj%pm%slope)) deallocate(rsrd_adj%pm%slope)
            if (allocated(rsrd_adj%pm%aspect)) deallocate(rsrd_adj%pm%aspect)
            if (allocated(rsrd_adj%pm%delta)) deallocate(rsrd_adj%pm%delta)
            return
        end if

        !> Allocate variables.
        allocate( &
            rsrd_adj%vs%elev(il1:il2), rsrd_adj%vs%xlng(il1:il2), &
            rsrd_adj%vs%ylat(il1:il2), rsrd_adj%vs%slope(il1:il2), &
            rsrd_adj%vs%aspect(il1:il2), rsrd_adj%vs%delta(il1:il2))
        rsrd_adj%vs%slope = 0.0
        rsrd_adj%vs%aspect = 0.0
        rsrd_adj%vs%delta = 0.0

        !> Assign values.
        do k = il1, il2

            !> Pull generic values from drainage_database.r2c
            rsrd_adj%vs%elev(k) = shd%ELEV(shd%lc%ILMOS(k))
            rsrd_adj%vs%xlng(k) = shd%XLNG(shd%lc%ILMOS(k))
            rsrd_adj%vs%ylat(k) = shd%YLAT(shd%lc%ILMOS(k))

            !> Overwrite with values provided by GRU (e.g., parameters.r2c).
            if (allocated(rsrd_adj%pm%elev)) then
                rsrd_adj%vs%elev(k) = rsrd_adj%pm%elev(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (allocated(rsrd_adj%pm%slope)) then
                rsrd_adj%vs%slope(k) = rsrd_adj%pm%slope(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
                pm%tp%xslp(k) = tand(rsrd_adj%pm%slope(shd%lc%ILMOS(k), shd%lc%JLMOS(k)))   ! Overwrite Estimated average slope of the GRU
            end if
            if (allocated(rsrd_adj%pm%aspect)) then
                rsrd_adj%vs%aspect(k) = rsrd_adj%pm%aspect(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
            if (allocated(rsrd_adj%pm%delta)) then
                rsrd_adj%vs%delta(k) = rsrd_adj%pm%delta(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end if
        end do

        !> De-allocate 'ROW' based fields (from parameters file).
        if (allocated(rsrd_adj%pm%slope)) deallocate(rsrd_adj%pm%slope)
        if (allocated(rsrd_adj%pm%aspect)) deallocate(rsrd_adj%pm%aspect)
        if (allocated(rsrd_adj%pm%delta)) deallocate(rsrd_adj%pm%delta)

        !> Print summary and remark that the process is active.
        call print_message('SOLARADJUSTFLAG is ACTIVE.')

        !> Print configuration information to file if 'DIAGNOSEMODE' is active.
        if (DIAGNOSEMODE) then
            write(line, FMT_GEN) 'Time_Zone', rsrd_adj%pm%Time_Zone
            call print_message_detail(line)
            write(line, FMT_GEN) 'CalcFreq', rsrd_adj%pm%CalcFreq
            call print_message_detail(line)
        end if

        !> Check values, print error messages for invalid values.
        !> The check is of 'GAT'-based variables, for which all tiles should have valid values.
        ierr = 0
        if (mod(24*60, rsrd_adj%pm%CalcFreq) /= 0) then
            write(line, FMT_GEN) rsrd_adj%pm%CalcFreq
            call print_message_detail( &
                'ERROR: CalcFreq must evenly divide into minutes in the day. 1440 mod ' // trim(adjustl(line)) // ' /= 0.')
            ierr = 1
        end if
        if (any(rsrd_adj%vs%elev < 0.0)) then
            call print_message_detail('ERROR: Values of ELEVATION are less than zero.')
            ierr = 1
        end if
        if (any(rsrd_adj%vs%slope < 0.0)) then
            call print_message_detail('ERROR: Values of SLOPE are less than zero.')
            ierr = 1
        end if
        if (any(rsrd_adj%vs%aspect < 0.0)) then
            call print_message_detail('ERROR: Values of ASPECT are less than zero.')
            ierr = 1
        end if
        if (ierr /= 0) call program_abort()

    end subroutine

    subroutine solar_adjust_within_tile(fls, shd, cm)

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
        if (.not. rsrd_adj%PROCESS_ACTIVE) return

        !> Call routine to calculate adjusted radiation value.
        call calc_rsrd_adjusted( &
            rsrd_adj%vs%elev(il1:il2), rsrd_adj%vs%xlng(il1:il2), &
            rsrd_adj%vs%ylat(il1:il2), &
            rsrd_adj%vs%slope(il1:il2), rsrd_adj%vs%aspect(il1:il2), &
            rsrd_adj%vs%delta(il1:il2), iln, &
            rsrd_adj%pm%Time_Zone, rsrd_adj%pm%CalcFreq, &
            cm%dat(ck%FB)%hf, &
            cm%dat(ck%FI)%hf, &
            cm%dat(ck%TT)%hf, &
            cm%dat(ck%P0)%hf, &
            cm%dat(ck%HU)%hf, &
            cm%dat(ck%RT)%hf, &
            cm%dat(ck%UV)%hf, &
            cm%dat(ck%FB)%GAT(il1:il2), &
            cm%dat(ck%FI)%GAT(il1:il2), &
            cm%dat(ck%TT)%GAT(il1:il2), &
            cm%dat(ck%P0)%GAT(il1:il2), &
            cm%dat(ck%HU)%GAT(il1:il2), &
            cm%dat(ck%RT)%GAT(il1:il2), &
            cm%dat(ck%UV)%GAT(il1:il2), &
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

            !> Update output variables.

    end subroutine

end module
