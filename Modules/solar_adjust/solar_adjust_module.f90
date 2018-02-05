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
    !*  Trans: Mean transmissivity of the atmosphere.
    !*  Time_Offset: Solar time offset from local time.
    !*  CalcFreq: Iterations per day (must divide the day into equal increments of minutes). [--].
    type solar_adjust_parameters
        real :: Trans = 0.818
        real :: Time_Offset = 0.67
        integer :: CalcFreq = 288
    end type

    !> Description:
    !>  Type for variables/constants.
    !>
    !> Variables:
    !*  elev: Elevation. [m].
    !*  ylat: Latitude. [degrees].
    !*  slope: Slope of surface. [--].
    !*  aspect: Aspect of surface.
    type solar_adjust_variables
        real, dimension(:), allocatable :: elev, ylat, slope, aspect
    end type

    !> Description:
    !>  Type for 'solar_adjust' parameters and variables.
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
            case ('trans')
                call value(args(2), rsrd_adj%pm%Trans, ierr)
            case ('time_offset')
                call value(args(2), rsrd_adj%pm%Time_Offset, ierr)
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
        use sa_mesh_shared_variables
        use climate_forcing
        use FLAGS

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
        integer :: iun = 58
        logical print_out, write_out

        !> Parse options.
        call solar_adjust_parse_options(rsrd_adj%RUNOPTIONSFLAG, ierr)

        !> Return if module is not enabled.
        if (.not. rsrd_adj%PROCESS_ACTIVE) return

        !> Print summary and remark that the process is active.
        !> Print configuration information to file if 'DIAGNOSEMODE' is active.
        print_out = (ro%VERBOSEMODE > 0)
        write_out = (ro%VERBOSEMODE > 0 .and. MODELINFOOUTFLAG > 0)
        if (write_out) write(iun, 1100)
        if (write_out .and. ro%DIAGNOSEMODE > 0) then
            write(iun, 1110) 'Trans', rsrd_adj%pm%Trans
            write(iun, 1110) 'Time_Offset', rsrd_adj%pm%Time_Offset
            write(iun, 1110) 'CalcFreq', rsrd_adj%pm%CalcFreq
            write(iun, *)
        end if

        !> Check values, print error messages for invalid values.
        if (mod(24*60, rsrd_adj%pm%CalcFreq) /= 0) then
            if (print_out) print 1120, rsrd_adj%pm%CalcFreq
            if (write_out) write(iun, 1120) rsrd_adj%pm%CalcFreq
            stop
        end if

        !> Allocate variables.
        allocate( &
            rsrd_adj%vs%elev(il1:il2), rsrd_adj%vs%ylat(il1:il2), rsrd_adj%vs%slope(il1:il2), &
            rsrd_adj%vs%aspect(il1:il2))
        rsrd_adj%vs%slope = 0.0
        rsrd_adj%vs%aspect = 0.0

        !> Assign values.
        do k = il1, il2
            rsrd_adj%vs%elev(k) = shd%ELEV(shd%lc%ILMOS(k))
            rsrd_adj%vs%ylat(k) = shd%YLAT(shd%lc%ILMOS(k))
        end do

        !> Format statements.
1100    format(1x, "SOLAR_ADJUST is ACTIVE.")
1110    format(999(1x, g16.9))
1120    format( &
            /1x, "ERROR: CalcFreq must evenly divide into minutes in the day.", &
            /3x, "1440 mod ", i4, " /= 0")
1130    format(3x, (a))

    end subroutine

    subroutine solar_adjust_within_tile(fls, shd, cm)

        !> Required for MESH variables.
        use model_files_variables
        use sa_mesh_shared_variables
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
        real, dimension(il1:il2) :: rsrd_direct, rsrd_diffuse, rsrd_adjusted

        !> Return if module is not enabled.
        if (.not. rsrd_adj%PROCESS_ACTIVE) return

        !> Call routine to calculate adjusted radiation value.
        call calc_rsrd_adjusted( &
            rsrd_adj%vs%elev(il1:il2), rsrd_adj%vs%ylat(il1:il2), &
            rsrd_adj%vs%slope(il1:il2), rsrd_adj%vs%aspect(il1:il2), iln, &
            rsrd_adj%pm%Trans, rsrd_adj%pm%Time_Offset, rsrd_adj%pm%CalcFreq, &
            cm%dat(ck%FB)%hf, &
            cm%dat(ck%FB)%GAT(il1:il2), &
            rsrd_direct(il1:il2), rsrd_diffuse(il1:il2), rsrd_adjusted(il1:il2), &
            ic%now%year, ic%now%jday, ic%now%hour)

        !> Update radiation.
        !> Must update 'GRD' separately for output (e.g., energy_balance.csv).
        cm%dat(ck%FB)%GAT(il1:il2) = rsrd_adjusted(il1:il2)
        cm%dat(ck%FB)%GRD = 0.0
        do k = il1, il2
            cm%dat(ck%FB)%GRD(shd%lc%ILMOS(k)) = cm%dat(ck%FB)%GRD(shd%lc%ILMOS(k)) + &
                rsrd_adjusted(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
        end do

        !> Update output variables.

    end subroutine

end module
