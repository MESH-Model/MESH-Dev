!>
!> FOR SPL WATROUTE (MODIFIED RPN CODE)
!>
!> Description: Write output files for for offline routing. Compatible
!>              with the older RPN standalone RTE code.
!>
!> Updated:
!>  2015-11-09  DGP - Pulled code from MESH_Driver.f90.
!>
module process_SA_RTE

    use model_files_variabletypes, only: fl_ids

    implicit none

    !> Option flags for offline routing.
    type SA_RTE_flags

        !> Flag used to active the process module.
        logical :: PROCESS_ACTIVE = .false.

        !> Format of the variable name is PRINT(VARIABLE_TERM)R2CFILEFLAG.
        !>   PRINT(VARIABLE_TERM)R2CFILEFLAG = 0 means no output is written.
        !>   PRINT(VARIABLE_TERM)R2CFILEFLAG = 1 means output is written.
        integer :: PRINTRFFR2CFILEFLAG = 1
        integer :: PRINTRCHR2CFILEFLAG = 1
        integer :: PRINTLKGR2CFILEFLAG = 0

    end type

    !* SA_RTE_flgs: Current configuration of the module. Sub-keys of
    !*                this object control what output is written for
    !*                offline routing.
    type(SA_RTE_flags), save :: SA_RTE_flgs

    type SA_RTE_file_keys

        !> WR_runoff.r2c
        integer :: RFF = 1

        !> WR_recharge.r2c
        integer :: RCH = 2

    end type

    type(fl_ids), save :: SA_RTE_fls

    type(SA_RTE_file_keys), save :: SA_RTE_flkeys

    !* frame_now: Number of current frame in output file.
    integer frame_now

    !* RFF: Hourly simulated runoff. [mm].
    !* RCH: Hourly simulated recharge. [mm].
    !* LKG: (Not known but may used in the future).
    real, dimension(:, :), allocatable :: RFF, RCH, LKG

    contains

    !> Description: Write output of runoff variables to file for offline
    !>              routing. Data are written in R2C format and are
    !>              compatible with the old RPN RTE code.
    subroutine run_SA_RTE(shd, ic, wb)

        !> For: type(ShedGridParams) :: shd; cops
        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables

        !> For: type(iter_counter) :: ic
        use model_dates

        !> For: type(water_balance) :: wb
        use MODEL_OUTPUT

        !> Input variables.
        !* shd: Basin and watershed information.
        !* ic: Active counter.
        !* wb: Water balance at the current time-step.
        type(ShedGridParams), intent(in) :: shd
        type(iter_counter), intent(in) :: ic
        type(water_balance), intent(in) :: wb

        !> Return if the process is not active.
        if (.not. SA_RTE_flgs%PROCESS_ACTIVE) return

!todo: these can be removed at some point, as they've been added
!todo: as flags as a part of the model_output module.
        !> Call the tile connector to accumulate half-hourly values.
        call tile_connector(shd, RFF, RCH, LKG, ic%ts_daily, wb%rofo, wb%rofs, wb%rofb, ic%dts)

        !> Offline routing reads data every hour so data are written on
        !> the hour.
        if (ic%now_mins == 0) then

            !> The hour written to file is the model hour +1 as
            !> SA_MESH runs from 0-23 but the offline routing runs 1-24.
            !> The total number of frames in the output file is not
            !> usually known, so is set to (frame_now + 1).
            !> For: Runoff.
            if (SA_RTE_flgs%PRINTRFFR2CFILEFLAG == 1) then
                call write_r2c(SA_RTE_fls, SA_RTE_flkeys%RFF, shd, &
                               (frame_now + 1), 1, frame_now, 1, 6, &
                               ic%now_year, ic%now_month, ic%now_day, (ic%now_hour + 1), &
                               RFF)
            end if

            !> For: Recharge.
            if (SA_RTE_flgs%PRINTRCHR2CFILEFLAG == 1) then
                call write_r2c(SA_RTE_fls, SA_RTE_flkeys%RCH, shd, &
                               (frame_now + 1), 1, frame_now, 1, 6, &
                               ic%now_year, ic%now_month, ic%now_day, (ic%now_hour + 1), &
                               RCH)
            end if

            !> Update frame counters.
            frame_now = frame_now + 1

        end if !(ic%now_mins == 0) then

    end subroutine

    subroutine configure_SA_RTE_fls()

        !> Allocate file object.
        allocate(SA_RTE_fls%fl(2))

    end subroutine

    subroutine configure_SA_RTE(shd, ic)

        !> For: type(ShedGridParams) :: shd; cops
        use sa_mesh_shared_variabletypes

        !> For: type(iter_counter) :: ic
        use model_dates

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        type(iter_counter), intent(in) :: ic

        !> Local variables.
        integer ierr

        !> Return if the process is not active.
        if (.not. SA_RTE_flgs%PROCESS_ACTIVE) return

        !> Allocate variables used by the process module.
        allocate(RFF(shd%yCount, shd%xCount), &
                 RCH(shd%yCount, shd%xCount), &
                 LKG(shd%yCount, shd%xCount), stat = ierr)

        !> Stop if an allocation error has occured.
        if (ierr /= 0) then
            print 1114, 'Standalone RTE input'
            print 1118, 'Grid square rows', shd%yCount
            print 1118, 'Grid square columns', shd%xCount
            stop
        end if

1114 format(/1x, 'Error allocating ', a, ' variables.', &
            /1x, 'Check that these bounds are within an acceptable range.', /)
1118 format(3x, a, ': ', i6)

        !> Initialize variables arrays.
        RFF = 0.0
        RCH = 0.0
        LKG = 0.0

        !> Initialize counter for frames.
        frame_now = 1

        !> Write header information to the output files.
        !> The active variable should align to MODELFLG in the old RPN RTE code.
        !>   MODELFLG = (i, r, l) then runoff   (RFF)
        !>   MODELFLG =     r     then recharge (RCH)
        !>   MODELFLG =        l  then leakage  (LKG)
        !> Output of leakage is not currently supported.
        !> File names could be read from the event file. However, at
        !> present these are hard-coded or set using VARIABLEFILESFLAG).
        !> For: Runoff (MODELFLG = (r, l, i)).
        if (SA_RTE_flgs%PRINTRFFR2CFILEFLAG == 1) then
            call write_r2c(SA_RTE_fls, SA_RTE_flkeys%RFF, shd, &
                           1, 0, 1, 1, &
                           ic%now_year, ic%now_month, ic%now_day, (ic%now_hour + 1), &
                           RFF, &
!todo: replace source with LSS flag
                           'channel_inflow', 'mm', 'flow', 'CLASS', 'SA_MESH_DRIVER')
        end if

        !> For: Recharge (MODELFLG = r).
        if (SA_RTE_flgs%PRINTRCHR2CFILEFLAG == 1) then
            call write_r2c(SA_RTE_fls, SA_RTE_flkeys%RCH, shd, &
                           0, 1, 0, 1, 1, &
                           ic%now_year, ic%now_month, ic%now_day, (ic%now_hour + 1), &
                           RCH, &
!todo: replace source with LSS flag
                           'recharge', 'mm', 'flow', 'CLASS', 'SA_MESH_DRIVER')
        end if

    end subroutine

end module
