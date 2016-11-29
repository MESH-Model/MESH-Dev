!>
!> FOR SPL WATROUTE (MODIFIED RPN CODE)
!>
!> Description: Write output files for for offline routing. Compatible
!>              with the older RPN standalone RTE code.
!>
!> Updated:
!>  2015-11-09  DGP - Pulled code from MESH_Driver.f90.
!>
module SA_RTE_module

    use model_files_variabletypes, only: fl_ids

    implicit none

    !> Option flags for offline routing.
    type SA_RTE_flags

        !> Flag used to enable the module.
        logical :: PROCESS_ACTIVE = .false.

        !> Format of the variable name is PRINT(VARIABLE_TERM)R2CFILEFLAG.
        !>   PRINT(VARIABLE_TERM)R2CFILEFLAG = 0 means no output is written.
        !>   PRINT(VARIABLE_TERM)R2CFILEFLAG = 1 means output is written.
        integer :: PRINTRFFR2CFILEFLAG = 1
        integer :: PRINTRCHR2CFILEFLAG = 1
!        integer :: PRINTLKGR2CFILEFLAG = 0

    end type

    !* SA_RTE_flgs: Configuration flags for the module. Sub-keys of
    !*              this object control what output is written for
    !*              offline routing.
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
    !* LKG: Leakage is not used.
    real, dimension(:, :), allocatable :: RFF, RCH

    contains

    !> Description: Write output of runoff variables to file for offline
    !>              routing. Data are written in R2C format and are
    !>              compatible with the old RPN RTE code.
!todo: these can be removed at some point, as they've been added
!todo: as flags as a part of the model_output module.
    subroutine SA_RTE(shd, wb)

        !> For: type(ShedGridParams) :: shd; cops
        use sa_mesh_shared_variables

        !> For: type(iter_counter) :: ic
        !* ic: Active counter.
        use model_dates

        !> For: type(water_balance) :: wb
        use MODEL_OUTPUT

        !> Input variables.
        !* shd: Basin and watershed information.
        !* wb: Water balance at the current time-step.
        type(ShedGridParams), intent(in) :: shd
        type(water_balance), intent(in) :: wb

        !> Local variables.
        logical writeout

        !> Return if the process is not active.
        if (.not. SA_RTE_flgs%PROCESS_ACTIVE) return

        !> Call the tile connector to accumulate half-hourly values for
        !> each variable.
        !>
        !> Offline routing reads data every hour so data are written
        !> the last time-step of the hour.
        !>
        !> The hour written to file is the model hour +1 as
        !> SA_MESH runs from 0-23 but the offline routing runs 1-24.
        !> The total number of frames in the output file is not
        !> usually known, so is set to (frame_now + 1).

        !> Determine if this is the last time-step of the hour.
        writeout = (mod(ic%ts_hourly, 3600/ic%dts) == 0)
!-        print *, ic%now_jday, ic%now_hour, ic%now_mins, writeout

        !> For: Runoff (RFF).
        if (SA_RTE_flgs%PRINTRFFR2CFILEFLAG == 1) then
            call tile_connector(shd, (wb%rofo + wb%rofs), RFF, .true.)
            if (writeout) then
                call write_r2c(SA_RTE_fls, SA_RTE_flkeys%RFF, shd, ic, (frame_now + 1), 0, frame_now, 0, 6, RFF)
                RFF = 0.0
            end if
        end if

        !> For: Recharge (RCH).
        if (SA_RTE_flgs%PRINTRCHR2CFILEFLAG == 1) then
            call tile_connector(shd, wb%rofb, RCH, .true.)
            if (writeout) then
                call write_r2c(SA_RTE_fls, SA_RTE_flkeys%RCH, shd, ic, (frame_now + 1), 0, frame_now, 0, 6, RCH)
                RCH = 0.0
            end if
        end if

        !> Update frame counters.
        if (writeout) frame_now = frame_now + 1

    end subroutine

    subroutine SA_RTE_init_fls()

        !> Allocate file object.
        allocate(SA_RTE_fls%fl(2))

    end subroutine

    subroutine SA_RTE_init(shd)

        !> For: type(ShedGridParams) :: shd, ro%, cops%
        use sa_mesh_shared_variables

        !> For: type(iter_counter) :: ic
        use model_dates

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

        !> Local variables.
        integer ierr

        !> Return if the process is not active.
        if (.not. SA_RTE_flgs%PROCESS_ACTIVE) return

        if (ro%VERBOSEMODE > 0) then
            print 1000
            print 1001
        end if

        !> Allocate and initialize the appropriate variables.
        if (SA_RTE_flgs%PRINTRFFR2CFILEFLAG == 1) then
            allocate(RFF(shd%yCount, shd%xCount), stat = ierr)
            if (ierr /= 0) then
                print 1004, 'RFF', shd%yCount, shd%xCount
                stop
            end if
            RFF = 0.0
        end if

        if (SA_RTE_flgs%PRINTRCHR2CFILEFLAG == 1) then
            allocate(RCH(shd%yCount, shd%xCount), stat = ierr)
            if (ierr /= 0) then
                print 1004, 'RCH', shd%yCount, shd%xCount
                stop
            end if
            RCH = 0.0
        end if

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
            call write_r2c(SA_RTE_fls, SA_RTE_flkeys%RFF, shd, ic, &
                           0, 0, 0, 0, 0, &
                           RFF, &
!todo: replace source with LSS flag
                           'channel_inflow', 'mm', 'flow', 'CLASS', 'SA_MESH_DRIVER')
            if (ro%VERBOSEMODE > 0) print 1002, 'RFF', adjustl(trim(SA_RTE_fls%fl(SA_RTE_flkeys%RFF)%fn))
        end if

        !> For: Recharge (MODELFLG = r).
        if (SA_RTE_flgs%PRINTRCHR2CFILEFLAG == 1) then
            call write_r2c(SA_RTE_fls, SA_RTE_flkeys%RCH, shd, ic, &
                           0, 0, 0, 0, 0, &
                           RCH, &
!todo: replace source with LSS flag
                           'recharge', 'mm', 'flow', 'CLASS', 'SA_MESH_DRIVER')
            if (ro%VERBOSEMODE > 0) print 1002, 'RCH', adjustl(trim(SA_RTE_fls%fl(SA_RTE_flkeys%RCH)%fn))
        end if

        if (ro%VERBOSEMODE > 0) print 1000

1000    format(/1x, '*****************************************************************', /)
1001    format(1x, 'Standalone Routing is active.')
1002    format(3x, 'Writing output for ', (a), ' to: ', (a))

1004    format(/1x, "ERROR: Allocating: '", (a), "'", &
               /3x, 'Using:', &
               /5x, 'yCount=', i10, &
               /5x, 'xCount=', i10, /)

    end subroutine

end module
