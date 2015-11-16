module process_WF_ROUTE_config

    use model_files_variabletypes, only: fl_ids

    implicit none

    !> *****************************************************************
    !> Object variables.
    !> *****************************************************************

    !>
    !> Description: Two types define objects used throughout the
    !>              module. WF_RTE_flags contains flags to enable and
    !>              disable the module and control its output.
    !>              WF_RTE_file_keys contains indices to objects
    !>              containing file information. WR_RTE_fls uses the
    !>              fls_id type from module_files_variabletypes to
    !>              store this information.
    !>

    type WF_RTE_flags

        !> Flag used to enable the module.
        logical :: PROCESS_ACTIVE = .true.

        integer :: STREAMFLOWFLAG = 1
        integer :: STREAMFLOWOUTFLAG = 2

    end type

    !> WF_RTE_flgs: Configuration flags for the module.
    type(WF_RTE_flags), save :: WF_RTE_flgs

    type WF_RTE_file_keys

        !> Input files.
        !* stfl_in: MESH_input_streamflow.txt
        !* resv_in: MESH_input_reservoir.txt
        integer :: stfl_in = 1
        integer :: resv_in = 2

        !> Output files.
        !* stfl_daily: MESH_output_streamflow.csv
        !* stfl_cumm: MESH_output_streamflow_cumulative.csv
        !* stfl_ts: MESH_output_streamflow_all.csv
        integer :: stfl_daily = 3
        integer :: stfl_cumm = 4
        integer :: stfl_ts = 5

    end type

    !> WF_RTE_fls: Stores information about files used by the module.
    type(fl_ids), save :: WF_RTE_fls

    type(WF_RTE_file_keys), save :: WF_RTE_flks

    !> *****************************************************************
    !> Local variables.
    !> *****************************************************************

    contains

    !> *****************************************************************
    !> Subroutines.
    !> *****************************************************************

    !>
    !> Description: Allocate the object containing file information.
    !>
    subroutine configure_WF_ROUTE_fls()

        !> Allocate file object.
        allocate(WF_RTE_fls%fl(5))

    end subroutine

    !>
    !> Description: Check for the existence of input files, open them,
    !>              print diagnostic information, skip records, and open
    !>              the output files, in preparation for running the
    !>              WF_ROUTE process.
    !>
    subroutine config_WF_ROUTE(shd, ic)

        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_dates

        type(ShedGridParams), intent(in) :: shd
        type(iter_counter), intent(in) :: ic

        if (.not. WF_RTE_flgs%PROCESS_ACTIVE) return

    end subroutine

end module
