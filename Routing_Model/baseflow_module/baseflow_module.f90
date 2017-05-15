module baseflow_module

    implicit none

    !>
    !> BASEFLOW module external parameter type
    !>
    !> Run options:
    !*  BASEFLOWFLAG: Flag that specifies baseflow routine.
    !>
    !> Initialization (states):
    !*  WrchrgIni: Initial constant recharge for cold start (mm/hr).
    !*  QbIni: Initial constant baseflow for cold start (mm/hr).
    !>
    !> Parameters (Hydrology):
    !> Routine 1: BASEFLOWFLAG 1
    !*  dgwsh: Delay time of the overlying soil layers in the  aquifer (hour).
    !*  agwsh: Recession constant of the aquifer.
    !>
    !> Routine 2: BASEFLOWFLAG 2
    !*  WF_LZFA: lower zone function (mm s-1).
    !*  WF_LZFPWR: exponent on the lower zone storage in the lower zone funnction.
    !>
    type BaseflowParameters
        integer :: BASEFLOWFLAG = 0
        real WrchrgIni, QbIni
        real, dimension(:, :), allocatable :: dgwsh, agwsh
        real, dimension(:, :), allocatable :: WF_LZFA, WF_LZFPWR
    end type

    !>
    !> BASEFLOW internal variables
    !>

    real, dimension(:), allocatable :: Wseep, Wrchrg, Qb

    !> BASEFLOWFLAG (1)
    real, dimension(:), allocatable :: dgw, agw
    real Wrchrg_new, Qb_new

    !> BASEFLOWFLAG (2)
    real, dimension(:), allocatable :: WF_LZFA, WF_LZFPWR

    !* Instance of the 'BaseflowParameters' type (global).
    type(BaseflowParameters), save :: lzsp

    contains

    subroutine LZS_init(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        use mpi_shared_variables
        use sa_mesh_shared_variables
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        use FLAGS

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(dates_model) :: ts
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sp
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        integer NA, NTYPE, NML, k, m, j, i, ierr
        integer :: iun = 58
        character(len=30) :: NMTESTFORMAT

        !> Return if BASEFLOWFLAG is not active
        if (lzsp%BASEFLOWFLAG == 0) return

        NA = shd%NA
        NTYPE = shd%lc%NTYPE
        NML = shd%lc%NML

        !> Summarize current BASEFLOWFLAG configuration to file.
        if (ipid == 0 .and. MODELINFOOUTFLAG > 0) then
            write(iun, "('BASEFLOW component ACTIVATED')")
            write(iun, *)
            select case (lzsp%BASEFLOWFLAG)
                case (1)
                    write(iun, "(a22, f8.3)") 'WRCHRG_INI', lzsp%WrchrgIni
                    write(iun, "(a22, f8.3)") 'QB_INI', lzsp%QbIni
                    write(iun, *)
                    write(NMTESTFORMAT, "(a17, i3, 'f10.2)')") "('DGWSH'", NTYPE
                    write(iun, NMTESTFORMAT) (lzsp%dgwsh(1, m), m = 1, NTYPE)
                    write(NMTESTFORMAT, "(a17, i3, 'f10.2)')") "('AGWSH'", NTYPE
                    write(iun, NMTESTFORMAT) (lzsp%agwsh(1, m), m = 1, NTYPE)
                case (2)
                    write(NMTESTFORMAT, "(a17, i3, 'f10.2)')") "('WF_LZFPWR'", NTYPE
                    write(iun, NMTESTFORMAT) (lzsp%WF_LZFPWR(1, m), m = 1, NTYPE)
                    write(NMTESTFORMAT, "(a17, i3, 'f10.2)')") "('WF_LZFA'", NTYPE
                    write(iun, NMTESTFORMAT) (lzsp%WF_LZFA(1, m), m = 1, NTYPE)
                case default
                    write(iun, "('WARNING: Configuration not supported.')")
                    write(iun, "(a26, i4)") 'BASEFLOWFLAG', lzsp%BASEFLOWFLAG
            end select
            write(iun, *)
        end if

        !> Summarize current BASEFLOWFLAG configuration to screen.
        if (ro%VERBOSEMODE > 0) print '(/1x, (a), /)', 'BASEFLOW component ACTIVATED'

        !> Initialize and distribute BASEFLOWFLAG initial values and parameterization.
        select case (lzsp%BASEFLOWFLAG)
            case (1)
                allocate(dgw(NML), agw(NML), Wseep(NML), Wrchrg(NML), Qb(NML))
                Wseep = 0.0
                Wrchrg = lzsp%WrchrgIni
                Qb = lzsp%QbIni
                do k = 1, NML
                    dgw(k) = lzsp%dgwsh(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
                    agw(k) = lzsp%agwsh(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
                end do
            case (2)
                allocate(WF_LZFPWR(NML), WF_LZFA(NML), Wseep(NML), Wrchrg(NML), Qb(NML))
                Wseep = 0.0
                Wrchrg = lzsp%WrchrgIni
                Qb = lzsp%QbIni
                do k = 1, NML
                    WF_LZFPWR(k) = lzsp%WF_LZFPWR(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
                    WF_LZFA(k) = lzsp%WF_LZFA(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
                end do
            case default
                print *, ' WARNING: BASEFLOWFLAG ', lzsp%BASEFLOWFLAG, ' not configured.'
                stop
        end select

        if (RESUMEFLAG == 4 .or. RESUMEFLAG == 5) then
            select case (lzsp%BASEFLOWFLAG)
                case (1)
                    iun = fls%fl(mfk%f883)%iun
                    open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.lzsp.luo_2012', status = 'old', action = 'read', &
                        form = 'unformatted', access = 'sequential', iostat = ierr)
                    read(iun) Wrchrg
                    read(iun) Qb
                    close(iun)
                case (2)
                    iun = fls%fl(mfk%f883)%iun
                    open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.lzsp.wfqlz', status = 'old', action = 'read', &
                        form = 'unformatted', access = 'sequential', iostat = ierr)
                    read(iun) Wrchrg
                    close(iun)
            end select
        end if

    end subroutine

    subroutine LZS_within_tile(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        use mpi_shared_variables
        use sa_mesh_shared_variables
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(dates_model) :: ts
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sp
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        integer NA, NTYPE, NML, k, m, j, i, iun, ierr

        !> Return if BASEFLOWFLAG is not active
!+        if (lzsp%BASEFLOWFLAG == 0) return
        return

        NA = shd%NA
        NTYPE = shd%lc%NTYPE
        NML = shd%lc%NML

!todo+++: Perhaps land-unit indexing can be done prior in the sequence
!todo+++: of initialization, after reading the drainage database.
!todo+++: Then, variables could be allocated (il1:il2) instead of
!todo+++: (1:ILG) to reduce the memory footprint of the model per node.

        !> Calculate Indices.
!+        call mpi_split_nml(inp, izero, ipid, NML, shd%lc%ILMOS, il1, il2, ilen)

        !> Calculate contribution of baseflow to lower zone storage and redistribute runoff.
!+        select case (lzsp%BASEFLOWFLAG)
!+            case (1)
!+                ROFGAT = ROFGAT - ROFBGAT
!+                Wseep = ROFBGAT*3600.0
!+                do k = il1, il2
!+                    call baseFlow(Wseep(k), dgw(k), Wrchrg(k), agw(k), Qb(k), 1.0, Wrchrg_new, Qb_new)
!+                    ROFBGAT(k) = Qb_new/3600.0
!+                    Qb(k) = Qb_new
!+                    Wrchrg(k) = Wrchrg_new
!+                end do
!+                ROFGAT = ROFGAT + ROFBGAT
!+                WTRGGAT = WTRGGAT - (Wseep/3600.0 - ROFBGAT)
!+            case (2)
!+                ROFGAT = ROFGAT - ROFBGAT
!+                Wseep = ROFBGAT
!+                Wrchrg = Wrchrg + ROFBGAT
!+                call baseflow_wfqlz(WF_LZFA, WF_LZFPWR, Wrchrg, ROFBGAT, NML, il1, il2)
!+                ROFGAT = ROFGAT + ROFBGAT
!+                WTRGGAT = WTRGGAT - (Wseep - ROFBGAT)
!+        end select

    end subroutine

    subroutine LZS_finalize(fls, shd)

        use mpi_shared_variables
        use model_files_variables
        use sa_mesh_shared_variables
        use model_dates

        !> For: SAVERESUMEFLAG
        use FLAGS

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd

        !> Local variables.
        integer ierr, iun

        !> Return if not the head node.
        if (ipid /= 0) return

        !> Return if BASEFLOWFLAG is not active
        if (lzsp%BASEFLOWFLAG == 0) return

        if (SAVERESUMEFLAG == 4 .or. SAVERESUMEFLAG == 5) then
            select case (lzsp%BASEFLOWFLAG)
                case (1)
                    iun = fls%fl(mfk%f883)%iun
                    open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.lzsp.luo_2012', status = 'replace', &
                        action = 'write', form = 'unformatted', access = 'sequential', iostat = ierr)
                    write(iun) Wrchrg
                    write(iun) Qb
                    close(iun)
                case (2)
                    iun = fls%fl(mfk%f883)%iun
                    open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.lzsp.wfqlz', status = 'replace', &
                        action = 'write', form = 'unformatted', access = 'sequential', iostat = ierr)
                    write(iun) Wrchrg
                    close(iun)
            end select
        end if

    end subroutine

end module
