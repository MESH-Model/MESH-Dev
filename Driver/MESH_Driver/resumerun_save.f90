!> Description:
!>  Subroutine to save the run state to file.
subroutine resumerun_save(fls, shd, cm)

    use mpi_module
    use model_files_variables
    use sa_mesh_common
    use climate_forcing
    use model_dates

    !> Process modules.
    use RUNCLASS36_constants
    use RUNCLASS36_variables
    use RUNCLASS36_config
    use RUNSVS113_variables
    use WF_ROUTE_config
    use area_watflood, only: fhr
    use rte_module
    use baseflow_module
    use save_basin_output
    use SIMSTATS

    implicit none

    !> Input variables.
    type(fl_ids) fls
    type(ShedGridParams) shd
    type(clim_info) cm

    !> Local variables.
    integer iun, j, z
    character(len = DEFAULT_LINE_LENGTH) args(100), line, fname
    logical now

    !> Return if not the head node.
    if (.not. ISHEADNODE .or. vs%flgs%save%state == FLAG_OFF) return

    !> Check if now is the time for the I/O operation.
    now = .false.
    select case (vs%flgs%save%freq)

        !> User-specified frequency.
        case (FREQ_YLY)
            now = (ic%now%year /= ic%next%year)
        case (FREQ_MLY)
            now = (ic%now%month /= ic%next%month)

        !> End of run/force.
        case (FREQ_NOW)
            now = .true.
    end select

    !> Return if now is not the time for the I/O operation.
    if (.not. now) return

    !> Save files.

    !> txt: In text format.
    if (btest(vs%flgs%save%flo%ffmt, FFMT_TXT)) then
        fname = 'MESH_variables.txt'
!+        call reset_tab()
        call print_message('SAVING: ' // trim(fname))
!+        call increase_tab()
        iun = 100
        open(iun, file = fname, action = 'write', iostat = z)
        if (z /= 0) then
            call print_error('Unable to open the file. States cannot be saved.')
            call program_abort()
        end if
        write(iun, '(a)') '!< BASINCELLS'
        write(iun, FMT_GEN) 'NA', shd%NA
        if (shd%NA > 0) then
            write(iun, FMT_GEN) 'CELLAREA', (shd%AREA(j), j = 1, shd%NA)
            write(iun, FMT_GEN) 'YLAT', (shd%ylat(j), j = 1, shd%NA)
            write(iun, FMT_GEN) 'XLON', (shd%xlng(j), j = 1, shd%NA)
        end if
        write(iun, '(a)') '!< LANDUNITS'
        write(iun, FMT_GEN) 'NTYPE', shd%lc%NTYPE
        if (shd%lc%NTYPE > 0) then
            write(iun, FMT_GEN) 'NML', shd%lc%NML
            write(iun, FMT_GEN) 'ILG', shd%lc%ILG
            write(iun, FMT_GEN) 'NLMOS', (shd%lc%ILMOS(j), j = 1, shd%lc%ILG)
            write(iun, FMT_GEN) 'MLMOS', (shd%lc%JLMOS(j), j = 1, shd%lc%ILG)
        end if
        write(iun, '(a)') '!< WATERUNITS'
        write(iun, FMT_GEN) 'NWAT', shd%wc%NTYPE
        if (shd%wc%NTYPE > 0) then
            write(iun, FMT_GEN) 'NMW', shd%wc%NML
            write(iun, FMT_GEN) 'IWG', shd%wc%ILG
            write(iun, FMT_GEN) 'NWMOS', (shd%wc%ILMOS(j), j = 1, shd%wc%ILG)
            write(iun, FMT_GEN) 'MWMOS', (shd%wc%JLMOS(j), j = 1, shd%wc%ILG)
        end if
        write(iun, '(a)') '!< SOILPROFILE'
        write(iun, FMT_GEN) 'NSL', shd%lc%IGND
        if (shd%lc%IGND > 0) then
            write(iun, FMT_GEN) 'ZSOIL', (shd%lc%sl%DELZ(j), j = 1, shd%lc%IGND)
            write(iun, FMT_GEN) 'ZBOT', (shd%lc%sl%ZBOT(j), j = 1, shd%lc%IGND)
        end if
        write(iun, '(a)') '!< SOILTEXTURE'
        write(iun, '(a)') '!< SOILHYDRO'
        write(iun, '(a)') '!< CHANNELMAP'
        write(iun, FMT_GEN) 'NAA', shd%NAA
        if (shd%NAA > 0) then
            write(iun, FMT_GEN) 'NEXT', (shd%NEXT(j), j = 1, shd%NA)
            write(iun, FMT_GEN) 'DA', (shd%DA(j), j = 1, shd%NA)
            write(iun, FMT_GEN) 'CHNLLENGTH', (shd%CHNL_LEN(j), j = 1, shd%NA)
            write(iun, FMT_GEN) 'CHNLSLOPE', (shd%SLOPE_CHNL(j), j = 1, shd%NA)
            write(iun, FMT_GEN) 'IBSN', (shd%IAK(j), j = 1, shd%NA)
        end if
        write(iun, '(a)') '!< CANOPY'
        write(iun, FMT_GEN) 'NCNPY'
        write(iun, FMT_GEN) 'NCNPY1'
        write(iun, '(a)') '!< WF_ROUTE'
        write(iun, FMT_GEN) 'WF_STATUS', WF_RTE_flgs%PROCESS_ACTIVE
        if (WF_RTE_flgs%PROCESS_ACTIVE) then
            write(iun, FMT_GEN) 'WF_JAN1', JAN
            write(iun, FMT_GEN) 'WF_ITIME', WF_TIMECOUNT
        end if
        write(iun, '(a)') '!< RTE'
        write(iun, FMT_GEN) 'RTE_STATUS', rteflg%PROCESS_ACTIVE
        if (rteflg%PROCESS_ACTIVE) then
            write(iun, FMT_GEN) 'RTE_FHR', fhr
        end if
        if (bflm%BASEFLOWFLAG == 1) then
            write(iun, '(a)') '!< LZS_LUO2012'
            write(iun, FMT_GEN) 'BASEFLOWFLAG', 'luo_2012'
            write(iun, FMT_GEN) 'LZSL12_QB', Qb
        end if
        if (bflm%BASEFLOWFLAG == 2) then
            write(iun, '(a)') '!< LZS_WF'
            write(iun, FMT_GEN) 'BASEFLOWFLAG', 'wf_lzs'
        end if
        if (mtsflg%AUTOCALIBRATIONFLAG == 1) then
            write(iun, '(a)') '!< SIMSTATS'
            write(iun, FMT_GEN) 'SIMS_NCAL', ncal
            write(iun, FMT_GEN) 'SIMS_NS', ns
            if (mtsflg%PREEMPTIONFLAG == 1) then
                write(iun, FMT_GEN) 'SIMS_FBEST', fbest
                write(iun, FMT_GEN) 'SIMS_FTEST', ftest
            end if
            do j = 1, ns
                write(iun, FMT_GEN) 'SIMS_QOBS', j, qobs(1:ncal, j)
                write(iun, FMT_GEN) 'SIMS_QSIM', j, qsim(1:ncal, j)
            end do
        end if
        write(iun, '(a)') '!< MESHDRIVER'
        write(iun, FMT_GEN) 'NOWYEAR', ic%now%year
        write(iun, FMT_GEN) 'NOWJDAY', ic%now%jday
        write(iun, FMT_GEN) 'NOWMONTH', ic%now%month
        write(iun, FMT_GEN) 'NOWCDAY', ic%now%day
        write(iun, FMT_GEN) 'NOWHOUR', ic%now%hour
        write(iun, FMT_GEN) 'NOWMINS', ic%now%mins
        write(iun, FMT_GEN) 'IYEAR', ic%iter%year
        write(iun, FMT_GEN) 'IJDAY', ic%iter%jday
        write(iun, FMT_GEN) 'IMONTH', ic%iter%month
        write(iun, FMT_GEN) 'ICDAY', ic%iter%day
        write(iun, FMT_GEN) 'IHOUR', ic%iter%hour
        write(iun, FMT_GEN) 'IMINS', ic%iter%mins
        write(iun, FMT_GEN) 'TSD', ic%ts_daily
        write(iun, FMT_GEN) 'TSHLY', ic%ts_hourly
        write(iun, FMT_GEN) 'TSHFHLY', ic%ts_halfhourly
        write(iun, FMT_GEN) 'TSTOT', ic%ts_count
!+        write(iun, FMT_GEN) 'PRECACC', TOTAL_PRE
!+        write(iun, FMT_GEN) 'EVPTACC', TOTAL_EVAP
!+        write(iun, FMT_GEN) 'ROFTACC', TOTAL_ROF
!+        write(iun, FMT_GEN) 'ROFOACC', TOTAL_ROFO
!+        write(iun, FMT_GEN) 'ROFSACC', TOTAL_ROFS
!+        write(iun, FMT_GEN) 'ROFBACC', TOTAL_ROFB
!+        write(iun, FMT_GEN) 'STGINIT', STG_INI
        write(iun, FMT_GEN) 'QOMEASD', fms%stmg%qomeas%val
        write(iun, FMT_GEN) 'QOSIMD', out%d%grid%qo(fms%stmg%meta%rnk(:))
        if (ro%RUNTILE) then
            write(iun, '(a)') '!< MESHTILE'
            write(iun, FMT_GEN) 'CMAS', 'GAT', stas%cnpy%cmas
            write(iun, FMT_GEN) 'GRO', 'GAT', stas%cnpy%gro
            write(iun, FMT_GEN) 'QAC', 'GAT', stas%cnpy%qac
            write(iun, FMT_GEN) 'RCAN', 'GAT', stas%cnpy%rcan
            write(iun, FMT_GEN) 'SNCAN', 'GAT', stas%cnpy%sncan
            write(iun, FMT_GEN) 'SNO', 'GAT', stas%sno%sno
            write(iun, FMT_GEN) 'ALBS', 'GAT', stas%sno%albs
            write(iun, FMT_GEN) 'RHOS', 'GAT', stas%sno%rhos
            write(iun, FMT_GEN) 'WSNO', 'GAT', stas%sno%wsno
            write(iun, FMT_GEN) 'TAC', 'GAT', stas%cnpy%tac
            write(iun, FMT_GEN) 'TBAR', 'GAT', stas%sl%tbar
            write(iun, FMT_GEN) 'TBAS', 'GAT', stas%sl%tbas
            write(iun, FMT_GEN) 'TCAN', 'GAT', stas%cnpy%tcan
            write(iun, FMT_GEN) 'THIC', 'GAT', stas%sl%thic
            write(iun, FMT_GEN) 'THLQ', 'GAT', stas%sl%thlq
            write(iun, FMT_GEN) 'TPND', 'GAT', stas%sfc%tpnd
            write(iun, FMT_GEN) 'TSFS', 'GAT', stas%sfc%tsfs
            write(iun, FMT_GEN) 'TSNO', 'GAT', stas%sno%tsno
            write(iun, FMT_GEN) 'ZPND', 'GAT', stas%sfc%zpnd
            write(iun, FMT_GEN) 'LZS', 'GAT', stas%lzs%ws
            write(iun, FMT_GEN) 'DZS', 'GAT', stas%dzs%ws
        end if
        if (ro%RUNGRID) then
            write(iun, '(a)') '!< MESHCELL'
            write(iun, FMT_GEN) 'QI', 'GRD', stas_grid%chnl%qi
            write(iun, FMT_GEN) 'STGCH', 'GRD', stas_grid%chnl%stg
            write(iun, FMT_GEN) 'QO', 'GRD', stas_grid%chnl%qo
        end if
        close(iun)
    end if

    !> seq: Sequential binary format.
    if (btest(vs%flgs%save%flo%ffmt, FFMT_SEQ)) then

        !> Append the date to the default resume filename if auto resume is enabled.
        if (vs%flgs%save%freq /= FREQ_NUL .and. vs%flgs%save%freq /= FREQ_NOW) then
            write(line, "(i4.4, '_', i3.3)") ic%next%year, ic%next%jday
            fname = fls%fl(mfk%f883)%fn
            fls%fl(mfk%f883)%fn = trim(fname(1:index(fname, '.'))) // trim(adjustl(line)) // trim(fname(index(fname, '.'):))
        end if

        !> Save files.
        if (index(vs%flgs%save%bin, '+STASONLY') == 0 .and. index(vs%flgs%save%bin, '+CLASSPROG') == 0) then
            call climate_module_resume_save(fls, shd, cm)
            call save_init_prog_variables_class(fls)
            call bflm_resume_save(fls, shd)
            call WF_ROUTE_resume_save(fls, shd)
            call run_rte_resume_save(fls, shd)
            call run_save_basin_output_resume_save(fls, shd)
            call stats_state_save(fls)
        else if (index(vs%flgs%save%bin, '+CLASSPROG') == 0) then
            call save_init_prog_variables_class(fls)
            call bflm_resume_save(fls, shd)
            call WF_ROUTE_resume_save(fls, shd)
            call run_rte_resume_save(fls, shd)
        else
            call save_init_prog_variables_class_row(fls, shd)
        end if
        if (vs%flgs%save%freq /= FREQ_NUL .and. vs%flgs%save%freq /= FREQ_NOW) then
            fls%fl(mfk%f883)%fn = fname
        end if
    end if

    !> r2c: From r2c by grid.

    !> csv: From CSV by GRU.

    !> Save the resume date ('next') to the auto resume file.
    if (vs%flgs%save%freq /= FREQ_NUL .and. vs%flgs%save%freq /= FREQ_NOW) then
        fname = 'auto_resume.ini'
!+        call reset_tab()
        call print_message('SAVING: ' // trim(fname))
!+        call increase_tab()
        iun = 100
        open(iun, file = fname, action = 'write', iostat = z)
        if (z /= 0) then
            call print_error('Unable to open the file. States cannot be saved.')
            call program_abort()
        end if
        write(iun, "(4i4, ' !< START_YEAR, START_JDAY, START_HOUR, START_MINS')") &
            ic%next%year, ic%next%jday, ic%next%hour, ic%next%mins
        write(iun, "(8x, 2i4, ' !< START_MONTH, START_DAY')") ic%next%month, ic%next%day
        close(iun)
    end if

end subroutine
