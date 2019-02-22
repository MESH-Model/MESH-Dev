module runsvs_mesh

    use mpi_module
    use model_files_variables
    use sa_mesh_common
    use climate_forcing
    use model_dates

    implicit none

!    integer, parameter :: fid_ini = 50
!    integer, parameter :: fid_met = 51
!    integer, parameter :: fid_out = 52

!    integer nt      ! Number of time steps
!    integer dateo   ! Starting date of the run
!    integer houro   ! Starting hour of the run
!    real :: dt = 1800.0        ! Time step duration (sec)
!    real :: sigma_u = 0.995    ! Sigma level of momentum forcing
!    real :: sigma_t = 0.995    ! Sigma level of scalar forcings
!    integer xcount  ! Number of columns in the grid
!    integer ycount  ! Number of lines in the grid
!    logical :: observed_forcing = .false.
!    character(len = 255) :: inifile = ''
!    character(len = 255) :: interpfile = ''
!    character(len = 255) :: geofile = ''
!    character(len = 255) :: metfile = ''
!    character(len = 255) :: outfile = ''
!    character(len = 255) :: rtefile = ''

!    integer, parameter :: bussiz = runsvs_busdim
!    real bus(bussiz)
    integer bussiz
    real, dimension(:), allocatable :: bus
    integer datecmc_o, date_f, hour_f
!    integer datecmc_v, date_v, hour_v, istat, bidon
!    integer kount
!    real(kind = 8) kdt

!>>>svs_output
!    integer :: iout_dly = 151, iout_hly = 152, iout_ts = 153 !150+N is output unit number for CLASS output; should be able to recycle its use
!    integer :: iout_wat_bal = 157
!    real preacc_dly, preacc_hly, preacc_tot, runoff_acc, wsoil_tot, isoil_tot
!    real bal_in_out, stock, bal_tot, bal_pre, wsoil_ini
!<<<svs_output

    type runsvs_mesh_variables
        real, dimension(:, :), allocatable :: vf
        real, dimension(:), allocatable :: lnz0
    end type

    !* PROCESS_ACTIVE: Variable to enable SVS.
    type runsvs_mesh_container
        logical :: PROCESS_ACTIVE = .false.
        type(runsvs_mesh_variables) vs
    end type

    type(runsvs_mesh_container), save, public :: svs_mesh

    private

    public runsvs_mesh_init, runsvs_mesh_within_tile

    contains

    subroutine runsvs_mesh_init(shd, fls, cm)

        use runsvs_mod
        use runsvs_utils
        use phy_options
        use svs_configs
        use sfc_options
!        use runsvs_io

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

!#include "options.cdk"
#include "isbapar.cdk"
!#include "surfcon.cdk"
#include "thermoconsts.inc"

        integer k, ki, kj, j, jj, m
        real sumfcanz0
        character(len = DEFAULT_LINE_LENGTH) line

        integer, external :: newdate
!        external incdatr
        external svs, inicover_svs
!        external inisoili_svs, phyopt_initdata, runsvs_init

        !> Return if the process is not marked active or if not the head node.
        if (.not. svs_mesh%PROCESS_ACTIVE) return

        !> Initialize common blocks, read options and configuration file.
        sigma_u = 0.995
        sigma_t = 0.995
        observed_forcing = .false.

        nl_svs = shd%lc%IGND
        allocate(dl_svs(nl_svs))
        dl_svs = shd%lc%sl%zbot
        call svs_bus_init(il2 - il1 + 1)
        bussiz = runsvs_busdim
        allocate(bus(bussiz))
        bus = 0.0

        !> Parse CLASS variables to bus.
        do k = 0, NG - 1

            !> Basic configuration.
            ki = shd%lc%ILMOS(il1 + k)
            kj = shd%lc%JLMOS(il1 + k)

            !> Convert lat, lon to radian.
!            bus(dlat + k) = ((shd%yOrigin + shd%yDelta*shd%yyy(ki)) - shd%yDelta/2.0)*PI/180.0
!            bus(dlon + k) = ((shd%xOrigin + shd%xDelta*shd%xxx(ki)) - shd%xDelta/2.0)*PI/180.0
            bus(dlat + k) = shd%ylat(ki)*PI/180.0
            bus(dlon + k) = shd%xlng(ki)*PI/180.0

            !> Map CLASS parameters to SVS parameters.
            !* zusl: Height of wind forcing.
            !* ztsl: Height of temperature forcing.
            if (observed_forcing) then
                bus(zusl + k) = pm%sfp%zrfm(il1 + k)
                bus(ztsl + k) = pm%sfp%zrfh(il1 + k)
            end if

            !> Parameters.
            !* vegf+   3*NG: Needleleaf evergreen.
            !* vegf+   6*NG: Broadleaf deciduous.
            !* vegf+  14*NG: Crops.
            !* vegf+  13*NG: Grass.
            !* vegf+  20*NG: Urban.
            !* slop: Subgrid-scale slope.
            !* draindens: Drainage density (km/km2 converted to m/m2 but provided already by CLASS in m/m2).
            !* rootdp: Max depth of root zone.
            if (allocated(svs_mesh%vs%vf)) then
                do m = 1199, 1174, -1
                    bus(vegf + (1199 - m)*NG + k) = svs_mesh%vs%vf(il1 + k, 1200 - m)
                end do
            else
                bus(vegf + 3*NG + k) = pm%cp%fcan(il1 + k, 1)
                bus(vegf + 6*NG + k) = pm%cp%fcan(il1 + k, 2)
                bus(vegf + 14*NG + k) = pm%cp%fcan(il1 + k, 3)
                bus(vegf + 13*NG + k) = pm%cp%fcan(il1 + k, 4)
                bus(vegf + 20*NG + k) = pm%cp%fcan(il1 + k, 5)
            end if
            bus(slop + k) = pm%tp%xslp(il1 + k)
            bus(draindens + k) = pm%hp%dd(il1 + k)!*0.001
            bus(rootdp + k) = pm%slp%sdep(il1 + k)

            !> Compute weighted average of log z0 wrt vegetation
            !> (used for momentum only - local z0 used for temperature/humidity).
            if (allocated(svs_mesh%vs%lnz0)) then
                bus(z0 + k) = svs_mesh%vs%lnz0(il1 + k)
            else
                bus(z0 + k) = 0.0
                sumfcanz0 = 0.0
                do j = 1, 5
                    bus(z0 + k) = bus(z0 + k) + pm%cp%fcan(il1 + k, j)*pm%cp%lnz0(il1 + k, j)
                    sumfcanz0 = sumfcanz0 + pm%cp%fcan(il1 + k, j)
                end do
                if (sumfcanz0 > 0.0) then
                    bus(z0 + k) = bus(z0 + k)/sumfcanz0
                end if
            end if
            bus(z0 + k) = exp(bus(z0 + k))

            !> For soil texture we ignore negative numbers
            !> which signal special soils (organic/impermeable/glaciers).
            !> Map soil texture.
            !> IGND == 3 (CLASS traditional)
            !>       1              1-2
            !>       2               3
            !>       3              4-7
            !> IGND >= 5
            !> CLASS layer  <->  SVS layer
            !>       1              1-2
            !>       2              3-4
            !>       3              5
            !>       4              6
            !>       5              7
!            bus(sand + k) = max(pm%slp%sand(il1 + k, 1), 0.0)
!            bus(sand + NG + k) = max(pm%slp%sand(il1 + k, 1), 0.0)
!            bus(sand + 2*NG + k) = max(pm%slp%sand(il1 + k, 2), 0.0)
!            bus(clay + k) = max(pm%slp%clay(il1 + k, 1), 0.0)
!            bus(clay + NG + k) = max(pm%slp%clay(il1 + k, 1), 0.0)
!            bus(clay + 2*NG + k) = max(pm%slp%clay(il1 + k, 2), 0.0)
!            if (shd%lc%IGND >= 5) then
!                bus(sand + 3*NG + k) = max(pm%slp%sand(il1 + k, 2), 0.0)
!                bus(sand + 4*NG + k) = max(pm%slp%sand(il1 + k, 3), 0.0)
!                bus(sand + 5*NG + k) = max(pm%slp%sand(il1 + k, 4), 0.0)
!                bus(sand + 6*NG + k) = max(pm%slp%sand(il1 + k, 5), 0.0)
!                bus(clay + 3*NG + k) = max(pm%slp%clay(il1 + k, 2), 0.0)
!                bus(clay + 4*NG + k) = max(pm%slp%clay(il1 + k, 3), 0.0)
!                bus(clay + 5*NG + k) = max(pm%slp%clay(il1 + k, 4), 0.0)
!                bus(clay + 6*NG + k) = max(pm%slp%clay(il1 + k, 5), 0.0)
!            else
!                do j = 3, 6
!                    bus(sand + j*NG + k) = max(pm%slp%sand(il1 + k, 3), 0.0)
!                    bus(clay + j*NG + k) = max(pm%slp%clay(il1 + k, 3), 0.0)
!                end do
!            end if
            do j = 1, nl_svs ! model layers
                bus(sand + (j - 1)*NG + k) = max(pm%slp%sand(il1 + k, j), 0.0)
                bus(clay + (j - 1)*NG + k) = max(pm%slp%clay(il1 + k, j), 0.0)
            end do

            !> Map soil moisture.
            !> CLASS layer  <->  SVS layer
            !>       1              1-2
            !>       2               3
            !>       3              4-7
!            bus(wsoil + k) = vs%tile%thlq(il1 + k, 1)
!            bus(wsoil + NG + k) = vs%tile%thlq(il1 + k, 2)
!            bus(wsoil + 2*NG + k) = vs%tile%thlq(il1 + k, 3)
!            do j = 3, 6
!                bus(wsoil + j*NG + k) = vs%tile%thlq(il1 + k, 3)
!            end do
            do j = 1, KHYD ! permeable layers, min from runsvs_init
                bus(wsoil + (j - 1)*NG + k) = max(vs%tile%thlq(il1 + k, j), bus(wfc + k))
            end do

            !> Map soil temperature.
            !> CLASS layer  <->  SVS layer
            !>       1               1
            !>       2               2
            bus(tsoil + k) = vs%tile%tbar(il1 + k, 1)! + tcdk
            bus(tsoil + NG + k) = vs%tile%tbar(il1 + k, 2)! + tcdk
            bus(tground + k) = vs%tile%tbar(il1 + k, 1)! + tcdk
            bus(tground + NG + k) = vs%tile%tbar(il1 + k, 2)! + tcdk
!todo
!               bus(tground + (j - 1)*NG + k) = bus(tmoins + k)
!               bus(tsoil + (j - 1)*NG + k) = bus(tmoins + k)
!               bus(tvege + (j - 1)*NG + ik = bus(tmoins + k)
!               bus(tsnowveg + (j - 1)*NG + k) = bus(tmoins + k)

            !> Map vegetation temperature.
            do j = 0, 1
                bus(tvege + j*NG + k) = vs%tile%tcan(il1 + k)! + tcdk
                bus(tsnowveg + j*NG + k) = vs%tile%tsno(il1 + k)! + tcdk
            end do

            !> Map snow properties.
            !* snoro: Density (kg/m3) to relative density wrt ice.
            do j = 0, 1
                bus(tsnow + j*NG + k) = vs%tile%tsno(il1 + k)! + tcdk
            end do
!todo
!               bus(tsnow + (j - 1)*NG + k) = tcdk
            if (vs%tile%rhos(il1 + k) > 0.0) then
                bus(snoro + k) = vs%tile%rhos(il1 + k)/900.0
                bus(snvro + k) = vs%tile%rhos(il1 + k)/900.0
            else !from runsvs_init
                bus(snoro + k) = 0.1
                bus(snvro + k) = 0.1
            end if
            bus(snoal + k) = vs%tile%albs(il1 + k)
            bus(snval + k) = vs%tile%albs(il1 + k)
        end do

        !> Summary.
        if (DIAGNOSEMODE) then
            call reset_tab()
            call print_message('')
            call print_message('--------------------------------')
            call print_message('SVS DIAGNOSTICS')
            call print_message('--------------------------------')
            write(line, "('TILE:             ', i3)") 1
            call print_message(line)
            call print_message('--------------------------------')
            write(line, "('LATITUDE:         ', f10.1)") bus(dlat)*180.0/PI
            call print_message(line)
            write(line, "('LONGITUDE:        ', f10.1)") bus(dlon)*180.0/PI
            call print_message(line)
            call print_message('VEGETATION COVER:')
            do m = 1199, 1174, -1
                write(line, "('% ', i5, '        ', f8.3)") m, bus(vegf + (1199 - m)*NG)*100.0
                call print_message(line)
            end do
            write(line, "('ROUGHNESS LENGTH: ', f8.3)") bus(z0)
            call print_message(line)
            write(line, "('SLOPE:            ', f8.3)") bus(slop)
            call print_message(line)
            write(line, "('DRAIN.DENSITY     ', f8.3)") bus(draindens)
            call print_message(line)
            write(line, "('ROOT DEPTH:       ', f8.3)") bus(rootdp)
            call print_message(line)
            call print_message('% SAND:')
            do j = 1, nl_svs ! model layers
                write(line, "(' LAYER ', i3, ': ', f8.3)") j, bus(sand + (j - 1)*NG)
                call print_message(line)
            end do
            call print_message('% CLAY:')
            do j = 1, nl_svs ! model layers
                write(line, "(' LAYER ', i3, ': ', f8.3)") j, bus(clay + (j - 1)*NG)
                call print_message(line)
            end do
            call print_message('--------------------------------')
            write(line, "('PERMEABLE LAYERS: ', i3)") KHYD
            call print_message(line)
            call print_message('SOIL MOISTURE:')
            do j = 1, KHYD ! permeable layers
                write(line, "(' LAYER ', i3, ': ', f8.3)") j, bus(wsoil + (j - 1)*NG)
                call print_message(line)
            end do
            write(line, "('SOIL TEMPERATURE: ', 2f8.3)") bus(tsoil), bus(tsoil + NG)
            call print_message(line)
            write(line, "('VEGETATION TEMP.: ', 2f8.3)") bus(tvege), bus(tvege + NG)
            call print_message(line)
            write(line, "('SNOW TEMPERATURE: ', 2f8.3)") bus(tsnow), bus(tsnow + NG)
            call print_message(line)
            write(line, "('SNOW DENSITY:     ', 2f8.3)") bus(snoro), bus(snvro)
            call print_message(line)
            write(line, "('SNOW ALBEDO:      ', 2f8.3)") bus(snoal), bus(snval)
            call print_message(line)
            call print_message('--------------------------------')
            call print_message('SOIL MAPPING')
            call print_message('--------------------------------')
            call print_message('DATABASE: ' // trim(soiltext))
            call print_message('WEIGHTS [METERS]:')
            do j = 1, nl_svs ! model layers
                write(line, "(' LAYER ', i3, ' DEPTH: ', f8.3)") j, dl_svs(j)
                call print_message(line)
                do jj = 1, nl_stp ! database layers
                    if (soiltext == 'GSDE') then
                        write(line, "('- ', (a), ' DEPTH: ', f8.3, ' WEIGHT: ', f8.3)") trim(soiltext), dl_gsde(jj), weights(j, jj)
                    else if (soiltext == 'SLC') then
                        write(line, "('- ', (a), ' DEPTH: ', f8.3, ' WEIGHT: ', f8.3)") trim(soiltext), dl_slc(jj), weights(j, jj)
                    else if (soiltext == 'SOILGRIDS') then
                        write(line, "('- ', (a), ' DEPTH: ', f8.3, ' WEIGHT: ', f8.3)") trim(soiltext), dl_soilgrids(jj), weights(j, jj)
                    end if
                    call print_message(line)
                end do
            end do
            call print_message('--------------------------------')
            call print_message('')
        end if

        !> Initialize surface parameters.
        call init_soil_text_levels()
        call inisoili_svs(bus, bussiz, NG)
!        call inisoili_svs(NG, 1)

        !> Initialize variables.
        call runsvs_init(bus, bussiz)

!>>>svs_output
!        if (ISHEADNODE) then

            !> Daily.
!            open(iout_dly, file = './' // trim(fls%GENDIR_OUT) // '/' // 'svs_out.csv', action = 'write')
!            write(iout_dly, 1010) 'YEAR', 'DAY', 'PRE', 'PRATE'
!            preacc_dly = 0.0 !reset accumulators

            !> Hourly.
!            open(iout_hly, file = './' // trim(fls%GENDIR_OUT) // '/' // 'svs1_temp_hourly.csv', action = 'write')
!            write(iout_hly, 1010) 'YEAR', 'DAY', 'HOUR', &
!                'SWE', 'SD', 'SNALB', 'TSN1', 'TSN2', 'TSNAVG', 'RAINRATE', 'SNOWRATE', 'WSN'
!            write(iout_hly, 1010) 'YEAR', 'DAY', 'HOUR', 'PRE'
!            preacc_hly = 0.0 !reset accumulators

!            open(iout_wat_bal, file = './' // trim(fls%GENDIR_OUT) // '/' // 'svs1_out_watbal_hourly.csv', action = 'write')
!            write(iout_wat_bal, 1010) 'YEAR', 'DAY', 'HOUR', &
!                'PCP_ACC', 'EVP_ACC', 'LATF_ACC', 'DRAI_ACC', 'RUNO_ACC', 'WSOIL_TOT', 'ISOIL_TOT', &
!                'SWE', 'SWE_VEG', 'WSN', 'WSN_VEG', 'WVEG', 'VEGH', 'VEGL'

!            open(iout_ts, file = './' // trim(fls%GENDIR_OUT) // '/' // 'svs_out_ts.csv', action = 'write')
!            write(iout_ts, 1010) 'YEAR', 'DAY', 'HOUR', 'MINS', 'RPCP', 'SPCP'
!        end if

!1010    format(9999(g15.7e2, ','))
!<<<svs_output

    end subroutine

    subroutine runsvs_mesh_within_tile(shd, fls, cm)

        use runsvs_mod
        use runsvs_utils
        use svs_configs
        use sfc_options
!        use runsvs_io

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

!#include "options.cdk"
#include "isbapar.cdk"
!#include "surfcon.cdk"
#include "thermoconsts.inc"

!        integer, parameter :: bussiz = runsvs_busdim
!        real bus(bussiz)
!        integer datecmc_o, date_f, hour_f
        integer datecmc_v, date_v, hour_v, istat, kount, bidon
        real(kind = 8) kdt

        integer k, ki, kj, j
        real FRAC

        integer, external :: newdate
        external incdatr
        external svs, inicover_svs
!        external inisoili_svs, phyopt_initdata, runsvs_init

        !> Return if the process is not active or if not the head node.
        if (.not. svs_mesh%PROCESS_ACTIVE .or. .not. (ipid /= 0 .or. izero == 0)) return

        !> Time-step.
        dt = real(ic%dts)
        kount = ic%ts_count - 1

        !> First time-step.
        if (kount == 0) then
            dateo = ic%now%year*10000 + ic%now%month*100 + ic%now%day
            houro = ic%now%hour*1000000 + ic%now%mins*10000
            istat = newdate(datecmc_o, dateo, houro, 3)
        end if

        !> Determine time stamps of current date.
        kdt = kount*(dt*1.0D0)/3600.0D0

        !> Compute date valid.
        call incdatr(datecmc_v, datecmc_o, kdt)

        !> Convert to old style.
        istat = newdate(datecmc_v, date, bidon, -4)

        !> Convert to printable.
        istat = newdate(datecmc_v, date_v, hour_v, -3)

        !> Loop tiles.
        do k = 0, NG - 1

!todo
!+ Option 1
!+ Rainfall and snowfall rate are read separetely
!+            bus(rainrate + k) = cm%dat(ck%RR)%GAT(il1 + k)/1000.0
!+            bus(snowrate + k) = cm%dat(ck%SR)%GAT(il1 + k)/1000.0
!> Option 2
!> Rainfall and snowfall rate are derived from total precipitation rate
!> assuming a separation at 0 degC (as in GEM-Hydro)
            if(cm%dat(ck%TT)%GAT(il1 + k) > tcdk) then
                bus(rainrate + k) = cm%dat(ck%RT)%GAT(il1 + k)/1000.0
                bus(snowrate + k) = 0.0
            else
                bus(rainrate + k) = 0.0
                bus(snowrate + k) = cm%dat(ck%RT)%GAT(il1 + k)/1000.0
            end if
            bus(flusolis + k) = cm%dat(ck%FB)%GAT(il1 + k)
            bus(fdsi + k) = cm%dat(ck%FI)%GAT(il1 + k)
            bus(tmoins + k) = cm%dat(ck%TT)%GAT(il1 + k)
            bus(humoins + k) = cm%dat(ck%HU)%GAT(il1 + k)
            bus(umoins + k) = cm%dat(ck%UV)%GAT(il1 + k)
            bus(vmoins + k) = 0.0
            bus(pmoins + k) = cm%dat(ck%P0)%GAT(il1 + k)
        end do

        call compvirttemp(sigma_t, bus, bussiz)
        if (.not. observed_forcing) call surflayerheight(sigma_u, sigma_t, bus, bussiz)

        !> Call subroutine to compute layer thicknesses.
        call layer_thickness()

        !> Update vegetation parameters as a function of julian day.
        call inicover_svs(bus, bussiz, kount, NG)

        !> Integrate SVS for one time step.
        call svs(bus, bussiz, bidon, 1, dt, kount, 1, NG, NG, 1)

        !> Transfer variables.
        do k = 0, NG - 1
            vs%tile%qac(il1 + k) = bus(qsurf + k)
            vs%tile%rcan(il1 + k) = bus(wveg + k)
            vs%tile%tac(il1 + k) = bus(tsurf + k)
            vs%tile%tcan(il1 + k) = (bus(tvege + k) + bus(tvege + NG + k) + bus(tsnowveg + k) + bus(tsnowveg + NG + k))/4.0
            vs%tile%sno(il1 + k) = bus(snoma + k)
            vs%tile%albs(il1 + k) = (bus(snoal + k) + bus(snval + k))/2.0
            vs%tile%rhos(il1 + k) = ((bus(snoro + k) + bus(snvro + k))/2.0)*900.0
            vs%tile%tsno(il1 + k) = (bus(tsnow + k) + bus(tsnow + NG + k))/2.0
            if (bus(snoma + k) > 0.0) then
                vs%tile%wsno(il1 + k) = bus(wsnow + k)
            else
                vs%tile%wsno(il1 + k) = 0.0
            end if
            vs%tile%evap(il1 + k) = bus(wflux + k)
            vs%tile%qevp(il1 + k) = bus(fv + k)
            vs%tile%hfs(il1 + k) = bus(fc + k)
            vs%tile%rofo(il1 + k) = max(0.0, bus(runofftot + k))/ic%dts
!EG_MOD add lateral flow from layers 1 to KHYD
            vs%tile%rofs(il1 + k) = 0.0
            do j = 1, KHYD
                vs%tile%rofs(il1 + k) = vs%tile%rofs(il1 + k) + max(0.0, bus(latflw + (j - 1)*NG + k))/ic%dts
            end do
            vs%tile%thic(il1 + k, 1) = bus(isoil + k)
            vs%tile%thlq(il1 + k, 1) = bus(wsoil + k)
            vs%tile%thlq(il1 + k, 2) = bus(wsoil + NG + k)
            do j = 3, shd%lc%IGND
                vs%tile%thlq(il1 + k, j) = bus(wsoil + (j - 1)*NG + k)
            end do
            vs%tile%tbar(il1 + k, 1) = bus(tsoil + k)
            do j = 2, shd%lc%IGND
                vs%tile%tbar(il1 + k, j) = bus(tsoil + NG + k)
            end do
!-            vs%tile%gflx(il1 + k, :) =
            vs%tile%rofb(il1 + k) = max(0.0, bus(watflow + KHYD*NG + k))/ic%dts
        end do

!>>>svs_output
!        if (ISHEADNODE) then

            !> Daily.
!            preacc_dly = preacc_dly + sum(bus(rainrate:(rainrate + NG - 1)))*ic%dts + sum(bus(snowrate:(snowrate + NG - 1)))*ic%dts !sum of all 'var' in bus?; depth
!            if (ic%now%day /= ic%next%day) then !last time-step of day
!                write(iout_dly, 1010) ic%now%year, ic%now%jday, &
!                    preacc_dly, & !daily acc.
!                    (preacc_dly/real(ic%ts_daily*ic%dts)) !rate = (value)/seconds in day using ts_daily and dts
!                preacc_dly = 0.0 !reset accumulators
!            end if

            !> Hourly.
!            preacc_hly = preacc_hly + sum(bus(rainrate:(rainrate + NG - 1)))*ic%dts + sum(bus(snowrate:(snowrate + NG - 1)))*ic%dts !sum of all 'var' in bus?; depth
!            preacc_tot = preacc_tot + &
!                1000.0*sum(bus(rainrate:(rainrate + NG - 1)))*ic%dts + 1000.0*sum(bus(snowrate:(snowrate + NG - 1)))*ic%dts !sum of all 'var' in bus?; depth
!            runoff_acc = runoff_acc + bus(runofftot)
!            wsoil_tot = 0.0
!            isoil_tot = 0.0
!            call layer_thickness() !called above
!            do j = 1, KHYD
!                wsoil_tot = wsoil_tot + 1000.0*bus(wsoil + (j - 1)*NG)*delz(j) ! mm
!            end do

!            if (ic%now%hour /= ic%next%hour) then !last time-step of hour
!                write(iout_hly, 1010) ic%now%year, ic%now%jday, ic%now%hour, preacc_hly !daily acc.
!                write(iout_hly, 1010) ic%now%year, ic%now%jday, ic%now%hour, &
!                    bus(snoma), bus(snodpl), bus(snoal), &
!                    bus(tsnow), bus(tsnow + 1), bus(tsnavg), bus(rainrate), bus(snowrate), bus(wsnow)
                !daily acc.
!                preacc_hly = 0.0 !reset accumulators
!                write(iout_wat_bal, 1010) ic%now%year, ic%now%jday, ic%now%hour, &
!                    preacc_tot, bus(accevap), bus(latflaf), &
!                    bus(drainaf), runoff_acc, wsoil_tot, isoil_tot, &
!                    bus(snoma), bus(snvma), bus(wsnow), bus(wsnv), bus(wveg), bus(vegh), bus(vegl)
!            end if

!            if (kount == 0) then
!                wsoil_ini = wsoil_tot
!                end if
!            bal_in_out = preacc_tot - bus(accevap) - bus(drainaf) - runoff_acc - bus(latflaf)
!            stock = (1 - bus(vegh))*bus(snoma) + bus(vegh)*bus(snvma) + &
!                wsoil_tot - wsoil_ini + isoil_tot + bus(wveg)*(bus(vegl) + bus(vegh))
!            bal_tot = bal_in_out - stock
!            if ((abs(bal_tot - bal_pre)) > 0.1) then
!                write(*, *) 'Inbalance ', bal_tot, bal_tot - bal_pre
!            end if
!            bal_pre = bal_tot

!            write(iout_ts, 1010) ic%now%year, ic%now%jday, ic%now%hour, ic%now%mins, &
!                sum(bus(rainrate:(rainrate + NG - 1))), sum(bus(snowrate:(snowrate + NG - 1)))
!        end if

!1010    format(9999(g15.7e2, ','))
!<<<svs_output

    end subroutine

end module
