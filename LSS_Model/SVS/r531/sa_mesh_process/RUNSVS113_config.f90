module RUNSVS113_config

    use RUNSVS113_variables

    implicit none

    private

    public RUNSVS113_init, RUNSVS113_finalize

    contains

    subroutine RUNSVS113_init(shd, fls, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_variables
        use sa_mesh_utilities
        use climate_forcing
        use FLAGS

        use RUNSVS_mod
        use runsvs_utils
!        use runsvs_io

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

        !> For SAVERESUMEFLAG 3
        real(kind = 4), dimension(:, :), allocatable :: ALBSROW, CMAIROW, GROROW, QACROW, RCANROW, &
            RHOSROW, SCANROW, SNOROW, TACROW, TBASROW, &
            TCANROW, TPNDROW, TSNOROW, WSNOROW, ZPNDROW
        real(kind = 4), dimension(:, :, :), allocatable :: TBARROW, THICROW, THLQROW, TSFSROW

        integer NA, NTYPE, NSL, ierr, iun

#include "options.cdk"
#include "isbapar.cdk"
#include "surfcon.cdk"
#include "thermoconsts.inc"

!        integer, parameter :: bussiz = runsvs_busdim
!        real bus(bussiz)
!        integer datecmc_o
!        integer datecmc_v, date_v, hour_v, date_f, hour_f, istat, kount, bidon
!        real(kind = 8) kdt
        integer k, ki, kj, j
        real sumfcanz0

        integer, external :: newdate
!        external incdatr
        external svs, inicover_svs
!        external inisoili_svs, phyopt_initdata, runsvs_init

        !> Return if the process is not marked active or if not the head node.
        if (.not. RUNSVS113_flgs%PROCESS_ACTIVE) return

        !> Initialize common blocks, read options and configuration file.
!        read(*, nml = RUNSVS_OPT)
!        nt = 169440
!        dateo = 20020101
!        houro = 07000000
!        dt = 1800
        sigma_u = 0.995
        sigma_t = 0.995
        observed_forcing = .false.
!        inifile = '/cygdrive/c/Data/dprincz/OneDrive/Workspace/Data/MESH_Code/1_Main/TRUNK/LSS_Model/SVS/r531/data/02BA003.ini'
!        metfile = '/cygdrive/c/Data/dprincz/OneDrive/Workspace/Data/MESH_Code/1_Main/TRUNK/LSS_Model/SVS/r531/data/02BA003.met'
!        outfile = &
!            '/cygdrive/c/Data/dprincz/OneDrive/Workspace/Data/MESH_Code/1_Main/TRUNK/LSS_Model/SVS/r531/data/02BA003_sa_mesh.out'
!        xcount = 5
!        ycount = 7

!        call svs_bus_init(xcount*ycount)
        call svs_bus_init(il2 - il1 + 1)
        bussiz = runsvs_busdim
        allocate(bus(bussiz))
        bus = 0.0
!        delt = dt

#include "surfcon_ini.cdk"

        call phyopt_initdata()
!        call open_files(inifile, metfile, outfile)
!        open(fid_out, file = outfile)

        !> Read CLASS-style INI file.
!        call read_ini_file(bus, bussiz)

        !> Resume the state of prognostic variables from file.
        select case (RESUMEFLAG)

            !> RESUMEFLAG 3.
            case (3)

                !> Open the resume state file.
                iun = fls%fl(mfk%f883)%iun
                open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.svs', status = 'old', action = 'read', &
                     form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

                !> Local indices.
                NA = shd%NA
                NTYPE = shd%lc%NTYPE
                NSL = shd%lc%IGND

                !> Allocate temporary variables.
                allocate(ALBSROW(NA, NTYPE), CMAIROW(NA, NTYPE), GROROW(NA, NTYPE), QACROW(NA, NTYPE), RCANROW(NA, NTYPE), &
                         RHOSROW(NA, NTYPE), SCANROW(NA, NTYPE), SNOROW(NA, NTYPE), TACROW(NA, NTYPE), TBASROW(NA, NTYPE), &
                         TCANROW(NA, NTYPE), TPNDROW(NA, NTYPE), TSNOROW(NA, NTYPE), WSNOROW(NA, NTYPE), ZPNDROW(NA, NTYPE), &
                         TBARROW(NA, NTYPE, NSL), THICROW(NA, NTYPE, NSL), THLQROW(NA, NTYPE, NSL), TSFSROW(NA, NTYPE, 4))

                !> Read inital values from the file.
                read(iun) ALBSROW
                read(iun) CMAIROW
                read(iun) GROROW
                read(iun) QACROW
                read(iun) RCANROW
                read(iun) RHOSROW
                read(iun) SCANROW
                read(iun) SNOROW
                read(iun) TACROW
                read(iun) TBARROW
                read(iun) TBASROW
                read(iun) TCANROW
                read(iun) THICROW
                read(iun) THLQROW
                read(iun) TPNDROW
                read(iun) TSFSROW
                read(iun) TSNOROW
                read(iun) WSNOROW
                read(iun) ZPNDROW

                !> Close the file to free the unit.
                close(iun)

                !> Scatter the temporary variables.
                do k = il1, il2

                    !> Grab the grid and GRU of the current tile.
                    ki = shd%lc%ILMOS(k)
                    kj = shd%lc%JLMOS(k)

                    !> Assign values.
                    stas%sno%albs(k) = ALBSROW(ki, kj)
                    stas%cnpy%cmas(k) = CMAIROW(ki, kj)
                    stas%cnpy%gro(k) = GROROW(ki, kj)
                    stas%cnpy%qac(k) = QACROW(ki, kj)
                    stas%cnpy%rcan(k) = RCANROW(ki, kj)
                    stas%sno%rhos(k) = RHOSROW(ki, kj)
                    stas%cnpy%sncan(k) = SCANROW(ki, kj)
                    stas%sno%sno(k) = SNOROW(ki, kj)
                    stas%cnpy%tac(k) = TACROW(ki, kj)
                    stas%sl%tbar(k, :) = TBARROW(ki, kj, :)
                    stas%sl%tbas(k) = TBASROW(ki, kj)
                    stas%cnpy%tcan(k) = TCANROW(ki, kj)
                    stas%sl%thic(k, :) = THICROW(ki, kj, :)
                    stas%sl%thlq(k, :) = THLQROW(ki, kj, :)
                    stas%sfc%tpnd(k) = TPNDROW(ki, kj)
                    stas%sfc%tsfs(k, :) = TSFSROW(ki, kj, :)
                    stas%sno%tsno(k) = TSNOROW(ki, kj)
                    stas%sno%wsno(k) = WSNOROW(ki, kj)
                    stas%sfc%zpnd(k) = ZPNDROW(ki, kj)

                end do

                !> Deallocate temporary variables.
                deallocate(ALBSROW, CMAIROW, GROROW, QACROW, RCANROW, &
                           RHOSROW, SCANROW, SNOROW, TACROW, TBASROW, &
                           TCANROW, TPNDROW, TSNOROW, WSNOROW, ZPNDROW, &
                           TBARROW, THICROW, THLQROW, TSFSROW)

            !> RESUMEFLAG 4.
            case (4)
                call read_init_prog_variables_class(fls)

            !> RESUMEFLAG 5.
            case (5)
                call read_init_prog_variables_class(fls)

        end select !case (RESUMEFLAG)

        !> Parse CLASS variables to bus.
        do k = 0, NG - 1

            !> Basic configuration.
            ki = shd%lc%ILMOS(il1 + k)
            kj = shd%lc%JLMOS(il1 + k)

            !> Convert lat, lon to radian.
            bus(dlat + k) = ((shd%yOrigin + shd%yDelta*shd%yyy(ki)) - shd%yDelta/2.0)*PI/180.0
            bus(dlon + k) = ((shd%xOrigin + shd%xDelta*shd%xxx(ki)) - shd%xDelta/2.0)*PI/180.0

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
            bus(vegf + 3*NG + k) = pm%cp%fcan(il1 + k, 1)
            bus(vegf + 6*NG + k) = pm%cp%fcan(il1 + k, 2)
            bus(vegf + 14*NG + k) = pm%cp%fcan(il1 + k, 3)
            bus(vegf + 13*NG + k) = pm%cp%fcan(il1 + k, 4)
            bus(vegf + 20*NG + k) = pm%cp%fcan(il1 + k, 5)
            bus(slop + k) = pm%tp%xslp(il1 + k)
            bus(draindens + k) = pm%hp%dd(il1 + k)!*0.001
            bus(rootdp + k) = pm%slp%sdep(il1 + k)

            !> Compute weighted average of log z0 wrt vegetation
            !> (used for momentum only - local z0 used for temperature/humidity).
            bus(z0 + k) = 0.0
            sumfcanz0 = 0.0
            do j = 1, 5
                bus(z0 + k) = bus(z0 + k) + pm%cp%fcan(il1 + k, j)*pm%cp%lnz0(il1 + k, j)
                sumfcanz0 = sumfcanz0 + pm%cp%fcan(il1 + k, j)
            end do
            if (sumfcanz0 > 0.0) then
                bus(z0 + k) = bus(z0 + k)/sumfcanz0
            end if
            bus(z0 + k) = exp(bus(z0 + k))

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

            !> For soil texture we ignore negative numbers
            !> which signal special soils (organic/impermeable/glaciers).
            bus(sand + k) = max(pm%slp%sand(il1 + k, 1), 0.0)
            bus(sand + NG + k) = max(pm%slp%sand(il1 + k, 1), 0.0)
            bus(sand + 2*NG + k) = max(pm%slp%sand(il1 + k, 2), 0.0)
            bus(clay + k) = max(pm%slp%clay(il1 + k, 1), 0.0)
            bus(clay + NG + k) = max(pm%slp%clay(il1 + k, 1), 0.0)
            bus(clay + 2*NG + k) = max(pm%slp%clay(il1 + k, 2), 0.0)
            if (shd%lc%IGND >= 5) then
                bus(sand + 3*NG + k) = max(pm%slp%sand(il1 + k, 2), 0.0)
                bus(sand + 4*NG + k) = max(pm%slp%sand(il1 + k, 3), 0.0)
                bus(sand + 5*NG + k) = max(pm%slp%sand(il1 + k, 4), 0.0)
                bus(sand + 6*NG + k) = max(pm%slp%sand(il1 + k, 5), 0.0)
                bus(clay + 3*NG + k) = max(pm%slp%clay(il1 + k, 2), 0.0)
                bus(clay + 4*NG + k) = max(pm%slp%clay(il1 + k, 3), 0.0)
                bus(clay + 5*NG + k) = max(pm%slp%clay(il1 + k, 4), 0.0)
                bus(clay + 6*NG + k) = max(pm%slp%clay(il1 + k, 5), 0.0)
            else
                do j = 3, 6
                    bus(sand + j*NG + k) = max(pm%slp%sand(il1 + k, 3), 0.0)
                    bus(clay + j*NG + k) = max(pm%slp%clay(il1 + k, 3), 0.0)
                end do
            end if

            !> State variables.

            !> Map soil soil moisture.
            !> CLASS layer  <->  SVS layer
            !>       1              1-2
            !>       2               3
            !>       3              4-7
            bus(wdsoil + k) = stas%sl%thlq(il1 + k, 1)
            bus(wdsoil + NG + k) = stas%sl%thlq(il1 + k, 2)
            bus(wdsoil + 2*NG + k) = stas%sl%thlq(il1 + k, 3)
            do j = 3, 6
                bus(wdsoil + j*NG + k) = stas%sl%thlq(il1 + k, 3)
            end do

            !> Map soil temperature.
            !> CLASS layer  <->  SVS layer
            !>       1               1
            !>       2               2
            bus(tsoil + k) = stas%sl%tbar(il1 + k, 1)! + tcdk
            bus(tsoil + NG + k) = stas%sl%tbar(il1 + k, 2)! + tcdk
            bus(tground + k) = stas%sl%tbar(il1 + k, 1)! + tcdk
            bus(tground + NG + k) = stas%sl%tbar(il1 + k, 2)! + tcdk

            !> Map vegetation temperature.
            do j = 0, 1
                bus(tvege + j*NG + k) = stas%cnpy%tcan(il1 + k)! + tcdk
                bus(tsnowveg + j*NG + k) = stas%cnpy%tcan(il1 + k)! + tcdk
            end do

            !> Map snow properties.
            !* snoro: Density (kg/m3) to relative density wrt ice.
            do j = 0, 1
                bus(tsnow + j*NG + k) = stas%sno%tsno(il1 + k)! + tcdk
            end do
            bus(snoro + k) = stas%sno%rhos(il1 + k)/900.0
            bus(snvro + k) = stas%sno%rhos(il1 + k)/900.0
            bus(snoal + k) = stas%sno%albs(il1 + k)
            bus(snval + k) = stas%sno%albs(il1 + k)

        end do

        !> Time loop.

        !> Convert start date/hour to CMC datestamp.
!        istat = newdate(datecmc_o, dateo, houro, 3)
!        kount = 0
!        do kount = 0, nt

            !> Determine time stamps of current date.
!            kdt = kount*(dt*1.0D0)/3600.0D0

            !> Compute date valid.
!            call incdatr(datecmc_v, datecmc_o, kdt)

            !> Convert to old style.
!            istat = newdate(datecmc_v, date, bidon, -4)

            !> Convert to printable.
!            istat = newdate(datecmc_v, date_v, hour_v, -3)

            !> Read meteorological forcing data.
            !> Careful: at kount=0 we read data for kount=1 so we skip reading if kount=1.
!            if (kount == 0 .or. (kount /= 1 .and. (date_f < date_v .or. hour_f < hour_v))) then
!                call read_met_file(date_v, hour_v, date_f, hour_f, bus, bussiz)
!                call compvirttemp(sigma_t, bus, bussiz)
!                if (.not. observed_forcing) call surflayerheight(sigma_u, sigma_t, bus, bussiz)
!            end if

            !> Initialize parameters and state variables at kount=0.
!            if (kount == 0) then

                !> Initialize surface parameters.
                call inisoili_svs(bus, bussiz, NG)

                !> Initialize state variables.
                call runsvs_init(bus, bussiz)

!            end if

            !> Update vegetation parameters as a function of julian day.
!            call inicover_svs(bus, bussiz, kount, NG)

            !> Integrate SVS for one time step.
!            call svs(bus, bussiz, bidon, 1, dt, kount, 1, NG, NG, 1)

            !> Write outputs (currently in ASCII format).
!            call write_out_file(date_v, hour_v, bus, bussiz)

!        end do

        !> Wrap up.
!        call close_files()

!        stop 'by RUNSVS113_init()'

    end subroutine

    subroutine RUNSVS113_finalize(shd, fls, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_variables
        use climate_forcing
        use FLAGS

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

        !> For SAVERESUMEFLAG 3
        real(kind = 4), dimension(:, :), allocatable :: ALBSROW, CMAIROW, GROROW, QACROW, RCANROW, &
            RHOSROW, SCANROW, SNOROW, TACROW, TBASROW, &
            TCANROW, TPNDROW, TSNOROW, WSNOROW, ZPNDROW
        real(kind = 4), dimension(:, :, :), allocatable :: TBARROW, THICROW, THLQROW, TSFSROW

        integer NA, NTYPE, NSL, k, ki, kj, ierr, iun

        !> Return if the process is not marked active or if not the head node.
        if (.not. RUNSVS113_flgs%PROCESS_ACTIVE) return

        !> Only the head node writes CLASS output.
        if (.not. ipid == 0) return

        !> Local indices.
        NA = shd%NA
        NTYPE = shd%lc%NTYPE
        NSL = shd%lc%IGND

        !> Save the state of prognostic variables to file.
        select case (SAVERESUMEFLAG)

            !> SAVERESUMEFLAG 3.
            case (3)

                !> Open the resume state file.
                iun = fls%fl(mfk%f883)%iun
                open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.svs', status = 'replace', action = 'write', &
                     form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

                !> Allocate and initialize temporary variables.
                allocate(ALBSROW(NA, NTYPE), CMAIROW(NA, NTYPE), GROROW(NA, NTYPE), QACROW(NA, NTYPE), RCANROW(NA, NTYPE), &
                         RHOSROW(NA, NTYPE), SCANROW(NA, NTYPE), SNOROW(NA, NTYPE), TACROW(NA, NTYPE), TBASROW(NA, NTYPE), &
                         TCANROW(NA, NTYPE), TPNDROW(NA, NTYPE), TSNOROW(NA, NTYPE), WSNOROW(NA, NTYPE), ZPNDROW(NA, NTYPE), &
                         TBARROW(NA, NTYPE, NSL), THICROW(NA, NTYPE, NSL), THLQROW(NA, NTYPE, NSL), TSFSROW(NA, NTYPE, 4))
                ALBSROW = 0.0; CMAIROW = 0.0; GROROW = 0.0; QACROW = 0.0; RCANROW = 0.0; RHOSROW = 0.0
                SCANROW = 0.0; SNOROW = 0.0; TACROW = 0.0; TBASROW = 0.0; TCANROW = 0.0; TPNDROW = 0.0
                TSNOROW = 0.0; WSNOROW = 0.0; ZPNDROW = 0.0
                TBARROW = 0.0; THICROW = 0.0; THLQROW = 0.0; TSFSROW = 0.0

                !> Gather the temporary variables.
                do k = 1, shd%lc%NML

                    !> Grab the grid and GRU of the current tile.
                    ki = shd%lc%ILMOS(k)
                    kj = shd%lc%JLMOS(k)

                    !> Assign values.
                    ALBSROW(ki, kj) = stas%sno%albs(k)
                    CMAIROW(ki, kj) = stas%cnpy%cmas(k)
                    GROROW(ki, kj) = stas%cnpy%gro(k)
                    QACROW(ki, kj) = stas%cnpy%qac(k)
                    RCANROW(ki, kj) = stas%cnpy%rcan(k)
                    RHOSROW(ki, kj) = stas%sno%rhos(k)
                    SCANROW(ki, kj) = stas%cnpy%sncan(k)
                    SNOROW(ki, kj) = stas%sno%sno(k)
                    TACROW(ki, kj) = stas%cnpy%tac(k)
                    TBARROW(ki, kj, :) = stas%sl%tbar(k, :)
                    TBASROW(ki, kj) = stas%sl%tbas(k)
                    TCANROW(ki, kj) = stas%cnpy%tcan(k)
                    THICROW(ki, kj, :) = stas%sl%thic(k, :)
                    THLQROW(ki, kj, :) = stas%sl%thlq(k, :)
                    TPNDROW(ki, kj) = stas%sfc%tpnd(k)
                    TSFSROW(ki, kj, :) = stas%sfc%tsfs(k, :)
                    TSNOROW(ki, kj) = stas%sno%tsno(k)
                    WSNOROW(ki, kj) = stas%sno%wsno(k)
                    ZPNDROW(ki, kj) = stas%sfc%zpnd(k)

                end do

                !> Read inital values from the file.
                write(iun) ALBSROW
                write(iun) CMAIROW
                write(iun) GROROW
                write(iun) QACROW
                write(iun) RCANROW
                write(iun) RHOSROW
                write(iun) SCANROW
                write(iun) SNOROW
                write(iun) TACROW
                write(iun) TBARROW
                write(iun) TBASROW
                write(iun) TCANROW
                write(iun) THICROW
                write(iun) THLQROW
                write(iun) TPNDROW
                write(iun) TSFSROW
                write(iun) TSNOROW
                write(iun) WSNOROW
                write(iun) ZPNDROW

                !> Close the file to free the unit.
                close(iun)

                !> Deallocate temporary variables.
                deallocate(ALBSROW, CMAIROW, GROROW, QACROW, RCANROW, &
                           RHOSROW, SCANROW, SNOROW, TACROW, TBASROW, &
                           TCANROW, TPNDROW, TSNOROW, WSNOROW, ZPNDROW, &
                           TBARROW, THICROW, THLQROW, TSFSROW)

            !> SAVERESUMEFLAG 4.
            case (4)
                call save_init_prog_variables_class(fls)

            !> RESUMEFLAG 5.
            case (5)
                call save_init_prog_variables_class(fls)

        end select !case (SAVERESUMEFLAG)

    end subroutine

end module
