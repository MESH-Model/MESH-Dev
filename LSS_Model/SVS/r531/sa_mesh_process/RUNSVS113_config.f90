module RUNSVS113_config

    use sa_mesh_shared_variables
    use RUNSVS113_variables

    implicit none

    private

    public RUNSVS113_init

    contains

    subroutine RUNSVS113_init(shd, fls, ts, cm, wb, eb, sp)

        use mpi_shared_variables
        use sa_mesh_shared_parameters
        use sa_mesh_shared_variables
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        use RUNSVS_mod
        use runsvs_utils
!        use runsvs_io

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(dates_model) :: ts
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sp

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
        call svs_bus_init(shd%lc%NML)
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

        !> Parse CLASS variables to bus.
        do k = 0, NG - 1

            !> Basic configuration.
            ki = shd%lc%ILMOS(k + 1)
            kj = shd%lc%JLMOS(k + 1)

            !> Convert lat, lon to radian.
            bus(dlat + k) = ((shd%yOrigin + shd%yDelta*shd%yyy(ki)) - shd%yDelta/2.0)*PI/180.0
            bus(dlon + k) = ((shd%xOrigin + shd%xDelta*shd%xxx(ki)) - shd%xDelta/2.0)*PI/180.0

            !> Map CLASS parameters to SVS parameters.
            !* zusl: Height of wind forcing.
            !* ztsl: Height of temperature forcing.
            if (observed_forcing) then
                bus(zusl + k) = pm%sfp%zrfm(k + 1)
                bus(ztsl + k) = pm%sfp%zrfh(k + 1)
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
            bus(vegf + 3*NG + k) = pm%cp%fcan(k + 1, 1)
            bus(vegf + 6*NG + k) = pm%cp%fcan(k + 1, 2)
            bus(vegf + 14*NG + k) = pm%cp%fcan(k + 1, 3)
            bus(vegf + 13*NG + k) = pm%cp%fcan(k + 1, 4)
            bus(vegf + 20*NG + k) = pm%cp%fcan(k + 1, 5)
            bus(slop + k) = pm%tp%xslp(k + 1)
            bus(draindens + k) = pm%hp%dd(k + 1)!*0.001
            bus(rootdp + k) = pm%slp%sdep(k + 1)

            !> Compute weighted average of log z0 wrt vegetation
            !> (used for momentum only - local z0 used for temperature/humidity).
            bus(z0 + k) = 0.0
            sumfcanz0 = 0.0
            do j = 1, 5
                bus(z0 + k) = bus(z0 + k) + pm%cp%fcan(k + 1, j)*pm%cp%lnz0(k + 1, j)
                sumfcanz0 = sumfcanz0 + pm%cp%fcan(k + 1, j)
            end do
            if (sumfcanz0 > 0.0) then
                bus(z0 + k) = bus(z0 + k)/sumfcanz0
            end if
            bus(z0 + k) = exp(bus(z0 + k))

            !> Map soil texture.
            !> CLASS layer  <->  SVS layer
            !>       1              1-2
            !>       2               3
            !>       3              4-7
            !> For soil texture we ignore negative numbers
            !> which signal special soils (organic/impermeable/glaciers).
            bus(sand + k) = max(pm%slp%sand(k + 1, 1), 0.0)
            bus(sand + NG + k) = max(pm%slp%sand(k + 1, 1), 0.0)
            bus(sand + 2*NG + k) = max(pm%slp%sand(k + 1, 2), 0.0)
            bus(clay + k) = max(pm%slp%clay(k + 1, 1), 0.0)
            bus(clay + NG + k) = max(pm%slp%clay(k + 1, 1), 0.0)
            bus(clay + 2*NG + k) = max(pm%slp%clay(k + 1, 2), 0.0)
            do j = 3, 6
                bus(sand + j*NG + k) = max(pm%slp%sand(k + 1, 3), 0.0)
                bus(clay + j*NG + k) = max(pm%slp%clay(k + 1, 3), 0.0)
            end do

            !> State variables.

            !> Map soil soil moisture.
            !> CLASS layer  <->  SVS layer
            !>       1              1-2
            !>       2               3
            !>       3              4-7
            bus(wdsoil + k) = stas%sl%thlq(k + 1, 1)
            bus(wdsoil + NG + k) = stas%sl%thlq(k + 1, 2)
            bus(wdsoil + 2*NG + k) = stas%sl%thlq(k + 1, 3)
            do j = 3, 6
                bus(wdsoil + j*NG + k) = stas%sl%thlq(k + 1, 3)
            end do

            !> Map soil temperature.
            !> CLASS layer  <->  SVS layer
            !>       1               1
            !>       2               2
            bus(tsoil + k) = stas%sl%tbar(k + 1, 1)! + tcdk
            bus(tsoil + NG + k) = stas%sl%tbar(k + 1, 2)! + tcdk
            bus(tground + k) = stas%sl%tbar(k + 1, 1)! + tcdk
            bus(tground + NG + k) = stas%sl%tbar(k + 1, 2)! + tcdk

            !> Map vegetation temperature.
            do j = 0, 1
                bus(tvege + j*NG + k) = stas%cnpy%tcan(k + 1)! + tcdk
                bus(tsnowveg + j*NG + k) = stas%cnpy%tcan(k + 1)! + tcdk
            end do

            !> Map snow properties.
            !* snoro: Density (kg/m3) to relative density wrt ice.
            do j = 0, 1
                bus(tsnow + j*NG + k) = stas%sno%tsno(k + 1)! + tcdk
            end do
            bus(snoro + k) = stas%sno%rhos(k + 1)/900.0
            bus(snvro + k) = stas%sno%rhos(k + 1)/900.0
            bus(snoal + k) = stas%sno%albs(k + 1)
            bus(snval + k) = stas%sno%albs(k + 1)

        end do

        !> Summarize what we just did.
        if (ro%DIAGNOSEMODE > 0) then
            print *
            print *, '--------------------------------'
            print *, 'SVS DIAGNOSTICS'
            print *, '--------------------------------'
            print *, 'CLASS INI file read successfully'
            print *, '--------------------------------'
            print *, 'LOCATION: (', bus(dlat)*180.0/PI, ',', bus(dlon)*180.0/PI, ')'
            print *, 'VEGETATION COVER:'
            print *, '% NEEDLELEAF:     ', bus(vegf + 3*NG)*100
            print *, '% BROADLEAF:      ', bus(vegf + 6*NG)*100
            print *, '% CROPS:          ', bus(vegf + 14*NG)*100
            print *, '% GRASS:          ', bus(vegf + 13*NG)*100
            print *, '% URBAN:          ', bus(vegf + 20*NG)*100
            print *, 'ROUGHNESS LENGTH: ', bus(z0)
            print *, 'SLOPE:            ', bus(slop)
            print *, 'DRAIN.DENSITY     ', bus(draindens)
            print *, 'ROOT DEPTH:       ', bus(rootdp)
            print *, '% SAND:           ', bus(sand), bus(sand + NG), bus(sand + 2*NG)
            print *, '% CLAY:           ', bus(clay), bus(clay + NG), bus(clay + 2*NG)
            print *, '--------------------------------'
            print *, 'SOIL MOISTURE:    ', bus(wdsoil), bus(wdsoil + NG), bus(wdsoil + 2*NG)
            print *, 'SOIL TEMPERATURE: ', bus(tsoil), bus(tsoil + NG)
            print *, 'VEGETATION TEMP.: ', bus(tvege), bus(tvege + NG)
            print *, 'SNOW TEMPERATURE: ', bus(tsnow), bus(tsnow + NG)
            print *, 'SNOW DENSITY:     ', bus(snoro), bus(snvro)
            print *, 'SNOW ALBEDO:      ', bus(snoal), bus(snval)
            print *, '--------------------------------'
            print *
        end if

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

end module
