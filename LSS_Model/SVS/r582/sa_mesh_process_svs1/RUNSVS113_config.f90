module RUNSVS113_config

    use sa_mesh_shared_variables
    use RUNSVS113_variables

    implicit none

    private

    public RUNSVS113_init

    contains

    subroutine RUNSVS113_init(shd, fls, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_shared_variables
        use climate_forcing

        use RUNSVS_mod
        use runsvs_utils
        use phy_options
!        use runsvs_io

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

!#include "options.cdk"
#include "isbapar.cdk"
!#include "surfcon.cdk"
#include "thermoconsts.inc"


        integer k, ki, kj, j
        real sumfcanz0

        integer, external :: newdate
!        external incdatr
        external svs, inicover_svs
!        external inisoili_svs, phyopt_initdata, runsvs_init

        !> Return if the process is not marked active or if not the head node.
        if (.not. RUNSVS113_flgs%PROCESS_ACTIVE) return

        !> Initialize common blocks, read options and configuration file.

        sigma_u = 0.995
        sigma_t = 0.995
        observed_forcing = .true.

        call svs_bus_init(shd%lc%NML)
        bussiz = runsvs_busdim
        allocate(bus(bussiz))
        bus = 0.0

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
            bus(sand + k) = max(pm%slp%sand(k + 1, 1), 0.0)
            bus(sand + NG + k) = max(pm%slp%sand(k + 1, 1), 0.0)
            bus(sand + 2*NG + k) = max(pm%slp%sand(k + 1, 2), 0.0)
            bus(clay + k) = max(pm%slp%clay(k + 1, 1), 0.0)
            bus(clay + NG + k) = max(pm%slp%clay(k + 1, 1), 0.0)
            bus(clay + 2*NG + k) = max(pm%slp%clay(k + 1, 2), 0.0)
            if (shd%lc%IGND >= 5) then
                bus(sand + 3*NG + k) = max(pm%slp%sand(k + 1, 2), 0.0)
                bus(sand + 4*NG + k) = max(pm%slp%sand(k + 1, 3), 0.0)
                bus(sand + 5*NG + k) = max(pm%slp%sand(k + 1, 4), 0.0)
                bus(sand + 6*NG + k) = max(pm%slp%sand(k + 1, 5), 0.0)
                bus(clay + 3*NG + k) = max(pm%slp%clay(k + 1, 2), 0.0)
                bus(clay + 4*NG + k) = max(pm%slp%clay(k + 1, 3), 0.0)
                bus(clay + 5*NG + k) = max(pm%slp%clay(k + 1, 4), 0.0)
                bus(clay + 6*NG + k) = max(pm%slp%clay(k + 1, 5), 0.0)
            else
                do j = 3, 6
                    bus(sand + j*NG + k) = max(pm%slp%sand(k + 1, 3), 0.0)
                    bus(clay + j*NG + k) = max(pm%slp%clay(k + 1, 3), 0.0)
                end do
            end if

            !> State variables.

            !> Map soil soil moisture.
            !> CLASS layer  <->  SVS layer
            !>       1              1-2
            !>       2               3
            !>       3              4-7
            bus(wsoil + k) = stas%sl%thlq(k + 1, 1)
            bus(wsoil + NG + k) = stas%sl%thlq(k + 1, 2)
            bus(wsoil + 2*NG + k) = stas%sl%thlq(k + 1, 3)
            do j = 3, 6
                bus(wsoil + j*NG + k) = stas%sl%thlq(k + 1, 3)
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
                bus(tsnowveg + j*NG + k) = stas%sno%tsno(k + 1)! + tcdk
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
            print *, '% SAND:           ', bus(sand), bus(sand + NG), bus(sand + 2*NG), bus(sand + 3*NG), bus(sand + 6*NG)
            print *, '% CLAY:           ', bus(clay), bus(clay + NG), bus(clay + 2*NG)
            print *, '--------------------------------'
            print *, 'SOIL MOISTURE:    ', bus(wsoil), bus(wsoil + NG), bus(wsoil + 2*NG)
            print *, 'SOIL TEMPERATURE: ', bus(tsoil), bus(tsoil + NG)
            print *, 'VEGETATION TEMP.: ', bus(tvege), bus(tvege + NG)
            print *, 'SNOW TEMPERATURE: ', bus(tsnow), bus(tsnow + NG)
            print *, 'SNOW DENSITY:     ', bus(snoro), bus(snvro)
            print *, 'SNOW ALBEDO:      ', bus(snoal), bus(snval)
            print *, '--------------------------------'
            print *
        end if

        !> Time loop.

	!> Initialize surface parameters.
	call inisoili_svs(bus, bussiz, NG)
	!call inisoili_svs(NG, 1)

	!> Initialize state variables.
	call runsvs_init(bus, bussiz)

!>>>svs_output
    !> Daily.
!    open(iout_dly, file = './' // trim(fls%GENDIR_OUT) // '/' // 'svs_out.csv', status = 'unknown', action = 'write')
!    write(iout_dly, 1010) 'YEAR', 'DAY', 'PRE', 'PRATE'
    preacc_dly = 0.0 !reset accumulators

    !> Hourly.
    open(iout_hly, file = './' // trim(fls%GENDIR_OUT) // '/' // 'svs1_out_hourly.csv', status = 'unknown', action = 'write')
    write(iout_hly, 1010) 'YEAR', 'DAY', 'HOUR', 'SWE', 'SD','SNALB'
!    write(iout_hly, 1010) 'YEAR', 'DAY', 'HOUR', 'PRE'
    preacc_hly = 0.0 !reset accumulators

    !> Time-step.
!    open(iout_ts, file = './' // trim(fls%GENDIR_OUT) // '/' // 'svs_out_ts.csv', status = 'unknown', action = 'write')
!    write(iout_ts, 1010) 'YEAR', 'DAY', 'HOUR', 'MINS', 'RPCP', 'SPCP'

1010    format(9999(g15.7e2, ','))
!<<<svs_output

    end subroutine

end module
