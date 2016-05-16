module SVS_module

    use SVS_module_variables

    implicit none

    private

    public RUNSVS

    contains

    subroutine RUNSVS(shd, fls, ts, ic, cm, wb, eb, sp)

        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_files_variabletypes
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
        type(iter_counter) :: ic
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
!        integer datecmc_o, date_f, hour_f
        integer datecmc_v, date_v, hour_v, istat, kount, bidon
        real(kind = 8) kdt

        integer i
        real fb(NG), fi(NG), pr(NG), ta(NG), qa(NG), uv(NG), p0(NG)

        integer, external :: newdate
        external incdatr
        external svs, inicover_svs
!        external inisoili_svs, phyopt_initdata, runsvs_init

        !> Return if the process is not marked active.
        if (.not. SVS_flgs%PROCESS_ACTIVE) return

        !> Initialize common blocks, read options and configuration file.
!        read(*, nml = RUNSVS_OPT)
!        nt = 169440
!        dateo = 20020101
!        houro = 07000000
!        dt = 1800
!        sigma_u = 0.995
!        sigma_t = 0.995
!        observed_forcing = .false.
!        inifile = '/cygdrive/c/Data/dprincz/OneDrive/Workspace/Data/MESH_Code/1_Main/TRUNK/LSS_Model/SVS/r531/data/02BA003.ini'
!        metfile = '/cygdrive/c/Data/dprincz/OneDrive/Workspace/Data/MESH_Code/1_Main/TRUNK/LSS_Model/SVS/r531/data/02BA003.met'
!        outfile = &
!            '/cygdrive/c/Data/dprincz/OneDrive/Workspace/Data/MESH_Code/1_Main/TRUNK/LSS_Model/SVS/r531/data/02BA003_sa_mesh.out'
!        xcount = 5
!        ycount = 7

!        call svs_bus_init(xcount*ycount)
!        bussiz = runsvs_busdim
!        allocate(bus(bussiz))
!        bus = 0.0
!        delt = dt

#include "surfcon_ini.cdk"

!        call phyopt_initdata()
!        call open_files(inifile, metfile, outfile)

        !> Read CLASS-style INI file.
!        call read_ini_file(bus, bussiz)

        !> Time loop.

        !> Convert start date/hour to CMC datestamp.
!        istat = newdate(datecmc_o, dateo, houro, 3)
        dt = real(ic%dts)
        kount = ic%ts_count - 1
!        do kount = 0, nt

        if (kount == 0) then
            dateo = ic%now_year*10000 + ic%now_month*100 + ic%now_day
            houro = ic%now_hour*1000000 + ic%now_mins*10000
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

            !> Read meteorological forcing data.
            !> Careful: at kount=0 we read data for kount=1 so we skip reading if kount=1.
!            if (kount == 0) then
!                call read_met_file(date_v, hour_v, date_f, hour_f, bus, bussiz)
!            end if

!        do j = 1, yCount
!            do i = 1, xCount
!                k = i + (j - 1)*xCount - 1
!                if(metr2c(j, i, ck%TT) > tcdk) then
!                    bus(rainrate + k) = metr2c(j, i, ck%RT)/1000.0
!                    bus(snowrate + k) = 0.0
!                else
!                    bus(rainrate + k) = 0.0
!                    bus(snowrate + k) = metr2c(j, i, ck%RT)/1000.0
!                end if
!                bus(flusolis + k) = metr2c(j, i, ck%FB)
!                bus(fdsi + k) = metr2c(j, i, ck%FI)
!                bus(tmoins + k) = metr2c(j, i, ck%TT)
!                bus(humoins + k) = metr2c(j, i, ck%HU)
!                bus(umoins + k) = metr2c(j, i, ck%UV)
!                bus(vmoins + k) = 0.0
!                bus(pmoins + k) = metr2c(j, i, ck%P0)
!            end do
!        end do

        do i = 0, NG - 1
            if(cm%dat(ck%TT)%GRD(i + 1) > tcdk) then
                bus(rainrate + i) = cm%dat(ck%RT)%GRD(i + 1)/1000.0
                bus(snowrate + i) = 0.0
            else
                bus(rainrate + i) = 0.0
                bus(snowrate + i) = cm%dat(ck%RT)%GRD(i + 1)/1000.0
            end if
            bus(flusolis + i) = cm%dat(ck%FB)%GRD(i + 1)
            bus(fdsi + i) = cm%dat(ck%FI)%GRD(i + 1)
            bus(tmoins + i) = cm%dat(ck%TT)%GRD(i + 1)
            bus(humoins + i) = cm%dat(ck%HU)%GRD(i + 1)
            bus(umoins + i) = cm%dat(ck%UV)%GRD(i + 1)
            bus(vmoins + i) = 0.0
            bus(pmoins + i) = cm%dat(ck%P0)%GRD(i + 1)
        end do

                call compvirttemp(sigma_t, bus, bussiz)
                if (.not. observed_forcing) call surflayerheight(sigma_u, sigma_t, bus, bussiz)

            !> Initialize parameters and state variables at kount=0.
!            if (kount == 0) then

                !> Initialize surface parameters.
!                call inisoili_svs(bus, bussiz, NG)

                !> Initialize state variables.
!                call runsvs_init(bus, bussiz)

!            end if

            !> Update vegetation parameters as a function of julian day.
            call inicover_svs(bus, bussiz, kount, NG)
            !kount = ic%ts_count

            !> Integrate SVS for one time step.
            call svs(bus, bussiz, bidon, 1, dt, kount, 1, NG, NG, 1)

            !> Write outputs (currently in ASCII format).
!            call write_out_file(date_v, hour_v, bus, bussiz)
!        do i = 0, NG - 1
!            bus(runofftotaf + i) = bus(runofftotaf + i) + bus(runofftot + i)
!            if (hour_v == 0) then
!                write (fid_out, '(i8, a1, i8.8, i5, 10(f10.4))') date_v, '.', hour_v, (i + 1), &
!                    (bus(wdsoil + j*NG + i), j = 0, 6), bus(runofftotaf + i), bus(latflaf + i), bus(drainaf + i)
!            end if
!        end do

        do i = 0, NG - 1
            wb%ROFO(i + 1) = bus(runofftot + i)
            wb%ROFS(i + 1) = bus(latflw + i)
            wb%ROFB(i + 1) = bus(watflow + 6*NG + i)
        end do
        wb%ROF = wb%ROFO + wb%ROFS + wb%ROFB
        wb%PRE = cm%dat(ck%RT)%GRD

            !> Read meteorological forcing data.
            !> Careful: at kount=0 we read data for kount=1 so we skip reading if kount=1.
!            if (date_f < date_v .or. hour_f < hour_v) then
!                call read_met_file(date_v, hour_v, date_f, hour_f, bus, bussiz)
!            end if

!        end do
!        kount = kount + 1

        !> Wrap up.
!        call close_files()

!        stop 'by RUNSVS()'

    end subroutine

end module
