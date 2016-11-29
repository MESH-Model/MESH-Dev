module RUNSVS113_module

    use RUNSVS113_variables

    implicit none

    private

    public RUNSVS113

    contains

    subroutine RUNSVS113(shd, fls, ts, cm, wb, eb, sp)

        use mpi_shared_variables
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
!        integer datecmc_o, date_f, hour_f
        integer datecmc_v, date_v, hour_v, istat, kount, bidon
        real(kind = 8) kdt

        integer k, ki, kj, j
        real FRAC

        integer, external :: newdate
        external incdatr
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

            !> Read meteorological forcing data.
            !> Careful: at kount=0 we read data for kount=1 so we skip reading if kount=1.
!            if (kount == 0) then
!                call read_met_file(date_v, hour_v, date_f, hour_f, bus, bussiz)
!            end if

        do k = 0, NG - 1
            if(cm%dat(ck%TT)%GAT(k + 1) > tcdk) then
                bus(rainrate + k) = cm%dat(ck%RT)%GAT(k + 1)/1000.0
                bus(snowrate + k) = 0.0
            else
                bus(rainrate + k) = 0.0
                bus(snowrate + k) = cm%dat(ck%RT)%GAT(k + 1)/1000.0
            end if
            bus(flusolis + k) = cm%dat(ck%FB)%GAT(k + 1)
            bus(fdsi + k) = cm%dat(ck%FI)%GAT(k + 1)
            bus(tmoins + k) = cm%dat(ck%TT)%GAT(k + 1)
            bus(humoins + k) = cm%dat(ck%HU)%GAT(k + 1)
            bus(umoins + k) = cm%dat(ck%UV)%GAT(k + 1)
            bus(vmoins + k) = 0.0
            bus(pmoins + k) = cm%dat(ck%P0)%GAT(k + 1)
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
!        do k = 0, NG - 1
!            bus(runofftotaf + k) = bus(runofftotaf + k) + bus(runofftot + k)
!            if (hour_v == 0) then
!                write (fid_out, '(i8, a1, i8.8, i5, 10(f10.4))') date_v, '.', hour_v, (k + 1), &
!                    (bus(wdsoil + j*NG + k), j = 0, 6), bus(runofftotaf + k), bus(latflaf + k), bus(drainaf + k)
!            end if
!        end do

!-        wb%PRE = 0.0
!-        wb%ROFO = 0.0
!-        wb%ROFS = 0.0
!-        wb%ROFB = 0.0
!-        do k = 0, NG - 1

            !> Grab the Grid, GRU indices for the NML element.
!-            ki = shd%lc%ILMOS(k + 1)
!-            kj = shd%lc%JLMOS(k + 1)

            !> Calculate the contributing FRAC.
!-            FRAC = shd%lc%ACLASS(ki, kj)*shd%FRAC(ki)

            !> Accumulate totals.
!-            wb%PRE(ki) = wb%PRE(ki) + cm%dat(ck%RT)%GRD(ki)*FRAC*ic%dts
!-            if (bus(runofftot + k) > 0.0) wb%ROFO(ki) = wb%ROFO(ki) + bus(runofftot + k)*FRAC
!-            if (bus(latflw + k) > 0.0) wb%ROFS(ki) = wb%ROFS(ki) + bus(latflw + k)*FRAC
!-            if (bus(watflow + 6*NG + k) > 0.0) wb%ROFB(ki) = wb%ROFB(ki) + bus(watflow + 6*NG + k)*FRAC
!-        end do
!-        wb%ROF = wb%ROFO + wb%ROFS + wb%ROFB

        !> Transfer variables.
        do k = 0, NG - 1
            stas%cnpy%qac(k + 1) = bus(qsurf + k)
            stas%cnpy%rcan(k + 1) = bus(wveg + k)
!-            stas%cnpy%sncan(k + 1) =
            stas%cnpy%tac(k + 1) = bus(tsurf + k)
            stas%cnpy%tcan(k + 1) = (bus(tvege + k) + bus(tvege + NG + k) + bus(tsnowveg + k) + bus(tsnowveg + NG + k))/4.0
!-            stas%cnpy%cmai(k + 1) =
!-            stas%cnpy%gro(k + 1) =
!-            stas%cnpy%pevp(k + 1) =
            stas%sno%sno(k + 1) = bus(snoma + k)
            stas%sno%albs(k + 1) = (bus(snoal + k) + bus(snval + k))/2.0
!-            stas%sno%fsno(k + 1) =
            stas%sno%rhos(k + 1) = ((bus(snoro + k) + bus(snvro + k))/2.0)*900.0
            stas%sno%tsno(k + 1) = (bus(tsnow + k) + bus(tsnow + NG + k))/2.0
            if (bus(snoma + k) > 0.0) then
                stas%sno%wsno(k + 1) = bus(wsnow + k)
            else
                stas%sno%wsno(k + 1) = 0.0
            end if
!-            stas%sfc%tpnd(k + 1) =
!-            stas%sfc%zpnd(k + 1) =
!-            stas%sfc%pndw(k + 1) =
            stas%sfc%evap(k + 1) = bus(etr + k)*(86400.0/ic%dts)
            stas%sfc%qevp(k + 1) = bus(fv + k)
            stas%sfc%hfs(k + 1) = bus(fc + k)
            stas%sfc%rofo(k + 1) = max(0.0, bus(runofftot + k))/ic%dts
!-            stas%sfc%tsfs(k + 1, :) =
!-            stas%sl%tbas(k + 1) =
            stas%sl%rofs(k + 1) = max(0.0, bus(latflw + k))/ic%dts
            stas%sl%thic(k + 1, 1) = bus(isoil + k)
!-            stas%sl%fzws(k + 1, :) =
            stas%sl%thlq(k + 1, 1) = bus(wdsoil + k)
            stas%sl%thlq(k + 1, 2) = bus(wdsoil + NG + k)
            do j = 3, shd%lc%IGND
                stas%sl%thlq(k + 1, j) = bus(wdsoil + j*NG + k)
            end do
!-            stas%sl%lqws(k + 1, :) =
            stas%sl%tbar(k + 1, 1) = bus(tsoil + k)
            do j = 2, shd%lc%IGND
                stas%sl%tbar(k + 1, j) = bus(tsoil + NG + k)
            end do
!-            stas%sl%gflx(k + 1, :) =
            stas%lzs%rofb(k + 1) = max(0.0, bus(watflow + 6*NG + k))/ic%dts
        end do

            !> Read meteorological forcing data.
            !> Careful: at kount=0 we read data for kount=1 so we skip reading if kount=1.
!            if (date_f < date_v .or. hour_f < hour_v) then
!                call read_met_file(date_v, hour_v, date_f, hour_f, bus, bussiz)
!            end if

!        end do
!        kount = kount + 1

        !> Wrap up.
!        call close_files()

!        stop 'by RUNSVS113()'

    end subroutine

end module
