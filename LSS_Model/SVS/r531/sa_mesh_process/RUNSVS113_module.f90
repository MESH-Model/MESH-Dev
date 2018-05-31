module RUNSVS113_module

    use RUNSVS113_variables

    implicit none

    private

    public RUNSVS113

    contains

    subroutine RUNSVS113(shd, fls, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_common
        use model_dates
        use climate_forcing

        use RUNSVS_mod
        use runsvs_utils
!        use runsvs_io

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

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

        !> MPI: Return if running in parallel and not the head node.
        if (.not. (ipid /= 0 .or. izero == 0)) return

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
!                write (fid_out, '(i8, a1, i8.8, i5, 10(f10.4))') date_v, '.', hour_v, (il1 + k), &
!                    (bus(wdsoil + j*NG + k), j = 0, 6), bus(runofftotaf + k), bus(latflaf + k), bus(drainaf + k)
!            end if
!        end do

        !> Transfer variables.
        do k = 0, NG - 1
            vs%tile%rcan(il1 + k) = bus(wveg + k)
!-            vs%tile%sncan(il1 + k) =
!-            vs%tile%cmas(il1 + k) =
            vs%tile%tac(il1 + k) = bus(tsurf + k)
            vs%tile%tcan(il1 + k) = (bus(tvege + k) + bus(tvege + NG + k) + bus(tsnowveg + k) + bus(tsnowveg + NG + k))/4.0
            vs%tile%qac(il1 + k) = bus(qsurf + k)
!-            vs%tile%gro(il1 + k) =
            vs%tile%sno(il1 + k) = bus(snoma + k)
            vs%tile%albs(il1 + k) = (bus(snoal + k) + bus(snval + k))/2.0
!-            vs%tile%fsno(il1 + k) =
            vs%tile%rhos(il1 + k) = ((bus(snoro + k) + bus(snvro + k))/2.0)*900.0
            if (bus(snoma + k) > 0.0) then
                vs%tile%wsno(il1 + k) = bus(wsnow + k)
                vs%tile%tsno(il1 + k) = (bus(tsnow + k) + bus(tsnow + NG + k))/2.0
            else
                vs%tile%wsno(il1 + k) = 0.0
                vs%tile%tsno(il1 + k) = 0.0
            end if
!-            vs%tile%zpnd(il1 + k) =
!-            vs%tile%tpnd(il1 + k) =
!-            vs%tile%pevp(il1 + k) =
            vs%tile%evap(il1 + k) = bus(fvap + k)
            vs%tile%qevp(il1 + k) = bus(fv + k)
            vs%tile%hfs(il1 + k) = bus(fc + k)
            vs%tile%rofo(il1 + k) = max(0.0, bus(runofftot + k))/ic%dts
!-            vs%tile%tsfs(il1 + k, :) =
!-            vs%tile%tbas(il1 + k) =
!EG_MOD add lateral flow from all layers
            vs%tile%rofs(il1 + k) = 0.0
            do j = 0, 6
                vs%tile%rofs(il1 + k) = vs%tile%rofs(il1 + k) + max(0.0, bus(latflw + j*NG + k))/ic%dts
            end do
            vs%tile%thic(il1 + k, 1) = bus(isoil + k)
            vs%tile%thlq(il1 + k, 1) = bus(wdsoil + k)
            vs%tile%thlq(il1 + k, 2) = bus(wdsoil + NG + k)
            do j = 3, shd%lc%IGND
                vs%tile%thlq(il1 + k, j) = bus(wdsoil + j*NG + k)
            end do
            vs%tile%tbar(il1 + k, 1) = bus(tsoil + k)
            do j = 2, shd%lc%IGND
                vs%tile%tbar(il1 + k, j) = bus(tsoil + NG + k)
            end do
!-            vs%tile%gflx(il1 + k, :) =
            vs%tile%rofb(il1 + k) = max(0.0, bus(watflow + 6*NG + k))/ic%dts
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
