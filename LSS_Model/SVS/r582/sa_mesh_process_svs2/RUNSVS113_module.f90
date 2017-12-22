module RUNSVS113_module

    use RUNSVS113_variables

    implicit none

    private

    public RUNSVS113

    contains

    subroutine RUNSVS113(shd, fls, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_shared_variables
        use model_dates
        use climate_forcing

        use RUNSVS_mod
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

        !> Return if the process is not marked active or if not the head node.
        if (.not. RUNSVS113_flgs%PROCESS_ACTIVE) return


        dt = real(ic%dts)
        kount = ic%ts_count - 1

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


        do k = 0, NG - 1
!
!################
!  Precipitation
!
! Option 1
! Rainfall and snowfall rate are read separetely
               
                bus(rainrate + k) = cm%dat(ck%RR)%GAT(k+1)/1000.0
                bus(snowrate + k) = cm%dat(ck%SR)%GAT(k+1)/1000.0

! Option 2
! Rainfall and snowfall rate are derived from total precipitation rate
! assuming a separation at 0 degC (as in GEM-Hydro)

!            if(cm%dat(ck%TT)%GAT(k + 1) > tcdk) then
!                bus(rainrate + k) = cm%dat(ck%RT)%GAT(k + 1)/1000.0
!                bus(snowrate + k) = 0.0
!            else
!                bus(rainrate + k) = 0.0
!                bus(snowrate + k) = cm%dat(ck%RT)%GAT(k + 1)/1000.0
!            end if
!################
! Other forcing

            bus(flusolis + k) = cm%dat(ck%FB)%GAT(k + 1)
            bus(fdsi + k) = cm%dat(ck%FI)%GAT(k + 1)
            bus(tmoins + k) = cm%dat(ck%TT)%GAT(k + 1)
            bus(humoins + k) = cm%dat(ck%HU)%GAT(k + 1)
            bus(umoins + k) = cm%dat(ck%UV)%GAT(k + 1)
            bus(vmoins + k) = 0.0
            bus(pmoins + k) = cm%dat(ck%P0)%GAT(k + 1)
        end do

      !  write(*,*) 'Forcing',bus(flusolis + k),bus(fdsi + k),bus(tmoins + k),bus(humoins + k), bus(umoins + k)
      !  write(*,*) 'Forcing',bus(rainrate + k),bus(snowrate + k)

        call compvirttemp(sigma_t, bus, bussiz)
        if (.not. observed_forcing) call surflayerheight(sigma_u, sigma_t, bus, bussiz)

        !call subroutine to compute layer thicknesses
        call layer_thickness()

       ! Update vegetation parameters as a function of julian day.
        call inicover_svs(bus, bussiz, kount, NG)

        !> Integrate SVS for one time step.
        call svs(bus, bussiz, bidon, 1, dt, kount, 1, NG, NG, 1)

        !> Transfer variables.
        do k = 0, NG - 1
            stas%cnpy%qac(k + 1) = bus(qsurf + k)
            stas%cnpy%rcan(k + 1) = bus(wveg + k)
            stas%cnpy%tac(k + 1) = bus(tsurf + k)
            stas%cnpy%tcan(k + 1) = (bus(tvege + k) + bus(tvege + NG + k) + bus(tsnowveg + k) + bus(tsnowveg + NG + k))/4.0
            stas%sno%sno(k + 1) = bus(snoma + k)
            stas%sno%albs(k + 1) = (bus(snoal + k) + bus(snval + k))/2.0
            stas%sno%rhos(k + 1) = ((bus(snoro + k) + bus(snvro + k))/2.0)*900.0
            stas%sno%tsno(k + 1) = (bus(tsnow + k) + bus(tsnow + NG + k))/2.0
            if (bus(snoma + k) > 0.0) then
                stas%sno%wsno(k + 1) = bus(wsnow + k)
            else
                stas%sno%wsno(k + 1) = 0.0
            end if

            stas%sfc%evap(k + 1) = bus(fvap + k)
            stas%sfc%qevp(k + 1) = bus(fv + k)
            stas%sfc%hfs(k + 1) = bus(fc + k)
            stas%sfc%rofo(k + 1) = max(0.0, bus(runofftot + k))/ic%dts

!EG_MOD add lateral flow from all layers
            stas%sl%rofs(k + 1) = 0.0
            do j = 0, 6
                stas%sl%rofs(k + 1) = stas%sl%rofs(k + 1) + max(0.0, bus(latflw + j*NG + k))/ic%dts
            end do
            stas%sl%thic(k + 1, 1) = bus(isoil + k)
!-            stas%sl%fzws(k + 1, :) =
            stas%sl%thlq(k + 1, 1) = bus(wsoil + k)
            stas%sl%thlq(k + 1, 2) = bus(wsoil + NG + k)
            do j = 3, shd%lc%IGND
                stas%sl%thlq(k + 1, j) = bus(wsoil + (j-1)*NG + k)
            end do
!-            stas%sl%lqws(k + 1, :) =
            stas%sl%tbar(k + 1, 1) = bus(tsoil + k)
            do j = 2, shd%lc%IGND
                stas%sl%tbar(k + 1, j) = bus(tsoil + NG + k)
            end do
!-            stas%sl%gflx(k + 1, :) =
            stas%lzs%rofb(k + 1) = max(0.0, bus(watflow + 6*NG + k))/ic%dts
        end do


!>>>svs_output
    !> Daily.
    preacc_dly = preacc_dly + sum(bus(rainrate:(rainrate + NG - 1)))*ic%dts + sum(bus(snowrate:(snowrate + NG - 1)))*ic%dts !sum of all 'var' in bus?; depth
    if (ic%ts_daily == 3600*24/ic%dts) then !last time-step of day
  !      write(iout_dly, 1010) ic%now%year, ic%now%jday, &
  !          preacc_dly, & !daily acc.
  !          (preacc_dly/real(ic%ts_daily*ic%dts)) !rate = (value)/seconds in day using ts_daily and dts
        preacc_dly = 0.0 !reset accumulators
    end if


    !> Hourly.
    preacc_hly = preacc_hly + sum(bus(rainrate:(rainrate + NG - 1)))*ic%dts + sum(bus(snowrate:(snowrate + NG - 1)))*ic%dts !sum of all 'var' in bus?; depth
    if (ic%ts_hourly == 3600/ic%dts) then !last time-step of hour

!       write(iout_hly, 1010) ic%now%year, ic%now%jday, ic%now%hour, preacc_hly 
        preacc_hly = 0.0 !reset accumulators
        
        write(iout_hly, 1010) ic%now%year, ic%now%jday, ic%now%hour, bus(snoma), bus(snodpl),bus(snoal) !daily acc.

        write(iout_hly_soil, 1010) ic%now%year, ic%now%jday, ic%now%hour, bus(tsoil), bus(tsoil+1),bus(tsoil+2), &
                        bus(tsoil+3), bus(tsoil+4),bus(tsoil+5),bus(tsoil+6)  !daily acc.

    end if

    !> Time-step.
  !  write(iout_ts, 1010) ic%now%year, ic%now%jday, ic%now%hour, ic%now%mins, &
  !      sum(bus(rainrate:(rainrate + NG - 1))), sum(bus(snowrate:(snowrate + NG - 1)))

1010    format(9999(g15.7e2, ','))
!<<<svs_output


    end subroutine

end module
