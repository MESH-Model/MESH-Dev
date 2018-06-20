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
	if (.not. (ipid /= 0 .or. izero == 0)) return

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
               
!                bus(rainrate + k) = cm%dat(ck%RR)%GAT(il1 + k)/1000.0
!                bus(snowrate + k) = cm%dat(ck%SR)%GAT(il1 + k)/1000.0

! Option 2
! Rainfall and snowfall rate are derived from total precipitation rate
! assuming a separation at 0 degC (as in GEM-Hydro)

            if(cm%dat(ck%TT)%GAT(il1 + k) > tcdk) then
                bus(rainrate + k) = cm%dat(ck%RT)%GAT(il1 + k)/1000.0
                bus(snowrate + k) = 0.0
            else
                bus(rainrate + k) = 0.0
                bus(snowrate + k) = cm%dat(ck%RT)%GAT(il1 + k)/1000.0
            end if
!################
! Other forcing

            bus(flusolis + k) = cm%dat(ck%FB)%GAT(il1 + k)
            bus(fdsi + k) = cm%dat(ck%FI)%GAT(il1 + k)
            bus(tmoins + k) = cm%dat(ck%TT)%GAT(il1 + k)
            bus(humoins + k) = cm%dat(ck%HU)%GAT(il1 + k)
            bus(umoins + k) = cm%dat(ck%UV)%GAT(il1 + k)
            bus(vmoins + k) = 0.0
            bus(pmoins + k) = cm%dat(ck%P0)%GAT(il1 + k)
        end do

      !  k=0
      !  write(*,*) 'Forcing',bus(flusolis + k),bus(fdsi + k),bus(tmoins + k),bus(humoins + k), bus(umoins + k)
      !  write(*,*) 'Forcing',bus(rainrate + k),bus(snowrate + k)

                call compvirttemp(sigma_t, bus, bussiz)
                if (.not. observed_forcing) call surflayerheight(sigma_u, sigma_t, bus, bussiz)

            !call subroutine to compute layer thicknesses
             call layer_thickness()
  
            !> Update vegetation parameters as a function of julian day.
            call inicover_svs(bus, bussiz, kount, NG)


            !> Integrate SVS for one time step.
            call svs(bus, bussiz, bidon, 1, dt, kount, 1, NG, NG, 1)

        !> Transfer variables.

        do k = 0, NG - 1
            stas%cnpy%qac(il1 + k) = bus(qsurf + k)
            stas%cnpy%rcan(il1 + k) = bus(wveg + k)
            stas%cnpy%tac(il1 + k) = bus(tsurf + k)
            stas%cnpy%tcan(il1 + k) = (bus(tvege + k) + bus(tvege + NG + k) + bus(tsnowveg + k) + bus(tsnowveg + NG + k))/4.0
            stas%sno%sno(il1 + k) = bus(snoma + k)
            stas%sno%albs(il1 + k) = (bus(snoal + k) + bus(snval + k))/2.0
            stas%sno%rhos(il1 + k) = ((bus(snoro + k) + bus(snvro + k))/2.0)*900.0
            stas%sno%tsno(il1 + k) = (bus(tsnow + k) + bus(tsnow + NG + k))/2.0
            if (bus(snoma + k) > 0.0) then
                stas%sno%wsno(il1 + k) = bus(wsnow + k)
            else
                stas%sno%wsno(il1 + k) = 0.0
            end if
!-            stas%sfc%pndw(il1 + k) =
            stas%sfc%evap(il1 + k) = bus(wflux + k)
            stas%sfc%qevp(il1 + k) = bus(fv + k)
            stas%sfc%hfs(il1 + k) = bus(fc + k)
            stas%sfc%rofo(il1 + k) = max(0.0, bus(runofftot + k))/ic%dts
!EG_MOD add lateral flow from all layers
            stas%sl%rofs(il1 + k) = 0.0
            do j = 0, 6
                stas%sl%rofs(il1 + k) = stas%sl%rofs(il1 + k) + max(0.0, bus(latflw + j*NG + k))/ic%dts
            end do
            stas%sl%thic(il1 + k, 1) = bus(isoil + k)
!-            stas%sl%fzws(il1 + k, :) =
            stas%sl%thlq(il1 + k, 1) = bus(wsoil + k)
            stas%sl%thlq(il1 + k, 2) = bus(wsoil + NG + k)
            do j = 3, shd%lc%IGND
                stas%sl%thlq(il1 + k, j) = bus(wsoil + (j-1)*NG + k)
            end do
!-            stas%sl%lqws(il1 + k, :) =
            stas%sl%tbar(il1 + k, 1) = bus(tsoil + k)
            do j = 2, shd%lc%IGND
                stas%sl%tbar(il1 + k, j) = bus(tsoil + NG + k)
            end do
!-            stas%sl%gflx(il1 + k, :) =
            stas%lzs%rofb(il1 + k) = max(0.0, bus(watflow + KDP*NG + k))/ic%dts
        end do

!>>>svs_output
    !> Daily.
    preacc_dly = preacc_dly + sum(bus(rainrate:(rainrate + NG - 1)))*ic%dts + sum(bus(snowrate:(snowrate + NG - 1)))*ic%dts !sum of all 'var' in bus?; depth
    if (ic%ts_daily == 3600*24/ic%dts) then !last time-step of day
!        write(iout_dly, 1010) ic%now%year, ic%now%jday, &
!            preacc_dly, & !daily acc.
!            (preacc_dly/real(ic%ts_daily*ic%dts)) !rate = (value)/seconds in day using ts_daily and dts
        preacc_dly = 0.0 !reset accumulators
    end if

    !> Hourly.
    preacc_hly = preacc_hly + sum(bus(rainrate:(rainrate + NG - 1)))*ic%dts + sum(bus(snowrate:(snowrate + NG - 1)))*ic%dts !sum of all 'var' in bus?; depth
    preacc_tot = preacc_tot + 1000.*sum(bus(rainrate:(rainrate + NG - 1)))*ic%dts + 1000.*sum(bus(snowrate:(snowrate + NG - 1)))*ic%dts !sum of all 'var' in bus?; depth
    runoff_acc = runoff_acc + bus(runofftot)

    wsoil_tot = 0.
    isoil_tot = 0.
    call layer_thickness()
    do j = 0, shd%lc%IGND
         wsoil_tot  = wsoil_tot+ 1000.*bus(wsoil + j*NG)*delz(j+1) ! mm
    end do


    if (ic%ts_hourly == 3600/ic%dts) then !last time-step of hour
!        write(iout_hly, 1010) ic%now%year, ic%now%jday, ic%now%hour, preacc_hly !daily acc.
        write(iout_hly, 1010) ic%now%year, ic%now%jday, ic%now%hour, bus(snoma), bus(snodpl),bus(snoal), &
             bus(tsnow), bus(tsnow+1), bus(tsnavg), bus(rainrate), bus(snowrate), bus(wsnow)
 !daily acc.
        preacc_hly = 0.0 !reset accumulators

        write(iout_wat_bal, 1010) ic%now%year, ic%now%jday, ic%now%hour, preacc_tot, bus(accevap),bus(latflaf), &
                        bus(drainaf),runoff_acc,wsoil_tot,isoil_tot,bus(snoma),bus(snvma),bus(wsnow),bus(wsnv),bus(wveg),bus(vegh),bus(vegl)


    end if


   if (kount == 0) then
       wsoil_ini = wsoil_tot
   end if

    bal_in_out = preacc_tot-bus(accevap)-bus(drainaf)-runoff_acc-bus(latflaf)
    stock = (1-bus(vegh))*bus(snoma)+bus(vegh)*bus(snvma)+wsoil_tot-wsoil_ini+isoil_tot+bus(wveg)*(bus(vegl)+bus(vegh))
 
    bal_tot = bal_in_out-stock


 !   if((abs(bal_tot-bal_pre))>0.1) then
 !      write(*,*) 'Inbalance ',bal_tot,bal_tot-bal_pre
 !    end if

    bal_pre = bal_tot



    !> Time-step.
!    write(iout_ts, 1010) ic%now%year, ic%now%jday, ic%now%hour, ic%now%mins, &
!        sum(bus(rainrate:(rainrate + NG - 1))), sum(bus(snowrate:(snowrate + NG - 1)))

1010    format(9999(g15.7e2, ','))
!<<<svs_output


    end subroutine

end module
