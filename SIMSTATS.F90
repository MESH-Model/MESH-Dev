module simstats
!>******************************************************************************
!> Description:     
!>    
!>******************************************************************************    

use mesh_input_module
use flags
use model_files
use model_dates

use simstats_nse, only: nse_calc
use simstats_sae, only: sae_calc
use simstats_saesrt, only: saesrt_calc
use calc_drms
use calc_abserr

implicit none

private
public stats_init, stats_update_daily, stats_write, &
    fbest, ftest

!integer, parameter :: dp=kind(0.d0)

!> MAM - FOR AUTOCALIBRATION USING PRE-EMPTION - A MAXIMUM OF 1 YEAR (365 DAYS)
!> DAILY STREAM FLOW IS SUPPOSED TO BE USED FOR AUTOCALIBRATION PURPOSE.
!* NCAL:    ACTUAL NUMBER OF CALIBRATION DATA
!* COUNTER: COUNTER FOR THE NUMBER OF PRE-EMPTION STARTS
!* EXISTS:  LOGICAL TO CHECK IF "pre_emption_value.txt" FILE EXISTS
!* SAE   :  SUM OF ABSOLUTE VALUE OF ERRORS (DEVIATIONS BETWEEN OBSERVED
!*          AND SIMULATED STREAM FLOWS)
!* SAESRT:  SUM OF ABSOLUTE VALUE OF ERRORS (DEVIATIONS BETWEEN SORTED OBSERVED
!*          AND SORTED SIMULATED STREAM FLOWS)
!* NSE   : MEAN NASH-SUTCLIFFE EFFIECIENCY INDEX OF DAILY FLOWS
!* FBEST:  SAE AT PREVIOUS TIME STEP TRIAL
!* FTEST:  SAE AT CURRENT TIME STEP TRIAL
!* QOBS  :  OBSERVED DAILY STREAM FLOW
!* QSIM  :  SIMULATED DAILY STREAM FLOW

integer :: ncal, j
logical :: exists
real :: fbest, ftest
real, dimension(:, :), allocatable :: qobs, qsim

!STATISTICS FOR MONTE CARLO SIMULATION
real, dimension(:), allocatable :: bias, nsd, lnsd, nsw, tpd, tpw

type(model_output_drms) :: st_drms
type(model_output_abserr) :: st_abserr

contains

!include "SAE.F90"
!include "SAESRT.F90"
!include "SAEMSRT.F90"
!include "NSE.F90"

subroutine calc_stats(obs, sim, n, bias, nsd, lnsd, nsw, tpd)
    !>
    !>       January 25, 2012 - M.A. Mekonnen
    !>=======================================================================
    !>
    !>       The function computes model efficiency coefficients.
    !>
    !>=======================================================================
    !>
    !>       OBS        -   Observed values 
    !>       SIM        -   Simulated values 
    !>       N          -   Number of days
    !>       NS         -   Number of stations
    !>       NMIN       -   Minimum number of days for model spin-up
    !>
    !>       SAE        -   Absolute value error
    !>       MAE        -   Mean absolute value error
    !>       RMSE       -   Root mean squared error
    !>       BIAS       -   
    !>       NSD        -   Nash-Sutcliffe coefficient
    !>       LNSD       -   Nash-Sutcliffe coefficient 
    !                       (with the natural logarithm of runoff)
    !>=======================================================================

    !INCOMING VARIABLES
    !    IMPLICIT NONE
    integer :: i, j, iw, n, nw
    real, intent(in), dimension(:) :: obs, sim
    real, allocatable ::  obsw(:), simw(:), errw(:), errwm(:)
    integer :: ilf !number of day left out in the calculation of metrics

    !OUTGOING VARIABLES
    real :: bias, nsd, lnsd, nsw, tpd, tp

    !LOCAL VARIABLES
    integer :: nad, naw, ipo(1), ips(1)
    real :: ltol
    real :: obsdm, obswm, lobsdm
    real :: errd(n), errdm(n), errtp
    real :: lerrd(n), lerrdm(n)

    !> Intrinsic Function
    intrinsic maxloc

    !FOR WEEKLY CALCULATIONS
    nw = ceiling(n / 7.0)
    allocate(obsw(nw), simw(nw), errw(nw), errwm(nw))

    !> Tolerance for low-flow values.
    ltol = 0.0001

    !Day left out in the calculation
    if (METRICSINCLUDESPINUP == 1) then
        ilf = 1
    else
        ilf = METRICSSPINUP
    end if

    !INITIALIZE OUTPUT AND LOCAL VARIABLES
    bias = 0.0
    nsd = 0.0
    lnsd = 0.0
    nsw = 0.0
    tpd = 0.0
    tpw  = 0.0
    obsdm = 0.0
    obsw = 0.0
    simw = 0.0
    obswm = 0.0
    errd = 0.0
    errdm = 0.0
    errw  = 0.0
    errwm = 0.0
    errtp = 0
    lerrd = 0.0
    lerrdm = 0.0

    !WEEKLY OBSERVED AND SIMULATED VALUES
    iw = 0
    do i = ilf, ncal, 7
        iw = iw + 1
        j = min(i + 6, n)
        obsw(iw) = sum(obs(i:j))
        simw(iw) = sum(sim(i:j))
    end do

    !MEAN OF OBSERVED RUNOFF
    nad    = count(obs(ilf:ncal) >= 0.0)
    obsdm  = sum(obs(ilf:ncal), mask = obs(ilf:ncal) >= 0.0) / nad
    lobsdm = sum(log(obs(ilf:ncal) + ltol), mask = obs(ilf:ncal) >= 0.0) / nad

    !MEAN OF WEEKLY RUNOFF
    naw = count(obsw(1:nw) >= 0.0)
    obswm  = sum(obsw(1:nw), mask = obsw(1:nw) >= 0.0) / naw

    !CALCULATE ERRORS FOR RUNOFF GREATER THAN ZERO - DAILY
    where(obs(ilf:ncal) >= 0.0)
        errd(ilf:ncal)   = obs(ilf:ncal) - sim(ilf:ncal)
        errdm(ilf:ncal)  = obs(ilf:ncal) - obsdm
        lerrd(ilf:ncal)  = log(obs(ilf:ncal) + ltol) - log(sim(ilf:ncal) + ltol)
        lerrdm(ilf:ncal) = log(obs(ilf:ncal) + ltol) - lobsdm
    end where

    !CALCULATE ERRORS FOR RUNOFF GREATER THAN ZERO - WEEKLY
    where(obsw(1:nw) >= 0.0)
        errw(1:nw) = obsw(1:nw) - simw(1:nw)
        errwm(1:nw) = obsw(1:nw) - obswm
    end where

    !CALCULATE THE STATISTICAL COEFFICIENTS
    bias = sum(errd(ilf:ncal)) / (obsdm * nad)
    nsd  = 1.0 - sum(errd**2) / sum(errdm**2)
    lnsd = 1.0 - sum(lerrd**2) / sum(lerrdm**2)
    nsw  = 1.0 - sum(errw**2) / sum(errwm**2)

    !TIME TO PEAK - DAILY BASIS
    errtp = 0.0
    do i = ilf, ncal, 365
        j = min(i+364, n)
        if(obs(max(i, j-182)) > 0.0) then
            ipo = maxloc(obs(i:j))
            ips = maxloc(sim(i:j))
            errtp = ipo(1) - ips(1)
        else
            errtp = 0.0
        end if
        tpd = tpd + abs(errtp)
    end do

    !TIME TO PEAK - WEAKLY BASIS
    !ERRTP = 0
    !DO I = 1,NW, 52
    !   J = MIN(I+51,NW)
    !   IF(OBSW(MAX(I, J-25)) > 0.0)THEN
    !      IPO = MAXLOC(OBSW(I:J))
    !      IPS = MAXLOC(SIMW(I:J))
    !      ERRTP  = IPO(1) - IPS(1)
    !   ELSE
    !      ERRTP = 0
    !   ENDIF
    !   TPW = TPW + ABS(ERRTP)
    !ENDDO

end subroutine

subroutine stats_init(ts, ns)
!>******************************************************************************
!> Description:     
!>    
!>******************************************************************************    

    !* nyears: Number of simulation years
    !* ns: Number of streamflow gauges
    integer          , intent(in) :: ns
    type(dates_model), intent(in) :: ts

    if (autocalibrationflag == 0) return

    !+todo replace unit number with variable
    write(6, *) "================================================="
    write(6, *)
    write(6, *) "     SA_MESH IS RUNNING IN AUTOCALIBRATION MODE"

    !+todo separate out pre-emption
    if (preemptionflag >= 1) write(6, *) "                USING PRE-EMPTION"

    write(6, *)
    write(6, *) "================================================="
    write(6, *)

    !+todo split into stats_read()
    if (preemptionflag >= 1) then
        inquire(file="pre_emption_value.txt", exist = exists)
        if (exists) then
            open(100, file="pre_emption_value.txt")
            read(100, *) fbest
            close(100)
        else
            fbest = +1.0e+10
        end if
    end if

    ncal = 0

    allocate(qobs(ts%nr_days, ns), qsim(ts%nr_days, ns))
    allocate(bias(ns), nsd(ns), lnsd(ns), nsw(ns), tpd(ns), tpw(ns))
    
    qobs = 0.0
    qsim = 0.0

end subroutine

subroutine stats_update_daily(qhyd_avg, qsyn_avg, ncount)
!>******************************************************************************
!> Description:     
!>    
!>******************************************************************************    
    
    integer, intent(in) :: ncount
    real, dimension(:), intent(in) :: qhyd_avg, qsyn_avg

    if (autocalibrationflag == 0) return

    ncal = ncal + 1
    do j = 1, size(qobs, 2)
        qobs(ncal, :) = qhyd_avg(:)
        qsim(ncal, :) = qsyn_avg(:) / ncount
    end do

    if (objfnflag == 0) then
        ftest = sae_calc(qobs(1:ncal, :), qsim(1:ncal, :), ncal, size(qobs, 2), autocalibrationflag)
    elseif (objfnflag == 1) then
        ftest = saesrt_calc(qobs(1:ncal, :), qsim(1:ncal, :), ncal, size(qobs, 2), autocalibrationflag)
    elseif (objfnflag == 2) then
        write(6, *) "THE SAEMSRT (OBJECTIVE FUNCTION = 2) IS NOT ", &
            "CURRENTLY FUNCTIONAL FOR PRE-EMPTION CASE"
    elseif (objfnflag == 3) then
        write(6, *) "THE NSE (OBJECTIVE FUNCTION = 3) IS NOT ", &
            "CURRENTLY FUNCTIONAL FOR PRE-EMPTION CASE" , &
            "TRY NEGNSE (OBJECTIVE FUNCTION = 4)"
    elseif (objfnflag == 4) then
        ftest = nse_calc(qobs(1:ncal, :), qsim(1:ncal, :), ncal, size(qobs, 2), autocalibrationflag)
        ftest = -1.0 * ftest
    end if
    
end subroutine

subroutine stats_write(fls)
!>******************************************************************************
!> Description:     
!>    
!>******************************************************************************
   
    type(fl_ids)::fls

    if (autocalibrationflag == 0) return

    open(100, file="function_out.txt")
    if (preemptionflag >= 1) &
        ftest = ftest*nyears*366 / ncal
    write(100, *) ftest
    close(100)

    do j = 1, size(qobs, 2)
        call calc_stats(qobs(1:ncal, j) , &
                        qsim(1:ncal, j) , &
                        ncal            , &
                        bias(j)         , &
                        nsd(j)          , &
                        lnsd(j)         , &
                        nsw(j)          , &
                        tpd(j)          )
    end do

    inquire(file="MonteCarlo.txt", exist=exists)
    if (exists) then
        open(100, file="MonteCarlo.txt", position="append", status="old")
    else
        open(100, file="MonteCarlo.txt", status="unknown")
        write(100, *) "BIAS ", "NSD ", "NSW ", "TPD "
    end if

    write(100, *) (bias(j), nsd(j), nsw(j), int(tpd(j)), j = 1, size(qobs, 2))
    close(100)

    !> Write Nash-Sutcliffe coefficient.
    !todo: there's probably a better way to store a set of multiple statistics in one file.
    open(100, file="NS.txt", status="unknown")
    write(100, *) (nsd(j), j = 1, size(qobs, 2)), sum(nsd)/size(qobs, 2)
    close(100)

    !> Write weekly Nash-Sutcliffe coefficient.
    open(100, file="NSW.txt", status="unknown")
    write(100, *) (nsw(j), j = 1, size(qobs, 2))
    close(100)

    !> Write daily root mean squared error
    st_drms = calc_drms_value(0, ncal, qobs, qsim)
    open(100, file="drms.txt", status="unknown")
    write(100, *) st_drms%value_gauge, st_drms%value_gauge_avg
    close(100)

    !> Write mean absolute error
    st_abserr = calc_abserr_value(0, ncal, qobs, qsim)
    open(100, file="abserr.txt", status="unknown")
    write(100, *) st_abserr%value_gauge, st_abserr%value_gauge_avg
    close(100)

    if ((VARIABLEFILESFLAG .eq. 1) .and. (fls%fl(5)%isInit)) then
       open(fls%fl(5)%unit, file=trim(adjustl(fls%fl(5)%name)))
    else   
       open(100, file='Metrics_Out.txt')
    endif

    write(100, *) "MAE ", "RMSE ", "BIAS ", "NSD ", "lnNSD ", "TPD "
    write(100, *) (st_abserr%value_gauge(j), st_drms%value_gauge(j), bias(j), &
                   nsd(j), lnsd(j), int(tpd(j)), j = 1, size(qobs, 2))
    close(100)

end subroutine

end module
