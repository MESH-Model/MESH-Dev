module simstats

use mesh_input_module
use flags

use simstats_nse, only: nse_calc
use simstats_sae, only: sae_calc
use simstats_saesrt, only: saesrt_calc

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
real, dimension(:), allocatable :: mae, rmse, bias, nsd, nsw, tpd, tpw

contains

!include "SAE.F90"
!include "SAESRT.F90"
!include "SAEMSRT.F90"
!include "NSE.F90"

subroutine calc_stats(obs, sim, n, bias, nsd, nsw, tpd)
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
!>       NS         -   Nash-Sutcliffe coefficient
!>       NSLN       -   Nash-Sutcliffe coefficient 
!                       (with the natural logarithm of runoff)
!>=======================================================================

!INCOMING VARIABLES
!    IMPLICIT NONE
integer :: i, j, iw, n, nw
real, intent(in), dimension(:) :: obs, sim
real, allocatable ::  obsw(:), simw(:), errw(:), errwm(:)

!OUTGOING VARIABLES
real :: bias, nsd, nsw, tpd, tpw

!LOCAL VARIABLES
integer :: nad, naw, ipo(1), ips(1)
real :: obsdm, obswm
real :: errd(n), errdm(n), errtp
    
!> Intrinsic Function
intrinsic maxloc

!FOR WEEKLY CALCULATIONS
nw = ceiling(n / 7.0)
allocate(obsw(nw), simw(nw), errw(nw), errwm(nw))

!INITIALIZE OUTPUT AND LOCAL VARIABLES
bias = 0.0
nsd = 0.0
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

!WEEKLY OBSERVED AND SIMULATED VALUES
iw = 0
do i = 1, n, 7
    iw = iw + 1
    j = min(i + 6, n)
    obsw(iw) = sum(obs(i:j))
    simw(iw) = sum(sim(i:j))
end do

!MEAN OF OBSERVED RUNOFF
nad = count(obs(1:n) >= 0.0)
obsdm = sum(obs(1:n), mask = obs(1:n) >= 0.0) / nad

!MEAN OF WEEKLY RUNOFF
naw = count(obsw(1:nw) >= 0.0)
obswm = sum(obsw(1:nw), mask = obsw(1:nw) >= 0.0) / naw

!CALCULATE ERRORS FOR RUNOFF GREATER THAN ZERO - DAILY
where(obs(1:n) >= 0.0)
    errd(1:n) = obs(1:n) - sim(1:n)
    errdm(1:n) = obs(1:n) - obsdm
end where

!CALCULATE ERRORS FOR RUNOFF GREATER THAN ZERO - WEEKLY
where(obsw(1:nw) >= 0.0)
    errw(1:nw) = obsw(1:nw) - simw(1:nw)
    errwm(1:nw) = obsw(1:nw) - obswm
end where

!CALCULATE THE STATISTICAL COEFFICIENTS
bias = sum(errd(1:n)) / (obsdm * nad)
nsd = 1.0 - sum(errd*errd) / sum(errdm*errdm)
nsw = 1.0 - sum(errw*errw) / sum(errwm*errwm)

!TIME TO PEAK - DAILY BASIS
errtp = 0.0
do i = 1, n, 365
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

subroutine stats_init(nyears, ns)

!* nyears: Number of simulation years
!* ns: Number of streamflow gauges
integer :: nyears, ns

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

allocate(qobs(nyears*366, ns), qsim(nyears*366, ns))
allocate(mae(ns), rmse(ns), bias(ns), nsd(ns), nsw(ns), tpd(ns), tpw(ns))

end subroutine

subroutine stats_update_daily(qhyd_avg, qsyn_avg, ncount)

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

subroutine stats_write()

if (autocalibrationflag == 0) return

open(100, file="function_out.txt")
if (preemptionflag >= 1) &
    ftest = ftest*nyears*366 / ncal
write(100, *) ftest
close(100)

do j = 1, size(qobs, 2)
    call calc_stats(qobs(1:ncal, j), qsim(1:ncal, j), ncal, bias(j), nsd(j), nsw(j), tpd(j))
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

open(100, file="NS.txt", status="unknown")
write(100, *) (nsd(j), j = 1, size(qobs, 2))
close(100)

open(100, file="NSW.txt", status="unknown")
write(100, *) (nsw(j), j = 1, size(qobs, 2))
close(100)

end subroutine

end module
