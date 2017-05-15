!> Adapted from:
!>  LISFLOOD - Distributed Water Balance and Flood Simulation Model -
!>      Revised User Manual 2013
!>
!> LISFLOOD:
!>  Van Der Knijff, J. M., J. Younis, and A. P. J. De Roo.
!>      "LISFLOOD: a GIS-based distributed model for river basin scale
!>      water balance and flood simulation." International Journal of
!>      Geographical Information Science 24.2 (2010): 189-212.
!>      http://dx.doi.org/10.1080/13658810802549154
!> Detail:
!>  Burek, Peter. "LISFLOOD, distributed water balance and flood
!>      simulation model revised user manual 2013." (2013).
!>      http://dx.publications.europa.eu/10.2788/24719
!>
subroutine RUNLISFLOOD_simulateReservoirs(iyear, iday, rank, ireach, qi1, dt, qo2, store2)

    use reservoir
    use EF_MODULE

    implicit none

    !> Input variables.
    !*  rank: Rank of the cell. [--].
    !*  ireach: ID of the reservoir/reach outlet. [--].
    !*  qi1: Inflow at the beginning of the current time-step. [m3 s-1].
    integer, intent(in) :: iyear, iday, rank, ireach
    real, intent(in) :: qi1
    real, intent(in) :: dt

    !> Output variables.
    !*  qo2: Outflow from the reservoir at the end of the time-step. [m3 s-1].
    !*  store2: Storage in the reservoir at the end of the time-step. [m3].
    real, intent(out) :: qo2, store2

    !> Local variables.
    integer irsv, imonth

!            Reservoirs are simulated as points in the channel network. The inflow into each
!            reservoir equals the channel flow upstream of the reservoir. The outflow
!            behaviour is described by a number of parameters. First, each reservoir has a
!            total storage capacity S [m3]. The relative filling of a reservoir, F, is a fraction
!            between 0 and 1. There are three ‘special’ filling levels. First, each reservoir has a
!            ‘dead storage’ fraction, since reservoirs never empty completely. The
!            corresponding filling fraction is the ‘conservative storage limit’, Lc. For safety
!            reasons a reservoir is never filled to the full storage capacity. The ‘flood storage
!            limit’ (Lf) represents this maximum allowed storage fraction. The buffering
!            capacity of a reservoir is the storage available between the ‘flood storage limit’
!            and the ‘normal storage limit’ (Ln). Three additional parameters define the way
!            the outflow of a reservoir is regulated. For e.g. ecological reasons each reservoir
!            has a ‘minimum outflow’ (Omin, [m3 s-1]). For high discharge situations, the ‘nondamaging
!            outflow’ (Ond, [m3 s-1]) is the maximum possible outflow that will not
!            cause problems downstream. The ‘normal outflow’ (Onorm, [m3 s-1]) is valid once
!            the reservoir reaches its ‘normal storage’ filling level.
!                where:
!                    S: Reservoir storage capacity [m3]
!                    F: Reservoir fill (fraction, 1 at total storage capacity) [-]
!                    Lc: Conservative storage limit [-]
!                    Ln: Normal storage limit [-]
!                    Lf: Flood storage limit [-]
!                    Omin: Minimum outflow [m3 s-1]
!                    Onorm: Normal outflow [m3 s-1]
!                    Ond: Non-damaging outflow [m3 s-1]
!                    Ires: Reservoir inflow [m3 s-1]
!            In order to prevent numerical problems, the reservoir outflow is calculated using a
!            user-defined time interval (or dt, if it is smaller than this value).

    call FIND_MONTH(iday, iyear, imonth)
    call get_reservoir(rank, irsv)
    call compute_reservoir(resrvs%rsvr(irsv), qi1, 2, dt, imonth)
    qo2 = resrvs%rsvr(irsv)%flowSIM(2)
    store2 = resrvs%rsvr(irsv)%stoSIM(2)

end subroutine
