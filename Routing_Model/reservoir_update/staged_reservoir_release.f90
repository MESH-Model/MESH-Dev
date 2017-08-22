!>
subroutine staged_reservoir_release(iyear, iday, rank, ireach, qi1, dt, qo2, store2)

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

    call FIND_MONTH(iday, iyear, imonth)
    call get_reservoir(rank, irsv)
    call compute_reservoir(resrvs%rsvr(irsv), qi1, 2, dt, imonth)
    qo2 = resrvs%rsvr(irsv)%flowSIM(2)
    store2 = resrvs%rsvr(irsv)%stoSIM(2)

end subroutine
