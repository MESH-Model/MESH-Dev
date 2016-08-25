module model_output_variabletypes

    !> Description: Type-set to store a single-record of hydrograph
    !>              values for multiple gauges or to store a
    !>              time-series of hydrograph values for a single
    !>              streamflow gauge location.
    type streamflow_hydrograph

        !* name: ID printed to output files.
        !* iy:
        !* qhyd: Observed value.
        !* qsyn: Synthesis.
        character(len=8), dimension(:), allocatable :: name
        integer, dimension(:), allocatable :: iy, jx, n
        real, dimension(:), allocatable :: i, j, qhyd, qsyn

        !* ns: Number of streamflow gauges or records.
        integer ns

    end type

    !> Description: Type-set to store reservoir release, storage, and
    !>              abstraction for multiple reservoirs or of a
    !>              time-series for a single reservoir.
    type reservoir_release

        !* rls: Reservoir release.
        !* store: Storage.
        !* abst: Storage abstracted from the reservoir for demand.
        real, dimension(:), allocatable :: rls
        real, dimension(:), allocatable :: store
        real, dimension(:), allocatable :: abst

        !* nr: Number of reservoirs or records.
        integer nr

    end type

end module
