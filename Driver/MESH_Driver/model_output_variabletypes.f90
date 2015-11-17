module model_output_variabletypes

    !> Description: Type-set to store a single-record of hydrograph
    !>              values for multiple gauges or to store a
    !>              time-series of hydrograph values for a single
    !>              streamflow gauge location.
    type streamflow_hydrograph

        !* qhyd: Observed value.
        !* qsyn: Synthesis.
        real, dimension(:), allocatable :: qhyd
        real, dimension(:), allocatable :: qsyn

        !* ns: Number of streamflow gauges or records.
        integer ns

    end type

end module
