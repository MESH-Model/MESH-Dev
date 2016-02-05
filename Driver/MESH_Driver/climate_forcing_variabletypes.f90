module climate_forcing_variabletypes

    use model_dates

    implicit none

    type clim_info_series

        integer nattr
        character*20 attrtype
        character*200, dimension(:), allocatable :: attr

    end type

    type clim_info_read

!        type(counter_date_julian) :: start_date
        integer :: timeSize = 1  !minimum size of block read DEFINED IN THE INPUT FILE
        integer :: nseries = 0
        integer, dimension(:), allocatable :: ntimes ! number of time in each block of readed data
        character(20) :: id_var !climate variable name
        integer :: filefmt = 0
        character*200, dimension(:), allocatable :: name
        integer unitR !Number unit
        logical openFl !true if file is open
        integer :: readIndx = 1 !index in the block of time that we are reading
        integer :: itime = 1 !time index
        character freq !time freq of data
        integer :: timestep_now = 0
        integer :: hf = 30 !hourly flag

        !* climv: Values for forcing data. (1: Land Element (GAT); 2: Series; 3: Time-step).
        !> Values are stored at the GAT level, as it is the finest level of
        !> elemental computation in the model (e.g., with CLASS).
        real, dimension(:, :, :), allocatable :: climv

        !* GRD: Values for forcing data. (1: Grid)
        !> Values are averaged to the grid-level for grid-based processing and certain output.
        !> Gridded values aren't used to drive the model, as they are incompatible with
        !> data input at the GRU- (e.g., in one of the CSV formats) or GAT-level.
        real, dimension(:), allocatable :: GRD

        !* GAT: Values for forcing data. (1: Land Element)
        real, dimension(:), allocatable :: GAT

        !* alpha: Uniform weight to assign when there are multiple series of data. (1: Series).
!        real, dimension(:), allocatable :: alpha
        type(clim_info_series), dimension(:), allocatable :: series

    end type !clim_info_read

    type clim_info

        !* nclim: Number of climate variables.
!        integer :: nclim = 7
        integer :: basefileunit = 89
        type(clim_info_read) :: clin(7)
        type(counter_date_julian) :: start_date

    end type !clim_info

end module
