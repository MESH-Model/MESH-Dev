module model_files_variables

    implicit none

    type model_file_keys

        !> MESH_input_run_options.ini
        integer :: f53 = 1

        !> MESH_drainage_database.r2c
        integer :: f20 = 11

        !> MESH_parameters_CLASS.ini
        integer :: f50 = 2

        !> MESH_parameters_hydrology.ini
        integer :: f23 = 3

        !> MESH_input_soil_levels.txt
        integer :: f52 = 10

        !> MESH_ggeo.ini
        integer :: f18 = 7

        !> WR_runoff.r2c
        integer :: f31 = 12

        !> WR_recharge.r2c
        integer :: f32 = 13

        !> Basin_average_water_balance.csv
        integer :: f900 = 4

        !> MESH_output_streamflow.csv
        integer :: f70 = 6

        !> out_response
        integer :: out_response = 8

!        integer :: MetricsSummaryOut = 5

        !> int_statVariables.seq
        integer :: f883 = 9

    end type

    type(model_file_keys), save :: mfk

end module
