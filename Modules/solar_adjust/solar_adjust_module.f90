!> Description:
!>  Calculate theoretical shortwave radiation using method proposed by
!>  Garnier and Ohmura (1970) to adjust for slope and aspect.
!>  Calls "calc_rsrd_adjusted".
!>
!> Author: Zelalem Tesemma (of Solar_Adjust.m; last updated Jan 30, 2018)
!> Converted to Fortran: Feb 1, 2018 (exact)
!> Fortran code optimized/consolidated; subroutine isolated
!>  ('program' component replaced by 'solar_adjust_module';
!>      'solar_adjust_module' coupled with MESH): Feb 2, 2018.
module solar_adjust_module

    implicit none

    !> Description:
    !>  Type for parameters (options).
    !>
    !> Variables:
    !*  Trans: Mean transmissivity of the atmosphere.
    !*  Time_Offset: Solar time offset from local time.
    !*  CalcFreq: Iterations per day (must divide the day into equal increments of minutes). [--].
    type solar_adjust_parameters
        real :: Trans = 0.818
        real :: Time_Offset = 0.67
        integer :: CalcFreq = 288
    end type

    !> Description:
    !>  Type for variables/constants.
    !>
    !> Variables:
    !*  elev: Elevation. [m].
    !*  ylat: Latitude. [degrees].
    !*  slope: Slope of surface. [--].
    !*  aspect: Aspect of surface.
    type solar_adjust_variables
        real, dimension(:), allocatable :: elev, ylat, slope, aspect
    end type

    !> Description:
    !>  Type for 'solar_adjust' parameters and variables.
    type solar_adjust_container
        type(solar_adjust_parameters) pm
        type(solar_adjust_variables) vs
    end type

    !* fsadj: Instance of 'solar_adjust' parameters and variables.
    type(solar_adjust_container), save :: rsrd_adj

    contains

    subroutine solar_adjust_init(fls, shd, cm)

        !> Required for MESH variables.
        use model_files_variables
        use sa_mesh_shared_variables
        use climate_forcing

        !> Required for 'il1', 'il2', and 'iln'.
        !> Because 'il1' and 'il2' are not passed to the subroutine, the
        !> subset of the stride is used here instead.
        use mpi_module

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(clim_info), intent(in) :: cm

        !> Local variables.
        integer k

        !> Allocate variables.
        allocate( &
            rsrd_adj%vs%elev(il1:il2), rsrd_adj%vs%ylat(il1:il2), rsrd_adj%vs%slope(il1:il2), &
            rsrd_adj%vs%aspect(il1:il2))
        rsrd_adj%vs%slope = 0.0
        rsrd_adj%vs%aspect = 0.0

        !> Assign values.
        do k = il1, il2
            rsrd_adj%vs%elev(k) = shd%ELEV(shd%lc%ILMOS(k))
            rsrd_adj%vs%ylat(k) = shd%YLAT(shd%lc%ILMOS(k))
        end do

        !> Write summary to file.

    end subroutine

    subroutine solar_adjust_within_tile(fls, shd, cm)

        !> Required for MESH variables.
        use model_files_variables
        use sa_mesh_shared_variables
        use climate_forcing

        !> Required for 'il1', 'il2', and 'iln'.
        !> Because 'il1' and 'il2' are not passed to subroutine, the
        !> subset of the stride is used here instead.
        use mpi_module

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd

        !> Input/output variables.
        type(clim_info) :: cm

        !> Local variables.
        real, dimension(il1:il2) :: rsrd_direct, rsrd_diffuse, rsrd_adjusted

        !> Call routine to calculate adjusted radiation value.
        call calc_rsrd_adjusted( &
            rsrd_adj%vs%elev(il1:il2), rsrd_adj%vs%ylat(il1:il2), &
            rsrd_adj%vs%slope(il1:il2), rsrd_adj%vs%aspect(il1:il2), iln, &
            rsrd_adj%pm%Trans, rsrd_adj%pm%Time_Offset, rsrd_adj%pm%CalcFreq, &
            cm%dat(ck%FB)%hf, &
            cm%dat(ck%FB)%GAT(il1:il2), &
            rsrd_direct(il1:il2), rsrd_diffuse(il1:il2), rsrd_adjusted(il1:il2), &
            ic%now%year, ic%now%jday, ic%now%hour)

        !> Update radiation.
        cm%dat(ck%FB)%GAT(il1:il2) = rsrd_adjusted(il1:il2)

        !> Update output variables.

    end subroutine

end module
