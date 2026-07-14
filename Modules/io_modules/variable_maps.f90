!> Description:
!>  Module that contains model variable mapping and routines.
module variable_maps

    implicit none

    contains

    subroutine model_variable_activate_from_name(group, variable_name, quiet, variable_assigned, error_status)

        !> Modules.
        use print_routines
        use strings, only: uppercase
        use variable_names
        use model_variables, only: model_variables_fields

        !> Input/output variables.
        type(model_variables_fields), pointer :: group
        character(len = *), intent(in) :: variable_name
        logical, intent(in), optional :: quiet
        logical, intent(out), optional :: variable_assigned
        integer, intent(out) :: error_status

        !> Local variables.
        logical v

        !> Check verbosity.
        v = .true.
        if (present(quiet)) v = .not. quiet

        !> Check for errors.
        if (present(variable_assigned)) variable_assigned = .false.
        if (.not. associated(group)) then

            !> Check if the group is associated.
            if (v) call print_error("The variable group to activate the '" // trim(variable_name) // "' variable is not active.")
            error_status = 1
        else if (group%dim_length == 0) then

            !> Print an error and return if the 'dim_length' has not been set for the variable group.
            if (v) call print_error( &
                "The dimensions of the variable group are not defined to activate the '" // trim(variable_name) // "' variable.")
            error_status = 1
        else

            !> Return status.
            error_status = 0
        end if

        !> Activate and initialize variable.
        if (present(variable_assigned)) variable_assigned = .true.
        select case (uppercase(variable_name))

            !> Basin attributes (general).
            case (VN_NEXT)
                if (.not. allocated(group%next_id)) then
                    allocate(group%next_id(group%dim_length))
                    group%next_id = huge(group%next_id)
                end if
            case (VN_GRIDAREA)
                if (.not. allocated(group%surface_area)) then
                    allocate(group%surface_area(group%dim_length))
                    group%surface_area = huge(group%surface_area)
                end if
            case (VN_ELEV)
                if (.not. allocated(group%topo_elev)) then
                    allocate(group%topo_elev(group%dim_length))
                    group%topo_elev = huge(group%topo_elev)
                end if
            case (VN_TOPOSLOPE, 'INTSLOPE')
                if (.not. allocated(group%topo_slope)) then
                    allocate(group%topo_slope(group%dim_length))
                    group%topo_slope = huge(group%topo_slope)
                end if

            !> Drainage/routing attributes.
            case (VN_CHNLSLOPE)
                if (.not. allocated(group%chnl_slope)) then
                    allocate(group%chnl_slope(group%dim_length))
                    group%chnl_slope = huge(group%chnl_slope)
                end if
            case (VN_CHNLLENGTH)
                if (.not. allocated(group%chnl_length)) then
                    allocate(group%chnl_length(group%dim_length))
                    group%chnl_length = huge(group%chnl_length)
                end if
            case (VN_ICHNL, 'CHNL')
                if (.not. allocated(group%ichnl)) then
                    allocate(group%ichnl(group%dim_length))
                    group%ichnl = huge(group%ichnl)
                end if
            case (VN_IREACH, 'REACH')
                if (.not. allocated(group%ireach)) then
                    allocate(group%ireach(group%dim_length))
                    group%ireach = huge(group%ireach)
                end if
            case (VN_DA)
                if (.not. allocated(group%drainage_area)) then
                    allocate(group%drainage_area(group%dim_length))
                    group%drainage_area = huge(group%drainage_area)
                end if
            case (VN_BNKFLL, 'BANKFULL')
                if (.not. allocated(group%bankfull)) then
                    allocate(group%bankfull(group%dim_length))
                    group%bankfull = huge(group%bankfull)
                end if

            !> Meteorology/climatology variables.
            case (VN_FSIN)
                if (.not. allocated(group%fsin)) then
                    allocate(group%fsin(group%dim_length))
                    group%fsin = huge(group%fsin)
                end if
            case (VN_FLIN)
                if (.not. allocated(group%flin)) then
                    allocate(group%flin(group%dim_length))
                    group%flin = huge(group%flin)
                end if
            case (VN_TA)
                if (.not. allocated(group%ta)) then
                    allocate(group%ta(group%dim_length))
                    group%ta = huge(group%ta)
                end if
            case (VN_QA)
                if (.not. allocated(group%qa)) then
                    allocate(group%qa(group%dim_length))
                    group%qa = huge(group%qa)
                end if
            case (VN_PRES)
                if (.not. allocated(group%pres)) then
                    allocate(group%pres(group%dim_length))
                    group%pres = huge(group%pres)
                end if
            case (VN_UU)
                if (.not. allocated(group%uu)) then
                    allocate(group%uu(group%dim_length))
                    group%uu = huge(group%uu)
                end if
            case (VN_VV)
                if (.not. allocated(group%vv)) then
                    allocate(group%vv(group%dim_length))
                    group%vv = huge(group%vv)
                end if
            case (VN_UV)
                if (.not. allocated(group%uv)) then
                    allocate(group%uv(group%dim_length))
                    group%uv = huge(group%uv)
                end if
            case (VN_WDIR)
                if (.not. allocated(group%wdir)) then
                    allocate(group%wdir(group%dim_length))
                    group%wdir = huge(group%wdir)
                end if
            case (VN_PRERN)
                if (.not. allocated(group%prern)) then
                    allocate(group%prern(group%dim_length))
                    group%prern = huge(group%prern)
                end if
            case (VN_PRESNO)
                if (.not. allocated(group%presno)) then
                    allocate(group%presno(group%dim_length))
                    group%presno = huge(group%presno)
                end if
            case (VN_PRE)
                if (.not. allocated(group%pre)) then
                    allocate(group%pre(group%dim_length))
                    group%pre = huge(group%pre)
                end if

            !> Groundwater/lower zone storage variables.
            case (VN_RCHG)
                if (.not. allocated(group%rchg)) then
                    allocate(group%rchg(group%dim_length))
                    group%rchg = huge(group%rchg)
                end if

            !> Routing variables.
            case (VN_RFF)
                if (.not. allocated(group%rff)) then
                    allocate(group%rff(group%dim_length))
                    group%rff = huge(group%rff)
                end if

            !> Others (not saved).
            case default
                if (present(variable_assigned)) variable_assigned = .false.
        end select

    end subroutine

    subroutine model_variable_activate_from_list(group, variable_names, quiet, variables_assigned, error_status)

        !> Modules.
        use model_variables, only: model_variables_fields

        !> Input/output variables.
        type(model_variables_fields), pointer :: group
        character(len = *), dimension(:), intent(in) :: variable_names
        logical, intent(in), optional :: quiet
        logical, dimension(size(variable_names)), intent(out), optional :: variables_assigned
        integer, intent(out) :: error_status

        !> Local variables.
        integer z, i

        !> Initiate the return status.
        error_status = 0

        !> Activate and initialize variables.
        do i = 1, size(variable_names)
            call model_variable_activate_from_name(group, variable_names(i), quiet, variables_assigned(i), z)
            if (z /= 0) error_status = z
        end do

    end subroutine

    subroutine model_variable_dim_name_by_level(variable_name, dim_names)

        !> Modules.
        use strings, only: uppercase
        use variable_names
        use mesh_io_constants

        !> Input/output variables.
        character(len = *), intent(in) :: variable_name
        character(len = *), dimension(:), allocatable :: dim_names

        !> Allocate unallocated array.
        if (.not. allocated(dim_names)) allocate(dim_names(1))

        !> Identify and assign 'dim_names' by variable.
        select case (uppercase(variable_name))

            !> Basin attributes (general).
            case (VN_NEXT)
                dim_names = (/DIM_NAME_CELL/)
            case (VN_GRIDAREA)
                dim_names = (/DIM_NAME_CELL/)
            case (VN_ELEV)
                dim_names = (/DIM_NAME_CELL/)
            case (VN_TOPOSLOPE, 'INTSLOPE')
                dim_names = (/DIM_NAME_CELL/)

            !> Drainage/routing attributes.
            case (VN_CHNLSLOPE)
                dim_names = (/DIM_NAME_CELL/)
            case (VN_CHNLLENGTH)
                dim_names = (/DIM_NAME_CELL/)
            case (VN_ICHNL, 'CHNL')
                dim_names = (/DIM_NAME_CELL/)
            case (VN_IREACH, 'REACH')
                dim_names = (/DIM_NAME_CELL/)
            case (VN_DA)
                dim_names = (/DIM_NAME_CELL/)
            case (VN_BNKFLL, 'BANKFULL')
                dim_names = (/DIM_NAME_CELL/)

            !> Meteorology/climatology variables.
            case (VN_FSIN)
                dim_names = (/DIM_NAME_GRU/)
            case (VN_FLIN)
                dim_names = (/DIM_NAME_GRU/)
            case (VN_TA)
                dim_names = (/DIM_NAME_GRU/)
            case (VN_QA)
                dim_names = (/DIM_NAME_GRU/)
            case (VN_PRES)
                dim_names = (/DIM_NAME_GRU/)
            case (VN_UU)
                dim_names = (/DIM_NAME_GRU/)
            case (VN_VV)
                dim_names = (/DIM_NAME_GRU/)
            case (VN_UV)
                dim_names = (/DIM_NAME_GRU/)
            case (VN_WDIR)
                dim_names = (/DIM_NAME_GRU/)
            case (VN_PRERN)
                dim_names = (/DIM_NAME_GRU/)
            case (VN_PRESNO)
                dim_names = (/DIM_NAME_GRU/)
            case (VN_PRE)
                dim_names = (/DIM_NAME_GRU/)

            !> Groundwater/lower zone storage variables.
            case (VN_RCHG)
                dim_names = (/DIM_NAME_CELL/)

            !> Routing variables.
            case (VN_RFF)
                dim_names = (/DIM_NAME_CELL/)

            !> Others (not saved).
            case default
                dim_names = (/DIM_NAME_BASIN/)
        end select

    end subroutine

    subroutine model_variable_assign_mapped_field_from_name( &
        group, model_variable, variable_name, quiet, variable_assigned, error_status)

        !> Modules.
        use print_routines
        use strings, only: uppercase
        use variable_names
        use model_variables, only: model_variables_fields
        use field_utilities, only: assign_field

        !> Input/output variables.
        type(model_variables_fields), pointer :: group
        class(*), intent(in) :: model_variable
        character(len = *), intent(in) :: variable_name
        logical, intent(in), optional :: quiet
        logical, intent(out), optional :: variable_assigned
        integer, intent(out) :: error_status

        !> Local variables.
        logical v

        !> Check verbosity.
        v = .true.
        if (present(quiet)) v = .not. quiet

        !> Check for errors.
        if (present(variable_assigned)) variable_assigned = .false.
        if (.not. associated(group)) then

            !> Check if the group is associated.
            if (v) call print_error("The variable group to activate the '" // trim(variable_name) // "' variable is not active.")
            error_status = 1
        else if (group%dim_length == 0) then

            !> Print an error and return if the 'dim_length' has not been set for the variable group.
            if (v) call print_error( &
                "The dimensions of the variable group are not defined to activate the '" // trim(variable_name) // "' variable.")
            error_status = 1
        else

            !> Return status.
            error_status = 0
        end if

        !> Activate and initialize variable.
        if (present(variable_assigned)) variable_assigned = .true.
        select case (uppercase(variable_name))

            !> Basin attributes (general).
            case (VN_NEXT)
                if (.not. allocated(group%next_id)) then
                    call model_variable_activate_from_name(group, variable_name, quiet, variable_assigned, error_status)
                end if
                if (error_status == 0) call assign_field(model_variable, group%next_id, error_status)
            case (VN_GRIDAREA)
                if (.not. allocated(group%surface_area)) then
                    call model_variable_activate_from_name(group, variable_name, quiet, variable_assigned, error_status)
                end if
                if (error_status == 0) call assign_field(model_variable, group%surface_area, error_status)
            case (VN_ELEV)
                if (.not. allocated(group%topo_elev)) then
                    call model_variable_activate_from_name(group, variable_name, quiet, variable_assigned, error_status)
                end if
                if (error_status == 0) call assign_field(model_variable, group%topo_elev, error_status)
            case (VN_TOPOSLOPE, 'INTSLOPE')
                if (.not. allocated(group%topo_slope)) then
                    call model_variable_activate_from_name(group, variable_name, quiet, variable_assigned, error_status)
                end if
                if (error_status == 0) call assign_field(model_variable, group%topo_slope, error_status)

            !> Drainage/routing attributes.
            case (VN_CHNLSLOPE)
                if (.not. allocated(group%chnl_slope)) then
                    call model_variable_activate_from_name(group, variable_name, quiet, variable_assigned, error_status)
                end if
                if (error_status == 0) call assign_field(model_variable, group%chnl_slope, error_status)
            case (VN_CHNLLENGTH)
                if (.not. allocated(group%chnl_length)) then
                    call model_variable_activate_from_name(group, variable_name, quiet, variable_assigned, error_status)
                end if
                if (error_status == 0) call assign_field(model_variable, group%chnl_length, error_status)
            case (VN_ICHNL, 'CHNL')
                if (.not. allocated(group%ichnl)) then
                    call model_variable_activate_from_name(group, variable_name, quiet, variable_assigned, error_status)
                end if
                if (error_status == 0) call assign_field(model_variable, group%ichnl, error_status)
            case (VN_IREACH, 'REACH')
                if (.not. allocated(group%ireach)) then
                    call model_variable_activate_from_name(group, variable_name, quiet, variable_assigned, error_status)
                end if
                if (error_status == 0) call assign_field(model_variable, group%ireach, error_status)
            case (VN_DA)
                if (.not. allocated(group%drainage_area)) then
                    call model_variable_activate_from_name(group, variable_name, quiet, variable_assigned, error_status)
                end if
                if (error_status == 0) call assign_field(model_variable, group%drainage_area, error_status)
            case (VN_BNKFLL, 'BANKFULL')
                if (.not. allocated(group%bankfull)) then
                    call model_variable_activate_from_name(group, variable_name, quiet, variable_assigned, error_status)
                end if
                if (error_status == 0) call assign_field(model_variable, group%bankfull, error_status)

            !> Meteorology/climatology variables.
            case (VN_FSIN)
                if (.not. allocated(group%fsin)) then
                    call model_variable_activate_from_name(group, variable_name, quiet, variable_assigned, error_status)
                end if
                if (error_status == 0) call assign_field(model_variable, group%fsin, error_status)
            case (VN_FLIN)
                if (.not. allocated(group%flin)) then
                    call model_variable_activate_from_name(group, variable_name, quiet, variable_assigned, error_status)
                end if
                if (error_status == 0) call assign_field(model_variable, group%flin, error_status)
            case (VN_TA)
                if (.not. allocated(group%ta)) then
                    call model_variable_activate_from_name(group, variable_name, quiet, variable_assigned, error_status)
                end if
                if (error_status == 0) call assign_field(model_variable, group%ta, error_status)
            case (VN_QA)
                if (.not. allocated(group%qa)) then
                    call model_variable_activate_from_name(group, variable_name, quiet, variable_assigned, error_status)
                end if
                if (error_status == 0) call assign_field(model_variable, group%qa, error_status)
            case (VN_PRES)
                if (.not. allocated(group%pres)) then
                    call model_variable_activate_from_name(group, variable_name, quiet, variable_assigned, error_status)
                end if
                if (error_status == 0) call assign_field(model_variable, group%pres, error_status)
            case (VN_UU)
                if (.not. allocated(group%uu)) then
                    call model_variable_activate_from_name(group, variable_name, quiet, variable_assigned, error_status)
                end if
                if (error_status == 0) call assign_field(model_variable, group%uu, error_status)
            case (VN_VV)
                if (.not. allocated(group%vv)) then
                    call model_variable_activate_from_name(group, variable_name, quiet, variable_assigned, error_status)
                end if
                if (error_status == 0) call assign_field(model_variable, group%vv, error_status)
            case (VN_UV)
                if (.not. allocated(group%uv)) then
                    call model_variable_activate_from_name(group, variable_name, quiet, variable_assigned, error_status)
                end if
                if (error_status == 0) call assign_field(model_variable, group%uv, error_status)
            case (VN_WDIR)
                if (.not. allocated(group%wdir)) then
                    call model_variable_activate_from_name(group, variable_name, quiet, variable_assigned, error_status)
                end if
                if (error_status == 0) call assign_field(model_variable, group%wdir, error_status)
            case (VN_PRERN)
                if (.not. allocated(group%prern)) then
                    call model_variable_activate_from_name(group, variable_name, quiet, variable_assigned, error_status)
                end if
                if (error_status == 0) call assign_field(model_variable, group%prern, error_status)
            case (VN_PRESNO)
                if (.not. allocated(group%presno)) then
                    call model_variable_activate_from_name(group, variable_name, quiet, variable_assigned, error_status)
                end if
                if (error_status == 0) call assign_field(model_variable, group%presno, error_status)
            case (VN_PRE)
                if (.not. allocated(group%pre)) then
                    call model_variable_activate_from_name(group, variable_name, quiet, variable_assigned, error_status)
                end if
                if (error_status == 0) call assign_field(model_variable, group%pre, error_status)

            !> Groundwater/lower zone storage variables.
            case (VN_RCHG)
                if (.not. allocated(group%rchg)) then
                    call model_variable_activate_from_name(group, variable_name, quiet, variable_assigned, error_status)
                end if
                if (error_status == 0) call assign_field(model_variable, group%rchg, error_status)

            !> Routing variables.
            case (VN_RFF)
                if (.not. allocated(group%rff)) then
                    call model_variable_activate_from_name(group, variable_name, quiet, variable_assigned, error_status)
                end if
                if (error_status == 0) call assign_field(model_variable, group%rff, error_status)

            !> Others (not saved).
            case default
                if (present(variable_assigned)) variable_assigned = .false.
        end select

    end subroutine

end module
