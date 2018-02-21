!> Description:
!>  Open and read in values from soil.ini file to replace values
!>  calculated using %sand and %clay from CLASS.ini.
!>  Bruce Davison, August 13, 2004
subroutine READ_SOIL_INI(fls, shd)

    use model_files_variables
    use sa_mesh_variables
    use sa_mesh_utilities
    use FLAGS

    !> Input variables.
    type(fl_ids) :: fls
    type(ShedGridParams) :: shd

    !> Local variables.
    integer NTYPE, iun, m, j, ierr

    !> Return if the option is not active.
    if (SOILINIFLAG /= 5) return

    !> Open the file.
    call print_screen('READING: ' // trim(fls%fl(mfk%f54)%fn))
    call print_screen('REMARK: This file supports only 3 soil layers.', 3)
    iun = fls%fl(mfk%f54)%iun
    open(iun, file = fls%fl(mfk%f54)%fn, status = 'old', action = 'read', err = 997)

    !> Assign local variables.
    NTYPE = shd%lc%NTYPE

    !> Read variables from the file.
    read(iun, *, err = 999)
    read(iun, *, err = 999) (pm_gru%slp%thpor(m, 1), m = 1, NTYPE)
    read(iun, *, err = 999)
    read(iun, *, err = 999) (pm_gru%slp%thpor(m, 2), m = 1, NTYPE)
    read(iun, *, err = 999)
    read(iun, *, err = 999) (pm_gru%slp%thpor(m, 3), m = 1, NTYPE)
    read(iun, *, err = 999)
    read(iun, *, err = 999) (pm_gru%slp%thlret(m, 1), m = 1, NTYPE)
    read(iun, *, err = 999)
    read(iun, *, err = 999) (pm_gru%slp%thlret(m, 2), m = 1, NTYPE)
    read(iun, *, err = 999)
    read(iun, *, err = 999) (pm_gru%slp%thlret(m, 3), m = 1, NTYPE)
    read(iun, *, err = 999)
    read(iun, *, err = 999) (pm_gru%slp%thlmin(m, 1), m = 1, NTYPE)
    read(iun, *, err = 999)
    read(iun, *, err = 999) (pm_gru%slp%thlmin(m, 2), m = 1, NTYPE)
    read(iun, *, err = 999)
    read(iun, *, err = 999) (pm_gru%slp%thlmin(m, 3), m = 1, NTYPE)
    read(iun, *, err = 999)
    read(iun, *, err = 999) (pm_gru%slp%bi(m, 1), m = 1, NTYPE)
    read(iun, *, err = 999)
    read(iun, *, err = 999) (pm_gru%slp%bi(m, 2), m = 1, NTYPE)
    read(iun, *, err = 999)
    read(iun, *, err = 999) (pm_gru%slp%bi(m, 3), m = 1, NTYPE)
    read(iun, *, err = 999)
    read(iun, *, err = 999) (pm_gru%slp%psisat(m, 1), m = 1, NTYPE)
    read(iun, *, err = 999)
    read(iun, *, err = 999) (pm_gru%slp%psisat(m, 2), m = 1, NTYPE)
    read(iun, *, err = 999)
    read(iun, *, err = 999) (pm_gru%slp%psisat(m, 3), m = 1, NTYPE)
    read(iun, *, err = 999)
    read(iun, *, err = 999) (pm_gru%slp%grksat(m, 1), m = 1, NTYPE)
    read(iun, *, err = 999)
    read(iun, *, err = 999) (pm_gru%slp%grksat(m, 2), m = 1, NTYPE)
    read(iun, *, err = 999)
    read(iun, *, err = 999) (pm_gru%slp%grksat(m, 3), m = 1, NTYPE)
    read(iun, *, err = 999)
    read(iun, *, err = 999) (pm_gru%slp%hcps(m, 1), m = 1, NTYPE)
    read(iun, *, err = 999)
    read(iun, *, err = 999) (pm_gru%slp%hcps(m, 2), m = 1, NTYPE)
    read(iun, *, err = 999)
    read(iun, *, err = 999) (pm_gru%slp%hcps(m, 3), m = 1, NTYPE)
    read(iun, *, err = 999)
    read(iun, *, err = 999) (pm_gru%slp%tcs(m, 1), m = 1, NTYPE)
    read(iun, *, err = 999)
    read(iun, *, err = 999) (pm_gru%slp%tcs(m, 2), m = 1, NTYPE)
    read(iun, *, err = 999)
    read(iun, *, err = 999) (pm_gru%slp%tcs(m, 3), m = 1, NTYPE)
    close(iun)

    !> Distribute the variables.
    do m = 1, NTYPE
        do j = 4, shd%lc%IGND
            pm_gru%slp%thpor(m, j) = pm_gru%slp%thpor(m, 3)
            pm_gru%slp%thlret(m, j) = pm_gru%slp%thlret(m, 3)
            pm_gru%slp%thlmin(m, j) = pm_gru%slp%thlmin(m, 3)
            pm_gru%slp%bi(m, j) = pm_gru%slp%bi(m, 3)
            pm_gru%slp%psisat(m, j) = pm_gru%slp%psisat(m, 3)
            pm_gru%slp%grksat(m, j) = pm_gru%slp%grksat(m, 3)
            pm_gru%slp%hcps(m, j) = pm_gru%slp%hcps(m, 3)
            pm_gru%slp%tcs(m, j) = pm_gru%slp%tcs(m, 3)
        end do
    end do

    return

    !> Stop: File not found.
997 call print_error('Unable to open file.')
    call print_message('Set SOILINITFLAG to a value less than 5. Hydraulic properties will be calculated from soil percentages.')
    call print_message('If the soil percentages add to greater than 100%:')
    call print_message_detail('SOILINIFLAG set to 1 - MESH will use the soil percentages as specified.')
    call print_message_detail('SOILINIFLAG set to 2 - MESH will adjust soil percentages in favor of sand.')
    call print_message_detail('SOILINIFLAG set to 3 - MESH will adjust soil percentages in favor of clay.')
    call print_message_detail('SOILINIFLAG set to 4 - MESH will proportionally adjust the soil percentages.')
    call stop_program()

    !> Stop: Premature end of file.
999 call print_error('Unable to read from file.')
    call stop_program()

    !> Format statements.
1001    format(9999(g15.6, 1x))

end subroutine
