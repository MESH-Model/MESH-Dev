!> Description:
!>  Subroutine to read parameters from a single-frame 'r2c' format file.
!>  Values are parsed by order of RANK and stored in variables that must
!>  allocated 1:NA.
!>
!> Input:
!*  shd: Basin 'shed' object (properties).
!*  iun: Unit of the input file.
!*  fname: Full path to the file (default: './MESH_input_parameters.r2c').
subroutine read_parameters_r2c(shd, iun, fname, ierr)

    !> strings: For 'lowercase' function.
    !> sa_mesh_common: For common MESH variables and routines.
    !> ensim_io: For routines to read 'r2c' format file.
    use strings
    use sa_mesh_common
    use ensim_io

    !> Process modules: Required for process variables, parameters.
    use RUNCLASS36_variables
    use RUNSVS113_variables
    use baseflow_module
    use rte_module
    use PBSM_module

    implicit none

    !> Input variables.
    type(ShedGridParams), intent(in) :: shd
    integer, intent(in) :: iun
    character(len = *), intent(in) :: fname

    !> Output variables.
    integer, intent(out) :: ierr

    !> Local variables.
    type(ensim_keyword), dimension(:), allocatable :: vkeyword
    type(ensim_attr), dimension(:), allocatable :: vattr
    integer nkeyword, nattr, ilvl, n, l, j, i, z
    character(len = MAX_WORD_LENGTH) tfield, tlvl
    real, dimension(:), allocatable :: ffield
    character(len = DEFAULT_LINE_LENGTH) line

    !> Initialize the return status.
    ierr = 0

    !> Open the file and read the header.
    call reset_tab()
    call print_message('READING: ' // trim(adjustl(fname)))
    call increase_tab()
    call open_ensim_input(iun, fname, ierr)
    if (ierr /= 0) return
    call parse_header_ensim(iun, vkeyword, nkeyword, ierr)
    if (ierr /= 0) return

    !> Check the spatial definition in the header.
    call validate_header_spatial( &
        vkeyword, nkeyword, &
        shd%CoordSys%Proj, shd%xCount, shd%xDelta, shd%xOrigin, shd%yCount, shd%yDelta, shd%yOrigin, &
        ierr)
    if (ierr /= 0) return

    !> Get the list of attributes.
    call parse_header_attribute_ensim(iun, vkeyword, nkeyword, vattr, nattr, ierr)
    if (ierr /= 0) then
        call print_error('Error reading attributes from the header in the file.')
        return
    end if
    if (nattr == 0) call print_warning('No attributes were found in the file.')

    !> Advance past the end of the header.
    call advance_past_header(iun, fname, ierr)
    if (ierr /= 0) then
        call print_error('Encountered premature end of file.')
        return
    end if

    !> Read and parse the attribute data.
    call load_data_r2c(iun, fname, vattr, nattr, shd%xCount, shd%yCount, .false., ierr)
    if (ierr /= 0) then
        call print_error('Error reading attribute values in the file.')
        return
    end if

    !> Distribute the data to the appropriate variable.
    allocate(ffield(shd%NA))
    n = 0
    do l = 1, nattr

        !> Extract variable name and level.
        z = 0
        tfield = lowercase(vattr(l)%attr)
        i = index(trim(adjustl(tfield)), ' ')
        ilvl = 0
        if (i > 0) then
            tlvl = tfield((i + 1):)
            tfield = tfield(1:i)
            call value(tlvl, ilvl, z)
            if (z /= 0) ilvl = 0
        end if
        if (DIAGNOSEMODE) call print_message("Reading '" // trim(tfield) // "'.")

        !> Assign the data to a vector.
        call r2c_to_rank(iun, vattr, nattr, l, shd%xxx, shd%yyy, shd%NA, ffield, shd%NA, ierr)
        if (ierr /= 0) then
            call print_error("Unable to read the '" // trim(vattr(l)%attr) // "' attribute.")
            return
        end if

        !> Determine the variable.
        z = 0
        select case (adjustl(tfield))

            !> RUNCLASS36 and RUNSVS113.
            case ('fcan')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                    select case (adjustl(tlvl))
                        case ('nl')
                            pm_grid%cp%fcan(:, 1) = ffield
                        case ('bl')
                            pm_grid%cp%fcan(:, 2) = ffield
                        case ('cr')
                            pm_grid%cp%fcan(:, 3) = ffield
                        case ('gr')
                            pm_grid%cp%fcan(:, 4) = ffield
                        case ('ur')
                            pm_grid%cp%fcan(:, 5) = ffield
                        case default
                            do j = 1, size(pm_grid%cp%fcan, 1)
                                pm_grid%cp%fcan(:, j) = ffield
                            end do
                    end select
                end if
            case ('lnz0')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                    select case (adjustl(tlvl))
                        case ('nl')
                            pm_grid%cp%lnz0(:, 1) = ffield
                        case ('bl')
                            pm_grid%cp%lnz0(:, 2) = ffield
                        case ('cr')
                            pm_grid%cp%lnz0(:, 3) = ffield
                        case ('gr')
                            pm_grid%cp%lnz0(:, 4) = ffield
                        case ('ur')
                            pm_grid%cp%lnz0(:, 5) = ffield
                        case default
                            do j = 1, size(pm_grid%cp%lnz0, 2)
                                pm_grid%cp%lnz0(:, j) = ffield
                            end do
                    end select
                end if
            case ('sdep')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                    pm_grid%slp%sdep = ffield
                end if
            case ('xslp')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                    pm_grid%tp%xslp = ffield
                end if
            case ('dd', 'dden')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                    pm_grid%hp%dd = ffield

                    !> Unit conversion if units are km km-2.
                    if (index(lowercase(vattr(l)%units), 'km') > 0) then
                        call print_remark("'" // trim(vattr(l)%attr) // "' converted from 'km km -2' to 'm m-2'.")
                        pm_grid%hp%dd = pm_grid%hp%dd/1000.0
                    end if
                end if
            case ('sand')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                    if (ilvl == 0) then
                        do ilvl = 1, shd%lc%IGND
                            pm_grid%slp%sand(:, ilvl) = ffield
                        end do
                    else if (ilvl <= shd%lc%IGND) then
                        pm_grid%slp%sand(:, ilvl) = ffield
                    else
                        z = 1
                    end if
                end if
            case ('clay')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                    if (ilvl == 0) then
                        do ilvl = 1, shd%lc%IGND
                            pm_grid%slp%clay(:, ilvl) = ffield
                        end do
                    else if (ilvl <= shd%lc%IGND) then
                        pm_grid%slp%clay(:, ilvl) = ffield
                    else
                        z = 1
                    end if
                end if
            case ('orgm')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                    if (ilvl == 0) then
                        do ilvl = 1, shd%lc%IGND
                            pm_grid%slp%orgm(:, ilvl) = ffield
                        end do
                    else if (ilvl <= shd%lc%IGND) then
                        pm_grid%slp%orgm(:, ilvl) = ffield
                    else
                        z = 1
                    end if
                end if

            !> RUNCLASS36.
            case ('iwf')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE) then
                    pm_grid%tp%iwf = int(ffield)
                end if

            !> BASEFLOWFLAG == 2 (lower zone storage).
            case ('pwr')
                if (bflm%BASEFLOWFLAG == 2) bflm%pm_grid%pwr = ffield
            case ('flz')
                if (bflm%BASEFLOWFLAG == 2) bflm%pm_grid%flz = ffield

            !> RPN RTE (Watflood, 2007).
            case ('r1n')
                if (rteflg%PROCESS_ACTIVE) rtepm%r1n = ffield
            case ('r2n')
                if (rteflg%PROCESS_ACTIVE) rtepm%r2n = ffield
            case ('mndr')
                if (rteflg%PROCESS_ACTIVE) rtepm%mndr = ffield
            case ('widep')
                if (rteflg%PROCESS_ACTIVE) rtepm%widep = ffield
            case ('aa2')
                if (rteflg%PROCESS_ACTIVE) rtepm%aa2 = ffield
            case ('aa3')
                if (rteflg%PROCESS_ACTIVE) rtepm%aa3 = ffield
            case ('aa4')
                if (rteflg%PROCESS_ACTIVE) rtepm%aa4 = ffield
!                case ('theta')
!                case ('kcond')

            !> PBSM (blowing snow).
            case ('fetch')
                if (pbsm%PROCESS_ACTIVE) pbsm%pm_grid%fetch = ffield
            case ('ht')
                if (pbsm%PROCESS_ACTIVE) pbsm%pm_grid%Ht = ffield
            case ('n_s')
                if (pbsm%PROCESS_ACTIVE) pbsm%pm_grid%N_S = ffield
            case ('a_s')
                if (pbsm%PROCESS_ACTIVE) pbsm%pm_grid%A_S = ffield
            case ('distrib')
                if (pbsm%PROCESS_ACTIVE) pbsm%pm_grid%Distrib = ffield

            !> Unrecognized.
            case default
                z = 2
        end select

        !> Status flags.
        if (z == 1) then
            line = "'" // trim(vattr(l)%attr) // "' has an unrecognized category or level out-of-bounds: " // trim(tlvl)
            call print_warning(line)
        else if (z == 2) then
            call print_warning("'" // trim(vattr(l)%attr) // "' is not recognized.")
        else if (z == 0) then
            n = n + 1
        end if
    end do

    !> Print number of active parameters.
    write(line, FMT_GEN) n
    call print_message('Active parameters in file: ' // trim(adjustl(line)))

    !> Close the file to free the unit.
    close(iun)

end subroutine
