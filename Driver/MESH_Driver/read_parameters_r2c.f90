!> Description:
!>  Subroutine to read parameters from a single-frame 'r2c' format file.
!>  Values are parsed by order of RANK and stored in variables that must
!>  allocated 1:NA.
!>
!> Input:
!*  shd: Basin 'shed' object (properties).
!*  iun: Unit of the input file.
!*  fname: Full path to the file (default: './MESH_input_parameters.r2c').
subroutine read_parameters_r2c(shd, iun, fname)

    !> strings: For 'lowercase' function.
    !> sa_mesh_variables: Required for MESH variables, parameters.
    !> sa_mesh_utilities: Required for printing/writing messages, VERBOSEMODE, DIAGNOSEMODE.
    !> ensim_io: Required for read 'r2c' format file.
    use strings
    use sa_mesh_variables
    use sa_mesh_utilities
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

    !> Local variables.
    type(ensim_keyword), dimension(:), allocatable :: vkeyword
    type(ensim_attr), dimension(:), allocatable :: vattr
    integer nkeyword, nattr, ilvl, l, i, ierr
    character(len = MAX_WORD_LENGTH) tfield, tlvl
    real, dimension(:), allocatable :: ffield
    character(len = DEFAULT_LINE_LENGTH) line

    !> Open the file and read the header.
    call open_ensim_file(iun, fname, ierr, VERBOSEMODE)
    call parse_header_ensim(iun, fname, vkeyword, nkeyword, ierr)
    call validate_header_spatial( &
        fname, vkeyword, nkeyword, &
        shd%xCount, shd%xDelta, shd%xOrigin, shd%yCount, shd%yDelta, shd%yOrigin, &
        VERBOSEMODE)

    !> Get the list of attributes.
    call parse_header_attribute_ensim(iun, fname, vkeyword, nkeyword, vattr, nattr, ierr)

    !> Read and parse the attribute data.
    call load_data_r2c(iun, fname, vattr, nattr, shd%xCount, shd%yCount, .false., ierr)

    !> Distribute the data to the appropriate variable.
    allocate(ffield(shd%NA))
    do l = 1, nattr

        !> Extract variable name and level.
        tfield = lowercase(vattr(l)%attr)
        i = index(trim(adjustl(tfield)), ' ')
        ilvl = 0
        if (i > 0) then
            tlvl = tfield((i + 1):)
            tfield = tfield(1:i)
            call value(tlvl, ilvl, ierr)
            if (ierr /= 0) ilvl = 0
        end if

        !> Assign the data to a vector.
        call r2c_to_rank(iun, fname, vattr, nattr, l, shd%xxx, shd%yyy, shd%NA, ffield, shd%NA, VERBOSEMODE)

        !> Determine the variable.
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
                            write(line, 1001) ilvl
                            call print_warning( &
                                trim(vattr(l)%attr) // ': Unrecognized category or level out-of-bounds: ' // trim(tlvl))
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
                            write(line, 1001) ilvl
                            call print_warning( &
                                trim(vattr(l)%attr) // ': Unrecognized category or level out-of-bounds: ' // trim(tlvl))
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
                        call print_remark(trim(vattr(l)%attr) // ": Unit conversion from 'm m-2' to 'km km -2'.")
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
                        write(line, 1001) ilvl
                        call print_warning( &
                            trim(vattr(l)%attr) // ': Unrecognized category or level out-of-bounds: ' // trim(adjustl(line)))
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
                        write(line, 1001) ilvl
                        call print_warning( &
                            trim(vattr(l)%attr) // ': Unrecognized category or level out-of-bounds: ' // trim(adjustl(line)))
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
                        write(line, 1001) ilvl
                        call print_warning( &
                            trim(vattr(l)%attr) // ': Unrecognized category or level out-of-bounds: ' // trim(adjustl(line)))
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

            !> Print a warning if the variable name is not recognized.
            case default
                if (DIAGNOSEMODE) call print_remark("'" // trim(vattr(l)%attr) // "' is not a recognized named.")
        end select
    end do

    !> Close the file to free the unit.
    close(iun)

    !> Format statements.
1001    format(9999(g15.6, 1x))

end subroutine
