!>
!> Description:
!>  Subroutine to read parameters from file, in r2c format. Parameter
!>  values are saved directly to the shared parameter object at the
!>  grid level, accessible by sa_mesh_shared_variables.
!>
!> Input:
!*  shd: Basin shed object, containing information about the grid
!*      definition read from MESH_drainage_database.r2c.
!*  iun: Unit of the input file (default: 100).
!*  fname: Full path to the file (default: './MESH_input_parameters.r2c').
!>
subroutine read_parameters_r2c(shd, iun, fname)

    use strings
    use mpi_module
    use sa_mesh_shared_variables
    use ensim_io

    use RUNCLASS36_variables
    use RUNSVS113_variables
    use baseflow_module
    use rte_module
    use PBSM_module

    implicit none

    !> Input variables.
    type(ShedGridParams) :: shd
    integer :: iun
    character(len = *) :: fname

    !> Local variables.
    type(ensim_keyword), dimension(:), allocatable :: vkeyword
    type(ensim_attr), dimension(:), allocatable :: vattr
    integer nkeyword, nattr, iattr, ilvl, i, ierr
    character(len = MAX_WORD_LENGTH) tfield, tlvl
    real, dimension(:), allocatable :: ffield
    logical verbose

    !> Local variables.
    verbose = (ro%VERBOSEMODE > 0)

    !> Open the file and read the header.
    call open_ensim_file(iun, fname, ierr, verbose)
    call parse_header_ensim(iun, fname, vkeyword, nkeyword, ierr)
    call validate_header_spatial( &
        fname, vkeyword, nkeyword, &
        shd%xCount, shd%xDelta, shd%xOrigin, shd%yCount, shd%yDelta, shd%yOrigin, &
        verbose)

    !> Read and parse attributes in the header.
    call parse_header_attribute_ensim(iun, fname, vkeyword, nkeyword, vattr, nattr, ierr)

    !> Read and parse the single frame data.
    call load_data_r2c(iun, fname, vattr, nattr, shd%xCount, shd%yCount, .false., ierr)

    !> Distribute the data to the appropriate variable.
    allocate(ffield(shd%NA))
    do iattr = 1, nattr

        !> Extract variable name and level.
        tfield = lowercase(vattr(iattr)%attr)
        i = index(trim(adjustl(tfield)), ' ')
        ilvl = 0
        if (i > 0) then
            tlvl = tfield((i + 1):)
            tfield = tfield(1:i)
            call value(tlvl, ilvl, ierr)
            if (ierr /= 0) ilvl = 0
        end if

        !> Assign the data to a vector.
        call r2c_to_rank(iun, fname, vattr, nattr, iattr, shd%xxx, shd%yyy, shd%NA, ffield, shd%NA, verbose)

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
                    if (index(lowercase(vattr(iattr)%units), 'km') > 0) then
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
                        print 1130, adjustl(fname), trim(adjustl(vattr(iattr)%attr))
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
                        print 1130, adjustl(fname), trim(adjustl(vattr(iattr)%attr))
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
                        print 1130, adjustl(fname), trim(adjustl(vattr(iattr)%attr))
                    end if
                end if

            !> RUNCLASS36.
            case ('iwf')
                if (RUNCLASS36_flgs%PROCESS_ACTIVE) then
                    pm_grid%tp%iwf = ffield
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
                if (verbose) print 1120, adjustl(fname), trim(adjustl(vattr(iattr)%attr))

        end select
    end do

    !> Close the file to free the unit.
    close(iun)

1110    format(3x, 999(1x, g16.9))
1120    format(3x, 'WARNING: Unrecognized attribute in ', (a), ': ', (a))
1130    format(3x, 'WARNING: Unrecognized category or level out-of-bounds in ', (a), ': ', (a))

end subroutine
