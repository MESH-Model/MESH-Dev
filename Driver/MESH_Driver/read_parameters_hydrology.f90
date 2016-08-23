!> *********************************************************************
!> Open and read in values from MESH_parameters_hydrology.ini file
!> *********************************************************************

subroutine READ_PARAMETERS_HYDROLOGY(shd, fls)

    use strings
    use sa_mesh_shared_variabletypes
    use sa_mesh_shared_variables
    use model_files_variabletypes
    use model_files_variables
    use FLAGS

    use RUNCLASS36_variables

    use WF_ROUTE_config, only: wfp

    use baseflow_module, only: lzsp

    implicit none

    !> Local variables for parsing control flag strings.
    integer, parameter :: MaxLenField = 20, MaxArgs = 20, MaxLenLine = 100
    character(MaxLenLine) in_line
    character(MaxLenField), dimension(MaxArgs) :: out_args
    integer nargs
    character(1) :: delim = ' '

    !> Input variables.
!-    integer M_C, INDEPPAR, DEPPAR
!-    real WF_R2(M_C)
!-    character(8) RELEASE

!    type(HydrologyParameters) :: hp

    type(ShedGridParams) :: shd
    type(fl_ids):: fls

    !> Local variables.
!-    integer OPTFLAGS
    integer NTYPE, NA, NRVR
    integer iun, ierr, m, n, j, i
    real, dimension(:), allocatable :: INDEPPARVAL
    real, dimension(:, :), allocatable :: DEPPARVAL
    character(8) FILE_VER
!-    logical :: VER_OK = .true.

    NA = shd%NA
    NTYPE = shd%lc%NTYPE

    if (ro%VERBOSEMODE > 0) write(6, '(a)', advance = 'no') 'READING: MESH_parameters_hydrology.ini '

    iun = fls%fl(mfk%f23)%iun
    open(iun, file = trim(adjustl(fls%fl(mfk%f23)%fn)), status = 'old', action = 'read', iostat = ierr)

    !> Check for errors opening the file.
    if (ierr /= 0) then
        print *, 'FAILED'
        print *
        print *, 'MESH_parameters_hydrology.ini could not be opened. ', &
                 'Ensure that the file exists and restart the program.'
        stop
    end if

    !> Check the file version (if RELFLG = 1.0).
!-    if (RELFLG == 1) then

        !> Read the file version.
        read(iun, '(a8)') FILE_VER
        if (index(FILE_VER, ':') > 0) then
            FILE_VER = trim(adjustl(FILE_VER(1:index(FILE_VER, ':') - 1)))
        else if (index(FILE_VER, ' ') > 0) then
            FILE_VER = trim(adjustl(FILE_VER(1:index(FILE_VER, ' ') - 1)))
        else
            FILE_VER = trim(adjustl(FILE_VER))
        end if
!+        VER_OK = .false.
!+        if (FILE_VER == RELEASE) then
!+            VER_OK = .true.
!+        end if

        !> Wrong file version.
!-        if (.not. VER_OK) then
!-            print *
!-            if (len(trim(adjustl(FILE_VER))) > 0) then
!-                print *, ' File version: ', FILE_VER
!-            else
!-                print *, 'This file is out of date.'
!-            end if

!-            print *, 'MESH requires file version: ', RELEASE
!-            print *, 'Please update MESH_parameters_hydrology.ini.'
!-            print *, 'The file must contain the version number'
!-            print *, 'on the first line, followed by a colon.'
!-            print *, 'EXAMPLE: '
!-            print *, RELEASE, ': MESH_parameters_hydrology.ini'
!-            print *
!-            print *, 'Please insure that all other parameters'
!-            print *, 'are also updated.'
!-            stop
!-        end if
!-    else
!-        read(iun, *)
!-    end if

    !> Option flags.
    read(iun, *)
    read(iun, *)
    read(iun, *) n
    if (n > 0) then
        do i = 1, n
            read(iun, *)
        end do
    end if

    !> *****************************************************************
    !> River channel routing variables.
    !> *****************************************************************

    !> Initialize variables.
    NRVR = shd%NRVR
    allocate(wfp%r1(NRVR), wfp%r2(NRVR), &
             wfp%aa1(NRVR), wfp%aa2(NRVR), wfp%aa3(NRVR), wfp%aa4(NRVR))
    wfp%r1 = 0.0
    wfp%r2 = 0.0
    wfp%aa1 = 1.0
    wfp%aa2 = 11.0
    wfp%aa3 = 0.43
    wfp%aa4 = 1.0

    !> Read variables from file.
    read(iun, *)
    read(iun, *)
    select case (FILE_VER)

        !> Version 2.0. (2.980 is a placeholder for an intermediate commit to TRUNK).
        case ('2.980')

            !> Read number of channel routing parameters.
            read(iun, *) n
            do i = 1, n
                read(iun,'(a)') in_line
                call parse(in_line, delim, out_args, nargs)
                select case (lowercase(out_args(1)))

                    !> WF_R2.
                    case ('r2')
                        do j = 1, NRVR
                            call value(out_args(j + 1), wfp%r2(j), ierr)
                            if (ierr /= 0) then
                                print 9100, trim(out_args(1)), j, trim(out_args(j + 1))
                                stop
                            end if
                        end do
9100    format(//3x, 'Error converting channel routing parameter ', (a), ' #', i3, ": '", (a), "'", /)

                    !> Unrecognized parameter name.
                    case default
                        print *
                        print 9200, trim(out_args(1))
                        stop
9200    format(//3x, 'Unrecognized channel routing parameter: ', (a), /)

                end select
            end do

        !> Original format of the hydrology.ini file.
        case default
            read(iun, *) (wfp%r2(i), i = 1, NRVR)

    end select
    do i = 1, NRVR
        if (wfp%r2(i) <= 0) then
            print *
            print *, 'River roughness =0 (in MESH parameters_hydrology.ini) '
            print *, 'Even if you only have only one river class, all initial WF_R2'
            print *, 'values must be non-zero in MESH_parameters_hydrology.ini '
            stop
        end if
    end do

    !> GRU-independent parameters.
    read(iun, *)
    read(iun, *)
    read(iun, *) n
    if (n > 0) then

        !> Allocate and distribute variables.
        allocate(INDEPPARVAL(n))
        do i = 1, n
            read(iun, *, err = 9000) INDEPPARVAL(i)
        end do

        !> Count for active flags (read from run options).
        j = 0

        !> FROZENSOILINIFLAG (infiltration into frozen soils).
        if (FROZENSOILINFILFLAG == 1) then
            if (n < (4 + NYEARS)) then
                SOIL_POR_MAX = INDEPPARVAL(1)
                SOIL_DEPTH = INDEPPARVAL(2)
                S0 = INDEPPARVAL(3)
                T_ICE_LENS = INDEPPARVAL(4)
                do i = 5, NYEARS
                    t0_ACC(i - 4) = INDEPPARVAL(i)
                end do
            else
                print *
                print *, 'ERROR: FROZEN SOIL INFILTRATION FLAG IS ON BUT CORRESPONDING PARAMETER VALUES ', &
                         'ARE NOT CORRECTLY SPECIFIED IN THE HYDROLOGY INITIALIZATION FILE.'
                print *, 'PROVIDE PARAMETER VALUES FOR:'
                print *, 'MAXIMUM SOIL POROSITY [0 - 1]'
                print *, 'DEPTH FROM SURFACE TO BOTTOM OF ROOTING ZONE FOR MAXIMUM WATER HOLDING CAPACITY, m'
                print *, 'SURFACE SOIL SATURATION [0 - 1]'
                print *, 'OVER NIGHT TEMPERATURE TO CAUSE ICE LENS [-50 - 0]'
                print *, 'OPPORTUNITY TIME IN HOURS [100 - 1000] FOR EACH SIMULATION YEAR'
                print *, 'AN EMPIRICAL EQUATION WILL BE USED FOR OPPORTUNITY TIME VALUES SET TO 0'
                print *
                stop
            end if
            j = j + (4 + NYEARS)
        end if

        !> BASEFLOWFLAG (lower zone storage).
        if (lzsp%BASEFLOWFLAG > 0) then
            lzsp%WrchrgIni = INDEPPARVAL(j + 1)
            lzsp%QbIni = INDEPPARVAL(j + 2)
            j = j + 2
        end if

        !> Clean-up/deallocate variable.
        deallocate(INDEPPARVAL)
    end if

    !> GRU-dependent parameters.
    read(iun, *)
    read(iun, *)
    read(iun, *) i
    read(iun, *) n
    if (n > 0) then

        !> Check that GRU count matches the GRU count from the shd file.
        if (i /= NTYPE) then
            print *, 'Number of GRUs in hydrology file: ', i
            print *, 'Number of GRUs in drainage database: ', NTYPE
            print *, 'Please adjust these values.'
            stop
        end if
        read(iun, *)
        if (n < 9) then
            print *
            print *, 'ERROR: THE NUMBER OF GRU DEPENDANT HYDROLOGY PARAMETERS SHOULD BE 9'
            print *, 'PLEASE REFER TO THE CURRENT TEMPLATE OF HYDROLOGY PARAMETERS FILE.'
            print *
            stop
        end if

        !> Allocate and distribute variables.
        allocate(DEPPARVAL(n, NTYPE))
        if (lzsp%BASEFLOWFLAG > 0) then
            select case (lzsp%BASEFLOWFLAG)
                case (1)
                    allocate(lzsp%dgwsh(NA, NTYPE), lzsp%agwsh(NA, NTYPE))
                case (2)
                    allocate(lzsp%WF_LZFA(NA, NTYPE), lzsp%WF_LZFPWR(NA, NTYPE))
            end select
        end if
        do i = 1, n
            read(iun, *, err = 9001) (DEPPARVAL(i, j), j = 1, NTYPE)
        end do
!todo: change this to il2, il2
        do i = 1, NA
            do m = 1, NTYPE

                !> CLASS ponding limits.
                hp%ZSNLROW(i, m) = DEPPARVAL(1, m)
                hp%ZPLSROW(i, m) = DEPPARVAL(2, m)
                hp%ZPLGROW(i, m) = DEPPARVAL(3, m)

                !> FROZENSOILINIFLAG (infiltration into frozen soils).
                hp%FRZCROW(i, m) = DEPPARVAL(4, m)

                !> PDMROF.
                hp%CMAXROW(i, m) = DEPPARVAL(5, m)
                hp%CMINROW(i, m) = DEPPARVAL(6, m)
                hp%BROW(i, m) = DEPPARVAL(7, m)
                hp%K1ROW(i, m) = DEPPARVAL(8, m)
                hp%K2ROW(i, m) = DEPPARVAL(9, m)

                !> Count for active flags (read from run options).
                j = 9

                !> PBSM (blowing snow model).
                if (PBSMFLAG == 1) then
                    hp%fetchROW(i, m) = DEPPARVAL(10, m)
                    hp%HtROW(i, m) = DEPPARVAL(11, m)
                    hp%N_SROW(i, m) = DEPPARVAL(12, m)
                    hp%A_SROW(i, m) = DEPPARVAL(13, m)
                    hp%DistribROW(i, m) = DEPPARVAL(14, m)
                    j = j + 5
                end if

                !> BASEFLOWFLAG (lower zone storage).
                if (lzsp%BASEFLOWFLAG > 0) then
                    select case (lzsp%BASEFLOWFLAG)
                        case (1)
                            lzsp%dgwsh(i, m) = DEPPARVAL(j + 1, m)
                            lzsp%agwsh(i, m) = DEPPARVAL(j + 2, m)
                            j = j + 2
                        case (2)
                            lzsp%WF_LZFPWR(i, m) = DEPPARVAL(j + 1, m)
                            lzsp%WF_LZFA(i, m) = DEPPARVAL(j + 2, m)
                            j = j + 2
                    end select
                end if
            end do
        end do

        !> Clean-up/deallocate the variable.
        deallocate(DEPPARVAL)
    end if

    !> Close the file.
    close(iun)
    if (ro%VERBOSEMODE > 0) print *, 'READ: SUCCESSFUL, FILE: CLOSED'

    return

9000    print *, 'ERROR: READING GRU-INDEPENDENT PARAMETER ', i, 'IN THE HYDROLOGY FILE.'
        stop

9001    print *, 'ERROR: READING GRU-DEPENDENT PARAMETER: ROW ', i, 'COLUMN ', j, 'IN THE HYDROLOGY FILE.'
        stop

end subroutine
