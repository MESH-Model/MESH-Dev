!> Description:
!>  Subroutine to read the resume configuration.
subroutine resumerun_config(fls, shd, ierr)

    use strings
    use mesh_io_constants
    use model_files_variables
    use sa_mesh_common
    use model_dates
    use date_utilities, only: jday_to_date
    use resume_run

    implicit none

    !> Input variables.
    type(fl_ids) fls
    type(ShedGridParams) shd

    !> Output variables.
    integer, intent(out) :: ierr

    !> Local variables.
    integer iun, n, i, z
    character(len = DEFAULT_LINE_LENGTH) args(50), line
    character(len = DEFAULT_FIELD_LENGTH) fname
    logical lstate

    !> Initialize the return variable.
    ierr = 0

    !> Print messages.
    call print_new_section('READING: Resume run configuration')
    call increase_tab()

    !> Assign the default options for RESUMEFLAG.
    resume_options%resume%state = FLAG_OFF
    resume_options%resume%freq = FREQ_NUL
    resume_options%resume%flo%ext = FILE_TYPE_NUL
    resume_options%resume%bin = ''

    !> Parse RESUMEFLAG.
    call parse(RESUMEFLAG, ' ', args, n)
    do i = 2, n
        select case (lowercase(args(i)))

            !> Operational state (active or auto).
            case ('on')
                resume_options%resume%state = FLAG_ON
            case ('auto')
                resume_options%resume%state = FLAG_AUTO

            !> Operational state (inactive).
            case ('0', 'off')

                !> Disable options that could re-enable the flag and exit.
                resume_options%resume%state = FLAG_OFF
                resume_options%resume%flo%ext = FILE_TYPE_NUL
                exit

            !> Legacy options.
            case ('2', '1')
                call print_error("'RESUMEFLAG " // trim(adjustl(args(i))) // "' is not supported. Use 'RESUMEFLAG 4' instead.")
                ierr = 1
                return
            case ('3')
                if (resume_options%resume%state == FLAG_OFF) resume_options%resume%state = FLAG_ON
                if (.not. btest(resume_options%resume%flo%ext, FILE_TYPE_SEQ)) then
                    resume_options%resume%flo%ext = resume_options%resume%flo%ext + radix(FILE_TYPE_SEQ)**FILE_TYPE_SEQ
                end if
                if (index(resume_options%resume%bin, '+CLASSPROG') == 0) then
                    resume_options%resume%bin = trim(resume_options%resume%bin) // '+CLASSPROG'
                end if
            case ('4')
                if (resume_options%resume%state == FLAG_OFF) resume_options%resume%state = FLAG_ON
                if (.not. btest(resume_options%resume%flo%ext, FILE_TYPE_SEQ)) then
                    resume_options%resume%flo%ext = resume_options%resume%flo%ext + radix(FILE_TYPE_SEQ)**FILE_TYPE_SEQ
                end if
            case ('5')
                if (resume_options%resume%state == FLAG_OFF) resume_options%resume%state = FLAG_ON
                if (.not. btest(resume_options%resume%flo%ext, FILE_TYPE_SEQ)) then
                    resume_options%resume%flo%ext = resume_options%resume%flo%ext + radix(FILE_TYPE_SEQ)**FILE_TYPE_SEQ
                end if
                if (index(resume_options%resume%bin, '+STASONLY') == 0) then
                    resume_options%resume%bin = trim(resume_options%resume%bin) // '+STASONLY'
                end if
            case ('6', 'nc', 'nc4', 'netcdf')
#ifdef NETCDF
                if (resume_options%resume%state == FLAG_OFF) resume_options%resume%state = FLAG_ON
                if (.not. btest(resume_options%resume%flo%ext, FILE_TYPE_NC4)) then
                    resume_options%resume%flo%ext = resume_options%resume%flo%ext + radix(FILE_TYPE_NC4)**FILE_TYPE_NC4
                end if
                if (index(resume_options%resume%bin, '+STASONLY') == 0) then
                    resume_options%resume%bin = trim(resume_options%resume%bin) // '+STASONLY'
                end if
#else
                call print_error( &
                    "The NetCDF format is specified for a resume file but the module is not active. " // &
                    "A version of MESH compiled with the NetCDF library must be used to use files in this format.")
                ierr = 1
                return
#endif

            !> File formats.
!+            case ('r2c')
!+                resume_options%resume%flo%ext = resume_options%resume%flo%ext + radix(FILE_TYPE_R2C)**FILE_TYPE_R2C
            case ('seq', 'binseq')
                if (.not. btest(resume_options%resume%flo%ext, FILE_TYPE_SEQ)) then
                    resume_options%resume%flo%ext = resume_options%resume%flo%ext + radix(FILE_TYPE_SEQ)**FILE_TYPE_SEQ
                end if
!+            case ('txt')
!+                resume_options%resume%flo%ext = resume_options%resume%flo%ext + radix(FILE_TYPE_TXT)**FILE_TYPE_TXT
!+            case ('csv')
!+                resume_options%resume%flo%ext = resume_options%resume%flo%ext + radix(FILE_TYPE_CSV)**FILE_TYPE_CSV

            !> Directives.
            case ('only')
                resume_options%resume%bin = ''
            case ('class')
                if (index(resume_options%resume%bin, '+CLASSPROG') == 0) then
                    resume_options%resume%bin = trim(resume_options%resume%bin) // '+CLASSPROG'
                end if
            case ('states')
                if (index(resume_options%resume%bin, '+STASONLY') == 0) then
                    resume_options%resume%bin = trim(resume_options%resume%bin) // '+STASONLY'
                end if
            case default

                !> Unknown or unsupported option.
                call print_warning("Unknown or unsupported option '" // trim(args(i)) // "'.")
        end select
    end do

    !> Check for bad configuration.
    if (resume_options%resume%state == FLAG_ON .and. resume_options%resume%flo%ext == FILE_TYPE_NUL) then
!+        call print_warning("RESUMEFLAG is active with no file format specified. Regular plain text format 'txt' is assumed.")
!+        resume_options%resume%flo%ext = radix(FILE_TYPE_TXT)**FILE_TYPE_TXT
        call print_error("RESUMEFLAG is active with no file format specified.")
        ierr = 1
        return
    else if (resume_options%resume%flo%ext /= FILE_TYPE_NUL .and. resume_options%resume%state == FLAG_OFF) then
        call print_remark( &
            "A file extension is active without directives to control the variables to resume. " // &
            "Model states only '+STASONLY' for all active modules without preservation of simulation statistics or totals " // &
            "will be resumed.")
        if (resume_options%resume%state == FLAG_OFF) resume_options%resume%state = FLAG_ON
        if (index(resume_options%resume%bin, '+STASONLY') == 0) then
            resume_options%resume%bin = trim(resume_options%resume%bin) // '+STASONLY'
        end if
    end if

    !> Echo configuration.
    if (resume_options%resume%state == FLAG_ON .or. resume_options%resume%state == FLAG_AUTO) then
        line = ''
        if (resume_options%resume%freq == FREQ_YEARLY) line = ' yearly' // trim(line)
        if (resume_options%resume%freq == FREQ_MONTHLY) line = ' monthly' // trim(line)
        if (index(resume_options%resume%bin, '+CLASSPROG') > 0) line = ' only class' // trim(line)
        if (index(resume_options%resume%bin, '+STASONLY') > 0) line = ' only states' // trim(line)
        if (btest(resume_options%resume%flo%ext, FILE_TYPE_R2C)) line = ' r2c' // trim(line)
        if (btest(resume_options%resume%flo%ext, FILE_TYPE_SEQ)) line = ' seq' // trim(line)
        if (btest(resume_options%resume%flo%ext, FILE_TYPE_TXT)) line = ' txt' // trim(line)
        if (btest(resume_options%resume%flo%ext, FILE_TYPE_CSV)) line = ' csv' // trim(line)
        if (btest(resume_options%resume%flo%ext, FILE_TYPE_NC4)) line = ' nc' // trim(line)
        if (resume_options%resume%state == FLAG_AUTO) then
            line = ' auto' // trim(line)
        else
            line = ' on' // trim(line)
        end if
        line = 'RESUMEFLAG' // trim(line)
        call print_message("RESUMEFLAG is ACTIVE: " // trim(line))
    else
        call print_message("RESUMEFLAG is not active: RESUMEFLAG off")
    end if

    !> Assign default options for SAVERESUMEFLAG.
    resume_options%save%state = FLAG_OFF
    resume_options%save%freq = FREQ_NUL
    resume_options%save%flo%ext = FILE_TYPE_NUL
    resume_options%save%bin = ''

    !> Parse SAVERESUMEFLAG.
    call parse(SAVERESUMEFLAG, ' ', args, n)
    do i = 2, n
        select case (lowercase(args(i)))

            !> Operational state (active).
            case ('on')
                resume_options%save%state = FLAG_ON

            !> Operational state (inactive).
            case ('0', 'off')

                !> Disable options that could re-enable the flag and exit.
                resume_options%save%state = FLAG_OFF
                resume_options%save%freq = FREQ_NUL
                resume_options%save%flo%ext = FILE_TYPE_NUL
                exit

            !> Legacy options.
            case ('2', '1')
                call print_error("'SAVERESUMEFLAG " // trim(adjustl(args(i))) // "' is not supported. Use 'SAVERESUMEFLAG 4' instead.")
                ierr = 1
                return
            case ('3')
                if (resume_options%save%state == FLAG_OFF) resume_options%save%state = FLAG_ON
                if (.not. btest(resume_options%save%flo%ext, FILE_TYPE_SEQ)) then
                    resume_options%save%flo%ext = resume_options%save%flo%ext + radix(FILE_TYPE_SEQ)**FILE_TYPE_SEQ
                end if
                if (index(resume_options%save%bin, '+CLASSPROG') == 0) then
                    resume_options%save%bin = trim(resume_options%save%bin) // '+CLASSPROG'
                end if
            case ('4')
                if (resume_options%save%state == FLAG_OFF) resume_options%save%state = FLAG_ON
                if (.not. btest(resume_options%save%flo%ext, FILE_TYPE_SEQ)) then
                    resume_options%save%flo%ext = resume_options%save%flo%ext + radix(FILE_TYPE_SEQ)**FILE_TYPE_SEQ
                end if
            case ('5')
                if (resume_options%save%state == FLAG_OFF) resume_options%save%state = FLAG_ON
                if (.not. btest(resume_options%save%flo%ext, FILE_TYPE_SEQ)) then
                    resume_options%save%flo%ext = resume_options%save%flo%ext + radix(FILE_TYPE_SEQ)**FILE_TYPE_SEQ
                end if
                if (index(resume_options%save%bin, '+STASONLY') == 0) then
                    resume_options%save%bin = trim(resume_options%save%bin) // '+STASONLY'
                end if
            case ('6', 'nc', 'nc4', 'netcdf')
#ifdef NETCDF
                if (resume_options%save%state == FLAG_OFF) resume_options%save%state = FLAG_ON
                if (.not. btest(resume_options%save%flo%ext, FILE_TYPE_NC4)) then
                    resume_options%save%flo%ext = resume_options%save%flo%ext + radix(FILE_TYPE_NC4)**FILE_TYPE_NC4
                end if
                if (index(resume_options%save%bin, '+STASONLY') == 0) then
                    resume_options%save%bin = trim(resume_options%save%bin) // '+STASONLY'
                end if
#else
                call print_error( &
                    "The NetCDF format is specified for a resume file but the module is not active. " // &
                    "A version of MESH compiled with the NetCDF library must be used to use files in this format.")
                ierr = 1
                return
#endif

            !> Frequency for I/O functions that are repeated.
            case ('yearly', 'yly', 'y')
                resume_options%save%freq = FREQ_YEARLY
            case ('monthly', 'mly', 'm')
                resume_options%save%freq = FREQ_MONTHLY

            !> File formats.
!+            case ('r2c')
!+                resume_options%save%flo%ext = resume_options%save%flo%ext + radix(FILE_TYPE_R2C)**FILE_TYPE_R2C
            case ('seq', 'binseq')
                if (.not. btest(resume_options%save%flo%ext, FILE_TYPE_SEQ)) then
                    resume_options%save%flo%ext = resume_options%save%flo%ext + radix(FILE_TYPE_SEQ)**FILE_TYPE_SEQ
                end if
!+            case ('txt')
!+                resume_options%save%flo%ext = resume_options%save%flo%ext + radix(FILE_TYPE_TXT)**FILE_TYPE_TXT
!+            case ('csv')
!+                resume_options%save%flo%ext = resume_options%save%flo%ext + radix(FILE_TYPE_CSV)**FILE_TYPE_CSV

            !> Directives.
            case ('only')
                resume_options%save%bin = ''
            case ('class')
                if (index(resume_options%save%bin, '+CLASSPROG') == 0) then
                    resume_options%save%bin = trim(resume_options%save%bin) // '+CLASSPROG'
                end if
            case ('states')
                if (index(resume_options%save%bin, '+STASONLY') == 0) then
                    resume_options%save%bin = trim(resume_options%save%bin) // '+STASONLY'
                end if
            case default

                !> Unknown or unsupported option.
                call print_warning("Unknown or unsupported option '" // trim(args(i)) // "'.")
        end select
    end do

    !> Automatically enable the flag if any output frequency is enabled.
    if (resume_options%save%freq /= FREQ_NUL) resume_options%save%state = FLAG_ON

    !> Check for bad configuration.
    if (resume_options%save%state == FLAG_ON .and. resume_options%save%flo%ext == FILE_TYPE_NUL) then
!+        call print_warning("SAVERESUMEFLAG is active with no file format specified. Regular plain text format 'txt' is assumed.")
!+        resume_options%save%flo%ext = radix(FILE_TYPE_TXT)**FILE_TYPE_TXT
        call print_error("SAVERESUMEFLAG is active with no file format specified.")
        ierr = 1
        return
    else if (resume_options%save%flo%ext /= FILE_TYPE_NUL .and. resume_options%save%state == FLAG_OFF) then
        call print_remark( &
            "A file extension is active without directives to control the variables to save. " // &
            "Model states only '+STASONLY' for all active modules without preservation of simulation statistics or totals " // &
            "will be saved.")
        if (resume_options%save%state == FLAG_OFF) resume_options%save%state = FLAG_ON
        if (index(resume_options%save%bin, '+STASONLY') == 0) then
            resume_options%save%bin = trim(resume_options%save%bin) // '+STASONLY'
        end if
    end if

    !> Echo configuration.
    if (resume_options%save%state == FLAG_ON .or. resume_options%save%state == FLAG_AUTO) then
        line = ''
        if (resume_options%save%freq == FREQ_YEARLY) line = ' yearly' // trim(line)
        if (resume_options%save%freq == FREQ_MONTHLY) line = ' monthly' // trim(line)
        if (index(resume_options%save%bin, '+CLASSPROG') > 0) line = ' only class' // trim(line)
        if (index(resume_options%save%bin, '+STASONLY') > 0) line = ' only states' // trim(line)
        if (btest(resume_options%save%flo%ext, FILE_TYPE_R2C)) line = ' r2c' // trim(line)
        if (btest(resume_options%save%flo%ext, FILE_TYPE_SEQ)) line = ' seq' // trim(line)
        if (btest(resume_options%save%flo%ext, FILE_TYPE_TXT)) line = ' txt' // trim(line)
        if (btest(resume_options%save%flo%ext, FILE_TYPE_CSV)) line = ' csv' // trim(line)
        if (btest(resume_options%save%flo%ext, FILE_TYPE_NC4)) line = ' nc' // trim(line)
        if (resume_options%save%state == FLAG_AUTO) then
            line = ' auto' // trim(line)
        else
            line = ' on' // trim(line)
        end if
        line = 'SAVERESUMEFLAG' // trim(line)
        call print_message("SAVERESUMEFLAG is ACTIVE: " // trim(line))
    else
        call print_message("SAVERESUMEFLAG is not active: SAVERESUMEFLAG off")
    end if

!temp
    !> Auto-save/resume does not currently support option 4. Throw an error.
    if (resume_options%save%freq /= FREQ_NUL .and. &
        .not. (index(resume_options%save%bin, '+STASONLY') > 0 .or. index(resume_options%save%bin, '+CLASSPROG') > 0)) then
        call print_error( &
            "Auto-saving only supports SAVERESUMEFLAG options that save model states only " // &
            "(without time or time-stepping information). The auto-save options, such as 'monthly' and 'yearly', can be added " // &
            "to these configurations of SAVERESUMEFLAG:")
        call increase_tab()
        call print_message("SAVERESUMEFLAG on nc only states")
        call print_message("SAVERESUMEFLAG on seq only states")
        call print_message("SAVERESUMEFLAG on seq only class")
        call print_message("SAVERESUMEFLAG 6")
        call print_message("SAVERESUMEFLAG 5")
        call print_message("SAVERESUMEFLAG 3")
        call decrease_tab()
        ierr = 1
        return
    end if

    !> Check for auto resume file.
    if (resume_options%resume%state == FLAG_AUTO) then
        fname = 'auto_resume.ini'
        call print_new_section("READING: " // trim(fname))
        call increase_tab()
        inquire(file = fname, exist = lstate)
        if (lstate) then

            !> Open the file.
            iun = 100
            open(iun, file = fname, status = 'old', action = 'read', iostat = z)
            if (z /= 0) then
                call print_error("Unable to open the file.")
                call program_abort()
            end if

            !> Read the simulation start date from the file.
            read(iun, *, iostat = z) ic%start%year, ic%start%jday, ic%start%hour, ic%start%mins
            call jday_to_date(ic%start%year, ic%start%jday, ic%start%month, ic%start%day)
            if (z /= 0) then
                call print_error("Unable to read the resume date from the file.")
                call program_abort()
            end if
!todo: format statement
            write(line, "(i5, '/', i3.3, ' ', i2.2, ':', i2.2)") ic%start%year, ic%start%jday, ic%start%hour, ic%start%mins
            call print_message( &
                "Simulation start date revised to: " // trim(adjustl(line)) // ". The previous run state will be resumed.")
            close(iun)
        else

            !> Print a warning if the auto resume file does not exist.
            call print_warning( &
                "Auto-resume is active but '" // trim(fname) // "' cannot be found. The 'auto' option is ignored at this time.")
            call print_message("Reverting to regular RESUMEFLAG behavior.")

            !> Override the auto resume functionality.
            resume_options%resume%state = FLAG_ON
        end if
    end if

    !> Reset tabs.
    call reset_tab()

end subroutine
