!> Description:
!>  Subroutine to read the resume configuration.
subroutine resumerun_config(fls, shd, cm, ierr)

    use strings
    use model_files_variables
    use sa_mesh_common
    use climate_forcing

    implicit none

    !> Input variables.
    type(fl_ids) fls
    type(ShedGridParams) shd
    type(clim_info) cm

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
    call reset_tab()
    call print_message('READING: Resume run configuration')
    call increase_tab()

    !> Assign the default options for RESUMEFLAG.
    vs%flgs%resume%state = FLAG_OFF
    vs%flgs%resume%freq = FREQ_NUL
    vs%flgs%resume%flo%ffmt = FFMT_NUL
    vs%flgs%resume%bin = ''

    !> Parse RESUMEFLAG.
    call parse(RESUMEFLAG, ' ', args, n)
    do i = 1, n
        select case (lowercase(args(i)))

            !> Operational state.
            case ('on')
                vs%flgs%resume%state = FLAG_ON
            case ('auto')
                vs%flgs%resume%state = FLAG_AUTO

            !> Legacy options.
            case ('2', '1')
                call print_error('RESUMEFLAG ' // trim(adjustl(args(i))) // 'is not supported. Use RESUMEFLAG 4 instead.')
                ierr = 1
                return
            case ('3')
                if (vs%flgs%resume%state == FLAG_OFF) vs%flgs%resume%state = FLAG_ON
                if (.not. btest(vs%flgs%resume%flo%ffmt, FFMT_SEQ)) then
                    vs%flgs%resume%flo%ffmt = vs%flgs%resume%flo%ffmt + radix(FFMT_SEQ)**FFMT_SEQ
                end if
                if (index(vs%flgs%resume%bin, '+CLASSPROG') == 0) then
                    vs%flgs%resume%bin = trim(vs%flgs%resume%bin) // '+CLASSPROG'
                end if
            case ('4')
                if (vs%flgs%resume%state == FLAG_OFF) vs%flgs%resume%state = FLAG_ON
                if (.not. btest(vs%flgs%resume%flo%ffmt, FFMT_SEQ)) then
                    vs%flgs%resume%flo%ffmt = vs%flgs%resume%flo%ffmt + radix(FFMT_SEQ)**FFMT_SEQ
                end if
            case ('5')
                if (vs%flgs%resume%state == FLAG_OFF) vs%flgs%resume%state = FLAG_ON
                if (.not. btest(vs%flgs%resume%flo%ffmt, FFMT_SEQ)) then
                    vs%flgs%resume%flo%ffmt = vs%flgs%resume%flo%ffmt + radix(FFMT_SEQ)**FFMT_SEQ
                end if
                if (index(vs%flgs%resume%bin, '+STASONLY') == 0) then
                    vs%flgs%resume%bin = trim(vs%flgs%resume%bin) // '+STASONLY'
                end if

            !> File formats.
            case ('r2c')
                vs%flgs%resume%flo%ffmt = vs%flgs%resume%flo%ffmt + radix(FFMT_R2C)**FFMT_R2C
            case ('seq', 'binseq')
                if (.not. btest(vs%flgs%resume%flo%ffmt, FFMT_SEQ)) then
                    vs%flgs%resume%flo%ffmt = vs%flgs%resume%flo%ffmt + radix(FFMT_SEQ)**FFMT_SEQ
                end if
            case ('txt')
                vs%flgs%resume%flo%ffmt = vs%flgs%resume%flo%ffmt + radix(FFMT_TXT)**FFMT_TXT
            case ('csv')
                vs%flgs%resume%flo%ffmt = vs%flgs%resume%flo%ffmt + radix(FFMT_CSV)**FFMT_CSV

            !> Directives.
            case ('only')
                vs%flgs%resume%bin = ''
            case ('class')
                if (index(vs%flgs%resume%bin, '+CLASSPROG') == 0) then
                    vs%flgs%resume%bin = trim(vs%flgs%resume%bin) // '+CLASSPROG'
                end if
            case ('states')
                if (index(vs%flgs%resume%bin, '+STASONLY') == 0) then
                    vs%flgs%resume%bin = trim(vs%flgs%resume%bin) // '+STASONLY'
                end if
        end select
    end do

    !> Check for bad configuration.
    if (vs%flgs%resume%state == FLAG_ON .and. vs%flgs%resume%flo%ffmt == FFMT_NUL) then
!+        call print_warning("RESUMEFLAG is active with no file format specified. Regular plain text format 'txt' is assumed.")
!+        vs%flgs%resume%flo%ffmt = radix(FFMT_TXT)**FFMT_TXT
        call print_error('RESUMEFLAG is active with no file format specified.')
        ierr = 1
        return
    end if

    !> Echo configuration.
    if (vs%flgs%resume%state == FLAG_ON .or. vs%flgs%resume%state == FLAG_AUTO) then
        line = ''
        if (vs%flgs%resume%freq == FREQ_YLY) line = ' yearly' // trim(line)
        if (vs%flgs%resume%freq == FREQ_MLY) line = ' monthly' // trim(line)
        if (index(vs%flgs%resume%bin, '+CLASSPROG') > 0) line = ' only class' // trim(line)
        if (index(vs%flgs%resume%bin, '+STASONLY') > 0) line = ' only states' // trim(line)
        if (btest(vs%flgs%resume%flo%ffmt, FFMT_R2C)) line = ' r2c' // trim(line)
        if (btest(vs%flgs%resume%flo%ffmt, FFMT_CSV)) line = ' csv' // trim(line)
        if (btest(vs%flgs%resume%flo%ffmt, FFMT_SEQ)) line = ' seq' // trim(line)
        if (btest(vs%flgs%resume%flo%ffmt, FFMT_TXT)) line = ' txt' // trim(line)
        if (vs%flgs%resume%state == FLAG_AUTO) then
            line = ' auto' // trim(line)
        else
            line = ' on' // trim(line)
        end if
        line = 'RESUMEFLAG' // trim(line)
        call print_message('RESUMEFLAG is ACTIVE: ' // trim(line))
    else
        call print_message('RESUMEFLAG is not active: RESUMEFLAG off')
    end if

    !> Assign default options for SAVERESUMEFLAG.
    vs%flgs%save%state = FLAG_OFF
    vs%flgs%save%freq = FREQ_NUL
    vs%flgs%save%flo%ffmt = FFMT_NUL
    vs%flgs%save%bin = ''

    !> Parse SAVERESUMEFLAG.
    call parse(SAVERESUMEFLAG, ' ', args, n)
    do i = 1, n
        select case (lowercase(args(i)))

            !> Operational state.
            case ('on')
                vs%flgs%save%state = FLAG_ON

            !> Legacy options.
            case ('2', '1')
                call print_error('SAVERESUMEFLAG ' // trim(adjustl(args(i))) // 'is not supported. Use SAVERESUMEFLAG 4 instead.')
                ierr = 1
                return
            case ('3')
                if (vs%flgs%save%state == FLAG_OFF) vs%flgs%save%state = FLAG_ON
                if (.not. btest(vs%flgs%save%flo%ffmt, FFMT_SEQ)) then
                    vs%flgs%save%flo%ffmt = vs%flgs%save%flo%ffmt + radix(FFMT_SEQ)**FFMT_SEQ
                end if
                if (index(vs%flgs%save%bin, '+CLASSPROG') == 0) then
                    vs%flgs%save%bin = trim(vs%flgs%save%bin) // '+CLASSPROG'
                end if
            case ('4')
                if (vs%flgs%save%state == FLAG_OFF) vs%flgs%save%state = FLAG_ON
                if (.not. btest(vs%flgs%save%flo%ffmt, FFMT_SEQ)) then
                    vs%flgs%save%flo%ffmt = vs%flgs%save%flo%ffmt + radix(FFMT_SEQ)**FFMT_SEQ
                end if
            case ('5')
                if (vs%flgs%save%state == FLAG_OFF) vs%flgs%save%state = FLAG_ON
                if (.not. btest(vs%flgs%save%flo%ffmt, FFMT_SEQ)) then
                    vs%flgs%save%flo%ffmt = vs%flgs%save%flo%ffmt + radix(FFMT_SEQ)**FFMT_SEQ
                end if
                if (index(vs%flgs%save%bin, '+STASONLY') == 0) then
                    vs%flgs%save%bin = trim(vs%flgs%save%bin) // '+STASONLY'
                end if

            !> Frequency for I/O functions that are repeated.
            case ('yearly', 'yly', 'y')
                vs%flgs%save%freq = FREQ_YLY
            case ('monthly', 'mly', 'm')
                vs%flgs%save%freq = FREQ_MLY

            !> File formats.
            case ('r2c')
                vs%flgs%save%flo%ffmt = vs%flgs%save%flo%ffmt + radix(FFMT_R2C)**FFMT_R2C
            case ('seq', 'binseq')
                if (.not. btest(vs%flgs%save%flo%ffmt, FFMT_SEQ)) then
                    vs%flgs%save%flo%ffmt = vs%flgs%save%flo%ffmt + radix(FFMT_SEQ)**FFMT_SEQ
                end if
            case ('txt')
                vs%flgs%save%flo%ffmt = vs%flgs%save%flo%ffmt + radix(FFMT_TXT)**FFMT_TXT
            case ('csv')
                vs%flgs%save%flo%ffmt = vs%flgs%save%flo%ffmt + radix(FFMT_CSV)**FFMT_CSV

            !> Directives.
            case ('only')
                vs%flgs%save%bin = ''
            case ('class')
                if (index(vs%flgs%save%bin, '+CLASSPROG') == 0) then
                    vs%flgs%save%bin = trim(vs%flgs%save%bin) // '+CLASSPROG'
                end if
            case ('states')
                if (index(vs%flgs%save%bin, '+STASONLY') == 0) then
                    vs%flgs%save%bin = trim(vs%flgs%save%bin) // '+STASONLY'
                end if
        end select
    end do

    !> Automatically enable the flag if any output frequency is enabled.
    if (vs%flgs%save%freq /= FREQ_NUL) vs%flgs%save%state = FLAG_ON

    !> Check for bad configuration.
    if (vs%flgs%save%state == FLAG_ON .and. vs%flgs%save%flo%ffmt == FFMT_NUL) then
!+        call print_warning("SAVERESUMEFLAG is active with no file format specified. Regular plain text format 'txt' is assumed.")
!+        vs%flgs%save%flo%ffmt = radix(FFMT_TXT)**FFMT_TXT
        call print_error('SAVERESUMEFLAG is active with no file format specified.')
        ierr = 1
        return
    end if

    !> Echo configuration.
    if (vs%flgs%save%state == FLAG_ON .or. vs%flgs%save%state == FLAG_AUTO) then
        line = ''
        if (vs%flgs%save%freq == FREQ_YLY) line = ' yearly' // trim(line)
        if (vs%flgs%save%freq == FREQ_MLY) line = ' monthly' // trim(line)
        if (index(vs%flgs%save%bin, '+CLASSPROG') > 0) line = ' only class' // trim(line)
        if (index(vs%flgs%save%bin, '+STASONLY') > 0) line = ' only states' // trim(line)
        if (btest(vs%flgs%save%flo%ffmt, FFMT_R2C)) line = ' r2c' // trim(line)
        if (btest(vs%flgs%save%flo%ffmt, FFMT_CSV)) line = ' csv' // trim(line)
        if (btest(vs%flgs%save%flo%ffmt, FFMT_SEQ)) line = ' seq' // trim(line)
        if (btest(vs%flgs%save%flo%ffmt, FFMT_TXT)) line = ' txt' // trim(line)
        if (vs%flgs%save%state == FLAG_AUTO) then
            line = ' auto' // trim(line)
        else
            line = ' on' // trim(line)
        end if
        line = 'SAVERESUMEFLAG' // trim(line)
        call print_message('SAVERESUMEFLAG is ACTIVE: ' // trim(line))
    else
        call print_message('SAVERESUMEFLAG is not active: SAVERESUMEFLAG off')
    end if

!temp
    !> Auto-save/resume does not currently support option 4. Throw an error.
    if (vs%flgs%save%freq /= FREQ_NUL .and. &
        .not. (index(vs%flgs%save%bin, '+STASONLY') > 0 .or. index(vs%flgs%save%bin, '+CLASSPROG') > 0)) then
        call print_error('Auto-save of resume files does not currently support this option.')
        call print_message("The 'monthly' and 'yearly' options can added only to these configurations of SAVERESUMEFLAG:")
        call print_message_detail('SAVERESUMEFLAG on seq only states')
        call print_message_detail('SAVERESUMEFLAG on seq only class')
        call print_message_detail('SAVERESUMEFLAG 5')
        call print_message_detail('SAVERESUMEFLAG 3')
        ierr = 1
        return
    end if

    !> Check for auto resume file.
    if (vs%flgs%resume%state == FLAG_AUTO) then
        fname = 'auto_resume.ini'
        call reset_tab()
        call print_message('READING: ' // trim(fname))
        call increase_tab()
        inquire(file = fname, exist = lstate)
        if (lstate) then

            !> Open the file.
            iun = 100
            open(iun, file = fname, status = 'old', action = 'read', iostat = z)
            if (z /= 0) then
                call print_error('Unable to open the file.')
                call program_abort()
            end if

            !> Read the simulation start date from the file.
            read(iun, *, iostat = z) ic%start%year, ic%start%jday, ic%start%hour, ic%start%mins
            if (z /= 0) then
                call print_error('Unable to read the resume date from the file.')
                call program_abort()
            end if
            write(line, "(i5, '/', i3.3, ' ', i2.2, ':', i2.2)") ic%start%year, ic%start%jday, ic%start%hour, ic%start%mins
            call print_message( &
                'Simulation start date revised to: ' // trim(adjustl(line)) // '. The previous run state will be resumed.')
            close(iun)
        else

            !> Print a warning if the resume file does not exist.
            call print_warning( &
                'Auto-resume is active but ' // trim(fname) // ' cannot be found. A previous run state will not be resumed.')
            call print_message('RESUMEFLAG is revised: RESUMEFLAG off')

            !> Override the resume functionality.
            vs%flgs%resume%state = FLAG_OFF
        end if
    end if

end subroutine
