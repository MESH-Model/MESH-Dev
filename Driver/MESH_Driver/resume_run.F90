module resume_run

    !> 'mesh_io': For I/O field and file types, options and routines.
    use mesh_io

    implicit none

    !* SAVE/RESUMEFLAG: Saves or resume states from file.
    !>  Legacy options:
    !>      - 0: Disabled (new option: none).
    !>      - 1: Not supported.
    !>      - 2: Not supported.
    !>      - 3: CLASS prognostic states in binary sequential format (new option: seq only class).
    !>      - 4: All resume variables in binary sequential format (new option: seq).
    !>      - 5: All prognostic states in binary sequential format (new option: seq only states).
    !>  Options:
    !>      - FLAG_OFF: Do not save or resume the run state to and from file (default).
    !>      - FLAG_ON: Save and resume run state to and from file.
    !>      - FLAG_AUTO: Automatically resume the run state in the presence of auto_resume.ini (RESUMEFLAG only).
    !>  File format options (enables SAVERESUMEFLAG):
    !>      - FILE_TYPE_SEQ: Sequential binary format.
    !>  Output frequency options (default is only at the end of the run):
    !>      - FREQ_MLY: Before the beginning of the next month.
    !>      - FREQ_YLY: Before the beginning of the next year.
    character(len = LONG_FIELD_LENGTH), save :: RESUMEFLAG = 'none'
    character(len = LONG_FIELD_LENGTH), save :: SAVERESUMEFLAG = 'none'

    !> Type: io_state_flag
    !>
    !> Variables:
    !*  active: .true. if active.
    !*  freq: Frequency for I/O functions that are repeated.
    !*  bin: Read/write directives.
    !*  flo: File properties.
    type io_state_flag
        integer :: state = FLAG_OFF
        integer :: freq = FREQ_NUL
        character(len = LONG_FIELD_LENGTH) :: bin = ''
        type(file_info) flo
    end type

    !> Type: io_state_flags
    !>  Container for types of 'io_state_flag'.
    type io_state_flags
        type(io_state_flag) save, resume, assim
    end type

    type(io_state_flags), save :: resume_options

end module
