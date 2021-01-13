module mesh_io_options

    implicit none

    !> Constants for file formats.
    !* FFMT_NUL: None/no file format applicable or not set.
    !* FFMT_R2C: Ensim/Green Kenue R2C format (multi-attribute single framed or single attribute multi-frame).
    !* FFMT_TXT: Whitespace delimited plain text format.
    !* FFMT_CSV: Comma-delimited plain text format.
    !* FFMT_SEQ: Binary sequential format with no predefined structure.
    integer, parameter :: FFMT_NUL = 0
    integer, parameter :: FFMT_R2C = 1
    integer, parameter :: FFMT_TXT = 2
    integer, parameter :: FFMT_CSV = 3
    integer, parameter :: FFMT_SEQ = 4

    !> Constants for field lengths.
    !* FLEN_FIELD: Default length of any field.
    !* FLEN_FPATH: Default length of any file path.
    integer, parameter :: FLEN_FIELD = 200
    integer, parameter :: FLEN_FPATH = 2000

    !> Type for generic file definition with format and path information.
    !* iun: File handle/unit.
    !* ffmt: File format (multiple formats supported via btest).
    !* fpath: File path.
    type io_file
        integer :: iun = -1
        integer :: ffmt = FFMT_NUL
        character(len = FLEN_FIELD) :: fpath = ''
    end type

    !> Constants for binary flag states.
    !* FLAG_OFF: Disabled/inactive.
    !* FLAG_ON: Enabled/active.
    !* FLAG_AUTO: Automatic (e.g., if dependent on other flags disabled or enabled).
    integer, parameter :: FLAG_OFF = 0
    integer, parameter :: FLAG_ON = 1
    integer, parameter :: FLAG_AUTO = 2

    !> Constants for time frequencies of inputs and outputs.
    !* FREQ_NUL: None/no frequency applicable or not set.
    !* FREQ_YLY: Yearly, before the beginning of the next year.
    !* FREQ_MLY: Monthly, before the beginning of the next month.
    !* FREQ_DLY: Daily, before the beginning of the next day.
    !* FREQ_HLY: Hourly, before the beginning of the next hour.
    !* FREQ_PTS: Per model time-step (model configuration dependent).
    !* FREQ_NOW: Instantaneous (e.g., if dependent on a particular time or process).
    !* FREQ_SECS: At the end of a pre-defined increment in seconds.
    !* FREQ_MONS: At the end of a pre-defined increment in minutes.
    !* FREQ_HRS: At the end of a pre-defined increment in hours.
    !* FREQ_DAYS: At the end of a pre-defined increment in days.
    !* FREQ_IC: A pre-defined 'ic' counter date, where values matched are those greater than zero.
    integer, parameter :: FREQ_NUL = 0
    integer, parameter :: FREQ_YLY = 1
    integer, parameter :: FREQ_MLY = 2
    integer, parameter :: FREQ_DLY = 3
    integer, parameter :: FREQ_HLY = 4
    integer, parameter :: FREQ_PTS = 5
    integer, parameter :: FREQ_NOW = 6
    integer, parameter :: FREQ_SECS = 7
    integer, parameter :: FREQ_MONS = 8
    integer, parameter :: FREQ_HRS = 9
    integer, parameter :: FREQ_DAYS = 10
    integer, parameter :: FREQ_IC = 11

    contains

    !> Subroutine to initialize file object with optionally provided variables.
    subroutine io_file_init(flo, iun, ffmt, fpath)

        !> Input/output variables.
        !* flo: File object.
        type(io_file) flo

        !> Optional variables.
        !* iun: File handle/unit.
        !* ffmt: File format (multiple formats supported via btest).
        !* fpath: File path.
        integer, optional :: iun
        integer, optional :: ffmt
        character(len = *), optional :: fpath

        !> Assign values.
        if (present(iun)) then
            flo%iun = iun
        else
            flo%iun = -1
        end if
        if (present(ffmt)) then
            flo%ffmt = ffmt
        else
            flo%ffmt = FFMT_NUL
        end if
        if (present(fpath)) then
            flo%fpath = fpath
        else
            flo%fpath = ''
        end if

    end subroutine

end module
