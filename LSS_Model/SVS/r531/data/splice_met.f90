program splice_met

    implicit none

    character(len=1000) line, field
    integer :: ierr = 0, j, i

    !> Input met file.
    open(10, file='02BA003.met', action='read', iostat = ierr)

    !> Output files.
    open(21, file='met1.r2c')
    open(22, file='met2.r2c')
    open(23, file='met3.r2c')
    open(24, file='met4.r2c')
    open(25, file='met5.r2c')
    open(26, file='met6.r2c')
    open(27, file='met7.r2c')

    !> Splice records in the file.
    do while (ierr == 0)

        !> Read line from file.
        read(10, '(a)', iostat = ierr) line
        if (ierr /= 0) exit

        !> Split by the tab character.
        j = 1
        do i = 1, len_trim(line)
            if (iachar(line(i:i)) == 9) then

                !> Write the end of the line if a tab character.
                write(20 + j, '(a)')

                !> Increment the counter for the next file.
                j = j + 1
            else

                !> Append the character to the line.
                write(20 + j, '(a)', advance = 'no') line(i:i)
            end if
        end do

        !> Write the end of the line.
        !> This assumes it doesn't end with tab character.
        write(20 + j, '(a)')
    end do

    !> Close files.
    close(10)
    close(21)
    close(22)
    close(23)
    close(24)
    close(25)
    close(26)
    close(27)

end program