      subroutine READ_SOIL_LEVELS(shd, fls)

      use sa_mesh_shared_variables
      use model_files_variables

      implicit none

      !> Input variables.
      type(ShedGridParams) :: shd
      type(fl_ids) :: fls

      !> Local variables.
      integer :: IGND = 0, ierr, iun, i
      real DELZ_TEST, ZBOT_TEST

      !> Determine the value of IGND from MESH_input_soil_levels.txt
      shd%lc%IGND = 0

      !> Open soil levels file and check for IOSTAT errors.
      iun = fls%fl(mfk%f52)%iun
      open(iun,
     &     file = trim(adjustl(fls%fl(mfk%f52)%fn)),
     &     status = 'old',
     &     action = 'read',
     &     iostat = ierr)

      !> Check if there was an error opening the file.
      if (ierr /= 0) then
        print 1002
        stop
      else if (ro%VERBOSEMODE > 0) then
        write(6, '(a)', advance = 'no')
     +    'READING: MESH_input_soil_levels.txt'
      end if

      !> Count the number of soil layers.
      DELZ_TEST = 1.0
      do while (DELZ_TEST /= 0.0 .and. ierr == 0)
        read(iun, *, iostat = ierr) DELZ_TEST
        IGND = IGND + 1
      end do

      !> because IGND increments the first time that IGND_TEST = 0.0
      IGND = IGND - 1
      shd%lc%IGND = IGND

!todo: put in a warning that at least 3 layers are needed.

      rewind(iun)

      allocate(shd%lc%sl%DELZ(IGND), shd%lc%sl%ZBOT(IGND))
      if (ro%DIAGNOSEMODE > 0) print 1011
      do i = 1, IGND
        read(iun, *) DELZ_TEST
        shd%lc%sl%DELZ(i) = DELZ_TEST
        if (i > 1 .and. IGND > 2) then
          shd%lc%sl%ZBOT(i) = shd%lc%sl%ZBOT(i - 1) + DELZ_TEST
        else
          shd%lc%sl%ZBOT(i) = DELZ_TEST
        end if
        if (ro%DIAGNOSEMODE > 0) then
          print 1012, i, shd%lc%sl%DELZ(i), shd%lc%sl%ZBOT(i)
        end if
      end do

      close(iun)
      if (ro%VERBOSEMODE > 0) then
        write(6, *) ' READ: SUCCESSFUL, FILE: CLOSED'
      end if

1002  format(
     +  /1x, 'MESH_input_soil_levels.txt could not be opened.',
     +  /1x, 'Ensure that the file exists and restart the program.', /)
1011  format(/1x, 'Soil layer summary (IGND)')
1012  format(3x, 'Soil layer (IGND): ', i6, 2f10.3)

      return

      end subroutine
