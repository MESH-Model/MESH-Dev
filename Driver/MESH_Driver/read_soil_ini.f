      subroutine READ_SOIL_INI(shd, fls)

      use sa_mesh_shared_variables
      use sa_mesh_shared_output_variables
      use model_files_variables

!todo: remove this
	  use FLAGS

      !> Input variables.
      type(ShedGridParams) :: shd
      type(fl_ids) :: fls

      !> Local variables.
      integer NTYPE, NSL, ierr, iun, m, j

!> *********************************************************************
!>  Open and read in values from soil.ini file
!>  Bruce Davison, August 13, 2004
!>  Changes to the soil parameters so that they're read-in directly.
!>  Read in the soil parameters that used to be calculated from %sand, %clay
!> *********************************************************************

      if (SOILINIFLAG /= 5) return

      iun = fls%fl(mfk%f54)%iun
      open(iun, file=adjustl(trim(fls%fl(mfk%f54)%fn)), status='old',
     &     action='read', iostat=ierr)

      !> Check to see if the file exists.
      if (ierr /= 0) then
        print *, 'ERROR: The soil.ini file was not found.'
        print *, 'You can set SOILINIFLAG to ',
     &   	     'values less than 5 and MESH will ',
     &           'use soil percentages ',
     &	         'from MESH_parameters_CLASS.ini file.'
        print *, 'Below is what MESH will do if the sum of soil ',
     &           'percentages is greater than 100%:'
		print *, 'For SOILINIFLAG set to 1 - ',
     &  		 'MESH will use the soil percentages as specified'
		print *, 'For SOILINIFLAG set to 2 - ',
     &	         'MESH will adjust soil percentages in favor of sand'
		print *, 'For SOILINIFLAG set to 3 - ',
     &	         'MESH will adjust soil percentages in favor of clay'
		print *, 'For SOILINIFLAG set to 4 - ',
     &	         'MESH will proportionally adjust the soil percentages'
		stop
      end if

      if (ro%VERBOSEMODE > 0) then
        print *, 'The soil.ini file was found'
        print *, 'CLASSBHYD.f will be used'
      end if

      NTYPE = shd%lc%NTYPE
      NSL = shd%lc%IGND

      !> Read variables from the file.
      read(iun, *)
      read(iun, *) (pmrow%slp%thpor(m, 1), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (pmrow%slp%thpor(m, 2), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (pmrow%slp%thpor(m, 3), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (pmrow%slp%thlret(m, 1), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (pmrow%slp%thlret(m, 2), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (pmrow%slp%thlret(m, 3), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (pmrow%slp%thlmin(m, 1), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (pmrow%slp%thlmin(m, 2), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (pmrow%slp%thlmin(m, 3), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (pmrow%slp%bi(m, 1), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (pmrow%slp%bi(m, 2), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (pmrow%slp%bi(m, 3), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (pmrow%slp%psisat(m, 1), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (pmrow%slp%psisat(m, 2), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (pmrow%slp%psisat(m, 3), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (pmrow%slp%grksat(m, 1), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (pmrow%slp%grksat(m, 2), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (pmrow%slp%grksat(m, 3), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (pmrow%slp%hcps(m, 1), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (pmrow%slp%hcps(m, 2), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (pmrow%slp%hcps(m, 3), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (pmrow%slp%tcs(m, 1), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (pmrow%slp%tcs(m, 2), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (pmrow%slp%tcs(m, 3), m = 1, NTYPE)

      close(iun)

      !> Distribute the variables.
      do m = 1, NTYPE
        do j = 4, IGND
          pmrow%slp%thpor(m, j) = pmrow%slp%thpor(m, 3)
          pmrow%slp%thlret(m, j) = pmrow%slp%thlret(m, 3)
          pmrow%slp%thlmin(m, j) = pmrow%slp%thlmin(m, 3)
          pmrow%slp%bi(m, j) = pmrow%slp%bi(m, 3)
          pmrow%slp%psisat(m, j) = pmrow%slp%psisat(m, 3)
          pmrow%slp%grksat(m, j) = pmrow%slp%grksat(m, 3)
          pmrow%slp%hcps(m, j) = pmrow%slp%hcps(m, 3)
          pmrow%slp%tcs(m, j) = pmrow%slp%tcs(m, 3)
        end do
      end do

      return

      end subroutine
