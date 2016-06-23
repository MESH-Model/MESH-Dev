      subroutine READ_SOIL_INI(shd, fls)

      use sa_mesh_shared_variabletypes
      use sa_mesh_shared_variables
      use model_files_variabletypes
      use model_files_variables
      use RUNCLASS36_variables

!todo: remove this
	  use FLAGS

      !> Input variables.
      type(ShedGridParams) :: shd
      type(fl_ids) :: fls

      !> Local variables.
      integer NA, NTYPE, IGND, ierr, iun, j, m, i

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

      NA = shd%NA
      NTYPE = shd%lc%NTYPE
      IGND = shd%lc%IGND

      !> Read variables from the file.
      read(iun, *)
      read(iun, *) (sv%wc_thpor(1, m, 1), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (sv%wc_thpor(1, m, 2), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (sv%wc_thpor(1, m, 3), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (sv%wc_thlret(1, m, 1), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (sv%wc_thlret(1, m, 2), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (sv%wc_thlret(1, m, 3), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (sv%wc_thlmin(1, m, 1), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (sv%wc_thlmin(1, m, 2), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (sv%wc_thlmin(1, m, 3), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (sv%wc_bi(1, m, 1), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (sv%wc_bi(1, m, 2), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (sv%wc_bi(1, m, 3), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (sv%wc_psisat(1, m, 1), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (sv%wc_psisat(1, m, 2), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (sv%wc_psisat(1, m, 3), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (sv%wc_grksat(1, m, 1), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (sv%wc_grksat(1, m, 2), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (sv%wc_grksat(1, m, 3), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (sv%wc_hcps(1, m, 1), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (sv%wc_hcps(1, m, 2), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (sv%wc_hcps(1, m, 3), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (sv%wc_tcs(1, m, 1), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (sv%wc_tcs(1, m, 2), m = 1, NTYPE)
      read(iun, *)
      read(iun, *) (sv%wc_tcs(1, m, 3), m = 1, NTYPE)

      close(iun)

      !> Distribute the variables.
      do i = 1, NA
        do m = 1, NTYPE
          do j = 1, 3
            sv%wc_thpor(i, m, j) = sv%wc_thpor(1, m, j)
            sv%wc_thlret(i, m, j) = sv%wc_thlret(1, m, j)
            sv%wc_thlmin(i, m, j) = sv%wc_thlmin(1, m, j)
            sv%wc_bi(i, m, j) = sv%wc_bi(1, m, j)
            sv%wc_psisat(i, m, j) = sv%wc_psisat(1, m, j)
            sv%wc_grksat(i, m, j) = sv%wc_grksat(1, m, j)
            sv%wc_hcps(i, m, j) = sv%wc_hcps(1, m, j)
            sv%wc_tcs(i, m, j) = sv%wc_tcs(1, m, j)
          end do
          do j=4,IGND
            sv%wc_thpor(i, m, j) = sv%wc_thpor(i, m, 3)
            sv%wc_thlret(i, m, j) = sv%wc_thlret(i, m, 3)
            sv%wc_thlmin(i, m, j) = sv%wc_thlmin(i, m, 3)
            sv%wc_bi(i, m, j) = sv%wc_bi(i, m, 3)
            sv%wc_psisat(i, m, j) = sv%wc_psisat(i, m, 3)
            sv%wc_grksat(i, m, j) = sv%wc_grksat(i, m, 3)
            sv%wc_hcps(i, m, j) = sv%wc_hcps(i, m, 3)
            sv%wc_tcs(i, m, j) = sv%wc_tcs(i, m, 3)
          end do
        end do
      end do

      return

      end subroutine
