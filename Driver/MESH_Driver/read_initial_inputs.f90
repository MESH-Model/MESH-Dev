subroutine READ_INITIAL_INPUTS(shd, ts, cm, fls)

    use mpi_shared_variables
    use mpi_utilities
    use sa_mesh_shared_parameters
    use sa_mesh_shared_variables
    use sa_mesh_shared_output_variables
    use model_files_variabletypes
    use model_files_variables
    use model_dates
    use FLAGS
    use climate_forcing

    use RUNCLASS36_constants
    use RUNCLASS36_variables
    use RUNCLASS36_save_output
    use cropland_irrigation_variables, only: cip, ciprot, cifg

    implicit none

!> DECLARE THE READ IN VARIABLES.
!> ----------------------------
!> VALUES NEEDED for drainage_database and/or new_shd.r2c
!      integer
!     +  WF_LAND_COUNT,
!     +  LATDEGMIN, LATMINMIN, LATDEGMAX, LATMINMAX,
!     +  LONDEGMIN, LONMINMIN, LONDEGMAX, LONMINMAX
!      real WF_LAND_MAX, WF_LAND_SUM
!> declared in MESH_INPUT_MODULE
!>  BASIN_FRACTION, WF_NHYD,
!>  WF_QR, WF_QBASE, WF_QI2, WF_QO1, WF_QO2, WF_STORE1, WF_STORE2,
!>  WF_QI1

    character(8) RELEASE

    !> The types that contain allocatable values
    type(ShedGridParams) :: shd
    type(CLIM_INFO) :: cm
    type(dates_model) :: ts
    type(fl_ids):: fls

    !> Local variables.
    integer NA, NTYPE, NML, NSL, ierr, k, n, i, m, j

!> ====================================
!> read the RUN_OPTIONS input file called "MESH_input_run_options.ini"
!> and SET or RESET any CONTROL FLAGS
!> and READ the GRID OUTPUT DIRECTORIES.
    call READ_RUN_OPTIONS(ts, cm, fls)

    !> Open status file.
    if (ipid == 0 .and. MODELINFOOUTFLAG > 0) then
        open(58, file = './' // trim(fls%GENDIR_OUT) // '/MESH_output_echo_print.txt')
    end if

!> And Open and read in values from new_shd.r2c file
!> *********************************************************************
!> DRAINAGE DATABASE (BASIN SHD) (DRAINAGE_DATABASE.TXT):
!> IS NO LONGER USED.  DRAINAGE_DATABASE.TXT HAS BEEN REPLACED WITH
!> THE BASIN SHD FILE.  READ_SHED_EF, FROM STAND-ALONE RTE.EXE
!> (WATROUTE), IS CALLED TO READ THE NEW FILE.
    if (SHDFILEFLAG == 1) then

    open(fls%fl(mfk%f20)%iun, file=adjustl(trim(fls%fl(mfk%f20)%fn)), status='old', iostat=ierr)
        if (ierr == 0) then
            close(fls%fl(mfk%f20)%iun)
            print *, 'Reading Drainage Database from MESH_drainage_database.r2c'
            call READ_SHED_EF(fls, mfk%f20, shd)
            write(6, *) ' READ: SUCCESSFUL, FILE: CLOSED'
!>
!>*******************************************************************
!>
!          allocate(
!     &      BASIN_FRACTION(NA))
!+        ALLOCATE (WF_NHYD(NA), WF_QR(NA),
!+     &  WF_QBASE(NA), WF_QI2(NA), WF_QO1(NA), WF_QO2(NA),
!+     &  WF_STORE1(NA), WF_STORE2(NA), WF_QI1(NA), SNOGRD(NA),
!+     &  )
!          BASIN_FRACTION(1) = -1
            shd%lc%ILG = shd%NA*shd%lc%NTYPE
        else
            print *, 'ERROR with event.evt or new_shd.r2c'
            stop
        end if

    else if (SHDFILEFLAG == 0) then

!> *********************************************************************
!> Open and read in values from MESH_input_drainage_database.txt file
!>   if new_shd.r2c file was not found
!> *********************************************************************
!        open(UNIT=20, FILE='MESH_input_drainage_database.txt',
!     &    STATUS='OLD', IOSTAT=ierr)
!        if (ierr == 0) then
!          print *, 'Reading Drainage Database from ',
!     &      'MESH_input_drainage_database.txt'
!        else
!          print *, 'MESH_input_drainage_database.txt not found'
!          stop
!        end if
!        read(20, '(i5, 50x, i5)') NA, NAA
!        read(20, '(f10.0, 5x, 2i5)') AL, NRVR, NTYPE
!        GRDN = 0.0
!        GRDE = 0.0

!todo change the code or documentation on these variables.
!ANDY Set ILG from the read-in values
!        ILG = NA*NTYPE

!> Using IOSTAT allows us to try to read input that may or may not exist.
!> If all of the values successfully get read, IOSTAT=VarName will set
!> VarName to 0. If all of the values were not successfully read,
!> VarName would be set to 1 or more. In this case, the VarName that
!> we are using is ierr.
!        read(20, '(12i5, 2f5.0)', IOSTAT=ierr) IYMIN, WF_IYMAX,
!     &    JXMIN, WF_JXMAX, LATDEGMIN, LATMINMIN, LATDEGMAX, LATMINMAX,
!     &    LONDEGMIN, LONMINMIN, LONDEGMAX, LONMINMAX, GRDN, GRDE

!> Condition for Lat/Long by Frank S Sept/1999
!        if (GRDN > 0.0) then
!          IYMIN = LATDEGMIN*60 + LATMINMIN
!          WF_IYMAX = LATDEGMAX*60 + LATMINMAX
!          JXMIN = LONDEGMIN*60 + LONMINMIN
!          WF_JXMAX = LONDEGMAX*60 + LONMINMAX

!        else
!> Define GRDN & GRDE for UTM
!          GRDN = AL/1000.0
!          GRDE = AL/1000.0
!        end if
!        read(20, '(2i5)') YCOUNT, XCOUNT

!> check if we are going to get an "array bounds out of range" error
!        if (YCOUNT > 100) then
!          write(6, *) 'WARNING: The height of the basin is very high.',
!     *      'This may negatively impact performance.'
!-+          PRINT *, 'size of grid arrays in MESH: ',M_Y
!-+          PRINT *, 'number up/down (north/south) ',
!-+     &             'grids from MESH_drainage_database.txt'
!-+     PRINT *, ' file: ',YCOUNT
!-+          PRINT *, 'Please adjust these values.'
!-+          STOP
!        end if

!        if (XCOUNT > 100) then
!          write(6, *) 'WARNING: The width of the basin is very high. ',
!     *     'This may negatively impact performance.'
!-+          PRINT *, 'size of grid arrays in MESH: ',M_X
!-+          PRINT *, 'no. of east/west (left/right) grids from ',
!-+     &             'MESH_drainage_database.txt'
!-+     PRINT *, ' file: ',XCOUNT
!-+          PRINT *, 'Please adjust these values.'
!-+          STOP
!        end if

!ANDY Allocation of variables that use NA and NTYPE
!        allocate(WF_IBN(NA), WF_IROUGH(NA),
!     &    WF_ICHNL(NA), WF_NEXT(NA), WF_ELEV(NA), WF_IREACH(NA),
!     &    WF_DA(NA), WF_BNKFLL(NA), WF_CHANNELSLOPE(NA),
!     &    FRAC(NA), BASIN_FRACTION(NA))
!        allocate(ACLASS(NA, NTYPE))

!ANDY Zero everything we just allocated
!        do i = 1, NA
!          do j = 1, NTYPE
!            ACLASS(i, j) = 0
!          end do
!        end do
!        do i = 1, NA
!          WF_IBN(i) = 0
!          WF_IROUGH(i) = 0
!          WF_ICHNL(i) = 0
!          WF_NEXT(i) = 0
!          WF_ELEV(i) = 0
!          WF_IREACH(i) = 0
!          WF_DA(i) = 0
!          WF_BNKFLL(i) = 0
!          WF_CHANNELSLOPE(i) = 0
!          FRAC(i) = 0
!          BASIN_FRACTION(i) = 0
!        end do

        !Set this to ensure basin fraction will be set later on
!        BASIN_FRACTION(1) = -1

!        allocate(YYY(NA), XXX(NA))

!        do i = 1, YCOUNT
!          read(20, *)
!        end do

!        do i = 1, NA
!          read(20, '(5x, 2i5, 3f10.5, i7, 5i5, f5.2, 15f5.2)') YYY(i),
!     &      XXX(i), WF_DA(i), WF_BNKFLL(i), WF_CHANNELSLOPE(i),
!     &      WF_ELEV(i), WF_IBN(i), WF_IROUGH(i), WF_ICHNL(i),
!     &      WF_NEXT(i), WF_IREACH(i), FRAC(i),
!     &      (ACLASS(i, j), j = 1, NTYPE)
!> check to make sure land cover areas sum to 100%
!          WF_LAND_COUNT = 1
!          WF_LAND_MAX = 0.0
!          WF_LAND_SUM = 0.0
!          do j = 1, NTYPE
!            WF_LAND_SUM = WF_LAND_SUM + ACLASS(i, j)
!            if (ACLASS(i, j) > WF_LAND_MAX) then
!              WF_LAND_COUNT = j
!              WF_LAND_MAX = ACLASS(i, j)
!            end if
!          end do
!          if (WF_LAND_SUM /= 1.0) THEN
!            ACLASS(i, WF_LAND_COUNT) =
!     &        ACLASS(i, WF_LAND_COUNT) - (WF_LAND_SUM - 1.0)
!          end if
!        end do

!        close(20)

    end if ! IF SHDFILE...

    !> Assign shd values to local variables.
    NA = shd%NA
    NTYPE = shd%lc%NTYPE

    if (shd%xCount > 100) then
        write(6, *) &
            'WARNING: The width of the basin is very high. ', &
            'This may negatively impact performance.'
    end if
    if (shd%yCount > 100) then
        write(6, *) &
            'WARNING: The height of the basin is very high. ', &
            'This may negatively impact performance.'
    end if
    if (shd%lc%ILG > 1500) then
        write(6, *) &
            'WARNING: The number of grid squares in the basin', &
            ' is very high. This may negatively impact performance.'
    end if

    if (allocated (shd%ylat)) deallocate (shd%ylat)
    if (allocated (shd%xlng)) deallocate (shd%xlng)

    !> Determine coordinates for intermediate grid locations.
    allocate(shd%ylat(NA), shd%xlng(NA))
    do i = 1, NA
            !LATLENGTH = shd%AL/1000.0/(111.136 - 0.5623*cos(2*(DEGLAT*PI/180.0)) + 0.0011*cos(4*(DEGLAT*PI/180.0)))
            !LONGLENGTH = shd%AL/1000.0/(111.4172*cos((DEGLAT*PI/180.0)) - 0.094*cos(3*(DEGLAT*PI/180.0)) + 0.0002*cos(5*(DEGLAT*PI/180.0)))
        shd%ylat(i) = (shd%yOrigin + shd%yDelta*shd%yyy(i)) - shd%yDelta/2.0
        shd%xlng(i) = (shd%xOrigin + shd%xDelta*shd%xxx(i)) - shd%xDelta/2.0
    end do

    !> Determine the number of active tile elements.
!todo: fix this.
    shd%wc%ILG = shd%lc%ILG

    if (allocated(shd%lc%ILMOS)) deallocate(shd%lc%ILMOS)
    if (allocated(shd%lc%JLMOS)) deallocate(shd%lc%JLMOS)
    if (allocated(shd%wc%ILMOS)) deallocate(shd%wc%ILMOS)
    if (allocated(shd%wc%JLMOS)) deallocate(shd%wc%JLMOS)

    allocate(shd%lc%ILMOS(shd%lc%ILG), shd%lc%JLMOS(shd%lc%ILG), &
             shd%wc%ILMOS(shd%wc%ILG), shd%wc%JLMOS(shd%wc%ILG))

    !> Count the number of tiles that are land 'lc' or water 'wc' and
    !> store the respective ID's of the grid and GRU in the 'ILMOS' and
    !> 'JLMOS' variables.
    shd%lc%NML = 0
    shd%wc%NML = 0
    do i = 1, NA
        if (shd%FRAC(i) > 0.0) then
            do m = 1, NTYPE

                !> Only count active GRUs (with > 0.0 contributing fraction).
                if (shd%lc%ACLASS(i, m) > 0.0) then
                    if (shd%IAK(i) > 0) then

                        !> Land.
                        shd%lc%NML = shd%lc%NML + 1
                        shd%lc%ILMOS(shd%lc%NML) = i
                        shd%lc%JLMOS(shd%lc%NML) = m

                    else

                        !> Water.
                        shd%wc%NML = shd%wc%NML + 1
                        shd%wc%ILMOS(shd%wc%NML) = i
                        shd%wc%JLMOS(shd%wc%NML) = m

                    end if
                end if

            end do
        end if
    end do

    !> Write information about tile configuration to file.
    if (ipid == 0 .and. MODELINFOOUTFLAG > 0 .and. ro%DIAGNOSEMODE > 0) then

        !> Land tiles.
        write(58, 1210) 'land', 'NML', shd%lc%NML
        if (shd%lc%NML > 0) then
            write(58, 1910) 'Index', 'Grid', 'GRU'
            do k = 1, shd%lc%NML
                write(58, 1910) k, shd%lc%ILMOS(k), shd%lc%JLMOS(k)
            end do
        end if

        !> Water tiles.
        write(58, 1210) 'water', 'NMW', shd%wc%NML
        if (shd%wc%NML > 0) then
            write(58, 1910) 'Index', 'Grid', 'GRU'
            do k = 1, shd%wc%NML
                write(58, 1910) k, shd%wc%ILMOS(k), shd%wc%JLMOS(k)
            end do
        end if

    end if

1210    format(/1x, 'Configuration of ', (a), ' tiles', &
               /1x, 'Total number (', (a), '): ', g16.9)
1910    format(3(g16.9))

    !> Store the number of active tile elements to initialize variables.
    NML = shd%lc%NML

    !> Calculate the operational indices in the current node.
    call mpi_split_nml(inp, izero, ipid, NML, shd%lc%ILMOS, il1, il2, ilen)
    if (ro%DIAGNOSEMODE > 0) print 1062, ipid, NML, ilen, il1, il2

1062    format(/1x, 'Configuration and distribution of the domain', &
               /3x, 'Current process: ', i10, &
               /3x, 'Tile land elements: ', i10, &
               /3x, 'Length of single array: ', i10, &
               /3x, 'Starting index: ', i10, &
               /3x, 'Stopping index: ', i10, /)

    !> Open and read in soil depths from file.
    call READ_SOIL_LEVELS(shd, fls)
    print *, 'IGND = ', shd%lc%IGND

    !> Store the number of soil layers to initialize variables.
    NSL = shd%lc%IGND

    if (allocated(pm%tp%gc)) deallocate (pm%tp%gc)
    if (allocated(pm%tp%fare)) deallocate (pm%tp%fare)
    if (allocated(pm%tp%xslp)) deallocate (pm%tp%xslp)
    if (allocated(pm%tp%mid)) deallocate (pm%tp%mid)
    if (allocated(pm%cp%fcan)) deallocate ( pm%cp%fcan)
    if (allocated(pm%cp%z0or)) deallocate (pm%cp%z0or)
    if (allocated(pm%cp%lnz0)) deallocate (pm%cp%lnz0)
    if (allocated(pm%cp%alvc)) deallocate (pm%cp%alvc)
    if (allocated(pm%cp%alic)) deallocate (pm%cp%alic)
    if (allocated(pm%cp%lamx)) deallocate (pm%cp%lamx)
    if (allocated(pm%cp%lamn)) deallocate (pm%cp%lamn)
    if (allocated(pm%cp%cmas)) deallocate (pm%cp%cmas)
    if (allocated(pm%cp%root)) deallocate (pm%cp%root)
    if (allocated(pm%cp%rsmn)) deallocate (pm%cp%rsmn)
    if (allocated(pm%cp%qa50)) deallocate (pm%cp%qa50)
    if (allocated(pm%cp%vpda)) deallocate (pm%cp%vpda)
    if (allocated(pm%cp%vpdb)) deallocate (pm%cp%vpdb)
    if (allocated(pm%cp%psga)) deallocate (pm%cp%psga)
    if (allocated(pm%cp%psgb)) deallocate (pm%cp%psgb)
    if (allocated(pm%sfp%zbld)) deallocate (pm%sfp%zbld)
    if (allocated(pm%sfp%zrfh)) deallocate (pm%sfp%zrfh)
    if (allocated(pm%sfp%zrfm)) deallocate (pm%sfp%zrfm)
    if (allocated(pm%sfp%zplg)) deallocate (pm%sfp%zplg)
    if (allocated(pm%snp%zsnl)) deallocate (pm%snp%zsnl)
    if (allocated(pm%snp%zpls)) deallocate (pm%snp%zpls)
    if (allocated(pm%slp%sdep)) deallocate (pm%slp%sdep)
    if (allocated(pm%slp%alwet)) deallocate (pm%slp%alwet)
    if (allocated(pm%slp%aldry)) deallocate (pm%slp%aldry)
    if (allocated(pm%slp%delz)) deallocate (pm%slp%delz)
    if (allocated(pm%slp%zbot)) deallocate (pm%slp%zbot)
    if (allocated(pm%slp%sand)) deallocate (pm%slp%sand)
    if (allocated(pm%slp%clay)) deallocate (pm%slp%clay)
    if (allocated(pm%slp%orgm)) deallocate (pm%slp%orgm)
    if (allocated(pm%slp%thpor)) deallocate (pm%slp%thpor)
    if (allocated(pm%slp%thlret)) deallocate (pm%slp%thlret)
    if (allocated(pm%slp%thlmin)) deallocate (pm%slp%thlmin)
    if (allocated(pm%slp%thlrat)) deallocate (pm%slp%thlrat)
    if (allocated(pm%slp%bi)) deallocate (pm%slp%bi)
    if (allocated(pm%slp%psisat)) deallocate (pm%slp%psisat)
    if (allocated(pm%slp%psiwlt)) deallocate (pm%slp%psiwlt)
    if (allocated(pm%slp%grksat)) deallocate (pm%slp%grksat)
    if (allocated(pm%slp%thfc)) deallocate (pm%slp%thfc)
    if (allocated(pm%slp%hcps)) deallocate (pm%slp%hcps)
    if (allocated(pm%slp%tcs)) deallocate (pm%slp%tcs)
    if (allocated(pm%hp%drn)) deallocate (pm%hp%drn)
    if (allocated(pm%hp%dd)) deallocate (pm%hp%dd)
    if (allocated(pm%hp%grkf)) deallocate (pm%hp%grkf)
    if (allocated(pm%hp%mann)) deallocate (pm%hp%mann)
    if (allocated(pm%hp%ks)) deallocate (pm%hp%ks)

    !> Initialize parameter values.
    allocate(pm%tp%gc(NML), pm%tp%fare(NML), pm%tp%xslp(NML), pm%tp%mid(NML), &
             pm%cp%fcan(NML, ICP1), pm%cp%z0or(NML, ICP1), pm%cp%lnz0(NML, ICP1), pm%cp%alvc(NML, ICP1), pm%cp%alic(NML, ICP1), &
             pm%cp%lamx(NML, ICAN), pm%cp%lamn(NML, ICAN), pm%cp%cmas(NML, ICAN), pm%cp%root(NML, ICAN), pm%cp%rsmn(NML, ICAN), &
             pm%cp%qa50(NML, ICAN), pm%cp%vpda(NML, ICAN), pm%cp%vpdb(NML, ICAN), pm%cp%psga(NML, ICAN), pm%cp%psgb(NML, ICAN), &
             pm%sfp%zbld(NML), pm%sfp%zrfh(NML), pm%sfp%zrfm(NML), pm%sfp%zplg(NML), pm%snp%zsnl(NML), pm%snp%zpls(NML), &
             pm%slp%sdep(NML), pm%slp%alwet(NML), pm%slp%aldry(NML), &
             pm%slp%delz(NSL), pm%slp%zbot(NSL), &
             pm%slp%sand(NML, NSL), pm%slp%clay(NML, NSL), pm%slp%orgm(NML, NSL), &
             pm%slp%thpor(NML, NSL), pm%slp%thlret(NML, NSL), pm%slp%thlmin(NML, NSL), pm%slp%thlrat(NML, NSL), &
             pm%slp%bi(NML, NSL), pm%slp%psisat(NML, NSL), pm%slp%psiwlt(NML, NSL), pm%slp%grksat(NML, NSL), &
             pm%slp%thfc(NML, NSL), pm%slp%hcps(NML, NSL), pm%slp%tcs(NML, NSL), &
             pm%hp%drn(NML), pm%hp%dd(NML), pm%hp%grkf(NML), pm%hp%mann(NML), pm%hp%ks(NML))

    if (allocated (pmrow%tp%gc)) deallocate (pmrow%tp%gc)
    if (allocated (pmrow%tp%fare)) deallocate (pmrow%tp%fare)
    if (allocated (pmrow%tp%xslp)) deallocate (pmrow%tp%xslp)
    if (allocated (pmrow%tp%mid)) deallocate (pmrow%tp%mid)
    if (allocated (pmrow%cp%fcan)) deallocate (pmrow%cp%fcan)
    if (allocated (pmrow%cp%z0or)) deallocate (pmrow%cp%z0or)
    if (allocated (pmrow%cp%lnz0)) deallocate (pmrow%cp%lnz0)
    if (allocated (pmrow%cp%alvc)) deallocate (pmrow%cp%alvc)
    if (allocated (pmrow%cp%alic)) deallocate (pmrow%cp%alic)
    if (allocated (pmrow%cp%lamx)) deallocate (pmrow%cp%lamx)
    if (allocated (pmrow%cp%lamn)) deallocate (pmrow%cp%lamn)
    if (allocated (pmrow%cp%cmas)) deallocate (pmrow%cp%cmas)
    if (allocated (pmrow%cp%root)) deallocate (pmrow%cp%root)
    if (allocated (pmrow%cp%rsmn)) deallocate (pmrow%cp%rsmn)
    if (allocated (pmrow%cp%qa50)) deallocate (pmrow%cp%qa50)
    if (allocated (pmrow%cp%vpda)) deallocate (pmrow%cp%vpda)
    if (allocated (pmrow%cp%vpdb)) deallocate (pmrow%cp%vpdb)
    if (allocated (pmrow%cp%psga)) deallocate (pmrow%cp%psga)
    if (allocated (pmrow%cp%psgb)) deallocate (pmrow%cp%psgb)
    if (allocated (pmrow%sfp%zbld)) deallocate (pmrow%sfp%zbld)
    if (allocated (pmrow%sfp%zrfh)) deallocate (pmrow%sfp%zrfh)
    if (allocated (pmrow%sfp%zrfm)) deallocate (pmrow%sfp%zrfm)
    if (allocated (pmrow%sfp%zplg)) deallocate (pmrow%sfp%zplg)
    if (allocated (pmrow%snp%zsnl)) deallocate (pmrow%snp%zsnl)
    if (allocated (pmrow%snp%zpls)) deallocate (pmrow%snp%zpls)
    if (allocated (pmrow%slp%sdep)) deallocate (pmrow%slp%sdep)
    if (allocated (pmrow%slp%alwet)) deallocate (pmrow%slp%alwet)
    if (allocated (pmrow%slp%aldry)) deallocate (pmrow%slp%aldry)
    if (allocated (pmrow%slp%delz)) deallocate (pmrow%slp%delz)
    if (allocated (pmrow%slp%zbot)) deallocate (pmrow%slp%zbot)
    if (allocated (pmrow%slp%sand)) deallocate (pmrow%slp%sand)
    if (allocated (pmrow%slp%clay)) deallocate (pmrow%slp%clay)
    if (allocated (pmrow%slp%orgm)) deallocate (pmrow%slp%orgm)
    if (allocated (pmrow%slp%thpor)) deallocate (pmrow%slp%thpor)
    if (allocated (pmrow%slp%thlret)) deallocate (pmrow%slp%thlret)
    if (allocated (pmrow%slp%thlmin)) deallocate (pmrow%slp%thlmin)
    if (allocated (pmrow%slp%thlrat)) deallocate (pmrow%slp%thlrat)
    if (allocated (pmrow%slp%bi)) deallocate (pmrow%slp%bi)
    if (allocated (pmrow%slp%psisat)) deallocate (pmrow%slp%psisat)
    if (allocated (pmrow%slp%psiwlt)) deallocate (pmrow%slp%psiwlt)
    if (allocated (pmrow%slp%grksat)) deallocate (pmrow%slp%grksat)
    if (allocated (pmrow%slp%thfc)) deallocate (pmrow%slp%thfc)
    if (allocated (pmrow%slp%hcps)) deallocate (pmrow%slp%hcps)
    if (allocated (pmrow%slp%tcs)) deallocate (pmrow%slp%tcs)
    if (allocated (pmrow%hp%drn)) deallocate (pmrow%hp%drn)
    if (allocated (pmrow%hp%dd)) deallocate (pmrow%hp%dd)
    if (allocated (pmrow%hp%grkf)) deallocate (pmrow%hp%grkf)
    if (allocated (pmrow%hp%mann)) deallocate (pmrow%hp%mann)
    if (allocated (pmrow%hp%ks)) deallocate (pmrow%hp%ks)

    !> Initialize parameter values for output ('ROW' indexing).
    allocate(pmrow%tp%gc(1), pmrow%tp%fare(NTYPE), pmrow%tp%xslp(NTYPE), pmrow%tp%mid(NTYPE), &
             pmrow%cp%fcan(NTYPE, ICP1), pmrow%cp%z0or(NTYPE, ICP1), pmrow%cp%lnz0(NTYPE, ICP1), &
             pmrow%cp%alvc(NTYPE, ICP1), pmrow%cp%alic(NTYPE, ICP1), &
             pmrow%cp%lamx(NTYPE, ICAN), pmrow%cp%lamn(NTYPE, ICAN), pmrow%cp%cmas(NTYPE, ICAN), &
             pmrow%cp%root(NTYPE, ICAN), pmrow%cp%rsmn(NTYPE, ICAN), &
             pmrow%cp%qa50(NTYPE, ICAN), pmrow%cp%vpda(NTYPE, ICAN), pmrow%cp%vpdb(NTYPE, ICAN), &
             pmrow%cp%psga(NTYPE, ICAN), pmrow%cp%psgb(NTYPE, ICAN), &
             pmrow%sfp%zbld(1), pmrow%sfp%zrfh(1), pmrow%sfp%zrfm(1), &
             pmrow%sfp%zplg(NTYPE), pmrow%snp%zsnl(NTYPE), pmrow%snp%zpls(NTYPE), &
             pmrow%slp%sdep(NTYPE), pmrow%slp%alwet(NTYPE), pmrow%slp%aldry(NTYPE), &
             pmrow%slp%delz(NSL), pmrow%slp%zbot(NSL), &
             pmrow%slp%sand(NTYPE, NSL), pmrow%slp%clay(NTYPE, NSL), pmrow%slp%orgm(NTYPE, NSL), &
             pmrow%slp%thpor(NTYPE, NSL), pmrow%slp%thlret(NTYPE, NSL), pmrow%slp%thlmin(NTYPE, NSL), &
             pmrow%slp%thlrat(NTYPE, NSL), &
             pmrow%slp%bi(NTYPE, NSL), pmrow%slp%psisat(NTYPE, NSL), pmrow%slp%psiwlt(NTYPE, NSL), pmrow%slp%grksat(NTYPE, NSL), &
             pmrow%slp%thfc(NTYPE, NSL), pmrow%slp%hcps(NTYPE, NSL), pmrow%slp%tcs(NTYPE, NSL), &
             pmrow%hp%drn(NTYPE), pmrow%hp%dd(NTYPE), pmrow%hp%grkf(NTYPE), pmrow%hp%mann(NTYPE), pmrow%hp%ks(NTYPE))

    if (allocated (stas%cnpy%qac)) deallocate (stas%cnpy%qac)
    if (allocated (stas%cnpy%rcan)) deallocate (stas%cnpy%rcan)
    if (allocated (stas%cnpy%sncan)) deallocate (stas%cnpy%sncan)
    if (allocated (stas%cnpy%tac)) deallocate (stas%cnpy%tac)
    if (allocated (stas%cnpy%tcan)) deallocate (stas%cnpy%tcan)
    if (allocated (stas%cnpy%cmai)) deallocate (stas%cnpy%cmai)
    if (allocated (stas%cnpy%gro)) deallocate (stas%cnpy%gro)
    if (allocated (stas%cnpy%pevp)) deallocate (stas%cnpy%pevp)
    if (allocated (stas%cnpy%evpb)) deallocate (stas%cnpy%evpb)
    if (allocated (stas%cnpy%arrd)) deallocate (stas%cnpy%arrd)

    !> Initialize states.

    !> Canopy.
    stas%cnpy%n = NML
    allocate(stas%cnpy%qac(NML), stas%cnpy%rcan(NML), stas%cnpy%sncan(NML), stas%cnpy%tac(NML), stas%cnpy%tcan(NML), &
             stas%cnpy%cmai(NML), stas%cnpy%gro(NML), stas%cnpy%pevp(NML), stas%cnpy%evpb(NML), stas%cnpy%arrd(NML))
    stas%cnpy%qac = 0.0; stas%cnpy%rcan = 0.0; stas%cnpy%sncan = 0.0; stas%cnpy%tac = 0.0; stas%cnpy%tcan = 0.0
    stas%cnpy%cmai = 0.0; stas%cnpy%gro = 0.0; stas%cnpy%pevp = 0.0; stas%cnpy%evpb = 0.0; stas%cnpy%arrd = 0.0

    if (allocated (stas%sno%sno)) deallocate (stas%sno%sno)
    if (allocated (stas%sno%albs)) deallocate (stas%sno%albs)
    if (allocated (stas%sno%fsno)) deallocate (stas%sno%fsno)
    if (allocated (stas%sno%rhos)) deallocate (stas%sno%rhos)
    if (allocated (stas%sno%tsno)) deallocate (stas%sno%tsno)
    if (allocated (stas%sno%wsno)) deallocate (stas%sno%wsno)

    !> Snow.
    stas%sno%n = NML
    allocate(stas%sno%sno(NML), stas%sno%albs(NML), stas%sno%fsno(NML), stas%sno%rhos(NML), stas%sno%tsno(NML), stas%sno%wsno(NML))
    stas%sno%sno = 0.0; stas%sno%albs = 0.0; stas%sno%fsno = 0.0; stas%sno%rhos = 0.0; stas%sno%tsno = 0.0; stas%sno%wsno = 0.0

    if (allocated (stas%sfc%tpnd)) deallocate (stas%sfc%tpnd)
    if (allocated (stas%sfc%zpnd)) deallocate (stas%sfc%zpnd)
    if (allocated (stas%sfc%pndw)) deallocate (stas%sfc%pndw)
    if (allocated (stas%sfc%evap)) deallocate (stas%sfc%evap)
    if (allocated (stas%sfc%qevp)) deallocate (stas%sfc%qevp)
    if (allocated (stas%sfc%hfs)) deallocate (stas%sfc%hfs)
    if (allocated (stas%sfc%rofo)) deallocate (stas%sfc%rofo)
    if (allocated (stas%sfc%tsfs)) deallocate (stas%sfc%tsfs)

    !> Surface or at near surface.
    stas%sfc%n = NML
    allocate(stas%sfc%tpnd(NML), stas%sfc%zpnd(NML), stas%sfc%pndw(NML), stas%sfc%evap(NML), stas%sfc%qevp(NML), &
             stas%sfc%hfs(NML), stas%sfc%rofo(NML), stas%sfc%tsfs(NML, 4))
    stas%sfc%tpnd = 0.0; stas%sfc%zpnd = 0.0; stas%sfc%pndw = 0.0; stas%sfc%evap = 0.0; stas%sfc%qevp = 0.0
    stas%sfc%hfs = 0.0; stas%sfc%rofo = 0.0; stas%sfc%tsfs = 0.0

    if (allocated (stas%sl%thic)) deallocate (stas%sl%thic)
    if (allocated (stas%sl%fzws)) deallocate (stas%sl%fzws)
    if (allocated (stas%sl%thlq)) deallocate (stas%sl%thlq)
    if (allocated (stas%sl%lqws)) deallocate (stas%sl%lqws)
    if (allocated (stas%sl%tbar)) deallocate (stas%sl%tbar)
    if (allocated (stas%sl%tbas)) deallocate (stas%sl%tbas)
    if (allocated (stas%sl%delzw)) deallocate (stas%sl%delzw)
    if (allocated (stas%sl%zbotw)) deallocate (stas%sl%zbotw)
    if (allocated (stas%sl%rofs)) deallocate (stas%sl%rofs)
    if (allocated (stas%sl%gflx)) deallocate (stas%sl%gflx)
    if (allocated (stas%sl%ggeo)) deallocate (stas%sl%ggeo)

    !> Soil layers.
    stas%sl%n = NML
    allocate(stas%sl%thic(NML, NSL), stas%sl%fzws(NML, NSL), stas%sl%thlq(NML, NSL), stas%sl%lqws(NML, NSL), &
             stas%sl%tbar(NML, NSL), stas%sl%tbas(NML), stas%sl%delzw(NML, NSL), stas%sl%zbotw(NML, NSL), stas%sl%rofs(NML), &
             stas%sl%gflx(NML, NSL), stas%sl%ggeo(NML))
    stas%sl%thic = 0.0; stas%sl%fzws = 0.0; stas%sl%thlq = 0.0; stas%sl%lqws = 0.0
    stas%sl%tbar = 0.0; stas%sl%tbas = 0.0; stas%sl%delzw = 0.0; stas%sl%zbotw = 0.0; stas%sl%rofs = 0.0
    stas%sl%gflx = 0.0; stas%sl%ggeo = 0.0

    if (allocated (stas%lzs%zlw)) deallocate (stas%lzs%zlw)
    if (allocated (stas%lzs%rofb)) deallocate (stas%lzs%rofb)

    !> Lower zone storage.
    stas%lzs%n = NML
    allocate(stas%lzs%zlw(NML), stas%lzs%rofb(NML))
    stas%lzs%zlw = 0.0; stas%lzs%rofb = 0.0

    if (allocated (stas%dzs%zlw)) deallocate (stas%dzs%zlw)
    if (allocated (stas%dzs%rofb)) deallocate (stas%dzs%rofb)

    !> Deep zone storage.
    stas%dzs%n = NML
    allocate(stas%dzs%zlw(NML), stas%dzs%rofb(NML))
    stas%dzs%zlw = 0.0; stas%dzs%rofb = 0.0

    if (allocated (stasrow%cnpy%qac)) deallocate (stasrow%cnpy%qac)
    if (allocated (stasrow%cnpy%tac)) deallocate (stasrow%cnpy%tac)
    if (allocated (stasrow%cnpy%tcan)) deallocate (stasrow%cnpy%tcan)
    if (allocated (stasrow%cnpy%rcan)) deallocate (stasrow%cnpy%rcan)
    if (allocated (stasrow%cnpy%sncan)) deallocate (stasrow%cnpy%sncan)
    if (allocated (stasrow%cnpy%cmai)) deallocate (stasrow%cnpy%cmai)
    if (allocated (stasrow%cnpy%gro)) deallocate (stasrow%cnpy%gro)
    if (allocated (stasrow%cnpy%pevp)) deallocate (stasrow%cnpy%pevp)
    if (allocated (stasrow%cnpy%evpb)) deallocate (stasrow%cnpy%evpb)
    if (allocated (stasrow%cnpy%arrd)) deallocate (stasrow%cnpy%arrd)
    if (allocated (stasrow%sno%sno)) deallocate (stasrow%sno%sno)
    if (allocated (stasrow%sno%albs)) deallocate (stasrow%sno%albs)
    if (allocated (stasrow%sno%fsno)) deallocate (stasrow%sno%fsno)
    if (allocated (stasrow%sno%rhos)) deallocate (stasrow%sno%rhos)
    if (allocated (stasrow%sno%tsno)) deallocate (stasrow%sno%tsno)
    if (allocated (stasrow%sno%wsno)) deallocate (stasrow%sno%wsno)
    if (allocated (stasrow%sfc%tpnd)) deallocate (stasrow%sfc%tpnd)
    if (allocated (stasrow%sfc%zpnd)) deallocate (stasrow%sfc%zpnd)
    if (allocated (stasrow%sfc%tsfs)) deallocate (stasrow%sfc%tsfs)
    if (allocated (stasrow%sl%thic)) deallocate (stasrow%sl%thic)
    if (allocated (stasrow%sl%fzws)) deallocate (stasrow%sl%fzws)
    if (allocated (stasrow%sl%thlq)) deallocate (stasrow%sl%thlq)
    if (allocated (stasrow%sl%lqws)) deallocate (stasrow%sl%lqws)
    if (allocated (stasrow%sl%tbar)) deallocate (stasrow%sl%tbar)
    if (allocated (stasrow%sl%tbas)) deallocate (stasrow%sl%tbas)
    if (allocated (stasrow%sl%delzw)) deallocate (stasrow%sl%delzw)
    if (allocated (stasrow%sl%zbotw)) deallocate (stasrow%sl%zbotw)
    if (allocated (stasrow%sl%rofs)) deallocate (stasrow%sl%rofs)
    if (allocated (stasrow%sl%gflx)) deallocate (stasrow%sl%gflx)
    if (allocated (stasrow%sl%ggeo)) deallocate (stasrow%sl%ggeo)
    if (allocated (stasrow%lzs%zlw)) deallocate (stasrow%lzs%zlw)
    if (allocated (stasrow%dzs%zlw)) deallocate (stasrow%dzs%zlw)

    !> Initiate state variables for output ('ROW' indexing).
    allocate(stasrow%cnpy%qac(NTYPE), stasrow%cnpy%tac(NTYPE), stasrow%cnpy%tcan(NTYPE), &
             stasrow%cnpy%rcan(NTYPE), stasrow%cnpy%sncan(NTYPE), &
             stasrow%cnpy%cmai(NTYPE), stasrow%cnpy%gro(NTYPE), &
             stasrow%cnpy%pevp(NTYPE), stasrow%cnpy%evpb(NTYPE), stasrow%cnpy%arrd(NTYPE), &
             stasrow%sno%sno(NTYPE), stasrow%sno%albs(NTYPE), stasrow%sno%fsno(NTYPE), stasrow%sno%rhos(NTYPE), &
             stasrow%sno%tsno(NTYPE), stasrow%sno%wsno(NTYPE), &
             stasrow%sfc%tpnd(NTYPE), stasrow%sfc%zpnd(NTYPE), stasrow%sfc%tsfs(NTYPE, 4), &
             stasrow%sl%thic(NTYPE, NSL), stasrow%sl%fzws(NTYPE, NSL), stasrow%sl%thlq(NTYPE, NSL), stasrow%sl%lqws(NTYPE, NSL), &
             stasrow%sl%tbar(NTYPE, NSL), stasrow%sl%tbas(NTYPE), &
             stasrow%sl%delzw(NTYPE, NSL), stasrow%sl%zbotw(NTYPE, NSL), stasrow%sl%rofs(NTYPE), &
             stasrow%sl%gflx(NTYPE, NSL), stasrow%sl%ggeo(NTYPE), &
             stasrow%lzs%zlw(NTYPE), &
             stasrow%dzs%zlw(NTYPE))
    stasrow%cnpy%qac = 0.0; stasrow%cnpy%tac = 0.0; stasrow%cnpy%tcan = 0.0
    stasrow%cnpy%rcan = 0.0; stasrow%cnpy%sncan = 0.0
    stasrow%cnpy%cmai = 0.0; stasrow%cnpy%gro = 0.0
    stasrow%cnpy%pevp = 0.0; stasrow%cnpy%evpb = 0.0; stasrow%cnpy%arrd = 0.0
    stasrow%sno%sno = 0.0; stasrow%sno%albs = 0.0; stasrow%sno%fsno = 0.0; stasrow%sno%rhos = 0.0
    stasrow%sno%tsno = 0.0; stasrow%sno%wsno = 0.0
    stasrow%sfc%tpnd = 0.0; stasrow%sfc%zpnd = 0.0; stasrow%sfc%tsfs = 0.0
    stasrow%sl%thic = 0.0; stasrow%sl%fzws = 0.0; stasrow%sl%thlq = 0.0; stasrow%sl%lqws = 0.0
    stasrow%sl%tbar = 0.0; stasrow%sl%tbas = 0.0; stasrow%sl%delzw = 0.0; stasrow%sl%zbotw = 0.0; stasrow%sl%rofs = 0.0
    stasrow%sl%gflx = 0.0; stasrow%sl%ggeo = 0.0
    stasrow%lzs%zlw = 0.0
    stasrow%dzs%zlw = 0.0

    !> Call 'CLASSD' to initialize constants.
!todo: replace this with a non-CLASS/generic version.
    call CLASSD

    !> Read parameters from file.
    call READ_PARAMETERS_CLASS(shd, fls, cm)

    !> Distribute the values.
    do k = il1, il2

        !> Grab the indices of the grid cell and GRU.
        i = shd%lc%ILMOS(k)
        m = shd%lc%JLMOS(k)

        !> Distribute the parameter values.
        pm%sfp%zrfm(k) = pmrow%sfp%zrfm(1)
        pm%sfp%zrfh(k) = pmrow%sfp%zrfh(1)
        pm%sfp%zbld(k) = pmrow%sfp%zbld(1)
        pm%tp%gc(k) = pmrow%tp%gc(1)
        pm%tp%fare(k) = pmrow%tp%fare(m)
        pm%tp%mid(k) = max(1, pmrow%tp%mid(m))
        pm%cp%fcan(k, :) = pmrow%cp%fcan(m, :)
        pm%cp%lnz0(k, :) = pmrow%cp%lnz0(m, :)
        pm%cp%alvc(k, :) = pmrow%cp%alvc(m, :)
        pm%cp%alic(k, :) = pmrow%cp%alic(m, :)
        pm%cp%lamx(k, :) = pmrow%cp%lamx(m, :)
        pm%cp%lamn(k, :) = pmrow%cp%lamn(m, :)
        pm%cp%cmas(k, :) = pmrow%cp%cmas(m, :)
        pm%cp%root(k, :) = pmrow%cp%root(m, :)
        pm%cp%rsmn(k, :) = pmrow%cp%rsmn(m, :)
        pm%cp%qa50(k, :) = pmrow%cp%qa50(m, :)
        pm%cp%vpda(k, :) = pmrow%cp%vpda(m, :)
        pm%cp%vpdb(k, :) = pmrow%cp%vpdb(m, :)
        pm%cp%psga(k, :) = pmrow%cp%psga(m, :)
        pm%cp%psgb(k, :) = pmrow%cp%psgb(m, :)
        pm%slp%sdep(k) = pmrow%slp%sdep(m)
        pm%hp%drn(k) = pmrow%hp%drn(m)
        if (allocated(shd%SLOPE_INT)) then
            pm%tp%xslp(k) = shd%SLOPE_INT(i) !taken from the drainage database.
        else
            pm%tp%xslp(k) = pmrow%tp%xslp(m) !taken by GRU from CLASS.ini
        end if
        if (allocated(shd%DRDN)) then
            pm%hp%dd(k) = shd%DRDN(i) !taken from the drainage database.
        else
            pm%hp%dd(k) = pmrow%hp%dd(m)/1000.0 !taken from CLASS.ini and from km/km^2 to m/m^2 for WATROF.
        end if
        pm%hp%mann(k) = pmrow%hp%mann(m)
        pm%hp%grkf(k) = pmrow%hp%grkf(m)
        pm%hp%ks(k) = pmrow%hp%ks(m)
        pm%slp%sand(k, :) = pmrow%slp%sand(m, :)
        pm%slp%clay(k, :) = pmrow%slp%clay(m, :)
        pm%slp%orgm(k, :) = pmrow%slp%orgm(m, :)

        !> Distribute the initial prognostic variable values.
        stas%cnpy%qac = 0.5e-2
        stas%cnpy%tcan(k) = stasrow%cnpy%tcan(m) + TFREZ
        stas%cnpy%tac(k) = stasrow%cnpy%tcan(m) + TFREZ
        stas%sno%tsno(k) = stasrow%sno%tsno(m) + TFREZ
        stas%sfc%tpnd(k) = stasrow%sfc%tpnd(m) + TFREZ
        stas%sfc%zpnd(k) = stasrow%sfc%zpnd(m)
        stas%cnpy%rcan(k) = stasrow%cnpy%rcan(m)
        stas%cnpy%sncan(k) = stasrow%cnpy%sncan(m)
        stas%sno%sno(k) = stasrow%sno%sno(m)
        stas%sno%albs(k) = stasrow%sno%albs(m)
        stas%sno%rhos(k) = stasrow%sno%rhos(m)
        stas%cnpy%gro(k) = stasrow%cnpy%gro(m)
        stas%sfc%tsfs(k, 1) = TFREZ
        stas%sfc%tsfs(k, 2) = TFREZ
        stas%sfc%tsfs(k, 3) = stasrow%sl%tbar(m, 1) + TFREZ
        stas%sfc%tsfs(k, 4) = stasrow%sl%tbar(m, 1) + TFREZ
        stas%sl%tbar(k, :) = stasrow%sl%tbar(m, :) + TFREZ
        stas%sl%thlq(k, :) = stasrow%sl%thlq(m, :)
        stas%sl%thic(k, :) = stasrow%sl%thic(m, :)
        stas%sl%tbas(k) = stasrow%sl%tbar(m, NSL) + TFREZ

    end do !k = il1, il2

    !> Check the grid output points.
!todo: fix this.
    if (ipid == 0) then
        do i = 1, WF_NUM_POINTS

            !> Check that output grid points aren't repeated and that the
            !> output directories exist.
            if (i < WF_NUM_POINTS) then
                do j = i + 1, WF_NUM_POINTS
                    if (op%N_OUT(i) == op%N_OUT(j) .and. op%II_OUT(i) == op%II_OUT(j)) then
                        print *
                        print *, 'Output for Grid ', op%N_OUT(i), ' and GRU ', &
                                 op%II_OUT(i), ' is repeated in grid output point: ', j
                        print *, 'Please adjust this grid output ', &
                                 'point in MESH_input_run_options.ini.'
                        stop
                    end if
                end do
            else
                open(17, file = './' // trim(adjustl(op%DIR_OUT(i))) // '/fort.17', status = 'unknown', iostat = ierr)
                if (ierr /= 0) then
                    print *
                    print *, 'Grid output point ', i
                    print *, 'The output directory does not exist: ' // trim(adjustl(op%DIR_OUT(i)))
                    print *, 'Please adjust this grid output point ', &
                             'in MESH_input_run_options.ini or create the ', &
                             'folder.'
                    stop
                else
                    close(17, status = 'delete')
                end if
            end if

            !> Check that grid output points are in the basin.
            if (op%N_OUT(i) > shd%NA) then
                write(6, *)
                write(6, *)
                write(6, *) 'Grids from basin watershed file: ', shd%NA
                write(6, *) 'Grid output point ', i, ' is in Grid: ', op%N_OUT(i)
                write(6, *) 'Please adjust this grid output point in ', 'MESH_input_run_options.ini'
                stop
            end if
        end do
    end if

    !> Distribute the starting date of the forcing files.
    do n = 1, cm%nclim
        cm%dat(n)%start_date%year = cm%start_date%year
        cm%dat(n)%start_date%jday = cm%start_date%jday
        cm%dat(n)%start_date%hour = cm%start_date%hour
        cm%dat(n)%start_date%mins = cm%start_date%mins
    end do

    !> Set the starting date from the forcing files if none is provided.
    if (ic%start%year == 0 .and. ic%start%jday == 0 .and. ic%start%hour == 0 .and. ic%start%mins == 0) then
        ic%start%year = cm%start_date%year
        ic%start%jday = cm%start_date%jday
        ic%start%hour = cm%start_date%hour
        ic%start%mins = cm%start_date%mins
    end if

    !> Initialize the current time-step.
    ic%now%year = ic%start%year
    ic%now%jday = ic%start%jday
    call julian2monthday(ic%now%jday, ic%now%year, ic%now%month, ic%now%day)
    ic%now%hour = ic%start%hour
    ic%now%mins = ic%start%mins

!> *********************************************************************
!> Open and read INITIAL SOIL MOISTURE AND SOIL TEMPERATURE values
!> when data is available
!> files: S_moisture.txt : soil moisture in layer 1, 2 and 3
!> files: T_temperature.txt : soil temperature in layer 1, 2 and 3
!> *********************************************************************
!>  FOR INITIAL SOIL MOISTURE AND SOIL TEMPERATURE
!>  Saul M. feb 26 2008

!todo - test this piece of code and make sure we understand how it works.
!todo - if we implement this, make it an option for the user to select GRU or grid initialization
    call READ_S_MOISTURE_TXT( &
            shd%yCount, shd%xCount, NA, NTYPE, NML, NSL, shd%yyy, shd%xxx, shd%lc%ILMOS, shd%lc%JLMOS, &
            stas%sl%thlq, &
            il1, il2)
    call READ_S_TEMPERATURE_TXT(&
            shd%yCount, shd%xCount, NA, NTYPE, NML, NSL, shd%yyy, shd%xxx, shd%lc%ILMOS, shd%lc%JLMOS, &
            stas%sl%tbar, &
            il1, il2)

    !> Call to read from soil.ini.
    call READ_SOIL_INI(shd, fls)

    if (allocated (hp%FRZCROW)) deallocate (hp%FRZCROW)
    if (allocated (hp%CMAXROW)) deallocate (hp%CMAXROW)
    if (allocated (hp%CMINROW)) deallocate (hp%CMINROW)
    if (allocated (hp%BROW)) deallocate (hp%BROW)
    if (allocated (hp%K1ROW)) deallocate (hp%K1ROW)
    if (allocated (hp%K2ROW)) deallocate (hp%K2ROW)
    if (allocated (hp%fetchROW)) deallocate (hp%fetchROW)
    if (allocated (hp%HtROW)) deallocate (hp%HtROW)
    if (allocated (hp%N_SROW)) deallocate (hp%N_SROW)
    if (allocated (hp%A_SROW)) deallocate (hp%A_SROW)
    if (allocated (hp%DistribROW)) deallocate (hp%DistribROW)

    !> Allocate additional parameters.
    allocate( &
        hp%FRZCROW(NA, NTYPE), &
        hp%CMAXROW(NA, NTYPE), hp%CMINROW(NA, NTYPE), hp%BROW(NA, NTYPE), hp%K1ROW(NA, NTYPE), hp%K2ROW(NA, NTYPE), &
        hp%fetchROW(NA, NTYPE), hp%HtROW(NA, NTYPE), hp%N_SROW(NA, NTYPE), hp%A_SROW(NA, NTYPE), hp%DistribROW(NA, NTYPE))

    NYEARS = ic%stop%year - ic%start%year + 1

    if (allocated (t0_ACC)) deallocate (t0_ACC)

    allocate(t0_ACC(NYEARS))
    t0_ACC = 0.0

    !> Allocate and initialize parameters for the cropland irrigation module.
    if (cifg%PROCESS_ACTIVE) then
        allocate( &
            ciprot%jdsow(NTYPE), ciprot%ldini(NTYPE), ciprot%lddev(NTYPE), ciprot%ldmid(NTYPE), ciprot%ldlate(NTYPE), &
            ciprot%Kcini(NTYPE), ciprot%Kcdev(NTYPE), ciprot%Kcmid(NTYPE), ciprot%Kclate(NTYPE))
        ciprot%jdsow = 0; ciprot%ldini = 0; ciprot%lddev = 0; ciprot%ldmid = 0; ciprot%ldlate = 0
        ciprot%Kcini = 0.0; ciprot%Kcdev = 0.0; ciprot%Kcmid = 0.0; ciprot%Kclate = 0.0
        allocate( &
            cip%jdsow(NML), cip%ldini(NML), cip%lddev(NML), cip%ldmid(NML), cip%ldlate(NML), &
            cip%Kcini(NML), cip%Kcdev(NML), cip%Kcmid(NML), cip%Kclate(NML))
        cip%jdsow = 0; cip%ldini = 0; cip%lddev = 0; cip%ldmid = 0; cip%ldlate = 0
        cip%Kcini = 0.0; cip%Kcdev = 0.0; cip%Kcmid = 0.0; cip%Kclate = 0.0
    end if

    !> Read parameters from file.
    call READ_PARAMETERS_HYDROLOGY(shd, fls)

    !> Distribute the values.
    do k = il1, il2

        !> Grab the indices of the grid cell and GRU.
        i = shd%lc%ILMOS(k)
        m = shd%lc%JLMOS(k)

        !> Distribute the parameter values.
        pm%snp%zsnl(k) = pmrow%snp%zsnl(m)
        pm%sfp%zplg(k) = pmrow%sfp%zplg(m)
        pm%snp%zpls(k) = pmrow%snp%zpls(m)

        !> Cropland irrigation module.
        if (cifg%PROCESS_ACTIVE) then
            cip%jdsow(k) = ciprot%jdsow(m)
            cip%ldini(k) = ciprot%ldini(m)
            cip%lddev(k) = ciprot%lddev(m)
            cip%ldmid(k) = ciprot%ldmid(m)
            cip%ldlate(k) = ciprot%ldlate(m)
            cip%Kcini(k) = ciprot%Kcini(m)
            cip%Kcdev(k) = ciprot%Kcdev(m)
            cip%Kcmid(k) = ciprot%Kcmid(m)
            cip%Kclate(k) = ciprot%Kclate(m)
        end if

    end do !k = il1, il2

end subroutine
