module RUNLAKE_module

    implicit none

    contains

    subroutine READ_PARAMETERS_LAKE()

        use RUNLAKE_variables

        implicit none

        !> Local variables.
        integer j, i, iun, ierr

        iun = 100
        open(iun, file = trim(adjustl('MESH_parameters_LAKE.ini')), status = 'old', action = 'read', iostat = ierr)

        !> Check for errors from opening the file.
        if (ierr /= 0) then
            print *
            print *, &
                'MESH_parameters_LAKE.ini could not be opened.', &
                'Ensure that the file exists and restart the program.'
            stop
        else
            print *, 'READING: MESH_parameters_LAKE.ini '
        end if

        read(iun, *) NTYPEL, DELT_L
        allocate(lakeTileParam(NTYPEL))
        read(iun, *) LAKEOUTDIR
        do i = 1, NTYPEL
            read(iun, *) lakeTileParam(i)%lakeTile                                           ! TITLE
            read(iun, *) lakeTileParam(i)%HLAK, lakeTileParam(i)%LLAK, lakeTileParam(i)%BLAK ! HLAK, LLAK, BLAK
            lakeTileParam(i)%NLAK = lakeTileParam(i)%HLAK/DELZLK
            allocate(lakeTileParam(i)%TLAK(lakeTileParam(i)%NLAK))
            read(iun, *) (lakeTileParam(i)%TLAK(j), j = 1, lakeTileParam(i)%NLAK)            ! TLAK
            where(lakeTileParam(i)%TLAK < 200.0) lakeTileParam(i)%TLAK = lakeTileParam(i)%TLAK + TFREZ
        end do
        close(100)

    end subroutine

    subroutine READ_LAKE_PROPORTION(shd)

        use RUNLAKE_variables
        use sa_mesh_shared_variables

        implicit none

        !> Input variables.
        type(ShedGridParams) :: shd

        !> Local variables.
        real, dimension(:, :), allocatable :: dummy
        integer :: k, j, i, iun, ierr

        iun = 100
        open(iun, file = adjustl(trim('MESH_LAKE_proportion.ini')), status = 'old', iostat = ierr)
        if (ierr == 0) then

            allocate(dummy(shd%yCount, shd%xCount))
            do k = 1, NTYPEL
                do i = 1, shd%yCount
                    read(iun, *) dummy(i, :)
                end do
                allocate(lakeTileParam(k)%FARE(shd%NA))
                do i = 1, shd%NA
                    lakeTileParam(k)%FARE(i) = dummy(shd%yyy(i), shd%xxx(i))
                end do
            end do

            !> Count the number of lake tiles in all cells.
            NMW = 0
            do k = 1, NTYPEL
                do i = 1, shd%NA
                    if (lakeTileParam(k)%FARE(i) > 0.0) NMW = NMW + 1
                end do
            end do

            print *, 'Reading proportion of lake tiles in MESH_LAKE_proportion.ini'
            print *, 'READ: SUCCESSFUL, FILE: CLOSED'
            close(iun)
        else
            print *, 'ERROR with MESH_LAKE_proportion.ini'
            stop
        end if
!        print*, NMW

    end subroutine

    subroutine gatherLakeTileParam(NA)

        use RUNLAKE_variables

        implicit none

        !> Input variables.
        integer, intent(in) :: NA

        !> Local variables.
        integer i1, k, i

        i1 = 1
        do i = 1, NA
            do k = 1, NTYPEL
                if (lakeTileParam(k)%FARE(i) > 0.0) then

                    !> Parameters and initial states.
                    ltp%HLAKGAT(i1) = lakeTileParam(k)%HLAK
                    ltp%LLAKGAT(i1) = lakeTileParam(k)%LLAK
                    ltp%BLAKGAT(i1) = lakeTileParam(k)%BLAK
                    ltp%NLAKGAT(i1) = lakeTileParam(k)%NLAK
                    ltp%TLAKGAT(i1, 1:lakeTileParam(k)%NLAK) = lakeTileParam(k)%TLAK(:)

                    !> NMW.
                    i1 = i1 + 1

                end if
            end do
        end do

    end subroutine

    subroutine gatherLakeTileVars(shd, cm)

        use RUNLAKE_variables
        use sa_mesh_shared_variables
        use climate_forcing

        implicit none

        !> Input variables.
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Local variables.
        real RDAY, DECL, HOUR, COSZ
        integer i1, k, i

        i1 = 1
        do i = 1, shd%NA
            do k = 1, NTYPEL
                if ( lakeTileParam(k)%FARE(i) > 0.0 ) then

                    !> Climate forcing and inputs.
                    cfiL%FSVH(i1) = cm%dat(ck%FB)%GRD(i)/2.0
                    cfiL%FSIH(i1) = cm%dat(ck%FB)%GRD(i)/2.0
                    cfiL%FDL(i1) = cm%dat(ck%FI)%GRD(i)
                    cfiL%PRE(i1) = cm%dat(ck%RT)%GRD(i)
                    cfiL%TA(i1) = cm%dat(ck%TT)%GRD(i)
                    cfiL%UL(i1) = cm%dat(ck%UV)%GRD(i)
                    cfiL%VL(i1) = 0.0
                    cfiL%PRES(i1) = cm%dat(ck%P0)%GRD(i)
                    cfiL%QA(i1) = cm%dat(ck%HU)%GRD(i)
                    catvL%RPRE(i1) = 0.
                    catvL%SPRE(i1) = 0.
                    catvL%RADJ(i1) = shd%ylat(i)*PI/180.0

                    !> Reference heights.
                    catvL%ZDM(i1) = 10.0
                    catvL%ZDH(i1) = 2.0

                    !> NMW.
                    i1 = i1 + 1

                end if
            end do
        end do
        catvL%ZRFM = pm%sfp%zrfm(1)
        catvL%ZRFH = pm%sfp%zrfh(1)

        !> CLASSL contains its own check of VMIN.
!        cfiL%VMOD = max(VMIN, cfiL%UL)

        !> This estimates the fractional cloud cover (FCLOGRD) by the basis
        !> of the solar zenith angle and the occurrence of precipitation.
        !> Assumed to be 1 (100%) when precipitation occurs and somewhere
        !> in the range of [0.1, 1] based on the location of the sun in the
        !> sky when precipitation is not occuring. (0.1 when the sun is at
        !> the zenith, 1 when the sun is at the horizon).
        RDAY = real(ic%now%jday) + (real(ic%now%hour) + real(ic%now%mins)/60.0)/24.0
        DECL = sin(2.0*PI*(284.0 + RDAY)/365.0)*23.45*PI/180.0
        HOUR = (real(ic%now%hour) + real(ic%now%mins)/60.0)*PI/12.0 - PI
        do k = 1, NMW
            COSZ = sin(catvL%RADJ(k))*sin(DECL) + cos(catvL%RADJ(k))*cos(DECL)*cos(HOUR)
            catvL%CSZ(k) = sign(max(abs(COSZ), 1.0e-3), COSZ)
        end do

    end subroutine

    subroutine checkEnergyBalance(n, energyBalSwitch, deltL)

        use RUNLAKE_variables
        use model_dates

        implicit none

        !> Input variables.
        !*  energyBalSwitch: 'before' for call before CLASSL; 'after' otherwise.
        !*  n: time counter (e.g., total iterations into simulation).
        !*  deltL: Increment of lake model time-step.
        character(len = 10), intent(in) :: energyBalSwitch
        integer, intent(in) :: n ! time counter
        integer, intent(in) :: deltL ! increment of lake model timestep

        !> Local variables.
        integer j, i
        real ICEBOT, ICETOP, RHOIW, HCAP, ZTOP, ZBOT, Z, QSUML

        !> Check energy balance.
        RHOIW = RHOICE/RHOW
        do i = 1, NMW
            ICEBOT = RHOIW*lpv%LKICEH(i)
            ICETOP = lpv%LKICEH(i) - ICEBOT
            if (ICEBOT >= DELSKIN) then
                HCAP = HCPICE
            else if (lpv%LKICEH(i) <= 0.0) then
                HCAP = HCPW
            else
                HCAP = (lpv%LKICEH(i)*HCPICE + (DELSKIN - ICEBOT)*HCPW)/DELSKIN
            end if
            if (trim(energyBalSwitch) == 'after') then
                ldv%CTLSTP(i) = ldv%CTLSTP(i) + HCAP*lpv%T0LAK(i)*DELSKIN
            else
                if (n == 1) then
                    ldv%CTLSTP(i) = -HCAP*ltp%TLAKGAT(i, 1)*DELSKIN
                else
                    ldv%CTLSTP(i) = -HCAP*lpv%T0LAK(i)*DELSKIN
                end if
            end if
            do j = 1, ltp%NLAKGAT(i)
                ZTOP = DELSKIN + DELZLK*(j - 1)
                ZBOT = DELSKIN + DELZLK*j
                if (ICEBOT >= ZBOT) then
                    HCAP = HCPICE
                else if (ICEBOT <= ZTOP) then
                    HCAP = HCPW
                else
                    Z = ICEBOT - ZTOP
                    HCAP = (Z*HCPICE + (DELZLK - Z)*HCPW)/DELZLK
                end if
                ldv%CTLSTP(i) = ldv%CTLSTP(i) - HCAP*ltp%TLAKGAT(i, j)*DELZLK
            end do
        end do

        !> Write output to file.
        if (trim(energyBalSwitch) == 'after') then
            ldv%CTLSTP(i) = ldv%CTLSTP(i)/DELT
            QSUML = ldv%FSGL(i) + ldv%FLGL(i) - ldv%HFSL(i) - ldv%HEVL(i) - ldv%HMFL(i)
            write(79, 6019) &
                ic%now%year, ic%now%jday, ic%now%hour, ic%now%mins+deltL, (QSUML - ldv%CTLSTP(i)), QSUML
        end if

6019    format(i4, 1x, 3(i3, 1x), 2f8.2)

    end subroutine

    subroutine RUNLAKE_within_tile(shd, cm)

        use RUNLAKE_variables
        use sa_mesh_shared_variables
        use climate_forcing

        implicit none

        !> Input variables.
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Local variables.
        character(len = 10) energyBalSwitch
        real DELT_CLASS
        integer i, nlakeIter

        !> Determine number of iterations.
        DELT_CLASS = DELT
        if (DELT_L >= 30.0) then
            nlakeIter = 1
            DELT = 30.0
        else
            nlakeIter = 30.0/DELT_L
            DELT = nint(30.0/nlakeIter)
        end if
!        print *, 'nlakeIter = ', nlakeIter

        lpvi%EXPW = 0.
        lpvi%DTEMP = 0.
        lpvi%HDPTH = 0.
        lpvi%DELU = 0.
        lpvi%GRED = 0.
        lpvi%TKELAK = 0.
        lpvi%T0LAK = 0.
        lpvi%LKICEH = 0.
        lpvi%RHOMIX = 0.
        lpvi%TSED = 0.
        lpvi%SNICEH = 0.
        lpvi%ROFICEH = 0.

        ldvi%CTLSTP = 0.
        ldvi%HFSL = 0.
        ldvi%HEVL = 0.
        ldvi%FSGL = 0.
        ldvi%FLGL = 0.
        ldvi%HMFL = 0.
        ldvi%HTCL = 0.
        ldvi%FICE = 0.
        ldvi%FLS = 0.
        ldvi%G0SL = 0.
        ldvi%FSGSL = 0.
        ldvi%FLGSL = 0.
        ldvi%HFSSL = 0.
        ldvi%HEVSL = 0.
        ldvi%HMFNL = 0.
        ldvi%HTCSL = 0.
        ldvi%PCPL = 0.
        ldvi%PCPNL = 0.
        ldvi%QFL = 0.
        ldvi%QFNL = 0.
        ldvi%ROFNL = 0.
        ldvi%SFTL = 0.
        ldvi%SFUL = 0.
        ldvi%SFVL = 0.
        ldvi%SFQL = 0.
        ldvi%SFHL = 0.
        ldvi%QLWOL = 0.
        ldvi%ALVL = 0.
        ldvi%ALIL = 0.
        ldvi%EFL = 0.
        ldvi%GTL = 0.
        ldvi%QGL = 0.
        ldvi%DRL = 0.
        ldvi%PETL = 0.
        ldvi%QSENL = 0.
        ldvi%TFXL = 0.
        ldvi%QEVPL = 0.
        ldvi%QFSL = 0.
        ldvi%QFXL = 0.
        ldvi%SNOL = 0.
        ldvi%RHOSL = 0.
        ldvi%TSNOL = 0.
        ldvi%ALBSL = 0.
        ldvi%WSNOL = 0.

        do i = 1, nlakeIter

            call gatherLakeTileVars(shd, cm)

            call CLASSI(catvL%VPD, catvL%TADP, catvL%PADR, catvL%RHOA, catvL%RHSI, &
                        catvL%RPCP, catvL%TRPC, catvL%SPCP, catvL%TSPC, cfiL%TA, cfiL%QA, &
                        cfiL%PRE, catvL%RPRE, catvL%SPRE, cfiL%PRES, &
                        IPCP, NMW, 1, NMW)

            ! Check energy balance before
            energyBalSwitch = 'before'
            call checkEnergyBalance(ic%ts_count, energyBalSwitch, (nint(DELT)*(i - 1)))
!            print *, ldv%CTLSTP(:)
!            print *, ic%now%year, ic%now%jday, ic%now%hour, ic%now%mins
!            print *, NMW, 1, NMW, shd%NA, NLAKMAX, ISLFD, IZREF, ITG
!            print *, DELT

            cdvL%CDH = 0.0
            cdvL%CDM = 0.0
            ldv%SNOL = 0.0
            ldv%RHOSL = 0.0
            ldv%TSNOL = 0.0
            ldv%ALBSL = 0.0
            ldv%WSNOL = 0.0
            ldv%QSENL = 0.0
            ldv%TFXL = 0.0
            ldv%QEVPL = 0.0
            ldv%QFSL = 0.0
            ldv%QFXL = 0.0
            ldv%PETL = 0.0
            ldv%EFL = 0.0
            ldv%GTL = 0.0
            ldv%QGL = 0.0
            ldv%DRL = 0.0
            ldv%SFTL = 0.0
            ldv%SFUL = 0.0
            ldv%SFVL = 0.0
            ldv%SFQL = 0.0
            ldv%SFHL = 0.0
            ldv%QLWOL = 0.0
            ldv%ALVL = 0.0
            ldv%ALIL = 0.0
            ldv%FSGL = 0.0
            ldv%FLGL = 0.0
            ldv%HFSL = 0.0
            ldv%HEVL = 0.0
            ldv%HMFL = 0.0
            ldv%HTCL = 0.0
            ldv%FSGSL = 0.0
            ldv%FLGSL = 0.0
            ldv%HFSSL = 0.0
            ldv%HEVSL = 0.0
            ldv%HMFNL = 0.0
            ldv%HTCSL = 0.0
            ldv%PCPL = 0.0
            ldv%PCPNL = 0.0
            ldv%QFL = 0.0
            ldv%QFNL = 0.0
            ldv%ROFNL = 0.0
            ldv%FICE = 0.0
            ldv%FLS = 0.0
            ldv%G0SL = 0.0
            ldv%FSDBL = 0.0
            ldv%FSFBL = 0.0
            ldv%FSSBL = 0.0

            !> Lake model.
            call CLASSL(ltp%HLAKGAT, ltp%LLAKGAT, ltp%BLAKGAT, ltp%NLAKGAT, ltp%TLAKGAT, &
                        lpv%T0LAK, lpv%HDPTH, lpv%LKICEH, lpv%SNICEH, lpv%ROFICEH, &
                        ldv%SNOL, ldv%RHOSL, ldv%TSNOL, ldv%ALBSL, ldv%WSNOL, &
                        cdvL%CDH, cdvL%CDM, ldv%QSENL, ldv%TFXL, ldv%QEVPL, ldv%QFSL, ldv%QFXL, &
                        ldv%PETL, ldv%EFL, ldv%GTL, ldv%QGL, ldv%DRL, &
                        ldv%SFTL, ldv%SFUL, ldv%SFVL, ldv%SFQL, ldv%SFHL, ldv%QLWOL, ldv%ALVL, ldv%ALIL, &
                        ldv%FSGL, ldv%FLGL, ldv%HFSL, ldv%HEVL, ldv%HMFL, ldv%HTCL, &
                        ldv%FSGSL, ldv%FLGSL, ldv%HFSSL, ldv%HEVSL, ldv%HMFNL, ldv%HTCSL, &
                        ldv%PCPL, ldv%PCPNL, ldv%QFL, ldv%QFNL, ldv%ROFNL, ldv%FICE, ldv%FLS, ldv%G0SL, &
                        lpv%EXPW, lpv%DTEMP, lpv%TKELAK, lpv%DELU, lpv%GRED, lpv%RHOMIX, &
                        cfiL%FSVH, cfiL%FSIH, cfiL%FDL, cfiL%UL, cfiL%VL, cfiL%TA, cfiL%QA, &
                        catvL%RHOA, catvL%PADR, cfiL%PRES, catvL%CSZ, catvL%ZRFM, catvL%ZRFH, &
                        catvL%ZDM, catvL%ZDH, catvL%RPCP, catvL%TRPC, catvL%SPCP, catvL%TSPC, catvL%RHSI, &
                        catvL%RADJ, ASVLGAT, ASILGAT, ldv%FSDBL, ldv%FSFBL, ldv%FSSBL, REFLGAT, BCSNLGAT, &
                        NMW, 1, NMW, NMW, NLAKMAX, ISLFD, IZREF, ITG, &
                        IALS, NBS, ISNOALB, IGL, IRSTRT, ic%ts_count, &
                        ic%now%year, ic%now%jday, ic%now%hour, (ic%now%mins + nint(DELT)*(i - 1)), lpv%TSED)

            !> Check energy balance after.
            energyBalSwitch = 'after'
            call checkEnergyBalance(ic%ts_count, energyBalSwitch, (nint(DELT)*(i - 1)))

            !> Accumulate lake prognostic variables.
            lpvi%EXPW = lpvi%EXPW + lpv%EXPW
            lpvi%DTEMP = lpvi%DTEMP + lpv%DTEMP
            lpvi%HDPTH = lpvi%HDPTH + lpv%HDPTH
            lpvi%DELU = lpvi%DELU + lpv%DELU
            lpvi%GRED = lpvi%GRED + lpv%GRED
            lpvi%TKELAK = lpvi%TKELAK + lpv%TKELAK
            lpvi%T0LAK = lpvi%T0LAK + lpv%T0LAK
            lpvi%LKICEH = lpvi%LKICEH + lpv%LKICEH
            lpvi%RHOMIX = lpvi%RHOMIX + lpv%RHOMIX
            lpvi%TSED = lpvi%TSED + lpv%TSED
            lpvi%SNICEH = lpvi%SNICEH + lpv%SNICEH
            lpvi%ROFICEH = lpvi%ROFICEH + lpv%ROFICEH

            !> Accumulate lake diagnostic variables.
            ldvi%CTLSTP = ldvi%CTLSTP + ldv%CTLSTP
            ldvi%HFSL = ldvi%HFSL + ldv%HFSL
            ldvi%HEVL = ldvi%HEVL + ldv%HEVL
            ldvi%FSGL = ldvi%FSGL + ldv%FSGL
            ldvi%FLGL = ldvi%FLGL + ldv%FLGL
            ldvi%HMFL = ldvi%HMFL + ldv%HMFL
            ldvi%HTCL = ldvi%HTCL + ldv%HTCL
            ldvi%FICE = ldvi%FICE + ldv%FICE
            ldvi%FLS = ldvi%FLS  + ldv%FLS
            ldvi%G0SL = ldvi%G0SL + ldv%G0SL
            ldvi%FSGSL = ldvi%FSGSL + ldv%FSGSL
            ldvi%FLGSL = ldvi%FLGSL + ldv%FLGSL
            ldvi%HFSSL = ldvi%HFSSL + ldv%HFSSL
            ldvi%HEVSL = ldvi%HEVSL + ldv%HEVSL
            ldvi%HMFNL = ldvi%HMFNL + ldv%HMFNL
            ldvi%HTCSL = ldvi%HTCSL + ldv%HTCSL
            ldvi%PCPL = ldvi%PCPL + ldv%PCPL
            ldvi%PCPNL = ldvi%PCPNL + ldv%PCPNL
            ldvi%QFL = ldvi%QFL + ldv%QFL
            ldvi%QFNL = ldvi%QFNL + ldv%QFNL
            ldvi%ROFNL = ldvi%ROFNL + ldv%ROFNL
            ldvi%SFTL = ldvi%SFTL + ldv%SFTL
            ldvi%SFUL = ldvi%SFUL + ldv%SFUL
            ldvi%SFVL = ldvi%SFVL + ldv%SFVL
            ldvi%SFQL = ldvi%SFQL + ldv%SFQL
            ldvi%SFHL = ldvi%SFHL + ldv%SFHL
            ldvi%QLWOL = ldvi%QLWOL + ldv%QLWOL
            ldvi%ALVL = ldvi%ALVL + ldv%ALVL
            ldvi%ALIL = ldvi%ALIL + ldv%ALIL
            ldvi%EFL = ldvi%EFL + ldv%EFL
            ldvi%GTL = ldvi%GTL + ldv%GTL
            ldvi%QGL = ldvi%QGL + ldv%QGL
            ldvi%DRL = ldvi%DRL + ldv%DRL
            ldvi%PETL = ldvi%PETL + ldv%PETL
            ldvi%QSENL = ldvi%QSENL + ldv%QSENL
            ldvi%TFXL = ldvi%TFXL + ldv%TFXL
            ldvi%QEVPL = ldvi%QEVPL + ldv%QEVPL
            ldvi%QFSL = ldvi%QFSL + ldv%QFSL
            ldvi%QFXL = ldvi%QFXL + ldv%QFXL
            ldvi%SNOL = ldvi%SNOL + ldv%SNOL
            ldvi%RHOSL = ldvi%RHOSL + ldv%RHOSL
            ldvi%TSNOL = ldvi%TSNOL + ldv%TSNOL
            ldvi%ALBSL = ldvi%ALBSL + ldv%ALBSL
            ldvi%WSNOL = ldvi%WSNOL + ldv%WSNOL
        end do

        !> Average of lake prognostic variables.
        lpvi%EXPW = lpvi%EXPW/nlakeIter
        lpvi%DTEMP = lpvi%DTEMP/nlakeIter
        lpvi%HDPTH = lpvi%HDPTH/nlakeIter
        lpvi%DELU = lpvi%DELU/nlakeIter
        lpvi%GRED = lpvi%GRED/nlakeIter
        lpvi%TKELAK = lpvi%TKELAK/nlakeIter
        lpvi%T0LAK = lpvi%T0LAK/nlakeIter
        lpvi%LKICEH = lpvi%LKICEH/nlakeIter
        lpvi%RHOMIX = lpvi%RHOMIX/nlakeIter
        lpvi%TSED = lpvi%TSED/nlakeIter
        lpvi%SNICEH = lpvi%SNICEH/nlakeIter
        lpvi%ROFICEH = lpvi%ROFICEH/nlakeIter

        !> Average of lake diagnostic variables.
        ldvi%CTLSTP = ldvi%CTLSTP/nlakeIter
        ldvi%HFSL = ldvi%HFSL/nlakeIter
        ldvi%HEVL = ldvi%HEVL/nlakeIter
        ldvi%FSGL = ldvi%FSGL/nlakeIter
        ldvi%FLGL = ldvi%FLGL/nlakeIter
        ldvi%HMFL = ldvi%HMFL/nlakeIter
        ldvi%HTCL = ldvi%HTCL/nlakeIter
        ldvi%FICE = ldvi%FICE/nlakeIter
        ldvi%FLS = ldvi%FLS/nlakeIter
        ldvi%G0SL = ldvi%G0SL/nlakeIter
        ldvi%FSGSL = ldvi%FSGSL/nlakeIter
        ldvi%FLGSL = ldvi%FLGSL/nlakeIter
        ldvi%HFSSL = ldvi%HFSSL/nlakeIter
        ldvi%HEVSL = ldvi%HEVSL/nlakeIter
        ldvi%HMFNL = ldvi%HMFNL/nlakeIter
        ldvi%HTCSL = ldvi%HTCSL/nlakeIter
        ldvi%PCPL = ldvi%PCPL/nlakeIter
        ldvi%PCPNL = ldvi%PCPNL/nlakeIter
        ldvi%QFL = ldvi%QFL/nlakeIter
        ldvi%QFNL = ldvi%QFNL/nlakeIter
        ldvi%ROFNL = ldvi%ROFNL/nlakeIter
        ldvi%SFTL = ldvi%SFTL/nlakeIter
        ldvi%SFUL = ldvi%SFUL/nlakeIter
        ldvi%SFVL = ldvi%SFVL/nlakeIter
        ldvi%SFQL = ldvi%SFQL/nlakeIter
        ldvi%SFHL = ldvi%SFHL/nlakeIter
        ldvi%QLWOL = ldvi%QLWOL/nlakeIter
        ldvi%ALVL = ldvi%ALVL/nlakeIter
        ldvi%ALIL = ldvi%ALIL/nlakeIter
        ldvi%EFL = ldvi%EFL/nlakeIter
        ldvi%GTL = ldvi%GTL/nlakeIter
        ldvi%QGL = ldvi%QGL/nlakeIter
        ldvi%DRL = ldvi%DRL/nlakeIter
        ldvi%PETL = ldvi%PETL/nlakeIter
        ldvi%QSENL = ldvi%QSENL/nlakeIter
        ldvi%TFXL = ldvi%TFXL/nlakeIter
        ldvi%QEVPL = ldvi%QEVPL/nlakeIter
        ldvi%QFSL = ldvi%QFSL/nlakeIter
        ldvi%QFXL = ldvi%QFXL/nlakeIter
        ldvi%SNOL = ldvi%SNOL/nlakeIter
        ldvi%RHOSL = ldvi%RHOSL/nlakeIter
        ldvi%TSNOL = ldvi%TSNOL/nlakeIter
        ldvi%ALBSL = ldvi%ALBSL/nlakeIter
        ldvi%WSNOL = ldvi%WSNOL/nlakeIter

        DELT = DELT_CLASS
!        print *, DELT

    end subroutine RUNLAKE_within_tile

    subroutine RUNLAKE_within_grid(shd, cm)

        use RUNLAKE_variables
        use sa_mesh_shared_variables
        use climate_forcing

        implicit none

        !> Input variables.
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

    end subroutine

end module RUNLAKE_module
