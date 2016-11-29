module RUNCLASS36_save_output

    use RUNCLASS36_constants
    use RUNCLASS36_variables

    implicit none

    !> GRID OUTPUT POINTS
    !* BNAM: TEMPORARY HOLD FOR OUTPUT DIRECTORY (12 CHARACTER STRING)
!    character(12) BNAM
    !* WF_NUM_POINTS: NUMBER OF GRID OUTPUTS
    !* I_OUT: OUTPUT GRID SQUARE TEMPORARY STORE
    integer WF_NUM_POINTS
!    integer I_OUT

    type OutputPoints

        !* N_OUT: GRID SQUARE TO OUTPUT
        !* II_OUT: GRU TO OUTPUT
        integer, dimension(:), allocatable :: N_OUT, II_OUT, K_OUT

        !* DIR_OUT: OUTPUT DIRECTORY (10 CHARACTER STRING)
        character*10, dimension(:), allocatable :: DIR_OUT
    end type

    type CLASSOUT_VARS
        real, dimension(:), allocatable :: &
            PREACC, GTACC, QEVPACC, EVAPACC, HFSACC, HMFNACC, &
            ROFACC, ROFOACC, ROFSACC, ROFBACC, WTBLACC, ALVSACC, ALIRACC, &
            RHOSACC, TSNOACC, WSNOACC, SNOARE, TCANACC, CANARE, SNOACC, &
            RCANACC, SCANACC, GROACC, FSINACC, FLINACC, FLUTACC, &
            TAACC, UVACC, PRESACC, QAACC
        real, dimension(:, :), allocatable :: &
            TBARACC, THLQACC, THICACC, THALACC, GFLXACC
    end type !CLASSOUT_VARS

    type(OutputPoints), save :: op
    type(CLASSOUT_VARS), save :: co

    contains

    subroutine CLASSOUT_open_files(shd)

        use mpi_shared_variables
        use sa_mesh_shared_variables

        type(ShedGridParams) :: shd

        !* BNAM: TEMPORARY HOLD FOR OUTPUT DIRECTORY (12 CHARACTER STRING)
        !* I_OUT: OUTPUT GRID SQUARE TEMPORARY STORE
        character(12) BNAM
        integer IGND, I_OUT
        character(20) IGND_CHAR
        character(2000) FMT
        integer k, j, i

        !> Return if the process is not marked active.
        if (.not. RUNCLASS36_flgs%PROCESS_ACTIVE) return

        !> check that run points are in the basin and that there are no repeats
        do i = 1, WF_NUM_POINTS
            if (op%N_OUT(i) > shd%NA) then
                print *, 'No. of grids from MESH_drainage_database.txt:', shd%NA
                print *, 'out point ', i, ' is: ', op%N_OUT(i)
                print *, 'please adjust MESH_run_options.ini file'
                stop
            end if
            if (i < WF_NUM_POINTS) then
                do j = i + 1, WF_NUM_POINTS
                    if (op%N_OUT(i) == op%N_OUT(j) .and. op%II_OUT(i) == op%II_OUT(j)) then
                        print *, 'grid number ', op%N_OUT(i)
                        print *, 'is repeated in MESH_run_options.ini file'
                        print *, 'please adjust MESH_run_options.ini file'
                        stop
                    end if
                end do
            end if
        end do

        !> Initialize and open files for CLASS output.
        if ((ipid /= 0 .or. izero == 0) .and. WF_NUM_POINTS > 0) then

            !> After GATPREP. Determine the GAT-index of the output point.
            op%K_OUT = 0
            do k = il1, il2
                do i = 1, WF_NUM_POINTS
                    if (op%N_OUT(i) == shd%lc%ILMOS(k) .and. op%II_OUT(i) == shd%lc%JLMOS(k)) op%K_OUT(i) = k
                end do
            end do

            !> Allocate the CLASS output variables.
            IGND = shd%lc%IGND
            allocate(co%PREACC(WF_NUM_POINTS), co%GTACC(WF_NUM_POINTS), co%QEVPACC(WF_NUM_POINTS), co%EVAPACC(WF_NUM_POINTS), &
                     co%HFSACC(WF_NUM_POINTS), co%HMFNACC(WF_NUM_POINTS), &
                     co%ROFACC(WF_NUM_POINTS), co%ROFOACC(WF_NUM_POINTS), co%ROFSACC(WF_NUM_POINTS), co%ROFBACC(WF_NUM_POINTS), &
                     co%WTBLACC(WF_NUM_POINTS), co%ALVSACC(WF_NUM_POINTS), co%ALIRACC(WF_NUM_POINTS), &
                     co%RHOSACC(WF_NUM_POINTS), co%TSNOACC(WF_NUM_POINTS), co%WSNOACC(WF_NUM_POINTS), co%SNOARE(WF_NUM_POINTS), &
                     co%TCANACC(WF_NUM_POINTS), co%CANARE(WF_NUM_POINTS), co%SNOACC(WF_NUM_POINTS), &
                     co%RCANACC(WF_NUM_POINTS), co%SCANACC(WF_NUM_POINTS), co%GROACC(WF_NUM_POINTS), co%FSINACC(WF_NUM_POINTS), &
                     co%FLINACC(WF_NUM_POINTS), co%FLUTACC(WF_NUM_POINTS), &
                     co%TAACC(WF_NUM_POINTS), co%UVACC(WF_NUM_POINTS), co%PRESACC(WF_NUM_POINTS), co%QAACC(WF_NUM_POINTS))
            allocate(co%TBARACC(WF_NUM_POINTS, IGND), co%THLQACC(WF_NUM_POINTS, IGND), &
                     co%THICACC(WF_NUM_POINTS, IGND), &
                     co%THALACC(WF_NUM_POINTS, IGND), co%GFLXACC(WF_NUM_POINTS, IGND))

            !> Initialize the CLASS output variables.
            co%PREACC = 0.0
            co%GTACC = 0.0
            co%QEVPACC = 0.0
            co%EVAPACC = 0.0
            co%HFSACC = 0.0
            co%HMFNACC = 0.0
            co%ROFACC = 0.0
            co%ROFOACC = 0.0
            co%ROFSACC = 0.0
            co%ROFBACC = 0.0
            co%WTBLACC = 0.0
            co%TBARACC = 0.0
            co%THLQACC = 0.0
            co%THICACC = 0.0
            co%THALACC = 0.0
            co%GFLXACC = 0.0
            co%ALVSACC = 0.0
            co%ALIRACC = 0.0
            co%RHOSACC = 0.0
            co%TSNOACC = 0.0
            co%WSNOACC = 0.0
            co%SNOARE = 0.0
            co%TCANACC = 0.0
            co%CANARE = 0.0
            co%SNOACC = 0.0
            co%RCANACC = 0.0
            co%SCANACC = 0.0
            co%GROACC = 0.0
            co%FSINACC = 0.0
            co%FLINACC = 0.0
            co%FLUTACC = 0.0
            co%TAACC = 0.0
            co%UVACC = 0.0
            co%PRESACC = 0.0
            co%QAACC = 0.0

            !> Open the files if the GAT-index of the output point resides on this node.
            do i = 1, WF_NUM_POINTS
                if ((ipid /= 0 .or. izero == 0) .and. (op%K_OUT(i) >= il1 .and. op%K_OUT(i) <= il2)) then

                    !> Open the files in the appropriate directory.
                    BNAM = op%DIR_OUT(i)
                    j = 1
                    open(150 + i*10 + j, file = './' // trim(adjustl(BNAM)) // '/CLASSOF1.csv'); j = j + 1
                    open(150 + i*10 + j, file = './' // trim(adjustl(BNAM)) // '/CLASSOF2.csv'); j = j + 1
                    open(150 + i*10 + j, file = './' // trim(adjustl(BNAM)) // '/CLASSOF3.csv'); j = j + 1
                    open(150 + i*10 + j, file = './' // trim(adjustl(BNAM)) // '/CLASSOF4.csv'); j = j + 1
                    open(150 + i*10 + j, file = './' // trim(adjustl(BNAM)) // '/CLASSOF5.csv'); j = j + 1
                    open(150 + i*10 + j, file = './' // trim(adjustl(BNAM)) // '/CLASSOF6.csv'); j = j + 1
                    open(150 + i*10 + j, file = './' // trim(adjustl(BNAM)) // '/CLASSOF7.csv'); j = j + 1
                    open(150 + i*10 + j, file = './' // trim(adjustl(BNAM)) // '/CLASSOF8.csv'); j = j + 1
                    open(150 + i*10 + j, file = './' // trim(adjustl(BNAM)) // '/CLASSOF9.csv'); j = j + 1
                    open(150 + i*10 + j, file = './' // trim(adjustl(BNAM)) // '/GRU_water_balance.csv')

                    !> Write project header information.
                    do j = 1, 9
                        write(150 + i*10 + j, "('CLASS TEST RUN:     ', 6a4)") TITLE1, TITLE2, TITLE3, TITLE4, TITLE5, TITLE6
                        write(150 + i*10 + j, "('RESEARCHER:         ', 6a4)") NAME1, NAME2, NAME3, NAME4, NAME5, NAME6
                        write(150 + i*10 + j, "('INSTITUTION:        ', 6a4)") PLACE1, PLACE2, PLACE3, PLACE4, PLACE5, PLACE6
                    end do

                    !> CLASSOF1.
                    write(150 + i*10 + 1, "('IDAY,IYEAR,FSSTAR,FLSTAR,QH,QE,SNOMLT,BEG," // &
                        'GTOUT,SNOACC(I),RHOSACC(I),WSNOACC(I),ALTOT,ROFACC(I),' // &
                        "ROFOACC(I),ROFSACC(I),ROFBACC(I)')")

                    !> CLASSOF2.
                    write(FMT, *) ''
                    do j = 1, IGND
                        write(IGND_CHAR, *) j
                        IGND_CHAR = adjustl(IGND_CHAR)
                        FMT = trim(adjustl(FMT)) // 'TBARACC(I ' // trim(IGND_CHAR) // ')-TFREZ,THLQACC(I ' // &
                            trim(IGND_CHAR) // '),THICACC(I ' // trim(IGND_CHAR) // '),'
                    end do
                    write(150 + i*10 + 2, "('IDAY,IYEAR," // trim(FMT) // "TCN,RCANACC(I),SCANACC(I),TSN,ZSN')")

                    !> CLASSOF3.
                    write(150 + i*10 + 3, "('IDAY,IYEAR,FSINACC(I),FLINACC(I)," // &
                        'TAACC(I)-TFREZ,UVACC(I),PRESACC(I),QAACC(I),PREACC(I),' // &
                        "EVAPACC(I)')")

                    !> CLASSOF4.
                    write(150 + i*10 + 4, "('IHOUR,IMIN,IDAY,IYEAR,FSSTAR,FLSTAR,QH,QE," // &
                        'SNOMLT,BEG,GTOUT,SNOROW(I M),RHOSROW(I M),WSNOROW(I M),ALTOT,' // &
                        "ROFROW(I M),TPN,ZPNDROW(I M),ZPND,FSTR')")

                    !> CLASSOF5.
                    write(FMT, *) ''
                    do j = 1, IGND
                        write(IGND_CHAR, *) j
                        IGND_CHAR = adjustl(IGND_CHAR)
                        FMT = trim(adjustl(FMT)) // 'TBARROW(I ' // trim(IGND_CHAR) // ')-TFREZ,THLQROW(I ' // &
                            trim(IGND_CHAR) // '),THICROW(I ' // trim(IGND_CHAR) // '),'
                    end do
                    write(150 + i*10 + 5, "('IHOUR,IMIN,IDAY,IYEAR," // trim(FMT) // "TCN,RCANROW(I M),SCANROW(I M),TSN,ZSN')")

                    !> CLASSOF6.
                    write(150 + i*10 + 6, "('IHOUR,IMIN,IDAY,FSDOWN(I),FDLGRD(I)," // &
                        "PREGRD(I),TAGRD(I)-TFREZ,UVGRD(I),PRESGRD(I),QAGRD(I)')")

                    !> CLASSOF7.
                    write(150 + i*10 + 7,"('TROFROW(I M),TROOROW(I M),TROSROW(I M)," // &
                        'TROBROW(I M),ROFROW(I M),ROFOROW(I M),ROFSROW(I M),' // &
                        "ROFBROW(I M),FCS(I),FGS(I),FC(I),FG(I)')")

                    !> CLASSOF8.
                    write(FMT, *) ''
                    do j = 1, IGND
                        write(IGND_CHAR, *) j
                        IGND_CHAR = adjustl(IGND_CHAR)
                        FMT = trim(adjustl(FMT)) // ',HMFGROW(I M ' // trim(IGND_CHAR) // ')'
                    end do
                    FMT = trim(adjustl(FMT)) // ',HTCCROW(I M),HTCSROW(I M)'
                    do j = 1, IGND
                        write(IGND_CHAR, *) j
                        IGND_CHAR = adjustl(IGND_CHAR)
                        FMT = trim(adjustl(FMT)) // ',HTCROW(I M ' // trim(IGND_CHAR) // ')'
                    end do
                    write(150 + i*10 + 8, "('FSGVROW(I M),FSGSROW(I M),FSGGROW(I M)," // &
                        'FLGVROW(I M),FLGSROW(I M),FLGGROW(I M),HFSCROW(I M),' // &
                        'HFSSROW(I M),HFSGROW(I M),HEVCROW(I M),HEVSROW(I M),' // &
                        'HEVGROW(I M),HMFCROW(I M),HMFNROW(I M)' // trim(FMT) // "')")

                    !> CLASSOF9.
                    write(FMT, *) ''
                    do j = 1, IGND
                        write(IGND_CHAR, *) j
                        IGND_CHAR = adjustl(IGND_CHAR)
                        FMT = trim(adjustl(FMT)) // 'QFCROW(I M ' // trim(IGND_CHAR) // '),'
                    end do
                    write(150 + i*10 + 9, "('PCFCROW(I M),PCLCROW(I M),PCPNROW(I M)," // &
                        'PCPGROW(I M),QFCFROW(I M),QFCLROW(I M),QFNROW(I M),QFGROW(I M),' // trim(FMT) // 'ROFCROW(I M),' // &
                        'ROFNROW(I M),ROFOROW(I M),ROFROW(I M),WTRCROW(I M),' // &
                        "WTRSROW(I M),WTRGROW(I M)')")

                    !> GRU water balance file.
                    write(FMT, *) ''
                    do j = 1, IGND
                        write(IGND_CHAR, *) j
                        IGND_CHAR = adjustl(IGND_CHAR)
                        FMT = trim(adjustl(FMT)) // 'THLQ' // trim(IGND_CHAR) // ','
                    end do
                    do j = 1, IGND
                        write(IGND_CHAR, *) j
                        IGND_CHAR = adjustl(IGND_CHAR)
                        FMT = trim(adjustl(FMT)) // 'THIC' // trim(IGND_CHAR) // ','
                    end do
                    write(150 + i*10 + 10, "('IHOUR,IMIN,IDAY,IYEAR," // &
                        'PRE,EVAP,ROF,ROFO,ROFS,ROFB,' // &
                        'SCAN,RCAN,SNO,WSNO,ZPND,' // trim(FMT) // "')")

                end if !(op%K_OUT(i) >= il1 .and. op%K_OUT(i) <= il2) then
            end do !i = 1, wf_num_points
        end if !(WF_NUM_POINTS > 0) then

    end subroutine

    subroutine CLASSOUT_update_files(shd)

        use mpi_shared_variables
        use sa_mesh_shared_variables
        use model_dates

        type(ShedGridParams), intent(in) :: shd

!* I_OUT: OUTPUT GRID SQUARE TEMPORARY STORE
        integer DELT, IGND, I_OUT
        real ALTOT, FSSTAR, FLSTAR, QH, QE, BEG, SNOMLT, ZSN, TCN, TSN, TPN, GTOUT
        real FARE, LCC
        real ZPND, FSTR
        character(20) IGND_CHAR
        integer NSUM, k, j, i

        !> Return if the process is not marked active.
        if (.not. RUNCLASS36_flgs%PROCESS_ACTIVE) return

        !> Constant variables.
        DELT = ic%dts
        IGND = shd%lc%IGND
        write(IGND_CHAR, *) IGND

        !> Write to CLASSOF* output files.
        do i = 1, WF_NUM_POINTS
            if ((ipid /= 0 .or. izero == 0) .and. (op%K_OUT(i) >= il1 .and. op%K_OUT(i) <= il2)) then

                !> Update variables.
                k = op%K_OUT(i)
                if (2.0*cfi%FSVH(k) > 0.0) then
                    ALTOT = (cdv%ALVS(k) + cdv%ALIR(k))/2.0
                else
                    ALTOT = 0.0
                end if
                FSSTAR = 2.0*cfi%FSVH(k)*(1.0 - ALTOT)
                FLSTAR = cfi%FDL(k) - SBC*cdv%GTE(k)**4
                QH = cdv%HFS(k)
                QE = cdv%QEVP(k)
                BEG = FSSTAR + FLSTAR - QH - QE
                SNOMLT = cdv%HMFN(k)
                if (cpv%RHOS(k) > 0.0) then
                    ZSN = cpv%SNO(k)/cpv%RHOS(k)
                else
                    ZSN = 0.0
                end if
                if (cpv%TCAN(k) > 0.01) then
                    TCN = cpv%TCAN(k) - TFREZ
                else
                    TCN = 0.0
                end if
                if (cpv%TSNO(k) > 0.01) then
                    TSN = cpv%TSNO(k) - TFREZ
                else
                    TSN = 0.0
                end if
                if (cpv%TPND(k) > 0.01) then
                    TPN = cpv%TPND(k) - TFREZ
                else
                    TPN = 0.0
                end if
                if (shd%wc%ILG == 1) then
                    GTOUT = cdv%GTE(k) - TFREZ
                else
                    GTOUT = 0.0
                end if
                ZPND = ZPNDPRECS(k)*cdv%FCS(k) + ZPONDPREC(k)*cdv%FC(k) + ZPONDPREG(k)*cdv%FG(k) + ZPNDPREGS(k)*cdv%FGS(k)
                FSTR = FSTRCS(k)*cdv%FCS(k) + FSTRC(k)*cdv%FC(k) + FSTRG(k)*cdv%FG(k) + FSTRGS(k)*cdv%FGS(k)

                !> Write to the CLASSOF* output files for sub-hourly output.
                write(150 + i*10 + 4, &
                      "(i2,',', i3,',', i5,',', i6,',', 9(f8.2,','), 2(f7.3,','), e11.3,',', f8.2,',', 3(f12.4,','))") &
                    ic%now%hour, ic%now%mins, ic%now%jday, ic%now%year, FSSTAR, FLSTAR, QH, &
                    QE, SNOMLT, BEG, GTOUT, cpv%SNO(k), &
                    cpv%RHOS(k), cpv%WSNO(k), ALTOT, cdv%ROF(k), &
                    TPN, cpv%ZPND(k), ZPND, FSTR
                write(150 + i*10 + 5, "(i2,',', i3,',', i5,',', i6,',', " // trim(adjustl(IGND_CHAR)) // &
                      "(f7.2,',', 2(f6.3,',')), f8.2,',', 2(f8.4,','), f8.2,',', f8.3,',')") &
                    ic%now%hour, ic%now%mins, ic%now%jday, ic%now%year, &
                    (cpv%TBAR(k, j) - TFREZ, cpv%THLQ(k, j), &
                    cpv%THIC(k, j), j = 1, IGND), TCN, &
                    cpv%RCAN(k), cpv%SNCAN(k), TSN, ZSN
                write(150 + i*10 + 6, &
                      "(i2,',', i3,',', i5,',', 2(f10.2,','), f12.6,',', f10.2,',', f8.2,',', f10.2,',', f15.9,',')") &
                    ic%now%hour, ic%now%mins, ic%now%jday, 2.0*cfi%FSVH(k), cfi%FDL(k), &
                    cfi%PRE(k), cfi%TA(k) - TFREZ, cfi%VMOD(k), cfi%PRES(k), &
                    cfi%QA(k)
                write(150 + i*10 + 7, "(999(e11.4,','))") &
                    cdv%TROF(k), cdv%TROO(k), cdv%TROS(k), &
                    cdv%TROB(k), cdv%ROF(k), cdv%ROFO(k), &
                    cdv%ROFS(k), cdv%ROFB(k), &
                    cdv%FCS(k), cdv%FGS(k), cdv%FC(k), cdv%FG(k)
                write(150 + i*10 + 8, "(999(f12.4,','))") &
                    cdv%FSGV(k), cdv%FSGS(k), cdv%FSGG(k), &
                    cdv%FLGV(k), cdv%FLGS(k), cdv%FLGG(k), &
                    cdv%HFSC(k), cdv%HFSS(k), cdv%HFSG(k), &
                    cdv%HEVC(k), cdv%HEVS(k), cdv%HEVG(k), &
                    cdv%HMFC(k), cdv%HMFN(k), &
                    (cdv%HMFG(k, j), j = 1, IGND), &
                    cdv%HTCC(k), cdv%HTCS(k), &
                    (cdv%HTC(k, j), j = 1, IGND)
                write(150 + i*10 + 9, "(999(e12.4,','))") &
                    cdv%PCFC(k), cdv%PCLC(k), cdv%PCPN(k), &
                    cdv%PCPG(k), cdv%QFCF(k), cdv%QFCL(k), &
                    cdv%QFN(k), cdv%QFG(k), (cdv%QFC(k, j), j = 1, IGND), &
                    cdv%ROFC(k), cdv%ROFN(k), &
                    cdv%ROFO(k), cdv%ROF(k), cdv%WTRC(k), &
                    cdv%WTRS(k), cdv%WTRG(k)
                write(150 + i*10 + 10, "(i2,',', i3,',', i5,',', i6,',', 999(f14.6,','))") &
                    ic%now%hour, ic%now%mins, ic%now%jday, ic%now%year, cfi%PRE(k)*DELT, cdv%QFS(k)*DELT, &
                    cdv%ROF(k)*DELT, cdv%ROFO(k)*DELT, cdv%ROFS(k)*DELT, cdv%ROFB(k)*DELT, &
                    cpv%SNCAN(k), cpv%RCAN(k), cpv%SNO(k), cpv%WSNO(k), &
                    cpv%ZPND(k)*RHOW, (cpv%THLQ(k, j)*RHOW*csfv%DELZW(k, j), j = 1, IGND), &
                    (cpv%THIC(k, j)*RHOICE*csfv%DELZW(k, j), j = 1, IGND)

                !> Calculate accumulated grid variables.
                do k = il1, il2
                    if (shd%lc%ILMOS(k) == op%N_OUT(i)) then

                        FARE = shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))*shd%FRAC(shd%lc%ILMOS(k))
                        LCC = shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))

                        co%PREACC(i) = co%PREACC(i) + cfi%PRE(k)*FARE*DELT
                        co%GTACC(i) = co%GTACC(i) + cdv%GTE(k)*FARE
                        co%QEVPACC(i) = co%QEVPACC(i) + cdv%QEVP(k)*FARE
                        co%EVAPACC(i) = co%EVAPACC(i) + cdv%QFS(k)*FARE*DELT
                        co%HFSACC(i) = co%HFSACC(i) + cdv%HFS(k)*FARE
                        co%HMFNACC(i) = co%HMFNACC(i) + cdv%HMFN(k)*FARE
                        co%ROFACC(i) = co%ROFACC(i) + cdv%ROF(k)*FARE*DELT
                        co%ROFOACC(i) = co%ROFOACC(i) + cdv%ROFO(k)*FARE*DELT
                        co%ROFSACC(i) = co%ROFSACC(i) + cdv%ROFS(k)*FARE*DELT
                        co%ROFBACC(i) = co%ROFBACC(i) + cdv%ROFB(k)*FARE*DELT
                        co%WTBLACC(i) = co%WTBLACC(i) + cdv%WTAB(k)*FARE
                        do j = 1, IGND
                            co%TBARACC(i, j) = co%TBARACC(i, j) + cpv%TBAR(k, j)*LCC
                            co%THLQACC(i, j) = co%THLQACC(i, j) + cpv%THLQ(k, j)*FARE
                            co%THICACC(i, j) = co%THICACC(i, j) + cpv%THIC(k, j)*FARE
                            co%THALACC(i, j) = co%THALACC(i, j) + (cpv%THLQ(k, j) + cpv%THIC(k, j))*FARE
                            co%GFLXACC(i, j) = co%GFLXACC(i, j) + cdv%GFLX(k, j)*FARE
                        end do
                        co%ALVSACC(i) = co%ALVSACC(i) + cdv%ALVS(k)*cfi%FSVH(k)*FARE
                        co%ALIRACC(i) = co%ALIRACC(i) + cdv%ALIR(k)*cfi%FSIH(k)*FARE
                        if (cpv%SNO(k) > 0.0) then
                            co%RHOSACC(i) = co%RHOSACC(i) + cpv%RHOS(k)*FARE
                            co%TSNOACC(i) = co%TSNOACC(i) + cpv%TSNO(k)*FARE
                            co%WSNOACC(i) = co%WSNOACC(i) + cpv%WSNO(k)*FARE
                            co%SNOARE(i) = co%SNOARE(i) + FARE
                        end if
                        if (cpv%TCAN(k) > 0.5) then
                            co%TCANACC(i) = co%TCANACC(i) + cpv%TCAN(k)*FARE
                            co%CANARE(i) = co%CANARE(i) + FARE
                        end if
                        co%SNOACC(i) = co%SNOACC(i) + cpv%SNO(k)*FARE
                        co%RCANACC(i) = co%RCANACC(i) + cpv%RCAN(k)*FARE
                        co%SCANACC(i) = co%SCANACC(i) + cpv%SNCAN(k)*FARE
                        co%GROACC(i) = co%GROACC(i) + cpv%GRO(k)*FARE
                        co%FSINACC(i) = co%FSINACC(i) + 2.0*cfi%FSVH(k)*FARE
                        co%FLINACC(i) = co%FLINACC(i) + cfi%FDL(k)*FARE
                        co%FLUTACC(i) = co%FLUTACC(i) + SBC*cdv%GTE(k)**4*FARE
                        co%TAACC(i) = co%TAACC(i) + cfi%TA(k)*FARE
                        co%UVACC(i) = co%UVACC(i) + cfi%VMOD(k)*FARE
                        co%PRESACC(i) = co%PRESACC(i) + cfi%PRES(k)*FARE
                        co%QAACC(i) = co%QAACC(i) + cfi%QA(k)*FARE
                    end if
                end do

                !> Write to the CLASSOF* output files for daily output.
                if (mod(ic%ts_daily, 86400/ic%dts) == 0) then

                    NSUM = ic%ts_daily

                    !> Calculate grid averages.
                    co%GTACC(i) = co%GTACC(i)/real(NSUM)
                    co%QEVPACC(i) = co%QEVPACC(i)/real(NSUM)
                    co%HFSACC(i) = co%HFSACC(i)/real(NSUM)
                    co%HMFNACC(i) = co%HMFNACC(i)/real(NSUM)
                    co%WTBLACC(i) = co%WTBLACC(i)/real(NSUM)
                    co%TBARACC(i, :) = co%TBARACC(i, :)/real(NSUM)
                    co%THLQACC(i, :) = co%THLQACC(i, :)/real(NSUM)
                    co%THICACC(i, :) = co%THICACC(i, :)/real(NSUM)
                    co%THALACC(i, :) = co%THALACC(i, :)/real(NSUM)
                    if (co%FSINACC(i) > 0.0) then
                        co%ALVSACC(i) = co%ALVSACC(i)/(co%FSINACC(i)*0.5)
                        co%ALIRACC(i) = co%ALIRACC(i)/(co%FSINACC(i)*0.5)
                    else
                        co%ALVSACC(i) = 0.0
                        co%ALIRACC(i) = 0.0
                    end if
                    if (co%SNOARE(i) > 0.0) then
                        co%RHOSACC(i) = co%RHOSACC(i)/co%SNOARE(i)
                        co%TSNOACC(i) = co%TSNOACC(i)/co%SNOARE(i)
                        co%WSNOACC(i) = co%WSNOACC(i)/co%SNOARE(i)
                    end if
                    if (co%CANARE(i) > 0.0) then
                        co%TCANACC(i) = co%TCANACC(i)/co%CANARE(i)
                    end if
                    co%SNOACC(i) = co%SNOACC(i)/real(NSUM)
                    co%RCANACC(i) = co%RCANACC(i)/real(NSUM)
                    co%SCANACC(i) = co%SCANACC(i)/real(NSUM)
                    co%GROACC(i) = co%GROACC(i)/real(NSUM)
                    co%FSINACC(i) = co%FSINACC(i)/real(NSUM)
                    co%FLINACC(i) = co%FLINACC(i)/real(NSUM)
                    co%FLUTACC(i) = co%FLUTACC(i)/real(NSUM)
                    co%TAACC(i) = co%TAACC(i)/real(NSUM)
                    co%UVACC(i) = co%UVACC(i)/real(NSUM)
                    co%PRESACC(i) = co%PRESACC(i)/real(NSUM)
                    co%QAACC(i) = co%QAACC(i)/real(NSUM)
                    ALTOT = (co%ALVSACC(i) + co%ALIRACC(i))/2.0
                    FSSTAR = co%FSINACC(i)*(1.0 - ALTOT)
                    FLSTAR = co%FLINACC(i) - co%FLUTACC(i)
                    QH = co%HFSACC(i)
                    QE = co%QEVPACC(i)
                    BEG = FSSTAR + FLSTAR - QH - QE
                    SNOMLT = co%HMFNACC(i)
                    if (co%RHOSACC(i) > 0.0) then
                        ZSN = co%SNOACC(i)/co%RHOSACC(i)
                    else
                        ZSN = 0.0
                    end if
                    if (co%TCANACC(i) > 0.01) then
                        TCN = co%TCANACC(i) - TFREZ
                    else
                        TCN = 0.0
                    end if
                    if (co%TSNOACC(i) > 0.01) then
                        TSN = co%TSNOACC(i) - TFREZ
                    else
                        TSN = 0.0
                    end if
                    if (shd%wc%ILG == 1) then
                        GTOUT = co%GTACC(i) - TFREZ
                    else
                        GTOUT = 0.0
                    end if

                    !> Write to the CLASSOF* output files for daily accumulated output.
                    write(150 + i*10 + 1, "(i4,',', i5,',', 9(f8.2,','), 2(f8.3,','), 999(f12.4,','))") &
                        ic%now%jday, ic%now%year, FSSTAR, FLSTAR, QH, QE, SNOMLT, &
                        BEG, GTOUT, co%SNOACC(i), co%RHOSACC(i), &
                        co%WSNOACC(i), ALTOT, co%ROFACC(i), co%ROFOACC(i), &
                        co%ROFSACC(i), co%ROFBACC(i)
                    write(150 + i*10 + 2, "(i4,',', i5,',', " // adjustl(IGND_CHAR) // "((f8.2,','), " // &
                          "2(f6.3,',')), f8.2,',', 2(f7.4,','), 2(f8.2,','))") &
                        ic%now%jday, ic%now%year, (co%TBARACC(i, j) - TFREZ, &
                        co%THLQACC(i, j), co%THICACC(i, j), j = 1, IGND), &
                        TCN, co%RCANACC(i), co%SCANACC(i), TSN, ZSN
                    write(150 + i*10 + 3, "(i4,',', i5,',', 3(f9.2,','), f8.2,',', " // &
                          "f10.2,',', e12.3,',', 2(f12.3,','))") &
                        ic%now%jday, ic%now%year, co%FSINACC(i), co%FLINACC(i), &
                        co%TAACC(i) - TFREZ, co%UVACC(i), co%PRESACC(i), &
                        co%QAACC(i), co%PREACC(i), co%EVAPACC(i)

                    !> Reset the CLASS output variables.
                    co%PREACC = 0.0
                    co%GTACC = 0.0
                    co%QEVPACC = 0.0
                    co%EVAPACC = 0.0
                    co%HFSACC = 0.0
                    co%HMFNACC = 0.0
                    co%ROFACC = 0.0
                    co%ROFOACC = 0.0
                    co%ROFSACC = 0.0
                    co%ROFBACC = 0.0
                    co%WTBLACC = 0.0
                    co%TBARACC = 0.0
                    co%THLQACC = 0.0
                    co%THICACC = 0.0
                    co%THALACC = 0.0
                    co%GFLXACC = 0.0
                    co%ALVSACC = 0.0
                    co%ALIRACC = 0.0
                    co%RHOSACC = 0.0
                    co%TSNOACC = 0.0
                    co%WSNOACC = 0.0
                    co%SNOARE = 0.0
                    co%TCANACC = 0.0
                    co%CANARE = 0.0
                    co%SNOACC = 0.0
                    co%RCANACC = 0.0
                    co%SCANACC = 0.0
                    co%GROACC = 0.0
                    co%FSINACC = 0.0
                    co%FLINACC = 0.0
                    co%FLUTACC = 0.0
                    co%TAACC = 0.0
                    co%UVACC = 0.0
                    co%PRESACC = 0.0
                    co%QAACC = 0.0
                end if !(NCOUNT == 48) then
            end if !(op%K_OUT(k) >= il1 .and. op%K_OUT(k) <= il2) then
        end do !i = 1, WF_NUM_POINTS

    end subroutine

end module
