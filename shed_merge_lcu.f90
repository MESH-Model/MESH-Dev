program run_shed_merge_lcu

    use EF_Module

    implicit none

    type(ShedParam) :: header

    character(128) :: qstr
    character(20) :: junk, junk20
    character(1) :: junk1
    character(80) :: comment
    character(79) :: hdrcomment(100)
    integer :: &
        chksum(160), thms(25), gr10k(25), saug(25), hmbr(25), &
        dffn(25), simpson(25), colum(50), canag(25), noire8k(25)
    integer :: &
        chsm, ndam1, iallcnt5, dummy1, ntest, nchr, iallocate, ios, &
        latdegmin, latminmin, latdegmax, latminmax, londegmin, &
        lonminmin, londegmax, lonminmax, i, j, n, ii, igridflg, &
        nrvr1, ntmp, l, newformat
    real :: sumclass, cintv, conv
    integer :: result1, n_hdr_lines, zone
    logical :: exists

    real, dimension(:, :), allocatable :: dummy

    data gr10k / &
        47, 161, 222, 229, 189, 142, 69, 39, 20, 7, 3, &
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /
    data noire8k / &
        0, 0, 82, 179, 299, 213, 117, 288, 263, 222, &
        206, 169, 129, 44, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /
    data ntest / -10588 / qstr / 'copyright n.kouwen' / nchr / 18 /
    data iallcnt5 / 0 /

    integer :: unitNum, flnNum, iStat

    character*1024 line, subString, tmpString
    character*128 keyword, value
    integer lineLen, keyLen, wordCount, attCount
    logical rStat, lineType, foundEndHeader

    character *64 attribName
    integer ai, vi, xi, yi, attLen, error, rank
    real*4 val

    call InitShedParam(header)


    foundEndHeader = .false.

    open(unitNum, fln(flnNum), status = 'old', iostat = ios)
    if (ios /= 0) then
        print *
        print *, 'Problems in file', fln(flnNum)
        write(*, 99162) fln(flnNum)
        write(98, 99162) fln(flnNum)
        print *, 'iostat code =', ios
        stop 'program aborted in read_shed_ef.for @ 130'
    end if

99162   format( &
            ' Warning: Error opening or reading fln:', a30 / &
            ' Probable cause: missing basin/bsnm_???_par.r2c input file' / &
            ' OR: in config.sys have you set files=100 & buffers=50?' / &
            ' OR: wrong number in line 2 of the event file for ' / &
            '     number of events listed in event file ')

    line(1:1) = '#'

    do while ( &
        (.not. foundEndHeader) .and. &
        ((line(1:1) == '#') .or. (line(1:1) == ':') .or.
        (len_trim(line) == 0)))

        read(unitNum, fmt='((A))', iostat = ios) line
        if (ios == -1) then
            write(6, '((A))') 'ERROR: Premature EndOfFile encountered'
            stop ' Stopped in read_shed_ef'
        end if

        rStat = Detab(line)
        line = adjustl(line)
        lineLen = len_trim(line)

        if (line(1:1) == ':') then
            wordCount = SplitLine(line, keyword, subString)
            rStat = ToLowerCase(keyword)
            KeyLen = len_trim(keyword)

            if (keyword(1:KeyLen) == ':endheader') then
                foundEndHeader = .true.
            else
                iStat = ParseShedParam(header, keyword, keyLen, subString)
                if (iStat < 0) then
                    write(*, '(2(A))') 'ERROR parsing ', fln(flnNum)
                    write(*, '(2(A))') '   in line: ', line
                    stop ' Stopped in read_shed_ef'
                    return
                end if
            end if
        end if
    end do !while (

    coordsys1 = header%r2cp%csp%projection
    datum1 = header%r2cp%csp%ellipsoid

    open(99, file = 'junk', status = 'unknown')
    write(99, 99000) header%r2cp%csp%zone
    rewind 99
    read(99, 99001) zone1
    close(99, status = 'delete')

99000 format(i10)
99001 format(a10)

    xcount = header%r2cp%xCount
    ycount = header%r2cp%yCount
    xorigin = header%r2cp%xOrigin
    yorigin = header%r2cp%yOrigin
    xdelta = header%r2cp%xDelta
    ydelta = header%r2cp%yDelta

    al = header%nominalGridSize_AL
    cintv = header%contourInterval

    ntype = header%classCount - 1

    nrvr = header%numRiverClasses

    conv = header%r2cp%unitConv

    na = header%totalNumOfGrids
    naa = header%numGridsInBasin
    nnprint = header%debugGridNo

    astep = al/1000.
    istep = int(astep)
    step2 = astep*astep
    if (istep < 1) istep = 1

    attCount = header%r2cp%ep%attCount
    call LoadAttributeData(header%r2cp%ep, xCount, yCount, unitNum)

    imaxi = ycount
    jmaxi = xcount

    if (IsLatLong(header%r2cp%csp)) then
        iymin = int(yorigin*60.0)
        iymax = int((yorigin + ycount*ydelta)*60.0)
        jxmin = int(xorigin*60.0)
        jxmax = int((xorigin + xcount*xdelta)*60.0)
        llflg = 'y'
        grde = xdelta*60.
        grdn = ydelta*60.
    else
        grde = xdelta/1000.
        grdn = ydelta/1000.
        jxmin = int(xorigin/1000.)
        jxmax = jxmin + grde*(xcount - 1)
        iymin = int(yorigin/1000.)
        iymax = iymin + grdn*(ycount - 1)
        llflg = 'n'
    end if !(IsLatLong(header%r2cp%csp)) then

    nastart = 1
    naend = naa
    imini = 1
    jmini = 1
    ib = imini + 1
    it = imaxi - 1

    if (iallcnt5 == 1) then

        allocate( &
            s(imaxi, jmaxi), dummy(imaxi, jmaxi), &
            xxx(na), yyy(na), flz2(na), pwr2(na), &
            sl2(na), irough(na), aclass(na,ntype+1), glacier_flag(na), &
            stat = iAllocate)

        do ai = 1, attCount

            attribName = header%r2cp%ep%attList(ai)%name(:)
            rStat = ToLowerCase(attribName)
            attLen = len_trim(attribName)
            if (attribName(1:attLen) == 'next') then
                allocate(next(na), stat = iAllocate)
            else if (attribName(1:attLen) == 'da') then
                allocate(da(na), stat = iAllocate)
            else if (attribName(1:attLen) == 'bankfull') then
                allocate(bnkfll(na), stat = iAllocate)
            else if (attribName(1:attLen) == 'chnlslope') then
                allocate(slope(na), stat = iAllocate)
            else if (attribName(1:attLen) == 'elev') then
                allocate(elev(na), stat = iAllocate)
            else if (attribName(1:attLen) == 'chnllength') then
                allocate(rl(na), stat = iAllocate)
            else if (attribName(1:attLen) == 'iak') then
                allocate(ibn(na), stat = iAllocate)
            else if (attribName(1:attLen) == 'intslope') then
                allocate(sl1(na), stat = iAllocate)
            else if (attribName(1:attLen) == 'chnl') then
                allocate(ichnl(na), stat = iAllocate)
            else if (attribName(1:attLen) == 'reach') then
                allocate(ireach(na), stat = iAllocate)
            else if (attribName(1:attLen) == 'demslope') then
                allocate(demslp(na), stat = iAllocate)
            else if (attribName(1:attLen) == 'drdn') then
                allocate(drdn(na), stat = iAllocate)
            else if (attribName(1:attLen) == 'flz') then
                allocate(flz(na), stat = iAllocate)
            else if (attribName(1:attLen) == 'pwr') then
                allocate(pwr(na), stat = iAllocate)
            else if (attribName(1:attLen) == 'r1n') then
                allocate(r1n(na), stat = iAllocate)
            else if (attribName(1:attLen) == 'r2n') then
                allocate(r2n(na), stat = iAllocate)
            else if (attribName(1:attLen) == 'mndr') then
                allocate(mndr(na), stat = iAllocate)
            else if (attribName(1:attLen) == 'aa2') then
                allocate(aa2(na), stat = iAllocate)
            else if (attribName(1:attLen) == 'aa3') then
                allocate(aa3(na), stat = iAllocate)
            else if (attribName(1:attLen) == 'aa4') then
                allocate(aa4(na), stat = iAllocate)
            else if (attribName(1:attLen) == 'theta') then
                allocate(theta(na), stat = iAllocate)
            else if (attribName(1:attLen) == 'widep') then
                allocate(widep(na), stat = iAllocate)
            else if (attribName(1:attLen) == 'kcond') then
                allocate(kcond(na), stat = iAllocate)
            else if (attribName(1:attLen) == 'gridarea') then
                allocate(grid_area(na), stat = iAllocate)
                allocate(frac(na), stat = iAllocate)
            end if

        end do !ai = 1, attCount

        if (iAllocate /= 0) stop &
            'Error with allocation of area16a arrays in sheda'

        iallcnt5 = 2

    end if !(iallcnt5 == 1) then

    if (unitnum == 31) then

!       for the shed file:
        do ai = 1, attCount - (ntype + 1)
            vi = 0
            attribName = header%r2cp%ep%attList(ai)%name
            rStat = ToLowerCase(attribName)
            attLen = len_trim(attribName)
            if (attribName(1:attLen) == 'rank') then
                do yi = 1, yCount
                    do xi = 1, xCount
                        vi = vi + 1
                        val = header%r2cp%ep%attList(ai)%val(vi)
                        s(yi, xi) = val
                    end do
                end do
            end if
        end do
    else

!       for the par file:
        do ai = 1, attCount
            vi = 0
            attribName = header%r2cp%ep%attList(ai)%name
            rStat = ToLowerCase(attribName)
            attLen = len_trim(attribName)
            if (attribName(1:attLen) == 'rank') then
                do yi = 1, yCount
                    do xi = 1, xCount
                        vi = vi + 1
                        val = header%r2cp%ep%attList(ai)%val(vi)
                        s(yi, xi) = val
                    end do
                end do
            end if
        end do
    end if

    vi = 0

    do yi = 1, yCount
        do xi = 1, xCount
            vi = vi + 1
            rank = s(yi, xi)
            if (rank > 0) then
                do ai = 1, attCount

                    attribName = header%r2cp%ep%attList(ai)%name
                    rStat = ToLowerCase(attribName)
                    attLen = len_trim(attribName)
                    val = header%r2cp%ep%attList(ai)%val(vi)

                    if (attribName(1:attLen) == 'next') then
                        next(rank) = int(val)
                    else if (attribName(1:attLen) == 'da') then
                        da(rank) = val
                    else if (attribName(1:attLen) == 'bankfull') then
                        bnkfll(rank) = val
                    else if (attribName(1:attLen) == 'chnlslope') then
                        slope(rank) = val
                    else if (attribName(1:attLen) == 'elev') then
                        elev(rank) = val
                    else if (attribName(1:attLen) == 'chnllength') then
                        rl(rank) = val
                    else if (attribName(1:attLen) == 'iak') then
                        ibn(rank) = val
                    else if (attribName(1:attLen) == 'intslope') then
                        sl1(rank) = val
                    else if (attribName(1:attLen) == 'chnl') then
                        ichnl(rank) = val
                    else if (attribName(1:attLen) == 'reach') then
                        ireach(rank) = val
                    else if (attribName(1:attLen) == 'drdn') then
                        drdn(rank) = val
                    else if (attribName(1:attLen) == 'demslope') then
                        demslp(rank) = val
                    else if (attribName(1:attLen) == 'flz') then
                        flz(rank) = val
                    else if (attribName(1:attLen) == 'pwr') then
                        pwr(rank) = val
                    else if (attribName(1:attLen) == 'r1n') then
                        r1n(rank) = val
                    else if (attribName(1:attLen) == 'r2n') then
                        r2n(rank) = val
                    else if (attribName(1:attLen) == 'mndr') then
                        mndr(rank) = val
                    else if (attribName(1:attLen) == 'aa2') then
                        aa2(rank) = val
                    else if (attribName(1:attLen) == 'aa3') then
                        aa3(rank) = val
                    else if (attribName(1:attLen) == 'aa4') then
                        aa4(rank) = val
                    else if (attribName(1:attLen) == 'theta') then
                        theta(rank) = val
                    else if (attribName(1:attLen) == 'widep') then
                        widep(rank) = val
                    else if (attribName(1:attLen) == 'kcond') then
                        kcond(rank) = val
                    else if (attribName(1:attLen) == 'gridarea') then
                        grid_area(rank) = val
                        frac(rank) = grid_area(rank)/al/al
                    end if
                end do !ai = 1, attCount
            end if !(rank > 0) then
        end do !xi = 1, xCount
    end do !yi = 1, yCount

    manningflg = 'y'

    if (unitnum == 31) then
        vi = 0
        do yi = 1, yCount
            do xi = 1, xCount
                vi = vi + 1
                rank = s(yi, xi)
                if (rank > 0) then
                    do ai = attCount - ntype, attCount
                        val = header%r2cp%ep%attList(ai)%val(vi)
                        aclass(rank, ai - (attCount - (ntype + 1))) = val
                    end do
                end if !(rank > 0) then
            end do !xi = 1, xCount
        end do !yi = 1, yCount
    end if

    iiprint = 1
    do ii = 2, ntype + 1
        if (aclass(nnprint, ii) > aclass(nnprint, iiprint)) iiprint = ii
    end do

    vi = 0
    do yi = 1, yCount
        do xi = 1, xCount
            vi = vi + 1
            rank = s(yi, xi)
            if (rank > 0) then
                xxx(rank) = xi
                yyy(rank) = yi
            end if
        end do
    end do

    do i = 1, ycount
        do j = 1, xcount
            dummy(i, j)=0.0
        end do
    end do

50000 format(999f6.2)
50001 format('class no=', i5)
50002 format('Land Cover class fractions:')

    do ai = 1, attCount
        deallocate (header%r2cp%ep%attList(ai)%val, stat = error)
        if (error /= 0) stop 'deallocation error in read_gsm_ef()'
    end do

    ichsm=1
    do i = 1, min(25, imaxi)
        chsm = 0
        do j = jmini, jmaxi
            chsm = chsm + s(i, j)
        end do
        if (chsm /= chksum(i)) then
            ichsm = 0
        end if
    end do

    chsm = 0
    do i = 1, min(25, imaxi)
        do j = jmini, jmaxi
            chsm = chsm + s(i, j)
        end do
    end do
    if (chsm == 0) then
        ichsm = 0
    end if

    if (nnprint > naa) then
        nnprint = naa/2
        ipr = yyy(nnprint)
        jpr = xxx(nnprint)
    else
        ipr = yyy(nnprint)
        jpr = xxx(nnprint)
    end if

    igridflg = 0
    do n = 1, naa
        sumclass = 0.0
        do ii = 1, ntype + 1
            sumclass = sumclass + aclass(n, ii)
        end do
        if (sumclass /= 1.0) then
            igridflg = 1
            write(*, 9023) n, yyy(n), xxx(n), sumclass
            do ii = 1, ntype + 1
                if (sumclass > 0.0) then
                    aclass(n, ii) = aclass(n, ii)/sumclass
                else
                    write(*, 9024) n, yyy(n), xxx(n)
                end if
            end do !ii = 1, ntype + 1
        end if !(sumclass /= 1.0) then
    end do !n = 1, naa

    nrvr1 = 0
    do n = 1, na

!       moved this to flowinit at one point but then it got
!       recalculated with each iteration when optimizing.
!       A serious snafu resulting in the convergence problem on opt.

        if (slope(n) < 0.0) then
            print *, 'In read_shed_ef reading the file :', fln(flnNum)
            print *, 'The slope in grid no ', n, ' is ', slope(n)
            print *, 'Please check the elevations in the map file '
            print *, 'or change the slope value in the shd file'
            print *, 'The former is recommended as the permanent solution'
            print *
            stop 'Program aborted in read_shed_ef @ 756'
        end if
        sl2(n) = sqrt(sl1(n))
        nrvr1 = max(nrvr1, ibn(n))
    end do !n = 1, na

    close(unitNum)

    write(*, 6006)
    write(*, 6007)

    write(*, *) ' Note: order not the same as the .shd file YET'

    if (.not. allocated(wetwid)) then
        allocate( &
            wetwid(na), chawid(na), chadep(na), wstore1(na), wstore2(na), &
            wcap(na), flowxa(na), chaxa(na), satxa(na), wetxa(na), hcha1(na), &
            hcha2(na), hwet1(na), hwet2(na), qin(na), qswevp(na), &
            qswrain(na), qiwet1(na), qiwet2(na), qowet1(na), qowet2(na), &
            wetarea(na), chaarea(na), bin_precip(na), wsat(na), wetfrac(na), &
            stat = iAllocate)
        if (iAllocate /= 0) stop &
            'Warning: error with allocation of areaswmp in read_shed_ef'
    end if !(.not. allocated(wetwid)) then

    if (.not. allocated(qi1)) then
        allocate( &
            qi1(na), qi2(na), qo1(na), qo2(na), qr(na), &
            d2(na), qda(na), cap(na), over(na), &
            qmax(na), res(na), &
            sump(na), store1(na), store2(na), att(na), &
            qbase(na), nreach(30), &
            rf(na, ntype + 1), rffs(na, ntype + 1), &
            r(na, ntype + 1), effpor(na, ntype + 1), &
            v(na, ntype + 1), totd1(na), totuzs(na), totsnw(na), qstream(na), &
            totchnl(na), totgrid(na), netflow(na), storinit(na), d1(na, ntype + 1), &
            d1fs(na, ntype + 1), uzs(na, ntype + 1), uzsfs(na, ntype + 1), lzs(na), &
            sumf(na, ntype + 1), sumrechrg(na), &
            sumffs(na, ntype + 1), snow(na, ntype + 1), sumrff(na), rechrg(na), &
            qlz(na), sr(ntype + 1), x4_R(ntype + 1), x5_R(ntype + 1), q1(na, ntype + 1), &
            q1fs(na, ntype + 1), qint(na, ntype + 1), qintfs(na, ntype + 1), &
            fake(ntype + 1), fakefs(ntype + 1), &
            qdrng(na), qdrngfs(na), &
            drng(na, ntype + 1), drngfs(na, ntype + 1), sq1(ntype + 1), &
            sq1fs(ntype + 1), sqint(ntype + 1), sqintfs(ntype + 1), sdrng(ntype + 1), &
            sdrngfs(ntype + 1), sexcess(ntype + 1), qstrm(na), &
            sumq1(na), sumqint(na), sumq1fs(na), sumqintfs(na), &
            stat = iAllocate)
        if (iAllocate /= 0) stop &
            'Warning: error with allocation of area1 in read_shed_ef'
    end if !(.not. allocated(qi1)) then

    if (.not. allocated(strloss)) then
        allocate(strloss(na), stat = iAllocate)
        if (iAllocate /= 0) stop &
            'Warning: error with allocation of areaeta arrays in spl9'
    end if

    if (.not. allocated(df)) then
        allocate(df(na, ntype + 1), dffs(na, ntype + 1), &
            qdrng2(na), qdrngfs2(na), &
            stat = iAllocate)
        if (iAllocate /= 0) stop
            'Warning: error with allocation of areaeta arrays in spl9'
    end if

    return

9998    ntmp = 0
        ndam = 0
        write(*, 9025)
        write(*, 9026)
        close(34)
        return

9999    ndam = 0
        write(*, 9026)
        close(34)
        return

99901   write(*, 99902) fln(4)
99902   format(' file', a30, ' not found for unit 34 - check event file')
        stop 'program stopped in shed.for at 99902'

99910   write(*, 99911) fln(4)
99911   format(' no data found or problems with data in ',  a30)
        stop 'program stopped in shed at 99911'


1000    format(i5)
1002    format(' ', i5, 'stream gage locations have been passed over')
1003    format(' ', i5, 'reservoir locations have been passed over')
1004    format(' ', i5, 'damage sites:')
1098    format(a80)
1099    format(2i5, 1x, a12, 7x, 4e10.3, f10.3)
1100    format(' ', 2i5, 1x, a12, 7x, 4e10.3, f10.3)
1101    format(' reading the stream gauge location file: ', a30)
1102    format(' ', 2i5, 1x, a12, 7x, 4e10.3, f10.3/)
1776    format(' ', 'l,iys(l),jxs(l)', 5i5)

5000    format(' Debug grid reset to grid number n,row col', 3i7)

6004    format(1x, 4i4, f7.1, f7.2, f8.4, 2f7.0, i3, f7.4, 2i3, 7f5.2)
6006    format(2x, 'basin file:')
6007    format( &
            4x, 'n   yy   xx      da       cap       slope    elv', &
            '     ibn   sl2    ich  next reach frac  imp area & fractions')
6014    format(i5)

9005    format(12i5, 2f5.0)

9023    format(' Warning: area correction in grid(n,i,j)', 3i5, f9.5)
9024    format(' Warning: total area = 0.0 for grid(n,i,j)', 3i5)
9025    format(' Warning: no reservoirs or lakes in bsnm.str file')
9026    format(' Warning: no damage sites in bsnm.str file')

end program !> run_shed_merge_lcu
