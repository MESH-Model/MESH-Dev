    subroutine READ_RUN_OPTIONS(ts, cm, fls)

        use mpi_flags

        use strings
        use sa_mesh_shared_variables
        use model_files_variabletypes
        use model_files_variables
        use model_dates
        use climate_forcing
        use FLAGS

        use SIMSTATS_config, only: mtsflg

        use RUNCLASS36_constants
        use RUNCLASS36_save_output
        use RUNSVS113_variables

        use WF_ROUTE_config

        use baseflow_module, only: lzsp

        use SA_RTE_module, only: SA_RTE_flgs

        !> Cropland irrigation module.
        use cropland_irrigation_variables

        implicit none

        !> Input variables.
        type(dates_model) :: ts
        type(CLIM_INFO) :: cm
        type(fl_ids) :: fls

        !> Local variables for parsing control flag strings.
        integer, parameter :: MaxLenField = 20, MaxArgs = 20, MaxLenLine = 100
        character(MaxLenLine) in_line
        character(MaxLenField), dimension(MaxArgs) :: out_args
        integer nargs
        character(1) delim

        !> Temporary variables.
        integer CONFLAGS, IROVAL, ierr, iun, j, i
        character(20) IRONAME
        character(10) GENDIR_OUT

        !>
        !> SET RUN OPTIONS
        !> READ THE RUN_OPTIONS.INI INPUT FILE TO SET OR RESET ANY CONTROL
        !> FLAGS AND READ THE GRID OUTPUT DIRECTORIES.
        !>
        !>    * IF IDISP = 0, VEGETATION DISPLACEMENT HEIGHTS ARE IGNORED,
        !>    * BECAUSE THE ATMOSPHERIC MODEL CONSIDERS THESE TO BE PART OF THE
        !>    * "TERRAIN".
        !>    * IF IDISP = 1, VEGETATION DISPLACEMENT HEIGHTS ARE CALCULATED.
        IDISP = 1

        !>    * IF IZREF = 1, THE BOTTOM OF THE ATMOSPHERIC MODEL IS TAKEN TO
        !>    * LIE AT THE GROUND SURFACE.
        !>    * IF IZREF = 2, THE BOTTOM OF THE ATMOSPHERIC MODEL IS TAKEN TO
        !>    * LIE AT THE LOCAL ROUGHNESS HEIGHT.
        IZREF = 1

        !>    * IF ISLFD = 0, DRCOEF IS CALLED FOR SURFACE STABILITY
        !>    * CORRECTIONS AND THE ORIGINAL GCM SET OF SCREEN-LEVEL DIAGNOSTIC
        !>    * CALCULATIONS IS DONE.
        !>    * IF ISLFD = 1, DRCOEF IS CALLED FOR SURFACE STABILITY
        !>    * CORRECTIONS AND SLDIAG IS CALLED FOR SCREEN-LEVEL DIAGNOSTIC
        !>    * CALCULATIONS.
        !>    * IF ISLFD = 2, FLXSURFZ IS CALLED FOR SURFACE STABILITY
        !>    * CORRECTIONS AND DIASURF IS CALLED FOR SCREEN-LEVEL DIAGNOSTIC
        !>    * CALCULATIONS.
        ISLFD = 2

        !>    * IF IPCP = 1, THE RAINFALL-SNOWFALL CUTOFF IS TAKEN TO LIE AT
        !>    * 0 dC.
        !>    * IF IPCP = 2, A LINEAR PARTITIONING OF PRECIPITATION BETWEEEN
        !>    * RAINFALL AND SNOWFALL IS DONE BETWEEN 0 dC AND 2 dC.
        !>    * IF IPCP = 3, RAINFALL AND SNOWFALL ARE PARTITIONED ACCORDING TO
        !>    * A POLYNOMIAL CURVE BETWEEN 0 dC AND 6 dC.
        !>    * IF IPCP=4, THE RAINFALL, SNOWFALL AND TOTAL PRECIPITATION RATES
        !>    * ARE READ IN DIRECTLY.
        IPCP = 1

        !>    * ITC, ITCG AND ITG ARE SWITCHES TO CHOOSE THE ITERATION SCHEME
        !>    * TO BE USED IN CALCULATING THE CANOPY OR GROUND SURFACE
        !>    * TEMPERATURE RESPECTIVELY.  IF THE SWITCH IS SET TO 1, A
        !>    * COMBINATION OF SECANT AND BISECTION METHODS IS USED; IF TO 2,
        !>    * THE NEWTON-RAPHSON METHOD IS USED.
        ITC = 2
        ITCG = 2
        ITG = 2

        !>    * IF IWF = 0, ONLY OVERLAND FLOW AND BASEFLOW ARE MODELLED, AND
        !>    * THE GROUND SURFACE SLOPE IS NOT MODELLED.
        !>    * IF IWF = 1, THE MODIFIED CALCULATIONS OF OVERLAND
        !>    * FLOW AND INTERFLOW ARE PERFORMED.
        !>    * IF IWF = 2, SAME AS IWF = 0 EXCEPT THAT OVERLAND FLOW IS
        !>    * MODELLED AS FILL AND SPILL PROCESS FROM A SERIES OF POTHOLES.
        !>    * DEFAULT VALUE IS 1.
        IWF = 1

        !>    * IF IPAI, IHGT, IALC, IALS AND IALG ARE ZERO, THE VALUES OF
        !>    * LEAF ARE INDEX, VEGETATION HEIGHT, CANOPY ALBEDO, SNOW ALBEDO
        !>    * AND SOIL ALBEDO RESPECTIVELY CALCULATED BY CLASS ARE USED.
        !>    * IF ANY OF THESE SWITCHES IS SET TO 1, THE VALUE OF THE
        !>    * CORRESPONDING PARAMETER CALCULATED BY CLASS IS OVERRIDDEN BY
        !>    * A USER-SUPPLIED INPUT VALUE.
        IPAI = 0
        IHGT = 0
        IALC = 0
        IALS = 0
        IALG = 0

        !>    * ICTEMMOD IS SET TO 1 IF CLASS IS BEING RUN IN CONJUNCTION WITH
        !>    * THE CANADIAN TERRESTRIAL ECOSYSTEM MODEL "CTEM"; OTHERWISE
        !>    * ICTEMMOD IS SET TO 0.
        ICTEMMOD = 0

        !> DAN * IF RELFLG = 0, ANY CONFIGURATION FILE IS READ THAT MATCHES
        !> DAN * THE FILE NAME IN THE OPEN STATEMENT.
        !> DAN * IF RELFLG = 1, ONLY CONFIGURATION FILES WHOSE VERSION MATCHES
        !> DAN * THE RELEASE OF MESH_DRIVER ARE READ.  THE PROGRAM STOPS IF THE
        !> DAN * TWO STRINGS DO NOT MATCH.
        !> DAN * THIS FLAG IS NOT APPLICABLE TO RUN_OPTIONS.INI, WHERE THIS FLAG
        !> DAN * MAY BE RESET).
        RELFLG = 1

        !> Set HOURLYFLAG to forcing data time step (in minutes).
        !> 30 minute forcing data should be the default
!-        HOURLYFLAG = 30

        !> set SHDFILEFLAG to 0 to use drainage_database.txt
        !> set SHDFILEFLAG to 1 to use new_shd.r2c
        SHDFILEFLAG = 1

        !> set RESUMEFLAG to 0 just as a default
        !> this will be set later by reading the run_options file
        !* if RESUMEFLAG is 0, the user doesn't want to use the resume file
        !* if RESUMEFLAG is 1, the user wants to run the resume file.
        !* if RESUMEFLAG is 2, the user wants to run the r2c resume file.
        RESUMEFLAG = 0

        !* if SAVERESUMEFLAG is 0, the user doesn't want to make the resume file.
        !* if SAVERESUMEFLAG is 1, the user wants to make the resume file.
        !* if SAVERESUMEFLAG is 2, the user wants to make the r2c resume file.
        !* if SAVERESUMEFLAG is 3, the user wants to make the only class prognostic variables resume file.
        SAVERESUMEFLAG = 0

        !> FORCING DATA FILES:
        !>  0 = read forcing data from .bin file
        !>  1 = read forcing data from .r2c
        !>  2 = read forcing data from .csv
        !>  3 = read forcing data from .seq binary sequential files
        !>  3 = read forcing data from .seq ascii sequential files
        !>  5 = read forcing data from load buffer in memory
!-        BASINSHORTWAVEFLAG = 0
!-        BASINLONGWAVEFLAG = 0
!-        BASINRAINFLAG = 0
!-        BASINTEMPERATUREFLAG = 0
!-        BASINWINDFLAG = 0
!-        BASINPRESFLAG = 0
!-        BASINHUMIDITYFLAG = 0

        !> SOIL INITIALIZATION  FLAG - DEFAULT = STOP SIMULATION IF SUM OF SOIL PERCENTAGES EXCEEDS 100%
        !> If SOILINIFLAG is 0, stop simulation if the sum of soil percentages is greater than 100%
        !> If SOILINIFLAG is 1, no adjustment to soil percentages even if the sum is greater than 100%
        !> If SOILINIFLAG is 2, adjust soil percentages in favor of sand
        !> If SOILINIFLAG is 3, adjust soil percentages in favor of clay
        !> If SOILINIFLAG is 4, adjust soil percentages proportionally
        !> If SOILINIFLAG is 5, directly read soil parameter values from soil.ini file.
        SOILINIFLAG = 0

        !> If OBJFNFLAG is 0 {DEFAULT} = SAE - SUM OF ABSOLUTE VALUE OF ERRORS
        !> If OBJFNFLAG is 1, SAESRT - SUM OF ABSOLUTE VALUE OF ERRORS AFTER SORTING
        !> If OBJFNFLAG is 2, SAEMSRT - SUM OF ABSOLUTE VALUE OF MOVING ERRORS AFTER SORTING
        !> If OBJFNFLAG is 3, NSE - MEAN NASH-SUTCLIFFE MODEL EFFICIENCY INDEX (+ve FOR MAXIMIZATION)
        !> IF OBJFNFLAG is 4, NSE - MEAN NASH-SUTFLIFFE MODEL EFFICIENCY INDEX (-ve FOR MINIMIZATION)
        OBJFNFLAG = 0

        WINDOWSIZEFLAG = 1
        WINDOWSPACINGFLAG = 1

        METRICSSTATSOUTFLAG = 1
        METRICSFILTEROBSFLAG = 1

        !> METRICSSPINUP specifies the starting day from which to calculate metrics.
        !> The starting day is relative to the beginning of the simulation; Day 1 is
        !> the first day of the simulation, regardless of the date or its Julian date
        !> in the year. If METRICSINCLUDESPINUP is set to 1, METRICSSPINUP is not used.
        METRICSSPINUP = 1

        !> If METRICSINCLUDESPINUP is set to 1 then metrics are calculated from the
        !> first day of the simulation (1:ndsim).
        !> If METRICSINCLUDESPINUP is set to 0 then metrics are calculated from
        !> METRICSSPINUP (METRICSSPINUP:ndsim).
        METRICSINCLUDESPINUP = 0

        !> If FROZENSOILINFILFLAG is 0, all snow melt infiltrates.
        !> If FROZENSOILINFILFLAG is 1, snow melt is partitioned to frozen soil infiltration
        !> and direct runoff based on the parameteric equation developed by Gray et al, 2001.
        FROZENSOILINFILFLAG = 0

        !> FORCIND DATA INTERPOLATION AT INTERMEDIATE TIME STEPS (WHEN THE TIME
        !> INTERVAL OF THE FORCING DATA IS GREATER THAN 30 MINUTE)
        !> DEFAULT = NO INTERPOLATION
!-        INTERPOLATIONFLAG = 0

        !* If SUBBASINFLAG is 1, calculations will only be done for grid squares that are
        !* in the watersheds of the locations listed in the streamflow files.
        !* If SUBBASINFLAG is 0, calculations will be made for all grid squares.
        SUBBASINFLAG = 0

        !* If R2COUTPUTFLAG is 1, R2C ascii file will be written for user specified
        !* variables.
        !* If R2COUTPUTFLAG is 2, R2C binary will be written for user specified
        !* variables (list of variables will be read from r2c_output.txt file).
        R2COUTPUTFLAG = 0

        !* If FROZENSOILINFILFLAG is 0, all snow melt infiltrates.
        !* If FROZENSOILINFILFLAG is 1, snow melt is partitioned to frozen soil infiltration
        !* and direct runoff based on the parameteric equation developed by Gray et al, 2001.
        FROZENSOILINFILFLAG = 0

        !* If WD3 is 0, existing WATDRN is used.
        !* If WD3 is 1, WATDRN by Ric (May, 2011) is used.
        WD3 = 0

        !* If WD3NEWFILE is 0, an existing "soil_out.txt" for MAPLE is used.
        !* If WD3NEWFILE is 1, "soil_out.txt" for MAPLE is created or overwritten.
        WD3NEWFILE = 1

        !* If WD3FLOW is 0, SUBFLW=SUBFLW,BASFLW=BASFLW.
        !* If WD3FLOW is 1, SUBFLW=SUBFLW+BASFLW,BASFLW=0.
        !* If WD3FLOW is 2, SUBFLW=SUBFLW,BASFLW=0.
        WD3FLOW = 0

        !* If WD3BKFC is 0, BULK_FC (WATROF)=0.
        !* If WD3BKFC is 1, BULK_FC remains unchanged in WATROF.
        WD3BKFC = 1

        !* set PBSMFLAG = 0 so by default blowing snow calculations are not made
        !* 1 =  blowing snow transport, sublimation & inter-GRU redistribution calculations are made
        PBSMFLAG = 0

        !* If LOCATIONFLAG is 0, gauge coordinates are read using 2I5 (Minutes) {Default}
        !* If LOCATIONFLAG is 1, gauge coordinates for BOTH MESH_input_streamflow.txt AND
        !*                       MESH_input_reservoir.txt are read using 2F7.1 (Minutes with 1 decimal)
        LOCATIONFLAG = 0

        !> SET N = 0 RESETS THE CLASS COUNTER.
!TODO: N is not a flag, move it somewhere else
!-        N = 0

        !> FLAGS FOR GEOTHERMAL FLUX FOR THE BOTTOM OF THE LAST SOIL LAYER
        !* If GGEOFLAG is GT 0,  READ UNIQUE VALUE FROM MESH_ggeo.INI FILE
        GGEOFLAG = 0

        !> BASIN ENERGY BALANCE OUTPUT FLAG
        !> If enabled, saves the energy balance output files.
        !>     0 = Create no output.
        !>     1 = Save the basin energy balance CSV files.
        BASINAVGEBFILEFLAG = 0

        !> BASIN SWE OUTPUT FLAG
        !> If enabled, saves the SCA and SWE output files.
        !>     0 = Create no output.
        !>     1 = Save the SCA and SWE output files.
        BASINSWEOUTFLAG = 0

        !> MODEL INFO OUTPUT FLAG
        !> If enabled, saves model configuration and run information to the
        !> echo_print.txt file.
        !>     0 = Create no output.
        !>     1 = Save the model configuration and run information to the
        !>         echo_print.txt file.
        MODELINFOOUTFLAG = 1

        !> The above parameter values are defaults, to change to a different
        !> value, use the MESH_input_run_options.ini file

!todo make this more clear for the user
!todo at the top, make a brief discription about the changes between
!todo  the different versions

        !>
        !> Open and read in values from MESH_input_run_options.ini file.
        !>
        iun = fls%fl(mfk%f53)%iun
        open(iun, &
             file = trim(adjustl(fls%fl(mfk%f53)%fn)), &
             status = 'old', &
             action = 'read', &
             iostat = ierr)

        !> Check for IOSTAT errors from opening the file.
        if (ierr /= 0) then
            print *
            print *, &
                'MESH_input_run_options.ini could not be opened.', &
                'Ensure that the file exists and restart the program.'
            stop
        else if (ro%VERBOSEMODE > 0) then
            write(6, '(a)', advance = 'no') 'READING: MESH_input_run_options.ini '
        end if

        !> Begin reading the control flags.
        do i = 1, 3
            read(iun, *)
        end do
        read(iun, '(i5)') CONFLAGS

        !> Read and parse the control flags.
        if (CONFLAGS > 0) then

            !> Control flags are parsed by space.
            delim = ' '
            do i = 1, CONFLAGS

                !> Read and parse the entire line.
                read(iun,'(A)') in_line
                call parse(in_line, delim, out_args, nargs)
                if (.not. nargs > 0) then
                    print 9358, i
                    stop
                end if

                !>
                !> PARSE CONTROL FLAG BEGINS.
                !>

                !> Determine the control flag and parse additional arguments.
                select case (trim(adjustl(out_args(1))))

                    case ('IDISP')
                        call value(out_args(2), IDISP, ierr)
                    case ('IZREF')
                        call value(out_args(2), IZREF, ierr)
                    case ('ISLFD')
                        call value(out_args(2), ISLFD, ierr)
                    case ('IPCP')
                        call value(out_args(2), IPCP, ierr)
                    case ('ITC')
                        call value(out_args(2), ITC, ierr)
                    case ('ITCG')
                        call value(out_args(2), ITCG, ierr)
                    case ('ITG')
                        call value(out_args(2), ITG, ierr)
                    case ('IWF')
                        call value(out_args(2), IWF, ierr)
                    case ('IPAI')
                        call value(out_args(2), IPAI, ierr)
                    case ('IHGT')
                        call value(out_args(2), IHGT, ierr)
                    case ('IALC')
                        call value(out_args(2), IALC, ierr)
                    case ('IALS')
                        call value(out_args(2), IALS, ierr)
                    case ('IALG')
                        call value(out_args(2), IALG, ierr)
                    case ('RESUMEFLAG')
                        call value(out_args(2), RESUMEFLAG, ierr)
                    case ('SAVERESUMEFLAG')
                        call value(out_args(2), SAVERESUMEFLAG, ierr)

                    !> Basin forcing time-step flag.
                    case ('HOURLYFLAG')
                        call value(out_args(2), IROVAL, ierr)
                        if (ierr == 0) then
                            do j = 1, cm%nclim
                                cm%dat(j)%hf = IROVAL
                            end do
                        end if

                    !> Model time-step.
                    case ('TIMESTEPFLAG')
                        call value(out_args(2), ic%dtmins, ierr)
                        ic%dts = ic%dtmins*60

                    case ('RELFLG')
                        call value(out_args(2), RELFLG, ierr)

                    !> CONSOLE OUTPUT OPTIONS
                    case ('VERBOSEMODE')
                        call value(out_args(2), ro%VERBOSEMODE, ierr)
                    case ('DIAGNOSEMODE')
                        call value(out_args(2), ro%DIAGNOSEMODE, ierr)

                    !> MPI OPTIONS
                    case ('MPIUSEBARRIER')
                        call value(out_args(2), MPIUSEBARRIER, ierr)

                    !> BASIN FORCING DATA OPTIONS
                    !> Basin forcing data.
                    case ('BASINSHORTWAVEFLAG')
                        call value(out_args(2), cm%dat(ck%FB)%ffmt, ierr)
                        if (ierr == 0) cm%dat(ck%FB)%factive = .true.
                        cm%dat(ck%FB)%id_var = 'FB'
                        if (cm%dat(ck%FB)%ffmt == 5) then
                            call value(out_args(3), cm%dat(ck%FB)%ffmt, ierr)
                            call value(out_args(4), cm%dat(ck%FB)%nblocks, ierr)
                        end if
                        do j = 3, nargs
                            if (len_trim(out_args(j)) > 3) then
                                if (out_args(j)(1:3) == 'hf=') then
                                    call value(out_args(j)(4:), cm%dat(ck%FB)%hf, ierr)
                                end if
                            end if
                            if (len_trim(out_args(j)) > 4) then
                                if (out_args(j)(1:4) == 'nts=') then
                                    call value(out_args(j)(5:), cm%dat(ck%FB)%nblocks, ierr)
                                end if
                            end if
                        end do
                    case ('BASINLONGWAVEFLAG')
                        call value(out_args(2), cm%dat(ck%FI)%ffmt, ierr)
                        if (ierr == 0) cm%dat(ck%FI)%factive = .true.
                        cm%dat(ck%FI)%id_var = 'FI'
                        if (cm%dat(ck%FI)%ffmt == 5) then
                            call value(out_args(3), cm%dat(ck%FI)%ffmt, ierr)
                            call value(out_args(4), cm%dat(ck%FI)%nblocks, ierr)
                        end if
                        do j = 3, nargs
                            if (len_trim(out_args(j)) > 3) then
                                if (out_args(j)(1:3) == 'hf=') then
                                    call value(out_args(j)(4:), cm%dat(ck%FI)%hf, ierr)
                                end if
                            end if
                            if (len_trim(out_args(j)) > 4) then
                                if (out_args(j)(1:4) == 'nts=') then
                                    call value(out_args(j)(5:), cm%dat(ck%FI)%nblocks, ierr)
                                end if
                            end if
                        end do
                    case ('BASINRAINFLAG')
                        call value(out_args(2), cm%dat(ck%RT)%ffmt, ierr)
                        if (ierr == 0) cm%dat(ck%RT)%factive = .true.
                        cm%dat(ck%RT)%id_var = 'RT'
                        if (cm%dat(ck%RT)%ffmt == 5) then
                            call value(out_args(3), cm%dat(ck%RT)%ffmt, ierr)
                            call value(out_args(4), cm%dat(ck%RT)%nblocks, ierr)
                        end if
                        do j = 3, nargs
                            if (len_trim(out_args(j)) > 3) then
                                if (out_args(j)(1:3) == 'hf=') then
                                    call value(out_args(j)(4:), cm%dat(ck%RT)%hf, ierr)
                                end if
                            end if
                            if (len_trim(out_args(j)) > 4) then
                                if (out_args(j)(1:4) == 'nts=') then
                                    call value(out_args(j)(5:), cm%dat(ck%RT)%nblocks, ierr)
                                end if
                            end if
                        end do
                    case ('BASINTEMPERATUREFLAG')
                        call value(out_args(2), cm%dat(ck%TT)%ffmt, ierr)
                        if (ierr == 0) cm%dat(ck%TT)%factive = .true.
                        cm%dat(ck%TT)%id_var = 'TT'
                        if (cm%dat(ck%TT)%ffmt == 5) then
                            call value(out_args(3), cm%dat(ck%TT)%ffmt, ierr)
                            call value(out_args(4), cm%dat(ck%TT)%nblocks, ierr)
                        end if
                        do j = 3, nargs
                            if (len_trim(out_args(j)) > 3) then
                                if (out_args(j)(1:3) == 'hf=') then
                                    call value(out_args(j)(4:), cm%dat(ck%TT)%hf, ierr)
                                end if
                            end if
                            if (len_trim(out_args(j)) > 4) then
                                if (out_args(j)(1:4) == 'nts=') then
                                    call value(out_args(j)(5:), cm%dat(ck%TT)%nblocks, ierr)
                                end if
                            end if
                        end do
                    case ('BASINWINDFLAG')
                        call value(out_args(2), cm%dat(ck%UV)%ffmt, ierr)
                        if (ierr == 0) cm%dat(ck%UV)%factive = .true.
                        cm%dat(ck%UV)%id_var = 'UV'
                        if (cm%dat(ck%UV)%ffmt == 5) then
                            call value(out_args(3), cm%dat(ck%UV)%ffmt, ierr)
                            call value(out_args(4), cm%dat(ck%UV)%nblocks, ierr)
                        end if
                        do j = 3, nargs
                            if (len_trim(out_args(j)) > 3) then
                                if (out_args(j)(1:3) == 'hf=') then
                                    call value(out_args(j)(4:), cm%dat(ck%UV)%hf, ierr)
                                end if
                            end if
                            if (len_trim(out_args(j)) > 4) then
                                if (out_args(j)(1:4) == 'nts=') then
                                    call value(out_args(j)(5:), cm%dat(ck%UV)%nblocks, ierr)
                                end if
                            end if
                        end do
                    case ('BASINPRESFLAG')
                        call value(out_args(2), cm%dat(ck%P0)%ffmt, ierr)
                        if (ierr == 0) cm%dat(ck%P0)%factive = .true.
                        cm%dat(ck%P0)%id_var = 'P0'
                        if (cm%dat(ck%P0)%ffmt == 5) then
                            call value(out_args(3), cm%dat(ck%P0)%ffmt, ierr)
                            call value(out_args(4), cm%dat(ck%P0)%nblocks, ierr)
                        end if
                        do j = 3, nargs
                            if (len_trim(out_args(j)) > 3) then
                                if (out_args(j)(1:3) == 'hf=') then
                                    call value(out_args(j)(4:), cm%dat(ck%P0)%hf, ierr)
                                end if
                            end if
                            if (len_trim(out_args(j)) > 4) then
                                if (out_args(j)(1:4) == 'nts=') then
                                    call value(out_args(j)(5:), cm%dat(ck%P0)%nblocks, ierr)
                                end if
                            end if
                        end do
                    case ('BASINHUMIDITYFLAG')
                        call value(out_args(2), cm%dat(ck%HU)%ffmt, ierr)
                        if (ierr == 0) cm%dat(ck%HU)%factive = .true.
                        cm%dat(ck%HU)%id_var = 'HU'
                        if (cm%dat(ck%HU)%ffmt == 5) then
                            call value(out_args(3), cm%dat(ck%HU)%ffmt, ierr)
                            call value(out_args(4), cm%dat(ck%HU)%nblocks, ierr)
                        end if
                        do j = 3, nargs
                            if (len_trim(out_args(j)) > 3) then
                                if (out_args(j)(1:3) == 'hf=') then
                                    call value(out_args(j)(4:), cm%dat(ck%HU)%hf, ierr)
                                end if
                            end if
                            if (len_trim(out_args(j)) > 4) then
                                if (out_args(j)(1:4) == 'nts=') then
                                    call value(out_args(j)(5:), cm%dat(ck%HU)%nblocks, ierr)
                                end if
                            end if
                        end do

                    case ('SHDFILEFLAG')
                        call value(out_args(2), SHDFILEFLAG, ierr)
                    case ('SOILINIFLAG')
                        call value(out_args(2), SOILINIFLAG, ierr)
                    case ('NRSOILAYEREADFLAG')
                        call value(out_args(2), NRSOILAYEREADFLAG, ierr)
                    case ('STREAMFLOWFLAG')
                        call value(out_args(2), j, ierr)
                        if (j == 1) then
                            WF_RTE_fstflout%freq = WF_RTE_fstflout%freq + radix(WF_RTE_fstflout%KTS)**WF_RTE_fstflout%KTS
                        end if
                    case ('PREEMPTIONFLAG')
                        call value(out_args(2), mtsflg%PREEMPTIONFLAG, ierr)

                    !> Interpolation flag for climate forcing data.
                    case ('INTERPOLATIONFLAG')
                        call value(out_args(2), IROVAL, ierr)
                        if (ierr == 0) then
                            do j = 1, cm%nclim
                                cm%dat(j)%ipflg = IROVAL
                            end do
                        end if

                    case ('SUBBASINFLAG')
                        call value(out_args(2), SUBBASINFLAG, ierr)
                    case ('R2COUTPUTFLAG')
                        call value(out_args(2), R2COUTPUTFLAG, ierr)
                    case ('OBJFNFLAG')
                        call value(out_args(2), OBJFNFLAG, ierr)
                    case ('AUTOCALIBRATIONFLAG')
                        call value(out_args(2), mtsflg%AUTOCALIBRATIONFLAG, ierr)
                    case ('WINDOWSIZEFLAG')
                        call value(out_args(2), WINDOWSIZEFLAG, ierr)
                    case ('WINDOWSPACINGFLAG')
                        call value(out_args(2), WINDOWSPACINGFLAG, ierr)
                    case ('METRICSSTATSOUTFLAG')
                        call value(out_args(2), METRICSSTATSOUTFLAG, ierr)
                    case ('METRICSFILTEROBSFLAG')
                        call value(out_args(2), METRICSFILTEROBSFLAG, ierr)
                    case ('METRICSSPINUP')
                        call value(out_args(2), METRICSSPINUP, ierr)
                    case ('METRICSINCLUDESPINUP')
                        call value(out_args(2), METRICSINCLUDESPINUP, ierr)
                    case ('FROZENSOILINFILFLAG')
                        call value(out_args(2), FROZENSOILINFILFLAG, ierr)
                    case ('PRINTRFFR2CFILEFLAG')
                        call value(out_args(2), SA_RTE_flgs%PRINTRFFR2CFILEFLAG, ierr)
                        SA_RTE_flgs%PROCESS_ACTIVE = .true.
                    case ('PRINTRCHR2CFILEFLAG')
                        call value(out_args(2), SA_RTE_flgs%PRINTRCHR2CFILEFLAG, ierr)
                        SA_RTE_flgs%PROCESS_ACTIVE = .true.
!+                    case ('PRINTLKGR2CFILEFLAG')
!+                        call value(out_args(2), SA_RTE_flgs%PRINTLKGR2CFILEFLAG, ierr)
!+                        SA_RTE_flgs%PROCESS_ACTIVE = .true.
                    case ('WD3')
                        call value(out_args(2), WD3, ierr)
                    case ('WD3NEWFILE')
                        call value(out_args(2), WD3NEWFILE, ierr)
                    case ('WD3FLOW')
                        call value(out_args(2), WD3FLOW, ierr)
                    case ('WD3BKFC')
                        call value(out_args(2), WD3BKFC, ierr)
                    case ('ICTEMMOD')
                        call value(out_args(2), ICTEMMOD, ierr)
                    case ('PBSMFLAG')
                        call value(out_args(2), PBSMFLAG, ierr)
                    case ('LOCATIONFLAG')
                        call value(out_args(2), LOCATIONFLAG, ierr)
                    case ('OUTFIELDSFLAG')
                        call value(out_args(2), OUTFIELDSFLAG, ierr)
                    case ('GGEOFLAG')
                        call value(out_args(2), GGEOFLAG, ierr)
                    case ('BASINBALANCEOUTFLAG')
                        call value(out_args(2), BASINAVGEBFILEFLAG, ierr)
                        BASINAVGWBFILEFLAG = BASINAVGEBFILEFLAG
                        BASINAVGEVPFILEFLAG = BASINAVGEBFILEFLAG
                    case ('BASINAVGEBFILEFLAG')
                        BASINAVGEBFILEFLAG = 0
                        do j = 2, nargs
                            select case (lowercase(out_args(j)))
                                case ('daily')
                                    BASINAVGEBFILEFLAG = 1
                                case ('all')
                                    BASINAVGEBFILEFLAG = 1
                                    exit
                                case ('default')
                                    BASINAVGEBFILEFLAG = 0
                                    exit
                                case ('none')
                                    BASINAVGEBFILEFLAG = 0
                                    exit
                            end select
                        end do

                    !> Time-averaged basin water balance output.
                    case ('BASINAVGWBFILEFLAG')
                        BASINAVGWBFILEFLAG = 0
                        do j = 2, nargs
                            select case (lowercase(out_args(j)))
                                case ('daily')
                                    BASINAVGWBFILEFLAG = BASINAVGWBFILEFLAG + 1
                                case ('monthly')
                                    BASINAVGWBFILEFLAG = BASINAVGWBFILEFLAG + 2
                                case ('hourly')
                                    BASINAVGWBFILEFLAG = BASINAVGWBFILEFLAG + 4
                                case ('ts')
                                    BASINAVGWBFILEFLAG = BASINAVGWBFILEFLAG + 8
                                case ('all')
                                    BASINAVGWBFILEFLAG = 1
                                    BASINAVGWBFILEFLAG = BASINAVGWBFILEFLAG + 2
                                    BASINAVGWBFILEFLAG = BASINAVGWBFILEFLAG + 4
                                    BASINAVGWBFILEFLAG = BASINAVGWBFILEFLAG + 8
                                    exit
                                case ('default')
                                    BASINAVGWBFILEFLAG = 1
                                    exit
                                case ('none')
                                    BASINAVGWBFILEFLAG = 0
                                    exit
                            end select
                        end do

                    !> Time-averaged basin PEVP-EVAP and EVPB output.
                    case ('BASINAVGEVPFILEFLAG')
                        BASINAVGEVPFILEFLAG = 0
                        do j = 2, nargs
                            select case (lowercase(out_args(j)))
                                case ('daily')
                                    BASINAVGEVPFILEFLAG = BASINAVGEVPFILEFLAG + 1
                                case ('monthly')
                                    BASINAVGEVPFILEFLAG = BASINAVGEVPFILEFLAG + 2
                                case ('hourly')
                                    BASINAVGEVPFILEFLAG = BASINAVGEVPFILEFLAG + 4
                                case ('ts')
                                    BASINAVGEVPFILEFLAG = BASINAVGEVPFILEFLAG + 8
                                case ('all')
                                    BASINAVGEVPFILEFLAG = 1
                                    BASINAVGEVPFILEFLAG = BASINAVGEVPFILEFLAG + 2
                                    BASINAVGEVPFILEFLAG = BASINAVGEVPFILEFLAG + 4
                                    BASINAVGEVPFILEFLAG = BASINAVGEVPFILEFLAG + 8
                                    exit
                                case ('default')
                                    BASINAVGEVPFILEFLAG = 1
                                    exit
                                case ('none')
                                    BASINAVGEVPFILEFLAG = 0
                                    exit
                            end select
                        end do

                    case ('MODELINFOOUTFLAG')
                        call value(out_args(2), MODELINFOOUTFLAG, ierr)

                    !> Streamflow output files.
                    case ('STREAMFLOWOUTFLAG')
                        WF_RTE_fstflout%freq = 0
                        do j = 2, nargs
                            select case (lowercase(out_args(j)))
                                case ('daily')
                                    WF_RTE_fstflout%freq = WF_RTE_fstflout%freq + radix(WF_RTE_fstflout%KDLY)**WF_RTE_fstflout%KDLY
                                case ('ts')
                                    WF_RTE_fstflout%freq = WF_RTE_fstflout%freq + radix(WF_RTE_fstflout%KTS)**WF_RTE_fstflout%KTS
                                case ('bal')
                                    WF_RTE_fstflout%fout_bal = .true.
                                case ('acc')
                                    WF_RTE_fstflout%fout_acc = .true.
                                case ('default')
                                    WF_RTE_fstflout%freq = radix(WF_RTE_fstflout%KDLY)**WF_RTE_fstflout%KDLY
                                    WF_RTE_fstflout%fout_hyd = .true.
                                    WF_RTE_fstflout%fout_bal = .false.
                                    WF_RTE_fstflout%fout_acc = .false.
                                    WF_RTE_fstflout%fout_header = .true.
                                    exit
                                case ('no_header')
                                    WF_RTE_fstflout%fout_header = .false.
                                case ('all')
                                    WF_RTE_fstflout%freq = radix(WF_RTE_fstflout%KDLY)**WF_RTE_fstflout%KDLY
                                    WF_RTE_fstflout%freq = WF_RTE_fstflout%freq + radix(WF_RTE_fstflout%KTS)**WF_RTE_fstflout%KTS
                                    WF_RTE_fstflout%fout_hyd = .true.
                                    WF_RTE_fstflout%fout_bal = .true.
                                    WF_RTE_fstflout%fout_acc = .true.
                                    exit
                                case ('none')
                                    WF_RTE_fstflout%freq = 0
                                    exit
                            end select
                        end do

                    !> Reservoir output files.
                    case ('REACHOUTFLAG')
                        WF_RTE_frsvrout%freq = 0
                        do j = 2, nargs
                            select case (lowercase(out_args(j)))
                                case ('ts')
                                    WF_RTE_frsvrout%freq = WF_RTE_frsvrout%freq + radix(WF_RTE_frsvrout%KTS)**WF_RTE_frsvrout%KTS
                                case ('default')
                                    WF_RTE_frsvrout%freq = 0
                                    WF_RTE_frsvrout%fout_header = .true.
                                    exit
                                case ('no_header')
                                    WF_RTE_frsvrout%fout_header = .false.
                                case ('all')
                                    WF_RTE_frsvrout%freq = radix(WF_RTE_frsvrout%KTS)**WF_RTE_frsvrout%KTS
                                    exit
                                case ('none')
                                    WF_RTE_frsvrout%freq = 0
                                    exit
                            end select
                        end do

                    case ('BASINSWEOUTFLAG')
                        call value(out_args(2), BASINSWEOUTFLAG, ierr)

                    !> BASEFLOW routing.
                    case ('BASEFLOWFLAG')
                        call value(out_args(2), lzsp%BASEFLOWFLAG, ierr)

                    !> Reservoir Release function flag (Number of WF_B coefficients).
                    case ('RESVRELSWFB')
                        call value(out_args(2), WF_RTE_flgs%RESVRELSWFB, ierr)

                    !> Cropland irrigation module.
                    case ('CROPLANDIRRIGATION')
                        cifg%ts_flag = 0
                        do j = 2, nargs
                            select case (lowercase(out_args(j)))
                                case ('daily')
                                    cifg%ts_flag = cifg%ts_flag + radix(civ%fk%KDLY)**civ%fk%KDLY
                                case ('hourly')
                                    cifg%ts_flag = cifg%ts_flag + radix(civ%fk%KHLY)**civ%fk%KHLY
                                case ('ts')
                                    cifg%ts_flag = cifg%ts_flag + radix(civ%fk%KTS)**civ%fk%KTS
                                case ('all')
                                    cifg%ts_flag = radix(civ%fk%KDLY)**civ%fk%KDLY
                                    cifg%ts_flag = cifg%ts_flag + radix(civ%fk%KHLY)**civ%fk%KHLY
                                    cifg%ts_flag = cifg%ts_flag + radix(civ%fk%KTS)**civ%fk%KTS
                                    exit
                                case ('default')
                                    cifg%ts_flag = radix(civ%fk%KDLY)**civ%fk%KDLY
                                    exit
                                case ('none')
                                    cifg%ts_flag = 0
                                    exit
                            end select
                        end do
                        cifg%PROCESS_ACTIVE = (cifg%ts_flag > 0)

                    !> Cropland irrigation module.
                    case ('RUNMODE')
                        do j = 2, nargs
                            select case (lowercase(out_args(j)))
                                case ('runsvs')
                                    RUNSVS113_flgs%PROCESS_ACTIVE = .true.
                                    RUNCLASS36_flgs%PROCESS_ACTIVE = .false.
                                case ('runclass')
                                    RUNCLASS36_flgs%PROCESS_ACTIVE = .true.
                                    RUNSVS113_flgs%PROCESS_ACTIVE = .false.
                                case ('default')
                                    RUNCLASS36_flgs%PROCESS_ACTIVE = .true.
                                    RUNSVS113_flgs%PROCESS_ACTIVE = .false.
                                    WF_RTE_flgs%PROCESS_ACTIVE = .true.
                                    exit
                                case ('diagnostic')
                                    RUNCLASS36_flgs%PROCESS_ACTIVE = .false.
                                    RUNSVS113_flgs%PROCESS_ACTIVE = .false.
                                    WF_RTE_flgs%PROCESS_ACTIVE = .false.
                                    exit
                            end select
                        end do
                        cifg%PROCESS_ACTIVE = (cifg%ts_flag > 0)

                    !> Unrecognized flag.
                    case default
                        print 9373, trim(out_args(1))
                        stop

                end select !case (trim(adjustl(out_args(1))))

                !>
                !> PARSE CONTROL FLAG ENDS.
                !>

                !> Error check.
                if (ierr /= 0) then
                    print 9484, trim(out_args(1)), i, trim(in_line)
                    stop
                end if

            end do !i = 1, CONFLAGS
        end if !(CONFLAGS > 0) then

9373    format(//1x, "WARNING: An unrecognized flag '", (a), &
               "' was found in the run options file.", &
               /1x, 'The flag may not be supported by this version of the model.', &
               /1x, 'Please ensure that the flag is properly named.', /)
9358    format(//1x, 'WARNING: An error occurred parsing Control Flag #', i5, &
               /1x, 'The flag or its arguments could not be parsed.', /)
9484    format(//1x, "WARNING: An error occurred parsing '", (a), "'.", &
               /1x, 'Flag #', I5, ': ', (a), /)

        do i = 1, 2
            read(iun, *)
        end do

        !> Output grid points.
        read(iun, '(i5)') WF_NUM_POINTS
        if (WF_NUM_POINTS > 10) then
            print *, 'WARNING: The number of grid output points is ', &
                'greater than ten. This may cause performance or ', &
                'stability issues.'
        end if
        read (iun, *)
        if (WF_NUM_POINTS > 0) then
            allocate(op%DIR_OUT(WF_NUM_POINTS), op%N_OUT(WF_NUM_POINTS), &
                     op%II_OUT(WF_NUM_POINTS), op%K_OUT(WF_NUM_POINTS), stat = ierr)
            if (ierr /= 0) then
                print *
                print *, 'Error allocating grid output point ', &
                    'variables.  Check that these bounds are within an ', &
                    'acceptable range.'
                print *, 'Bound 1 (grid output points): ', WF_NUM_POINTS
                stop
            end if
            read(iun, '(5i10)') (op%N_OUT(i), i = 1, WF_NUM_POINTS)
            read(iun, '(5i10)') (op%II_OUT(i), i = 1, WF_NUM_POINTS)
            read(iun, '(5a10)') (op%DIR_OUT(i), i = 1, WF_NUM_POINTS)
        else
            read(iun, *)
            read(iun, *)
            read(iun, *)
            allocate(op%DIR_OUT(1), op%N_OUT(1), op%II_OUT(1), op%K_OUT(1))
        end if !(WF_NUM_POINTS > 0)

        !> Check that output grid points aren't repeated and that the
        !> output directories exist.
!todo: fix this.
!-        do i = 1, WF_NUM_POINTS
!-            if (i < WF_NUM_POINTS) then
!-                do j = i + 1, WF_NUM_POINTS
!-                    if (op%N_OUT(i) == op%N_OUT(j) .and. op%II_OUT(i) == op%II_OUT(j)) then
!-                        print *
!-	                    print *, 'Output for Grid ', op%N_OUT(i), ' and GRU ', &
!-                            op%II_OUT(i), ' is repeated in grid output point: ', j
!-                        print *, 'Please adjust this grid output ', &
!-                            'point in MESH_input_run_options.ini.'
!-	                    stop
!-	                end if
!-                end do
!-            else
!-                open(17, file = './' // trim(adjustl(op%DIR_OUT(i))) // '/fort.17', status = 'unknown', iostat = ierr)
!-                if (ierr /= 0) then
!-                    print *
!-                    print *, 'Grid output point ', i
!-                    print *, 'The output directory does not exist: ' // trim(adjustl(op%DIR_OUT(i)))
!-                    print *, 'Please adjust this grid output point ', &
!-                        'in MESH_input_run_options.ini or create the ', &
!-                        'folder.'
!-                    stop
!-                else
!-                    close(17, status = 'delete')
!-                end if
!-            end if
!-        end do

	    !> Output folder for basin/high-level model output.
        read(iun, *)
        read(iun, *)
        read(iun, '(a10)') GENDIR_OUT
        call removesp(GENDIR_OUT)
        fls%GENDIR_OUT = adjustl(GENDIR_OUT)

        !> Simulation starting and stopping dates.
        read(iun, *)
        read(iun, *)
        read(iun, '(4i4)') ic%start%year, ic%start%jday, ic%start%hour, ic%start%mins
        read(iun, '(4i4)') ic%stop%year, ic%stop%jday, ic%stop%hour, ic%stop%mins

        call GET_DATES(ts)

        !> Close the file.
        close(iun)
        if (ro%VERBOSEMODE > 0) print *, 'READ: SUCCESSFUL, FILE: CLOSED'

        return

    end subroutine
