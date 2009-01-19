Public Class ParaMESH

    '****************************************************************************
    'ParaMESH - MESH PARAMETER CONFIGURATION UTILITY
    '****************************************************************************
    'ParaMESH is a utility used for creating and editing the configuration files 
    'that are used with WATFLOW 2.02, CLASS 3.3.1, and versions of MESH.
    '****************************************************************************

    '****************************************************************************
    'DEVELOPED BY, AUTHOR: D. Princz, May 2007
    'SUPERVISOR: Dr. R. Soulis, University of Waterloo (May 2007:September 2007)
    'SUPERVISOR: B. Davison, Environment Canada (Jan 2008:April 2008)
    '****************************************************************************

    '****************************************************************************
    'UPDATES: (DATE: MON DD YYYY, NAME, [RELEASE]; DESCRIPTION)
    '****************************************************************************
    'JUL 31 2007, D. Princz, V 1.0.0.0 (R0)
    '   -Compatible with WATFLOW 2.02, CLASS 3.3.1, and proposed MESH format; 
    '    uses six base file formats.
    '****************************************************************************
    'JUL 31 2007, D. Princz, V 1.0.0.1 (R1)
    '****************************************************************************
    'AUG 02 2007, D. Princz, V 1.0.0.2 (R2)
    '   -Fixed issues with GRU handling.
    '****************************************************************************
    'AUG 02 2007, D. Princz, V 1.0.0.3 (R3)
    '   -Introduced System Configuration form; houses the ability to edit and 
    '    update system's own configuration files, and help file (self-
    '    sustainability).
    '****************************************************************************
    'AUG 02 2007, D. Princz, V 1.0.0.4 (R4)
    '   -"Pre-Release"; released for review with Environment Canada.
    '   -Introduced ParaMESH Field-Guide; identifies relevant fields for the 
    '    selected environment (WATFLOW 2.02, CLASS 3.3.1).
    '   -Updated ParaMESH Help File handling; can now use semi-colons.
    '****************************************************************************
    'NOV 07 2007, D. Princz, V 1.1.0.0 (V1) **UNRELEASED!
    '   -Compatible with WATFLOW 2.02, MESH 1.0.cXX Series; system variables 
    '    redefined, system reserves updated, and the utility's architecture 
    '    updated to reflect the updated MESH file format.
    '   -Introduced ParaMESH Field-Check; validates user-defined values for all 
    '    "ENABLED" variables.
    '   -Introduced Administrative "Lock-Out"; the utility's configuration files 
    '    are password protected.
    '****************************************************************************
    'JAN 31 2008, D. Princz, V 1.1.0.0 (ParaMESH_1.1) **RELEASED!
    '   -Compatible with MESH 1.0.cXX Series; system interface updated, primary 
    '    and secondary parameters introduced, re-coded to support the updated 
    '    MESH file format.
    '   -ParaMESH Field-Check updated; called ParaMESH Parameter Check 
    '    (ParaCHECK).  ParaCHECK updated to check values as they are changed.
    '   -Mass value saves are removed, values are saved individually.
    '   -"Read" is renamed ParaREAD, "Write" is renamed ParaWRITE, "Check" is 
    '    renamed ParaCHECK.
    '   -ParaLOG, to log processes, and ParaSTOR, to handle backup values, are 
    '    added.
    '   -Central ProgressForm is removed, processes are tracked and saved in 
    '    internal arrays using ParaLOG procedures.
    '   -All external ParaMESH configuration files (ParaMESH.var, ParaMESH.ask, 
    '    ParaMESH.src, ParaMESH.log, ParaMESH.res) are removed; ParaMESH.var and  
    '    ParaMESH.res are amalgamated into ParaMESH.ini.  ParaMESH.ask is 
    '    replaced with ParaMESH.htm (interface is updated to browse ParaMESH.htm 
    '    file).  ParaMESH.src restore file is replaced with ParaMESH.bak.
    '   -Sample files are included with distribution, as is the ability to call 
    '    an external program; ParaMESH.SystemParameters(RunMESH) introduced.
    '   -ParaSTOR, ErrorForm is introduced for better error handling, including 
    '    system properties: AdminContact, ErrorMessage, SystemRelease, 
    '    SystemHierarchy; system parameters: ParaLOG, StopLOG, ParaSTOR, StartUp.
    '   -All dependables (hard-coded variables) are removed wherever possible 
    '    (most properties become controlled by properties in the ParaMESH.ini 
    '    file.
    '   -ParaMESH administrative tools are expanded to accomodate secondary 
    '    parameters, system environment control, controlling system parameters.
    '   -Ability to edit ParaMESH Help File internally is removed; the ParaMESH 
    '    Help File is converted to ParaMESH.htm to allow for easier editing.
    '   -WatflowForm is introduced.  All forms except for GRUForm and 
    '    ProcessForm are changed.
    '----------------------------------------------------------------------------
    '   -ParaMESH Installer is introduced.  Sample files and ParaMESH Help File 
    '    are included as external files in new distribution package.
    '****************************************************************************
    'MAR 31 2008, D. Princz, V 1.1.1.0 (ParaMESH_1.1.1)
    '   -ParaMESH Help File updated.
    '   -ParaLOAD, ParaSAVE are added; loading procedures are removed from most 
    '    forms and saving procedures are grouped together.
    '   -Most processes are removed from ParaMESH.vb, definitions, standards, 
    '    and very common shared subroutines remain.  All others are placed in 
    '    the Para*.vb series of files.
    '   -Pre-loading default values such that they are loaded if configuration 
    '    files do not exist in working directory.
    '   -Support for WATFLOW 2.02 is fixed (now operational, though inactive).
    '   -Compatible with MESH 1.0.cXX Series.
    '   -ParaMESH administrative utilities fully functional.
    '----------------------------------------------------------------------------
    '   -ParaMESH Installer is updated.  MESH 1.0.c02, sample files and ParaMESH 
    '    Help File are included as external files in new distribution package.
    '****************************************************************************
    'APR 01 2008, D. Princz, V 1.1.2.0 (ParaMESH_1.1.2)
    '   -Issues handling multiple GRUs is fixed.
    '   -Issues writing class.ini for WATFLOW environment fixed.
    '   -Supports disabling all grid output points.
    '----------------------------------------------------------------------------
    '   -ParaMESH Installer is updated.  MESH 1.0.c02, sample files and ParaMESH 
    '    Help File are included as external files in new distribution package.
    '****************************************************************************
    'APR 29 2008, D. Princz, V 1.1.3.0 (ParaMESH_1.1.3)
    '   -ParaREAD, ParaWRITE, ParaLOG, ParaLOAD, ParaSAVE, ParaSTOR, ParaCHECK, 
    '    and ParaMESH are updated to use common ParaMESH subroutines.
    '   -Error with reversed IHOUR and IMIN values fixed in class.ini and 
    '    ProcessForm
    '   -All code is updated for archiving.
    '----------------------------------------------------------------------------
    '   -ParaMESH Installer is updated.  MESH 1.0.c05, sample files and ParaMESH 
    '    Help File are included as external files in new distribution package.
    '****************************************************************************

    '****************************************************************************
    'ASSOCIATED FILES
    '****************************************************************************
    'ParaMESH.ini:      ParaMESH Configuration File
    'ParaMESH.htm:      ParaMESH Help File (may include other files, such as 
    '                   *.jpgs, which are included in the help file)
    '                   (May convert to *.mht single-file web page format)
    'ParaMESH.bak:      ParaMESH Restore File (may include class.txt, 
    '                   hydrology.txt, or flags.txt, which are written to 
    '                   application's directory if program crashes while 
    '                   configuration files are loaded)
    'ParaMESH_XXX.txt:  ParaMESH Log File (written by request)
    'ParaSTOR_XXX.txt:   ParaMESH Error File (written by request)
    '----------------------------------------------------------------------------
    'These files have been removed from the ParaMESH system (pre-ParaMESH_1.1):
    'ParaMESH.ask:  Old-Format ParaMESH Help File (reformed as ParaMESH.htm)
    'ParaMESH.log:  Old-Format ParaMESH Log File (renamed ParaMESH.txt)
    'ParaMESH.res:  Old-Format ParaMESH Configuration File (renamed ParaMESH.ini)
    'ParaMESH.src:  Old-Format ParaMESH Restore File (reformed as ParaMESH.bak)
    'ParaMESH.var:  Old-Format ParaMESH Parameter File (merged into ParaMESH.ini)
    '****************************************************************************

    '****************************************************************************
    'EXTERNAL ParaMESH PARAMETERS
    '****************************************************************************
    'Parameters used to store values that are read from and written to the 
    'configuration files.
    '****************************************************************************
    'First Dimension:   parameter store (0=active value, 1=backup value)
    'Second Dimension:  normally ColumnIndex (or Index if only 2D parameter, as 
    '                   is the case with xFlag parameters)
    'Third Dimension:   Index (normally the parameter set, as is the case with 
    '                   GridOutput and DependentGRU, or is the GRU)
    '****************************************************************************

    '***RUN_OPTIONS.INI CONFIGURATION FILE
    Public Shared ControlFlag(1, 0) As String
    Public Shared GridOutput(1, 2, 0) As String

    '***HYDROLOGY.INI (MESH FORMAT) CONFIGURATION FILE
    Public Shared OptionFlag(1, 0) As String
    Public Shared WF_R2(1, 4) As Double
    Public Shared IndependentGRU(1, 0) As String
    Public Shared DependentGRU(1, 0, 0) As String

    '***HYDROLOGY.INI (CLASS FORMAT) CONFIGURATION FILE
    Public Shared ClassFlag(1, 0) As String

    '***WATFLOW.INI CONFIGURATION FILE
    Public Shared BasinID(1) As String
    Public Shared ScalingFactor(1) As Double
    Public Shared GWINIT(1) As Double
    Public Shared ZPLIMG0(1) As Double
    Public Shared ZPLIMS0(1) As Double
    Public Shared ZPLIMP0(1) As Double
    Public Shared ZPLIMPS0(1) As Double
    Public Shared D100A(1, 5) As Double
    Public Shared WatflowFlag(1, 12) As String

    '***CLASS.INI CONFIGURATION FILE (PROJECT INFORMATION)
    Public Shared TITLE(1) As String
    Public Shared NAME(1) As String
    Public Shared PLACE(1) As String

    '***CLASS.INI CONFIGURATION FILE (SITE INFORMATION)
    Public Shared DEGLAT(1) As Double
    Public Shared DEGLON(1) As Double
    Public Shared ZRFM(1) As Double
    Public Shared ZRFH(1) As Double
    Public Shared ZBLD(1) As Double
    Public Shared GC(1) As Double
    Public Shared ILW(1) As Double
    Public Shared GRID(1) As Integer
    'FEB 04 2008 (DAN): Public Shared GRU(1, 1, 0) As String
    Public Shared GRU(1, 0) As String

    '***CLASS.INI CONFIGURATION FILE (FIRST BLOCK)
    Public Shared FCAN(1, 4, 0) As Double
    Public Shared LNZ0(1, 4, 0) As Double
    Public Shared ALVC(1, 4, 0) As Double
    Public Shared ALIC(1, 4, 0) As Double
    Public Shared LAMX(1, 3, 0) As Double
    Public Shared LAMN(1, 3, 0) As Double
    Public Shared CMAS(1, 3, 0) As Double
    Public Shared ROOT(1, 3, 0) As Double
    Public Shared RSMN(1, 3, 0) As Double
    Public Shared QA50(1, 3, 0) As Double
    Public Shared VPDA(1, 3, 0) As Double
    Public Shared VPDB(1, 3, 0) As Double
    Public Shared PSGA(1, 3, 0) As Double
    Public Shared PSGB(1, 3, 0) As Double
    Public Shared DRN(1, 0) As Double
    Public Shared SDEP(1, 0) As Double
    Public Shared FARE(1, 0) As Double
    Public Shared DDEN(1, 0) As Double
    Public Shared XSLP(1, 0) As Double
    Public Shared GRKF(1, 0) As Double
    Public Shared WFSF(1, 0) As Double
    Public Shared WFCI(1, 0) As Double
    Public Shared RSERVC01(1, 0) As Double
    Public Shared RSERVC02(1, 0) As Double
    Public Shared RSERVC03(1, 0) As Double
    Public Shared RSERVC04(1, 0) As Double
    Public Shared RSERVC05(1, 0) As Double
    Public Shared RSERVC06(1, 0) As Double
    Public Shared RSERVC07(1, 0) As Double
    Public Shared RSERVC08(1, 0) As Double
    Public Shared RSERVC09(1, 0) As Double
    Public Shared RSERVC10(1, 0) As Double
    Public Shared RSERVC11(1, 0) As Double
    Public Shared RSERVC12(1, 0) As Double

    '***CLASS.INI CONFIGURATION FILE (SECOND BLOCK)
    Public Shared SAND(1, 2, 0) As Double
    Public Shared CLAY(1, 2, 0) As Double
    Public Shared ORGM(1, 2, 0) As Double
    Public Shared TBAR(1, 2, 0) As Double
    Public Shared TCAN(1, 0) As Double
    Public Shared TSNO(1, 0) As Double
    Public Shared TPND(1, 0) As Double
    Public Shared THLQ(1, 2, 0) As Double
    Public Shared THIC(1, 2, 0) As Double
    Public Shared ZPND(1, 0) As Double
    Public Shared RCAN(1, 0) As Double
    Public Shared SCAN(1, 0) As Double
    Public Shared SNO(1, 0) As Double
    Public Shared ALBS(1, 0) As Double
    Public Shared RHOS(1, 0) As Double
    Public Shared GRO(1, 0) As Double
    Public Shared RSERVC13(1, 0) As Double
    Public Shared RSERVC14(1, 0) As Double
    Public Shared RSERVC15(1, 0) As Double
    Public Shared RSERVC16(1, 0) As Double
    Public Shared RSERVC17(1, 0) As Double
    Public Shared RSERVC18(1, 0) As Double
    Public Shared RSERVC19(1, 0) As Double
    Public Shared RSERVC20(1, 0) As Double
    Public Shared RSERVC21(1, 0) As Double
    Public Shared RSERVC22(1, 0) As Double
    Public Shared RSERVC23(1, 0) As Double
    Public Shared RSERVC24(1, 0) As Double
    Public Shared RSERVC25(1, 0) As Double
    Public Shared RSERVC26(1, 0) As Double

    '***CLASS.INI CONFIGURATION FILE (RUN TIMES)
    Public Shared HourlyStartDay(1) As Integer
    Public Shared HourlyStartYear(1) As Integer
    Public Shared HourlyStopDay(1) As Integer
    Public Shared HourlyStopYear(1) As Integer
    Public Shared DailyStartDay(1) As Integer
    Public Shared DailyStartYear(1) As Integer
    Public Shared DailyStopDay(1) As Integer
    Public Shared DailyStopYear(1) As Integer
    Public Shared SimStartDay(1) As Integer
    Public Shared SimStartYear(1) As Integer
    Public Shared SimStopDay(1) As Integer
    Public Shared SimStopYear(1) As Integer
    Public Shared MetStartMin(1) As Integer
    Public Shared MetStartHour(1) As Integer
    Public Shared MetStartDay(1) As Integer
    Public Shared MetStartYear(1) As Integer

    '****************************************************************************
    'INTERNAL FIXED ParaMESH PARAMETERS
    '****************************************************************************
    'Parameters used to store fixed ParaMESH values or configuration information.
    '****************************************************************************
    'SystemProperties(1, 0): System Properties
    '   0th Dimension:  property store (0=system property, 1=value)
    '   1st Dimension:  increases for each system property
    'FileProperties(5, 0): Environment File Definitions
    '   0th Dimension:  property store (0=environment call, 1=environment 
    '                   version, 2=subroutine used, 3=file name, 4=file header, 
    '                   5=accept other headers)
    '   1st Dimension:  increases for each file definition
    'PrimaryParameters(9, 0): Primary Parameter Definitions
    '   0th Dimension:  property store (0=parameter call, 1=has children, 
    '                   2=enabled, 3=printed description, 4=parameter 
    '                   description, 5=default value, 6=maximum field spacing, 
    '                   7=range, 8=range type, 9=parameter type)
    '   1st Dimension:  increases for each primary parameter definition
    'SecondaryParameters(10, 0): Secondary Parameters Definitions
    '   0th Dimension:  property store (0=parent parameter, 1=parameter call, 
    '                   2=enabled, 3=printed description, 4=parameter 
    '                   description, 5=default value, 6=maximum field spacing, 
    '                   7=range, 8=range type, 9=parameter type, 10=row index)
    '   1st Dimension:  increases for each secondary parameter definition
    'SystemParameters(1, 0): System Flags
    '   0th Dimension:  property store (0=system flag, 1=enabled)
    '   1st Dimension:  increases for each system flag
    'SpecialChecks(1, 0): Special ParaMESH Parameter Checks (ParaCHECK)
    '   0th Dimension:  property store (0=special check, 1=enabled)
    '   1st Dimension:  increases for each special check
    'ErrorDefinitions(1, 0): Error Message Definitions (not used)
    '   0th Dimension:  property store (0=message index, 1=error description)
    '   1st Dimension:  increases for each error definition
    'ParaCross(, ): Cross-Environment Shared Parameter Definitions (not used)
    '   0th Dimension:  property store
    '   1st Dimension:  increases for each shared parameter definition
    '----------------------------------------------------------------------------
    'These parameters have been removed from the ParaMESH system 
    '(pre-ParaMESH_1.1):
    'HelpFile(1, 0): Old-Format ParaMESH Help File
    '   0th Dimension:  property store (0=formatting flag ($,#), 1=content)
    '   1st Dimension:  increases for each help file line
    'VariableData(6, 0): Old-Format Parameter Definitions
    '   0th Dimension:  property store (0=parameter call, 1=parameter name, 
    '                   2=parameter description, 3=default value, 4=range, 
    '                   5=range type, 6=print description)
    '   1st Dimension:  increases for each parameter definition
    'SystemFile(1, 0): Old-Format ParaMESH Configuration File
    '   0th Dimension:  property store (0=system property, 1=value)
    '   1st Dimension:  increases for each system property
    'SourceFile(1, 0): Old-Format Backup Value Store
    '   0th Dimension:  property store (0=parameter call, 1=value)
    '   1st Dimension:  increases for each backup value stored
    '****************************************************************************

    Public Shared SystemProperties(1, 0) As String
    Public Shared FileProperties(7, 0) As String
    Public Shared PrimaryParameters(9, 0) As String
    Public Shared SecondaryParameters(10, 0) As String
    Public Shared SystemParameters(1, 0) As String
    Public Shared SpecialChecks(1, 0) As String
    Public Shared ErrorDefinitions(1, 0) As String
    Public Shared CrossFields(0, 0) As String

    '****************************************************************************
    'SHARED ParaMESH PARAMETERS
    '****************************************************************************
    'Parameters shared throughout the ParaMESH, which are redimensioned or 
    'modified as needed by the calling subroutine.
    '****************************************************************************
    'CheckList(6, 0): ParaCHECK List of Erroneous Values
    '   0th Dimension:  property store (0=parameter description, 1=error 
    '                   message, 2=existing value, 3=default or suggested value, 
    '                   4=parameter call, 5=value index, 6=value column index)
    '   1st Dimension:  increases for each erroneous value added to the list
    'ParameterList(5, 0): ParaSTOR Parameter List
    '   0th Dimension:  property store (0=parameter description, 1=existing 
    '                   value, 2=default or backup value, 3=parameter call, 
    '                   4=value index, 5=value column index)
    '   1st Dimension:  increases for each parameter added to the list
    'LogFile(0): ParaMESH Process Log
    '   0th Dimension:  process store; may increase for each process if 
    '                   ..SystemParameters(ParaLOG) is enabled
    'IgnoreCheck(1, 0): ParaCHECK Parameter Ignore List
    '   0th Dimension:  property store (0=parameter call, 1=description)
    '   1st Dimension:  increases for each parameter added to the list
    'DirectoryList(0): List of Previously Loaded Directories
    '   0th Dimension:  full directory path store; may increase for each 
    '                   directory added to the list
    'PathRoot: Directory where the Configuration Files will be Saved
    '****************************************************************************

    Public Shared CheckList(6, 0) As String
    Public Shared ParameterList(5, 0) As String
    Public Shared LogFile(0) As String
    Public Shared IgnoreCheck(1, 0) As String
    Public Shared DirectoryList(0) As String
    Public Shared PathRoot As String

    '****************************************************************************
    'PRIVATE ParaMESH PARAMETERS
    '****************************************************************************
    'Parameters shared and used only within the ParaMESH class.
    '****************************************************************************
    'Message Box Results: used to call user-directed prompt messages.
    'Private Integers: n, m are unique to the ParaMESH class so that similarly 
    'private integers from calling subroutines are not reset.
    '****************************************************************************

    Private Shared LoadRestore As New MsgBoxResult, DeleteRestore As New MsgBoxResult, LoadSamples As New MsgBoxResult, RemoveParameter As New MsgBoxResult
    Private Shared ParameterArray(,) As String, SplitArray() As String, TempArray() As String, n As Integer, m As Integer, p As Integer

    '*****************************************************************************
    'SUBROUTINE: ParaMESH.Arguments
    '*****************************************************************************
    'ParaMESH subroutines may be called by an external thread, which will only 
    'accept a single argument.  This subroutine is used to compile all arguments 
    'required by read subroutines into a single object array.
    'CallProcess denotes which read subroutine will be called.
    '*****************************************************************************

    Public Shared Function Arguments(ByVal CallProcess As String) As Object
        ReDim TempArray(0)

        '***POPULATE TempArray WITH ARGUMENTS FOR ProcessForm(Ext_ParaMESH)
        TempArray(0) = CallProcess

        '***RETURN TempArray
        Arguments = TempArray
    End Function

    '****************************************************************************
    'SUBROUTINE: CheckConfiguration                 'REMOVED:   NOV 07 2007 (DAN)
    '****************************************************************************
    'UPDATED: FEB 28 2008 (DAN) these methods have been integrated as core 
    '                           components of the methods used to read and write 
    '                           configuration files (external processes of 
    '                           ProcessForm)
    '****************************************************************************
    'Sets the system configuration, based on the configuration files that exist 
    'in the working directory by setting "WATFLOW", "CLASS", or "MESH" to 
    '"FORCE", if the files for each environment existm and if the parameters are 
    'not already set to "STOP".  Sets "MESH" to "FORCE" if no files exist, so 
    'that configuration files for MESH are written by default.
    '****************************************************************************

    '****************************************************************************
    'SUBROUTINE: CallOperation
    '****************************************************************************
    'UPDATED: APR 22 2008 (DAN) to use common ParaMESH subroutines
    '****************************************************************************
    'Used to call external ProcessForm subroutines to start large processses on 
    'a separate thread (reading or writing configuration files, checking 
    'parameter values, loading parameters values, etc.)
    '****************************************************************************

    Public Shared Sub CallOperation(ByVal Operation As String, ByVal ParentForm As System.Windows.Forms.IWin32Window, Optional ByVal Arguments() As Object = Nothing)

        '***A PROCESS IS ALREADY RUNNING IF ProgressForm IS VISIBLE
        If ProgressForm.Visible = True Then

            '***DO NOT CALL ANOTHER PROCESS THAT WILL CALL ProgressForm
            MsgBox("Please wait for the pending operation to complete.", MsgBoxStyle.OkOnly, "ParaMESH")
            Exit Sub
        End If

        '***CALL PROCESS
        Select Case Operation
            Case Is = "ParaMESH"

                '***CALL ProcessForm(Ext_ParaMESH) TO PERFORM OPERATION
                With ProgressForm
                    Select Case Arguments(0)
                        Case Is = "CreateGRU"
                            .Box_Process.Text = "Please wait while ParaMESH creates the GRU..."
                        Case Is = "RemoveGRU"
                            .Box_Process.Text = "Please wait while ParaMESH removes the selected GRU..."
                        Case Is = "LoadParameters"
                            .Box_Process.Text = "Please wait while ParaMESH loads the parameter definitions..."
                    End Select
                    .Show(ParentForm)
                End With
                With ProcessForm
                    If .Ext_ParaMESH.IsBusy = True Then
                        ParaLOG.AppendFile("ParaMESH is already performing a task on the ProcessForm(Ext_ParaMESH) thread", "ParaMESH.CallOperation")
                        While .Ext_ParaMESH.IsBusy = True
                            Application.DoEvents()
                        End While
                    End If
                    ParaLOG.AppendFile("ParaMESH has called the ProcessForm(Ext_ParaMESH) thread", "ParaMESH.CallOperation")
                    .Ext_ParaMESH.RunWorkerAsync(Arguments)
                    While .Ext_ParaMESH.IsBusy = True
                        Application.DoEvents()
                    End While
                End With
            Case Is = "ParaCHECK"

                '***CALL ProcessForm(Ext_ParaCHECK) TO VERIFY PARAMETER VALUES
                With ProgressForm
                    .Box_Process.Text = "Please wait while ParaCHECK verifies the parameter values..."
                    .Show(ParentForm)
                End With
                With ProcessForm
                    If .Ext_ParaCHECK.IsBusy = True Then
                        ParaLOG.AppendFile("ParaMESH is already performing a task on the ProcessForm(Ext_ParaCHECK) thread", "ParaMESH.CallOperation")
                        While .Ext_ParaCHECK.IsBusy = True
                            Application.DoEvents()
                        End While
                    End If
                    ParaLOG.AppendFile("ParaMESH has called the ProcessForm(Ext_ParaCHECK) thread", "ParaMESH.CallOperation")
                    .Ext_ParaCHECK.RunWorkerAsync(Arguments)
                    While .Ext_ParaCHECK.IsBusy = True
                        Application.DoEvents()
                    End While
                End With
            Case Is = "ParaSTOR"

                '***CALL ProcessForm(Ext_ParaSTOR) TO WRITE ParaMESH.bak RESTORATION FILE
                With ProgressForm
                    Select Case Arguments(0)
                        Case Is = "CallBackup"
                            .Box_Process.Text = "Please wait while ParaSTOR restores the parameter values..."
                        Case Is = "CallDefault"
                            .Box_Process.Text = "Please wait while ParaSTOR loads the default parameter values..."
                        Case Is = "SetDefault"
                            .Box_Process.Text = "Please wait while ParaSTOR sets the default parameter values..."
                    End Select
                    .Show(ParentForm)
                End With
                With ProcessForm
                    If .Ext_ParaSTOR.IsBusy = True Then
                        ParaLOG.AppendFile("ParaMESH is already performing a task on the ProcessForm(Ext_ParaSTOR) thread", "ParaMESH.CallOperation")
                        While .Ext_ParaSTOR.IsBusy = True
                            Application.DoEvents()
                        End While
                    End If
                    ParaLOG.AppendFile("ParaMESH has called the ProcessForm(Ext_ParaSTOR) thread", "ParaMESH.CallOperation")
                    .Ext_ParaSTOR.RunWorkerAsync(Arguments)
                    While .Ext_ParaSTOR.IsBusy = True
                        Application.DoEvents()
                    End While
                End With
            Case Is = "ParaREAD"

                '***CALL ProcessForm(Ext_LoadFiles) TO LOAD CONFIGURATION FILES
                With ProgressForm
                    Select Case Arguments(0)
                        Case Is = "ReadFiles"
                            .Box_Process.Text = "Please wait while ParaMESH loads the configuration files..."
                        Case Is = "SampleFiles"
                            .Box_Process.Text = "Please wait while ParaMESH creates the sample files..."
                    End Select
                    .Show(ParentForm)
                End With
                With ProcessForm
                    If .Ext_ParaREAD.IsBusy = True Then
                        ParaLOG.AppendFile("ParaMESH is already performing a task on the ProcessForm(Ext_ParaREAD) thread", "ParaMESH.CallOperation")
                        While .Ext_ParaREAD.IsBusy = True
                            Application.DoEvents()
                        End While
                    End If
                    ParaLOG.AppendFile("ParaMESH has called the ProcessForm(Ext_ParaREAD) thread", "ParaMESH.CallOperation")
                    .Ext_ParaREAD.RunWorkerAsync(Arguments)
                    While .Ext_ParaREAD.IsBusy = True
                        Application.DoEvents()
                    End While
                End With
            Case Is = "ParaWRITE"

                '***CALL ProcessForm(Ext_SaveFiles) TO SAVE CONFIGURATION FILES
                With ProgressForm
                    Select Case Arguments(0)
                        Case Is = "SaveFiles"
                            .Box_Process.Text = "Please wait while ParaMESH saves the configuration files..."
                        Case Is = "ErrorReport"
                            .Box_Process.Text = "Please wait while ParaMESH restores the configuration files..."
                    End Select
                    .Show(ParentForm)
                End With
                With ProcessForm
                    If .Ext_ParaWRITE.IsBusy = True Then
                        ParaLOG.AppendFile("ParaMESH is already performing a task on the ProcessForm(Ext_ParaWRITE) thread", "ParaMESH.CallOperation")
                        While .Ext_ParaWRITE.IsBusy = True
                            Application.DoEvents()
                        End While
                    End If
                    ParaLOG.AppendFile("ParaMESH has called the ProcessForm(Ext_ParaWRITE) thread", "ParaMESH.CallOperation")
                    .Ext_ParaWRITE.RunWorkerAsync(Arguments)
                    While .Ext_ParaWRITE.IsBusy = True
                        Application.DoEvents()
                    End While
                End With
            Case Is = "RunMESH"

                '***DO NOTHING IF ParaMESH.SystemParameters(RunExecutable) IS NOT ENABLED
                If ParaMESH.IsEnabled("RunExecutable", ParaMESH.SystemParameters, 1) = False Then
                    Exit Sub
                End If

                '***DO NOTHING IF ParaMESH(PathRoot) DOES NOT EXIST
                If ParaMESH.PathRoot = Nothing Then
                    Exit Sub
                End If

                '***DO NOTHING IF ParaMESH.SystemProperties(CallExecutable) DOES NOT EXIST
                If System.IO.File.Exists(ParaMESH.PathRoot & "\" & ParaMESH.SystemProperties(1, ParaMESH.FindIndex("CallExecutable", ParaMESH.SystemProperties))) = False Then
                    Exit Sub
                End If

                '***SET ProcessForm(Ext_RunMESH) INFORMATION TO RUN ParaMESH.SystemProperties(CallExecutable)
                '***SPLIT ..SystemProperties(CallExecutable) INTO FILE AND DIRECTORY, IF NECESSARY
                With ProcessForm.Ext_RunMESH.StartInfo
                    If ParaMESH.SystemProperties(1, ParaMESH.FindIndex("CallExecutable", ParaMESH.SystemProperties)).LastIndexOf("\") = -1 Then
                        .FileName = ParaLOAD.SystemProperties("CallExecutable")
                        .WorkingDirectory = ParaMESH.PathRoot
                    Else
                        .FileName = ParaLOAD.SystemProperties("CallExecutable").Substring(ParaLOAD.SystemProperties("CallExecutable").LastIndexOf("\") + 1)
                        .WorkingDirectory = ParaMESH.PathRoot & "\" & ParaLOAD.SystemProperties("CallExecutable").Substring(0, ParaLOAD.SystemProperties("CallExecutable").LastIndexOf("\"))
                    End If
                End With

                '***CALL ProcessForm(Ext_RunMESH) TO RUN MESH EXECUTABLE
                With ProgressForm
                    .Box_Process.Text = "Please wait while ParaMESH performs the requested operation..."
                    .Show(ParentForm)
                End With
                With ProcessForm
                    ParaLOG.AppendFile("ParaMESH has called the ProcessForm(Ext_RunMESH) thread", "ParaMESH.CallOperation")
                    .Ext_RunMESH.Start()
                    Do While .Ext_RunMESH.HasExited = False
                        Application.DoEvents()
                    Loop
                End With
        End Select
        ProgressForm.Close()
    End Sub

    '****************************************************************************
    'SUBROUTINE: StartUp
    '****************************************************************************
    'UPDATED: APR 22 2008 (DAN) to use common ParaMESH subroutines
    '****************************************************************************
    'These procedures are called when ParaMESH is started.
    '****************************************************************************

    Public Shared Sub StartUp()

        '***CALL ParaREAD(Configuration) TO READ THE ParaMESH CONFIGURATION FILE
        ParaREAD.Configuration()

        '***CHECK FOR THE ParaMESH RESTORE FILE IN APPLICATION DIRECTORY
        If System.IO.File.Exists(Application.StartupPath & "\ParaMESH.bak") Then
            LoadRestore = MsgBox("A ParaMESH restore file exists.  Do you wish ParaMESH to load the file?", MsgBoxStyle.YesNo, "ParaMESH")
            If LoadRestore = MsgBoxResult.Yes Then

                '***CALL ParaREAD(BakFile) TO LOAD THE ParaMESH RESTORE FILE
                ParaREAD.BAK()
            Else
                DeleteRestore = MsgBox("Do you wish to save the ParaMESH restore file to an alternate directory, before ParaMESH deletes the file?", MsgBoxStyle.YesNoCancel, "ParaMESH")
                If DeleteRestore = MsgBoxResult.Yes Then

                    '***CALL ProcessForm(Ext_BrowseDirectory) TO SELECT A DIRECTORY
                    With ProcessForm.Ext_BrowseDirectory
                        .Description = "Browse to the directory where ParaMESH will save the ParaMESH restore file."
                        .RootFolder = Environment.SpecialFolder.Desktop
                        .ShowDialog()
                    End With
                    n = 1
                    Do
                        If System.IO.File.Exists(ProcessForm.Ext_BrowseDirectory.SelectedPath & "\ParaMESH_" & (n).ToString("000") & ".bak") = False Then

                            '***MOVE ParaMESH.bak RESTORATION FILE TO SELECTED DIRECTORY
                            System.IO.File.Move(Application.StartupPath & "\ParaMESH.bak", ProcessForm.Ext_BrowseDirectory.SelectedPath & "\ParaMESH_" & (n).ToString("000") & ".bak")
                            Exit Do
                        End If
                        n += 1
                    Loop
                ElseIf DeleteRestore = MsgBoxResult.No Then

                    '***DELETE ParaMESH.bak RESTORATION FILE FROM APPLICATION DIRECTORY
                    System.IO.File.Delete(Application.StartupPath & "\ParaMESH.bak")
                Else

                    '***Disable ParaMESH.SystemParameters(StartUp) TO PREVENT THIS INSTANCE OF ParaMESH FROM RE-LOADING
                    ParaSAVE.SystemParameters("Disabled", "StartUp")

                    '***RESTART ParaMESH
                    Application.Restart()
                    Exit Sub
                End If
            End If
        End If

        '***CHECK WIDTH OF PRIMARY SYSTEM SCREEN RESOLUTION TO SEE IF HelpForm WILL DISPLAY CORRECTLY
        ParaLOG.AppendFile("ParaMESH is checking system resolution", "ParaMESH.StartUp")
        If Screen.PrimaryScreen.WorkingArea.Width < 1024 And ParaMESH.IsEnabled("StartHelp", ParaMESH.SystemParameters, 1) = True Then
            MsgBox("The primary screen resolution is less than 1024 pixels across.  The ParaMESH Help File may not display correctly.", MsgBoxStyle.OkOnly, "ParaMESH")

            '***Disable ParaMESH.SystemParameters(StartHelp)
            ParaSAVE.SystemParameters("Disabled", "StartHelp")
        End If

        '***LOAD ParaMESH SAMPLE FILES
        If ParaMESH.SystemParameters(1, ParaMESH.FindIndex("LoadSamples", ParaMESH.SystemParameters)) = "Enabled" And System.IO.Directory.Exists(Application.StartupPath & "\" & ParaLOAD.SystemProperties("SampleDirectory")) = True Then
            LoadSamples = MsgBox("Do you wish to load the ParaMESH sample files?", MsgBoxStyle.YesNo, "ParaMESH")
            If LoadSamples = MsgBoxResult.Yes Then

                '***CALL ParaMESH(CallTask) TO LOAD SAMPLE FILES
                ParaMESH.CallOperation("ParaREAD", ProcessForm, ParaREAD.Arguments("SampleFiles"))

                '***CALL ParaMESH(CallTask) TO READ SAMPLE FILES
                ParaMESH.CallOperation("ParaREAD", ProcessForm, ParaREAD.Arguments("ReadFiles"))
            End If
        End If

        '***Disable ..SystemParameters(LoadSamples)
        '***SAMPLE FILES ARE ONLY LOADED AUTOMATICALLY THE FIRST TIME ParaMESH IS RUN, THEY CAN BE LOADED USING LoadForm DURING ANY SESSION
        ParaSAVE.SystemParameters("Disabled", "LoadSamples")
    End Sub

    '****************************************************************************
    'SUBROUTINE: ShutDown
    '****************************************************************************
    'UPDATED: APR 22 2008 (DAN) to use common ParaMESH subroutines
    '****************************************************************************
    'These procedures are called each time ParaMESH is shut down.
    '****************************************************************************

    Public Shared Sub ShutDown()

        '***IF ParaMESH.SystemParameters(StartUp) IS ENABLED THEN SAVE ParaMESH CONFIGURATION
        If ParaMESH.IsEnabled("StartUp", ParaMESH.SystemParameters, 1) = True Then

            '***RESET ..SystemParameters(NoOutput) VALUE
            ParaSAVE.SystemParameters("Disabled", "NoOutput")

            '***CALL ParaWRITE(Configuration) TO SAVE THE ParaMESH CONFIGURATION FILE
            ParaWRITE.Configuration()
        End If
    End Sub

    '****************************************************************************
    'SUBROUTINE: AddSecondary
    '****************************************************************************
    'Add new secondary parameter for given PrimaryParameter to 
    'ParaMESH(SecondaryParameters).
    '****************************************************************************

    Public Shared Sub AddSecondary(ByVal PrimaryParameter As String)
        With IdentifierForm
            .Box_Identifier.Clear()
            .ShowDialog()
        End With

        '***CHECKS TO PREVENT ERRONEOUS OR REPETITIVE SECONDARY PARAMETER NAMING
        If IdentifierForm.Box_Identifier.Text = Nothing Then
            MsgBox("The parameter must have a valid identifier.  ParaMESH can not create the parameter.", MsgBoxStyle.OkOnly, "ParaMESH")
            Exit Sub
        ElseIf ParaMESH.FindIndex(IdentifierForm.Box_Identifier.Text, ParaMESH.PrimaryParameters) > -1 Then
            MsgBox("The parameter can not share an identifier with an exiting primary parameter.  ParaMESH can not create the parameter.", MsgBoxStyle.OkOnly, "ParaMESH")
            Exit Sub
        ElseIf ParaMESH.FindIndex(IdentifierForm.Box_Identifier.Text, ParaMESH.SecondaryParameters) > -1 Then
            MsgBox("The parameter already exists, or a parameter with the identifier exists for an other primary parameter.  ParaMESH can not create the parameter.", MsgBoxStyle.OkOnly, "ParaMESH")
            Exit Sub
        ElseIf IdentifierForm.Box_Identifier.Text.Contains(" ") = True Then
            MsgBox("The parameter identifer may not contain any spacing.  ParaMESH can not create the parameter.", MsgBoxStyle.OkOnly, "ParaMESH")
            Exit Sub
        End If

        '***ADD THE SECONDARY PARAMETER TO END OF ParaMESH(SecondaryParameters)
        ReDim Preserve ParaMESH.SecondaryParameters(ParaMESH.SecondaryParameters.GetUpperBound(0), ParaMESH.SecondaryParameters.GetUpperBound(1) + 1)
        For n = 0 To ParaMESH.SecondaryParameters.GetUpperBound(0)
            ParaMESH.SecondaryParameters(n, ParaMESH.SecondaryParameters.GetUpperBound(1)) = ""
        Next

        '***INHERIT THE FOLLOWING DEFINITIONS FROM PrimaryParameter IN ParaMESH(PrimaryParameters)
        ParaMESH.SecondaryParameters(0, ParaMESH.SecondaryParameters.GetUpperBound(1)) = PrimaryParameter
        ParaMESH.SecondaryParameters(1, ParaMESH.SecondaryParameters.GetUpperBound(1)) = IdentifierForm.Box_Identifier.Text

        '***MUST NOT USE INTEGER p AS IS USED BY CALLED ParaMESH(FindIndex) IN ParaLOAD(PrimaryParameters)
        For n = 2 To 9

            '***CALL ParaLOAD(PrimaryParameters) TO LOAD THE VALUES
            ParaMESH.SecondaryParameters(2, ParaMESH.SecondaryParameters.GetUpperBound(1)) = ParaLOAD.PrimaryParameters(n, PrimaryParameter)
        Next

        '***CALL ParaMESH(SecondaryCount) TO DETERMINE THE ASSUMED INDEX OF THIS PARAMETER
        ParaMESH.SecondaryParameters(10, ParaMESH.SecondaryParameters.GetUpperBound(1)) = ParaMESH.SecondaryCount(PrimaryParameter)
    End Sub

    '****************************************************************************
    'SUBROUTINE: RemoveSecondary
    '****************************************************************************
    'UPDATED: APR 22 2008 (DAN) to use common comparison (search is no longer 
    '                           case-sensitive.
    '****************************************************************************
    'Remove given SecondaryParameter associated with given PrimaryParameter from 
    'ParaMESH(SecondaryParameters).
    '****************************************************************************

    Public Shared Sub RemoveSecondary(ByVal SecondaryParameter As String, ByVal PrimaryParameter As String)
        RemoveParameter = MsgBox("Do you wish to permanently remove the secondary parameter: " & SecondaryParameter, MsgBoxStyle.YesNo, "ParaMESH")
        If RemoveParameter = MsgBoxResult.No Then
            Exit Sub
        End If

        '***FIND THE SECONDARY PARAMETER
        For n = 0 To ParaMESH.SecondaryParameters.GetUpperBound(1)
            If ParaMESH.SecondaryParameters(0, n).ToLower = PrimaryParameter.ToLower And ParaMESH.SecondaryParameters(1, n).ToLower = SecondaryParameter.ToLower Then

                '***CALL ParaMESH(RemoveField) TO REMOVE SecondaryParameter FROM ParaMESH(SecondaryParameters)
                ParaMESH.SecondaryParameters = ParaMESH.RemoveField(n, ParaMESH.SecondaryParameters)
                Exit For
            End If
        Next
    End Sub

    '****************************************************************************
    'SUBROUTINE: SetLabels                          'REMOVED:   FEB 05 2008 (DAN)
    '****************************************************************************
    'UPDATED: MAR 05 2008 (DAN) incorporated into x(Populate) for all forms
    'UPDATED: FEB 05 2008 (DAN) parameter labelling has been incorporated into 
    '                           ParaLOAD for into x.Event(Shown) for all forms
    '****************************************************************************
    'Populate the parameter labelling for all forms from the system variable 
    'information parameter.
    '****************************************************************************

    '****************************************************************************
    'SUBROUTINE: SetDefaults                        'REMOVED:   JAN 21 2008 (DAN)
    '****************************************************************************
    'UPDATED: MAR 05 2008 (DAN) reinstated into ParaMESH(SetDefaults)
    'UPDATED: MAR 04 2008 (DAN) temporarily reformed in ParaLOAD(DefaultValues)
    'UPDATED: JAN 21 2008 (DAN) REMOVED JAN 21 2008 (for using inefficient 
    '                           methods)
    '****************************************************************************
    'Populate the system variable information parameter from the ParaMESH.var 
    'file.  Populate the WATFLOW 2.02 and MESH parameters from the system 
    'variable information parameter.
    '****************************************************************************

    '****************************************************************************
    'SUBROUTINE: RedimGRU
    '****************************************************************************
    'UPDATED: MAR 07 2008 (DAN) modified to allow for redimensioning by any 
    '                           integer value
    '****************************************************************************
    'Redimensions all GRU-related parameters by the given UpperBound value.
    '****************************************************************************

    Public Shared Sub RedimGRU(ByVal GRU As Integer)

        '***GRU DESCRIPTION
        ReDim Preserve ParaMESH.GRU(ParaMESH.GRU.GetUpperBound(0), GRU)

        '***FIRST BLOCK
        ReDim Preserve ParaMESH.FCAN(ParaMESH.FCAN.GetUpperBound(0), ParaMESH.FCAN.GetUpperBound(1), GRU)
        ReDim Preserve ParaMESH.LNZ0(ParaMESH.LNZ0.GetUpperBound(0), ParaMESH.LNZ0.GetUpperBound(1), GRU)
        ReDim Preserve ParaMESH.ALVC(ParaMESH.ALVC.GetUpperBound(0), ParaMESH.ALVC.GetUpperBound(1), GRU)
        ReDim Preserve ParaMESH.ALIC(ParaMESH.ALIC.GetUpperBound(0), ParaMESH.ALIC.GetUpperBound(1), GRU)
        ReDim Preserve ParaMESH.LAMX(ParaMESH.LAMX.GetUpperBound(0), ParaMESH.LAMX.GetUpperBound(1), GRU)
        ReDim Preserve ParaMESH.LAMN(ParaMESH.LAMN.GetUpperBound(0), ParaMESH.LAMN.GetUpperBound(1), GRU)
        ReDim Preserve ParaMESH.CMAS(ParaMESH.CMAS.GetUpperBound(0), ParaMESH.CMAS.GetUpperBound(1), GRU)
        ReDim Preserve ParaMESH.ROOT(ParaMESH.ROOT.GetUpperBound(0), ParaMESH.ROOT.GetUpperBound(1), GRU)
        ReDim Preserve ParaMESH.RSMN(ParaMESH.RSMN.GetUpperBound(0), ParaMESH.RSMN.GetUpperBound(1), GRU)
        ReDim Preserve ParaMESH.QA50(ParaMESH.QA50.GetUpperBound(0), ParaMESH.QA50.GetUpperBound(1), GRU)
        ReDim Preserve ParaMESH.VPDA(ParaMESH.VPDA.GetUpperBound(0), ParaMESH.VPDA.GetUpperBound(1), GRU)
        ReDim Preserve ParaMESH.VPDB(ParaMESH.VPDB.GetUpperBound(0), ParaMESH.VPDB.GetUpperBound(1), GRU)
        ReDim Preserve ParaMESH.PSGA(ParaMESH.PSGA.GetUpperBound(0), ParaMESH.PSGA.GetUpperBound(1), GRU)
        ReDim Preserve ParaMESH.PSGB(ParaMESH.PSGB.GetUpperBound(0), ParaMESH.PSGB.GetUpperBound(1), GRU)
        ReDim Preserve ParaMESH.DRN(ParaMESH.DRN.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.SDEP(ParaMESH.SDEP.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.FARE(ParaMESH.FARE.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.DDEN(ParaMESH.DDEN.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.XSLP(ParaMESH.XSLP.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.GRKF(ParaMESH.GRKF.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.WFSF(ParaMESH.WFSF.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.WFCI(ParaMESH.WFCI.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC01(ParaMESH.RSERVC01.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC02(ParaMESH.RSERVC02.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC03(ParaMESH.RSERVC03.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC04(ParaMESH.RSERVC04.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC05(ParaMESH.RSERVC05.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC06(ParaMESH.RSERVC06.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC07(ParaMESH.RSERVC07.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC08(ParaMESH.RSERVC08.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC09(ParaMESH.RSERVC09.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC10(ParaMESH.RSERVC10.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC11(ParaMESH.RSERVC11.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC12(ParaMESH.RSERVC12.GetUpperBound(0), GRU)

        '***SECOND BLOCK
        ReDim Preserve ParaMESH.SAND(ParaMESH.SAND.GetUpperBound(0), ParaMESH.SAND.GetUpperBound(1), GRU)
        ReDim Preserve ParaMESH.CLAY(ParaMESH.CLAY.GetUpperBound(0), ParaMESH.CLAY.GetUpperBound(1), GRU)
        ReDim Preserve ParaMESH.ORGM(ParaMESH.ORGM.GetUpperBound(0), ParaMESH.ORGM.GetUpperBound(1), GRU)
        ReDim Preserve ParaMESH.TBAR(ParaMESH.TBAR.GetUpperBound(0), ParaMESH.TBAR.GetUpperBound(1), GRU)
        ReDim Preserve ParaMESH.TCAN(ParaMESH.TCAN.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.TSNO(ParaMESH.TSNO.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.TPND(ParaMESH.TPND.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.ZPND(ParaMESH.ZPND.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.THLQ(ParaMESH.THLQ.GetUpperBound(0), ParaMESH.THLQ.GetUpperBound(1), GRU)
        ReDim Preserve ParaMESH.THIC(ParaMESH.THIC.GetUpperBound(0), ParaMESH.THIC.GetUpperBound(1), GRU)
        ReDim Preserve ParaMESH.RCAN(ParaMESH.RCAN.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.SCAN(ParaMESH.SCAN.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.SNO(ParaMESH.SNO.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.ALBS(ParaMESH.ALBS.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RHOS(ParaMESH.RHOS.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.GRO(ParaMESH.GRO.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC13(ParaMESH.RSERVC13.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC14(ParaMESH.RSERVC14.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC15(ParaMESH.RSERVC15.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC16(ParaMESH.RSERVC16.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC17(ParaMESH.RSERVC17.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC18(ParaMESH.RSERVC18.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC19(ParaMESH.RSERVC19.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC20(ParaMESH.RSERVC20.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC21(ParaMESH.RSERVC21.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC22(ParaMESH.RSERVC22.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC23(ParaMESH.RSERVC23.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC24(ParaMESH.RSERVC24.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC25(ParaMESH.RSERVC25.GetUpperBound(0), GRU)
        ReDim Preserve ParaMESH.RSERVC26(ParaMESH.RSERVC26.GetUpperBound(0), GRU)
    End Sub

    '****************************************************************************
    'SUBROUTINE: TransferGRU
    '****************************************************************************
    'Transfers the values from given SourceGRU to given DestinationGRU for all 
    'GRU-related parameters.
    '****************************************************************************

    Public Shared Sub TransferGRU(ByVal SourceGRU As Integer, ByVal DestinationGRU As Integer)
        For m = 0 To ParaMESH.GRU.GetUpperBound(0)

            '***GRU DESCRIPTION
            ParaSAVE.GRU(ParaMESH.GRU(m, SourceGRU), DestinationGRU, m)

            '***FIRST BLOCK
            For n = 0 To ParaMESH.FCAN.GetUpperBound(1)
                ParaSAVE.FCAN(ParaMESH.FCAN(m, n, SourceGRU), n, DestinationGRU, m)
            Next
            For n = 0 To ParaMESH.LNZ0.GetUpperBound(1)
                ParaSAVE.LNZ0(ParaMESH.LNZ0(m, n, SourceGRU), n, DestinationGRU, m)
            Next
            For n = 0 To ParaMESH.ALVC.GetUpperBound(1)
                ParaSAVE.ALVC(ParaMESH.ALVC(m, n, SourceGRU), n, DestinationGRU, m)
            Next
            For n = 0 To ParaMESH.ALIC.GetUpperBound(1)
                ParaSAVE.ALIC(ParaMESH.ALIC(m, n, SourceGRU), n, DestinationGRU, m)
            Next
            For n = 0 To ParaMESH.LAMX.GetUpperBound(1)
                ParaSAVE.LAMX(ParaMESH.LAMX(m, n, SourceGRU), n, DestinationGRU, m)
            Next
            For n = 0 To ParaMESH.LAMN.GetUpperBound(1)
                ParaSAVE.LAMN(ParaMESH.LAMN(m, n, SourceGRU), n, DestinationGRU, m)
            Next
            For n = 0 To ParaMESH.CMAS.GetUpperBound(1)
                ParaSAVE.CMAS(ParaMESH.CMAS(m, n, SourceGRU), n, DestinationGRU, m)
            Next
            For n = 0 To ParaMESH.ROOT.GetUpperBound(1)
                ParaSAVE.ROOT(ParaMESH.ROOT(m, n, SourceGRU), n, DestinationGRU, m)
            Next
            For n = 0 To ParaMESH.RSMN.GetUpperBound(1)
                ParaSAVE.RSMN(ParaMESH.RSMN(m, n, SourceGRU), n, DestinationGRU, m)
            Next
            For n = 0 To ParaMESH.QA50.GetUpperBound(1)
                ParaSAVE.QA50(ParaMESH.QA50(m, n, SourceGRU), n, DestinationGRU, m)
            Next
            For n = 0 To ParaMESH.VPDA.GetUpperBound(1)
                ParaSAVE.VPDA(ParaMESH.VPDA(m, n, SourceGRU), n, DestinationGRU, m)
            Next
            For n = 0 To ParaMESH.VPDB.GetUpperBound(1)
                ParaSAVE.VPDB(ParaMESH.VPDB(m, n, SourceGRU), n, DestinationGRU, m)
            Next
            For n = 0 To ParaMESH.PSGA.GetUpperBound(1)
                ParaSAVE.PSGA(ParaMESH.PSGA(m, n, SourceGRU), n, DestinationGRU, m)
            Next
            For n = 0 To ParaMESH.PSGB.GetUpperBound(1)
                ParaSAVE.PSGB(ParaMESH.PSGB(m, n, SourceGRU), n, DestinationGRU, m)
            Next
            ParaSAVE.DRN(ParaMESH.DRN(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.SDEP(ParaMESH.SDEP(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.FARE(ParaMESH.FARE(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.DDEN(ParaMESH.DDEN(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.XSLP(ParaMESH.XSLP(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.GRKF(ParaMESH.GRKF(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.WFSF(ParaMESH.WFSF(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.WFCI(ParaMESH.WFCI(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC01(ParaMESH.RSERVC01(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC02(ParaMESH.RSERVC02(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC03(ParaMESH.RSERVC03(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC04(ParaMESH.RSERVC04(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC05(ParaMESH.RSERVC05(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC06(ParaMESH.RSERVC06(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC07(ParaMESH.RSERVC07(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC08(ParaMESH.RSERVC08(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC09(ParaMESH.RSERVC09(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC10(ParaMESH.RSERVC10(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC11(ParaMESH.RSERVC11(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC12(ParaMESH.RSERVC12(m, SourceGRU), DestinationGRU, m)

            '***SECOND BLOCK
            For n = 0 To ParaMESH.SAND.GetUpperBound(1)
                ParaSAVE.SAND(ParaMESH.SAND(m, n, SourceGRU), n, DestinationGRU, m)
            Next
            For n = 0 To ParaMESH.CLAY.GetUpperBound(1)
                ParaSAVE.CLAY(ParaMESH.CLAY(m, n, SourceGRU), n, DestinationGRU, m)
            Next
            For n = 0 To ParaMESH.ORGM.GetUpperBound(1)
                ParaSAVE.ORGM(ParaMESH.ORGM(m, n, SourceGRU), n, DestinationGRU, m)
            Next
            For n = 0 To ParaMESH.TBAR.GetUpperBound(1)
                ParaSAVE.TBAR(ParaMESH.TBAR(m, n, SourceGRU), n, DestinationGRU, m)
            Next
            ParaSAVE.TCAN(ParaMESH.TCAN(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.TSNO(ParaMESH.TSNO(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.TPND(ParaMESH.TPND(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.ZPND(ParaMESH.ZPND(m, SourceGRU), DestinationGRU, m)
            For n = 0 To ParaMESH.THLQ.GetUpperBound(1)
                ParaSAVE.THLQ(ParaMESH.THLQ(m, n, SourceGRU), n, DestinationGRU, m)
            Next
            For n = 0 To ParaMESH.THIC.GetUpperBound(1)
                ParaSAVE.THIC(ParaMESH.THIC(m, n, SourceGRU), n, DestinationGRU, m)
            Next
            ParaSAVE.RCAN(ParaMESH.RCAN(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.SCAN(ParaMESH.SCAN(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.SNO(ParaMESH.SNO(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.ALBS(ParaMESH.ALBS(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RHOS(ParaMESH.RHOS(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.GRO(ParaMESH.GRO(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC13(ParaMESH.RSERVC13(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC14(ParaMESH.RSERVC14(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC15(ParaMESH.RSERVC15(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC16(ParaMESH.RSERVC16(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC17(ParaMESH.RSERVC17(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC18(ParaMESH.RSERVC18(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC19(ParaMESH.RSERVC19(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC20(ParaMESH.RSERVC20(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC21(ParaMESH.RSERVC21(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC22(ParaMESH.RSERVC22(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC23(ParaMESH.RSERVC23(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC24(ParaMESH.RSERVC24(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC25(ParaMESH.RSERVC25(m, SourceGRU), DestinationGRU, m)
            ParaSAVE.RSERVC26(ParaMESH.RSERVC26(m, SourceGRU), DestinationGRU, m)
        Next
    End Sub

    '****************************************************************************
    'FUNCTION: FindIndex
    '****************************************************************************
    'UPDATED: APR 22 2008 (DAN) to use common comparison (no longer 
    '                           case-sensitive)
    'UPDATED: MAR 26 2008 (DAN) modified to use ParaMESH(PrimaryName) for 
    '                           finding secondary parameters
    'UPDATED: FEB 13 2008 (DAN) complex FindIndex for secondary parameters 
    '                           removed
    '****************************************************************************
    'Return the index of SourceCall in the given SourceArray.
    '****************************************************************************

    Public Shared Function FindIndex(ByVal SourceCall As String, ByVal SourceArray(,) As String) As Integer
        For p = 1 To SourceArray.GetUpperBound(1)
            If SourceArray(0, p).ToLower = SourceCall.ToLower Then

                '***FOR ALL GENERIC CASES
                FindIndex = p
                Exit Function
            ElseIf SourceArray(1, p).ToLower = SourceCall.ToLower Then

                '***FOR SECONDARY PARAMETERS
                FindIndex = p
                Exit Function
            ElseIf SourceArray(0, p).IndexOf("[") > 0 Then

                '***FOR SYSTEM ARRAYS, LOOK FOR "[" CONTAINER
                If SourceArray(0, p).Substring(0, SourceArray(0, p).IndexOf("[")).ToLower = SourceCall.ToLower Then
                    FindIndex = p
                    Exit Function
                End If
            End If
        Next

        '***RETURN -1 IF ParameterCall WAS NOT FOUND IN FromArray
        FindIndex = -1
    End Function

    '****************************************************************************
    'FUNCTION: RemoveField
    '****************************************************************************
    'Remove the speficied index (RemoveIndex) from the given two-dimensional 
    'array (FromArray).  Return the modified array.
    '****************************************************************************

    Public Shared Function RemoveField(ByVal RemoveIndex As Integer, ByVal FromArray(,) As String) As String(,)

        '***MOVES EVERY FIELD AFTER THE SPECIFIED RemoveIndex UP AN INDEX
        For n = RemoveIndex To FromArray.GetUpperBound(1) - 1
            For m = 0 To FromArray.GetUpperBound(0)
                FromArray(m, n) = FromArray(m, n + 1)
            Next
        Next

        '***REDIMENSION THE FromArray TO ACCOUNT FOR ONE LESS INDEX
        ReDim Preserve FromArray(FromArray.GetUpperBound(0), FromArray.GetUpperBound(1) - 1)

        '***RETURN FromArray
        RemoveField = FromArray
    End Function

    '****************************************************************************
    'FUNCTION: SecondaryName
    '****************************************************************************
    'UPDATED: APR 22 2008 (DAN) to use common comparison (no longer 
    '                           case-sensitive)
    '****************************************************************************
    'Return the secondary parameter name for the specific RowIndex of the given 
    'PrimaryParameter from ParaMESH(SecondaryParameters).
    '****************************************************************************

    Public Shared Function SecondaryName(ByVal RowIndex As Integer, ByVal PrimaryParameter As String) As String

        '***FIND RowIndex FOR PrimaryParameter IN ParaMESH(SecondaryParameters)
        For p = 1 To ParaMESH.SecondaryParameters.GetUpperBound(1)
            If ParaMESH.SecondaryParameters(0, p).ToLower = PrimaryParameter.ToLower And Convert.ToInt32(ParaMESH.SecondaryParameters(10, p)) = RowIndex Then

                '***RETURN THE PARAMETER NAME
                SecondaryName = ParaMESH.SecondaryParameters(1, p)
                Exit Function
            End If
        Next

        '***RETURN PrimaryParameter IF THE RowIndex AND PrimaryParameter WERE NOT FOUND
        SecondaryName = PrimaryParameter
    End Function

    '****************************************************************************
    'FUNCTION: PrimaryName
    '****************************************************************************
    'UPDATED: APR 22 2008 (DAN) to use common comparison (no longer 
    '                           case-sensitive)
    '****************************************************************************
    'Return the primary parameter name of the given SecondaryParameter from 
    'ParaMESH(PrimaryParameters).
    '****************************************************************************

    Public Shared Function PrimaryName(ByVal SecondaryParameter As String) As String

        '***FIND SecondaryParameter IN ParaMESH(SecondaryParameters)
        If ParaMESH.FindIndex(SecondaryParameter, ParaMESH.SecondaryParameters) > -1 Then

            '***RETURN THE PARAMETER NAME
            PrimaryName = ParaLOAD.SecondaryParameters(0, SecondaryParameter)
            Exit Function
        End If

        '***RETURN SecondaryParameter IF NOT FOUND IN ParaMESH(SecondaryParameters)
        PrimaryName = SecondaryParameter
    End Function

    '****************************************************************************
    'FUNCTION: FindColumn
    '****************************************************************************
    'UPDATED: APR 22 2008 (DAN) to use common comparison (no longer 
    '                           case-sensitive)
    '****************************************************************************
    'Return the index of SourceCall in first dimension of SourceArray.
    '****************************************************************************

    Public Shared Function FindColumn(ByVal SourceCall As String, ByVal SourceArray(,) As String) As Integer
        For p = 0 To SourceArray.GetUpperBound(0)
            If SourceArray(p, 0).Substring(SourceArray(p, 0).IndexOf("[") + 1, SourceArray(p, 0).IndexOf("]") - SourceArray(p, 0).IndexOf("[") - 1).ToLower = SourceCall.ToLower Then
                FindColumn = p
                Exit Function
            End If
        Next

        '***RETURN -1 IF SourceCall IS NOT FOUND IN SourceArray
        FindColumn = -1
    End Function

    '****************************************************************************
    'FUNCTION: IsEnabled
    '****************************************************************************
    'UPDATED: APR 22 2008 (DAN) to use common comparison (no longer 
    '                           case-sensitive)
    'UPDATE: MAR 07 2008 (DAN)  modified to accept any call for any 
    '                           two-dimensional array.
    '****************************************************************************
    'Checks if given SourceCall is enabled.  Returns True if the parameter is 
    'enabled, returns False if the parameter is not enabled.
    '****************************************************************************

    Public Shared Function IsEnabled(ByVal SourceCall As String, ByVal SourceArray(,) As String, Optional ByVal FieldIndex As Integer = 2) As Boolean

        '***RETURNS False BY DEFAULT
        IsEnabled = False
        Try

            '***CHECKS IF SourceCall IS ENABLED IN SourceArray
            If SourceArray(FieldIndex, ParaMESH.FindIndex(SourceCall, SourceArray)).ToLower = "enabled" Then
                IsEnabled = True
            End If
        Catch ex As Exception
            Exit Function
        End Try
    End Function

    '****************************************************************************
    'FUNCTION: GetDefault
    '****************************************************************************
    'Returns the default value for given ParameterCall from either 
    'ParaMESH(PrimaryParameters) or ParaMESH(SecondaryParameters).
    '****************************************************************************

    Public Shared Function GetDefault(ByVal ParameterCall As String) As String
        Try

            '***RETURN DEFAULT VALUE FOR SECONDARY PARAMETER
            GetDefault = ParaLOAD.SecondaryParameters(5, ParameterCall)
        Catch ex As Exception

            '***RETURN DEFAULT VALUE FOR PRIMARY PARAMETER
            GetDefault = ParaLOAD.PrimaryParameters(5, ParameterCall)
        End Try
    End Function

    '****************************************************************************
    'FUNCTION: SecondaryCount
    '****************************************************************************
    'Used to count how many secondary parameters a given primary parameter 
    'ParameterName will have by default based on ParaMESH(SecondaryParameters).  
    'Returns Count of associated secondary parameters with ParameterName.
    '****************************************************************************

    Public Shared Function SecondaryCount(ByVal ParameterName As String)

        '***SET SecondaryCount TO -1 BY DEFAULT (WILL CAUSE ERROR, CONTROLLED BY CALLING SUBROUTINE)
        SecondaryCount = -1
        For p = 0 To ParaMESH.SecondaryParameters.GetUpperBound(1)
            If ParaMESH.SecondaryParameters(0, p) = ParameterName Then

                '***RETURN HIGHEST FIELD INDEX OF SECONDARY PARAMETER FOR ParameterName IN ParaMESH(SecondaryParameters)
                If Convert.ToInt32(ParaMESH.SecondaryParameters(10, p)) > SecondaryCount Then
                    SecondaryCount = Convert.ToInt32(ParaMESH.SecondaryParameters(10, p))
                End If
            End If
        Next
    End Function

    '****************************************************************************
    'FUNCTION: CheckValue
    '****************************************************************************
    'UPDATE: MAR 05 2008 (DAN)  moved from ParaSAVE to ParaMESH, is used in more 
    '                           subroutines now
    '****************************************************************************
    'Checks parameter type so that given Value is assigned to ParameterName 
    'successfully.  Returns value as required type for ParameterName or returns 
    'zero on error.
    '****************************************************************************

    Public Shared Function CheckValue(ByVal Value As Object, ByVal ParameterName As String) As Object

        '***CHECK IF PARAMETER IS PRIMARY OR SECONDARY PARAMETER
        If ParaMESH.FindIndex(ParameterName, ParaMESH.PrimaryParameters) > -1 Then
            ParameterArray = ParaMESH.PrimaryParameters
        Else
            ParameterArray = ParaMESH.SecondaryParameters
        End If

        '***CHECK TYPE FOR PRIMARY PARAMETERS (IS SENT THRU AS PARAMETER NAME)
        Select Case ParameterArray(9, ParaMESH.FindIndex(ParameterName, ParameterArray))
            Case Is = "Integer"
                Try
                    ParaLOG.AppendFile(ParameterName & "# ..converting the Integer value: " & Value, "ParaMESH.CheckValue", "SavVar")
                    CheckValue = Convert.ToInt32(Value)
                Catch ex As Exception
                    CheckValue = 0
                End Try
            Case Is = "Double"
                Try
                    ParaLOG.AppendFile(ParameterName & "# ..converting the Double value: " & Value, "ParaMESH.CheckValue", "SavVar")
                    CheckValue = Convert.ToDouble(Value)
                Catch ex As Exception
                    CheckValue = 0
                End Try
            Case Else
                ParaLOG.AppendFile(ParameterName & "# ..converting the String value: " & Value, "ParaMESH.CheckValue", "SavVar")
                CheckValue = Convert.ToString(Value)
        End Select
    End Function

    '*****************************************************************************
    'SUBROUTINE: ParaSTOR.SaveValue
    '*****************************************************************************
    'UPDATE: MAR 22 2008 (DAN)  moved from ParaSTOR to ParaMESH, is used in more 
    '                           subroutines now
    'UPDATE: MAR 22 2008 (DAN)  moved from ParaCHECK to ParaSTOR
    'UPDATE: MAR 22 2008 (DAN)  renamed "SaveValue" from "Save", 
    '                           ParaSAVE(FindVariable) approach imported to 
    '                           reduce footprint in ParaSAVE class
    'UPDATE: JAN 11 2008 (DAN)  for IgnoreCheck approach
    'UPDATE: JAN 11 2008 (DAN)  for ParaLOG approach
    '*****************************************************************************
    'Calls ParaSAVE(ParameterCall) to save ParameterCall value.
    '*****************************************************************************

    Public Shared Sub SaveValue(ByVal Value As String, ByVal ParameterCall As String, ByVal Index As Integer, ByVal ColumnIndex As Integer)

        '***CALL ParaSAVE(ParameterCall) TO SAVE VALUE
        Select Case ParaMESH.PrimaryName(ParameterCall)
            Case Is = "TITLE"
                ParaSAVE.TITLE(Value)
            Case Is = "NAME"
                ParaSAVE.NAME(Value)
            Case Is = "PLACE"
                ParaSAVE.PLACE(Value)
            Case Is = "DEGLAT"
                ParaSAVE.DEGLAT(Value)
            Case Is = "DEGLON"
                ParaSAVE.DEGLON(Value)
            Case Is = "ZRFM"
                ParaSAVE.ZRFM(Value)
            Case Is = "ZRFH"
                ParaSAVE.ZRFH(Value)
            Case Is = "ZBLD"
                ParaSAVE.ZBLD(Value)
            Case Is = "GC"
                ParaSAVE.GC(Value)
            Case Is = "ILW"
                ParaSAVE.ILW(Value)
            Case Is = "GRID"
                ParaSAVE.GRID(Value)
            Case Is = "HourlyStartDay"
                ParaSAVE.HourlyStartDay(Value)
            Case Is = "HourlyStartYear"
                ParaSAVE.HourlyStartYear(Value)
            Case Is = "HourlyStopDay"
                ParaSAVE.HourlyStopDay(Value)
            Case Is = "HourlyStopYear"
                ParaSAVE.HourlyStopYear(Value)
            Case Is = "DailyStartDay"
                ParaSAVE.DailyStartDay(Value)
            Case Is = "DailyStartYear"
                ParaSAVE.DailyStartYear(Value)
            Case Is = "DailyStopDay"
                ParaSAVE.DailyStopDay(Value)
            Case Is = "DailyStopYear"
                ParaSAVE.DailyStopYear(Value)
            Case Is = "SimStartDay"
                ParaSAVE.SimStartDay(Value)
            Case Is = "SimStartYear"
                ParaSAVE.SimStartYear(Value)
            Case Is = "SimStopDay"
                ParaSAVE.SimStopDay(Value)
            Case Is = "SimStopYear"
                ParaSAVE.SimStopYear(Value)
            Case Is = "MetStartMin"
                ParaSAVE.MetStartMin(Value)
            Case Is = "MetStartHour"
                ParaSAVE.MetStartHour(Value)
            Case Is = "MetStartDay"
                ParaSAVE.MetStartDay(Value)
            Case Is = "MetStartYear"
                ParaSAVE.MetStartYear(Value)
            Case Is = "WF_R2"
                ParaSAVE.WF_R2(Value, Index)
            Case Is = "ControlFlag"
                ParaSAVE.ControlFlag(ParameterCall, Value)
            Case Is = "GridOutput"
                ParaSAVE.GridOutput(ParameterCall, Value, Index)
            Case Is = "OptionFlag"
                ParaSAVE.OptionFlag(ParameterCall, Value)
            Case Is = "IndependentGRU"
                ParaSAVE.IndependentGRU(ParameterCall, Value)
            Case Is = "DependentGRU"
                ParaSAVE.DependentGRU(ParameterCall, Value, Index)
            Case Is = "BasinID"
                ParaSAVE.BasinID(Value)
            Case Is = "ScalingFactor"
                ParaSAVE.ScalingFactor(Value)
            Case Is = "GWINIT"
                ParaSAVE.GWINIT(Value)
            Case Is = "WatflowFlag"
                ParaSAVE.WatflowFlag(ParameterCall, Value)
            Case Is = "ZPLIMG0"
                ParaSAVE.ZPLIMG0(Value)
            Case Is = "ZPLIMS0"
                ParaSAVE.ZPLIMS0(Value)
            Case Is = "ZPLIMP0"
                ParaSAVE.ZPLIMP0(Value)
            Case Is = "ZPLIMPS0"
                ParaSAVE.ZPLIMPS0(Value)
            Case Is = "D100A"
                ParaSAVE.D100A(Value, Index)
            Case Is = "GRU"
                ParaSAVE.GRU(Value, Index)
            Case Is = "FCAN"
                ParaSAVE.FCAN(Value, ColumnIndex, Index)
            Case Is = "LNZ0"
                ParaSAVE.LNZ0(Value, ColumnIndex, Index)
            Case Is = "ALVC"
                ParaSAVE.ALVC(Value, ColumnIndex, Index)
            Case Is = "ALIC"
                ParaSAVE.ALIC(Value, ColumnIndex, Index)
            Case Is = "LAMX"
                ParaSAVE.LAMX(Value, ColumnIndex, Index)
            Case Is = "LAMN"
                ParaSAVE.LAMN(Value, ColumnIndex, Index)
            Case Is = "CMAS"
                ParaSAVE.CMAS(Value, ColumnIndex, Index)
            Case Is = "ROOT"
                ParaSAVE.ROOT(Value, ColumnIndex, Index)
            Case Is = "RSMN"
                ParaSAVE.RSMN(Value, ColumnIndex, Index)
            Case Is = "QA50"
                ParaSAVE.QA50(Value, ColumnIndex, Index)
            Case Is = "VPDA"
                ParaSAVE.VPDA(Value, ColumnIndex, Index)
            Case Is = "VPDB"
                ParaSAVE.VPDB(Value, ColumnIndex, Index)
            Case Is = "PSGA"
                ParaSAVE.PSGA(Value, ColumnIndex, Index)
            Case Is = "PSGB"
                ParaSAVE.PSGB(Value, ColumnIndex, Index)
            Case Is = "DRN"
                ParaSAVE.DRN(Value, Index)
            Case Is = "SDEP"
                ParaSAVE.SDEP(Value, Index)
            Case Is = "FARE"
                ParaSAVE.FARE(Value, Index)
            Case Is = "DDEN"
                ParaSAVE.DDEN(Value, Index)
            Case Is = "XSLP"
                ParaSAVE.XSLP(Value, Index)
            Case Is = "GRKF"
                ParaSAVE.GRKF(Value, Index)
            Case Is = "WFSF"
                ParaSAVE.WFSF(Value, Index)
            Case Is = "WFCI"
                ParaSAVE.WFCI(Value, Index)
            Case Is = "RSERVC01"
                ParaSAVE.RSERVC01(Value, Index)
            Case Is = "RSERVC02"
                ParaSAVE.RSERVC02(Value, Index)
            Case Is = "RSERVC03"
                ParaSAVE.RSERVC03(Value, Index)
            Case Is = "RSERVC04"
                ParaSAVE.RSERVC04(Value, Index)
            Case Is = "RSERVC05"
                ParaSAVE.RSERVC05(Value, Index)
            Case Is = "RSERVC06"
                ParaSAVE.RSERVC06(Value, Index)
            Case Is = "RSERVC07"
                ParaSAVE.RSERVC07(Value, Index)
            Case Is = "RSERVC08"
                ParaSAVE.RSERVC08(Value, Index)
            Case Is = "RSERVC09"
                ParaSAVE.RSERVC09(Value, Index)
            Case Is = "RSERVC10"
                ParaSAVE.RSERVC10(Value, Index)
            Case Is = "RSERVC11"
                ParaSAVE.RSERVC11(Value, Index)
            Case Is = "RSERVC12"
                ParaSAVE.RSERVC12(Value, Index)
            Case Is = "SAND"
                ParaSAVE.SAND(Value, ColumnIndex, Index)
            Case Is = "CLAY"
                ParaSAVE.CLAY(Value, ColumnIndex, Index)
            Case Is = "ORGM"
                ParaSAVE.ORGM(Value, ColumnIndex, Index)
            Case Is = "TBAR"
                ParaSAVE.TBAR(Value, ColumnIndex, Index)
            Case Is = "TCAN"
                ParaSAVE.TCAN(Value, Index)
            Case Is = "TSNO"
                ParaSAVE.TSNO(Value, Index)
            Case Is = "TPND"
                ParaSAVE.TPND(Value, Index)
            Case Is = "THLQ"
                ParaSAVE.THLQ(Value, ColumnIndex, Index)
            Case Is = "THIC"
                ParaSAVE.THIC(Value, ColumnIndex, Index)
            Case Is = "ZPND"
                ParaSAVE.ZPND(Value, Index)
            Case Is = "RCAN"
                ParaSAVE.RCAN(Value, Index)
            Case Is = "SCAN"
                ParaSAVE.SCAN(Value, Index)
            Case Is = "SNO"
                ParaSAVE.SNO(Value, Index)
            Case Is = "ALBS"
                ParaSAVE.ALBS(Value, Index)
            Case Is = "RHOS"
                ParaSAVE.RHOS(Value, Index)
            Case Is = "GRO"
                ParaSAVE.GRO(Value, Index)
            Case Is = "RSERVC13"
                ParaSAVE.RSERVC13(Value, Index)
            Case Is = "RSERVC14"
                ParaSAVE.RSERVC14(Value, Index)
            Case Is = "RSERVC15"
                ParaSAVE.RSERVC15(Value, Index)
            Case Is = "RSERVC16"
                ParaSAVE.RSERVC16(Value, Index)
            Case Is = "RSERVC17"
                ParaSAVE.RSERVC17(Value, Index)
            Case Is = "RSERVC18"
                ParaSAVE.RSERVC18(Value, Index)
            Case Is = "RSERVC19"
                ParaSAVE.RSERVC19(Value, Index)
            Case Is = "RSERVC20"
                ParaSAVE.RSERVC20(Value, Index)
            Case Is = "RSERVC21"
                ParaSAVE.RSERVC21(Value, Index)
            Case Is = "RSERVC22"
                ParaSAVE.RSERVC22(Value, Index)
            Case Is = "RSERVC23"
                ParaSAVE.RSERVC23(Value, Index)
            Case Is = "RSERVC24"
                ParaSAVE.RSERVC24(Value, Index)
            Case Is = "RSERVC25"
                ParaSAVE.RSERVC25(Value, Index)
            Case Is = "RSERVC26"
                ParaSAVE.RSERVC26(Value, Index)
        End Select
    End Sub
End Class