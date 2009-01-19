Public Class ParaLOAD

    '*****************************************************************************
    'CLASS: ParaLOAD
    '*****************************************************************************
    'ParaLOAD is used to load parameter values.  Using this class eliminates the 
    'need for other classes and ParaMESH forms to directly modifying any 
    'parameter value, such that loading parameter values becomes a controlled 
    'and uniform process.
    '*****************************************************************************

    '****************************************************************************
    'PRIVATE ParaLOAD PARAMETERS
    '****************************************************************************
    'Parameters shared and used only within the ParaLOAD class.
    '****************************************************************************
    'Private Integers: r, s, t are unique to the ParaSAVE class so that similarly 
    'private integers from calling subroutines are not reset.
    '****************************************************************************

    Private Shared r As Integer, s As Integer, t As Integer

    Public Shared Function SystemProperties(ByVal SystemProperty As String) As String
        ParaLOG.AppendFile("ParaMESH is loading the system property: ..SystemProperties(" & SystemProperty & ")", "ParaLOAD.SystemProperties")
        SystemProperties = ParaMESH.SystemProperties(1, ParaMESH.FindIndex(SystemProperty, ParaMESH.SystemProperties))
    End Function

    Public Shared Function PrimaryParameters(ByVal Index As Integer, ByVal ParameterCall As String) As String
        ParaLOG.AppendFile("ParaMESH is loading Column " & (Index + 1).ToString & " of primary parameter: ..PrimaryParameters(" & ParameterCall & ")", "ParaLOAD.PrimaryParameters")
        PrimaryParameters = ParaMESH.PrimaryParameters(Index, ParaMESH.FindIndex(ParameterCall, ParaMESH.PrimaryParameters))
    End Function

    Public Shared Function SecondaryParameters(ByVal Index As Integer, ByVal ParameterCall As String) As String
        ParaLOG.AppendFile("ParaMESH is loading Column " & (Index + 1).ToString & " of secondary parameter: ..SecondaryParameters(" & ParameterCall & ")", "ParaLOAD.SecondaryParameters")
        SecondaryParameters = ParaMESH.SecondaryParameters(Index, ParaMESH.FindIndex(ParameterCall, ParaMESH.SecondaryParameters))
    End Function

    Public Shared Function DirectoryList() As String()
        ParaLOG.AppendFile("ParaMESH is loading ParaMESH(DirectoryList)", "ParaLOAD.DirectoryList")
        DirectoryList = ParaLOAD.SystemProperties("DirectoryList").Split(">")
    End Function

    Public Shared Function ControlFlag(ByVal ParameterName As String, Optional ByVal BakFlag As Integer = 0) As String
        ParaLOG.AppendFile(ParameterName & "# (Line " & (ParaMESH.SecondaryParameters(10, ParaMESH.FindIndex(ParameterName, ParaMESH.SecondaryParameters)) + 1).ToString & " of " & ParaMESH.PrimaryParameters(3, ParaMESH.FindIndex("ControlFlag", ParaMESH.PrimaryParameters)) & ")", "ParaLOAD.ControlFlag", "LoadVar")
        ControlFlag = ParaMESH.ControlFlag(BakFlag, ParaMESH.SecondaryParameters(10, ParaMESH.FindIndex(ParameterName, ParaMESH.SecondaryParameters)))
    End Function

    Public Shared Function GridOutput(ByVal ParameterName As String, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As String
        ParaLOG.AppendFile(ParameterName & "# (Line " & (ParaMESH.SecondaryParameters(10, ParaMESH.FindIndex(ParameterName, ParaMESH.SecondaryParameters)) + 1).ToString & ", Grid Output " & (Index + 1).ToString & " of " & ParaMESH.PrimaryParameters(3, ParaMESH.FindIndex("GridOutput", ParaMESH.PrimaryParameters)) & ")", "ParaLOAD.GridOutput", "LoadVar")
        GridOutput = ParaMESH.GridOutput(BakFlag, ParaMESH.SecondaryParameters(10, ParaMESH.FindIndex(ParameterName, ParaMESH.SecondaryParameters)), Index)
    End Function

    Public Shared Function OptionFlag(ByVal ParameterName As String, Optional ByVal BakFlag As Integer = 0) As String
        ParaLOG.AppendFile(ParameterName & "# (Line " & (ParaMESH.SecondaryParameters(10, ParaMESH.FindIndex(ParameterName, ParaMESH.SecondaryParameters)) + 1).ToString & " of " & ParaMESH.PrimaryParameters(3, ParaMESH.FindIndex("OptionFlag", ParaMESH.PrimaryParameters)) & ")", "ParaLOAD.OptionFlag", "LoadVar")
        OptionFlag = ParaMESH.OptionFlag(BakFlag, ParaMESH.SecondaryParameters(10, ParaMESH.FindIndex(ParameterName, ParaMESH.SecondaryParameters)))
    End Function

    Public Shared Function IndependentGRU(ByVal ParameterName As String, Optional ByVal BakFlag As Integer = 0) As String
        ParaLOG.AppendFile(ParameterName & "# (Line " & (ParaMESH.SecondaryParameters(10, ParaMESH.FindIndex(ParameterName, ParaMESH.SecondaryParameters)) + 1).ToString & " of " & ParaMESH.PrimaryParameters(3, ParaMESH.FindIndex("IndependentGRU", ParaMESH.PrimaryParameters)) & ")", "ParaLOAD.IndependentGRU", "LoadVar")
        IndependentGRU = ParaMESH.IndependentGRU(BakFlag, ParaMESH.SecondaryParameters(10, ParaMESH.FindIndex(ParameterName, ParaMESH.SecondaryParameters)))
    End Function

    Public Shared Function DependentGRU(ByVal ParameterName As String, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As String
        ParaLOG.AppendFile(ParameterName & "# (Line " & (ParaMESH.SecondaryParameters(10, ParaMESH.FindIndex(ParameterName, ParaMESH.SecondaryParameters)) + 1).ToString & ", GRU " & (Index + 1).ToString & " of " & ParaMESH.PrimaryParameters(3, ParaMESH.FindIndex("DependentGRU", ParaMESH.PrimaryParameters)) & ")", "ParaLOAD.DependentGRU", "LoadVar")
        DependentGRU = ParaMESH.DependentGRU(BakFlag, ParaMESH.SecondaryParameters(10, ParaMESH.FindIndex(ParameterName, ParaMESH.SecondaryParameters)), Index)
    End Function

    Public Shared Function WatflowFlag(ByVal ParameterName As String, Optional ByVal BakFlag As Integer = 0) As String
        ParaLOG.AppendFile(ParameterName & "# (Line " & (ParaMESH.SecondaryParameters(10, ParaMESH.FindIndex(ParameterName, ParaMESH.SecondaryParameters)) + 1).ToString & " of " & ParaMESH.PrimaryParameters(3, ParaMESH.FindIndex("WatflowFlag", ParaMESH.PrimaryParameters)) & ")", "ParaLOAD.WatflowFlag", "LoadVar")
        WatflowFlag = ParaMESH.WatflowFlag(BakFlag, ParaMESH.SecondaryParameters(10, ParaMESH.FindIndex(ParameterName, ParaMESH.SecondaryParameters)))
    End Function

    Public Shared Function WF_R2(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("WF_R2# (Column " & (Index + 1).ToString & ")", "ParaLOAD.WF_R2", "LoadVar")
        WF_R2 = ParaMESH.WF_R2(BakFlag, Index)
    End Function

    Public Shared Function BasinID(Optional ByVal BakFlag As Integer = 0) As String
        ParaLOG.AppendFile("BasinID#", "ParaLOAD.BasinID", "LoadVar")
        BasinID = ParaMESH.BasinID(BakFlag)
    End Function

    Public Shared Function ScalingFactor(Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("ScalingFactor#", "ParaLOAD.ScalingFactor", "LoadVar")
        ScalingFactor = ParaMESH.ScalingFactor(BakFlag)
    End Function

    Public Shared Function ZPLIMG0(Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("ZPLIMG0#", "ParaLOAD.ZPLIMG0", "LoadVar")
        ZPLIMG0 = ParaMESH.ZPLIMG0(BakFlag)
    End Function

    Public Shared Function ZPLIMS0(Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("ZPLIMS0#", "ParaLOAD.ZPLIMS0", "LoadVar")
        ZPLIMS0 = ParaMESH.ZPLIMS0(BakFlag)
    End Function

    Public Shared Function ZPLIMP0(Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("ZPLIMP0#", "ParaLOAD.ZPLIMP0", "LoadVar")
        ZPLIMP0 = ParaMESH.ZPLIMP0(BakFlag)
    End Function

    Public Shared Function ZPLIMPS0(Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("ZPLIMPS0#", "ParaLOAD.ZPLIMPS0", "LoadVar")
        ZPLIMPS0 = ParaMESH.ZPLIMPS0(BakFlag)
    End Function

    Public Shared Function D100A(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("D100A# (Column " & (Index + 1).ToString & ")", "ParaLOAD.D100A", "LoadVar")
        D100A = ParaMESH.D100A(BakFlag, Index)
    End Function

    Public Shared Function GWINIT(Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("GWINIT#", "ParaLOAD.GWINIT", "LoadVar")
        GWINIT = ParaMESH.GWINIT(BakFlag)
    End Function

    Public Shared Function TITLE(Optional ByVal BakFlag As Integer = 0) As String
        ParaLOG.AppendFile("TITLE#", "ParaLOAD.TITLE", "LoadVar")
        TITLE = ParaMESH.TITLE(BakFlag)
    End Function

    Public Shared Function NAME(Optional ByVal BakFlag As Integer = 0) As String
        ParaLOG.AppendFile("NAME#", "ParaLOAD.NAME", "LoadVar")
        NAME = ParaMESH.NAME(BakFlag)
    End Function

    Public Shared Function PLACE(Optional ByVal BakFlag As Integer = 0) As String
        ParaLOG.AppendFile("PLACE#", "ParaLOAD.PLACE", "LoadVar")
        PLACE = ParaMESH.PLACE(BakFlag)
    End Function

    Public Shared Function DEGLAT(Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("DEGLAT#", "ParaLOAD.DEGLAT", "LoadVar")
        DEGLAT = ParaMESH.DEGLAT(BakFlag)
    End Function

    Public Shared Function DEGLON(Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("DEGLON#", "ParaLOAD.DEGLON", "LoadVar")
        DEGLON = ParaMESH.DEGLON(BakFlag)
    End Function

    Public Shared Function ZRFM(Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("ZRFM#", "ParaLOAD.ZRFM", "LoadVar")
        ZRFM = ParaMESH.ZRFM(BakFlag)
    End Function

    Public Shared Function ZRFH(Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("ZRFH#", "ParaLOAD.ZRFH", "LoadVar")
        ZRFH = ParaMESH.ZRFH(BakFlag)
    End Function

    Public Shared Function ZBLD(Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("ZBLD#", "ParaLOAD.ZBLD", "LoadVar")
        ZBLD = ParaMESH.ZBLD(BakFlag)
    End Function

    Public Shared Function GC(Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("GC#", "ParaLOAD.GC", "LoadVar")
        GC = ParaMESH.GC(BakFlag)
    End Function

    Public Shared Function ILW(Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("ILW#", "ParaLOAD.ILW", "LoadVar")
        ILW = ParaMESH.ILW(BakFlag)
    End Function

    Public Shared Function GRID(Optional ByVal BakFlag As Integer = 0) As Integer
        ParaLOG.AppendFile("GRID#", "ParaLOAD.GRID", "LoadVar")
        GRID = ParaMESH.GRID(BakFlag)
    End Function

    Public Shared Function GRU(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As String
        ParaLOG.AppendFile("GRU# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.GRUDescription", "LoadVar")
        GRU = ParaMESH.GRU(BakFlag, Index)
    End Function

    Public Shared Function FCAN(ByVal ColumnIndex As Integer, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("FCAN# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (Index + 1).ToString & ")", "ParaLOAD.FCAN", "LoadVar")
        FCAN = ParaMESH.FCAN(BakFlag, ColumnIndex, Index)
    End Function

    Public Shared Function LNZ0(ByVal ColumnIndex As Integer, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("LNZ0# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (Index + 1).ToString & ")", "ParaLOAD.LNZ0", "LoadVar")
        LNZ0 = ParaMESH.LNZ0(BakFlag, ColumnIndex, Index)
    End Function

    Public Shared Function ALVC(ByVal ColumnIndex As Integer, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("ALVC# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (Index + 1).ToString & ")", "ParaLOAD.ALVC", "LoadVar")
        ALVC = ParaMESH.ALVC(BakFlag, ColumnIndex, Index)
    End Function

    Public Shared Function ALIC(ByVal ColumnIndex As Integer, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("ALIC# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (Index + 1).ToString & ")", "ParaLOAD.ALIC", "LoadVar")
        ALIC = ParaMESH.ALIC(BakFlag, ColumnIndex, Index)
    End Function

    Public Shared Function LAMX(ByVal ColumnIndex As Integer, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("LAMX# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (Index + 1).ToString & ")", "ParaLOAD.LAMX", "LoadVar")
        LAMX = ParaMESH.LAMX(BakFlag, ColumnIndex, Index)
    End Function

    Public Shared Function LAMN(ByVal ColumnIndex As Integer, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("LAMN# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (Index + 1).ToString & ")", "ParaLOAD.LAMN", "LoadVar")
        LAMN = ParaMESH.LAMN(BakFlag, ColumnIndex, Index)
    End Function

    Public Shared Function CMAS(ByVal ColumnIndex As Integer, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("CMAS# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (Index + 1).ToString & ")", "ParaLOAD.CMAS", "LoadVar")
        CMAS = ParaMESH.CMAS(BakFlag, ColumnIndex, Index)
    End Function

    Public Shared Function ROOT(ByVal ColumnIndex As Integer, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("ROOT# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (Index + 1).ToString & ")", "ParaLOAD.ROOT", "LoadVar")
        ROOT = ParaMESH.ROOT(BakFlag, ColumnIndex, Index)
    End Function

    Public Shared Function RSMN(ByVal ColumnIndex As Integer, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSMN# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSMN", "LoadVar")
        RSMN = ParaMESH.RSMN(BakFlag, ColumnIndex, Index)
    End Function

    Public Shared Function QA50(ByVal ColumnIndex As Integer, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("QA50# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (Index + 1).ToString & ")", "ParaLOAD.QA50", "LoadVar")
        QA50 = ParaMESH.QA50(BakFlag, ColumnIndex, Index)
    End Function

    Public Shared Function VPDA(ByVal ColumnIndex As Integer, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("VPDA# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (Index + 1).ToString & ")", "ParaLOAD.VPDA", "LoadVar")
        VPDA = ParaMESH.VPDA(BakFlag, ColumnIndex, Index)
    End Function

    Public Shared Function VPDB(ByVal ColumnIndex As Integer, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("VPDB# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (Index + 1).ToString & ")", "ParaLOAD.VPDB", "LoadVar")
        VPDB = ParaMESH.VPDB(BakFlag, ColumnIndex, Index)
    End Function

    Public Shared Function PSGA(ByVal ColumnIndex As Integer, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("PSGA# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (Index + 1).ToString & ")", "ParaLOAD.PSGA", "LoadVar")
        PSGA = ParaMESH.PSGA(BakFlag, ColumnIndex, Index)
    End Function

    Public Shared Function PSGB(ByVal ColumnIndex As Integer, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("PSGB# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (Index + 1).ToString & ")", "ParaLOAD.PSGB", "LoadVar")
        PSGB = ParaMESH.PSGB(BakFlag, ColumnIndex, Index)
    End Function

    Public Shared Function DRN(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("DRN# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.DRN", "LoadVar")
        DRN = ParaMESH.DRN(BakFlag, Index)
    End Function

    Public Shared Function SDEP(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("SDEP# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.SDEP", "LoadVar")
        SDEP = ParaMESH.SDEP(BakFlag, Index)
    End Function

    Public Shared Function FARE(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("FARE# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.FARE", "LoadVar")
        FARE = ParaMESH.FARE(BakFlag, Index)
    End Function

    Public Shared Function DDEN(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("DDEN# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.DDEN", "LoadVar")
        DDEN = ParaMESH.DDEN(BakFlag, Index)
    End Function

    Public Shared Function XSLP(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("XSLP# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.XSLP", "LoadVar")
        XSLP = ParaMESH.XSLP(BakFlag, Index)
    End Function

    Public Shared Function GRKF(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("GRKF# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.GRKF", "LoadVar")
        GRKF = ParaMESH.GRKF(BakFlag, Index)
    End Function

    Public Shared Function WFSF(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("WFSF# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.WFSF", "LoadVar")
        WFSF = ParaMESH.WFSF(BakFlag, Index)
    End Function

    Public Shared Function WFCI(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("WFCI# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.WFCI", "LoadVar")
        WFCI = ParaMESH.WFCI(BakFlag, Index)
    End Function

    Public Shared Function RSERVC01(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC01# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC01", "LoadVar")
        RSERVC01 = ParaMESH.RSERVC01(BakFlag, Index)
    End Function

    Public Shared Function RSERVC02(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC02# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC02", "LoadVar")
        RSERVC02 = ParaMESH.RSERVC02(BakFlag, Index)
    End Function

    Public Shared Function RSERVC03(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC03# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC03", "LoadVar")
        RSERVC03 = ParaMESH.RSERVC03(BakFlag, Index)
    End Function

    Public Shared Function RSERVC04(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC04# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC04", "LoadVar")
        RSERVC04 = ParaMESH.RSERVC04(BakFlag, Index)
    End Function

    Public Shared Function RSERVC05(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC05# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC05", "LoadVar")
        RSERVC05 = ParaMESH.RSERVC05(BakFlag, Index)
    End Function

    Public Shared Function RSERVC06(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC06# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC06", "LoadVar")
        RSERVC06 = ParaMESH.RSERVC06(BakFlag, Index)
    End Function

    Public Shared Function RSERVC07(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC07# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC07", "LoadVar")
        RSERVC07 = ParaMESH.RSERVC07(BakFlag, Index)
    End Function

    Public Shared Function RSERVC08(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC08# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC08", "LoadVar")
        RSERVC08 = ParaMESH.RSERVC08(BakFlag, Index)
    End Function

    Public Shared Function RSERVC09(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC09# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC09", "LoadVar")
        RSERVC09 = ParaMESH.RSERVC09(BakFlag, Index)
    End Function

    Public Shared Function RSERVC10(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC10# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC10", "LoadVar")
        RSERVC10 = ParaMESH.RSERVC10(BakFlag, Index)
    End Function

    Public Shared Function RSERVC11(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC11# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC11", "LoadVar")
        RSERVC11 = ParaMESH.RSERVC11(BakFlag, Index)
    End Function

    Public Shared Function RSERVC12(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC12# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC12", "LoadVar")
        RSERVC12 = ParaMESH.RSERVC12(BakFlag, Index)
    End Function

    Public Shared Function SAND(ByVal ColumnIndex As Integer, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("SAND# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (Index + 1).ToString & ")", "ParaLOAD.SAND", "LoadVar")
        SAND = ParaMESH.SAND(BakFlag, ColumnIndex, Index)
    End Function

    Public Shared Function CLAY(ByVal ColumnIndex As Integer, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("CLAY# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (Index + 1).ToString & ")", "ParaLOAD.CLAY", "LoadVar")
        CLAY = ParaMESH.CLAY(BakFlag, ColumnIndex, Index)
    End Function

    Public Shared Function ORGM(ByVal ColumnIndex As Integer, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("ORGM# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (Index + 1).ToString & ")", "ParaLOAD.ORGM", "LoadVar")
        ORGM = ParaMESH.ORGM(BakFlag, ColumnIndex, Index)
    End Function

    Public Shared Function TBAR(ByVal ColumnIndex As Integer, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("TBAR# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (Index + 1).ToString & ")", "ParaLOAD.TBAR", "LoadVar")
        TBAR = ParaMESH.TBAR(BakFlag, ColumnIndex, Index)
    End Function

    Public Shared Function TCAN(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("TCAN# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.TCAN", "LoadVar")
        TCAN = ParaMESH.TCAN(BakFlag, Index)
    End Function

    Public Shared Function TSNO(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("TSNO# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.TSNO", "LoadVar")
        TSNO = ParaMESH.TSNO(BakFlag, Index)
    End Function

    Public Shared Function TPND(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("TPND# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.TPND", "LoadVar")
        TPND = ParaMESH.TPND(BakFlag, Index)
    End Function

    Public Shared Function THLQ(ByVal ColumnIndex As Integer, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("THLQ# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (Index + 1).ToString & ")", "ParaLOAD.THLQ", "LoadVar")
        THLQ = ParaMESH.THLQ(BakFlag, ColumnIndex, Index)
    End Function

    Public Shared Function THIC(ByVal ColumnIndex As Integer, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("THIC# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (Index + 1).ToString & ")", "ParaLOAD.THIC", "LoadVar")
        THIC = ParaMESH.THIC(BakFlag, ColumnIndex, Index)
    End Function

    Public Shared Function ZPND(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("ZPND# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.ZPND", "LoadVar")
        ZPND = ParaMESH.ZPND(BakFlag, Index)
    End Function

    Public Shared Function RCAN(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RCAN# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RCAN", "LoadVar")
        RCAN = ParaMESH.RCAN(BakFlag, Index)
    End Function

    Public Shared Function SCAN(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("SCAN# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.SCAN", "LoadVar")
        SCAN = ParaMESH.SCAN(BakFlag, Index)
    End Function

    Public Shared Function SNO(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("SNO# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.SNO", "LoadVar")
        SNO = ParaMESH.SNO(BakFlag, Index)
    End Function

    Public Shared Function ALBS(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("ALBS# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.ALBS", "LoadVar")
        ALBS = ParaMESH.ALBS(BakFlag, Index)
    End Function

    Public Shared Function RHOS(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RHOS# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RHOS", "LoadVar")
        RHOS = ParaMESH.RHOS(BakFlag, Index)
    End Function

    Public Shared Function GRO(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("GRO# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.GRO", "LoadVar")
        GRO = ParaMESH.GRO(BakFlag, Index)
    End Function

    Public Shared Function RSERVC13(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC13# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC13", "LoadVar")
        RSERVC13 = ParaMESH.RSERVC13(BakFlag, Index)
    End Function

    Public Shared Function RSERVC14(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC14# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC14", "LoadVar")
        RSERVC14 = ParaMESH.RSERVC14(BakFlag, Index)
    End Function

    Public Shared Function RSERVC15(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC15# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC15", "LoadVar")
        RSERVC15 = ParaMESH.RSERVC15(BakFlag, Index)
    End Function

    Public Shared Function RSERVC16(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC16# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC16", "LoadVar")
        RSERVC16 = ParaMESH.RSERVC16(BakFlag, Index)
    End Function

    Public Shared Function RSERVC17(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC17# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC17", "LoadVar")
        RSERVC17 = ParaMESH.RSERVC17(BakFlag, Index)
    End Function

    Public Shared Function RSERVC18(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC18# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC18", "LoadVar")
        RSERVC18 = ParaMESH.RSERVC18(BakFlag, Index)
    End Function

    Public Shared Function RSERVC19(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC19# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC19", "LoadVar")
        RSERVC19 = ParaMESH.RSERVC19(BakFlag, Index)
    End Function

    Public Shared Function RSERVC20(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC20# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC20", "LoadVar")
        RSERVC20 = ParaMESH.RSERVC20(BakFlag, Index)
    End Function

    Public Shared Function RSERVC21(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC21# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC21", "LoadVar")
        RSERVC21 = ParaMESH.RSERVC21(BakFlag, Index)
    End Function

    Public Shared Function RSERVC22(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC22# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC22", "LoadVar")
        RSERVC22 = ParaMESH.RSERVC22(BakFlag, Index)
    End Function

    Public Shared Function RSERVC23(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC23# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC23", "LoadVar")
        RSERVC23 = ParaMESH.RSERVC23(BakFlag, Index)
    End Function

    Public Shared Function RSERVC24(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC24# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC24", "LoadVar")
        RSERVC24 = ParaMESH.RSERVC24(BakFlag, Index)
    End Function

    Public Shared Function RSERVC25(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC25# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC25", "LoadVar")
        RSERVC25 = ParaMESH.RSERVC25(BakFlag, Index)
    End Function

    Public Shared Function RSERVC26(ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0) As Double
        ParaLOG.AppendFile("RSERVC26# (GRU " & (Index + 1).ToString & ")", "ParaLOAD.RSERVC26", "LoadVar")
        RSERVC26 = ParaMESH.RSERVC26(BakFlag, Index)
    End Function

    Public Shared Function HourlyStartDay(Optional ByVal BakFlag As Integer = 0) As Integer
        ParaLOG.AppendFile("HourlyStartDay#", "ParaLOAD.HourlyStartDay", "LoadVar")
        HourlyStartDay = ParaMESH.HourlyStartDay(BakFlag)
    End Function

    Public Shared Function HourlyStartYear(Optional ByVal BakFlag As Integer = 0) As Integer
        ParaLOG.AppendFile("HourlyStartYear#", "ParaLOAD.HourlyStartYear", "LoadVar")
        HourlyStartYear = ParaMESH.HourlyStartYear(BakFlag)
    End Function

    Public Shared Function HourlyStopDay(Optional ByVal BakFlag As Integer = 0) As Integer
        ParaLOG.AppendFile("HourlyStopDay#", "ParaLOAD.HourlyStopDay", "LoadVar")
        HourlyStopDay = ParaMESH.HourlyStopDay(BakFlag)
    End Function

    Public Shared Function HourlyStopYear(Optional ByVal BakFlag As Integer = 0) As Integer
        ParaLOG.AppendFile("HourlyStopYear#", "ParaLOAD.HourlyStopYear", "LoadVar")
        HourlyStopYear = ParaMESH.HourlyStopYear(BakFlag)
    End Function

    Public Shared Function DailyStartDay(Optional ByVal BakFlag As Integer = 0) As Integer
        ParaLOG.AppendFile("DailyStartDay#", "ParaLOAD.DailyStartDay", "LoadVar")
        DailyStartDay = ParaMESH.DailyStartDay(BakFlag)
    End Function

    Public Shared Function DailyStartYear(Optional ByVal BakFlag As Integer = 0) As Integer
        ParaLOG.AppendFile("DailyStartYear#", "ParaLOAD.DailyStartYear", "LoadVar")
        DailyStartYear = ParaMESH.DailyStartYear(BakFlag)
    End Function

    Public Shared Function DailyStopDay(Optional ByVal BakFlag As Integer = 0) As Integer
        ParaLOG.AppendFile("DailyStopDay#", "ParaLOAD.DailyStopDay", "LoadVar")
        DailyStopDay = ParaMESH.DailyStopDay(BakFlag)
    End Function

    Public Shared Function DailyStopYear(Optional ByVal BakFlag As Integer = 0) As Integer
        ParaLOG.AppendFile("DailyStopYear#", "ParaLOAD.DailyStopYear", "LoadVar")
        DailyStopYear = ParaMESH.DailyStopYear(BakFlag)
    End Function

    Public Shared Function SimStartDay(Optional ByVal BakFlag As Integer = 0) As Integer
        ParaLOG.AppendFile("SimStartDay#", "ParaLOAD.SimStartDay", "LoadVar")
        SimStartDay = ParaMESH.SimStartDay(BakFlag)
    End Function

    Public Shared Function SimStartYear(Optional ByVal BakFlag As Integer = 0) As Integer
        ParaLOG.AppendFile("SimStartYear#", "ParaLOAD.SimStartYear", "LoadVar")
        SimStartYear = ParaMESH.SimStartYear(BakFlag)
    End Function

    Public Shared Function SimStopDay(Optional ByVal BakFlag As Integer = 0) As Integer
        ParaLOG.AppendFile("SimStopDay#", "ParaLOAD.SimStopDay", "LoadVar")
        SimStopDay = ParaMESH.SimStopDay(BakFlag)
    End Function

    Public Shared Function SimStopYear(Optional ByVal BakFlag As Integer = 0) As Integer
        ParaLOG.AppendFile("SimStopYear#", "ParaLOAD.SimStopYear", "LoadVar")
        SimStopYear = ParaMESH.SimStopYear(BakFlag)
    End Function

    Public Shared Function MetStartMin(Optional ByVal BakFlag As Integer = 0) As Integer
        ParaLOG.AppendFile("MetStartMin#", "ParaLOAD.MetStartMin", "LoadVar")
        MetStartMin = ParaMESH.MetStartMin(BakFlag)
    End Function

    Public Shared Function MetStartHour(Optional ByVal BakFlag As Integer = 0) As Integer
        ParaLOG.AppendFile("MetStartHour#", "ParaLOAD.MetStartHour", "LoadVar")
        MetStartHour = ParaMESH.MetStartHour(BakFlag)
    End Function

    Public Shared Function MetStartDay(Optional ByVal BakFlag As Integer = 0) As Integer
        ParaLOG.AppendFile("MetStartDay#", "ParaLOAD.MetStartDay", "LoadVar")
        MetStartDay = ParaMESH.MetStartDay(BakFlag)
    End Function

    Public Shared Function MetStartYear(Optional ByVal BakFlag As Integer = 0) As Integer
        ParaLOG.AppendFile("MetStartYear#", "ParaLOAD.MetStartYear", "LoadVar")
        MetStartYear = ParaMESH.MetStartYear(BakFlag)
    End Function
End Class
