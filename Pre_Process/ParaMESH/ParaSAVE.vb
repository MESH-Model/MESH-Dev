Public Class ParaSAVE

    '*****************************************************************************
    'CLASS: ParaSAVE
    '*****************************************************************************
    'ParaSAVE is used to save parameter values.  Using this class eliminates the 
    'need for other classes and ParaMESH forms to directly modifying any 
    'parameter value, such that saving parameter values becomes a controlled 
    'and uniform process.
    '*****************************************************************************

    '****************************************************************************
    'PRIVATE ParaSAVE PARAMETERS
    '****************************************************************************
    'Parameters shared and used only within the ParaSAVE class.
    '****************************************************************************
    'Private Integers: z is unique to the ParaSAVE class so that similarly 
    'private integers from calling subroutines are not reset.
    '****************************************************************************

    Private Shared z As Integer

    Public Shared Sub FileProperties(ByVal Value As String, ByVal ColumnIndex As Integer, ByVal LineIndex As Integer)
        ParaLOG.AppendFile("FileProperties# (Line " & (LineIndex + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ")", "ParaSAVE.FileProperties", "SavVar")
        ParaMESH.FileProperties(ColumnIndex, LineIndex) = Value
    End Sub

    Public Shared Sub SystemParameters(ByVal Value As String, ByVal SystemParameter As String)
        ParaLOG.AppendFile("ParaMESH is saving the system parameter: ParaMESH.SystemParameters(" & SystemParameter & ")", "ParaSAVE.SystemParameters")
        ParaMESH.SystemParameters(1, ParaMESH.FindIndex(SystemParameter, ParaMESH.SystemParameters)) = Value
    End Sub

    Public Shared Sub SpecialChecks(ByVal Value As String, ByVal SpecialCheck As String)
        ParaLOG.AppendFile("ParaMESH is saving the special check: ParaMESH.SpecialChecks(" & SpecialCheck & ")", "ParaSAVE.SpecialChecks")
        ParaMESH.SpecialChecks(1, ParaMESH.FindIndex(SpecialCheck, ParaMESH.SpecialChecks)) = Value
    End Sub

    Public Shared Sub SystemProperties(ByVal Value As String, ByVal SystemProperty As String)
        ParaLOG.AppendFile("ParaMESH is saving the system property: ParaMESH.SystemProperties(" & SystemProperty & ")", "ParaSAVE.SystemProperties")
        ParaMESH.SystemProperties(1, ParaMESH.FindIndex(SystemProperty, ParaMESH.SystemProperties)) = Value
    End Sub

    Public Shared Sub PrimaryParameters(ByVal Value As String, ByVal ParameterName As String, ByVal ColumnIndex As Integer)
        ParaLOG.AppendFile(ParameterName & "# ..Parameter Information (Column " & ColumnIndex + 1 & ")", "ParaSAVE.PrimaryParameters", "SavVar")
        ParaMESH.PrimaryParameters(ColumnIndex, ParaMESH.FindIndex(ParameterName, ParaMESH.PrimaryParameters)) = Value

        '***ENABLE OR DISABLE ANY SECONDARY PARAMETERS ASSOCIATED WITH ParameterName
        If ColumnIndex = 2 And Convert.ToBoolean(ParaMESH.PrimaryParameters(1, ParaMESH.FindIndex(ParameterName, ParaMESH.PrimaryParameters))) = True Then
            For z = 0 To ParaMESH.SecondaryParameters.GetUpperBound(1)
                If ParaMESH.SecondaryParameters(0, z) = ParameterName Then
                    ParaMESH.SecondaryParameters(ColumnIndex, z) = Value
                End If
            Next
        End If
    End Sub

    Public Shared Sub SecondaryParameters(ByVal Value As String, ByVal ParameterName As String, ByVal ColumnIndex As Integer)
        ParaLOG.AppendFile(ParameterName & "# ..Parameter Information (Column " & ColumnIndex + 1 & ")", "ParaSAVE.SecondaryParameters", "SavVar")
        ParaMESH.SecondaryParameters(ColumnIndex, ParaMESH.FindIndex(ParameterName, ParaMESH.SecondaryParameters)) = Value
    End Sub

    Public Shared Sub PathRoot(ByVal Value As String)
        ParaLOG.AppendFile("ParaMESH is setting the working directory: " & Value, "ParaSAVE.PathRoot")
        ParaMESH.PathRoot = Value
    End Sub

    Public Shared Sub DirectoryList(Optional ByVal ClearList As Boolean = False)
        If ClearList = True Then

            '***CLEAR ParaMESH.DirectoryList, SET TO CURRENT WORKING DIRECTORY (IF EXISTS)
            ParaLOG.AppendFile("ParaMESH is clearing the working directory list", "ParaSAVE.DirectoryList")
            ParaSAVE.SystemProperties("", "DirectoryList")

            '***SAVE ParaMESH(PathRoot) IF EXISTS
            If ParaMESH.PathRoot <> Nothing Then
                ParaSAVE.SystemProperties(ParaMESH.PathRoot, "DirectoryList")
            End If
            Exit Sub
        End If

        '***SAVE FullPathList TO ParaMESH.SystemProperties(DirectoryList)
        ParaLOG.AppendFile("ParaMESH is saving the working directory list: ParaMESH.SystemProperties(DirectoryList)", "ParaSAVE.DirectoryList")
        ParaMESH.SystemProperties(1, ParaMESH.FindIndex("DirectoryList", ParaMESH.SystemProperties)) = ""
        For z = 0 To ParaMESH.DirectoryList.GetUpperBound(0)
            If z > 0 Then
                ParaMESH.SystemProperties(1, ParaMESH.FindIndex("DirectoryList", ParaMESH.SystemProperties)) &= ">"
            End If
            ParaMESH.SystemProperties(1, ParaMESH.FindIndex("DirectoryList", ParaMESH.SystemProperties)) &= ParaMESH.DirectoryList(z)
        Next
    End Sub

    Public Shared Sub ControlFlag(ByVal ParameterName As String, ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile(ParameterName & "# (Line " & (ParaMESH.SecondaryParameters(10, ParaMESH.FindIndex(ParameterName, ParaMESH.SecondaryParameters)) + 1).ToString & " of " & ParaMESH.PrimaryParameters(3, ParaMESH.FindIndex("ControlFlag", ParaMESH.PrimaryParameters)) & ")", "ParaSAVE.ControlFlag", "SavVar")
        ParaMESH.ControlFlag(BakFlag, ParaMESH.SecondaryParameters(10, ParaMESH.FindIndex(ParameterName, ParaMESH.SecondaryParameters))) = ParaMESH.CheckValue(Value, ParameterName)
    End Sub

    Public Shared Sub GridOutput(ByVal ParameterName As String, ByVal Value As Object, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile(ParameterName & "# (Line " & (ParaMESH.SecondaryParameters(10, ParaMESH.FindIndex(ParameterName, ParaMESH.SecondaryParameters)) + 1).ToString & ", Grid Output " & (Index + 1).ToString & " of " & ParaMESH.PrimaryParameters(3, ParaMESH.FindIndex("GridOutput", ParaMESH.PrimaryParameters)) & ")", "ParaSAVE.GridOutput", "SavVar")
        ParaMESH.GridOutput(BakFlag, ParaMESH.SecondaryParameters(10, ParaMESH.FindIndex(ParameterName, ParaMESH.SecondaryParameters)), Index) = ParaMESH.CheckValue(Value, ParameterName)
    End Sub

    Public Shared Sub OptionFlag(ByVal ParameterName As String, ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile(ParameterName & "# (Line " & (ParaMESH.SecondaryParameters(10, ParaMESH.FindIndex(ParameterName, ParaMESH.SecondaryParameters)) + 1).ToString & " of " & ParaMESH.PrimaryParameters(3, ParaMESH.FindIndex("OptionFlag", ParaMESH.PrimaryParameters)) & ")", "ParaSAVE.OptionFlag", "SavVar")
        ParaMESH.OptionFlag(BakFlag, ParaMESH.SecondaryParameters(10, ParaMESH.FindIndex(ParameterName, ParaMESH.SecondaryParameters))) = ParaMESH.CheckValue(Value, ParameterName)
    End Sub

    Public Shared Sub IndependentGRU(ByVal ParameterName As String, ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile(ParameterName & "# (Line " & (ParaMESH.SecondaryParameters(10, ParaMESH.FindIndex(ParameterName, ParaMESH.SecondaryParameters)) + 1).ToString & " of " & ParaMESH.PrimaryParameters(3, ParaMESH.FindIndex("IndependentGRU", ParaMESH.PrimaryParameters)) & ")", "ParaSAVE.IndependentGRU", "SavVar")
        ParaMESH.IndependentGRU(BakFlag, ParaMESH.SecondaryParameters(10, ParaMESH.FindIndex(ParameterName, ParaMESH.SecondaryParameters))) = ParaMESH.CheckValue(Value, ParameterName)
    End Sub

    Public Shared Sub DependentGRU(ByVal ParameterName As String, ByVal Value As Object, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile(ParameterName & "# (Line " & (ParaMESH.SecondaryParameters(10, ParaMESH.FindIndex(ParameterName, ParaMESH.SecondaryParameters)) + 1).ToString & ", GRU " & (Index + 1).ToString & " of " & ParaMESH.PrimaryParameters(3, ParaMESH.FindIndex("DependentGRU", ParaMESH.PrimaryParameters)) & ")", "ParaSAVE.DependentGRU", "SavVar")
        ParaMESH.DependentGRU(BakFlag, ParaMESH.SecondaryParameters(10, ParaMESH.FindIndex(ParameterName, ParaMESH.SecondaryParameters)), Index) = ParaMESH.CheckValue(Value, ParameterName)
    End Sub

    Public Shared Sub WatflowFlag(ByVal ParameterName As String, ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile(ParameterName & "# (Line " & (ParaMESH.SecondaryParameters(10, ParaMESH.FindIndex(ParameterName, ParaMESH.SecondaryParameters)) + 1).ToString & " of " & ParaMESH.PrimaryParameters(3, ParaMESH.FindIndex("WatflowFlag", ParaMESH.PrimaryParameters)) & ")", "ParaSAVE.WatflowFlag", "SavVar")
        ParaMESH.WatflowFlag(BakFlag, ParaMESH.SecondaryParameters(10, ParaMESH.FindIndex(ParameterName, ParaMESH.SecondaryParameters))) = ParaMESH.CheckValue(Value, ParameterName)
    End Sub

    Public Shared Sub WF_R2(ByVal Value As Object, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("WF_R2# (Column " & (Index + 1).ToString, "ParaSAVE.WF_R2", "SavVar")
        ParaMESH.WF_R2(BakFlag, Index) = ParaMESH.CheckValue(Value, "WF_R2")
    End Sub

    Public Shared Sub BasinID(ByVal Value As String, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("BasinID#", "ParaSAVE.BasinID", "SavVar")
        ParaMESH.BasinID(BakFlag) = ParaMESH.CheckValue(Value, "BasinID")
    End Sub

    Public Shared Sub ScalingFactor(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("ScalingFactor#", "ParaSAVE.ScalingFactor", "SavVar")
        ParaMESH.ScalingFactor(BakFlag) = ParaMESH.CheckValue(Value, "ScalingFactor")
    End Sub

    Public Shared Sub ZPLIMG0(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("ZPLIMG0#", "ParaSAVE.ZPLIMG0", "SavVar")
        ParaMESH.ZPLIMG0(BakFlag) = ParaMESH.CheckValue(Value, "ZPLIMG0")
    End Sub

    Public Shared Sub ZPLIMS0(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("ZPLIMS0#", "ParaSAVE.ZPLIMS0", "SavVar")
        ParaMESH.ZPLIMS0(BakFlag) = ParaMESH.CheckValue(Value, "ZPLIMS0")
    End Sub

    Public Shared Sub ZPLIMP0(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("ZPLIMP0#", "ParaSAVE.ZPLIMP0", "SavVar")
        ParaMESH.ZPLIMP0(BakFlag) = ParaMESH.CheckValue(Value, "ZPLIMP0")
    End Sub

    Public Shared Sub ZPLIMPS0(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("ZPLIMPS0#", "ParaSAVE.ZPLIMPS0", "SavVar")
        ParaMESH.ZPLIMPS0(BakFlag) = ParaMESH.CheckValue(Value, "ZPLIMPS0")
    End Sub

    Public Shared Sub D100A(ByVal Value As Object, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("D100A# (Column " & (Index + 1).ToString, "ParaSAVE.D100A", "SavVar")
        ParaMESH.D100A(BakFlag, Index) = ParaMESH.CheckValue(Value, "D100A")
    End Sub

    Public Shared Sub GWINIT(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("GWINIT#", "ParaSAVE.GWINIT", "SavVar")
        ParaMESH.GWINIT(BakFlag) = ParaMESH.CheckValue(Value, "GWINIT")
    End Sub

    Public Shared Sub TITLE(ByVal Value As String, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("TITLE#", "ParaSAVE.TITLE", "SavVar")
        ParaMESH.TITLE(BakFlag) = ParaMESH.CheckValue(Value, "TITLE")
    End Sub

    Public Shared Sub NAME(ByVal Value As String, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("NAME#", "ParaSAVE.NAME", "SavVar")
        ParaMESH.NAME(BakFlag) = ParaMESH.CheckValue(Value, "NAME")
    End Sub

    Public Shared Sub PLACE(ByVal Value As String, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("PLACE#", "ParaSAVE.PLACE", "SavVar")
        ParaMESH.PLACE(BakFlag) = ParaMESH.CheckValue(Value, "PLACE")
    End Sub

    Public Shared Sub DEGLAT(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("DEGLAT#", "ParaSAVE.DEGLAT", "SavVar")
        ParaMESH.DEGLAT(BakFlag) = ParaMESH.CheckValue(Value, "DEGLAT")
    End Sub

    Public Shared Sub DEGLON(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("DEGLON#", "ParaSAVE.DEGLON", "SavVar")
        ParaMESH.DEGLON(BakFlag) = ParaMESH.CheckValue(Value, "DEGLON")
    End Sub

    Public Shared Sub ZRFM(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("ZRFM#", "ParaSAVE.ZRFM", "SavVar")
        ParaMESH.ZRFM(BakFlag) = ParaMESH.CheckValue(Value, "ZRFM")
    End Sub

    Public Shared Sub ZRFH(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("ZRFH#", "ParaSAVE.ZRFH", "SavVar")
        ParaMESH.ZRFH(BakFlag) = ParaMESH.CheckValue(Value, "ZRFH")
    End Sub

    Public Shared Sub ZBLD(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("ZBLD#", "ParaSAVE.ZBLD", "SavVar")
        ParaMESH.ZBLD(BakFlag) = ParaMESH.CheckValue(Value, "ZBLD")
    End Sub

    Public Shared Sub GC(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("GC#", "ParaSAVE.GC", "SavVar")
        ParaMESH.GC(BakFlag) = ParaMESH.CheckValue(Value, "GC")
    End Sub

    Public Shared Sub ILW(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("ILW#", "ParaSAVE.ILW", "SavVar")
        ParaMESH.ILW(BakFlag) = ParaMESH.CheckValue(Value, "ILW")
    End Sub

    Public Shared Sub GRID(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("GRID#", "ParaSAVE.GRID", "SavVar")
        ParaMESH.GRID(BakFlag) = ParaMESH.CheckValue(Value, "GRID")
    End Sub

    Public Shared Sub GRU(ByVal Value As String, ByVal Index As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("GRU# (GRU " & (Index + 1).ToString & ")", "ParaSAVE.GRU", "SavVar")
        ParaMESH.GRU(BakFlag, Index) = ParaMESH.CheckValue(Value, "GRU")
    End Sub

    Public Shared Sub FCAN(ByVal Value As Object, ByVal ColumnIndex As Integer, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("FCAN# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (GRU + 1).ToString & ")", "ParaSAVE.FCAN", "SavVar")
        ParaMESH.FCAN(BakFlag, ColumnIndex, GRU) = ParaMESH.CheckValue(Value, "FCAN")
    End Sub

    Public Shared Sub LNZ0(ByVal Value As Object, ByVal ColumnIndex As Integer, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("LNZ0# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (GRU + 1).ToString & ")", "ParaSAVE.LNZ0", "SavVar")
        ParaMESH.LNZ0(BakFlag, ColumnIndex, GRU) = ParaMESH.CheckValue(Value, "LNZ0")
    End Sub

    Public Shared Sub ALVC(ByVal Value As Object, ByVal ColumnIndex As Integer, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("ALVC# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (GRU + 1).ToString & ")", "ParaSAVE.ALVC", "SavVar")
        ParaMESH.ALVC(BakFlag, ColumnIndex, GRU) = ParaMESH.CheckValue(Value, "ALVC")
    End Sub

    Public Shared Sub ALIC(ByVal Value As Object, ByVal ColumnIndex As Integer, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("ALIC# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (GRU + 1).ToString & ")", "ParaSAVE.ALIC", "SavVar")
        ParaMESH.ALIC(BakFlag, ColumnIndex, GRU) = ParaMESH.CheckValue(Value, "ALIC")
    End Sub

    Public Shared Sub LAMX(ByVal Value As Object, ByVal ColumnIndex As Integer, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("LAMX# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (GRU + 1).ToString & ")", "ParaSAVE.LAMX", "SavVar")
        ParaMESH.LAMX(BakFlag, ColumnIndex, GRU) = ParaMESH.CheckValue(Value, "LAMX")
    End Sub

    Public Shared Sub LAMN(ByVal Value As Object, ByVal ColumnIndex As Integer, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("LAMN# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (GRU + 1).ToString & ")", "ParaSAVE.LAMN", "SavVar")
        ParaMESH.LAMN(BakFlag, ColumnIndex, GRU) = ParaMESH.CheckValue(Value, "LAMN")
    End Sub

    Public Shared Sub CMAS(ByVal Value As Object, ByVal ColumnIndex As Integer, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("CMAS# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (GRU + 1).ToString & ")", "ParaSAVE.CMAS", "SavVar")
        ParaMESH.CMAS(BakFlag, ColumnIndex, GRU) = ParaMESH.CheckValue(Value, "CMAS")
    End Sub

    Public Shared Sub ROOT(ByVal Value As Object, ByVal ColumnIndex As Integer, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("ROOT# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (GRU + 1).ToString & ")", "ParaSAVE.ROOT", "SavVar")
        ParaMESH.ROOT(BakFlag, ColumnIndex, GRU) = ParaMESH.CheckValue(Value, "ROOT")
    End Sub

    Public Shared Sub RSMN(ByVal Value As Object, ByVal ColumnIndex As Integer, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSMN# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSMN", "SavVar")
        ParaMESH.RSMN(BakFlag, ColumnIndex, GRU) = ParaMESH.CheckValue(Value, "RSMN")
    End Sub

    Public Shared Sub QA50(ByVal Value As Object, ByVal ColumnIndex As Integer, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("QA50# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (GRU + 1).ToString & ")", "ParaSAVE.QA50", "SavVar")
        ParaMESH.QA50(BakFlag, ColumnIndex, GRU) = ParaMESH.CheckValue(Value, "QA50")
    End Sub

    Public Shared Sub VPDA(ByVal Value As Object, ByVal ColumnIndex As Integer, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("VPDA# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (GRU + 1).ToString & ")", "ParaSAVE.VPDA", "SavVar")
        ParaMESH.VPDA(BakFlag, ColumnIndex, GRU) = ParaMESH.CheckValue(Value, "VPDA")
    End Sub

    Public Shared Sub VPDB(ByVal Value As Object, ByVal ColumnIndex As Integer, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("VPDB# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (GRU + 1).ToString & ")", "ParaSAVE.VPDB", "SavVar")
        ParaMESH.VPDB(BakFlag, ColumnIndex, GRU) = ParaMESH.CheckValue(Value, "VPDB")
    End Sub

    Public Shared Sub PSGA(ByVal Value As Object, ByVal ColumnIndex As Integer, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("PSGA# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (GRU + 1).ToString & ")", "ParaSAVE.PSGA", "SavVar")
        ParaMESH.PSGA(BakFlag, ColumnIndex, GRU) = ParaMESH.CheckValue(Value, "PSGA")
    End Sub

    Public Shared Sub PSGB(ByVal Value As Object, ByVal ColumnIndex As Integer, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("PSGB# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (GRU + 1).ToString & ")", "ParaSAVE.PSGB", "SavVar")
        ParaMESH.PSGB(BakFlag, ColumnIndex, GRU) = ParaMESH.CheckValue(Value, "PSGB")
    End Sub

    Public Shared Sub DRN(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("DRN# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.DRN", "SavVar")
        ParaMESH.DRN(BakFlag, GRU) = ParaMESH.CheckValue(Value, "DRN")
    End Sub

    Public Shared Sub SDEP(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("SDEP# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.SDEP", "SavVar")
        ParaMESH.SDEP(BakFlag, GRU) = ParaMESH.CheckValue(Value, "SDEP")
    End Sub

    Public Shared Sub FARE(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("FARE# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.FARE", "SavVar")
        ParaMESH.FARE(BakFlag, GRU) = ParaMESH.CheckValue(Value, "FARE")
    End Sub

    Public Shared Sub DDEN(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("DDEN# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.DDEN", "SavVar")
        ParaMESH.DDEN(BakFlag, GRU) = ParaMESH.CheckValue(Value, "DDEN")
    End Sub

    Public Shared Sub XSLP(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("XSLP# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.XSLP", "SavVar")
        ParaMESH.XSLP(BakFlag, GRU) = ParaMESH.CheckValue(Value, "XSLP")
    End Sub

    Public Shared Sub GRKF(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("GRKF# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.GRKF", "SavVar")
        ParaMESH.GRKF(BakFlag, GRU) = ParaMESH.CheckValue(Value, "GRKF")
    End Sub

    Public Shared Sub WFSF(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("WFSF# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.WFSF", "SavVar")
        ParaMESH.WFSF(BakFlag, GRU) = ParaMESH.CheckValue(Value, "WFSF")
    End Sub

    Public Shared Sub WFCI(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("WFCI# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.WFCI", "SavVar")
        ParaMESH.WFCI(BakFlag, GRU) = ParaMESH.CheckValue(Value, "WFCI")
    End Sub

    Public Shared Sub RSERVC01(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC01# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC01", "SavVar")
        ParaMESH.RSERVC01(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC01")
    End Sub

    Public Shared Sub RSERVC02(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC02# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC02", "SavVar")
        ParaMESH.RSERVC02(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC02")
    End Sub

    Public Shared Sub RSERVC03(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC03# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC03", "SavVar")
        ParaMESH.RSERVC03(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC03")
    End Sub

    Public Shared Sub RSERVC04(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC04# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC04", "SavVar")
        ParaMESH.RSERVC04(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC04")
    End Sub

    Public Shared Sub RSERVC05(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC05# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC05", "SavVar")
        ParaMESH.RSERVC05(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC05")
    End Sub

    Public Shared Sub RSERVC06(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC06# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC06", "SavVar")
        ParaMESH.RSERVC06(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC06")
    End Sub

    Public Shared Sub RSERVC07(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC07# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC07", "SavVar")
        ParaMESH.RSERVC07(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC07")
    End Sub

    Public Shared Sub RSERVC08(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC08# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC08", "SavVar")
        ParaMESH.RSERVC08(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC08")
    End Sub

    Public Shared Sub RSERVC09(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC09# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC09", "SavVar")
        ParaMESH.RSERVC09(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC09")
    End Sub

    Public Shared Sub RSERVC10(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC10# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC10", "SavVar")
        ParaMESH.RSERVC10(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC10")
    End Sub

    Public Shared Sub RSERVC11(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC11# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC11", "SavVar")
        ParaMESH.RSERVC11(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC11")
    End Sub

    Public Shared Sub RSERVC12(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC12# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC12", "SavVar")
        ParaMESH.RSERVC12(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC12")
    End Sub

    Public Shared Sub SAND(ByVal Value As Object, ByVal ColumnIndex As Integer, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("SAND# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (GRU + 1).ToString & ")", "ParaSAVE.SAND", "SavVar")
        ParaMESH.SAND(BakFlag, ColumnIndex, GRU) = ParaMESH.CheckValue(Value, "SAND")
    End Sub

    Public Shared Sub CLAY(ByVal Value As Object, ByVal ColumnIndex As Integer, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("CLAY# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (GRU + 1).ToString & ")", "ParaSAVE.CLAY", "SavVar")
        ParaMESH.CLAY(BakFlag, ColumnIndex, GRU) = ParaMESH.CheckValue(Value, "CLAY")
    End Sub

    Public Shared Sub ORGM(ByVal Value As Object, ByVal ColumnIndex As Integer, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("ORGM# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (GRU + 1).ToString & ")", "ParaSAVE.ORGM", "SavVar")
        ParaMESH.ORGM(BakFlag, ColumnIndex, GRU) = ParaMESH.CheckValue(Value, "ORGM")
    End Sub

    Public Shared Sub TBAR(ByVal Value As Object, ByVal ColumnIndex As Integer, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("TBAR# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (GRU + 1).ToString & ")", "ParaSAVE.TBAR", "SavVar")
        ParaMESH.TBAR(BakFlag, ColumnIndex, GRU) = ParaMESH.CheckValue(Value, "TBAR")
    End Sub

    Public Shared Sub TCAN(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("TCAN# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.TCAN", "SavVar")
        ParaMESH.TCAN(BakFlag, GRU) = ParaMESH.CheckValue(Value, "TCAN")
    End Sub

    Public Shared Sub TSNO(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("TSNO# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.TSNO", "SavVar")
        ParaMESH.TSNO(BakFlag, GRU) = ParaMESH.CheckValue(Value, "TSNO")
    End Sub

    Public Shared Sub TPND(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("TPND# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.TPND", "SavVar")
        ParaMESH.TPND(BakFlag, GRU) = ParaMESH.CheckValue(Value, "TPND")
    End Sub

    Public Shared Sub ZPND(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("ZPND# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.ZPND", "SavVar")
        ParaMESH.ZPND(BakFlag, GRU) = ParaMESH.CheckValue(Value, "ZPND")
    End Sub

    Public Shared Sub THLQ(ByVal Value As Object, ByVal ColumnIndex As Integer, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("THLQ# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (GRU + 1).ToString & ")", "ParaSAVE.THLQ", "SavVar")
        ParaMESH.THLQ(BakFlag, ColumnIndex, GRU) = ParaMESH.CheckValue(Value, "THLQ")
    End Sub

    Public Shared Sub THIC(ByVal Value As Object, ByVal ColumnIndex As Integer, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("THIC# (Column " & (ColumnIndex + 1).ToString & ", GRU " & (GRU + 1).ToString & ")", "ParaSAVE.THIC", "SavVar")
        ParaMESH.THIC(BakFlag, ColumnIndex, GRU) = ParaMESH.CheckValue(Value, "THIC")
    End Sub

    Public Shared Sub RCAN(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RCAN# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RCAN", "SavVar")
        ParaMESH.RCAN(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RCAN")
    End Sub

    Public Shared Sub SCAN(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("SCAN# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.SCAN", "SavVar")
        ParaMESH.SCAN(BakFlag, GRU) = ParaMESH.CheckValue(Value, "SCAN")
    End Sub

    Public Shared Sub SNO(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("SNO# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.SNO", "SavVar")
        ParaMESH.SNO(BakFlag, GRU) = ParaMESH.CheckValue(Value, "SNO")
    End Sub

    Public Shared Sub ALBS(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("ALBS# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.ALBS", "SavVar")
        ParaMESH.ALBS(BakFlag, GRU) = ParaMESH.CheckValue(Value, "ALBS")
    End Sub

    Public Shared Sub RHOS(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RHOS# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RHOS", "SavVar")
        ParaMESH.RHOS(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RHOS")
    End Sub

    Public Shared Sub GRO(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("GRO# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.GRO", "SavVar")
        ParaMESH.GRO(BakFlag, GRU) = ParaMESH.CheckValue(Value, "GRO")
    End Sub

    Public Shared Sub RSERVC13(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC13# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC13", "SavVar")
        ParaMESH.RSERVC13(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC13")
    End Sub

    Public Shared Sub RSERVC14(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC14# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC14", "SavVar")
        ParaMESH.RSERVC14(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC14")
    End Sub

    Public Shared Sub RSERVC15(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC15# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC15", "SavVar")
        ParaMESH.RSERVC15(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC15")
    End Sub

    Public Shared Sub RSERVC16(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC16# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC16", "SavVar")
        ParaMESH.RSERVC16(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC16")
    End Sub

    Public Shared Sub RSERVC17(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC17# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC17", "SavVar")
        ParaMESH.RSERVC17(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC17")
    End Sub

    Public Shared Sub RSERVC18(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC18# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC18", "SavVar")
        ParaMESH.RSERVC18(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC18")
    End Sub

    Public Shared Sub RSERVC19(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC19# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC19", "SavVar")
        ParaMESH.RSERVC19(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC19")
    End Sub

    Public Shared Sub RSERVC20(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC20# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC20", "SavVar")
        ParaMESH.RSERVC20(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC20")
    End Sub

    Public Shared Sub RSERVC21(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC21# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC21", "SavVar")
        ParaMESH.RSERVC21(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC21")
    End Sub

    Public Shared Sub RSERVC22(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC22# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC22", "SavVar")
        ParaMESH.RSERVC22(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC22")
    End Sub

    Public Shared Sub RSERVC23(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC23# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC23", "SavVar")
        ParaMESH.RSERVC23(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC23")
    End Sub

    Public Shared Sub RSERVC24(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC24# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC24", "SavVar")
        ParaMESH.RSERVC24(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC24")
    End Sub

    Public Shared Sub RSERVC25(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC25# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC25", "SavVar")
        ParaMESH.RSERVC25(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC25")
    End Sub

    Public Shared Sub RSERVC26(ByVal Value As Object, ByVal GRU As Integer, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("RSERVC26# (GRU " & (GRU + 1).ToString & ")", "ParaSAVE.RSERVC26", "SavVar")
        ParaMESH.RSERVC26(BakFlag, GRU) = ParaMESH.CheckValue(Value, "RSERVC26")
    End Sub

    Public Shared Sub HourlyStartDay(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("HourlyStartDay#", "ParaSAVE.HourlyStartDay", "SavVar")
        ParaMESH.HourlyStartDay(BakFlag) = ParaMESH.CheckValue(Value, "HourlyStartDay")
    End Sub

    Public Shared Sub HourlyStartYear(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("HourlyStartYear#", "ParaSAVE.HourlyStartYear", "SavVar")
        ParaMESH.HourlyStartYear(BakFlag) = ParaMESH.CheckValue(Value, "HourlyStartYear")
    End Sub

    Public Shared Sub HourlyStopDay(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("HourlyStopDay#", "ParaSAVE.HourlyStopDay", "SavVar")
        ParaMESH.HourlyStopDay(BakFlag) = ParaMESH.CheckValue(Value, "HourlyStopDay")
    End Sub

    Public Shared Sub HourlyStopYear(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("HourlyStopYear#", "ParaSAVE.HourlyStopYear", "SavVar")
        ParaMESH.HourlyStopYear(BakFlag) = ParaMESH.CheckValue(Value, "HourlyStopYear")
    End Sub

    Public Shared Sub DailyStartDay(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("DailyStartDay#", "ParaSAVE.DailyStartDay", "SavVar")
        ParaMESH.DailyStartDay(BakFlag) = ParaMESH.CheckValue(Value, "DailyStartDay")
    End Sub

    Public Shared Sub DailyStartYear(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("DailyStartYear#", "ParaSAVE.DailyStartYear", "SavVar")
        ParaMESH.DailyStartYear(BakFlag) = ParaMESH.CheckValue(Value, "DailyStartYear")
    End Sub

    Public Shared Sub DailyStopDay(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("DailyStopDay#", "ParaSAVE.DailyStopDay", "SavVar")
        ParaMESH.DailyStopDay(BakFlag) = ParaMESH.CheckValue(Value, "DailyStopDay")
    End Sub

    Public Shared Sub DailyStopYear(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("DailyStopYear#", "ParaSAVE.DailyStopYear", "SavVar")
        ParaMESH.DailyStopYear(BakFlag) = ParaMESH.CheckValue(Value, "DailyStopYear")
    End Sub

    Public Shared Sub SimStartDay(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("SimStartDay#", "ParaSAVE.SimStartDay", "SavVar")
        ParaMESH.SimStartDay(BakFlag) = ParaMESH.CheckValue(Value, "SimStartDay")
    End Sub

    Public Shared Sub SimStartYear(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("SimStartYear#", "ParaSAVE.SimStartYear", "SavVar")
        ParaMESH.SimStartYear(BakFlag) = ParaMESH.CheckValue(Value, "SimStartYear")
    End Sub

    Public Shared Sub SimStopDay(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("SimStopDay#", "ParaSAVE.SimStopDay", "SavVar")
        ParaMESH.SimStopDay(BakFlag) = ParaMESH.CheckValue(Value, "SimStopDay")
    End Sub

    Public Shared Sub SimStopYear(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("SimStopYear#", "ParaSAVE.SimStopYear", "SavVar")
        ParaMESH.SimStopYear(BakFlag) = ParaMESH.CheckValue(Value, "SimStopYear")
    End Sub

    Public Shared Sub MetStartMin(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("MetStartMin#", "ParaSAVE.MetStartMin", "SavVar")
        ParaMESH.MetStartMin(BakFlag) = ParaMESH.CheckValue(Value, "MetStartMin")
    End Sub

    Public Shared Sub MetStartHour(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("MetStartHour#", "ParaSAVE.MetStartHour", "SavVar")
        ParaMESH.MetStartHour(BakFlag) = ParaMESH.CheckValue(Value, "MetStartHour")
    End Sub

    Public Shared Sub MetStartDay(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("MetStartDay#", "ParaSAVE.MetStartDay", "SavVar")
        ParaMESH.MetStartDay(BakFlag) = ParaMESH.CheckValue(Value, "MetStartDay")
    End Sub

    Public Shared Sub MetStartYear(ByVal Value As Object, Optional ByVal BakFlag As Integer = 0)
        ParaLOG.AppendFile("MetStartYear#", "ParaSAVE.MetStartYear", "SavVar")
        ParaMESH.MetStartYear(BakFlag) = ParaMESH.CheckValue(Value, "MetStartYear")
    End Sub
End Class
