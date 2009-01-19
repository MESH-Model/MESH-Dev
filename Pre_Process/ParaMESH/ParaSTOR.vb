Public Class ParaSTOR

    '*****************************************************************************
    'CLASS: ParaSTOR
    '*****************************************************************************
    'ParaSTOR is used to handle all ParaMESH restore processes (with the exception 
    'of reading and writing the ParaMESH restore file, processes which have been 
    'moved to the ParaREAD and ParaWRITE classes).
    '*****************************************************************************

    '****************************************************************************
    'PRIVATE ParaSTOR PARAMETERS
    '****************************************************************************
    'Parameters shared and used only within the ParaSTOR class.
    '****************************************************************************
    'Message Box Results: used to call user-directed prompt messages.
    'Private Integers: a, b, c are unique to the ParaSTOR class so that 
    'similarly private integers from calling subroutines are not reset.
    '****************************************************************************

    '***SHARED PRIVATE VARIABLES
    Private Shared CheckValues As New MsgBoxResult
    Private Shared CheckArray(3, 0) As String, CrossArray(,) As String, ParameterArray(,) As String, SplitArray() As String, TempArray() As String, a As Integer, b As Integer, c As Integer

    '*****************************************************************************
    'SUBROUTINE: ParaSTOR.Arguments
    '*****************************************************************************
    'ParaSTOR subroutines may be called by an external thread, which will only 
    'accept a single argument.  This subroutine is used to compile all arguments 
    'required by read subroutines into a single object array.
    'CallProcess denotes which read subroutine will be called.
    '*****************************************************************************

    Public Shared Function Arguments(ByVal CallProcess As String, Optional ByVal ParentForm As String = Nothing, Optional ByVal GRU As Integer = -1, Optional ByVal SilentCall As Boolean = False) As Object
        ReDim TempArray(0)

        '***POPULATE TempArray WITH ARGUMENTS FOR ProcessForm(Ext_ParaSTOR)
        TempArray(0) = CallProcess
        TempArray(1) = ParentForm
        TempArray(2) = GRU
        TempArray(3) = SilentCall

        '***RETURN ARGUMENTS
        Arguments = TempArray
    End Function

    '*****************************************************************************
    'SUBROUTINE: ParaSTOR.AppendList
    '*****************************************************************************
    'UPDATE: MAR 22 2008 (DAN)  use shared processes to cut code (reference to 
    '                           ParaMESH subroutines
    'UPDATE: JAN 22 2008 (DAN)  to handle SetDefault, as well as Restore
    '*****************************************************************************
    'Writes ParameterList array for RestoreForm.Arr_RestoreList table.
    'Only writes values for parameters which are enabled in the either 
    'ParaMESH.PrimaryParameters or ParaMESH.SecondaryParameters array.
    'Sets a value of {0} if no value exists for numeric parameters.
    '*****************************************************************************

    Private Shared Sub AppendList(ByVal OldValue As String, ByVal NewValue As String, ByVal ParameterCall As String, Optional ByVal Index As Integer = -1, Optional ByVal ColumnIndex As Integer = -1, Optional ByVal GRU As Boolean = False)

        '***DO NOTHING IF ParaSTOR IS NOT ENABLED
        If ParaMESH.IsEnabled("ParaSTOR", ParaMESH.SystemParameters, 1) = False Then
            Exit Sub
        End If

        '***CHECK FOR ParameterCall IN PARAMETER ARRAYS
        ParaLOG.AppendFile(ParameterCall & "# ..finding parameter in the parameter arrays", "ParaSTOR.AppendList", "ValVar")
        If ParaMESH.FindIndex(ParameterCall, ParaMESH.PrimaryParameters) > -1 Then
            ParameterArray = ParaMESH.PrimaryParameters
        Else
            ParameterArray = ParaMESH.SecondaryParameters
        End If

        '***CHECK IF THE PARAMETER IS ENABLED IN PARAMETER ARRAY
        ParaLOG.AppendFile(ParameterCall & "# ..checking if the parameter is enabled", "ParaSTOR.AppendList", "ValVar")
        If ParaMESH.IsEnabled(ParameterCall, ParameterArray) = False Then
            Exit Sub
        End If

        '***WRITES PARAMETER INFORMATION TO ParaSTOR.ParameterList ARRAY
        ParaLOG.AppendFile(ParameterCall & "# (Index " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ") ..writing the parameter to the parameter checklist", "ParaSTOR.CallBackup", "PopVar")
        ParaMESH.ParameterList(0, ParaMESH.ParameterList.GetUpperBound(1)) = ParameterArray(4, ParaMESH.FindIndex(ParameterCall, ParameterArray))
        If ParameterArray Is ParaMESH.SecondaryParameters Then
            If Index > -1 And ColumnIndex > -1 And GRU = True Then
                ParaMESH.ParameterList(0, ParaMESH.ParameterList.GetUpperBound(1)) &= " (Column " & (ColumnIndex + 1).ToString & " of " & ParameterArray(0, ParaMESH.FindIndex(ParameterCall, ParameterArray)) & ")"
            ElseIf Index > -1 And ColumnIndex > -1 And GRU = False Then
                ParaMESH.ParameterList(0, ParaMESH.ParameterList.GetUpperBound(1)) &= " (Line " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & " of " & ParaMESH.PrimaryParameters(3, ParaMESH.FindIndex(ParameterArray(0, ParaMESH.FindIndex(ParameterCall, ParameterArray)), ParaMESH.PrimaryParameters)) & ")"
            ElseIf Index > -1 And ColumnIndex = -1 And GRU = False Then
                ParaMESH.ParameterList(0, ParaMESH.ParameterList.GetUpperBound(1)) &= " (Line " & (Index + 1).ToString & " of " & ParaMESH.PrimaryParameters(3, ParaMESH.FindIndex(ParameterArray(0, ParaMESH.FindIndex(ParameterCall, ParameterArray)), ParaMESH.PrimaryParameters)) & ")"
            End If
        ElseIf ColumnIndex > -1 Then
            ParaMESH.ParameterList(0, ParaMESH.ParameterList.GetUpperBound(1)) &= " (Column " & (ColumnIndex + 1).ToString & ")"
        End If
        ParaMESH.ParameterList(1, ParaMESH.ParameterList.GetUpperBound(1)) = OldValue
        ParaMESH.ParameterList(2, ParaMESH.ParameterList.GetUpperBound(1)) = NewValue

        '***IF NO VALUE EXISTS, ENTER ZERO FOR NUMERIC VALUES
        If ParaMESH.ParameterList(2, ParaMESH.ParameterList.GetUpperBound(1)) = Nothing And ParameterArray(9, ParaMESH.FindIndex(ParameterCall, ParameterArray)) <> "String" Then
            ParaMESH.ParameterList(2, ParaMESH.ParameterList.GetUpperBound(1)) = "0"
        End If
        ParaMESH.ParameterList(3, ParaMESH.ParameterList.GetUpperBound(1)) = ParameterCall
        ParaMESH.ParameterList(4, ParaMESH.ParameterList.GetUpperBound(1)) = Index
        ParaMESH.ParameterList(5, ParaMESH.ParameterList.GetUpperBound(1)) = ColumnIndex
        ReDim Preserve ParaMESH.ParameterList(ParaMESH.ParameterList.GetUpperBound(0), ParaMESH.ParameterList.GetUpperBound(1) + 1)
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaSTOR.Restore
    '*****************************************************************************
    'UPDATE: MAR 22 2008 (DAN)  check if ParaMESH.SystemParameters(ParaSTOR) is 
    '                           enabled
    '*****************************************************************************
    'Process for restoring backup values for ParaForm (or all forms is ParaForm = 
    'Nothing.
    'Writes ParameterList to RestoreForm.Arr_RestoreList.
    'Saves checked values (the checkbox in the RestoreForm.Grd_Restore column).
    'Checks the values with ParaCHECK.Array using the CheckArray array.
    'Repopulates the system forms with the changed values.
    '*****************************************************************************

    Public Shared Sub Restore(ByVal CallProcess As String, Optional ByVal ParentForm As String = Nothing, Optional ByVal GRU As Integer = -1, Optional ByVal SilentCall As Boolean = False)

        '***DO NOTHING IF ParaSTOR IS NOT ENABLED
        If ParaMESH.IsEnabled("ParaSTOR", ParaMESH.SystemParameters, 1) = False Then
            Exit Sub
        End If
        ReDim ParaMESH.ParameterList(ParaMESH.ParameterList.GetUpperBound(0), 0), CheckArray(CheckArray.GetUpperBound(0), 0)

        '***CALL ParaSTOR(RedimParameter) IF SilentCall IS TRUE
        If SilentCall = True Then
            ParaSTOR.RedimParameter()
        End If

        '***CHECK ProcessForm
        If ParentForm = Nothing Or ParentForm = "ProcessForm" Then
            ParaSTOR.ProcessFormSub(CallProcess, SilentCall)
        End If

        '***WatflowForm
        If ParentForm = Nothing Or ParentForm = "WatflowForm" Then
            ParaSTOR.WatflowFormSub(CallProcess, SilentCall)
        End If

        '***CHECK GRUForm
        If ParentForm = Nothing Or ParentForm = "GRUForm" Then
            If GRU = -1 Then
                For GRU = 0 To ParaMESH.GRU.GetUpperBound(1)
                    ParaSTOR.GRUFormSub(CallProcess, GRU, SilentCall)
                Next

                '***RESET GRU
                GRU = -1
            Else
                ParaSTOR.GRUFormSub(CallProcess, GRU, SilentCall)
            End If
        End If

        '***SAVE VALUES SilentCall IS TRUE
        If SilentCall = True Then
            For a = 0 To ParaMESH.ParameterList.GetUpperBound(1) - 1

                '***SAVE THE VALUE
                ParaMESH.SaveValue(ParaMESH.ParameterList(2, a), ParaMESH.ParameterList(3, a), ParaMESH.ParameterList(4, a), ParaMESH.ParameterList(5, a))
            Next

            '***EXIT SUB
            Exit Sub
        End If

        '***WRITE PARAMETER LIST TO RestoreForm
        With RestoreForm.Arr_ParameterList
            .Rows.Clear()

            '***WRITE ParaMESH(ParameterList) VALUE ARRAY TO RestoreForm (-1 BECAUSE ARRAY ALWAYS HAS LAST ROW EMPTY)
            ParaLOG.AppendFile("ParaMESH is populating the ParaSTOR summary table", "ParaSTOR.Restore")
            For a = 0 To ParaMESH.ParameterList.GetUpperBound(1) - 1
                .RowCount += 1
                .Item("Grd_ParameterName", a).Value = ParaMESH.ParameterList(0, a)
                .Item("Grd_OldValue", a).Value = ParaMESH.ParameterList(1, a)
                .Item("Grd_NewValue", a).Value = ParaMESH.ParameterList(2, a)
                .Item("Grd_ParameterCall", a).Value = ParaMESH.ParameterList(3, a)
                .Item("Grd_Index", a).Value = ParaMESH.ParameterList(4, a)
                .Item("Grd_ColumnIndex", a).Value = ParaMESH.ParameterList(5, a)
            Next

            '***SHOW RestoreForm
            RestoreForm.ShowDialog()

            '***SAVE AND VERIFY CHECKED VALUES
            For a = 0 To .RowCount - 1
                If .Item("Grd_Restore", a).Value = True Then

                    '***CALL ParaSAVE(Grd_NewValue) TO SAVE VALUE
                    ParaMESH.SaveValue(.Item("Grd_NewValue", a).Value, .Item("Grd_ParameterCall", a).Value, .Item("Grd_Index", a).Value, .Item("Grd_ColumnIndex", a).Value)

                    '***ADD VALUE TO ParaSTOR(CheckArray)
                    ParaSTOR.CheckArray(0, ParaSTOR.CheckArray.GetUpperBound(1)) = .Item("Grd_NewValue", a).Value
                    ParaSTOR.CheckArray(1, ParaSTOR.CheckArray.GetUpperBound(1)) = .Item("Grd_ParameterCall", a).Value
                    ParaSTOR.CheckArray(2, ParaSTOR.CheckArray.GetUpperBound(1)) = .Item("Grd_Index", a).Value
                    ParaSTOR.CheckArray(3, ParaSTOR.CheckArray.GetUpperBound(1)) = .Item("Grd_ColumnIndex", a).Value
                    ReDim Preserve ParaSTOR.CheckArray(ParaSTOR.CheckArray.GetUpperBound(0), ParaSTOR.CheckArray.GetUpperBound(1) + 1)
                End If
            Next

            '***CALL ParaCHECK(Array) TO CHECK CheckArray FOR ERRONEOUS PARAMETER VALUES
            ParaCHECK.Array(CheckArray)

            '***CALL ProcessForm(Populate) TO POPULATE ProcessForm
            ProcessForm.Populate()
        End With
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaSTOR.SetDefault
    '*****************************************************************************
    'Loads default values for all parameters.
    '*****************************************************************************

    Public Shared Sub SetDefault(ByVal CallProcess As String)
        ReDim ParaMESH.ParameterList(ParaMESH.ParameterList.GetUpperBound(0), 0)

        '***CHECK VALUES IF ParaCHECK IS ENABLED
        If ParaMESH.IsEnabled("ParaCHECK", ParaMESH.SystemParameters, 1) = True And ParaMESH.IsEnabled("IgnoreWarnings", ParaMESH.SystemParameters, 1) = False Then
            CheckValues = MsgBox("Erroneous default parameter values are ignored by ParaCHECK.  Do you wish to verify the active parameter values now, before they may be set as default parameter values?", MsgBoxStyle.YesNo, "ParaMESH")
            If CheckValues = MsgBoxResult.Yes Then

                '***CALL ParaMESH(CallOperation) TO CHECK VALUES
                ParaMESH.CallOperation("ParaCHECK", PreferenceForm)
            End If
        End If

        '***CALL ParaSTOR(SaveDefaults) TO LOAD EXISTING AND NEW DEFAULT VALUES
        ParaSTOR.ProcessFormSub(CallProcess, False)
        ParaSTOR.WatflowFormSub(CallProcess, False)
        ParaSTOR.GRUFormSub(CallProcess, 0, False)

        '***WRITE PARAMETER LIST TO DefaultForm
        With DefaultForm.Arr_ParameterList
            .Rows.Clear()

            '***WRITE ParaSTOR.ParameterList VALUE ARRAY TO RestoreForm (-1: LAST ROW IS EMPTY BY DEFAULT)
            ParaLOG.AppendFile("ParaMESH is populating the ParaSTOR summary table", "ParaSTOR.SetDefault")
            For a = 0 To ParaMESH.ParameterList.GetUpperBound(1) - 1
                .RowCount += 1
                .Item("Grd_ParameterName", a).Value = ParaMESH.ParameterList(0, a)
                .Item("Grd_OldValue", a).Value = ParaMESH.ParameterList(1, a)
                .Item("Grd_NewValue", a).Value = ParaMESH.ParameterList(2, a)
                .Item("Grd_ParameterCall", a).Value = ParaMESH.ParameterList(3, a)
                .Item("Grd_Index", a).Value = ParaMESH.ParameterList(4, a)
                .Item("Grd_ColumnIndex", a).Value = ParaMESH.ParameterList(5, a)
            Next
        End With

        '***SHOW DefaultForm
        DefaultForm.ShowDialog()

        '***SAVE DEFAULT VALUES
        With DefaultForm.Arr_ParameterList
            For a = 0 To .RowCount - 1
                If .Item("Grd_SetDefault", a).Value = True And .Item("Grd_NewValue", a).Value <> Nothing Then

                    '***SAVE THE VALUE
                    ParaLOG.AppendFile("ParaMESH is saving the default value {" & (.Item("Grd_NewValue", a).Value).ToString & "} for parameter: " & (.Item("Grd_ParameterCall", a).Value).ToString, "ParaSTOR.SetDefault")
                    Try
                        ParaSAVE.SecondaryParameters(.Item("Grd_NewValue", a).Value, .Item("Grd_ParameterCall", a).Value, 5)
                    Catch ex As Exception
                        ParaSAVE.PrimaryParameters(.Item("Grd_NewValue", a).Value, .Item("Grd_ParameterCall", a).Value, 5)
                    End Try
                End If
            Next
        End With
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaSTOR.ProcessFormSub
    '*****************************************************************************
    'Calls the backup value, the default value, or sets the default value for 
    'parameter values displayed by ProcessForm, based on the given CallProcess.
    '*****************************************************************************

    Private Shared Sub ProcessFormSub(ByVal CallProcess As String, ByVal SilentCall As Boolean)

        '***PROJECT INFORMATION
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.TITLE(0), ParaMESH.GetDefault("TITLE"), "TITLE")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.TITLE(0), ParaMESH.TITLE(1), "TITLE")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("TITLE"), ParaMESH.TITLE(0), "TITLE")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.NAME(0), ParaMESH.GetDefault("NAME"), "NAME")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.NAME(0), ParaMESH.NAME(1), "NAME")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("NAME"), ParaMESH.NAME(0), "NAME")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.PLACE(0), ParaMESH.GetDefault("PLACE"), "PLACE")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.PLACE(0), ParaMESH.PLACE(1), "PLACE")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("PLACE"), ParaMESH.PLACE(0), "PLACE")
        End If

        '***SITE INFORMATION
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.DEGLAT(0), ParaMESH.GetDefault("DEGLAT"), "DEGLAT")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.DEGLAT(0), ParaMESH.DEGLAT(1), "DEGLAT")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("DEGLAT"), ParaMESH.DEGLAT(0), "DEGLAT")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.DEGLON(0), ParaMESH.GetDefault("DEGLON"), "DEGLON")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.DEGLON(0), ParaMESH.DEGLON(1), "DEGLON")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("DEGLON"), ParaMESH.DEGLON(0), "DEGLON")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.ZRFM(0), ParaMESH.GetDefault("ZRFM"), "ZRFM")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.ZRFM(0), ParaMESH.ZRFM(1), "ZRFM")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("ZRFM"), ParaMESH.ZRFM(0), "ZRFM")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.ZRFH(0), ParaMESH.GetDefault("ZRFH"), "ZRFH")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.ZRFH(0), ParaMESH.ZRFH(1), "ZRFH")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("ZRFH"), ParaMESH.ZRFH(0), "ZRFH")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.ZBLD(0), ParaMESH.GetDefault("ZBLD"), "ZBLD")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.ZBLD(0), ParaMESH.ZBLD(1), "ZBLD")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("ZBLD"), ParaMESH.ZBLD(0), "ZBLD")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.GC(0), ParaMESH.GetDefault("GC"), "GC")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.GC(0), ParaMESH.GC(1), "GC")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("GC"), ParaMESH.GC(0), "GC")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.ILW(0), ParaMESH.GetDefault("ILW"), "ILW")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.ILW(0), ParaMESH.ILW(1), "ILW")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("ILW"), ParaMESH.ILW(0), "ILW")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.GRID(0), ParaMESH.GetDefault("GRID"), "GRID")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.GRID(0), ParaMESH.GRID(1), "GRID")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("GRID"), ParaMESH.GRID(0), "GRID")
        End If

        '***RUN TIMES
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.HourlyStartDay(0), ParaMESH.GetDefault("HourlyStartDay"), "HourlyStartDay")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.HourlyStartDay(0), ParaMESH.HourlyStartDay(1), "HourlyStartDay")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("HourlyStartDay"), ParaMESH.HourlyStartDay(0), "HourlyStartDay")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.HourlyStartYear(0), ParaMESH.GetDefault("HourlyStartYear"), "HourlyStartYear")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.HourlyStartYear(0), ParaMESH.HourlyStartYear(1), "HourlyStartYear")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("HourlyStartYear"), ParaMESH.HourlyStartYear(0), "HourlyStartYear")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.HourlyStopDay(0), ParaMESH.GetDefault("HourlyStopDay"), "HourlyStopDay")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.HourlyStopDay(0), ParaMESH.HourlyStopDay(1), "HourlyStopDay")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("HourlyStopDay"), ParaMESH.HourlyStopDay(0), "HourlyStopDay")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.HourlyStopYear(0), ParaMESH.GetDefault("HourlyStopYear"), "HourlyStopYear")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.HourlyStopYear(0), ParaMESH.HourlyStopYear(1), "HourlyStopYear")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("HourlyStopYear"), ParaMESH.HourlyStopYear(0), "HourlyStopYear")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.DailyStartDay(0), ParaMESH.GetDefault("DailyStartDay"), "DailyStartDay")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.DailyStartDay(0), ParaMESH.DailyStartDay(1), "DailyStartDay")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("DailyStartDay"), ParaMESH.DailyStartDay(0), "DailyStartDay")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.DailyStartYear(0), ParaMESH.GetDefault("DailyStartYear"), "DailyStartYear")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.DailyStartYear(0), ParaMESH.DailyStartYear(1), "DailyStartYear")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("DailyStartYear"), ParaMESH.DailyStartYear(0), "DailyStartYear")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.DailyStopDay(0), ParaMESH.GetDefault("DailyStopDay"), "DailyStopDay")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.DailyStopDay(0), ParaMESH.DailyStopDay(1), "DailyStopDay")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("DailyStopDay"), ParaMESH.DailyStopDay(0), "DailyStopDay")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.DailyStopYear(0), ParaMESH.GetDefault("DailyStopYear"), "DailyStopYear")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.DailyStopYear(0), ParaMESH.DailyStopYear(1), "DailyStopYear")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("DailyStopYear"), ParaMESH.DailyStopYear(0), "DailyStopYear")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.SimStartDay(0), ParaMESH.GetDefault("SimStartDay"), "SimStartDay")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.SimStartDay(0), ParaMESH.SimStartDay(1), "SimStartDay")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("SimStartDay"), ParaMESH.SimStartDay(0), "SimStartDay")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.SimStartYear(0), ParaMESH.GetDefault("SimStartYear"), "SimStartYear")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.SimStartYear(0), ParaMESH.SimStartYear(1), "SimStartYear")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("SimStartYear"), ParaMESH.SimStartYear(0), "SimStartYear")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.SimStopDay(0), ParaMESH.GetDefault("SimStopDay"), "SimStopDay")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.SimStopDay(0), ParaMESH.SimStopDay(1), "SimStopDay")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("SimStopDay"), ParaMESH.SimStopDay(0), "SimStopDay")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.SimStopYear(0), ParaMESH.GetDefault("SimStopYear"), "SimStopYear")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.SimStopYear(0), ParaMESH.SimStopYear(1), "SimStopYear")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("SimStopYear"), ParaMESH.SimStopDay(0), "SimStopYear")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.MetStartMin(0), ParaMESH.GetDefault("MetStartMin"), "MetStartMin")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.MetStartMin(0), ParaMESH.MetStartMin(1), "MetStartMin")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("MetStartMin"), ParaMESH.MetStartMin(0), "MetStartMin")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.MetStartHour(0), ParaMESH.GetDefault("MetStartHour"), "MetStartHour")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.MetStartHour(0), ParaMESH.MetStartHour(1), "MetStartHour")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("MetStartHour"), ParaMESH.MetStartHour(0), "MetStartHour")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.MetStartDay(0), ParaMESH.GetDefault("MetStartDay"), "MetStartDay")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.MetStartDay(0), ParaMESH.MetStartDay(1), "MetStartDay")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("MetStartDay"), ParaMESH.MetStartDay(0), "MetStartDay")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.MetStartYear(0), ParaMESH.GetDefault("MetStartYear"), "MetStartYear")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.MetStartYear(0), ParaMESH.MetStartYear(1), "MetStartYear")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("MetStartYear"), ParaMESH.MetStartYear(0), "MetStartYear")
        End If

        '***HYDROLOGIC PARAMETERS
        For a = 0 To ParaMESH.WF_R2.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.WF_R2(0, a), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "WF_R2")), ParaMESH.SecondaryName(a, "WF_R2"), a)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.WF_R2(0, a), ParaMESH.WF_R2(1, a), ParaMESH.SecondaryName(a, "WF_R2"), a)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "WF_R2")), ParaMESH.WF_R2(0, a), ParaMESH.SecondaryName(a, "WF_R2"), a)
            End If
        Next

        '***SECONDARY PARAMETERS
        For a = 0 To ParaMESH.ControlFlag.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.ControlFlag(0, a), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "ControlFlag")), ParaMESH.SecondaryName(a, "ControlFlag"), a)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.ControlFlag(0, a), ParaMESH.ControlFlag(1, a), ParaMESH.SecondaryName(a, "ControlFlag"), a)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "ControlFlag")), ParaMESH.ControlFlag(0, a), ParaMESH.SecondaryName(a, "ControlFlag"), a)
            End If
        Next
        If ParaMESH.IsEnabled("NoOutput", ParaMESH.SystemParameters, 1) = True And SilentCall = False Then
            MsgBox("ParaMESH will not include Grid Outputs in the restore process.  The parameter value does not exist.", MsgBoxStyle.OkOnly, "ParaMESH")
        ElseIf ParaMESH.IsEnabled("NoOutput", ParaMESH.SystemParameters, 1) = False Then
            For a = 0 To ParaMESH.GridOutput.GetUpperBound(2)
                For b = 0 To ParaMESH.GridOutput.GetUpperBound(1)
                    If CallProcess = "CallDefault" Then
                        ParaSTOR.AppendList(ParaMESH.GridOutput(0, b, a), ParaMESH.GetDefault(ParaMESH.SecondaryName(b, "GridOutput")), ParaMESH.SecondaryName(b, "GridOutput"), a, b)
                    ElseIf CallProcess = "CallBackup" Then
                        ParaSTOR.AppendList(ParaMESH.GridOutput(0, b, a), ParaMESH.GridOutput(1, b, a), ParaMESH.SecondaryName(b, "GridOutput"), a, b)
                    ElseIf CallProcess = "SetDefault" Then
                        ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(b, "GridOutput")), ParaMESH.GridOutput(0, b, a), ParaMESH.SecondaryName(b, "GridOutput"), a, b)
                    End If
                Next
            Next
        End If
        For a = 0 To ParaMESH.OptionFlag.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.OptionFlag(0, a), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "OptionFlag")), ParaMESH.SecondaryName(a, "OptionFlag"), a)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.OptionFlag(0, a), ParaMESH.OptionFlag(1, a), ParaMESH.SecondaryName(a, "OptionFlag"), a)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "OptionFlag")), ParaMESH.OptionFlag(0, a), ParaMESH.SecondaryName(a, "OptionFlag"), a)
            End If
        Next
        For a = 0 To ParaMESH.IndependentGRU.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.IndependentGRU(0, a), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "IndependentGRU")), ParaMESH.SecondaryName(a, "IndependentGRU"), a)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.IndependentGRU(0, a), ParaMESH.IndependentGRU(1, a), ParaMESH.SecondaryName(a, "IndependentGRU"), a)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "IndependentGRU")), ParaMESH.IndependentGRU(0, a), ParaMESH.SecondaryName(a, "IndependentGRU"), a)
            End If
        Next
        For a = 0 To ParaMESH.DependentGRU.GetUpperBound(2)
            For b = 0 To ParaMESH.DependentGRU.GetUpperBound(1)
                If CallProcess = "CallDefault" Then
                    ParaSTOR.AppendList(ParaMESH.DependentGRU(0, b, a), ParaMESH.GetDefault(ParaMESH.SecondaryName(b, "DependentGRU")), ParaMESH.SecondaryName(b, "DependentGRU"), a, b)
                ElseIf CallProcess = "CallBackup" Then
                    ParaSTOR.AppendList(ParaMESH.DependentGRU(0, b, a), ParaMESH.DependentGRU(1, b, a), ParaMESH.SecondaryName(b, "DependentGRU"), a, b)
                ElseIf CallProcess = "SetDefault" Then
                    ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(b, "DependentGRU")), ParaMESH.DependentGRU(0, b, a), ParaMESH.SecondaryName(b, "DependentGRU"), a, b)
                End If
            Next
        Next
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaSTOR.WatflowFormSub
    '*****************************************************************************
    'Calls the backup value, the default value, or sets the default value for 
    'parameter values displayed by WatflowForm, based on the given CallProcess.
    '*****************************************************************************

    Private Shared Sub WatflowFormSub(ByVal CallProcess As String, ByVal SilentCall As Boolean)

        '***BASIN INFORMATION, SCALING FACTOR
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.BasinID(0), ParaMESH.GetDefault("BasinID"), "BasinID")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.BasinID(0), ParaMESH.BasinID(1), "BasinID")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("BasinID"), ParaMESH.BasinID(0), "BasinID")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.ScalingFactor(0), ParaMESH.GetDefault("ScalingFactor"), "ScalingFactor")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.ScalingFactor(0), ParaMESH.ScalingFactor(1), "ScalingFactor")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("ScalingFactor"), ParaMESH.ScalingFactor(0), "ScalingFactor")
        End If

        '***INITIAL GROUNDWATER
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.GWINIT(0), ParaMESH.GetDefault("GWINIT"), "GWINIT")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.GWINIT(0), ParaMESH.GWINIT(1), "GWINIT")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("GWINIT"), ParaMESH.GWINIT(0), "GWINIT")
        End If

        '***WATFLOW FLAGS
        For a = 0 To ParaMESH.WatflowFlag.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.WatflowFlag(0, a), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "WatflowFlag")), ParaMESH.SecondaryName(a, "WatflowFlag"), a)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.WatflowFlag(0, a), ParaMESH.WatflowFlag(1, a), ParaMESH.SecondaryName(a, "WatflowFlag"), a)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "WatflowFlag")), ParaMESH.WatflowFlag(0, a), ParaMESH.SecondaryName(a, "WatflowFlag"), a)
            End If
        Next

        '***PONDING LIMITS
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.ZPLIMG0(0), ParaMESH.GetDefault("ZPLIMG0"), "ZPLIMG0")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.ZPLIMG0(0), ParaMESH.ZPLIMG0(1), "ZPLIMG0")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("ZPLIMG0"), ParaMESH.ZPLIMG0(0), "ZPLIMG0")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.ZPLIMS0(0), ParaMESH.GetDefault("ZPLIMS0"), "ZPLIMS0")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.ZPLIMS0(0), ParaMESH.ZPLIMS0(1), "ZPLIMS0")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("ZPLIMS0"), ParaMESH.ZPLIMS0(0), "ZPLIMS0")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.ZPLIMP0(0), ParaMESH.GetDefault("ZPLIMP0"), "ZPLIMP0")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.ZPLIMP0(0), ParaMESH.ZPLIMP0(1), "ZPLIMP0")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("ZPLIMP0"), ParaMESH.ZPLIMP0(0), "ZPLIMP0")
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.ZPLIMPS0(0), ParaMESH.GetDefault("ZPLIMPS0"), "ZPLIMPS0")
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.ZPLIMPS0(0), ParaMESH.ZPLIMPS0(1), "ZPLIMPS0")
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("ZPLIMPS0"), ParaMESH.ZPLIMPS0(0), "ZPLIMPS0")
        End If

        '***SNOW LIMITS
        For a = 0 To ParaMESH.D100A.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.D100A(0, a), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "D100A")), ParaMESH.SecondaryName(a, "D100A"), a)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.D100A(0, a), ParaMESH.D100A(1, a), ParaMESH.SecondaryName(a, "D100A"), a)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "D100A")), ParaMESH.D100A(0, a), ParaMESH.SecondaryName(a, "D100A"), a)
            End If
        Next
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaSTOR.GRUFormSub
    '*****************************************************************************
    'Calls the backup value, the default value, or sets the default value for 
    'parameter values displayed by GRUForm, based on the given CallProcess.
    '*****************************************************************************

    Private Shared Sub GRUFormSub(ByVal CallProcess As String, ByVal GRU As Integer, ByVal SilentCall As Boolean)

        '***GRU DESCRIPTION
        If CallProcess = "CallDefault" Then
            If ParaMESH.IsEnabled("GRU", ParaMESH.SpecialChecks, 1) = True Then
                ParaSTOR.AppendList(ParaMESH.GRU(0, GRU), ParaMESH.GetDefault("GRU") & " " & (GRU + 1).ToString, "GRU", GRU)
            Else
                ParaSTOR.AppendList(ParaMESH.GRU(0, GRU), ParaMESH.GetDefault("GRU"), "GRU", GRU)
            End If
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.GRU(0, GRU), ParaMESH.GRU(1, GRU), "GRU", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("GRU"), ParaMESH.GRU(0, GRU), "GRU", GRU)
        End If

        '***FIRST BLOCK
        For a = 0 To ParaMESH.FCAN.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.FCAN(0, a, GRU), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "FCAN")), ParaMESH.SecondaryName(a, "FCAN"), GRU, a, True)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.FCAN(0, a, GRU), ParaMESH.FCAN(1, a, GRU), ParaMESH.SecondaryName(a, "FCAN"), GRU, a, True)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "FCAN")), ParaMESH.FCAN(0, a, GRU), ParaMESH.SecondaryName(a, "FCAN"), GRU, a, True)
            End If
        Next
        For a = 0 To ParaMESH.LNZ0.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.LNZ0(0, a, GRU), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "LNZ0")), ParaMESH.SecondaryName(a, "LNZ0"), GRU, a, True)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.LNZ0(0, a, GRU), ParaMESH.LNZ0(1, a, GRU), ParaMESH.SecondaryName(a, "LNZ0"), GRU, a, True)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "LNZ0")), ParaMESH.LNZ0(0, a, GRU), ParaMESH.SecondaryName(a, "LNZ0"), GRU, a, True)
            End If
        Next
        For a = 0 To ParaMESH.ALVC.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.ALVC(0, a, GRU), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "ALVC")), ParaMESH.SecondaryName(a, "ALVC"), GRU, a, True)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.ALVC(0, a, GRU), ParaMESH.ALVC(1, a, GRU), ParaMESH.SecondaryName(a, "ALVC"), GRU, a, True)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "ALVC")), ParaMESH.ALVC(0, a, GRU), ParaMESH.SecondaryName(a, "ALVC"), GRU, a, True)
            End If
        Next
        For a = 0 To ParaMESH.ALIC.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.ALIC(0, a, GRU), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "ALIC")), ParaMESH.SecondaryName(a, "ALIC"), GRU, a, True)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.ALIC(0, a, GRU), ParaMESH.ALIC(1, a, GRU), ParaMESH.SecondaryName(a, "ALIC"), GRU, a, True)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "ALIC")), ParaMESH.ALIC(0, a, GRU), ParaMESH.SecondaryName(a, "ALIC"), GRU, a, True)
            End If
        Next
        For a = 0 To ParaMESH.LAMX.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.LAMX(0, a, GRU), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "LAMX")), ParaMESH.SecondaryName(a, "LAMX"), GRU, a, True)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.LAMX(0, a, GRU), ParaMESH.LAMX(1, a, GRU), ParaMESH.SecondaryName(a, "LAMX"), GRU, a, True)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "LAMX")), ParaMESH.LAMX(0, a, GRU), ParaMESH.SecondaryName(a, "LAMX"), GRU, a, True)
            End If
        Next
        For a = 0 To ParaMESH.LAMN.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.LAMN(0, a, GRU), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "LAMN")), ParaMESH.SecondaryName(a, "LAMN"), GRU, a, True)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.LAMN(0, a, GRU), ParaMESH.LAMN(1, a, GRU), ParaMESH.SecondaryName(a, "LAMN"), GRU, a, True)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "LAMN")), ParaMESH.LAMN(0, a, GRU), ParaMESH.SecondaryName(a, "LAMN"), GRU, a, True)
            End If
        Next
        For a = 0 To ParaMESH.CMAS.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.CMAS(0, a, GRU), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "CMAS")), ParaMESH.SecondaryName(a, "CMAS"), GRU, a, True)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.CMAS(0, a, GRU), ParaMESH.CMAS(1, a, GRU), ParaMESH.SecondaryName(a, "CMAS"), GRU, a, True)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "CMAS")), ParaMESH.CMAS(0, a, GRU), ParaMESH.SecondaryName(a, "CMAS"), GRU, a, True)
            End If
        Next
        For a = 0 To ParaMESH.ROOT.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.ROOT(0, a, GRU), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "ROOT")), ParaMESH.SecondaryName(a, "ROOT"), GRU, a, True)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.ROOT(0, a, GRU), ParaMESH.ROOT(1, a, GRU), ParaMESH.SecondaryName(a, "ROOT"), GRU, a, True)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "ROOT")), ParaMESH.ROOT(0, a, GRU), ParaMESH.SecondaryName(a, "ROOT"), GRU, a, True)
            End If
        Next
        For a = 0 To ParaMESH.RSMN.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.RSMN(0, a, GRU), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "RSMN")), ParaMESH.SecondaryName(a, "RSMN"), GRU, a, True)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.RSMN(0, a, GRU), ParaMESH.RSMN(1, a, GRU), ParaMESH.SecondaryName(a, "RSMN"), GRU, a, True)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "RSMN")), ParaMESH.RSMN(0, a, GRU), ParaMESH.SecondaryName(a, "RSMN"), GRU, a, True)
            End If
        Next
        For a = 0 To ParaMESH.QA50.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.QA50(0, a, GRU), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "QA50")), ParaMESH.SecondaryName(a, "QA50"), GRU, a, True)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.QA50(0, a, GRU), ParaMESH.QA50(1, a, GRU), ParaMESH.SecondaryName(a, "QA50"), GRU, a, True)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "QA50")), ParaMESH.QA50(0, a, GRU), ParaMESH.SecondaryName(a, "QA50"), GRU, a, True)
            End If
        Next
        For a = 0 To ParaMESH.VPDA.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.VPDA(0, a, GRU), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "VPDA")), ParaMESH.SecondaryName(a, "VPDA"), GRU, a, True)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.VPDA(0, a, GRU), ParaMESH.VPDA(1, a, GRU), ParaMESH.SecondaryName(a, "VPDA"), GRU, a, True)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "VPDA")), ParaMESH.VPDA(0, a, GRU), ParaMESH.SecondaryName(a, "VPDA"), GRU, a, True)
            End If
        Next
        For a = 0 To ParaMESH.VPDB.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.VPDB(0, a, GRU), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "VPDB")), ParaMESH.SecondaryName(a, "VPDB"), GRU, a, True)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.VPDB(0, a, GRU), ParaMESH.VPDB(1, a, GRU), ParaMESH.SecondaryName(a, "VPDB"), GRU, a, True)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "VPDB")), ParaMESH.VPDB(0, a, GRU), ParaMESH.SecondaryName(a, "VPDB"), GRU, a, True)
            End If
        Next
        For a = 0 To ParaMESH.PSGA.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.PSGA(0, a, GRU), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "PSGA")), ParaMESH.SecondaryName(a, "PSGA"), GRU, a, True)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.PSGA(0, a, GRU), ParaMESH.PSGA(1, a, GRU), ParaMESH.SecondaryName(a, "PSGA"), GRU, a, True)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "PSGA")), ParaMESH.PSGA(0, a, GRU), ParaMESH.SecondaryName(a, "PSGA"), GRU, a, True)
            End If
        Next
        For a = 0 To ParaMESH.PSGB.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.PSGB(0, a, GRU), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "PSGB")), ParaMESH.SecondaryName(a, "PSGB"), GRU, a, True)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.PSGB(0, a, GRU), ParaMESH.PSGB(1, a, GRU), ParaMESH.SecondaryName(a, "PSGB"), GRU, a, True)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "PSGB")), ParaMESH.PSGB(0, a, GRU), ParaMESH.SecondaryName(a, "PSGB"), GRU, a, True)
            End If
        Next
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.DRN(0, GRU), ParaMESH.GetDefault("DRN"), "DRN", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.DRN(0, GRU), ParaMESH.DRN(1, GRU), "DRN", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("DRN"), ParaMESH.DRN(0, GRU), "DRN", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.SDEP(0, GRU), ParaMESH.GetDefault("SDEP"), "SDEP", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.SDEP(0, GRU), ParaMESH.SDEP(1, GRU), "SDEP", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("SDEP"), ParaMESH.SDEP(0, GRU), "SDEP", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.FARE(0, GRU), ParaMESH.GetDefault("FARE"), "FARE", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.FARE(0, GRU), ParaMESH.FARE(1, GRU), "FARE", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("FARE"), ParaMESH.FARE(0, GRU), "FARE", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.DDEN(0, GRU), ParaMESH.GetDefault("DDEN"), "DDEN", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.DDEN(0, GRU), ParaMESH.DDEN(1, GRU), "DDEN", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("DDEN"), ParaMESH.DDEN(0, GRU), "DDEN", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.XSLP(0, GRU), ParaMESH.GetDefault("XSLP"), "XSLP", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.XSLP(0, GRU), ParaMESH.XSLP(1, GRU), "XSLP", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("XSLP"), ParaMESH.XSLP(0, GRU), "XSLP", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.GRKF(0, GRU), ParaMESH.GetDefault("GRKF"), "GRKF", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.GRKF(0, GRU), ParaMESH.GRKF(1, GRU), "GRKF", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("GRKF"), ParaMESH.GRKF(0, GRU), "GRKF", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.WFSF(0, GRU), ParaMESH.GetDefault("WFSF"), "WFSF", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.WFSF(0, GRU), ParaMESH.WFSF(1, GRU), "WFSF", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("WFSF"), ParaMESH.WFSF(0, GRU), "WFSF", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.WFCI(0, GRU), ParaMESH.GetDefault("WFCI"), "WFCI", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.WFCI(0, GRU), ParaMESH.WFCI(1, GRU), "WFCI", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("WFCI"), ParaMESH.WFCI(0, GRU), "WFCI", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC01(0, GRU), ParaMESH.GetDefault("RSERVC01"), "RSERVC01", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC01(0, GRU), ParaMESH.RSERVC01(1, GRU), "RSERVC01", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC01"), ParaMESH.RSERVC01(0, GRU), "RSERVC01", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC02(0, GRU), ParaMESH.GetDefault("RSERVC02"), "RSERVC02", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC02(0, GRU), ParaMESH.RSERVC02(1, GRU), "RSERVC02", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC02"), ParaMESH.RSERVC02(0, GRU), "RSERVC02", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC03(0, GRU), ParaMESH.GetDefault("RSERVC03"), "RSERVC03", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC03(0, GRU), ParaMESH.RSERVC03(1, GRU), "RSERVC03", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC03"), ParaMESH.RSERVC03(0, GRU), "RSERVC03", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC04(0, GRU), ParaMESH.GetDefault("RSERVC04"), "RSERVC04", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC04(0, GRU), ParaMESH.RSERVC04(1, GRU), "RSERVC04", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC04"), ParaMESH.RSERVC04(0, GRU), "RSERVC04", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC05(0, GRU), ParaMESH.GetDefault("RSERVC05"), "RSERVC05", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC05(0, GRU), ParaMESH.RSERVC05(1, GRU), "RSERVC05", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC05"), ParaMESH.RSERVC05(0, GRU), "RSERVC05", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC06(0, GRU), ParaMESH.GetDefault("RSERVC06"), "RSERVC06", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC06(0, GRU), ParaMESH.RSERVC06(1, GRU), "RSERVC06", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC06"), ParaMESH.RSERVC06(0, GRU), "RSERVC06", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC07(0, GRU), ParaMESH.GetDefault("RSERVC07"), "RSERVC07", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC07(0, GRU), ParaMESH.RSERVC07(1, GRU), "RSERVC07", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC07"), ParaMESH.RSERVC07(0, GRU), "RSERVC07", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC08(0, GRU), ParaMESH.GetDefault("RSERVC08"), "RSERVC08", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC08(0, GRU), ParaMESH.RSERVC08(1, GRU), "RSERVC08", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC08"), ParaMESH.RSERVC08(0, GRU), "RSERVC08", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC09(0, GRU), ParaMESH.GetDefault("RSERVC09"), "RSERVC09", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC09(0, GRU), ParaMESH.RSERVC09(1, GRU), "RSERVC09", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC09"), ParaMESH.RSERVC09(0, GRU), "RSERVC09", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC10(0, GRU), ParaMESH.GetDefault("RSERVC10"), "RSERVC10", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC10(0, GRU), ParaMESH.RSERVC10(1, GRU), "RSERVC10", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC10"), ParaMESH.RSERVC10(0, GRU), "RSERVC10", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC11(0, GRU), ParaMESH.GetDefault("RSERVC11"), "RSERVC11", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC11(0, GRU), ParaMESH.RSERVC11(1, GRU), "RSERVC11", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC11"), ParaMESH.RSERVC11(0, GRU), "RSERVC11", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC12(0, GRU), ParaMESH.GetDefault("RSERVC12"), "RSERVC12", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC12(0, GRU), ParaMESH.RSERVC12(1, GRU), "RSERVC12", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC12"), ParaMESH.RSERVC12(0, GRU), "RSERVC12", GRU)
        End If

        '***SECOND BLOCK
        For a = 0 To ParaMESH.SAND.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.SAND(0, a, GRU), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "SAND")), ParaMESH.SecondaryName(a, "SAND"), GRU, a, True)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.SAND(0, a, GRU), ParaMESH.SAND(1, a, GRU), ParaMESH.SecondaryName(a, "SAND"), GRU, a, True)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "SAND")), ParaMESH.SAND(0, a, GRU), ParaMESH.SecondaryName(a, "SAND"), GRU, a, True)
            End If
        Next
        For a = 0 To ParaMESH.CLAY.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.CLAY(0, a, GRU), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "CLAY")), ParaMESH.SecondaryName(a, "CLAY"), GRU, a, True)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.CLAY(0, a, GRU), ParaMESH.CLAY(1, a, GRU), ParaMESH.SecondaryName(a, "CLAY"), GRU, a, True)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "CLAY")), ParaMESH.CLAY(0, a, GRU), ParaMESH.SecondaryName(a, "CLAY"), GRU, a, True)
            End If
        Next
        For a = 0 To ParaMESH.ORGM.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.ORGM(0, a, GRU), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "ORGM")), ParaMESH.SecondaryName(a, "ORGM"), GRU, a, True)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.ORGM(0, a, GRU), ParaMESH.ORGM(1, a, GRU), ParaMESH.SecondaryName(a, "ORGM"), GRU, a, True)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "ORGM")), ParaMESH.ORGM(0, a, GRU), ParaMESH.SecondaryName(a, "ORGM"), GRU, a, True)
            End If
        Next
        For a = 0 To ParaMESH.TBAR.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.TBAR(0, a, GRU), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "TBAR")), ParaMESH.SecondaryName(a, "TBAR"), GRU, a, True)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.TBAR(0, a, GRU), ParaMESH.TBAR(1, a, GRU), ParaMESH.SecondaryName(a, "TBAR"), GRU, a, True)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "TBAR")), ParaMESH.TBAR(0, a, GRU), ParaMESH.SecondaryName(a, "TBAR"), GRU, a, True)
            End If
        Next
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.TCAN(0, GRU), ParaMESH.GetDefault("TCAN"), "TCAN", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.TCAN(0, GRU), ParaMESH.TCAN(1, GRU), "TCAN", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("TCAN"), ParaMESH.TCAN(0, GRU), "TCAN", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.TSNO(0, GRU), ParaMESH.GetDefault("TSNO"), "TSNO", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.TSNO(0, GRU), ParaMESH.TSNO(1, GRU), "TSNO", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("TSNO"), ParaMESH.TSNO(0, GRU), "TSNO", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.TPND(0, GRU), ParaMESH.GetDefault("TPND"), "TPND", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.TPND(0, GRU), ParaMESH.TPND(1, GRU), "TPND", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("TPND"), ParaMESH.TPND(0, GRU), "TPND", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.ZPND(0, GRU), ParaMESH.GetDefault("ZPND"), "ZPND", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.ZPND(0, GRU), ParaMESH.ZPND(1, GRU), "ZPND", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("ZPND"), ParaMESH.ZPND(0, GRU), "ZPND", GRU)
        End If
        For a = 0 To ParaMESH.THLQ.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.THLQ(0, a, GRU), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "THLQ")), ParaMESH.SecondaryName(a, "THLQ"), GRU, a, True)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.THLQ(0, a, GRU), ParaMESH.THLQ(1, a, GRU), ParaMESH.SecondaryName(a, "THLQ"), GRU, a, True)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "THLQ")), ParaMESH.THLQ(0, a, GRU), ParaMESH.SecondaryName(a, "THLQ"), GRU, a, True)
            End If
        Next
        For a = 0 To ParaMESH.THIC.GetUpperBound(1)
            If CallProcess = "CallDefault" Then
                ParaSTOR.AppendList(ParaMESH.THIC(0, a, GRU), ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "THIC")), ParaMESH.SecondaryName(a, "THIC"), GRU, a, True)
            ElseIf CallProcess = "CallBackup" Then
                ParaSTOR.AppendList(ParaMESH.THIC(0, a, GRU), ParaMESH.THIC(1, a, GRU), ParaMESH.SecondaryName(a, "THIC"), GRU, a, True)
            ElseIf CallProcess = "SetDefault" Then
                ParaSTOR.AppendList(ParaMESH.GetDefault(ParaMESH.SecondaryName(a, "THIC")), ParaMESH.THIC(0, a, GRU), ParaMESH.SecondaryName(a, "THIC"), GRU, a, True)
            End If
        Next
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RCAN(0, GRU), ParaMESH.GetDefault("RCAN"), "RCAN", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RCAN(0, GRU), ParaMESH.RCAN(1, GRU), "RCAN", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RCAN"), ParaMESH.RCAN(0, GRU), "RCAN", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.SCAN(0, GRU), ParaMESH.GetDefault("SCAN"), "SCAN", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.SCAN(0, GRU), ParaMESH.SCAN(1, GRU), "SCAN", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("SCAN"), ParaMESH.SCAN(0, GRU), "SCAN", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.SNO(0, GRU), ParaMESH.GetDefault("SNO"), "SNO", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.SNO(0, GRU), ParaMESH.SNO(1, GRU), "SNO", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("SNO"), ParaMESH.SNO(0, GRU), "SNO", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.ALBS(0, GRU), ParaMESH.GetDefault("ALBS"), "ALBS", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.ALBS(0, GRU), ParaMESH.ALBS(1, GRU), "ALBS", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("ALBS"), ParaMESH.ALBS(0, GRU), "ALBS", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RHOS(0, GRU), ParaMESH.GetDefault("RHOS"), "RHOS", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RHOS(0, GRU), ParaMESH.RHOS(1, GRU), "RHOS", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RHOS"), ParaMESH.RHOS(0, GRU), "RHOS", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.GRO(0, GRU), ParaMESH.GetDefault("GRO"), "GRO", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.GRO(0, GRU), ParaMESH.GRO(1, GRU), "GRO", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("GRO"), ParaMESH.GRO(0, GRU), "GRO", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC13(0, GRU), ParaMESH.GetDefault("RSERVC13"), "RSERVC13", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC13(0, GRU), ParaMESH.RSERVC13(1, GRU), "RSERVC13", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC13"), ParaMESH.RSERVC13(0, GRU), "RSERVC13", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC14(0, GRU), ParaMESH.GetDefault("RSERVC14"), "RSERVC14", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC14(0, GRU), ParaMESH.RSERVC14(1, GRU), "RSERVC14", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC14"), ParaMESH.RSERVC14(0, GRU), "RSERVC14", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC15(0, GRU), ParaMESH.GetDefault("RSERVC15"), "RSERVC15", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC15(0, GRU), ParaMESH.RSERVC15(1, GRU), "RSERVC15", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC15"), ParaMESH.RSERVC15(0, GRU), "RSERVC15", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC16(0, GRU), ParaMESH.GetDefault("RSERVC16"), "RSERVC16", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC16(0, GRU), ParaMESH.RSERVC16(1, GRU), "RSERVC16", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC16"), ParaMESH.RSERVC16(0, GRU), "RSERVC16", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC17(0, GRU), ParaMESH.GetDefault("RSERVC17"), "RSERVC17", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC17(0, GRU), ParaMESH.RSERVC17(1, GRU), "RSERVC17", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC17"), ParaMESH.RSERVC17(0, GRU), "RSERVC17", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC18(0, GRU), ParaMESH.GetDefault("RSERVC18"), "RSERVC18", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC18(0, GRU), ParaMESH.RSERVC18(1, GRU), "RSERVC18", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC18"), ParaMESH.RSERVC18(0, GRU), "RSERVC18", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC19(0, GRU), ParaMESH.GetDefault("RSERVC19"), "RSERVC19", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC19(0, GRU), ParaMESH.RSERVC19(1, GRU), "RSERVC19", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC19"), ParaMESH.RSERVC19(0, GRU), "RSERVC19", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC20(0, GRU), ParaMESH.GetDefault("RSERVC20"), "RSERVC20", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC20(0, GRU), ParaMESH.RSERVC20(1, GRU), "RSERVC20", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC20"), ParaMESH.RSERVC20(0, GRU), "RSERVC20", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC21(0, GRU), ParaMESH.GetDefault("RSERVC21"), "RSERVC21", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC21(0, GRU), ParaMESH.RSERVC21(1, GRU), "RSERVC21", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC21"), ParaMESH.RSERVC21(0, GRU), "RSERVC21", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC22(0, GRU), ParaMESH.GetDefault("RSERVC22"), "RSERVC22", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC22(0, GRU), ParaMESH.RSERVC22(1, GRU), "RSERVC22", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC22"), ParaMESH.RSERVC22(0, GRU), "RSERVC22", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC23(0, GRU), ParaMESH.GetDefault("RSERVC23"), "RSERVC23", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC23(0, GRU), ParaMESH.RSERVC23(1, GRU), "RSERVC23", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC23"), ParaMESH.RSERVC23(0, GRU), "RSERVC23", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC24(0, GRU), ParaMESH.GetDefault("RSERVC24"), "RSERVC24", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC24(0, GRU), ParaMESH.RSERVC24(1, GRU), "RSERVC24", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC24"), ParaMESH.RSERVC24(0, GRU), "RSERVC24", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC25(0, GRU), ParaMESH.GetDefault("RSERVC25"), "RSERVC25", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC25(0, GRU), ParaMESH.RSERVC25(1, GRU), "RSERVC25", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC25"), ParaMESH.RSERVC25(0, GRU), "RSERVC25", GRU)
        End If
        If CallProcess = "CallDefault" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC26(0, GRU), ParaMESH.GetDefault("RSERVC26"), "RSERVC26", GRU)
        ElseIf CallProcess = "CallBackup" Then
            ParaSTOR.AppendList(ParaMESH.RSERVC26(0, GRU), ParaMESH.RSERVC26(1, GRU), "RSERVC26", GRU)
        ElseIf CallProcess = "SetDefault" Then
            ParaSTOR.AppendList(ParaMESH.GetDefault("RSERVC26"), ParaMESH.RSERVC26(0, GRU), "RSERVC26", GRU)
        End If
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaSTOR.RedimParameter
    '*****************************************************************************
    'UPDATED: APR 23 2008 (DAN) to use ParaMESH(SecondaryCount)
    '*****************************************************************************
    'Redimensions parameter values to their default dimensions.
    '*****************************************************************************

    Private Shared Sub RedimParameter(Optional ByVal ParameterCall = Nothing)

        '***FIRST BLOCK
        Try
            ReDim Preserve ParaMESH.FCAN(ParaMESH.FCAN.GetUpperBound(0), ParaMESH.SecondaryCount("FCAN"), 0)
        Catch ex As Exception
            ReDim Preserve ParaMESH.FCAN(ParaMESH.FCAN.GetUpperBound(0), ParaMESH.FCAN.GetUpperBound(1), 0)
        End Try
        Try
            ReDim Preserve ParaMESH.LNZ0(ParaMESH.LNZ0.GetUpperBound(0), ParaMESH.SecondaryCount("LNZ0"), 0)
        Catch ex As Exception
            ReDim Preserve ParaMESH.LNZ0(ParaMESH.LNZ0.GetUpperBound(0), ParaMESH.LNZ0.GetUpperBound(1), 0)
        End Try
        Try
            ReDim Preserve ParaMESH.ALVC(ParaMESH.ALVC.GetUpperBound(0), ParaMESH.SecondaryCount("ALVC"), 0)
        Catch ex As Exception
            ReDim Preserve ParaMESH.ALVC(ParaMESH.ALVC.GetUpperBound(0), ParaMESH.ALVC.GetUpperBound(1), 0)
        End Try
        Try
            ReDim Preserve ParaMESH.ALIC(ParaMESH.ALIC.GetUpperBound(0), ParaMESH.SecondaryCount("ALIC"), 0)
        Catch ex As Exception
            ReDim Preserve ParaMESH.ALIC(ParaMESH.ALIC.GetUpperBound(0), ParaMESH.ALIC.GetUpperBound(1), 0)
        End Try
        Try
            ReDim Preserve ParaMESH.LAMX(ParaMESH.LAMX.GetUpperBound(0), ParaMESH.SecondaryCount("LAMX"), 0)
        Catch ex As Exception
            ReDim Preserve ParaMESH.LAMX(ParaMESH.LAMX.GetUpperBound(0), ParaMESH.LAMX.GetUpperBound(1), 0)
        End Try
        Try
            ReDim Preserve ParaMESH.LAMN(ParaMESH.LAMN.GetUpperBound(0), ParaMESH.SecondaryCount("LAMN"), 0)
        Catch ex As Exception
            ReDim Preserve ParaMESH.LAMN(ParaMESH.LAMN.GetUpperBound(0), ParaMESH.LAMN.GetUpperBound(1), 0)
        End Try
        Try
            ReDim Preserve ParaMESH.CMAS(ParaMESH.CMAS.GetUpperBound(0), ParaMESH.SecondaryCount("CMAS"), 0)
        Catch ex As Exception
            ReDim Preserve ParaMESH.CMAS(ParaMESH.CMAS.GetUpperBound(0), ParaMESH.CMAS.GetUpperBound(1), 0)
        End Try
        Try
            ReDim Preserve ParaMESH.ROOT(ParaMESH.ROOT.GetUpperBound(0), ParaMESH.SecondaryCount("ROOT"), 0)
        Catch ex As Exception
            ReDim Preserve ParaMESH.ROOT(ParaMESH.ROOT.GetUpperBound(0), ParaMESH.ROOT.GetUpperBound(1), 0)
        End Try
        Try
            ReDim Preserve ParaMESH.RSMN(ParaMESH.RSMN.GetUpperBound(0), ParaMESH.SecondaryCount("RSMN"), 0)
        Catch ex As Exception
            ReDim Preserve ParaMESH.RSMN(ParaMESH.RSMN.GetUpperBound(0), ParaMESH.RSMN.GetUpperBound(1), 0)
        End Try
        Try
            ReDim Preserve ParaMESH.QA50(ParaMESH.QA50.GetUpperBound(0), ParaMESH.SecondaryCount("QA50"), 0)
        Catch ex As Exception
            ReDim Preserve ParaMESH.QA50(ParaMESH.QA50.GetUpperBound(0), ParaMESH.QA50.GetUpperBound(1), 0)
        End Try
        Try
            ReDim Preserve ParaMESH.VPDA(ParaMESH.VPDA.GetUpperBound(0), ParaMESH.SecondaryCount("VPDA"), 0)
        Catch ex As Exception
            ReDim Preserve ParaMESH.VPDA(ParaMESH.VPDA.GetUpperBound(0), ParaMESH.VPDA.GetUpperBound(1), 0)
        End Try
        Try
            ReDim Preserve ParaMESH.VPDB(ParaMESH.VPDB.GetUpperBound(0), ParaMESH.SecondaryCount("VPDB"), 0)
        Catch ex As Exception
            ReDim Preserve ParaMESH.VPDB(ParaMESH.VPDB.GetUpperBound(0), ParaMESH.VPDB.GetUpperBound(1), 0)
        End Try
        Try
            ReDim Preserve ParaMESH.PSGA(ParaMESH.PSGA.GetUpperBound(0), ParaMESH.SecondaryCount("PSGA"), 0)
        Catch ex As Exception
            ReDim Preserve ParaMESH.PSGA(ParaMESH.PSGA.GetUpperBound(0), ParaMESH.PSGA.GetUpperBound(1), 0)
        End Try
        Try
            ReDim Preserve ParaMESH.PSGB(ParaMESH.PSGB.GetUpperBound(0), ParaMESH.SecondaryCount("PSGB"), 0)
        Catch ex As Exception
            ReDim Preserve ParaMESH.PSGB(ParaMESH.PSGB.GetUpperBound(0), ParaMESH.PSGB.GetUpperBound(1), 0)
        End Try

        '***SECOND BLOCK
        Try
            ReDim Preserve ParaMESH.SAND(ParaMESH.SAND.GetUpperBound(0), ParaMESH.SecondaryCount("SAND"), 0)
        Catch ex As Exception
            ReDim Preserve ParaMESH.SAND(ParaMESH.SAND.GetUpperBound(0), ParaMESH.SAND.GetUpperBound(1), 0)
        End Try
        Try
            ReDim Preserve ParaMESH.CLAY(ParaMESH.CLAY.GetUpperBound(0), ParaMESH.SecondaryCount("CLAY"), 0)
        Catch ex As Exception
            ReDim Preserve ParaMESH.CLAY(ParaMESH.CLAY.GetUpperBound(0), ParaMESH.CLAY.GetUpperBound(1), 0)
        End Try
        Try
            ReDim Preserve ParaMESH.ORGM(ParaMESH.ORGM.GetUpperBound(0), ParaMESH.SecondaryCount("ORGM"), 0)
        Catch ex As Exception
            ReDim Preserve ParaMESH.ORGM(ParaMESH.ORGM.GetUpperBound(0), ParaMESH.ORGM.GetUpperBound(1), 0)
        End Try
        Try
            ReDim Preserve ParaMESH.TBAR(ParaMESH.TBAR.GetUpperBound(0), ParaMESH.SecondaryCount("TBAR"), 0)
        Catch ex As Exception
            ReDim Preserve ParaMESH.TBAR(ParaMESH.TBAR.GetUpperBound(0), ParaMESH.TBAR.GetUpperBound(1), 0)
        End Try
        Try
            ReDim Preserve ParaMESH.THLQ(ParaMESH.THLQ.GetUpperBound(0), ParaMESH.SecondaryCount("THLQ"), 0)
        Catch ex As Exception
            ReDim Preserve ParaMESH.THLQ(ParaMESH.THLQ.GetUpperBound(0), ParaMESH.THLQ.GetUpperBound(1), 0)
        End Try
        Try
            ReDim Preserve ParaMESH.THIC(ParaMESH.THIC.GetUpperBound(0), ParaMESH.SecondaryCount("THIC"), 0)
        Catch ex As Exception
            ReDim Preserve ParaMESH.THIC(ParaMESH.THIC.GetUpperBound(0), ParaMESH.THIC.GetUpperBound(1), 0)
        End Try

        '***HYDROLOGIC PARAMETERS
        Try
            ReDim ParaMESH.WF_R2(ParaMESH.WF_R2.GetUpperBound(1), ParaMESH.SecondaryCount("WF_R2"))
        Catch ex As Exception
            ReDim ParaMESH.WF_R2(ParaMESH.WF_R2.GetUpperBound(0), 0)
        End Try

        '***SECONDARY PARAMETERS
        Try
            ReDim ParaMESH.ControlFlag(ParaMESH.ControlFlag.GetUpperBound(0), ParaMESH.SecondaryCount("ControlFlag"))
        Catch ex As Exception
            ReDim ParaMESH.ControlFlag(ParaMESH.ControlFlag.GetUpperBound(0), 0)
        End Try
        Try
            ReDim ParaMESH.GridOutput(ParaMESH.GridOutput.GetUpperBound(0), ParaMESH.SecondaryCount("GridOutput"), 0)
        Catch ex As Exception
            ReDim ParaMESH.GridOutput(ParaMESH.GridOutput.GetUpperBound(0), ParaMESH.GridOutput.GetUpperBound(1), 0)
        End Try
        Try
            ReDim ParaMESH.OptionFlag(ParaMESH.OptionFlag.GetUpperBound(0), ParaMESH.SecondaryCount("OptionFlag"))
        Catch ex As Exception
            ReDim ParaMESH.OptionFlag(ParaMESH.OptionFlag.GetUpperBound(0), 0)
        End Try
        Try
            ReDim ParaMESH.IndependentGRU(ParaMESH.IndependentGRU.GetUpperBound(0), ParaMESH.SecondaryCount("IndependentGRU"))
        Catch ex As Exception
            ReDim ParaMESH.IndependentGRU(ParaMESH.IndependentGRU.GetUpperBound(0), 0)
        End Try
        Try
            ReDim ParaMESH.DependentGRU(ParaMESH.DependentGRU.GetUpperBound(0), ParaMESH.SecondaryCount("DependentGRU"), 0)
        Catch ex As Exception
            ReDim ParaMESH.DependentGRU(ParaMESH.DependentGRU.GetUpperBound(0), ParaMESH.DependentGRU.GetUpperBound(1), 0)
        End Try

        '***WATFLOW PARAMETERS
        Try
            ReDim ParaMESH.WatflowFlag(ParaMESH.WatflowFlag.GetUpperBound(0), ParaMESH.SecondaryCount("WatflowFlag"))
        Catch ex As Exception
            ReDim ParaMESH.WatflowFlag(ParaMESH.WatflowFlag.GetUpperBound(0), 0)
        End Try
        Try
            ReDim ParaMESH.D100A(ParaMESH.D100A.GetUpperBound(0), ParaMESH.SecondaryCount("D100A"))
        Catch ex As Exception
            ReDim ParaMESH.D100A(ParaMESH.D100A.GetUpperBound(0), 0)
        End Try
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaSTOR.CrossFields (not used)
    '*****************************************************************************
    'Used to cross-reference shared parameters between environments.
    '*****************************************************************************

    Public Shared Sub CrossFields()
        SplitArray = ParaLOAD.SystemProperties("SystemHierarchy").ToString.Split(">")
        If SplitArray.GetUpperBound(0) < 1 Then
            Exit Sub
        End If
        ReDim CrossArray(SplitArray.GetUpperBound(0), ParaMESH.CrossFields.GetUpperBound(1) - 1)
        With CrossForm
            For b = 0 To CrossArray.GetUpperBound(1)
                For a = 0 To CrossArray.GetUpperBound(0)
                    CrossArray(a, b) = ParaMESH.CrossFields(ParaMESH.FindColumn(SplitArray(a), ParaMESH.CrossFields), b + 1)
                    .Arr_CrossList.RowCount = a + 1
                    .Arr_CrossList.Item("Grd_ParameterName", a).Value = CrossArray(a, b)
                    If a = 0 Then
                        .Arr_CrossList.Item("Grd_PreserveCheck", a).Value = True
                    End If
                Next
                If .Cmd_ListView.Text = "List View" Then
                    .Grd_PreserveCheck.Visible = True
                    .Grd_PreserveList.Visible = False
                    .ShowDialog()

                    '***SAVE VALUE
                    For a = 0 To .Arr_CrossList.RowCount - 1
                        If .Arr_CrossList.Item("Grd_PreserveCheck", a).Value = True Then
                            Select Case .Arr_CrossList.Item("Grd_ParameterCall", a).Value
                                Case Is = ""
                            End Select
                        End If
                    Next
                End If
            Next
            If .Cmd_ListView.Text = "Single View" Then
                .Arr_CrossList.RowCount = CrossArray.GetUpperBound(1) + 1
                .Grd_PreserveCheck.Visible = False
                .Grd_PreserveList.Visible = True
            End If
        End With
    End Sub
End Class
