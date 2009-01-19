Public Class ParaLOG

    '*****************************************************************************
    'CLASS: ParaLOG
    '*****************************************************************************
    'ParaLOG is used to handle all ParaMESH logging processes.  ParaLOG also 
    'contains the subroutine used to write the ParaMESH log file (it is a write 
    'subroutine that has not been included with ParaWRITE).
    '*****************************************************************************

    '****************************************************************************
    'PRIVATE ParaLOG PARAMETERS
    '****************************************************************************
    'Parameters shared and used only within the ParaREAD class.
    '****************************************************************************
    'Private Integers: f is unique to the ParaLOG class so that 
    'similarly private integers from calling subroutines are not reset.
    '****************************************************************************

    Private Shared f As Integer

    '*****************************************************************************
    'SUBROUTINE: ParaLOG.AppendFile
    '*****************************************************************************
    'UPDATED: APR 22 2008 (DAN) to use common ParaMESH subroutines.
    'UPDATED: FEB 05 2008 (DAN) added "LoadVar" as PreFormKey
    'UPDATED: JAN 14 2008 (DAN) to write processes to LogForm
    '*****************************************************************************
    'Receives Process string to write to ParaMESH log file, CallParent 
    '(subroutine) which calls the process, optional PreFormKey to flag processes 
    'for ParaMESH parameters (variables or arrays).
    'Keeps a record of a fixed number of processes based on the upper-bound limit 
    'of the ParaMESH.LogFile array.
    'Tracks the system time when the process was called.
    'Saves the ParaMESH log history to the shared ParaMESH.LogFile array
    '*****************************************************************************

    Public Shared Sub AppendFile(ByVal Process As String, ByVal CallSource As String, Optional ByVal PreFormKey As String = Nothing)

        '***DO NOTHING IF ..SystemParameters(StopLog) IS ENABLED
        If ParaMESH.IsEnabled("StopLOG", ParaMESH.SystemParameters, 1) = True Then
            Exit Sub
        End If

        '***WRITE THE PROCESS HEADER
        ParaMESH.LogFile(ParaMESH.LogFile.GetUpperBound(0)) = "Process: "

        '***WRITE THE PROCESS
        Select Case PreFormKey
            Case Is = Nothing
                ParaMESH.LogFile(ParaMESH.LogFile.GetUpperBound(0)) &= Process
            Case Else

                '***SPECIAL PARAMETER PROCESSES CALLED BY PreFormKey
                Select Case PreFormKey
                    Case Is = "DimArr"

                        '***DIMENSIONING AN ARRAY
                        ParaMESH.LogFile(ParaMESH.LogFile.GetUpperBound(0)) &= "ParaMESH is dimensioning the array: "
                    Case Is = "PopVar"

                        '***POPULATING A PARAMETER
                        ParaMESH.LogFile(ParaMESH.LogFile.GetUpperBound(0)) &= "ParaMESH is populating the parameter: "
                    Case Is = "ValVar"

                        '***VALIDATING A PARAMETER
                        ParaMESH.LogFile(ParaMESH.LogFile.GetUpperBound(0)) &= "ParaMESH is checking the parameter: "
                    Case Is = "SavVar"

                        '***SAVING A PARAMETER VALUE
                        ParaMESH.LogFile(ParaMESH.LogFile.GetUpperBound(0)) &= "ParaMESH is saving the parameter: "
                    Case Is = "WrtVar"

                        '***WRITING THE PARAMETER VALUE
                        ParaMESH.LogFile(ParaMESH.LogFile.GetUpperBound(0)) &= "ParaMESH is writing the parameter: "

                    Case Is = "LoadVar"

                        '***LOADING THE PARAMETER VALUE
                        ParaMESH.LogFile(ParaMESH.LogFile.GetUpperBound(0)) &= "ParaMESH is loading the parameter: "
                End Select

                '***DETERMINE IF VARIABLE IS PRIMARY OR SECONDARY PARAMETER
                If ParaMESH.FindIndex(Process.Substring(0, Process.IndexOf("#")), ParaMESH.PrimaryParameters) > -1 Then
                    ParaMESH.LogFile(ParaMESH.LogFile.GetUpperBound(0)) &= ParaMESH.PrimaryParameters(4, ParaMESH.FindIndex(Process.Substring(0, Process.IndexOf("#")).Trim, ParaMESH.PrimaryParameters))
                ElseIf ParaMESH.FindIndex(Process.Substring(0, Process.IndexOf("#")), ParaMESH.SecondaryParameters) > -1 Then
                    ParaMESH.LogFile(ParaMESH.LogFile.GetUpperBound(0)) &= ParaMESH.SecondaryParameters(4, ParaMESH.FindIndex(Process.Substring(0, Process.IndexOf("#")).Trim, ParaMESH.SecondaryParameters))
                End If

                '***WRITE THE PARAMETER CALL
                ParaMESH.LogFile(ParaMESH.LogFile.GetUpperBound(0)) &= " (" & Process.Substring(0, Process.IndexOf("#")).Trim & ") " & Process.Substring(Process.IndexOf("#") + 1)
        End Select

        '***WRITE THE PROCESS FOOTER, THE SUBROUTINE WHICH CALLED THE PROCESS, THE SYSTEM TIME
        ParaMESH.LogFile(ParaMESH.LogFile.GetUpperBound(0)) &= " ..Called By: " & CallSource & " ..System Time: " & System.DateTime.Now.ToString

        '***INSERT PLACEHOLDER FOR NEW RECORD IF ParaMESH.SystemParameters(ParaLOG) IS Enabled
        '***OTHERWISE, ParaMESH ONLY KEEPS RECORD OF A SINGLE PROCESS
        If ParaMESH.SystemParameters(1, ParaMESH.FindIndex("ParaLOG", ParaMESH.SystemParameters)) = "Enabled" Then
            ReDim Preserve ParaMESH.LogFile(ParaMESH.LogFile.GetUpperBound(0) + 1)
        End If

        '***WRITE ParaMESH(LogFile) TO ParaMESH PROCESS LOG TABLE IF ParaMESH.SystemParameters(ParaLOG) IS Enabled
        If ParaMESH.SystemParameters(1, ParaMESH.FindIndex("ParaLOG", ParaMESH.SystemParameters)) = "Enabled" Then
            With LogForm
                .Arr_LogForm.Rows.Insert(0)
                .Arr_LogForm.Item("Grd_LogMessage", 0).Value = ParaMESH.LogFile(ParaMESH.LogFile.GetUpperBound(0) - 1)
                .Box_Count.Text = ParaMESH.LogFile.GetUpperBound(0)
            End With
        End If
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaLOG.ClearFile
    '*****************************************************************************
    'Clears ParaMESH(LogFile) which keeps track of all ParaMESH processes.
    '*****************************************************************************

    Public Shared Sub ClearFile()

        '***DO NOTHING IF ..SystemParameters(ParaLOG) IS DISABLED
        If ParaMESH.SystemParameters(1, ParaMESH.FindIndex("ParaLOG", ParaMESH.SystemParameters)) = "Disabled" Then
            Exit Sub
        End If

        '***CLEAR ParaMESH(LogFile) AND ParaMESH PROCESS LOG TABLE
        ReDim ParaMESH.LogFile(0)
        LogForm.Arr_LogForm.Rows.Clear()
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaLOG.WriteFile
    '*****************************************************************************
    'UPDATED: APR 22 2008 (DAN) to use common ParaMESH subroutines.
    '*****************************************************************************
    'Writes the ParaMESH.LogFile array to the ParaMESH.log file in the 
    'application's root directory (StartupPath)
    '*****************************************************************************

    Public Shared Sub WriteFile()

        '***DO NOTHING IF ..SystemParameters(ParaLOG) IS DISABLED
        If ParaMESH.IsEnabled("ParaLOG", ParaMESH.SystemParameters, 1) = "Disabled" Then
            Exit Sub
        End If

        '***WRITE TO Application(StartupPath) DIRECTORY
        Dim WriteFile As New System.IO.StreamWriter(Application.StartupPath & "\ParaLOG.txt")

        '***WRITE THE NUMBER OF ENTRIES AND SYSTEM TIME
        WriteFile.WriteLine("#ParaMESH: Process Log [" & (ParaMESH.LogFile.GetUpperBound(0) + 1).ToString & "] Entries ..System Time: " & System.DateTime.Now.ToString)

        '***REVERSE ParaMESH(LogFile) SO THAT MOST RECENT LOG IS AT TOP
        System.Array.Reverse(ParaMESH.LogFile)

        '***WRITE THE LOG ENTRIES (1 BECAUSE FIRST ROW IS ALWAYS EMPTY)
        For f = 1 To ParaMESH.LogFile.GetUpperBound(0)
            WriteFile.WriteLine(ParaMESH.LogFile(f))
        Next
        WriteFile.Close()

        '***CALL notepad.exe TO OPEN ParaLOG.txt
        System.Diagnostics.Process.Start(System.Environment.SystemDirectory & "\notepad.exe", Application.StartupPath & "\ParaLOG.txt")

        '***REVERT ParaMESH(LogFile) SO THAT MOST RECENT LOG IS AT BOTTOM
        System.Array.Reverse(ParaMESH.LogFile)
    End Sub
End Class