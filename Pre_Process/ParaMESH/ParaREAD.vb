Public Class ParaREAD

    '*****************************************************************************
    'CLASS: ParaREAD
    '*****************************************************************************
    'ParaREAD is used to read configuration files, and holds all subroutines 
    'used by ParaMESH to read any external file.
    '*****************************************************************************

    '****************************************************************************
    'PRIVATE ParaREAD PARAMETERS
    '****************************************************************************
    'Parameters shared and used only within the ParaREAD class.
    '****************************************************************************
    'Message Box Results: used to call user-directed prompt messages.
    'Private Integers: i, j, k, m are unique to the ParaREAD class so that 
    'similarly private integers from calling subroutines are not reset.
    '****************************************************************************

    Private Shared LoadBak As New MsgBoxResult
    Private Shared CatchArray(4, 0) As String, TempArray(0) As String, Line() As String, i As Integer, j As Integer, k As Integer, m As Integer

    '*****************************************************************************
    'SUBROUTINE: ParaREAD.Arguments
    '*****************************************************************************
    'ParaREAD subroutines may be called by an external thread, which will only 
    'accept a single argument.  This subroutine is used to compile all arguments 
    'required by read subroutines into a single object array.
    'CallProcess denotes which read subroutine will be called.
    '*****************************************************************************

    Public Shared Function Arguments(ByVal CallProcess As String) As Object
        ReDim TempArray(0)

        '***POPULATE TempArray WITH ARGUMENTS FOR ProcessForm(Ext_ParaREAD)
        TempArray(0) = CallProcess

        '***RETURN TempArray
        Arguments = TempArray
    End Function

    '*****************************************************************************
    'SUBROUTINE: ParaREAD.MESH_RunOptions
    '*****************************************************************************
    'UPDATED: APR 22 2008 (DAN)  to use common ParaMESH subroutines.
    'UPDATED: JAN 09 2008 (DAN)  for ParaLOG approach.
    '*****************************************************************************
    'Read the run_options.ini configuration file for MESH.  The FileName and 
    'FileHeader are passed by the calling subroutine.  If AcceptVariants is True, 
    'then read any file with the defined FileName; otherwise, ensure the file 
    'contains the FileHeader before the remainder of the file is read.
    '*****************************************************************************

    Public Shared Sub MESH_RunOptions(ByVal FileName As String, Optional ByVal FileHeader As String = Nothing, Optional ByVal AcceptVariants As Boolean = True)

        '***CALL ParaREAD(FileExists) TO CHECK IF FileName EXISTS
        If ParaREAD.FileExists(FileName) = False Then
            Exit Sub
        End If

        '***CALL ParaREAD(ReadFile) TO READ THE CONFIGURATION FILE
        TempArray = ParaREAD.ReadFile(FileName)

        '***CALL ParaREAD(ValidFile) TO CHECK THAT CONTENTS OF FileName EXIST
        If ParaREAD.ValidFile(FileName, TempArray) = False Then
            Exit Sub
        End If

        '***CALL ParaREAD(ValidHeader) TO CHECK IF FileName MUST HAVE A VALID HEADER TO BE READ
        If ParaREAD.ValidHeader(FileName, FileHeader, AcceptVariants) = False Then
            Exit Sub
        End If

        '***REDIMENSION ParaREAD(CatchArray)
        ReDim ParaREAD.CatchArray(ParaREAD.CatchArray.GetUpperBound(0), 0)

        '***READ AND SAVE CONTROL FLAGS
        ParaLOG.AppendFile("ParaMESH is preparing to load the control flags: " & FileName, "ParaREAD.MESH_RunOptions")
        For i = 0 To TempArray.GetUpperBound(0) - 1
            If TempArray(i).Substring(0, 5) = "#####" Then
                If ParaMESH.IsEnabled("ControlFlag", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaLOG.AppendFile("ControlFlag#: " & FileName, "ParaREAD.MESH_RunOptions", "DimArr")
                        ReDim ParaMESH.ControlFlag(ParaMESH.ControlFlag.GetUpperBound(0), Convert.ToInt32(TempArray(i + 2).Substring(0, 5).Trim) - 1)
                        For j = 0 To ParaMESH.ControlFlag.GetUpperBound(1)
                            Try
                                ParaLOG.AppendFile(ParaMESH.SecondaryName(j, "ControlFlag") & "# ..reading Line " & (j + 1).ToString & ", Column 1: " & FileName, "ParaREAD.MESH_RunOptions", "PopVar")
                                For k = 0 To ParaMESH.ControlFlag.GetUpperBound(0)
                                    ParaSAVE.ControlFlag(ParaMESH.SecondaryName(j, "ControlFlag"), TempArray(j + i + 3).Substring(0, 5).Trim, k)
                                Next
                            Catch ex As Exception
                                ParaREAD.AppendArray(TempArray(j + i + 3).Substring(0, 5).Trim, ParaMESH.SecondaryName(j, "ControlFlag"), j)
                            End Try
                        Next
                        Exit For
                    Catch ex As Exception

                        '***OPEN FileName ON READ ERROR
                        MsgBox("The number of Control Flags in the configuration file is invalid.  Check that a valid integer value has been entered in Column 1 of Line " & (i + 2).ToString & " of: " & FileName, MsgBoxStyle.OkOnly & ", and then re-load the file.", "ParaMESH")
                        System.Diagnostics.Process.Start("notepad.exe", ParaMESH.PathRoot & "\" & FileName)
                    End Try
                End If
            End If
        Next

        '***READ AND SAVE GRID OUTPUTS
        ParaLOG.AppendFile("ParaMESH is preparing to load the grid output points: " & FileName, "ParaREAD.MESH_RunOptions")
        For i = i + 1 To TempArray.GetUpperBound(0) - 1
            If TempArray(i).Substring(0, 5) = "#####" Then
                If ParaMESH.IsEnabled("GridOutput", ParaMESH.PrimaryParameters) = True Then
                    Try
                        If Convert.ToInt32(TempArray(i + 2).Substring(0, 5).Trim) = 0 Then

                            '***IF NO GRID OUTPUT POINTS EXIST, ENABLE ..SystemParameters(NoOutput) FLAG
                            ReDim ParaMESH.GridOutput(ParaMESH.GridOutput.GetUpperBound(0), ParaMESH.GridOutput.GetUpperBound(1), 0)
                            ParaSAVE.SystemParameters("Enabled", "NoOutput")
                            Exit Try
                        End If

                        '***IF GRID OUTPUT POINTS EXIST
                        ParaLOG.AppendFile("GridOutput#: " & FileName, "ParaREAD.MESH_RunOptions", "DimArr")
                        ReDim ParaMESH.GridOutput(ParaMESH.GridOutput.GetUpperBound(0), ParaMESH.GridOutput.GetUpperBound(1), Convert.ToInt32(TempArray(i + 2).Substring(0, 5).Trim) - 1)
                        For k = 0 To ParaMESH.GridOutput.GetUpperBound(2)
                            For j = 0 To ParaMESH.GridOutput.GetUpperBound(1)
                                Try
                                    ParaLOG.AppendFile(ParaMESH.SecondaryName(j, "GridOutput") & "# ..reading Line " & (j + 1).ToString & ", Column " & (k + 1).ToString & ": " & FileName, "ParaREAD.MESH_RunOptions", "PopVar")
                                    For m = 0 To ParaMESH.GridOutput.GetUpperBound(0)
                                        ParaSAVE.GridOutput(ParaMESH.SecondaryName(j, "GridOutput"), TempArray(j + i + 4).Substring(0 + (k * 10), 10).Trim, k, m)
                                    Next
                                Catch ex As Exception
                                    ParaREAD.AppendArray(TempArray(j + i + 4).Substring(0 + (k * 10), 10).Trim, ParaMESH.SecondaryName(j, "GridOutput"), k, j, m)
                                End Try
                            Next
                        Next
                        Exit For
                    Catch ex As Exception

                        '***OPEN FileName ON READ ERROR
                        System.Diagnostics.Process.Start("notepad.exe", ParaMESH.PathRoot & "\" & FileName)
                        MsgBox("The number of Grid Output Points in the configuration file is invalid.  Check that a valid integer value has been entered in Column 1 of Line " & (i + 2).ToString & " of: " & FileName & ", and then re-load the file.", MsgBoxStyle.OkOnly, "ParaMESH")
                    End Try
                End If
            End If
        Next

        '***CALL ParaCHECK(Array) TO CHECK ParaREAD(CatchArray) FOR FILE READ ERRORS
        ParaCHECK.Array(ParaREAD.CatchArray)
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaREAD.MESH_HydrologyINI
    '*****************************************************************************
    'UPDATED: APR 22 2008 (DAN)  to use common ParaMESH subroutines.
    'UPDATED: JAN 09 2008 (DAN)  for ParaLOG approach.
    '*****************************************************************************
    'Read the hydrology.ini configuration file for MESH.  The FileName and 
    'FileHeader are passed by the calling subroutine.  If AcceptVariants is True, 
    'then read any file with the defined FileName; otherwise, ensure the file 
    'contains the FileHeader before the remainder of the file is read.
    '*****************************************************************************

    Public Shared Sub MESH_HydrologyINI(ByVal FileName As String, Optional ByVal FileHeader As String = Nothing, Optional ByVal AcceptVariants As Boolean = True)

        '***CALL ParaREAD(FileExists) TO CHECK IF FileName EXISTS
        If ParaREAD.FileExists(FileName) = False Then
            Exit Sub
        End If

        '***CALL ParaREAD(ReadFile) TO READ THE CONFIGURATION FILE
        TempArray = ParaREAD.ReadFile(FileName)

        '***CALL ParaREAD(ValidFile) TO CHECK THAT CONTENTS OF FileName EXIST
        If ParaREAD.ValidFile(FileName, TempArray) = False Then
            Exit Sub
        End If

        '***CALL ParaREAD(ValidHeader) TO CHECK IF FileName MUST HAVE A VALID HEADER TO BE READ
        If ParaREAD.ValidHeader(FileName, FileHeader, AcceptVariants) = False Then
            Exit Sub
        End If

        '***REDIMENSION ParaREAD(CatchArray)
        ReDim ParaREAD.CatchArray(ParaREAD.CatchArray.GetUpperBound(0), 0)

        '***READING AND SAVE OPTION FLAGS
        ParaLOG.AppendFile("ParaMESH is preparing to load the option flags: " & FileName, "ParaREAD.MESH_HydrologyINI")
        For i = 0 To TempArray.GetUpperBound(0) - 1
            If TempArray(i).Substring(0, 5) = "#####" Then
                If ParaMESH.IsEnabled("OptionFlag", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaLOG.AppendFile("OptionFlag#: " & FileName, "ParaREAD.MESH_HydrologyINI", "DimArr")
                        ReDim ParaMESH.OptionFlag(ParaMESH.OptionFlag.GetUpperBound(0), Convert.ToInt32(TempArray(i + 2).Substring(0, 5).Trim) - 1)
                        For j = 0 To ParaMESH.OptionFlag.GetUpperBound(1)
                            Try
                                ParaLOG.AppendFile(ParaMESH.SecondaryName(j, "OptionFlag") & "# ..reading Line " & (j + 1).ToString & ", Column 1: " & FileName, "ParaREAD.MESH_HydrologyINI", "PopVar")
                                For k = 0 To ParaMESH.OptionFlag.GetUpperBound(0)
                                    ParaSAVE.OptionFlag(ParaMESH.SecondaryName(j, "OptionFlag"), TempArray(j + i + 3).Substring(0, 5).Trim, k)
                                Next
                            Catch ex As Exception
                                ParaREAD.AppendArray(TempArray(j + i + 3).Substring(0, 5).Trim, ParaMESH.SecondaryName(j, "OptionFlag"), j)
                            End Try
                        Next
                        Exit For
                    Catch ex As Exception

                        '***OPEN FileName ON READ ERROR
                        MsgBox("The number of Option Flags in the configuration file is invalid.  Check that a valid integer value has been entered in Column 1 of Line " & (i + 2).ToString & " of: " & FileName & ", and then re-load the file.", MsgBoxStyle.OkOnly, "ParaMESH")
                        System.Diagnostics.Process.Start("notepad.exe", ParaMESH.PathRoot & "\" & FileName)
                    End Try
                End If
            End If
        Next

        '***READ AND SAVE CHANNEL ROUGHNESS FACTORS (WF_R2)
        '***CHECK FILE VERSION (HYDROLOGY.INI FILES WRITTEN BY ParaMESH THAT DO NOT HAVE WF_R2 VALUES HAVE MESH_driver RELEASE AND COLON IN FileHeader)
        If FileHeader.IndexOf(":") = -1 Then
            ParaLOG.AppendFile("ParaMESH is preparing to load the channel roughness factors (WF_R2): " & FileName, "ParaREAD.MESH_HydrologyINI")
            For i = i + 1 To TempArray.GetUpperBound(0) - 1
                If TempArray(i).Substring(0, 5) = "#####" Then
                    If ParaMESH.IsEnabled("WF_R2", ParaMESH.PrimaryParameters) = True Then
                        For j = 0 To ParaMESH.WF_R2.GetUpperBound(1)
                            Try
                                ParaLOG.AppendFile("WF_R2# ..reading Line 3, Column " & (j + 1).ToString & ": " & FileName, "ParaREAD.MESH_HydrologyINI", "PopVar")
                                For k = 0 To ParaMESH.WF_R2.GetUpperBound(0)
                                    ParaSAVE.WF_R2(TempArray(i + 2).Substring(0 + (6 * j), 6).Trim, j, k)
                                Next
                            Catch ex As Exception
                                ParaREAD.AppendArray(TempArray(i + 2).Substring(0 + (6 * j), 6).Trim, "WF_R2", j)
                            End Try
                        Next
                        Exit For
                    End If
                End If
            Next
        End If

        '***READ AND SAVE GRU-INDEPENDENT HYDROLOGIC PARAMETERS
        ParaLOG.AppendFile("ParaMESH is preparing to load the GRU-independent hydrologic parameters: " & FileName, "ParaREAD.MESH_HydrologyINI")
        For i = i + 1 To TempArray.GetUpperBound(0) - 1
            If TempArray(i).Substring(0, 5) = "#####" Then
                If ParaMESH.IsEnabled("IndependentGRU", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaLOG.AppendFile("IndependentGRU#: " & FileName, "ParaREAD.MESH_HydrologyINI", "DimArr")
                        ReDim ParaMESH.IndependentGRU(ParaMESH.IndependentGRU.GetUpperBound(0), Convert.ToInt32(TempArray(i + 2).Substring(0, 8).Trim) - 1)
                        For j = 0 To ParaMESH.IndependentGRU.GetUpperBound(1)
                            Try
                                ParaLOG.AppendFile(ParaMESH.SecondaryName(j, "IndependentGRU") & "# ..reading Line " & (j + 1).ToString & ", Column 1: " & FileName, "ParaREAD.MESH_HydrologyINI", "PopVar")
                                For k = 0 To ParaMESH.IndependentGRU.GetUpperBound(0)
                                    ParaSAVE.IndependentGRU(ParaMESH.SecondaryName(j, "IndependentGRU"), TempArray(j + i + 3).Substring(0, 8).Trim, k)
                                Next
                            Catch ex As Exception
                                ParaREAD.AppendArray(TempArray(j + i + 3).Substring(0, 8).Trim, ParaMESH.SecondaryName(j, "IndependentGRU"), j)
                            End Try
                        Next
                        Exit For
                    Catch ex As Exception

                        '***OPEN FileName ON READ ERROR
                        MsgBox("The number of GRU-Independent Hydrologic Parameters in the configuration file is invalid.  Check that a valid integer value has been entered in Column 1 of Line " & (i + 2).ToString & " of: " & FileName & ", and then re-load the file.", MsgBoxStyle.OkOnly, "ParaMESH")
                        System.Diagnostics.Process.Start("notepad.exe", ParaMESH.PathRoot & "\" & FileName)
                    End Try
                End If
            End If
        Next

        '***READ AND SAVE GRU-DEPENDENT HYDROLOGIC PARAMETERS
        ParaLOG.AppendFile("ParaMESH is preparing to load the GRU-dependent hydrologic parameters: " & FileName, "ParaREAD.MESH_HydrologyINI")
        For i = i + 1 To TempArray.GetUpperBound(0) - 1
            If TempArray(i).Substring(0, 5) = "#####" Then
                If ParaMESH.IsEnabled("DependentGRU", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaLOG.AppendFile("DependentGRU#: " & FileName, "ParaREAD.MESH_HydrologyINI", "DimArr")
                        ReDim ParaMESH.DependentGRU(ParaMESH.DependentGRU.GetUpperBound(0), Convert.ToInt32(TempArray(i + 3).Substring(0, 8).Trim) - 1, Convert.ToInt32(TempArray(i + 2).Substring(0, 8).Trim) - 1)
                        For k = 0 To ParaMESH.DependentGRU.GetUpperBound(2)
                            For j = 0 To ParaMESH.DependentGRU.GetUpperBound(1)
                                Try
                                    ParaLOG.AppendFile(ParaMESH.SecondaryName(j, "DependentGRU") & "# ..reading Line " & (j + 1).ToString & ", Column " & (k + 1).ToString & ": " & FileName, "ParaREAD.MESH_HydrologyINI", "PopVar")
                                    For m = 0 To ParaMESH.DependentGRU.GetUpperBound(0)
                                        ParaSAVE.DependentGRU(ParaMESH.SecondaryName(j, "DependentGRU"), TempArray(j + i + 5).Substring(0 + (k * 10), 10).Trim, k, m)
                                    Next
                                Catch ex As Exception
                                    ParaREAD.AppendArray(TempArray(j + i + 5).Substring(0 + (k * 10), 10).Trim, ParaMESH.SecondaryName(j, "DependentGRU"), k, j)
                                End Try
                            Next
                        Next
                        Exit For
                    Catch ex As Exception

                        '***OPEN FileName ON READ ERROR
                        MsgBox("The number of GRU-Dependent Hydrologic Parameters in the configuration file is invalid.  Check that a valid integer value has been entered in Column 1 of Line " & (i + 3).ToString & " and Line " & (i + 2).ToString & " of: " & FileName & ", and then re-load the file.", MsgBoxStyle.OkOnly, "ParaMESH")
                        System.Diagnostics.Process.Start("notepad.exe", ParaMESH.PathRoot & "\" & FileName)
                    End Try
                End If
            End If
        Next

        '***CALL ParaCHECK(Array) TO CHECK CatchArray FOR FILE READ ERRORS
        ParaCHECK.Array(CatchArray)
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaREAD.MESH_ClassINI
    '*****************************************************************************
    'UPDATED: APR 22 2008 (DAN) to use common ParaMESH subroutines.
    'UPDATED: JAN 09 2008 (DAN) for ParaLOG approach.
    '*****************************************************************************
    'Read the class.ini configuration file (for MESH).  The FileName and 
    'FileHeader are passed by the calling subroutine.  If AcceptVariants is True, 
    'then read any file with the defined FileName; otherwise, ensure the file 
    'contains the FileHeader before the remainder of the file is read.
    '*****************************************************************************

    Public Shared Sub MESH_ClassINI(ByVal FileName As String, Optional ByVal FileHeader As String = Nothing, Optional ByVal AcceptVariants As Boolean = True)

        '***CALL ParaREAD(FileExists) TO CHECK IF FileName EXISTS
        If ParaREAD.FileExists(FileName) = False Then
            Exit Sub
        End If

        '***CALL ParaREAD(ReadFile) TO READ THE CONFIGURATION FILE
        TempArray = ParaREAD.ReadFile(FileName)

        '***CALL ParaREAD(ValidFile) TO CHECK THAT CONTENTS OF FileName EXIST
        If ParaREAD.ValidFile(FileName, TempArray) = False Then
            Exit Sub
        End If

        '***CALL ParaREAD(ValidHeader) TO CHECK IF FileName MUST HAVE A VALID HEADER TO BE READ
        If ParaREAD.ValidHeader(FileName, FileHeader, AcceptVariants) = False Then
            Exit Sub
        End If

        '***REDIMENSION ParaREAD(CatchArray)
        ReDim ParaREAD.CatchArray(ParaREAD.CatchArray.GetUpperBound(0), 0)

        '***READ AND SAVE INFORMATION BLOCK PARAMETERS
        For j = 0 To 1
            If ParaMESH.IsEnabled("TITLE", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.TITLE(TempArray(0).Substring(0, 72).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(0).Substring(0, 72).Trim, "TITLE")
                End Try
            End If
            If ParaMESH.IsEnabled("NAME", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.NAME(TempArray(1).Substring(0, 72).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(1).Substring(0, 72).Trim, "NAME")
                End Try
            End If
            If ParaMESH.IsEnabled("PLACE", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.PLACE(TempArray(2).Substring(0, 72).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(2).Substring(0, 72).Trim, "PLACE")
                End Try
            End If
            If ParaMESH.IsEnabled("DEGLAT", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.DEGLAT(TempArray(3).Substring(0, 10).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(3).Substring(0, 10).Trim, "DEGLAT")
                End Try
            End If
            If ParaMESH.IsEnabled("DEGLON", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.DEGLON(TempArray(3).Substring(10, 10).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(3).Substring(10, 10).Trim, "DEGLON")
                End Try
            End If
            If ParaMESH.IsEnabled("ZRFM", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.ZRFM(TempArray(3).Substring(20, 10).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(3).Substring(20, 10).Trim, "ZRFM")
                End Try
            End If
            If ParaMESH.IsEnabled("ZRFH", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.ZRFH(TempArray(3).Substring(30, 10).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(3).Substring(30, 10).Trim, "ZRFH")
                End Try
            End If
            If ParaMESH.IsEnabled("ZBLD", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.ZBLD(TempArray(3).Substring(40, 10).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(3).Substring(40, 10).Trim, "ZBLD")
                End Try
            End If
            If ParaMESH.IsEnabled("GC", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.GC(TempArray(3).Substring(50, 7).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(3).Substring(50, 7).Trim, "GC")
                End Try
            End If
            If ParaMESH.IsEnabled("ILW", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.ILW(TempArray(3).Substring(57, 5).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(3).Substring(57, 5).Trim, "ILW")
                End Try
            End If
            If ParaMESH.IsEnabled("GRID", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.GRID(TempArray(3).Substring(62, 5).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(3).Substring(62, 5).Trim, "GRID")
                End Try
            End If
        Next

        '***CALL ParaCHECK(Array) TO CHECK CatchArray FOR FILE READ ERRORS
        ParaCHECK.Array(CatchArray)
        ReDim CatchArray(3, 0)

        '***READ THE NUMBER OF GRUS, REDIM THE GRU-RELATED PARAMETERS
        If ParaMESH.IsEnabled("GRU", ParaMESH.PrimaryParameters) = False Then

            '***MUST EXIT SUBROUTINE
            '***READING THE REST OF CLASS.INI RELIES ON THE NUMBER OF GRUS IN THE FILE
            Exit Sub
        Else
            Try

                '***CALL ParaMESH(RedimGRU) TO REDIM THE GRU-RELATED PARAMETERS
                ParaLOG.AppendFile("ParaMESH is preparing to load the GRUs ..reading Line 4, Column 9: " & FileName, "ParaREAD.MESH_ClassINI", "PopVar")
                ParaMESH.RedimGRU(Convert.ToInt32(TempArray(3).Substring(67, 5).Trim) - 1)
            Catch ex As Exception

                '***OPEN FileName ON ERROR
                MsgBox("The number of Grouped Response Units (GRUs) in the configuration file is invalid.  Check that a valid integer value has been entered in Column 9 of Line 4 of: " & FileName & ", and then re-load the file.", MsgBoxStyle.OkOnly, "ParaMESH")
                System.Diagnostics.Process.Start("notepad.exe", ParaMESH.PathRoot & "\" & FileName)

                '***MUST EXIT SUBROUTINE
                '***READING THE REST OF CLASS.INI RELIES ON THE NUMBER OF GRUS IN THE FILE
                Exit Sub
            End Try
        End If

        '***READ AND SAVE GRU-RELATED PARAMETERS
        For i = 0 To ParaMESH.GRU.GetUpperBound(1)

            '***CALL ParaMESH(SetDefaults) TO LOAD DEFAULT PARAMETER VALUES FOR ALL PARAMETERS IN THIS GRU
            ParaSTOR.Restore("CallDefault", "GRUForm", i, True)
            For j = 0 To ParaMESH.GRU.GetUpperBound(0)

                '***GRU DESCRIPTION
                ParaLOG.AppendFile("GRU# ..reading Line 5, Column 10 of Class " & (i + 1).ToString & ": " & FileName, "ParaREAD.MESH_ClassINI", "PopVar")
                If TempArray(4 + (15 * i)).Length > 76 Then
                    If TempArray(4 + (15 * i)).IndexOf("/") < -1 Then
                        ParaSAVE.GRU(TempArray(4 + (15 * i)).Substring(76, TempArray(4 + (15 * i)).IndexOf("/") - 1).Trim, i, j)
                    Else
                        ParaSAVE.GRU(TempArray(4 + (15 * i)).Substring(76).Trim, i, j)
                    End If
                End If

                '***READING FIRST BLOCK
                If ParaMESH.IsEnabled("FCAN", ParaMESH.PrimaryParameters) = True Then
                    For k = 0 To ParaMESH.FCAN.GetUpperBound(1)
                        Try
                            ParaSAVE.FCAN(TempArray(4 + (15 * i)).Substring(0 + (8 * k), 8).Trim, k, i, j)
                        Catch ex As Exception
                            ParaREAD.AppendArray(TempArray(4 + (15 * i)).Substring(0 + (8 * k), 8).Trim, "FCAN", i, k, True)
                        End Try
                    Next
                End If
                If ParaMESH.IsEnabled("LNZ0", ParaMESH.PrimaryParameters) = True Then
                    For k = 0 To ParaMESH.LNZ0.GetUpperBound(1)
                        Try
                            ParaSAVE.LNZ0(TempArray(5 + (15 * i)).Substring(0 + (8 * k), 8).Trim, k, i, j)
                        Catch ex As Exception
                            ParaREAD.AppendArray(TempArray(5 + (15 * i)).Substring(0 + (8 * k), 8).Trim, "LNZ0", i, k, True)
                        End Try
                    Next
                End If
                If ParaMESH.IsEnabled("ALVC", ParaMESH.PrimaryParameters) = True Then
                    For k = 0 To ParaMESH.ALVC.GetUpperBound(1)
                        Try
                            ParaSAVE.ALVC(TempArray(6 + (15 * i)).Substring(0 + (8 * k), 8).Trim, k, i, j)
                        Catch ex As Exception
                            ParaREAD.AppendArray(TempArray(6 + (15 * i)).Substring(0 + (8 * k), 8).Trim, "ALVC", i, k, True)
                        End Try
                    Next
                End If
                If ParaMESH.IsEnabled("ALIC", ParaMESH.PrimaryParameters) = True Then
                    For k = 0 To ParaMESH.ALIC.GetUpperBound(1)
                        Try
                            ParaSAVE.ALIC(TempArray(7 + (15 * i)).Substring(0 + (8 * k), 8).Trim, k, i, j)
                        Catch ex As Exception
                            ParaREAD.AppendArray(TempArray(7 + (15 * i)).Substring(0 + (8 * k), 8).Trim, "ALIC", i, k, True)
                        End Try
                    Next
                End If
                If ParaMESH.IsEnabled("LAMX", ParaMESH.PrimaryParameters) = True Then
                    For k = 0 To ParaMESH.LAMX.GetUpperBound(1)
                        Try
                            ParaSAVE.LAMX(TempArray(4 + (15 * i)).Substring(40 + (8 * k), 8).Trim, k, i, j)
                        Catch ex As Exception
                            ParaREAD.AppendArray(TempArray(4 + (15 * i)).Substring(40 + (8 * k), 8).Trim.Length, "LAMX", i, k, True)
                        End Try
                    Next
                End If
                If ParaMESH.IsEnabled("LAMN", ParaMESH.PrimaryParameters) = True Then
                    For k = 0 To ParaMESH.LAMN.GetUpperBound(1)
                        Try
                            ParaSAVE.LAMN(TempArray(5 + (15 * i)).Substring(40 + (8 * k), 8).Trim, k, i, j)
                        Catch ex As Exception
                            ParaREAD.AppendArray(TempArray(5 + (15 * i)).Substring(40 + (8 * k), 8).Trim.Length, "LAMN", i, k, True)
                        End Try
                    Next
                End If
                If ParaMESH.IsEnabled("CMAS", ParaMESH.PrimaryParameters) = True Then
                    For k = 0 To ParaMESH.CMAS.GetUpperBound(1)
                        Try
                            ParaSAVE.CMAS(TempArray(6 + (15 * i)).Substring(40 + (8 * k), 8).Trim, k, i, j)
                        Catch ex As Exception
                            ParaREAD.AppendArray(TempArray(6 + (15 * i)).Substring(40 + (8 * k), 8).Trim, "CMAS", i, k, True)
                        End Try
                    Next
                End If
                If ParaMESH.IsEnabled("ROOT", ParaMESH.PrimaryParameters) = True Then
                    For k = 0 To ParaMESH.ROOT.GetUpperBound(1)
                        Try
                            ParaSAVE.ROOT(TempArray(7 + (15 * i)).Substring(40 + (8 * k), 8).Trim, k, i, j)
                        Catch ex As Exception
                            ParaREAD.AppendArray(TempArray(7 + (15 * i)).Substring(40 + (8 * k), 8).Trim, "ROOT", i, k, True)
                        End Try
                    Next
                End If
                If ParaMESH.IsEnabled("QA50", ParaMESH.PrimaryParameters) = True Then
                    For k = 0 To ParaMESH.QA50.GetUpperBound(1)
                        Try
                            ParaSAVE.QA50(TempArray(8 + (15 * i)).Substring(40 + (8 * k), 8).Trim, k, i, j)
                        Catch ex As Exception
                            ParaREAD.AppendArray(TempArray(8 + (15 * i)).Substring(40 + (8 * k), 8).Trim, "QA50", i, k, True)
                        End Try
                    Next
                End If
                If ParaMESH.IsEnabled("RSMN", ParaMESH.PrimaryParameters) = True Then
                    For k = 0 To ParaMESH.RSMN.GetUpperBound(1)
                        Try
                            ParaSAVE.RSMN(TempArray(8 + (15 * i)).Substring(0 + (8 * k), 8).Trim, k, i, j)
                        Catch ex As Exception
                            ParaREAD.AppendArray(TempArray(8 + (15 * i)).Substring(0 + (8 * k), 8).Trim, "RSMN", i, k, True)
                        End Try
                    Next
                End If
                If ParaMESH.IsEnabled("VPDA", ParaMESH.PrimaryParameters) = True Then
                    For k = 0 To ParaMESH.VPDA.GetUpperBound(1)
                        Try
                            ParaSAVE.VPDA(TempArray(9 + (15 * i)).Substring(0 + (8 * k), 8).Trim, k, i, j)
                        Catch ex As Exception
                            ParaREAD.AppendArray(TempArray(9 + (15 * i)).Substring(0 + (8 * k), 8).Trim, "VPDA", i, k, True)
                        End Try
                    Next
                End If
                If ParaMESH.IsEnabled("VPDB", ParaMESH.PrimaryParameters) = True Then
                    For k = 0 To ParaMESH.VPDB.GetUpperBound(1)
                        Try
                            ParaSAVE.VPDB(TempArray(9 + (15 * i)).Substring(40 + (8 * k), 8).Trim, k, i, j)
                        Catch ex As Exception
                            ParaREAD.AppendArray(TempArray(9 + (15 * i)).Substring(40 + (8 * k), 8).Trim, "VPDB", i, k, True)
                        End Try
                    Next
                End If
                If ParaMESH.IsEnabled("PSGA", ParaMESH.PrimaryParameters) = True Then
                    For k = 0 To ParaMESH.PSGA.GetUpperBound(1)
                        Try
                            ParaSAVE.PSGA(TempArray(10 + (15 * i)).Substring(0 + (8 * k), 8).Trim, k, i, j)
                        Catch ex As Exception
                            ParaREAD.AppendArray(TempArray(10 + (15 * i)).Substring(0 + (8 * k), 8).Trim, "PSGA", i, k, True)
                        End Try
                    Next
                End If
                If ParaMESH.IsEnabled("PSGB", ParaMESH.PrimaryParameters) = True Then
                    For k = 0 To ParaMESH.PSGB.GetUpperBound(1)
                        Try
                            ParaSAVE.PSGB(TempArray(10 + (15 * i)).Substring(40 + (8 * k), 8).Trim, k, i, j)
                        Catch ex As Exception
                            ParaREAD.AppendArray(TempArray(10 + (15 * i)).Substring(40 + (8 * k), 8).Trim, "PSGB", i, k, True)
                        End Try
                    Next
                End If
                If ParaMESH.IsEnabled("DRN", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.DRN(TempArray(11 + (15 * i)).Substring(0, 8).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(11 + (15 * i)).Substring(0, 8).Trim, "DRN", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("SDEP", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.SDEP(TempArray(11 + (15 * i)).Substring(8, 8).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(11 + (15 * i)).Substring(8, 8).Trim, "SDEP", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("FARE", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.FARE(TempArray(11 + (15 * i)).Substring(16, 8).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(11 + (15 * i)).Substring(16, 8).Trim, "FARE", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("DDEN", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.DDEN(TempArray(11 + (15 * i)).Substring(24, 8).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(11 + (15 * i)).Substring(24, 8).Trim, "DDEN", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("XSLP", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.XSLP(TempArray(12 + (15 * i)).Substring(0, 8).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(12 + (15 * i)).Substring(0, 8).Trim, "XSLP", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("GRKF", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.GRKF(TempArray(12 + (15 * i)).Substring(8, 8).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(12 + (15 * i)).Substring(8, 8).Trim, "GRKF", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("WFSF", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.WFSF(TempArray(12 + (15 * i)).Substring(16, 8).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(12 + (15 * i)).Substring(16, 8).Trim, "WFSF", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("WFCI", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.WFCI(TempArray(12 + (15 * i)).Substring(24, 8).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(12 + (15 * i)).Substring(24, 8).Trim, "WFCI", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("RSERVC01", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC01(TempArray(8 + (15 * i)).Substring(32, 8).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(8 + (15 * i)).Substring(32, 8).Trim, "RSERVC01", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("RSERVC02", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC02(TempArray(9 + (15 * i)).Substring(32, 8).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(9 + (15 * i)).Substring(32, 8).Trim, "RSERVC02", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("RSERVC03", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC03(TempArray(10 + (15 * i)).Substring(32, 8).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(10 + (15 * i)).Substring(32, 8).Trim, "RSERVC03", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("RSERVC04", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC04(TempArray(11 + (15 * i)).Substring(32, 8).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(11 + (15 * i)).Substring(32, 8).Trim, "RSERVC04", i)
                    End Try
                End If

                '***RSERVC05 = ZPLIM
                If ParaMESH.IsEnabled("RSERVC05", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC05(TempArray(11 + (15 * i)).Substring(40, 8).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(11 + (15 * i)).Substring(40, 8).Trim, "RSERVC05", i)
                    End Try
                End If

                '***RSERVC06 = ZPLIMS
                If ParaMESH.IsEnabled("RSERVC06", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC06(TempArray(11 + (15 * i)).Substring(48, 8).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(11 + (15 * i)).Substring(48, 8).Trim, "RSERVC06", i)
                    End Try
                End If

                '***RSERVC07 = D100F
                If ParaMESH.IsEnabled("RSERVC07", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC07(TempArray(11 + (15 * i)).Substring(56, 8).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(11 + (15 * i)).Substring(56, 8).Trim, "RSERVC07", i)
                    End Try
                End If

                '***RSERVC08 = D100A
                If ParaMESH.IsEnabled("RSERVC08", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC08(TempArray(11 + (15 * i)).Substring(64, 8).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(11 + (15 * i)).Substring(64, 8).Trim, "RSERVC08", i)
                    End Try
                End If

                '***RSERVC09 = FSURFS
                If ParaMESH.IsEnabled("RSERVC09", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC09(TempArray(12 + (15 * i)).Substring(40, 8).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(12 + (15 * i)).Substring(40, 8).Trim, "RSERVC09", i)
                    End Try
                End If

                '***RSERVC10 = GRKSATV
                If ParaMESH.IsEnabled("RSERVC10", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC10(TempArray(12 + (15 * i)).Substring(48, 8).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(12 + (15 * i)).Substring(48, 8).Trim, "RSERVC10", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("RSERVC11", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC11(TempArray(15 + (15 * i)).Substring(50, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(15 + (15 * i)).Substring(50, 10).Trim, "RSERVC11", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("RSERVC12", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC12(TempArray(15 + (15 * i)).Substring(60, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(15 + (15 * i)).Substring(60, 10), "RSERVC12", i)
                    End Try
                End If

                '***READING SECOND BLOCK
                If ParaMESH.IsEnabled("SAND", ParaMESH.PrimaryParameters) = True Then
                    For k = 0 To ParaMESH.SAND.GetUpperBound(1)
                        Try
                            ParaSAVE.SAND(TempArray(13 + (15 * i)).Substring(0 + (10 * k), 10).Trim, k, i, j)
                        Catch ex As Exception
                            ParaREAD.AppendArray(TempArray(13 + (15 * i)).Substring(0 + (10 * k), 10).Trim, "SAND", i, k, True)
                        End Try
                    Next
                End If
                If ParaMESH.IsEnabled("CLAY", ParaMESH.PrimaryParameters) = True Then
                    For k = 0 To ParaMESH.CLAY.GetUpperBound(1)
                        Try
                            ParaSAVE.CLAY(TempArray(14 + (15 * i)).Substring(0 + (10 * k), 10).Trim, k, i, j)
                        Catch ex As Exception
                            ParaREAD.AppendArray(TempArray(14 + (15 * i)).Substring(0 + (10 * k), 10).Trim, "CLAY", i, k, True)
                        End Try
                    Next
                End If
                If ParaMESH.IsEnabled("ORGM", ParaMESH.PrimaryParameters) = True Then
                    For k = 0 To ParaMESH.ORGM.GetUpperBound(1)
                        Try
                            ParaSAVE.ORGM(TempArray(15 + (15 * i)).Substring(0 + (10 * k), 10).Trim, k, i, j)
                        Catch ex As Exception
                            ParaREAD.AppendArray(TempArray(15 + (15 * i)).Substring(0 + (10 * k), 10).Trim, "ORGM", i, k, True)
                        End Try
                    Next
                End If
                If ParaMESH.IsEnabled("TBAR", ParaMESH.PrimaryParameters) = True Then
                    For k = 0 To ParaMESH.TBAR.GetUpperBound(1)
                        Try
                            ParaSAVE.TBAR(TempArray(16 + (15 * i)).Substring(0 + (10 * k), 10).Trim, k, i, j)
                        Catch ex As Exception
                            ParaREAD.AppendArray(TempArray(16 + (15 * i)).Substring(0 + (10 * k), 10).Trim, "TBAR", i, k, True)
                        End Try
                    Next
                End If
                If ParaMESH.IsEnabled("TCAN", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.TCAN(TempArray(16 + (15 * i)).Substring(30, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(16 + (15 * i)).Substring(30, 10).Trim, "TCAN", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("TSNO", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.TSNO(TempArray(16 + (15 * i)).Substring(40, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(16 + (15 * i)).Substring(40, 10).Trim, "TSNO", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("TPND", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.TPND(TempArray(16 + (15 * i)).Substring(50, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(16 + (15 * i)).Substring(50, 10).Trim, "TPND", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("THLQ", ParaMESH.PrimaryParameters) = True Then
                    For k = 0 To ParaMESH.THLQ.GetUpperBound(1)
                        Try
                            ParaSAVE.THLQ(TempArray(17 + (15 * i)).Substring(0 + (10 * k), 10).Trim, k, i, j)
                        Catch ex As Exception
                            ParaREAD.AppendArray(TempArray(17 + (15 * i)).Substring(0 + (10 * k), 10).Trim, "THLQ", i, k, True)
                        End Try
                    Next
                End If
                If ParaMESH.IsEnabled("THIC", ParaMESH.PrimaryParameters) = True Then
                    For k = 0 To ParaMESH.THIC.GetUpperBound(1)
                        Try
                            ParaSAVE.THIC(TempArray(17 + (15 * i)).Substring(30 + (10 * k), 10).Trim, k, i, j)
                        Catch ex As Exception
                            ParaREAD.AppendArray(TempArray(17 + (15 * i)).Substring(30 + (10 * k), 10).Trim, "THIC", i, k, True)
                        End Try
                    Next
                End If
                If ParaMESH.IsEnabled("ZPND", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.ZPND(TempArray(17 + (15 * i)).Substring(60, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(17 + (15 * i)).Substring(60, 10).Trim, "ZPND", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("RCAN", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RCAN(TempArray(18 + (15 * i)).Substring(0, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(18 + (15 * i)).Substring(0, 10).Trim, "RCAN", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("SCAN", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.SCAN(TempArray(18 + (15 * i)).Substring(10, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(18 + (15 * i)).Substring(10, 10).Trim, "SCAN", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("SNO", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.SNO(TempArray(18 + (15 * i)).Substring(20, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(18 + (15 * i)).Substring(20, 10).Trim, "SNO", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("ALBS", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.ALBS(TempArray(18 + (15 * i)).Substring(30, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(18 + (15 * i)).Substring(30, 10).Trim, "ALBS", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("RHOS", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RHOS(TempArray(18 + (15 * i)).Substring(40, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(18 + (15 * i)).Substring(40, 10).Trim, "RHOS", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("GRO", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.GRO(TempArray(18 + (15 * i)).Substring(50, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(18 + (15 * i)).Substring(50, 10).Trim, "GRO", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("RSERVC13", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC13(TempArray(16 + (15 * i)).Substring(60, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(16 + (15 * i)).Substring(60, 10).Trim, "RSERVC13", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("RSERVC14", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC14(TempArray(18 + (15 * i)).Substring(60, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(18 + (15 * i)).Substring(60, 10).Trim, "RSERVC14", i)
                    End Try
                End If

                '***RSERVC15 = MDEST1
                If ParaMESH.IsEnabled("RSERVC15", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC15(TempArray(13 + k + (15 * i)).Substring(50, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(13 + k + (15 * i)).Substring(50, 10).Trim, "RSERVC15", i)
                    End Try
                End If

                '***RSERVC16 = FMDIV1
                If ParaMESH.IsEnabled("RSERVC16", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC16(TempArray(13 + k + (15 * i)).Substring(60, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(13 + k + (15 * i)).Substring(60, 10).Trim, "RSERVC16", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("RSERVC17", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC17(TempArray(18 + (15 * i)).Substring(60, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(18 + (15 * i)).Substring(60, 10).Trim, "RSERVC17", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("RSERVC18", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC18(TempArray(18 + (15 * i)).Substring(60, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(18 + (15 * i)).Substring(60, 10).Trim, "RSERVC18", i)
                    End Try
                End If

                '***RSERVC19 = MDEST1
                If ParaMESH.IsEnabled("RSERVC19", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC19(TempArray(13 + k + (15 * i)).Substring(50, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(13 + k + (15 * i)).Substring(50, 10).Trim, "RSERVC19", i)
                    End Try
                End If

                '***RSERVC20 = FMDIV2
                If ParaMESH.IsEnabled("RSERVC20", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC20(TempArray(13 + k + (15 * i)).Substring(60, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(13 + k + (15 * i)).Substring(60, 10).Trim, "RSERVC20", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("RSERVC21", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC21(TempArray(18 + (15 * i)).Substring(60, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(18 + (15 * i)).Substring(60, 10).Trim, "RSERVC21", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("RSERVC22", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC22(TempArray(18 + (15 * i)).Substring(60, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(18 + (15 * i)).Substring(60, 10).Trim, "RSERVC22", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("RSERVC23", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC23(TempArray(18 + (15 * i)).Substring(60, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(18 + (15 * i)).Substring(60, 10).Trim, "RSERVC23", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("RSERVC24", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC24(TempArray(18 + (15 * i)).Substring(60, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(18 + (15 * i)).Substring(60, 10).Trim, "RSERVC24", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("RSERVC25", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC25(TempArray(18 + (15 * i)).Substring(60, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(18 + (15 * i)).Substring(60, 10).Trim, "RSERVC25", i)
                    End Try
                End If
                If ParaMESH.IsEnabled("RSERVC26", ParaMESH.PrimaryParameters) = True Then
                    Try
                        ParaSAVE.RSERVC26(TempArray(18 + (15 * i)).Substring(60, 10).Trim, i, j)
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(18 + (15 * i)).Substring(60, 10).Trim, "RSERVC26", i)
                    End Try
                End If
            Next
        Next

        '***READ AND SAVE MODEL RUN TIMES
        For j = 0 To 1
            If ParaMESH.IsEnabled("HourlyStartDay", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.HourlyStartDay(TempArray(19 + (15 * (i - 1))).Substring(0, 10).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(19 + (15 * (i - 1))).Substring(0, 10).Trim, "HourlyStartDay")
                End Try
            End If
            If ParaMESH.IsEnabled("HourlyStopDay", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.HourlyStopDay(TempArray(19 + (15 * (i - 1))).Substring(10, 10).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(19 + (15 * (i - 1))).Substring(10, 10).Trim, "HourlyStopDay")
                End Try
            End If
            If ParaMESH.IsEnabled("DailyStartDay", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.DailyStartDay(TempArray(19 + (15 * (i - 1))).Substring(20, 10).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(19 + (15 * (i - 1))).Substring(20, 10).Trim, "DailyStartDay")
                End Try
            End If
            If ParaMESH.IsEnabled("DailyStopDay", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.DailyStopDay(TempArray(19 + (15 * (i - 1))).Substring(30, 10).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(19 + (15 * (i - 1))).Substring(30, 10).Trim, "DailyStopDay")
                End Try
            End If
            If ParaMESH.IsEnabled("SimStartDay", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.SimStartDay(TempArray(19 + (15 * (i - 1))).Substring(40, 10).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(19 + (15 * (i - 1))).Substring(40, 10).Trim, "SimStartDay")
                End Try
            End If
            If ParaMESH.IsEnabled("SimStopDay", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.SimStopDay(TempArray(19 + (15 * (i - 1))).Substring(50, 10).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(19 + (15 * (i - 1))).Substring(50, 10).Trim, "SimStopDay")
                End Try
            End If
            If ParaMESH.IsEnabled("HourlyStartYear", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.HourlyStartYear(TempArray(20 + (15 * (i - 1))).Substring(0, 10).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(20 + (15 * (i - 1))).Substring(0, 10).Trim, "HourlyStartYear")
                End Try
            End If
            If ParaMESH.IsEnabled("HourlyStopYear", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.HourlyStopYear(TempArray(20 + (15 * (i - 1))).Substring(10, 10).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(20 + (15 * (i - 1))).Substring(10, 10).Trim, "HourlyStopYear")
                End Try
            End If
            If ParaMESH.IsEnabled("DailyStartYear", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.DailyStartYear(TempArray(20 + (15 * (i - 1))).Substring(20, 10).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(20 + (15 * (i - 1))).Substring(20, 10).Trim, "DailyStartYear")
                End Try
            End If
            If ParaMESH.IsEnabled("DailyStopYear", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.DailyStopYear(TempArray(20 + (15 * (i - 1))).Substring(30, 10).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(20 + (15 * (i - 1))).Substring(30, 10).Trim, "DailyStopYear")
                End Try
            End If
            If ParaMESH.IsEnabled("SimStartYear", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.SimStartYear(TempArray(20 + (15 * (i - 1))).Substring(40, 10).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(20 + (15 * (i - 1))).Substring(40, 10).Trim, "SimStartYear")
                End Try
            End If
            If ParaMESH.IsEnabled("SimStopYear", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.SimStopYear(TempArray(20 + (15 * (i - 1))).Substring(50, 10).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(20 + (15 * (i - 1))).Substring(50, 10).Trim, "SimStopYear")
                End Try
            End If
            If ParaMESH.IsEnabled("MetStartHour", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.MetStartHour(TempArray(21 + (15 * (i - 1))).Substring(0, 10).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(21 + (15 * (i - 1))).Substring(0, 10).Trim, "MetStartHour")
                End Try
            End If
            If ParaMESH.IsEnabled("MetStartMin", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.MetStartMin(TempArray(21 + (15 * (i - 1))).Substring(10, 10).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(21 + (15 * (i - 1))).Substring(10, 10).Trim, "MetStartMin")
                End Try
            End If
            If ParaMESH.IsEnabled("MetStartDay", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.MetStartDay(TempArray(21 + (15 * (i - 1))).Substring(20, 10).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(21 + (15 * (i - 1))).Substring(20, 10).Trim, "MetStartDay")
                End Try
            End If
            If ParaMESH.IsEnabled("MetStartYear", ParaMESH.PrimaryParameters) = True Then
                Try
                    ParaSAVE.MetStartYear(TempArray(21 + (15 * (i - 1))).Substring(30, 10).Trim, j)
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(21 + (15 * (i - 1))).Substring(30, 10).Trim, "MetStartYear")
                End Try
            End If
        Next

        '***CALL ParaCHECK(Array) TO CHECK CatchArray FOR READ ERRORS
        ParaCHECK.Array(CatchArray)
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaREAD.WatflowINI
    '*****************************************************************************
    'UPDATED: APR 22 2008  to use common ParaMESH subroutines.
    '*****************************************************************************
    'Read the watflow.ini configuration file for WATFLOW.  The FileName and 
    'FileHeader are passed by the calling subroutine.  If AcceptVariants is True, 
    'then read any file with the defined FileName; otherwise, ensure the file 
    'contains the FileHeader before the remainder of the file is read.
    '*****************************************************************************

    Public Shared Sub WatflowINI(ByVal FileName As String, Optional ByVal FileHeader As String = Nothing, Optional ByVal AcceptVariants As Boolean = True)

        '***CALL ParaREAD(FileExists) TO CHECK IF FileName EXISTS
        If ParaREAD.FileExists(FileName) = False Then
            Exit Sub
        End If

        '***CALL ParaREAD(ReadFile) TO READ THE CONFIGURATION FILE
        TempArray = ParaREAD.ReadFile(FileName)

        '***CALL ParaREAD(ValidFile) TO CHECK THAT CONTENTS OF FileName EXIST
        If ParaREAD.ValidFile(FileName, TempArray) = False Then
            Exit Sub
        End If

        '***CALL ParaREAD(ValidHeader) TO CHECK IF FileName MUST HAVE A VALID HEADER TO BE READ
        If ParaREAD.ValidHeader(FileName, FileHeader, AcceptVariants) = False Then
            Exit Sub
        End If

        '***REDIMENSION ParaREAD(CatchArray)
        ReDim ParaREAD.CatchArray(ParaREAD.CatchArray.GetUpperBound(0), 0)

        '***READ AND BASIN ID
        If ParaMESH.IsEnabled("BasinID", ParaMESH.PrimaryParameters) = True Then
            ParaLOG.AppendFile("BasinID# ..reading Line 1: " & FileName, "ParaREAD.WatflowINI", "PopVar")
            Try
                For j = 0 To ParaMESH.BasinID.GetUpperBound(0)
                    ParaSAVE.BasinID(TempArray(0).Substring(23, 10).Trim, j)
                Next
            Catch ex As Exception
                ParaREAD.AppendArray(TempArray(0).Substring(23, 10).Trim, "BasinID")
            End Try
        End If

        '***READ AND SAVE CHANNEL ROUGHNESS FACTORS (WF_R2), SCALING FACTOR
        If ParaMESH.IsEnabled("WF_R2", ParaMESH.PrimaryParameters) = True Then
            For i = 0 To ParaMESH.WF_R2.GetUpperBound(1)
                ParaLOG.AppendFile("WF_R2# ..reading Line 3, Column " & (i + 1).ToString & ": " & FileName, "ParaREAD.WatflowINI", "PopVar")
                Try
                    For j = 0 To ParaMESH.WF_R2.GetUpperBound(0)
                        ParaSAVE.WF_R2(TempArray(2).Substring(0 + (6 * i), 6).Trim, i, j)
                    Next
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(2).Substring(0 + (6 * i), 6).Trim, "WF_R2", i)
                End Try
            Next
        End If
        If ParaMESH.IsEnabled("ScalingFactor", ParaMESH.PrimaryParameters) = True Then
            ParaLOG.AppendFile("ScalingFactor# ..reading Line 3, Column 6: " & FileName, "ParaREAD.WatflowINI", "PopVar")
            Try
                For j = 0 To ParaMESH.ScalingFactor.GetUpperBound(0)
                    ParaSAVE.ScalingFactor(TempArray(2).Substring(30, 6).Trim, j)
                Next
            Catch ex As Exception
                ParaREAD.AppendArray(TempArray(2).Substring(30, 6).Trim, "ScalingFactor")
            End Try
        End If

        '***READING WATFLOW FLAGS
        If ParaMESH.IsEnabled("WatflowFlag", ParaMESH.PrimaryParameters) = True Then
            For i = 0 To ParaMESH.WatflowFlag.GetUpperBound(1)
                ParaLOG.AppendFile(ParaMESH.SecondaryName(i, "WatflowFlag") & "# ..reading Line " & (i + 1).ToString & ", Column 1: " & FileName, "ParaREAD.WatflowINI", "PopVar")
                Try
                    For j = 0 To ParaMESH.WatflowFlag.GetUpperBound(0)
                        ParaSAVE.WatflowFlag(ParaMESH.SecondaryName(i, "WatflowFlag"), TempArray(i + 3).Substring(0, 5).Trim, j)
                    Next
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(i + 3).Substring(0, 5).Trim, ParaMESH.SecondaryName(i, "WatflowFlag"))
                End Try
            Next
        End If

        '***READING PONDING LIMITS (ZPLIMG0, ZPLIMS0, ZPLIMP0, ZPLIMPS0)
        If ParaMESH.IsEnabled("ZPLIMG0", ParaMESH.PrimaryParameters) = True Then
            ParaLOG.AppendFile("ZPLIMG0# ..reading Line 18, Column 1: " & FileName, "ParaREAD.WatflowINI", "PopVar")
            Try
                For j = 0 To ParaMESH.ZPLIMG0.GetUpperBound(0)
                    ParaSAVE.ZPLIMG0(TempArray(17).Substring(0, 6).Trim, j)
                Next
            Catch ex As Exception
                ParaREAD.AppendArray(TempArray(17).Substring(0, 6).Trim, "ZPLIMG0")
            End Try
        End If
        If ParaMESH.IsEnabled("ZPLIMS0", ParaMESH.PrimaryParameters) = True Then
            ParaLOG.AppendFile("ZPLIMS0# ..reading Line 18, Column 2: " & FileName, "ParaREAD.WatflowINI", "PopVar")
            Try
                For j = 0 To ParaMESH.ZPLIMG0.GetUpperBound(0)
                    ParaSAVE.ZPLIMS0(TempArray(17).Substring(6, 6).Trim, j)
                Next
            Catch ex As Exception
                ParaREAD.AppendArray(TempArray(17).Substring(6, 6).Trim, "ZPLIMS0")
            End Try
        End If
        If ParaMESH.IsEnabled("ZPLIMP0", ParaMESH.PrimaryParameters) = True Then
            ParaLOG.AppendFile("ZPLIMP0# ..reading Line 18, Column 3: " & FileName, "ParaREAD.WatflowINI", "PopVar")
            Try
                For j = 0 To ParaMESH.ZPLIMG0.GetUpperBound(0)
                    ParaSAVE.ZPLIMP0(TempArray(17).Substring(12, 6).Trim, j)
                Next
            Catch ex As Exception
                ParaREAD.AppendArray(TempArray(17).Substring(12, 6).Trim, "ZPLIMP0")
            End Try
        End If
        If ParaMESH.IsEnabled("ZPLIMPS0", ParaMESH.PrimaryParameters) = True Then
            ParaLOG.AppendFile("ZPLIMPS0# ..reading Line 18, Column 4: " & FileName, "ParaREAD.WatflowINI", "PopVar")
            Try
                For j = 0 To ParaMESH.ZPLIMG0.GetUpperBound(0)
                    ParaSAVE.ZPLIMPS0(TempArray(17).Substring(18, 6).Trim, j)
                Next
            Catch ex As Exception
                ParaREAD.AppendArray(TempArray(17).Substring(18, 6).Trim, "ZPLIMPS0")
            End Try
        End If

        '***READING SNOW LIMITS (D100A)
        If ParaMESH.IsEnabled("D100A", ParaMESH.PrimaryParameters) = True Then
            For i = 0 To ParaMESH.D100A.GetUpperBound(1)
                ParaLOG.AppendFile("D100A# ..reading Line 20, Column " & (i + 1).ToString & ": " & FileName, "ParaREAD.WatflowINI", "PopVar")
                Try
                    For j = 0 To ParaMESH.D100A.GetUpperBound(0)
                        ParaSAVE.D100A(TempArray(19).Substring(0 + (6 * i), 6).Trim, i, j)
                    Next
                Catch ex As Exception
                    ParaREAD.AppendArray(TempArray(19).Substring(0 + (6 * i), 6).Trim, "D100A", i)
                End Try
            Next
        End If

        '***READING INITIAL GROUNDWATER STORAGE (GWINIT)
        If ParaMESH.IsEnabled("GWINIT", ParaMESH.PrimaryParameters) = True Then
            ParaLOG.AppendFile("GWINIT# ..reading Line 20, Column 7: " & FileName, "ParaREAD.WatflowINI", "PopVar")
            Try
                For j = 0 To ParaMESH.GWINIT.GetUpperBound(0)
                    ParaSAVE.GWINIT(TempArray(19).Substring(36, 6).Trim, j)
                Next
            Catch ex As Exception
                ParaREAD.AppendArray(TempArray(19).Substring(36, 6).Trim, "GWINIT")
            End Try
        End If

        '***READ AND SAVE GRID OUTPUT POINTS
        If ParaMESH.IsEnabled("GridOutput", ParaMESH.PrimaryParameters) = True Then
            Try
                If Convert.ToInt32(TempArray(i + 2).Substring(0, 5).Trim) = 0 Then

                    '***IF NO GRID OUTPUT POINTS EXIST, ENABLE ..SystemParameters(NoOutput) FLAG
                    ReDim ParaMESH.GridOutput(ParaMESH.GridOutput.GetUpperBound(0), ParaMESH.GridOutput.GetUpperBound(1), 0)
                    ParaSAVE.SystemParameters("Enabled", "NoOutput")
                    Exit Try
                End If

                '***IF GRID OUTPUT POINTS EXIST
                ParaLOG.AppendFile("GridOutput#: " & FileName, "ParaREAD.WatflowINI", "DimArr")
                ReDim ParaMESH.GridOutput(ParaMESH.GridOutput.GetUpperBound(0), ParaMESH.GridOutput.GetUpperBound(1), Convert.ToInt32(TempArray(20).Substring(0, 5).Trim) - 1)
                For k = 0 To ParaMESH.GridOutput.GetUpperBound(2)
                    ParaLOG.AppendFile(ParaMESH.SecondaryName(2, "GridOutput") & "# ..reading Line " & (25 + k + 1).ToString & ", Column 1: " & FileName, "ParaREAD.WatflowINI", "PopVar")
                    Try
                        For j = 0 To ParaMESH.GridOutput.GetUpperBound(0)
                            ParaSAVE.GridOutput(ParaMESH.SecondaryName(2, "GridOutput"), TempArray(25 + k).Substring(0, 10).Trim, k, j)
                        Next
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(25 + k).Substring(0, 10).Trim, ParaMESH.SecondaryName(i, "GridOutput"), k, 2)
                    End Try
                    ParaLOG.AppendFile(ParaMESH.SecondaryName(0, "GridOutput") & "# ..reading Line " & (25 + k + 1).ToString & ", Column 2: " & FileName, "ParaREAD.WatflowINI", "PopVar")
                    Try
                        For j = 0 To ParaMESH.GridOutput.GetUpperBound(0)
                            ParaSAVE.GridOutput(ParaMESH.SecondaryName(0, "GridOutput"), TempArray(25 + k).Substring(11, 7).Trim, k, j)
                        Next
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(25 + k).Substring(11, 7).Trim, ParaMESH.SecondaryName(0, "GridOutput"), k, 0)
                    End Try
                    ParaLOG.AppendFile(ParaMESH.SecondaryName(1, "GridOutput") & "# ..reading Line " & (25 + k + 1).ToString & ", Column 3: " & FileName, "ParaREAD.WatflowINI", "PopVar")
                    Try
                        For j = 0 To ParaMESH.GridOutput.GetUpperBound(0)
                            ParaSAVE.GridOutput(ParaMESH.SecondaryName(1, "GridOutput"), TempArray(25 + k).Substring(18, 7).Trim, k, j)
                        Next
                    Catch ex As Exception
                        ParaREAD.AppendArray(TempArray(25 + k).Substring(18, 7).Trim, ParaMESH.SecondaryName(1, "GridOutput"), k, 1)
                    End Try
                Next
            Catch ex As Exception

                '***OPEN FileName ON READ ERROR
                MsgBox("The number of Grid Output Points in the configuration file is invalid.  Check that a valid integer value has been entered in Column 1 of Line 21 of: " & FileName & ", and then re-load the file.", MsgBoxStyle.OkOnly, "ParaMESH")
                System.Diagnostics.Process.Start("notepad.exe", ParaMESH.PathRoot & "\" & FileName)
            End Try
        End If

        '***CALL ParaCHECK(Array) TO CHECK ParaREAD(CatchArray) FOR READ ERRORS
        ParaCHECK.Array(ParaREAD.CatchArray)
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaSTOR.BAK
    '*****************************************************************************
    'UPDATE: APR 22 2008 (DAN)  to use common ParaMESH subroutines.
    'UPDATE: MAR 22 2008 (DAN)  moved from ParaSTOR to ParaWRITE; renamed "BAK" 
    '                           from "ParaSTOR.Read"
    'UPDATE: FEB 08 2008 (DAN)  moved back to ParaREAD, renamed BakFile
    'UPDATE: JAN 17 2008 (DAN)  integrated into ParaSTOR (from ParaREAD)
    'UPDATE: JAN 09 2008 (DAN)  for ParaLOG approach
    '*****************************************************************************
    'Process for reading ParaMESH.bak restoration file.
    '*****************************************************************************

    Public Shared Sub BAK()

        '***CALL ParaREAD(ReadFile) TO CHECK IF THE ParaMESH RESTORE FILE EXISTS
        If ParaREAD.FileExists("ParaMESH.bak") = False Then
            Exit Sub
        End If

        '***CALL ParaREAD(ReadFile) TO READ THE FILE
        TempArray = ParaREAD.ReadFile("ParaMESH.bak")

        '***CHECK IF ParaMESH RESTORE FILE IS CORRUPT
        If TempArray.Length = 0 Then
            MsgBox("The ParaMESH restore file is corrupt.  ParaMESH can not restore the configuration files.", MsgBoxStyle.OkOnly, "ParaMESH")

            '***DELETE THE ParaMESH RESTORE FILES

            '***DELETE THE ParaMESH RESTORE FILES FROM THE APPLICATION DIRECTORY
            System.IO.File.Delete(Application.StartupPath & "\ParaMESH.bak")
            If System.IO.File.Exists(Application.StartupPath & "\class.txt") = True Then
                System.IO.File.Delete(Application.StartupPath & "\class.txt")
            End If
            If System.IO.File.Exists(Application.StartupPath & "\hydrology.txt") = True Then
                System.IO.File.Delete(Application.StartupPath & "\hydrology.txt")
            End If
            If System.IO.File.Exists(Application.StartupPath & "\flags.txt") = True Then
                System.IO.File.Delete(Application.StartupPath & "\flags.txt")
            End If
        End If

        '***CHECK THE ENVIRONMENT THAT WAS USED TO SAVE THE ParaMESH RESTORE FILE
        If TempArray(2).Substring(TempArray(2).IndexOf(";") + 1) <> ParaLOAD.SystemProperties("SystemRelease") Then
            LoadBak = MsgBox("The ParaMESH restore file was saved using a different version of ParaMESH (" & TempArray(2).Substring(TempArray(2).IndexOf(";") + 1) & ").  Do you wish ParaMESH to continue loading the file?", MsgBoxStyle.YesNo, "ParaMESH")
            If LoadBak = MsgBoxResult.No Then
                MsgBox("You will be prompted to load the ParaMESH restore file the next time ParaMESH is loaded.", MsgBoxStyle.OkOnly, "ParaMESH")
                Exit Sub
            End If
        End If

        '***LOAD DEFAULT VALUES IF SOME OF THE ParaMESH RESTORE FILES DO NOT EXIST

        '***CALL ParaREAD SUBROUTINES TO LOAD THE ParaMESH RESTORE FILES FROM THE APPLICATION DIRECTORY
        ParaSAVE.PathRoot(Application.StartupPath)
        If System.IO.File.Exists(Application.StartupPath & "\class.txt") = True Then
            ParaREAD.MESH_ClassINI("class.txt")
        End If
        If System.IO.File.Exists(Application.StartupPath & "\hydrology.txt") = True Then
            ParaREAD.MESH_HydrologyINI("hydrology.txt")
        End If
        If System.IO.File.Exists(Application.StartupPath & "\flags.txt") = True Then
            ParaREAD.MESH_RunOptions("flags.txt")
        End If

        '***CHECK IF PATHROOT WRITTEN TO THE ParaMESH RESTORE FILE STILL EXISTS
        If System.IO.Directory.Exists(TempArray(1).Substring(TempArray(1).IndexOf(";") + 1)) = False Then
            LoadBak = MsgBox("The directory containing the configuration files, written to the ParaMESH restore file, no longer exists.  Do you wish to select a new directory to save the configuration files to?", MsgBoxStyle.YesNo, "ParaMESH")
            If LoadBak = MsgBoxResult.No Then
                MsgBox("You will be prompted to load the ParaMESH restore file the next time ParaMESH is loaded.", MsgBoxStyle.OkOnly, "ParaMESH")
                Exit Sub
            ElseIf LoadBak = MsgBoxResult.Yes Then

                '***CALL ProcessForm(Ext_BrowseDirectory) TO SET ParaMESH(PathRoot)
                ProcessForm.Ext_BrowseDirectory.ShowDialog()
                ParaSAVE.PathRoot(ProcessForm.Ext_BrowseDirectory.SelectedPath)
            End If
        Else

            '***SAVE ParaMESH(PathRoot)
            ParaSAVE.PathRoot(TempArray(1).Substring(TempArray(1).IndexOf(";") + 1))
        End If

        '***DELETE THE ParaMESH RESTORE FILES FROM THE APPLICATION DIRECTORY
        System.IO.File.Delete(Application.StartupPath & "\ParaMESH.bak")
        If System.IO.File.Exists(Application.StartupPath & "\class.txt") = True Then
            System.IO.File.Delete(Application.StartupPath & "\class.txt")
        End If
        If System.IO.File.Exists(Application.StartupPath & "\hydrology.txt") = True Then
            System.IO.File.Delete(Application.StartupPath & "\hydrology.txt")
        End If
        If System.IO.File.Exists(Application.StartupPath & "\flags.txt") = True Then
            System.IO.File.Delete(Application.StartupPath & "\flags.txt")
        End If

        '***CALL ProcessForm(Populate) TO POPULATE FORMS
        ProcessForm.Populate()
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaREAD.Configuration
    '*****************************************************************************
    'Read the ParaMESH Configuration File.
    '*****************************************************************************

    Public Shared Sub Configuration()

        '***CALL ParaREAD(FileExists) TO CHECK IF THE ParaMESH CONFIGURATION FILE EXISTS
        If ParaREAD.FileExists("ParaMESH.ini") = False Then
            MsgBox("The ParaMESH configuration file does not exist in the application's directory.  ParaMESH can not start.  Please re-install the application or contact your ParaMESH Administrator for assistance.", MsgBoxStyle.OkOnly, "ParaMESH")
            Application.Exit()
        End If

        '***CALL ParaREAD(ReadFile) TO READ THE ParaMESH CONFIGURATION FILE
        TempArray = ParaREAD.ReadFile("ParaMESH.ini")

        '***CHECK IF THE CONTENT OF THE ParaMESH CONFIGURATION FILE EXISTS
        If TempArray.Length = 0 Then
            MsgBox("The ParaMESH configuration file is corrupt.  ParaMESH can not start.  Please re-install the application or contact your ParaMESH Administrator for assistance.", MsgBoxStyle.OkOnly, "ParaMESH")
            Application.Exit()
        End If

        ParaMESH.SystemProperties = ParaREAD.PopulateSystemParameter("SystemProperties", ParaMESH.SystemProperties, TempArray)
        ParaMESH.FileProperties = ParaREAD.PopulateSystemParameter("FileProperties", ParaMESH.FileProperties, TempArray)
        ParaMESH.PrimaryParameters = ParaREAD.PopulateSystemParameter("PrimaryParameters", ParaMESH.PrimaryParameters, TempArray)
        ParaMESH.SecondaryParameters = ParaREAD.PopulateSystemParameter("SecondaryParameters", ParaMESH.SecondaryParameters, TempArray)
        ParaMESH.SystemParameters = ParaREAD.PopulateSystemParameter("SystemParameters", ParaMESH.SystemParameters, TempArray)
        ParaMESH.SpecialChecks = ParaREAD.PopulateSystemParameter("SpecialChecks", ParaMESH.SpecialChecks, TempArray)
        ParaMESH.CrossFields = ParaREAD.PopulateSystemParameter("CrossFields", ParaMESH.CrossFields, TempArray)
        ParaMESH.ErrorDefinitions = ParaREAD.PopulateSystemParameter("ErrorDefinitions", ParaMESH.ErrorDefinitions, TempArray)

        If ParaMESH.IsEnabled("ParaLOG", ParaMESH.SystemParameters, 1) = True Then
            If LogForm.Visible = False Then
                LogForm.Show()
            End If
        Else
            ReDim Preserve ParaMESH.LogFile(0)
            If LogForm.Visible = True Then
                LogForm.Close()
            End If
        End If

        '***CALL ParaLOAD(DirectoryList) TO POPULATE ParaMESH(DirectoryList)
        ParaMESH.DirectoryList = ParaLOAD.DirectoryList
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaREAD.AppendArray
    '*****************************************************************************
    'Add VariableName and its associated identifiers to ParaREAD(CatchArray) to 
    'be checked by ParaCHECK after a file is read.
    '*****************************************************************************

    Private Shared Sub AppendArray(ByVal Variable As String, ByVal VariableName As String, Optional ByVal Index As Integer = -1, Optional ByVal ColumnIndex As Integer = -1, Optional ByVal GRU As Boolean = False)
        ParaREAD.CatchArray(0, ParaREAD.CatchArray.GetUpperBound(1)) = Variable
        ParaREAD.CatchArray(1, ParaREAD.CatchArray.GetUpperBound(1)) = VariableName
        ParaREAD.CatchArray(2, ParaREAD.CatchArray.GetUpperBound(1)) = Index
        ParaREAD.CatchArray(3, ParaREAD.CatchArray.GetUpperBound(1)) = ColumnIndex
        ParaREAD.CatchArray(4, ParaREAD.CatchArray.GetUpperBound(1)) = GRU
        ReDim Preserve ParaREAD.CatchArray(ParaREAD.CatchArray.GetUpperBound(0), ParaREAD.CatchArray.GetUpperBound(1) + 1)
    End Sub

    '*****************************************************************************
    'FUNCTION: ParaREAD.PopulateSystemParameter
    '*****************************************************************************
    'Used to populate fixed ParaMESH parameters, containing information used to 
    'configure the program.
    '*****************************************************************************

    Private Shared Function PopulateSystemParameter(ByVal ArrayName As String, ByVal SystemArray(,) As String, ByVal FileArray() As String) As String(,)

        '***LOOK FOR SECTION FileArray IN "#Descriptor[" FORMAT
        For i = 0 To FileArray.GetUpperBound(0)
            If FileArray(i).Substring(0, 1) = "#" Then
                If FileArray(i).Substring(1, FileArray(i).IndexOf("[") - 1) = ArrayName Then
                    Exit For
                End If
            End If
        Next

        '***CHECK THAT ENTRIES FOR ArrayName EXIST
        If Convert.ToInt32(FileArray(i).Substring(FileArray(i).IndexOf("[", FileArray(i).IndexOf(",")) + 1, FileArray(i).IndexOf("]", FileArray(i).IndexOf(",")) - FileArray(i).IndexOf("[", FileArray(i).IndexOf(",")) - 1)) < 1 Then
            MsgBox("ParaMESH may encounter critical errors because entries for system parameter do not exist: " & ArrayName, MsgBoxStyle.OkOnly, "ParaMESH")
            PopulateSystemParameter = SystemArray
            Exit Function
        End If

        '***POPULATE SystemArray
        ReDim SystemArray(SystemArray.GetUpperBound(0), Convert.ToInt32(FileArray(i).Substring(FileArray(i).IndexOf("[", FileArray(i).IndexOf(",")) + 1, FileArray(i).IndexOf("]", FileArray(i).IndexOf(",")) - FileArray(i).IndexOf("[", FileArray(i).IndexOf(",")) - 1)) - 1)
        If FileArray(i).IndexOf("[", FileArray(i).IndexOf("[", FileArray(i).IndexOf(",")) + 1) > -1 Then
            ReDim SystemArray(Convert.ToInt32(FileArray(i).Substring(FileArray(i).IndexOf("[", FileArray(i).IndexOf("[", FileArray(i).IndexOf(",")) + 1) + 1, FileArray(i).IndexOf("]", FileArray(i).IndexOf("]", FileArray(i).IndexOf(",")) + 1) - FileArray(i).IndexOf("[", FileArray(i).IndexOf("[", FileArray(i).IndexOf(",")) + 1) - 1)) - 1, SystemArray.GetUpperBound(1))
        End If
        For j = 1 To SystemArray.GetUpperBound(1) + 1
            Line = FileArray(i + j).Split(",".ToCharArray, SystemArray.GetUpperBound(0) + 1)
            For k = 0 To SystemArray.GetUpperBound(0)
                SystemArray(k, j - 1) = Line(k).Trim
            Next
        Next

        '***RETURN SystemArray
        PopulateSystemParameter = SystemArray
    End Function

    '*****************************************************************************
    'FUNCTION: ParaREAD.ReadFile
    '*****************************************************************************
    'Used to read given FileName in ParaMESH(PathRoot)
    '*****************************************************************************

    Public Shared Function ReadFile(ByVal FileName As String) As String()

        '***OPEN FileName AND READ CONTENTS TO TempArray
        ParaLOG.AppendFile("ParaMESH is opening the configuration file: " & FileName, "ParaREAD.ReadFile")
        Dim OpenFile As New System.IO.StreamReader(ParaMESH.PathRoot & "\" & FileName)
        ReDim TempArray(0)
        ParaLOG.AppendFile("ParaMESH is reading the configuration file: " & FileName, "ParaREAD.ReadFile")
        Do Until OpenFile.Peek = -1
            TempArray(TempArray.GetUpperBound(0)) = OpenFile.ReadLine()
            ReDim Preserve TempArray(TempArray.GetUpperBound(0) + 1)
        Loop
        ReDim Preserve TempArray(TempArray.GetUpperBound(0) - 1)

        '***CLOSE FileName (ITS CONTENTS HAVE BEEN SAVED TO TempArray)
        OpenFile.Close()

        '***RETURN TempArray
        ReadFile = TempArray
    End Function

    '*****************************************************************************
    'FUNCTION: ParaREAD.FileExists
    '*****************************************************************************
    'Checks if FileName exists in PathDirectory.  Returns True if it does, and 
    'False if it does not.
    '*****************************************************************************

    Public Shared Function FileExists(ByVal FileName As String) As Boolean

        '***ASSUME THE FILE EXISTS
        FileExists = True

        '***CHECK IF FileName EXISTS
        ParaLOG.AppendFile("ParaMESH is checking if the configuration file exists: " & FileName, "ParaREAD.FileExists")
        If System.IO.File.Exists(ParaMESH.PathRoot & "\" & FileName) = False Then
            MsgBox("The configuration file does not exist.  ParaMESH will load default parameter values for: " & FileName, MsgBoxStyle.OkOnly, "ParaMESH")
            FileExists = False
        End If
    End Function

    '*****************************************************************************
    'FUNCTION: ParaREAD.ValidFile
    '*****************************************************************************
    'Checks that TempArray contains data.  Returns True if TempArray has been 
    'populated, returns False if it has not.
    '*****************************************************************************

    Public Shared Function ValidFile(ByVal FileName As String, ByVal TempArray() As String) As Boolean

        '***ASSUME THE CONTENTS OF FileName EXIST
        ValidFile = True

        '***CHECK THAT THE CONTENTS OF FileName EXIST
        ParaLOG.AppendFile("ParaMESH is checking that the contents of the configuration file exist: " & FileName, "ParaREAD.ValidFile")
        If TempArray.Length = 0 Then
            MsgBox("The configuration file is corrupt.  ParaMESH will load default parameter values for: " & FileName, MsgBoxStyle.OkOnly, "ParaMESH")
            ValidFile = False
        End If
    End Function

    '*****************************************************************************
    'FUNCTION: ParaREAD.ValidHeader
    '*****************************************************************************
    'Checks if the header of FileName matches given FileHeader.
    '*****************************************************************************

    Public Shared Function ValidHeader(ByVal FileName As String, ByVal FileHeader As String, ByVal AcceptVariants As Boolean) As Boolean

        '***ASSUME FileName HAS A VALID FILE HEADER
        ValidHeader = True

        '***IF FileHeader IS DEFINED AND AcceptVariants IS FALSE, ONLY READ FileName WITH MATCHING HEADER
        If FileHeader <> Nothing And AcceptVariants = False Then
            ParaLOG.AppendFile("ParaMESH is checking the configuration file's header: " & FileName, "ParaREAD.ValidHeader")
            If TempArray(0).Trim.Length >= FileHeader.Trim.Length Then
                If TempArray(0).Trim.Substring(0, FileHeader.Length).ToUpper <> FileHeader.Trim.ToUpper Then
                    MsgBox("The configuration file may not be compatible with this version of MESH.  ParaMESH will load default values for: " & FileName, MsgBoxStyle.OkOnly, "ParaMESH")
                    ValidHeader = False
                End If
            Else
                If TempArray(0).Trim.ToUpper <> FileHeader.Trim.ToUpper Then
                    MsgBox("The configuration file may not be compatible with this version of MESH.  ParaMESH will load default values for: " & FileName, MsgBoxStyle.OkOnly, "ParaMESH")
                    ValidHeader = False
                End If
            End If
        End If
    End Function
End Class
