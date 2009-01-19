Public Class ParaWRITE

    '*****************************************************************************
    'CLASS: ParaWRITE
    '*****************************************************************************
    'ParaWRITE is used to write configuration files, and holds all subroutines 
    'used by ParaMESH to write any external file.
    '*****************************************************************************

    '****************************************************************************
    'PRIVATE ParaWRITE PARAMETERS
    '****************************************************************************
    'Parameters shared and used only within the ParaWRITE class.
    '****************************************************************************
    'Private Integers: i, j, k, m are unique to the ParaWRITE class so that 
    'similarly private integers from calling subroutines are not reset.
    '****************************************************************************

    Private Shared PadArray(0) As String, TextOutArray() As String, TempArray() As String, WriteString As String, i As Integer, j As Integer, k As Integer, m As Integer

    '*****************************************************************************
    'SUBROUTINE: ParaWRITE.Arguments
    '*****************************************************************************
    'ParaWRITE subroutines may be called by an external thread, which will only 
    'accept a single argument.  This subroutine is used to compile all arguments 
    'required by read subroutines into a single object array.
    'CallProcess denotes which read subroutine will be called.
    '*****************************************************************************

    Public Shared Function Arguments(ByVal CallProcess As String) As Object
        ReDim TempArray(0)

        '***POPULATE TempArray WITH ARGUMENTS FOR ProcessForm(Ext_ParaWRITE)
        TempArray(0) = CallProcess

        '***RETURN TempArray
        Arguments = TempArray
    End Function

    '*****************************************************************************
    'SUBROUTINE: ParaWRITE.MESH_RunOptions
    '*****************************************************************************
    'UPDATED: APR 22 2008 (DAN)  to use common ParaMESH subroutines.
    'UPDATED: JAN 09 2008 (DAN)  for ParaLOG approach.
    '*****************************************************************************
    'Write the run_options.ini configuration file for MESH.  The FileName and 
    'FileHeader are passed by the calling subroutine.
    '*****************************************************************************

    Public Shared Sub MESH_RunOptions(ByVal FileName As String, Optional ByVal FileHeader As String = Nothing)

        '***OPEN FileName TO WRITE FILE
        ParaLOG.AppendFile("ParaMESH is writing the configuration file: " & FileName, "ParaWRITE.MESH_RunOptions")
        Dim WriteFile As New System.IO.StreamWriter(ParaMESH.PathRoot & "\" & FileName)

        '***WRITE FileHeader IF IT IS DEFINED
        If FileHeader <> Nothing Then
            WriteFile.WriteLine(FileHeader)
        End If

        '***WRITE CONTROL FLAG HEADER INFORMATION
        ParaLOG.AppendFile("ControlFlag# ..writing header information: " & FileName, "ParaWRITE.MESH_RunOptions", "WrtVar")
        WriteFile.WriteLine("##### " & ParaLOAD.PrimaryParameters(3, "ControlFlag") & " #####")
        WriteFile.WriteLine("#".PadLeft(ParaWRITE.PadIndex("ControlFlag"), "-"))
        ParaLOG.AppendFile("ControlFlag# ..writing count: " & FileName, "ParaWRITE.MESH_RunOptions", "WrtVar")
        WriteFile.WriteLine((ParaMESH.ControlFlag.GetUpperBound(1) + 1).ToString.PadLeft(ParaWRITE.PadIndex("ControlFlag")) & " # Number of Control Flags")

        '***WRITE CONTROL FLAGS
        For i = 0 To ParaMESH.ControlFlag.GetUpperBound(1)
            ParaLOG.AppendFile("ControlFlag# ..writing Column 1, Line " & (i + 1).ToString & ": " & FileName, "ParaWRITE.MESH_RunOptions", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaMESH.ControlFlag(0, i), ParaMESH.SecondaryName(i, "ControlFlag")).PadLeft(ParaWRITE.PadIndex("ControlFlag")) & " #" & i + 1 & " ")
            ParaLOG.AppendFile("ControlFlag# ..writing Column 2, Line " & (i + 1).ToString & ": " & FileName, "ParaWRITE.MESH_RunOptions", "WrtVar")
            WriteFile.Write(ParaLOAD.SecondaryParameters(3, ParaMESH.SecondaryName(i, "ControlFlag")))
            WriteFile.WriteLine()
        Next

        '***WRITE GRID OUTPUT POINT HEADER INFORMATION
        ParaLOG.AppendFile("GridOutput# ..writing header information: " & FileName, "ParaWRITE.MESH_RunOptions", "WrtVar")
        WriteFile.WriteLine("##### " & ParaLOAD.PrimaryParameters(3, "GridOutput") & " #####")
        WriteFile.WriteLine("#".PadLeft(5, "-"))
        ParaLOG.AppendFile("GridOutput# ..writing count: " & FileName, "ParaWRITE.MESH_RunOptions", "WrtVar")
        If ParaMESH.IsEnabled("NoOutput", ParaMESH.SystemParameters, 1) = False Then
            WriteFile.Write((ParaMESH.GridOutput.GetUpperBound(2) + 1).ToString.PadLeft(5))
        Else

            '***NO GRID OUTPUT POINTS HAVE BEEN SELECTED
            WriteFile.Write("0".PadLeft(5))
        End If
        WriteFile.WriteLine(" # Number of Grid Output Points")
        ParaLOG.AppendFile("GridOutput# ..writing header information: " & FileName, "ParaWRITE.MESH_RunOptions", "WrtVar")
        For i = 0 To ParaMESH.GridOutput.GetUpperBound(2)
            WriteFile.Write("#".PadLeft(ParaWRITE.PadIndex("GridOutput"), "-"))
        Next
        WriteFile.WriteLine()

        '***WRITE GRID OUTPUT POINTS
        For j = 0 To ParaMESH.GridOutput.GetUpperBound(1)
            If ParaMESH.IsEnabled("NoOutput", ParaMESH.SystemParameters, 1) = False Then
                For i = 0 To ParaMESH.GridOutput.GetUpperBound(2)
                    ParaLOG.AppendFile("GridOutput# ..writing Column " & (i + 1).ToString & ", Line " & (j + 1).ToString & ": " & FileName, "ParaWRITE.MESH_RunOptions", "WrtVar")
                    WriteFile.Write(ParaWRITE.FormatString(ParaMESH.GridOutput(0, j, i), ParaMESH.SecondaryName(j, "GridOutput")).PadLeft(ParaWRITE.PadIndex("GridOutput")))
                Next
            End If
            WriteFile.WriteLine(" # " & ParaLOAD.SecondaryParameters(3, ParaMESH.SecondaryName(j, "GridOutput")))
        Next

        '***CLOSE FileName
        WriteFile.Close()
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaWRITE.MESH_HydrologyINI
    '*****************************************************************************
    'UPDATED: APR 22 2008 (DAN)  to use common ParaMESH subroutines.
    'UPDATED: JAN 09 2008 (DAN)  for ParaLOG approach.
    '*****************************************************************************
    'Write the hydrology.ini configuration file for MESH.  The FileName and 
    'FileHeader are passed by the calling subroutine.
    '*****************************************************************************

    Public Shared Sub MESH_HydrologyINI(ByVal FileName As String, Optional ByVal FileHeader As String = Nothing)

        '***OPEN FileName TO WRITE FILE
        ParaLOG.AppendFile("ParaMESH is writing the configuration file: " & FileName, "ParaWRITE.MESH_HydrologyINI")
        Dim WriteFile As New System.IO.StreamWriter(ParaMESH.PathRoot & "\" & FileName)

        '***WRITE FileHeader IF IT IS DEFINED
        If FileHeader <> Nothing Then
            WriteFile.WriteLine(FileHeader)
        End If

        '***WRITE OPTION FLAG HEADER INFORMATION
        ParaLOG.AppendFile("OptionFlag# ..writing header information: " & FileName, "ParaWRITE.MESH_HydrologyINI", "WrtVar")
        WriteFile.WriteLine("##### " & ParaLOAD.PrimaryParameters(3, "OptionFlag") & " #####")
        WriteFile.WriteLine("#".PadLeft(ParaWRITE.PadIndex("OptionFlag"), "-"))
        ParaLOG.AppendFile("OptionFlag# ..writing count: " & FileName, "ParaWRITE.MESH_HydrologyINI", "WrtVar")
        WriteFile.WriteLine((ParaMESH.OptionFlag.GetUpperBound(1) + 1).ToString.PadLeft(ParaWRITE.PadIndex("OptionFlag")) & " # Number of Option Flags")

        '***WRITE OPTION FLAGS
        For i = 0 To ParaMESH.OptionFlag.GetUpperBound(1)
            ParaLOG.AppendFile("OptionFlag# ..writing Column 1, Line " & (i + 1).ToString & ": " & FileName, "ParaWRITE.MESH_HydrologyINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaMESH.OptionFlag(0, i), ParaMESH.SecondaryName(i, "OptionFlag")).PadLeft(ParaWRITE.PadIndex("OptionFlag")) & " #" & i + 1 & " ")
            ParaLOG.AppendFile("OptionFlag# ..writing Column 2, Line " & (i + 1).ToString & ": " & FileName, "ParaWRITE.MESH_HydrologyINI", "WrtVar")
            WriteFile.WriteLine(ParaLOAD.SecondaryParameters(3, ParaMESH.SecondaryName(i, "OptionFlag")))
        Next

        '***WRITE CHANNEL ROUGHNESS FACTOR (WF_R2) HEADER INFORMATION
        '***CHECK FILE VERSION (HYDROLOGY.INI FILES WRITTEN BY ParaMESH THAT DO NOT HAVE WF_R2 VALUES HAVE MESH_driver RELEASE AND COLON IN FileHeader)
        If FileHeader.IndexOf(":") = -1 Then
            ParaLOG.AppendFile("WF_R2# ..writing header information: " & FileName, "ParaWRITE.MESH_HydrologyINI", "WrtVar")
            WriteFile.WriteLine("##### Channel River Roughness Factors (WF_R2) #####")
            For i = 0 To ParaMESH.WF_R2.GetUpperBound(1)
                WriteFile.Write("#".PadLeft(ParaWRITE.PadIndex("WF_R2"), "-"))
            Next
            WriteFile.WriteLine()

            '***WRITE CHANNEL ROUGHNESS FACTORS (WF_R2)
            For i = 0 To ParaMESH.WF_R2.GetUpperBound(1)
                ParaLOG.AppendFile("WF_R2# ..writing Column " & (i + 1).ToString & ": " & FileName, "ParaWRITE.MESH_HydrologyINI", "WrtVar")
                WriteFile.Write(ParaWRITE.FormatString(ParaMESH.WF_R2(0, i), "WF_R2").PadLeft(ParaWRITE.PadIndex("WF_R2")))
            Next
            WriteFile.WriteLine()
        End If

        '***WRITE GRU-INDEPENDENT HYDROLOGIC PARAMETER HEADER INFORMATION
        ParaLOG.AppendFile("IndependentGRU# ..writing header information: " & FileName, "ParaWRITE.MESH_HydrologyINI", "WrtVar")
        WriteFile.WriteLine("##### " & ParaLOAD.PrimaryParameters(3, "IndependentGRU") & " #####")
        WriteFile.WriteLine("#".PadLeft(ParaWRITE.PadIndex("IndependentGRU"), "-"))
        ParaLOG.AppendFile("IndependentGRU# ..writing count: " & FileName, "ParaWRITE.MESH_HydrologyINI", "WrtVar")
        WriteFile.WriteLine((ParaMESH.IndependentGRU.GetUpperBound(1) + 1).ToString.PadLeft(ParaWRITE.PadIndex("IndependentGRU")) & " # Number of GRU-Independent Hydrologic Parameters")

        '***WRITE GRU-INDEPENDENT HYDROLOGIC PARAMETERS
        For i = 0 To ParaMESH.IndependentGRU.GetUpperBound(1)
            ParaLOG.AppendFile("IndependentGRU# ..writing Column 1, Line " & (i + 1).ToString & ": " & FileName, "ParaWRITE.MESH_HydrologyINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaMESH.IndependentGRU(0, i), ParaMESH.SecondaryName(i, "IndependentGRU")).PadLeft(ParaWRITE.PadIndex("IndependentGRU")) & " #" & i + 1 & " ")
            ParaLOG.AppendFile("IndependentGRU# ..writing Column 2, Line " & (i + 1).ToString & ": " & FileName, "ParaWRITE.MESH_HydrologyINI", "WrtVar")
            WriteFile.WriteLine(ParaLOAD.SecondaryParameters(3, ParaMESH.SecondaryName(i, "IndependentGRU")))
        Next

        '***WRITING GRU-DEPENDENT HYDROLOGIC PARAMETER HEADER INFORMATION
        ParaLOG.AppendFile("DependentGRU# ..writing header information: " & FileName, "ParaWRITE.MESH_HydrologyINI", "WrtVar")
        WriteFile.WriteLine("##### " & ParaLOAD.PrimaryParameters(3, "DependentGRU") & " #####")
        WriteFile.WriteLine("#".PadLeft(8, "-"))
        ParaLOG.AppendFile("DependentGRU# ..writing count: " & FileName, "ParaWRITE.MESH_HydrologyINI", "WrtVar")
        WriteFile.WriteLine((ParaMESH.DependentGRU.GetUpperBound(2) + 1).ToString.PadLeft(8) & " # Number of GRUs")
        WriteFile.WriteLine((ParaMESH.DependentGRU.GetUpperBound(1) + 1).ToString.PadLeft(8) & " # Number of GRU-Dependent Hydrologic Parameters")
        For i = 0 To ParaMESH.DependentGRU.GetUpperBound(2)
            WriteFile.Write("#".PadLeft(ParaWRITE.PadIndex("DependentGRU"), "-"))
        Next
        WriteFile.WriteLine()

        '***WRITE GRU-DEPENDENT HYDROLOGIC PARAMETERS
        For j = 0 To ParaMESH.DependentGRU.GetUpperBound(1)
            For i = 0 To ParaMESH.DependentGRU.GetUpperBound(2)
                ParaLOG.AppendFile("DependentGRU# ..writing Column " & (i + 1).ToString & ", Line " & (j + 1).ToString & ": " & FileName, "ParaWRITE.MESH_HydrologyINI", "WrtVar")
                WriteFile.Write(ParaWRITE.FormatString(ParaMESH.DependentGRU(0, j, i), ParaMESH.SecondaryName(j, "DependentGRU")).PadLeft(ParaWRITE.PadIndex("DependentGRU")))
            Next
            WriteFile.WriteLine(" # " & ParaLOAD.SecondaryParameters(3, ParaMESH.SecondaryName(j, "DependentGRU")))
        Next

        '***CLOSE FileName
        WriteFile.Close()
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaWRITE.MESH_ClassINI
    '*****************************************************************************
    'UPDATED: APR 22 2008 (DAN)  to use common ParaMESH subroutines.
    'UPDATED: JAN 10 2008 (DAN)  combined formatting approach (eliminates need 
    '                            for ParaWRITE.Format)
    'UPDATED: JAN 09 2008 (DAN)  for ParaLOG approach
    '*****************************************************************************
    'Write the class.ini configuration file (for MESH).  The FileName and 
    'FileHeader are passed by the calling subroutine.
    '*****************************************************************************

    Public Shared Sub MESH_ClassINI(ByVal FileName As String, Optional ByVal FileHeader As String = Nothing)

        '***OPEN FileName FOR WRITING
        ParaLOG.AppendFile("ParaMESH is writing the configuration file: " & FileName, "ParaWRITE.MESH_ClassINI")
        Dim WriteFile As New System.IO.StreamWriter(ParaMESH.PathRoot & "\" & FileName)

        '***WRITE FileHeader IF IT IS DEFINED
        If FileHeader <> Nothing Then
            WriteFile.WriteLine(FileHeader)
        End If

        '***WRITE INFORMATION BLOCK
        ParaLOG.AppendFile("TITLE# ..writing Line 1: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.WriteLine("  " & ParaWRITE.FormatString(ParaMESH.TITLE(0), "TITLE").PadRight(ParaWRITE.PadIndex("TITLE")))
        ParaLOG.AppendFile("NAME#: ..writing Line 2: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.WriteLine("  " & ParaWRITE.FormatString(ParaMESH.NAME(0), "NAME").PadRight(ParaWRITE.PadIndex("NAME")))
        ParaLOG.AppendFile("PLACE# ..writing Line 3: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.WriteLine("  " & ParaWRITE.FormatString(ParaMESH.PLACE(0), "PLACE").PadRight(ParaWRITE.PadIndex("PLACE")))
        ParaLOG.AppendFile("DEGLAT# ..writing Column 1, Line 4: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.DEGLAT(0), "DEGLAT").PadLeft(ParaWRITE.PadIndex("DEGLAT")))
        ParaLOG.AppendFile("DEGLON# ..writing Column 2, Line 4: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.DEGLON(0), "DEGLON").PadLeft(ParaWRITE.PadIndex("DEGLON")))
        ParaLOG.AppendFile("ZRFM# ..writing Column 3, Line 4: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.ZRFM(0), "ZRFM").PadLeft(ParaWRITE.PadIndex("ZRFM")))
        ParaLOG.AppendFile("ZRFH# ..writing Column 4, Line 4: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.ZRFH(0), "ZRFH").PadLeft(ParaWRITE.PadIndex("ZRFH")))
        ParaLOG.AppendFile("ZBLD# ..writing Column 5, Line 4: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.ZBLD(0), "ZBLD").PadLeft(ParaWRITE.PadIndex("ZBLD")))
        ParaLOG.AppendFile("GC# ..writing Column 6, Line 4: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.GC(0), "GC").PadLeft(ParaWRITE.PadIndex("GC")))
        ParaLOG.AppendFile("ILW# ..writing Column 7, Line 4: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.ILW(0), "ILW").PadLeft(ParaWRITE.PadIndex("ILW")))
        ParaLOG.AppendFile("GRID# ..writing Column 8, Line 4: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.GRID(0), "GRID").PadLeft(ParaWRITE.PadIndex("GRID")))
        ParaLOG.AppendFile("GRU# ..writing Column 9, Line 4: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.WriteLine((ParaMESH.GRU.GetUpperBound(1) + 1).ToString.PadLeft(5))

        '***WRITE FIRST BLOCK
        For i = 0 To ParaMESH.GRU.GetUpperBound(1)
            For j = 0 To ParaMESH.FCAN.GetUpperBound(1)
                ParaLOG.AppendFile("FCAN# ..writing Column " & j.ToString & ", Line 5 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
                WriteFile.Write(ParaWRITE.FormatString(ParaMESH.FCAN(0, j, i), "FCAN").PadLeft(ParaWRITE.PadIndex("FCAN")))
            Next
            For j = 0 To ParaMESH.LAMX.GetUpperBound(1)
                ParaLOG.AppendFile("LAMX# ..writing Column " & j.ToString & ", Line 5 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
                WriteFile.Write(ParaWRITE.FormatString(ParaMESH.LAMX(0, j, i), "LAMX").PadLeft(ParaWRITE.PadIndex("LAMX")))
            Next
            WriteFile.WriteLine()
            For j = 0 To ParaMESH.LNZ0.GetUpperBound(1)
                ParaLOG.AppendFile("LNZ0# ..writing Column " & j.ToString & ", Line 6 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
                WriteFile.Write(ParaWRITE.FormatString(ParaMESH.LNZ0(0, j, i), "LNZ0").PadLeft(ParaWRITE.PadIndex("LNZ0")))
            Next
            For j = 0 To ParaMESH.LAMN.GetUpperBound(1)
                ParaLOG.AppendFile("LAMN# ..writing Column " & j.ToString & ", Line 6 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
                WriteFile.Write(ParaWRITE.FormatString(ParaMESH.LAMN(0, j, i), "LAMN").PadLeft(ParaWRITE.PadIndex("LAMN")))
            Next
            WriteFile.WriteLine()
            For j = 0 To ParaMESH.ALVC.GetUpperBound(1)
                ParaLOG.AppendFile("ALVC# ..writing Column " & j.ToString & ", Line 7 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
                WriteFile.Write(ParaWRITE.FormatString(ParaMESH.ALVC(0, j, i), "ALVC").PadLeft(ParaWRITE.PadIndex("ALVC")))
            Next
            For j = 0 To ParaMESH.CMAS.GetUpperBound(1)
                ParaLOG.AppendFile("CMAS# ..writing Column " & j.ToString & ", Line 7 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
                WriteFile.Write(ParaWRITE.FormatString(ParaMESH.CMAS(0, j, i), "CMAS").PadLeft(ParaWRITE.PadIndex("CMAS")))
            Next
            WriteFile.WriteLine()
            For j = 0 To ParaMESH.ALIC.GetUpperBound(1)
                ParaLOG.AppendFile("ALIC# ..writing Column " & j.ToString & ", Line 8 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
                WriteFile.Write(ParaWRITE.FormatString(ParaMESH.ALIC(0, j, i), "ALIC").PadLeft(ParaWRITE.PadIndex("ALIC")))
            Next
            For j = 0 To ParaMESH.ROOT.GetUpperBound(1)
                ParaLOG.AppendFile("ROOT# ..writing Column " & j.ToString & ", Line 8 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
                WriteFile.Write(ParaWRITE.FormatString(ParaMESH.ROOT(0, j, i), "ROOT").PadLeft(ParaWRITE.PadIndex("ROOT")))
            Next
            WriteFile.WriteLine()
            For j = 0 To ParaMESH.RSMN.GetUpperBound(1)
                ParaLOG.AppendFile("RSMN# ..writing Column " & j.ToString & ", Line 9 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
                WriteFile.Write(ParaWRITE.FormatString(ParaMESH.RSMN(0, j, i), "RSMN").PadLeft(ParaWRITE.PadIndex("RSMN")))
            Next
            ParaLOG.AppendFile("RSERVC01# ..writing Column " & j.ToString & ", Line 9 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC01(i), "RSERVC01").PadLeft(ParaWRITE.PadIndex("RSERVC01")))
            For j = 0 To ParaMESH.QA50.GetUpperBound(1)
                ParaLOG.AppendFile("QA50# ..writing Column " & j.ToString & ", Line 9 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
                WriteFile.Write(ParaWRITE.FormatString(ParaMESH.QA50(0, j, i), "QA50").PadLeft(ParaWRITE.PadIndex("QA50")))
            Next
            WriteFile.WriteLine()
            For j = 0 To ParaMESH.VPDA.GetUpperBound(1)
                ParaLOG.AppendFile("VPDA# ..writing Column " & j.ToString & ", Line 10 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
                WriteFile.Write(ParaWRITE.FormatString(ParaMESH.VPDA(0, j, i), "VPDA").PadLeft(ParaWRITE.PadIndex("VPDA")))
            Next
            ParaLOG.AppendFile("RSERVC02# ..writing Column " & j.ToString & ", Line 10 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC02(i), "RSERVC02").PadLeft(ParaWRITE.PadIndex("RSERVC02")))
            For j = 0 To ParaMESH.VPDB.GetUpperBound(1)
                ParaLOG.AppendFile("VPDB# ..writing Column " & j.ToString & ", Line 10 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
                WriteFile.Write(ParaWRITE.FormatString(ParaMESH.VPDB(0, j, i), "VPDB").PadLeft(ParaWRITE.PadIndex("VPDB")))
            Next
            WriteFile.WriteLine()
            For j = 0 To ParaMESH.PSGA.GetUpperBound(1)
                ParaLOG.AppendFile("PSGA# ..writing Column " & j.ToString & ", Line 11 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
                WriteFile.Write(ParaWRITE.FormatString(ParaMESH.PSGA(0, j, i), "PSGA").PadLeft(ParaWRITE.PadIndex("PSGA")))
            Next
            ParaLOG.AppendFile("RSERVC03# ..writing Column " & j.ToString & ", Line 11 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC03(i), "RSERVC03").PadLeft(ParaWRITE.PadIndex("RSERVC03")))
            For j = 0 To ParaMESH.PSGB.GetUpperBound(1)
                ParaLOG.AppendFile("PSGB# ..writing Column " & j.ToString & ", Line 11 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
                WriteFile.Write(ParaWRITE.FormatString(ParaMESH.PSGB(0, j, i), "PSGB").PadLeft(ParaWRITE.PadIndex("PSGB")))
            Next
            WriteFile.WriteLine()
            ParaLOG.AppendFile("DRN# ..writing Column 1, Line 12 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaMESH.DRN(0, i), "DRN").PadLeft(ParaWRITE.PadIndex("DRN")))
            ParaLOG.AppendFile("SDEP# ..writing Column 2, Line 12 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaMESH.SDEP(0, i), "SDEP").PadLeft(ParaWRITE.PadIndex("SDEP")))
            ParaLOG.AppendFile("FARE# ..writing Column 3, Line 12 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaMESH.FARE(0, i), "FARE").PadLeft(ParaWRITE.PadIndex("FARE")))
            ParaLOG.AppendFile("DDEN# ..writing Column 4, Line 12 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaMESH.DDEN(0, i), "DDEN").PadLeft(ParaWRITE.PadIndex("DDEN")))
            ParaLOG.AppendFile("RSERVC04# ..writing Column 5, Line 12 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC04(i), "RSERVC04").PadLeft(ParaWRITE.PadIndex("RSERVC04")))
            ParaLOG.AppendFile("RSERVC05# ..writing Column 6, Line 12 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC05(i), "RSERVC05").PadLeft(ParaWRITE.PadIndex("RSERVC05")))
            ParaLOG.AppendFile("RSERVC06# ..writing Column 7, Line 12 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC06(i), "RSERVC06").PadLeft(ParaWRITE.PadIndex("RSERVC06")))
            ParaLOG.AppendFile("RSERVC07# ..writing Column 8, Line 12 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC07(i), "RSERVC07").PadLeft(ParaWRITE.PadIndex("RSERVC07")))
            ParaLOG.AppendFile("RSERVC08# ..writing Column 9, Line 12 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC08(i), "RSERVC08").PadLeft(ParaWRITE.PadIndex("RSERVC08")))
            WriteFile.WriteLine()
            ParaLOG.AppendFile("XSLP# ..writing Column 1, Line 13 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaMESH.XSLP(0, i), "XSLP").PadLeft(ParaWRITE.PadIndex("XSLP")))
            ParaLOG.AppendFile("GRKF# ..writing Column 2, Line 13 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaMESH.GRKF(0, i), "GRKF").PadLeft(ParaWRITE.PadIndex("GRKF")))
            ParaLOG.AppendFile("WFSF# ..writing Column 3, Line 13 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaMESH.WFSF(0, i), "WFSF").PadLeft(ParaWRITE.PadIndex("WFSF")))
            ParaLOG.AppendFile("WFCI# ..writing Column 4, Line 13 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaMESH.WFCI(0, i), "WFCI").PadLeft(ParaWRITE.PadIndex("WFCI")))
            ParaLOG.AppendFile("GRU# ..writing Column 5, Line 13 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write((i + 1).ToString.PadLeft(8))
            ParaLOG.AppendFile("RSERVC09# ..writing Column 6, Line 13 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC09(i), "RSERVC09").PadLeft(ParaWRITE.PadIndex("RSERVC09")))
            ParaLOG.AppendFile("RSERVC10# ..writing Column 7, Line 13 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC10(i), "RSERVC10").PadLeft(ParaWRITE.PadIndex("RSERVC10")))
            ParaLOG.AppendFile("RSERVC11# ..writing Column 8, Line 13 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC11(i), "RSERVC11").PadLeft(ParaWRITE.PadIndex("RSERVC11")))
            ParaLOG.AppendFile("RSERVC12# ..writing Column 9, Line 13 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC12(i), "RSERVC12").PadLeft(ParaWRITE.PadIndex("RSERVC12")))
            WriteFile.WriteLine()

            '***WRITE SECOND BLOCK
            For j = 0 To ParaMESH.SAND.GetUpperBound(1)
                ParaLOG.AppendFile("SAND# ..writing Column " & j.ToString & ", Line 14 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
                WriteFile.Write(ParaWRITE.FormatString(ParaMESH.SAND(0, j, i), "SAND").PadLeft(ParaWRITE.PadIndex("SAND")))
            Next
            ParaLOG.AppendFile("RSERVC13# ..writing Column 4, Line 14 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC13(i), "RSERVC13").PadLeft(ParaWRITE.PadIndex("RSERVC13")))
            ParaLOG.AppendFile("RSERVC14# ..writing Column 5, Line 14 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC14(i), "RSERVC14").PadLeft(ParaWRITE.PadIndex("RSERVC14")))
            ParaLOG.AppendFile("RSERVC15# ..writing Column 6, Line 14 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC15(i), "RSERVC15").PadLeft(ParaWRITE.PadIndex("RSERVC15")))
            ParaLOG.AppendFile("RSERVC16# ..writing Column 7, Line 14 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC16(i), "RSERVC16").PadLeft(ParaWRITE.PadIndex("RSERVC16")))
            WriteFile.WriteLine()
            For j = 0 To ParaMESH.CLAY.GetUpperBound(1)
                ParaLOG.AppendFile("CLAY# ..writing Column " & j.ToString & ", Line 15 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
                WriteFile.Write(ParaWRITE.FormatString(ParaMESH.CLAY(0, j, i), "CLAY").PadLeft(ParaWRITE.PadIndex("CLAY")))
            Next
            ParaLOG.AppendFile("RSERVC17# ..writing Column 4, Line 15 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC17(i), "RSERVC17").PadLeft(ParaWRITE.PadIndex("RSERVC17")))
            ParaLOG.AppendFile("RSERVC18# ..writing Column 5, Line 15 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC18(i), "RSERVC18").PadLeft(ParaWRITE.PadIndex("RSERVC18")))
            ParaLOG.AppendFile("RSERVC19# ..writing Column 6, Line 15 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC19(i), "RSERVC19").PadLeft(ParaWRITE.PadIndex("RSERVC19")))
            ParaLOG.AppendFile("RSERVC20# ..writing Column 7, Line 15 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC20(i), "RSERVC20").PadLeft(ParaWRITE.PadIndex("RSERVC20")))
            WriteFile.WriteLine()
            For j = 0 To ParaMESH.ORGM.GetUpperBound(1)
                ParaLOG.AppendFile("ORGM# ..writing Column " & j.ToString & ", Line 16 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
                WriteFile.Write(ParaWRITE.FormatString(ParaMESH.ORGM(0, j, i), "ORGM").PadLeft(ParaWRITE.PadIndex("ORGM")))
            Next
            ParaLOG.AppendFile("RSERVC21# ..writing Column 4, Line 16 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC21(i), "RSERVC21").PadLeft(ParaWRITE.PadIndex("RSERVC21")))
            ParaLOG.AppendFile("RSERVC22# ..writing Column 5, Line 16 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC22(i), "RSERVC22").PadLeft(ParaWRITE.PadIndex("RSERVC22")))
            ParaLOG.AppendFile("RSERVC23# ..writing Column 6, Line 16 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC23(i), "RSERVC23").PadLeft(ParaWRITE.PadIndex("RSERVC23")))
            ParaLOG.AppendFile("RSERVC24# ..writing Column 7, Line 16 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC24(i), "RSERVC24").PadLeft(ParaWRITE.PadIndex("RSERVC24")))
            WriteFile.WriteLine()
            For j = 0 To ParaMESH.TBAR.GetUpperBound(1)
                ParaLOG.AppendFile("TBAR# ..writing Column " & j.ToString & ", Line 17 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
                WriteFile.Write(ParaWRITE.FormatString(ParaMESH.TBAR(0, j, i), "TBAR").PadLeft(ParaWRITE.PadIndex("TBAR")))
            Next
            ParaLOG.AppendFile("TCAN# ..writing Column 4, Line 17 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaMESH.TCAN(0, i), "TCAN").PadLeft(ParaWRITE.PadIndex("TCAN")))
            ParaLOG.AppendFile("TSNO# ..writing Column 5, Line 17 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaMESH.TSNO(0, i), "TSNO").PadLeft(ParaWRITE.PadIndex("TSNO")))
            ParaLOG.AppendFile("TPND# ..writing Column 6, Line 17 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaMESH.TPND(0, i), "TPND").PadLeft(ParaWRITE.PadIndex("TPND")))
            ParaLOG.AppendFile("RSERVC25# ..writing Column 7, Line 17 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC25(i), "RSERVC25").PadLeft(ParaWRITE.PadIndex("RSERVC25")))
            WriteFile.WriteLine()
            For j = 0 To ParaMESH.THLQ.GetUpperBound(1)
                ParaLOG.AppendFile("THLQ# ..writing Column " & j.ToString & ", Line 18 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
                WriteFile.Write(ParaWRITE.FormatString(ParaMESH.THLQ(0, j, i), "THLQ").PadLeft(ParaWRITE.PadIndex("THLQ")))
            Next
            For j = 0 To ParaMESH.THIC.GetUpperBound(1)
                ParaLOG.AppendFile("THIC# ..writing Column " & j.ToString & ", Line 18 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
                WriteFile.Write(ParaWRITE.FormatString(ParaMESH.THIC(0, j, i), "THIC").PadLeft(ParaWRITE.PadIndex("THIC")))
            Next
            ParaLOG.AppendFile("ZPND# ..writing Column 7, Line 18 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaMESH.ZPND(0, i), "ZPND").PadLeft(ParaWRITE.PadIndex("ZPND")))
            WriteFile.WriteLine()
            ParaLOG.AppendFile("RCAN# ..writing Column 1, Line 19 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaMESH.RCAN(0, i), "RCAN").PadLeft(ParaWRITE.PadIndex("RCAN")))
            ParaLOG.AppendFile("SCAN# ..writing Column 2, Line 19 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaMESH.SCAN(0, i), "SCAN").PadLeft(ParaWRITE.PadIndex("SCAN")))
            ParaLOG.AppendFile("SNO# ..writing Column 3, Line 19 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaMESH.SNO(0, i), "SNO").PadLeft(ParaWRITE.PadIndex("SNO")))
            ParaLOG.AppendFile("ALBS# ..writing Column 4, Line 19 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaMESH.ALBS(0, i), "ALBS").PadLeft(ParaWRITE.PadIndex("ALBS")))
            ParaLOG.AppendFile("RHOS# ..writing Column 5, Line 19 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaMESH.RHOS(0, i), "RHOS").PadLeft(ParaWRITE.PadIndex("RHOS")))
            ParaLOG.AppendFile("GRO# ..writing Column 6, Line 19 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaMESH.GRO(0, i), "GRO").PadLeft(ParaWRITE.PadIndex("GRO")))
            ParaLOG.AppendFile("RSERVC26# ..writing Column 7, Line 19 of Class " & i.ToString & ": " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaLOAD.RSERVC26(i), "RSERVC26").PadLeft(ParaWRITE.PadIndex("RSERVC26")))
            WriteFile.WriteLine()
        Next

        '***WRITE MODEL RUN TIMES
        ParaLOG.AppendFile("HourlyStartDay# ..writing Column 1, Line 20: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.HourlyStartDay(0), "HourlyStartDay").PadLeft(ParaWRITE.PadIndex("HourlyStartDay")))
        ParaLOG.AppendFile("HourlyStopDay# ..writing Column 2, Line 20: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.HourlyStopDay(0), "HourlyStopDay").PadLeft(ParaWRITE.PadIndex("HourlyStopDay")))
        ParaLOG.AppendFile("DailyStartDay# ..writing Column 3, Line 20: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.DailyStartDay(0), "DailyStartDay").PadLeft(ParaWRITE.PadIndex("DailyStartDay")))
        ParaLOG.AppendFile("DailyStopDay# ..writing Column 4, Line 20: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.DailyStopDay(0), "DailyStopDay").PadLeft(ParaWRITE.PadIndex("DailyStopDay")))
        ParaLOG.AppendFile("SimStartDay# ..writing Column 5, Line 20: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.SimStartDay(0), "SimStartDay").PadLeft(ParaWRITE.PadIndex("SimStartDay")))
        ParaLOG.AppendFile("SimStopDay# ..writing Column 6, Line 20: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.SimStopDay(0), "SimStopDay").PadLeft(ParaWRITE.PadIndex("SimStopDay")))
        WriteFile.WriteLine()
        ParaLOG.AppendFile("HourlyStartYear# ..writing Column 1, Line 21: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.HourlyStartYear(0), "HourlyStartYear").PadLeft(ParaWRITE.PadIndex("HourlyStartYear")))
        ParaLOG.AppendFile("HourlyStopYear# ..writing Column 2, Line 21: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.HourlyStopYear(0), "HourlyStopYear").PadLeft(ParaWRITE.PadIndex("HourlyStopYear")))
        ParaLOG.AppendFile("DailyStartYear# ..writing Column 3, Line 21: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.DailyStartYear(0), "DailyStartYear").PadLeft(ParaWRITE.PadIndex("DailyStartYear")))
        ParaLOG.AppendFile("DailyStopYear# ..writing Column 4, Line 21: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.DailyStopYear(0), "DailyStopYear").PadLeft(ParaWRITE.PadIndex("DailyStopYear")))
        ParaLOG.AppendFile("SimStartYear# ..writing Column 5, Line 21: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.SimStartYear(0), "SimStartYear").PadLeft(ParaWRITE.PadIndex("SimStartYear")))
        ParaLOG.AppendFile("SimStopYear# ..writing Column 6, Line 21: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.SimStopYear(0), "SimStopYear").PadLeft(ParaWRITE.PadIndex("SimStopYear")))
        WriteFile.WriteLine()
        ParaLOG.AppendFile("MetStartHour# ..writing Column 1, Line 22: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.MetStartHour(0), "MetStartHour").PadLeft(ParaWRITE.PadIndex("MetStartHour")))
        ParaLOG.AppendFile("MetStartMin# ..writing Column 2, Line 22: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.MetStartMin(0), "MetStartMin").PadLeft(ParaWRITE.PadIndex("MetStartMin")))
        ParaLOG.AppendFile("MetStartDay# ..writing Column 3, Line 22: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.MetStartDay(0), "MetStartDay").PadLeft(ParaWRITE.PadIndex("MetStartDay")))
        ParaLOG.AppendFile("MetStartYear# ..writing Column 4, Line 22: " & FileName, "ParaWRITE.MESH_ClassINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.MetStartYear(0), "MetStartYear").PadLeft(ParaWRITE.PadIndex("MetStartYear")))
        WriteFile.WriteLine()

        '***CLOSE FileName
        WriteFile.Close()

        '***CALL ParaWRITE(PadClassINI) TO PAD FileName
        ParaWRITE.PadClassINI(FileName)
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaWRITE.WatflowINI
    '*****************************************************************************
    'UPDATED: APR 22 2008  to use common ParaMESH subroutines.
    '*****************************************************************************
    'Write the watflow.ini configuration file for WATFLOW.  The FileName and 
    'FileHeader are passed by the calling subroutine.
    '*****************************************************************************

    Public Shared Sub WatflowINI(ByVal FileName As String, Optional ByVal FileHeader As String = Nothing, Optional ByVal BakFlag As Integer = 0)

        '***OPEN FileName TO WRITE FILE
        ParaLOG.AppendFile("ParaMESH is writing the configuration file: " & FileName, "ParaWRITE.WatflowINI")
        Dim WriteFile As New System.IO.StreamWriter(ParaMESH.PathRoot & "\" & FileName)

        '***WRITE FileHeader IF IT IS DEFINED
        If FileHeader <> Nothing Then
            If FileHeader.Length > 23 Then
                WriteFile.Write(FileHeader.Substring(0, 23))
            Else
                WriteFile.WriteLine(FileHeader)
            End If
        End If
        ParaLOG.AppendFile("BasinID#", "ParaWRITE.WatflowINI", "WrtVar")
        WriteFile.WriteLine(ParaWRITE.FormatString(ParaMESH.BasinID(BakFlag), "BasinID").ToLower.PadRight(ParaWRITE.PadIndex("BasinID")))

        '***WRITE CHANNEL ROUGHNESS AND SCALING FACTORS
        WriteFile.WriteLine("-----#-----#-----#-----#-----#-----# wf_r2, scaling factor")
        For i = 0 To ParaMESH.WF_R2.GetUpperBound(1)
            ParaLOG.AppendFile("WF_R2# ..writing Column " & (i + 1).ToString, "ParaWRITE.WatflowINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaMESH.WF_R2(BakFlag, i), "WF_R2").PadLeft(ParaWRITE.PadIndex("WF_R2")))
        Next
        ParaLOG.AppendFile("ScalingFactor#", "ParaWRITE.WatflowINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.ScalingFactor(BakFlag), "ScalingFactor").PadLeft(ParaWRITE.PadIndex("ScalingFactor")))
        WriteFile.WriteLine()

        '***WRITE WATFLOW FLAGS
        For i = 0 To ParaMESH.WatflowFlag.GetUpperBound(1)
            ParaLOG.AppendFile("WatflowFlag# ..writing Column 1, Line " & (i + 1).ToString & ": " & FileName, "ParaWRITE.WatflowINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaMESH.WatflowFlag(BakFlag, i), "WatflowFlag").PadLeft(ParaWRITE.PadIndex("WatflowFlag")) & "#")
            ParaLOG.AppendFile("WatflowFlag# ..writing Column 2, Line " & (i + 1).ToString & ": " & FileName, "ParaWRITE.WatflowINI", "WrtVar")
            WriteFile.WriteLine(ParaLOAD.SecondaryParameters(3, ParaMESH.SecondaryName(i, "WatflowFlag")))
        Next

        '***WRITE PONDING LIMITS
        WriteFile.WriteLine("-----#-----#-----#-----# zplimg0, zplims0, zplimp0, zplimps0")
        ParaLOG.AppendFile("ZPLIMG0#", "ParaWRITE.WatflowINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.ZPLIMG0(BakFlag), "ZPLIMG0").PadLeft(ParaWRITE.PadIndex("ZPLIMG0")))
        ParaLOG.AppendFile("ZPLIMS0#", "ParaWRITE.WatflowINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.ZPLIMS0(BakFlag), "ZPLIMS0").PadLeft(ParaWRITE.PadIndex("ZPLIMS0")))
        ParaLOG.AppendFile("ZPLIMP0#", "ParaWRITE.WatflowINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.ZPLIMP0(BakFlag), "ZPLIMP0").PadLeft(ParaWRITE.PadIndex("ZPLIMP0")))
        ParaLOG.AppendFile("ZPLIMPS0#", "ParaWRITE.WatflowINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.ZPLIMPS0(BakFlag), "ZPLIMPS0").PadLeft(ParaWRITE.PadIndex("ZPLIMPS0")))
        WriteFile.WriteLine()

        '***WRITE SNOW LIMITS AND INITIAL GROUND WATER VALUE
        WriteFile.WriteLine("-----#-----#-----#-----#-----#-----#-----# snowlimv, gwstor_min")
        For i = 0 To ParaMESH.D100A.GetUpperBound(1)
            ParaLOG.AppendFile("D100A# ..writing Column " & (i + 1).ToString, "ParaWRITE.WatflowINI", "WrtVar")
            WriteFile.Write(ParaWRITE.FormatString(ParaMESH.D100A(BakFlag, i), "D100A").PadLeft(ParaWRITE.PadIndex("D100A")))
        Next
        ParaLOG.AppendFile("GWINIT#", "ParaWRITE.WatflowINI", "WrtVar")
        WriteFile.Write(ParaWRITE.FormatString(ParaMESH.GWINIT(BakFlag), "GWINIT").PadLeft(ParaWRITE.PadIndex("GWINIT")))
        WriteFile.WriteLine()

        '***WRITE GRID OUTPUT POINT COUNT
        If ParaMESH.IsEnabled("NoOutput", ParaMESH.SystemParameters, 1) = False Then
            ParaLOG.AppendFile("GridOutput# ..writing count", "ParaWRITE.WatflowINI", "WrtVar")
            WriteFile.Write((ParaMESH.GridOutput.GetUpperBound(2) + 1).ToString.PadLeft(5))
        Else
            WriteFile.Write("0".PadLeft(5))
        End If
        WriteFile.Write("# Number of Grid Output Points")
        WriteFile.WriteLine()

        '***WRITE GRID OUTPUT POINTS
        WriteFile.WriteLine("---------#------#------#")
        WriteFile.WriteLine("Output   #Grid  #Land  #")
        WriteFile.WriteLine("Directory#Number#Class #")
        WriteFile.WriteLine("---------#------#------#")
        If ParaMESH.IsEnabled("NoOutput", ParaMESH.SystemParameters, 1) = False Then
            For i = 0 To ParaMESH.GridOutput.GetUpperBound(2)
                WriteFile.Write(ParaWRITE.FormatString(ParaMESH.GridOutput(BakFlag, 2, i), ParaMESH.SecondaryName(2, "GridOutput")).PadRight(10))
                WriteFile.Write(ParaWRITE.FormatString(ParaMESH.GridOutput(BakFlag, 0, i), ParaMESH.SecondaryName(0, "GridOutput")).PadLeft(7))
                WriteFile.Write(ParaWRITE.FormatString(ParaMESH.GridOutput(BakFlag, 1, i), ParaMESH.SecondaryName(1, "GridOutput")).PadLeft(7))
                WriteFile.WriteLine()
            Next
        End If

        '***CLOSE FileName
        WriteFile.Close()
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaWRITE.PadClassINI
    '*****************************************************************************
    'Pad the class.ini configuration file for any environment (MESH, CLASS, 
    'WATFLOW).  The environment is recognised by FileName.
    '*****************************************************************************

    Private Shared Sub PadClassINI(ByVal FileName As String)

        '***READ FileName INTO PadArray
        Dim ReadFile As New System.IO.StreamReader(ParaMESH.PathRoot & "\" & FileName)
        ReDim PadArray(0)
        Do Until ReadFile.Peek = -1
            PadArray(PadArray.GetUpperBound(0)) = ReadFile.ReadLine
            ReDim Preserve PadArray(PadArray.GetUpperBound(0) + 1)
        Loop
        ReDim Preserve PadArray(PadArray.GetUpperBound(0) - 1)
        ReadFile.Close()

        '***OPEN FileName TO RE-WRITE PADDED FILE
        Dim WriteFile As New System.IO.StreamWriter(ParaMESH.PathRoot & "\" & FileName)
        Select Case FileName.ToLower
            Case Is = "mesh_parameters_class.ini"

                '***PAD AND WRITE FILE
                WriteFile.WriteLine(PadArray(0).PadRight(72) & "  01")
                WriteFile.WriteLine(PadArray(1).PadRight(72) & "  02")
                WriteFile.WriteLine(PadArray(2).PadRight(72) & "  03")
                WriteFile.WriteLine(PadArray(3).PadRight(72) & "  04")
                For i = 0 To Convert.ToInt32(PadArray(3).Substring(67, 5).Trim) - 1
                    WriteFile.WriteLine(PadArray(4 + (15 * i)).PadRight(72) & "  05" & ParaMESH.GRU(0, i).ToString.Trim & ParaWRITE.PadName("FCAN") & ParaWRITE.PadName("LAMX"))
                    WriteFile.WriteLine(PadArray(5 + (15 * i)).PadRight(72) & "  06" & ParaWRITE.PadName("LNZ0", True) & ParaWRITE.PadName("LAMN"))
                    WriteFile.WriteLine(PadArray(6 + (15 * i)).PadRight(72) & "  07" & ParaWRITE.PadName("ALVC", True) & ParaWRITE.PadName("CMAS"))
                    WriteFile.WriteLine(PadArray(7 + (15 * i)).PadRight(72) & "  08" & ParaWRITE.PadName("ALIC", True) & ParaWRITE.PadName("ROOT"))
                    WriteFile.WriteLine(PadArray(8 + (15 * i)).PadRight(72).Substring(0, 32).PadRight(40) & PadArray(8 + (15 * i)).Substring(40, 32) & "  09" & ParaWRITE.PadName("RSMN", True) & ParaWRITE.PadName("RSERVC01") & ParaWRITE.PadName("QA50"))
                    WriteFile.WriteLine(PadArray(9 + (15 * i)).PadRight(72).Substring(0, 32).PadRight(40) & PadArray(9 + (15 * i)).Substring(40, 32) & "  10" & ParaWRITE.PadName("VPDA", True) & ParaWRITE.PadName("RSERVC02") & ParaWRITE.PadName("VPDB"))
                    WriteFile.WriteLine(PadArray(10 + (15 * i)).PadRight(72).Substring(0, 32).PadRight(40) & PadArray(10 + (15 * i)).Substring(40, 32) & "  11" & ParaWRITE.PadName("PSGA", True) & ParaWRITE.PadName("RSERVC03") & ParaWRITE.PadName("PSGB"))
                    WriteFile.WriteLine(PadArray(11 + (15 * i)).PadRight(72).Substring(0, 32).PadRight(72) & "  12" & ParaWRITE.PadName("DRN", True) & ParaWRITE.PadName("SDEP") & ParaWRITE.PadName("FARE") & ParaWRITE.PadName("DDEN") & ParaWRITE.PadName("RSERVC04") & ParaWRITE.PadName("RSERVC05") & ParaWRITE.PadName("RSERVC06") & ParaWRITE.PadName("RSERVC07") & ParaWRITE.PadName("RSERVC08"))
                    WriteFile.WriteLine(PadArray(12 + (15 * i)).PadRight(72).Substring(0, 40).PadRight(72) & "  13" & ParaWRITE.PadName("XSLP", True) & ParaWRITE.PadName("GRKF") & ParaWRITE.PadName("WFSF") & ParaWRITE.PadName("WFCI") & "/GRU" & ParaWRITE.PadName("RSERVC09") & ParaWRITE.PadName("RSERVC10") & ParaWRITE.PadName("RSERVC11") & ParaWRITE.PadName("RSERVC12"))
                    WriteFile.WriteLine(PadArray(13 + (15 * i)).PadRight(72).Substring(0, 30).PadRight(72) & "  14" & ParaWRITE.PadName("SAND", True) & ParaWRITE.PadName("RSERVC13") & ParaWRITE.PadName("RSERVC14") & ParaWRITE.PadName("RSERVC15") & ParaWRITE.PadName("RSERVC16"))
                    WriteFile.WriteLine(PadArray(14 + (15 * i)).PadRight(72).Substring(0, 30).PadRight(72) & "  15" & ParaWRITE.PadName("CLAY", True) & ParaWRITE.PadName("RSERVC17") & ParaWRITE.PadName("RSERVC18") & ParaWRITE.PadName("RSERVC19") & ParaWRITE.PadName("RSERVC20"))
                    WriteFile.WriteLine(PadArray(15 + (15 * i)).PadRight(72).Substring(0, 30).PadRight(72) & "  16" & ParaWRITE.PadName("ORGM", True) & ParaWRITE.PadName("RSERVC21") & ParaWRITE.PadName("RSERVC22") & ParaWRITE.PadName("RSERVC23") & ParaWRITE.PadName("RSERVC24"))
                    WriteFile.WriteLine(PadArray(16 + (15 * i)).PadRight(72).Substring(0, 60).PadRight(72) & "  17" & ParaWRITE.PadName("TBAR", True) & ParaWRITE.PadName("TCAN") & ParaWRITE.PadName("TSNO") & ParaWRITE.PadName("TPND") & ParaWRITE.PadName("RSERVC25"))
                    WriteFile.WriteLine(PadArray(17 + (15 * i)).PadRight(72) & "  18" & ParaWRITE.PadName("THLQ", True) & ParaWRITE.PadName("THIC") & ParaWRITE.PadName("ZPND"))
                    WriteFile.WriteLine(PadArray(18 + (15 * i)).PadRight(72).Substring(0, 60).PadRight(72) & "  19" & ParaWRITE.PadName("RCAN", True) & ParaWRITE.PadName("SCAN") & ParaWRITE.PadName("SNO") & ParaWRITE.PadName("ALBS") & ParaWRITE.PadName("RHOS") & ParaWRITE.PadName("GRO") & ParaWRITE.PadName("RSERVC26"))
                Next
                WriteFile.WriteLine(PadArray(19 + (15 * (i - 1))).PadRight(72).Substring(0, 60).PadRight(72) & "  20")
                WriteFile.WriteLine(PadArray(20 + (15 * (i - 1))).PadRight(72).Substring(0, 60).PadRight(72) & "  21")
                WriteFile.WriteLine(PadArray(21 + (15 * (i - 1))).PadRight(72).Substring(0, 40).PadRight(72) & "  22")
                WriteFile.WriteLine("123456789012345678901234567890123456789012345678901234567890123456789012")
            Case Else

                '***WRITE UN-PADDED FILE
                For i = 0 To PadArray.GetUpperBound(0) - 1
                    WriteFile.WriteLine(PadArray(i))
                Next
        End Select

        '***CLOSE FileName
        WriteFile.Close()
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaWRITE.ParaSTOR
    '*****************************************************************************
    'Writes the ParaSTOR error report to FullPath, which includes the file's name 
    'and full directory path.
    '*****************************************************************************

    Public Shared Sub ParaSTOR(ByVal FullPath As String)

        '***OPEN ParaSTOR FILE
        Dim WriteFile As New System.IO.StreamWriter(FullPath)
        With WriteFile
            For i = 1 To 72
                .Write("=")
            Next
            .WriteLine()

            '***WRITE HEADER INFORMATION
            .WriteLine("ParaSTOR Error Report, " & System.DateTime.Now)
            .WriteLine()
            TextOutArray = ParaWRITE.FormatError(ErrorForm.Box_Message.Text, 72)
            For i = 0 To TextOutArray.GetUpperBound(0)
                .WriteLine(TextOutArray(i))
            Next
            For i = 1 To 72
                .Write("=")
            Next
            .WriteLine()
            .WriteLine()
            For i = 1 To 72
                .Write("=")
            Next
            .WriteLine()

            '***WRITE SYSTEM INFORMATION
            .WriteLine("System Information")
            .WriteLine()
            .WriteLine("Administrator: " & ErrorForm.Box_Administrator.Text & ", " & ErrorForm.Box_AdminContact.Text)
            .WriteLine("Release: " & ErrorForm.Box_SystemRelease.Text)
            .WriteLine("Hierarchy: " & ErrorForm.Box_SystemHierarchy.Text)
            For i = 1 To 72
                .Write("=")
            Next
            .WriteLine()
            .WriteLine()
            For i = 1 To 72
                .Write("=")
            Next
            .WriteLine()

            '***WRITE ERROR INFORMATION, AND MESSAGE
            .WriteLine("Error Information, ParaLOG")
            For i = 1 To 72
                .Write("=")
            Next
            .WriteLine()
            .WriteLine()
            .WriteLine("#Time: " & ErrorForm.Box_ErrorTime.Text)
            .WriteLine()
            TextOutArray = ParaWRITE.FormatError("#" & ErrorForm.Box_ErrorProcess.Text, 72)
            For i = 0 To TextOutArray.GetUpperBound(0)
                .WriteLine(TextOutArray(i))
            Next

            '***WRITE END OF ERROR MESSAGE
            .WriteLine("#End")
            .WriteLine()
            For i = 1 To 72
                .Write("=")
            Next
            .WriteLine()

            '***WRITE END OF FILE
            .WriteLine("End of ParaSTOR Report")
            .WriteLine()
            TextOutArray = ParaWRITE.FormatError("ParaMESH Developed By: D. Princz, For: R. Soulis, University of Waterloo (Waterloo, ON), For: B. Davison, EC (NHRC)", 72)
            For i = 0 To TextOutArray.GetUpperBound(0)
                .WriteLine(TextOutArray(i))
            Next
            For i = 1 To 72
                .Write("=")
            Next
            .WriteLine()

            '***CLOSE ParaSTOR FILE
            .Close()
        End With
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaWRITE.BAK
    '*****************************************************************************
    'UPDATE: MAR 22 2008 (DAN)  removed from ParaSTOR, put into ParaWRITE; 
    '                           renamed "BAK" from "ParaSTOR.Read"
    'UPDATE: FEB 08 2008 (DAN)  placed back into ParaWRITE, renamed BakFile
    'UPDATE: JAN 17 2008 (DAN)  integrated into ParaSTOR (from ParaWRITE)
    'UPDATE: JAN 09 2008 (DAN)  for ParaLOG approach
    '*****************************************************************************
    'Process for writing ParaMESH.bak restoration file (on critical system 
    'error).
    '*****************************************************************************

    Public Shared Sub BAK()

        '***PREVENT ParaMESH FROM LOGGING ANY FUTURE PROCESSES
        ParaSAVE.SystemParameters("Enabled", "StopLOG")

        '***SAVE CONFIGURATION FILE DATA IF CONFIGURATION FILES HAVE BEEN LOADED
        If ParaMESH.PathRoot <> Nothing Then
            Dim WriteFile As New System.IO.StreamWriter(Application.StartupPath & "\ParaMESH.bak")

            '***WRITE HEADER INFORMATION AND ParaMESH(PathRoot)
            WriteFile.WriteLine("ParaMESH Recovery Source File;" & System.DateTime.Now)
            WriteFile.WriteLine("PathRoot;" & ParaMESH.PathRoot)
            WriteFile.WriteLine("SystemRelease;" & ParaMESH.SystemProperties(1, ParaMESH.FindIndex("SystemRelease", ParaMESH.SystemProperties)))
            WriteFile.Close()

            '***RESET ParaMESH(PathRoot) TO APPLICATION'S STARTUP PATH
            ParaMESH.PathRoot = Application.StartupPath

            '***CALL ParaWRITE SUBROUTINES TO WRITE CONFIGURATION FILES
            ParaWRITE.MESH_ClassINI("class.txt")
            ParaWRITE.MESH_HydrologyINI("hydrology.txt")
            ParaWRITE.MESH_RunOptions("flags.txt")
        End If

        '***IF ..SystemParameters(ParaLOG) IS DISABLED THEN SHOW ErrorForm
        '***ELSE WRITE ERROR LOG
        If ParaMESH.IsEnabled("ParaLOG", ParaMESH.SystemParameters, 1) = False Then

            '***SHOW ParaSTOR ERROR REPORT
            ErrorForm.ShowDialog()
        Else

            '***CALL ParaLOG(WriteFile) TO WRITE PROCESS LOG
            ParaLOG.WriteFile()
        End If

        '***A NEW INSTANCE OF THE APPLICATION IS PURPOSELY STARTED BEFORE THE ParaMESH.ini FILE IS REWRITTEN
        '***RESTART ParaMESH
        Application.Restart()
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaWRITE.Configuration
    '*****************************************************************************
    'UPDATED: APR 22 2008  to use common ParaMESH subroutines.
    '*****************************************************************************
    'Writes the ParaMESH configuration file.
    '*****************************************************************************

    Public Shared Sub Configuration()

        '***CALL ParaSAVE(DirectoryList) TO SAVE DIRECTORY LIST
        ParaSAVE.DirectoryList()

        '***OPEN ParaMESH CONFIGURATION FILE
        Dim WriteFile As New System.IO.StreamWriter(Application.StartupPath & "\ParaMESH.ini")

        '***WRITE SYSTEM PROPERTIES ARRAY
        WriteFile.WriteLine("#SystemProperties[System Properties],RowCount[" & (ParaMESH.SystemProperties.GetUpperBound(1) + 1).ToString & "]")
        For i = 0 To ParaMESH.SystemProperties.GetUpperBound(1)
            For j = 0 To ParaMESH.SystemProperties.GetUpperBound(0) - 1
                WriteFile.Write(ParaMESH.SystemProperties(j, i) & ",")
            Next
            WriteFile.WriteLine(ParaMESH.SystemProperties(ParaMESH.SystemProperties.GetUpperBound(0), i))
        Next

        '***WRITE FILE PROPERTIES ARRAY
        WriteFile.WriteLine("#FileProperties[File Properties],RowCount[" & (ParaMESH.FileProperties.GetUpperBound(1) + 1).ToString & "]")
        For i = 0 To ParaMESH.FileProperties.GetUpperBound(1)
            For j = 0 To ParaMESH.FileProperties.GetUpperBound(0) - 1
                WriteFile.Write(ParaMESH.FileProperties(j, i) & ",")
            Next
            WriteFile.WriteLine(ParaMESH.FileProperties(ParaMESH.FileProperties.GetUpperBound(0), i))
        Next

        '***WRITE PRIMARY PARAMETERS ARRAY
        WriteFile.WriteLine("#PrimaryParameters[Primary Parameters],RowCount[" & (ParaMESH.PrimaryParameters.GetUpperBound(1) + 1).ToString & "]")
        For i = 0 To ParaMESH.PrimaryParameters.GetUpperBound(1)
            For j = 0 To ParaMESH.PrimaryParameters.GetUpperBound(0) - 1
                WriteFile.Write(ParaMESH.PrimaryParameters(j, i) & ",")
            Next
            WriteFile.WriteLine(ParaMESH.PrimaryParameters(ParaMESH.PrimaryParameters.GetUpperBound(0), i))
        Next

        '***WRITE SECONDARY PARAMETERS ARRAY
        WriteFile.WriteLine("#SecondaryParameters[Secondary Parameters],RowCount[" & (ParaMESH.SecondaryParameters.GetUpperBound(1) + 1).ToString & "]")
        For i = 0 To ParaMESH.SecondaryParameters.GetUpperBound(1)
            For j = 0 To ParaMESH.SecondaryParameters.GetUpperBound(0) - 1
                WriteFile.Write(ParaMESH.SecondaryParameters(j, i) & ",")
            Next
            WriteFile.WriteLine(ParaMESH.SecondaryParameters(ParaMESH.SecondaryParameters.GetUpperBound(0), i))
        Next

        '***WRITE SYSTEM PARAMETERS ARRAY
        WriteFile.WriteLine("#SystemParameters[System Settings],RowCount[" & (ParaMESH.SystemParameters.GetUpperBound(1) + 1).ToString & "]")
        For i = 0 To ParaMESH.SystemParameters.GetUpperBound(1)
            For j = 0 To ParaMESH.SystemParameters.GetUpperBound(0) - 1
                WriteFile.Write(ParaMESH.SystemParameters(j, i) & ",")
            Next
            WriteFile.WriteLine(ParaMESH.SystemParameters(ParaMESH.SystemParameters.GetUpperBound(0), i))
        Next

        '***WRITE SPECIAL CHECKS ARRAY
        WriteFile.WriteLine("#SpecialChecks[Special Parameter Checks],RowCount[" & (ParaMESH.SpecialChecks.GetUpperBound(1) + 1).ToString & "]")
        For i = 0 To ParaMESH.SpecialChecks.GetUpperBound(1)
            For j = 0 To ParaMESH.SpecialChecks.GetUpperBound(0) - 1
                WriteFile.Write(ParaMESH.SpecialChecks(j, i) & ",")
            Next
            WriteFile.WriteLine(ParaMESH.SpecialChecks(ParaMESH.SpecialChecks.GetUpperBound(0), i))
        Next

        '***WRITE CROSS FIELDS ARRAY
        WriteFile.WriteLine("#CrossFields[ParaSTOR Cross Fields],RowCount[" & (ParaMESH.CrossFields.GetUpperBound(1) + 1).ToString & "],ColumnCount[" & (ParaMESH.CrossFields.GetUpperBound(0) + 1).ToString & "]")
        For i = 0 To ParaMESH.CrossFields.GetUpperBound(1)
            For j = 0 To ParaMESH.CrossFields.GetUpperBound(0) - 1
                WriteFile.Write(ParaMESH.CrossFields(j, i) & ",")
            Next
            WriteFile.WriteLine(ParaMESH.CrossFields(ParaMESH.CrossFields.GetUpperBound(0), i))
        Next

        '***WRITE ERROR DEFINITIONS ARRAY
        WriteFile.WriteLine("#ErrorDefinitions[Error Message Definitions],RowCount[" & (ParaMESH.ErrorDefinitions.GetUpperBound(1) + 1).ToString & "]")
        For i = 0 To ParaMESH.ErrorDefinitions.GetUpperBound(1)
            For j = 0 To ParaMESH.ErrorDefinitions.GetUpperBound(0) - 1
                WriteFile.Write(ParaMESH.ErrorDefinitions(j, i) & ",")
            Next
            WriteFile.WriteLine(ParaMESH.ErrorDefinitions(ParaMESH.ErrorDefinitions.GetUpperBound(0), i))
        Next

        '***CLOSE ParaMESH CONFIGURATION FILE
        WriteFile.Close()

        '***CALL ParaREAD(Configuration) TO RE-READ THE DATA (REFRESH THE SYSTEM)
        ParaREAD.Configuration()
    End Sub

    '*****************************************************************************
    'FUNCTION: ParaWRITE.PadIndex
    '*****************************************************************************
    'UPDATED: APR 22 2008 (DAN)  to use common ParaMESH subroutines.
    '*****************************************************************************
    'Finds the alloweable string length to be printed for given VariableName in 
    'the ParaMESH parameters arrays (Column 6).
    '*****************************************************************************

    Private Shared Function PadIndex(ByVal VariableName As String) As Integer
        If ParaMESH.FindIndex(VariableName, ParaMESH.PrimaryParameters) < 0 Then
            PadIndex = Convert.ToInt32(ParaLOAD.SecondaryParameters(6, VariableName))
        Else
            PadIndex = Convert.ToInt32(ParaLOAD.PrimaryParameters(6, VariableName))
        End If
    End Function

    '*****************************************************************************
    'FUNCTION: ParaWRITE.PadName
    '*****************************************************************************
    'UPDATED: APR 22 2008 (DAN)  to use common ParaMESH subroutines.
    '*****************************************************************************
    'Finds the parameter name to label each line in the class.ini file.  If 
    'FirstName is true, the parameter name is written without a leading back 
    'slash.
    '*****************************************************************************

    Private Shared Function PadName(ByVal VariableName As String, Optional ByVal FirstName As Boolean = False) As String
        If FirstName = False And ParaMESH.IsEnabled(VariableName, ParaMESH.PrimaryParameters, 1) = True Then

            '***RETURN PARAMETER NAME WITH LEADING BACK-SLASH
            PadName = "/" & ParaLOAD.PrimaryParameters(3, VariableName)
            Exit Function
        End If

        '***RETURN PARAMETER NAME
        PadName = ParaLOAD.PrimaryParameters(3, VariableName)
    End Function

    '*****************************************************************************
    'FUNCTION: ParaWRITE.FormatString
    '*****************************************************************************
    'UPDATED: APR 22 2008 (DAN)  to use common ParaMESH subroutines.
    'UPDATED: JAN 18 2008 (DAN)  to handle disabled parameters.
    '*****************************************************************************
    'Formats the Variable as a Double, integer, or string value.  Accounts for 
    'larger Double numbers with exponents.  Returns formatted Variable string.
    '*****************************************************************************

    Private Shared Function FormatString(ByVal Variable As Object, ByVal VariableName As String) As String

        '***DETERMINE IF IS PRIMARY OR SECONDARY PARAMETER
        Dim ParameterArray(,) As String
        If ParaMESH.FindIndex(VariableName, ParaMESH.PrimaryParameters) < 0 Then
            ParameterArray = ParaMESH.SecondaryParameters
        Else
            ParameterArray = ParaMESH.PrimaryParameters
        End If

        '***SET FORMAT STRING TO NOTHING BY DEFAULT
        FormatString = ""

        '***DO NOTHING IF DISABLED PARAMETERS
        If ParaMESH.IsEnabled(VariableName, ParameterArray) = False Then
            Exit Function
        End If

        '***FORMAT STRING
        Select Case ParameterArray(9, ParaMESH.FindIndex(VariableName, ParameterArray)).ToLower
            Case Is = "double"
                Try
                    If Variable.ToString.Length > Convert.ToInt32(ParameterArray(6, ParaMESH.FindIndex(VariableName, ParameterArray))) + 1 Then
                        If Convert.ToInt32(ParameterArray(6, ParaMESH.FindIndex(VariableName, ParameterArray))) >= 10 Then
                            FormatString = Convert.ToDouble(Variable).ToString("#0.0###E+0")
                        ElseIf 10 > Convert.ToInt32(ParameterArray(6, ParaMESH.FindIndex(VariableName, ParameterArray))) >= 8 Then
                            FormatString = Convert.ToDouble(Variable).ToString("#0.0#E+0")
                        ElseIf 8 > Convert.ToInt32(ParameterArray(6, ParaMESH.FindIndex(VariableName, ParameterArray))) >= 7 Then
                            FormatString = Convert.ToDouble(Variable).ToString("#0.0E+0")
                        End If
                    Else
                        FormatString = Convert.ToDouble(Variable).ToString("##0.0###")
                    End If
                Catch ex As Exception
                    FormatString = "0"
                End Try
            Case Is = "integer"
                Try
                    FormatString = Convert.ToInt32(Variable)
                Catch ex As Exception
                    FormatString = "0"
                End Try
            Case Is = "string"
                Try
                    FormatString = Variable.ToString
                Catch ex As Exception
                    FormatString = ""
                End Try
        End Select
    End Function

    '*****************************************************************************
    'FUNCTION: ParaWRITE.FormatError
    '*****************************************************************************
    'Formats MessageString into an array, with each line limited to LineLength in 
    'lenght.  Returns TextOutArray.
    '*****************************************************************************

    Private Shared Function FormatError(ByVal MessageString As String, ByVal LineLength As Integer) As String()
        ReDim ParaWRITE.TextOutArray(0)

        '***SPLIT MESSAGE STRING INTO BLOCKS IN ParaWRITE(TextOutArray)
        Do Until MessageString.Length < LineLength
            If MessageString.Substring(0, LineLength).LastIndexOf(" ") < 0 Then
                ParaWRITE.TextOutArray(ParaWRITE.TextOutArray.GetUpperBound(0)) = MessageString.Substring(0, LineLength)
                MessageString = MessageString.Remove(0, LineLength)
            Else
                ParaWRITE.TextOutArray(ParaWRITE.TextOutArray.GetUpperBound(0)) = MessageString.Substring(0, MessageString.Substring(0, LineLength).LastIndexOf(" ") + 1)
                MessageString = MessageString.Remove(0, MessageString.Substring(0, LineLength).LastIndexOf(" ") + 1)
            End If
            ReDim Preserve ParaWRITE.TextOutArray(ParaWRITE.TextOutArray.GetUpperBound(0) + 1)
        Loop
        ParaWRITE.TextOutArray(ParaWRITE.TextOutArray.GetUpperBound(0)) = MessageString

        '***RETURN ParaWRITE(TextOutArray)
        FormatError = ParaWRITE.TextOutArray
    End Function
End Class
