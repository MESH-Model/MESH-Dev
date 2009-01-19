Public Class ParaCHECK

    '*****************************************************************************
    'CLASS: ParaCHECK
    '*****************************************************************************
    'ParaCHECK is used verify parameter values given range sets and definitions 
    'in the ParaMESH parameter arrays.  Special checks are also called which are 
    'used to check parameters whose definitions may be more complex than a simple 
    'range set definition.
    '*****************************************************************************

    '****************************************************************************
    'PRIVATE ParaCHECK PARAMETERS
    '****************************************************************************
    'Parameters shared and used only within the ParaCHECK class.
    '****************************************************************************
    'Private Integers: g, h, q are unique to the ParaCHECK class so that 
    'similarly private integers from calling subroutines are not reset.
    '****************************************************************************

    '***SHARED PRIVATE VARIABLES
    Private Shared ParameterArray(,) As String, RangeSet() As String, g As Integer, h As Integer, q As Integer

    '*****************************************************************************
    'SUBROUTINE: ParaCHECK.CheckRange
    '*****************************************************************************
    'UPDATE: MAR 22 2008 (DAN)  to use public calls (to lessen resource use)
    'UPDATE: FEB 06 2008 (DAN)  combined repetitive code (writing CheckList)
    'UPDATE: FEB 06 2008 (DAN)  AAUGH, restructuring; fix indexing (ColumnIndex..)
    'UPDATE: JAN 23 2008 (DAN)  to accept primary (ParaMESH.PrimaryParameters) 
    '                           and secondary (ParaMESH.SecondaryParameters) 
    '                           parameter arrays.
    'UPDATE: JAN 11 2008 (DAN)  for IgnoreCheck approach
    'UPDATE: JAN 11 2008 (DAN)  streamlining, validating process structure
    'UPDATE: JAN 10 2008 (DAN)  for ParaLOG approach
    '*****************************************************************************
    'Checks the value for any enabled parameter (ParaMESH.PrimaryParameters 
    'array, Column 2).
    'Checks that the value (Value) of the given parameter (ParameterCall) 
    'exists, and that a numerical value is valid (with no spaces and no more than 
    'one Double place).
    'Calls the range set (column 6) and range type (Column 7) in the 
    'ParaMESH.PrimaryParameters array to check that the given parameter 
    '(ParameterCall)'s value (Value) is valid.
    'Writes each range check to the ParaCHECK.CheckList array.
    'Calls the ParaCHECK.SpecialCheck subroutine.
    '*****************************************************************************

    Private Shared Sub CheckRange(ByVal Value As String, ByVal ParameterCall As String, Optional ByVal Index As Integer = -1, Optional ByVal ColumnIndex As Integer = -1, Optional ByVal GRU As Boolean = False)

        '***CHECK IF PARAMETER IS ON ParaCHECK IGNORE LIST (ParaMESH.IgnoreCheck)
        ParaLOG.AppendFile(ParameterCall & "# ..checking if the parameter is on the ParaMESH Parameter Check (ParaCHECK) ignore list", "ParaCHECK.CheckRange", "ValVar")
        If ParaMESH.FindIndex(ParameterCall, ParaMESH.IgnoreCheck) > -1 Then
            Exit Sub
        End If

        '***DETERMINE PARAMETER ARRAY TO USE
        ParaLOG.AppendFile(ParameterCall & "# ..finding the parameter in the parameter arrays", "ParaCHECK.CheckRange", "ValVar")
        If ParaMESH.FindIndex(ParameterCall, ParaMESH.PrimaryParameters) > -1 Then
            ParameterArray = ParaMESH.PrimaryParameters
        Else
            ParameterArray = ParaMESH.SecondaryParameters
        End If

        '***CHECK IF ParameterCall IS ENABLED
        If ParaMESH.IsEnabled(ParameterCall, ParameterArray) = False Then
            Exit Sub
        End If

        '***SET UP FIELD IN ParaMESH(CheckList) ARRAY
        ParaLOG.AppendFile(ParameterCall & "# (Index " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & "..setting up the ParaCHECK.CheckList array", "ParaCHECK.CheckRange", "ValVar")
        ParaMESH.CheckList(0, ParaMESH.CheckList.GetUpperBound(1)) = ParameterArray(4, ParaMESH.FindIndex(ParameterCall, ParameterArray))
        ParaMESH.CheckList(2, ParaMESH.CheckList.GetUpperBound(1)) = Value
        ParaMESH.CheckList(3, ParaMESH.CheckList.GetUpperBound(1)) = ParameterArray(5, ParaMESH.FindIndex(ParameterCall, ParameterArray))

        '***IF NO DEFAULT VALUE EXISTS, ENTER ZERO IF NOT STRING
        If ParaMESH.CheckList(3, ParaMESH.CheckList.GetUpperBound(1)) = Nothing And ParameterArray(9, ParaMESH.FindIndex(ParameterCall, ParameterArray)) <> "String" Then
            ParaMESH.CheckList(3, ParaMESH.CheckList.GetUpperBound(1)) = "0"
        End If
        ParaMESH.CheckList(4, ParaMESH.CheckList.GetUpperBound(1)) = ParameterCall
        ParaMESH.CheckList(5, ParaMESH.CheckList.GetUpperBound(1)) = Index
        ParaMESH.CheckList(6, ParaMESH.CheckList.GetUpperBound(1)) = ColumnIndex

        '***CHECK THAT THE VALUE EXISTS
        ParaLOG.AppendFile(ParameterCall & "# (Index " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ") ..checking if the value exists", "ParaCHECK.CheckRange", "ValVar")
        If Value = Nothing Then
            If ParameterArray Is ParaMESH.SecondaryParameters Then
                If ColumnIndex > -1 Then
                    ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = "The value (Line " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & " of " & ParaMESH.PrimaryParameters(3, ParaMESH.FindIndex(ParameterArray(0, ParaMESH.FindIndex(ParameterCall, ParameterArray)), ParaMESH.PrimaryParameters)) & ") does not exist."
                Else
                    ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = "The value (Line " & (Index + 1).ToString & " of " & ParaMESH.PrimaryParameters(3, ParaMESH.FindIndex(ParameterArray(0, ParaMESH.FindIndex(ParameterCall, ParameterArray)), ParaMESH.PrimaryParameters)) & ") does not exist."
                End If
            ElseIf Index > -1 And ColumnIndex = -1 Then
                ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = "The value (GRU " & (Index + 1).ToString & ") does not exist."
            ElseIf Index > -1 And ColumnIndex > -1 Then
                ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = "The value (GRU " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ") does not exist."
            ElseIf Index = -1 And ColumnIndex > -1 Then
                ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = "The value (Column " & (ColumnIndex + 1).ToString & ") does not exist."
            Else
                ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = "The value does not exist."
            End If

            '***CALL ParaCHECK(ErrorFix) TO SEE IF THERE IS A STANDARD FIX FOR THE ERROR
            ParaCHECK.QuickFix(Value, ParameterCall, ParameterArray, Index, ColumnIndex)
            ReDim Preserve ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), ParaMESH.CheckList.GetUpperBound(1) + 1)
            Exit Sub
        End If

        '***SET UP ERROR FIELD IN ParaMESH(CheckList)
        ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = "The value {" & Value & "}"
        If ParameterArray Is ParaMESH.SecondaryParameters Then
            If Index > -1 And ColumnIndex > -1 And GRU = True Then
                ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) &= " (GRU " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & " of " & ParameterArray(0, ParaMESH.FindIndex(ParameterCall, ParameterArray)) & ")"
            ElseIf Index > -1 And ColumnIndex > -1 And GRU = False Then
                ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) &= " (Line " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & " of " & ParaMESH.PrimaryParameters(3, ParaMESH.FindIndex(ParameterArray(0, ParaMESH.FindIndex(ParameterCall, ParameterArray)), ParaMESH.PrimaryParameters)) & ")"
            ElseIf Index > -1 And ColumnIndex = -1 And GRU = False Then
                ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) &= " (Line " & (Index + 1).ToString & " of " & ParaMESH.PrimaryParameters(3, ParaMESH.FindIndex(ParameterArray(0, ParaMESH.FindIndex(ParameterCall, ParameterArray)), ParaMESH.PrimaryParameters)) & ")"
            ElseIf Index > -1 And ColumnIndex = -1 And GRU = True Then
                ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) &= " (GRU " & (Index + 1).ToString & ")"
            End If
        ElseIf Index > -1 And ColumnIndex > -1 Then
            ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) &= " (GRU " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ")"
        ElseIf Index > -1 And ColumnIndex = -1 Then
            ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) &= " (GRU " & (Index + 1).ToString & ")"
        End If

        '***CHECKS IF VALUE IS EQUAL TO DEFAULT VALUE (ParaMESH ASSUMES DEFAULT VALUES ARE CORRECT)
        ParaLOG.AppendFile(ParameterCall & "# (Index " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ") ..checking default value", "ParaCHECK.CheckRange", "ValVar")
        If Value = ParameterArray(5, ParaMESH.FindIndex(ParameterCall, ParameterArray)) Then

            '***SPECIAL PARAMETER CHECKS
            ParaCHECK.SpecialChecks(Value, ParameterCall, ParameterArray, Index, ColumnIndex)

            '***RESET ParaCHECK.CheckList(1, 0) (ERROR MESSAGE) TO NOTHING
            '***ERRORNEOUS VALUES ARE IDENTIFIED BY THE POPULATED ERROR MESSAGE
            ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = Nothing
            Exit Sub
        End If

        '***CHECK STRING VALUES FOR [ANY GENERIC CHECK YET TO BE DETERMINED]
        ParaLOG.AppendFile(ParameterCall & "# (Index " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ") ..checking string values", "ParaCHECK.CheckRange", "ValVar")
        If ParameterArray(9, ParaMESH.FindIndex(ParameterCall, ParameterArray)) = "String" Then

            '***ANY CHECKS FOR STRING PARAMETERS SHOULD GO HERE
            '***CALL SPECIAL CHECK IF NECESSARY

            '***CHECKS THAT LENGTH OF String IS NOT GREATER THAN LENGTH OF FIELD
            If Value.Length > ParameterArray(6, ParaMESH.FindIndex(ParameterCall, ParameterArray)) Then
                ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) &= " is {" & (Value.Length).ToString & "} characters long.  The value must be less than or equal to {" & ParameterArray(6, ParaMESH.FindIndex(ParameterCall, ParameterArray)) & "} characters in length to be successfully written to the configuration file."
                ParaMESH.CheckList(3, ParaMESH.CheckList.GetUpperBound(1)) = Value.Substring(0, ParameterArray(6, ParaMESH.FindIndex(ParameterCall, ParameterArray)))
                ReDim Preserve ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), ParaMESH.CheckList.GetUpperBound(1) + 1)
                Exit Sub
            End If

            '***RESET ParaCHECK.CheckList(1, 0) (ERROR MESSAGE) TO NOTHING
            ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = Nothing
            Exit Sub
        End If

        '***VALUES WHICH ARE NOT VALID NUMBERS DO NOT CONTINUE BEYOND THIS POINT

        '***CHECK IS NUMERIC VALUE
        ParaLOG.AppendFile(ParameterCall & "# (Index " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ") ..checking if the value is a valid number", "ParaCHECK.CheckRange", "ValVar")
        Try
            Convert.ToDouble(Value)
        Catch ex As Exception
            If ParameterArray(9, ParaMESH.FindIndex(ParameterCall, ParameterArray)) = "Double" Then
                ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) &= " is not a number.  Check that the value is a valid number."
            ElseIf ParameterArray(9, ParaMESH.FindIndex(ParameterCall, ParameterArray)) = "Integer" Then
                ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) &= " is not a number.  Check that the value is a valid whole number."
            End If
            ReDim Preserve ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), ParaMESH.CheckList.GetUpperBound(1) + 1)
            Exit Sub
        End Try

        '***CHECK THAT INTEGERS ARE WHOLE NUMBERS
        ParaLOG.AppendFile(ParameterCall & "# (Index " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ") ..checking if the value must be a whole number", "ParaCHECK.CheckRange", "ValVar")
        If Value.Contains(".") = True And ParameterArray(9, ParaMESH.FindIndex(ParameterCall, ParameterArray)) = "Integer" And Convert.ToInt32(Value.Substring(Value.IndexOf(".") + 1)) <> 0 Then
            ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) &= " must be a whole number."
            ReDim Preserve ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), ParaMESH.CheckList.GetUpperBound(1) + 1)
            Exit Sub
        End If

        '***START RANGE CHECKS
        ParaLOG.AppendFile(ParameterCall & "# (Index " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ") ..populating the range set", "ParaCHECK.CheckRange", "ValVar")
        RangeSet = ParameterArray(7, ParaMESH.FindIndex(ParameterCall, ParameterArray)).Split(":")

        '***VALUES WILL ONLY BE CHECKED FOR PARAMETERS WITH RANGE SETS
        If RangeSet(0) <> Nothing Then
            Select Case ParameterArray(8, ParaMESH.FindIndex(ParameterCall, ParameterArray))
                Case Is = "Continuous"

                    '***CONTINOUS CHECK (MUST BE WITHIN OR EQUAL TO BOUNDS OF RANGE)
                    ParaLOG.AppendFile(ParameterCall & "# (Index " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ") ..checking if the value {" & Value & "} is within the range set for Range(Continuous)", "ParaCHECK.CheckRange", "ValVar")
                    If Convert.ToDouble(Value) < Convert.ToDouble(RangeSet(0)) Or Convert.ToDouble(Value) > Convert.ToDouble(RangeSet(RangeSet.GetUpperBound(0))) Then
                        ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) &= " must be greater than or equal to {" & RangeSet(0) & "}, and less than or equal to {" & RangeSet(RangeSet.GetUpperBound(0)) & "}."

                        '***CALL ParaCHECK.ErrorFix TO SEE IF THERE IS A STANDARD FIX FOR THE ERROR
                        ParaCHECK.QuickFix(Value, ParameterCall, ParameterArray, Index, ColumnIndex)
                        ReDim Preserve ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), ParaMESH.CheckList.GetUpperBound(1) + 1)
                        Exit Sub
                    End If
                Case Is = "Discontinuous"

                    '***DISCONTINOUS CHECK (MAY ONLY BE EQUAL TO ANY VALUE IN THE RANGE SET)
                    ParaLOG.AppendFile(ParameterCall & "# (Index " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ") ..checking if the value {" & Value & "} is within the range set for Range(Discontinuous)", "ParaCHECK.CheckRange", "ValVar")
                    For g = 0 To RangeSet.GetUpperBound(0)
                        If Convert.ToDouble(Value) = Convert.ToDouble(RangeSet(g)) Then
                            Exit Select
                        End If
                    Next
                    ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) &= " must equal"
                    For g = 0 To RangeSet.GetUpperBound(0)
                        If g > 0 And g < RangeSet.GetUpperBound(0) Then
                            ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) &= ","
                        ElseIf g > 0 And g = RangeSet.GetUpperBound(0) Then
                            ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) &= " or"
                        End If
                        ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) &= " {" & RangeSet(g) & "}"
                        If g = RangeSet.GetUpperBound(0) Then
                            ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) &= "."
                        End If
                    Next

                    '***CALL ParaCHECK.ErrorFix TO SEE IF THERE IS A STANDARD FIX FOR THE ERROR
                    ParaCHECK.QuickFix(Value, ParameterCall, ParameterArray, Index, ColumnIndex)
                    ReDim Preserve ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), ParaMESH.CheckList.GetUpperBound(1) + 1)
                    Exit Sub
                Case Is = "Less Than Or Equal To"

                    '***LESS THAN OR EQUAL TO (MUST BE LESS THAN OR EQUAL TO FIRST VALUE OF RANGE SET)
                    ParaLOG.AppendFile(ParameterCall & "# (Index " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ") ..checking if the value {" & Value & "} is within the range set for Range(Less Than Or Equal To)", "ParaCHECK.CheckRange", "ValVar")
                    If Convert.ToDouble(Value) > Convert.ToDouble(RangeSet(0)) Then
                        ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) &= " must be less than or equal to {" & RangeSet(0) & "}."

                        '***CALL ParaCHECK.ErrorFix TO SEE IF THERE IS A STANDARD FIX FOR THE ERROR
                        ParaCHECK.QuickFix(Value, ParameterCall, ParameterArray, Index, ColumnIndex)
                        ReDim Preserve ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), ParaMESH.CheckList.GetUpperBound(1) + 1)
                        Exit Sub
                    End If
                Case Is = "Greater Than Or Equal To"

                    '***GREATER THAN OR EQUAL TO (MUST BE GREATER THAN OR EQUAL TO FIRST VALUE OF RANGE SET)
                    ParaLOG.AppendFile(ParameterCall & "# (Index " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ") ..checking if the value {" & Value & "} is within the range set for Range(Greater Than Or Equal To)", "ParaCHECK.CheckRange", "ValVar")
                    If Convert.ToDouble(Value) < Convert.ToDouble(RangeSet(0)) Then
                        ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) &= " must be greater than or equal to {" & RangeSet(0) & "}."

                        '***CALL ParaCHECK.ErrorFix TO SEE IF THERE IS A STANDARD FIX FOR THE ERROR
                        ParaCHECK.QuickFix(Value, ParameterCall, ParameterArray, Index, ColumnIndex)
                        ReDim Preserve ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), ParaMESH.CheckList.GetUpperBound(1) + 1)
                        Exit Sub
                    End If
            End Select
        End If

        '***RESET FIELD 1 OF ParaMESH(CheckList) (ERROR MESSAGE) TO NOTHING
        '***ERRORNEOUS VALUES ARE IDENTIFIED BY THE POPULATED ERROR MESSAGE
        ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = Nothing

        '***SPECIAL PARAMETER CHECKS
        ParaCHECK.SpecialChecks(Value, ParameterCall, ParameterArray, Index, ColumnIndex)
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaCHECK.ErrorFix
    '*****************************************************************************
    'Checks a parameter array for special conditions.
    'Contains a collection of special checks which cannot be represented by 
    'a generic range type (Column 7) in the ParaMESH.PrimaryParameters array.
    '*****************************************************************************

    Public Shared Sub QuickFix(ByVal Value As String, ByVal ParameterCall As String, ByVal ParameterArray(,) As String, ByVal Index As Integer, ByVal ColumnIndex As Integer)

        '***REVERT ParameterCall TO PARENT IF IS SECONDARY PARAMETER
        If ParameterArray Is ParaMESH.SecondaryParameters Then
            ParameterCall = ParaMESH.SecondaryParameters(0, ParaMESH.FindIndex(ParameterCall, ParaMESH.SecondaryParameters))
        End If

        '***CHECK IF SPECIAL CHECK IS ENABLED
        If ParaMESH.IsEnabled(ParameterCall, ParaMESH.SpecialChecks, 1) = False Then
            Exit Sub
        End If
        Select Case ParameterCall
            Case Is = "MetStartHour"

                '***CHECKS IF ParaMESH(MetStartHour) IS {24}, CHANGES ParaMESH(MetStartHour) TO {0}
                ParaLOG.AppendFile(ParameterCall & "# ..special condition: finding parameter in the parameter checklist", "ParaCHECK.QuickFix", "ValVar")
                If ParaMESH.CheckList(2, ParaMESH.CheckList.GetUpperBound(1)) = "24" Then
                    ParaLOG.AppendFile(ParameterCall & "# ..special condition: set an erroneous value of {24} to {0}", "ParaCHECK.QuickFix", "ValVar")
                    ParaMESH.CheckList(3, ParaMESH.CheckList.GetUpperBound(1)) = "0"
                End If
            Case Is = "GRU"

                '***CHECKS IF ParaMESH(GRU) EXISTS, CHANGES TO DEFAULT ParaMESH(GRU) WITH GRU NUMBER
                ParaLOG.AppendFile(ParameterCall & "# ..special condition: finding parameter in the parameter checklist", "ParaCHECK.QuickFix", "ValVar")
                If ParaMESH.CheckList(2, ParaMESH.CheckList.GetUpperBound(1)) = Nothing Then
                    ParaLOG.AppendFile(ParameterCall & "# ..special condition: set description to include GRU number", "ParaCHECK.QuickFix", "ValVar")
                    ParaMESH.CheckList(3, ParaMESH.CheckList.GetUpperBound(1)) = ParaLOAD.PrimaryParameters(5, "GRU") & " " & (Index + 1).ToString
                End If
            Case Is = "GridOutput"

                '***IGNORES ParaMESH(OutputGrid) ERRORS IF ..SystemParameters(NoOutput) IS ENABLED
                ParaLOG.AppendFile(ParameterCall & "# ..special condition: ignore does not exist error if ..SystemParameters(NoOutput) is enabled", "ParaCHECK.SpecialChecks", "ValVar")
                If ParaMESH.IsEnabled("NoOutput", ParaMESH.SystemParameters, 1) = True Then

                    '***CHECK IF GridOutput EXISTS IN ParaMESH(CheckList)
                    For g = 0 To ParaMESH.CheckList.GetUpperBound(1)
                        If ParaMESH.CheckList(1, g) <> Nothing And ParaMESH.CheckList(4, g) <> Nothing Then
                            If ParaMESH.CheckList(1, g).Contains("does not exist.") = True And ParaMESH.PrimaryName(ParaMESH.CheckList(4, g)) = ParameterCall Then

                                '***RESET ERROR DESCRIPTION SO THAT FIELD IS REMOVED BY ParaCHECK(TrimList)
                                ParaMESH.CheckList(1, g) = Nothing
                            End If
                        End If
                    Next
                End If
        End Select
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaCHECK.SpecialChecks
    '*****************************************************************************
    'UPDATE: MAR 26 2008 (DAN)  modified to save parameter value, may be checking 
    '                           the unsaved parameter value otherwise
    'UPDATE: MAR    2008 (DAN)  checks ParaMESH(SpecialChecks) if check is 
    '                           enabled (is no longer hard-coded)
    'UPDATE: FEB 06 2008 (DAN)  split to handle "SpecialChecks" (for parameters 
    '                           which have passed the generic parameter check) 
    '                           and "ErrorFix" (for conditions which may fix the 
    '                           erroneous parameter value)
    'UPDATE: JAN 28 2008 (DAN)  disabled checks for THLQ and THIC
    'UPDATE: JAN 18 2008 (DAN)  handle soil layers separately (LAMN, THLQ, THIC)
    'UPDATE: JAN 14 2008 (DAN)  to accept primary (ParaMESH.PrimaryParameters) and 
    '                           secondary (ParaMESH.SecondaryParameters) 
    '                           parameter arrays
    'UPDATE: JAN 14 2008 (DAN)  error with ParaMESH.PrimaryParameters array 
    '                           referencing fixed; calling 3rd instead of 4th 
    '                           Field for default parameter values
    'UPDATE: JAN 11 2008 (DAN)  added case for ParaMESH.MetStartHour
    'UPDATE: JAN 11 2008 (DAN)  streamlining, validating process structure
    'UPDATE: JAN 10 2008 (DAN)  for ParaLOG approach
    '*****************************************************************************
    'Checks specific parameters for special conditions.
    'Contains a collection of special checks which cannot be represented by 
    'a generic range type (Column 7) in the ParaMESH.PrimaryParameters array.
    'Checks if any other parameters, on which the special check will depend, have 
    'been flagged in the parameter check list (ParaCHECK.CheckList); performing 
    'the check with an erroneous value may cause a critical error, such as 
    'invalid conversion operations.
    '*****************************************************************************

    Private Shared Sub SpecialChecks(ByVal Value As String, ByVal ParameterCall As String, ByVal ParameterArray(,) As String, ByVal Index As Integer, ByVal ColumnIndex As Integer)

        '***REVERT ParameterCall TO PARENT IF IS SECONDARY PARAMETER
        If ParameterArray Is ParaMESH.SecondaryParameters Then
            ParameterCall = ParaMESH.SecondaryParameters(0, ParaMESH.FindIndex(ParameterCall, ParaMESH.SecondaryParameters))
        End If

        '***CHECK IF SPECIAL CHECK IS ENABLED
        If ParaMESH.IsEnabled(ParameterCall, ParaMESH.SpecialChecks, 1) = False Then
            Exit Sub
        End If

        '***SET UP FIELD IN ParaCHECK.CheckList ARRAY
        ParaLOG.AppendFile(ParameterCall & "# (Index " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & "..setting up the ParaCHECK.CheckList array", "ParaCHECK.CheckRange", "ValVar")
        ParaMESH.CheckList(0, ParaMESH.CheckList.GetUpperBound(1)) = ParameterArray(4, ParaMESH.FindIndex(ParameterCall, ParameterArray))
        ParaMESH.CheckList(4, ParaMESH.CheckList.GetUpperBound(1)) = ParameterCall
        ParaMESH.CheckList(5, ParaMESH.CheckList.GetUpperBound(1)) = Index
        ParaMESH.CheckList(6, ParaMESH.CheckList.GetUpperBound(1)) = ColumnIndex
        Select Case ParameterCall
            Case Is = "FCAN"

                '***CHECK ParaMESH(CheckList) FOR ERRONEOUS DEPENDENT PARAMETER VALUE
                ParaLOG.AppendFile(ParameterCall & "# (GRU " & (Index + 1).ToString & ") ..special condition: checking for erroneous dependents (FCAN) in ParaMESH(CheckList)", "ParaCHECK.SpecialCheck", "ValVar")
                For g = 0 To ParaMESH.CheckList.GetUpperBound(1) - 1
                    If ParaMESH.CheckList(1, g) <> Nothing And ParaMESH.CheckList(4, g) = ParameterCall And ParaMESH.CheckList(5, g) = Index Then
                        Exit Sub
                    End If
                Next

                '***CALL ParaSAVE(FCAN) TO SAVE VALUE
                ParaSAVE.FCAN(Value, ColumnIndex, Index)

                '***CHECKS THAT ALL COLUMNS OF ParaMESH.FCAN SUM TO 1
                ParaLOG.AppendFile(ParameterCall & "# (GRU " & (Index + 1).ToString & ") ..special condition: checking if values sum to {1}", "ParaCHECK.SpecialCheck", "ValVar")
                If ParaMESH.FCAN(0, 0, Index) * 100 + ParaMESH.FCAN(0, 1, Index) * 100 + ParaMESH.FCAN(0, 2, Index) * 100 + ParaMESH.FCAN(0, 3, Index) * 100 + ParaMESH.FCAN(0, 4, Index) * 100 <> 100 Then
                    For g = 0 To ParaMESH.FCAN.GetUpperBound(1)
                        ParaMESH.CheckList(0, ParaMESH.CheckList.GetUpperBound(1)) = ParameterArray(4, ParaMESH.FindIndex(ParameterCall, ParameterArray))
                        ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = "The sum of the " & ParameterArray(4, ParaMESH.FindIndex(ParameterCall, ParameterArray)) & " (" & ParameterCall & ")" & " values must equal {1} (GRU " & Index + 1 & ", Column " & g + 1 & ")."
                        ParaMESH.CheckList(2, ParaMESH.CheckList.GetUpperBound(1)) = ParaMESH.FCAN(0, g, Index)
                        ParaMESH.CheckList(3, ParaMESH.CheckList.GetUpperBound(1)) = ParaMESH.FCAN(0, g, Index)
                        ParaMESH.CheckList(4, ParaMESH.CheckList.GetUpperBound(1)) = ParameterCall
                        ParaMESH.CheckList(5, ParaMESH.CheckList.GetUpperBound(1)) = Index
                        ParaMESH.CheckList(6, ParaMESH.CheckList.GetUpperBound(1)) = g
                        ReDim Preserve ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), ParaMESH.CheckList.GetUpperBound(1) + 1)
                    Next
                    Exit Sub
                End If
            Case Is = "HourlyStartDay"

                '***CHECK ParaMESH(CheckList) FOR ERRONEOUS DEPENDENT PARAMETER VALUE
                ParaLOG.AppendFile(ParameterCall & "# ..special condition: checking for erroneous dependents (HourlyStartYear) in ParaMESH(CheckList)", "ParaCHECK.SpecialCheck", "ValVar")
                For g = 0 To ParaMESH.CheckList.GetUpperBound(1) - 1
                    If ParaMESH.CheckList(1, g) <> Nothing And ParaMESH.CheckList(4, g) = "HourlyStartYear" Then
                        Exit Select
                    End If
                Next

                '***CALL ParaSAVE(HourlyStartDay) TO SAVE VALUE
                ParaSAVE.HourlyStartDay(Value)

                '***CHECKS THAT ParaMESH(HourlyStartDay) IS < 366 IF ParaMESH(HourlyStartYear) IS A LEAP-YEAR
                ParaLOG.AppendFile(ParameterCall & "# ..special condition: checking if value {" & ParaMESH.HourlyStartDay(0) & "} accounts for leap-year", "ParaCHECK.SpecialCheck", "ValVar")
                If ParaMESH.HourlyStartYear(0) Mod 4 > 0 Or (ParaMESH.HourlyStartYear(0) Mod 100 = 0 And ParaMESH.HourlyStartYear(0) Mod 400 <> 0) Then
                    If ParaMESH.HourlyStartDay(0) < 1 Or ParaMESH.HourlyStartDay(0) > 365 Then
                        ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = "The value {" & ParaMESH.HourlyStartDay(0) & "} must be greater than or equal to {1} and less than or equal to {365} because {" & ParaMESH.HourlyStartYear(0) & "} is a leap-year."
                        ParaMESH.CheckList(2, ParaMESH.CheckList.GetUpperBound(1)) = ParaMESH.HourlyStartDay(0)
                        ParaMESH.CheckList(3, ParaMESH.CheckList.GetUpperBound(1)) = "365"
                        ReDim Preserve ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), ParaMESH.CheckList.GetUpperBound(1) + 1)
                        Exit Sub
                    End If
                End If
            Case Is = "HourlyStopDay"

                '***CHECK ParaMESH(CheckList) FOR ERRONEOUS DEPENDENT PARAMETER VALUE
                ParaLOG.AppendFile(ParameterCall & "# ..special condition: checking for erroneous dependents (HourlyStopYear) in ParaMESH(CheckList)", "ParaCHECK.SpecialCheck", "ValVar")
                For g = 0 To ParaMESH.CheckList.GetUpperBound(1) - 1
                    If ParaMESH.CheckList(1, g) <> Nothing And ParaMESH.CheckList(4, g) = "HourlyStopYear" Then
                        Exit Select
                    End If
                Next

                '***CALL ParaSAVE(HourlyStopDay) TO SAVE VALUE
                ParaSAVE.HourlyStopDay(Value)

                '***CHECKS THAT ParaMESH(HourlyStopDay) IS < 366 IF ParaMESH(HourlyStopYear) IS A LEAP-YEAR
                ParaLOG.AppendFile(ParameterCall & "# ..special condition: checking if value {" & ParaMESH.HourlyStopDay(0) & "} accounts for leap-year", "ParaCHECK.SpecialCheck", "ValVar")
                If ParaMESH.HourlyStopYear(0) Mod 4 > 0 Or (ParaMESH.HourlyStopYear(0) Mod 100 = 0 And ParaMESH.HourlyStopYear(0) Mod 400 <> 0) Then
                    If ParaMESH.HourlyStopDay(0) < 1 Or ParaMESH.HourlyStopDay(0) > 365 Then
                        ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = "The value {" & ParaMESH.HourlyStopDay(0) & "} must be greater than or equal to {1} and less than or equal to {365} because {" & ParaMESH.HourlyStopYear(0) & "} is a leap-year."
                        ParaMESH.CheckList(2, ParaMESH.CheckList.GetUpperBound(1)) = ParaMESH.HourlyStopDay(0)
                        ParaMESH.CheckList(3, ParaMESH.CheckList.GetUpperBound(1)) = "365"
                        ReDim Preserve ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), ParaMESH.CheckList.GetUpperBound(1) + 1)
                        Exit Sub
                    End If
                End If

            Case Is = "DailyStartDay"

                '***CHECK ParaMESH(CheckList) FOR ERRONEOUS DEPENDENT PARAMETER VALUE
                ParaLOG.AppendFile(ParameterCall & "# ..special condition: checking for erroneous dependents (DailyStartYear) in ParaMESH(CheckList)", "ParaCHECK.SpecialCheck", "ValVar")
                For g = 0 To ParaMESH.CheckList.GetUpperBound(1) - 1
                    If ParaMESH.CheckList(1, g) <> Nothing And ParaMESH.CheckList(4, g) = "DailyStartYear" Then
                        Exit Select
                    End If
                Next

                '***CALL ParaSAVE(DailyStartDay) TO SAVE VALUE
                ParaSAVE.DailyStartDay(Value)

                '***CHECKS THAT ParaMESH(DailyStartDay) IS < 366 IF ParaMESH(DailyStartYear) IS A LEAP-YEAR
                ParaLOG.AppendFile(ParameterCall & "# ..special condition: checking if value {" & ParaMESH.DailyStartDay(0) & "} accounts for leap-year", "ParaCHECK.SpecialCheck", "ValVar")
                If ParaMESH.DailyStartYear(0) Mod 4 > 0 Or (ParaMESH.DailyStartYear(0) Mod 100 = 0 And ParaMESH.DailyStartYear(0) Mod 400 <> 0) Then
                    If ParaMESH.DailyStartDay(0) < 1 Or ParaMESH.DailyStartDay(0) > 365 Then
                        ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = "The value {" & ParaMESH.DailyStartDay(0) & "} must be greater than or equal to {1} and less than or equal to {365} because {" & ParaMESH.DailyStartYear(0) & "} is a leap-year."
                        ParaMESH.CheckList(2, ParaMESH.CheckList.GetUpperBound(1)) = ParaMESH.DailyStartDay(0)
                        ParaMESH.CheckList(3, ParaMESH.CheckList.GetUpperBound(1)) = "365"
                        ReDim Preserve ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), ParaMESH.CheckList.GetUpperBound(1) + 1)
                        Exit Sub
                    End If
                End If
            Case Is = "DailyStopDay"

                '***CHECK ParaMESH(CheckList) FOR ERRONEOUS DEPENDENT PARAMETER VALUE
                ParaLOG.AppendFile(ParameterCall & "# ..special condition: checking for erroneous dependents (DailyStopYear) in ParaMESH(CheckList)", "ParaCHECK.SpecialCheck", "ValVar")
                For g = 0 To ParaMESH.CheckList.GetUpperBound(1) - 1
                    If ParaMESH.CheckList(1, g) <> Nothing And ParaMESH.CheckList(4, g) = "DailyStopYear" Then
                        Exit Select
                    End If
                Next

                '***CALL ParaSAVE(DailyStopDay) TO SAVE VALUE
                ParaSAVE.DailyStopDay(Value)

                '***CHECKS THAT ParaMESH(DailyStopDay) IS < 366 IF ParaMESH(DailyStopYear) IS A LEAP-YEAR
                ParaLOG.AppendFile(ParameterCall & "# ..special condition: checking if value {" & ParaMESH.DailyStopDay(0) & "} accounts for leap-year", "ParaCHECK.SpecialCheck", "ValVar")
                If ParaMESH.DailyStopYear(0) Mod 4 > 0 Or (ParaMESH.DailyStopYear(0) Mod 100 = 0 And ParaMESH.DailyStopYear(0) Mod 400 <> 0) Then
                    If ParaMESH.DailyStopDay(0) < 1 Or ParaMESH.DailyStopDay(0) > 365 Then
                        ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = "The value {" & ParaMESH.DailyStopDay(0) & "} must be greater than or equal to {1} and less than or equal to {365} because {" & ParaMESH.DailyStopYear(0) & "} is a leap-year."
                        ParaMESH.CheckList(2, ParaMESH.CheckList.GetUpperBound(1)) = ParaMESH.DailyStopDay(0)
                        ParaMESH.CheckList(3, ParaMESH.CheckList.GetUpperBound(1)) = "365"
                        ReDim Preserve ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), ParaMESH.CheckList.GetUpperBound(1) + 1)
                        Exit Sub
                    End If
                End If
            Case Is = "SimStartDay"

                '***CHECK ParaMESH(CheckList) FOR ERRONEOUS DEPENDENT PARAMETER VALUE
                ParaLOG.AppendFile(ParameterCall & "# ..special condition: checking for erroneous dependents (SimStartYear) in ParaMESH(CheckList)", "ParaCHECK.SpecialCheck", "ValVar")
                For g = 0 To ParaMESH.CheckList.GetUpperBound(1) - 1
                    If ParaMESH.CheckList(1, g) <> Nothing And ParaMESH.CheckList(4, g) = "SimStartYear" Then
                        Exit Select
                    End If
                Next

                '***CALL ParaSAVE(SimStartDay) TO SAVE VALUE
                ParaSAVE.SimStartDay(Value)

                '***CHECKS THAT ParaMESH(SimStartDay) IS < 366 IF ParaMESH(SimStartYear) IS A LEAP-YEAR
                ParaLOG.AppendFile(ParameterCall & "# ..special condition: checking if value {" & ParaMESH.SimStartDay(0) & "} accounts for leap-year", "ParaCHECK.SpecialCheck", "ValVar")
                If ParaMESH.SimStartYear(0) Mod 4 > 0 Or (ParaMESH.SimStartYear(0) Mod 100 = 0 And ParaMESH.SimStartYear(0) Mod 400 <> 0) Then
                    If ParaMESH.SimStartDay(0) < 1 Or ParaMESH.SimStartDay(0) > 365 Then
                        ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = "The value {" & ParaMESH.SimStartDay(0) & "} must be greater than or equal to {1} and less than or equal to {365} because {" & ParaMESH.SimStartYear(0) & "} is a leap-year."
                        ParaMESH.CheckList(2, ParaMESH.CheckList.GetUpperBound(1)) = ParaMESH.SimStartDay(0)
                        ParaMESH.CheckList(3, ParaMESH.CheckList.GetUpperBound(1)) = "365"
                        ReDim Preserve ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), ParaMESH.CheckList.GetUpperBound(1) + 1)
                        Exit Sub
                    End If
                End If
            Case Is = "SimStopDay"

                '***CHECK ParaMESH(CheckList) FOR ERRONEOUS DEPENDENT PARAMETER VALUE
                ParaLOG.AppendFile(ParameterCall & "# ..special condition: checking for erroneous dependents (SimStopYear) in ParaMESH(CheckList)", "ParaCHECK.SpecialCheck", "ValVar")
                For g = 0 To ParaMESH.CheckList.GetUpperBound(1) - 1
                    If ParaMESH.CheckList(1, g) <> Nothing And ParaMESH.CheckList(4, g) = "SimStopYear" Then
                        Exit Select
                    End If
                Next

                '***CALL ParaSAVE(SimStopDay) TO SAVE VALUE
                ParaSAVE.SimStopDay(Value)

                '***CHECKS THAT ParaMESH(SimStopDay) IS < 366 IF ParaMESH(SimStopYear) IS A LEAP-YEAR
                ParaLOG.AppendFile(ParameterCall & "# ..special condition: checking if value {" & ParaMESH.SimStopDay(0) & "} accounts for leap-year", "ParaCHECK.SpecialCheck", "ValVar")
                If ParaMESH.SimStopYear(0) Mod 4 > 0 Or (ParaMESH.SimStopYear(0) Mod 100 = 0 And ParaMESH.SimStopYear(0) Mod 400 <> 0) Then
                    If ParaMESH.SimStopDay(0) < 1 Or ParaMESH.SimStopDay(0) > 365 Then
                        ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = "The value {" & ParaMESH.SimStopDay(0) & "} must be greater than or equal to {1} and less than or equal to {365} because {" & ParaMESH.SimStopYear(0) & "} is a leap-year."
                        ParaMESH.CheckList(2, ParaMESH.CheckList.GetUpperBound(1)) = ParaMESH.SimStopDay(0)
                        ParaMESH.CheckList(3, ParaMESH.CheckList.GetUpperBound(1)) = "365"
                        ReDim Preserve ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), ParaMESH.CheckList.GetUpperBound(1) + 1)
                        Exit Sub
                    End If
                End If
            Case Is = "MetStartDay"

                '***CHECK ParaMESH(CheckList) FOR ERRONEOUS DEPENDENT PARAMETER VALUE
                ParaLOG.AppendFile(ParameterCall & "# ..special condition: checking for erroneous dependents (MetStartYear) in ParaMESH(CheckList)", "ParaCHECK.SpecialCheck", "ValVar")
                For g = 0 To ParaMESH.CheckList.GetUpperBound(1) - 1
                    If ParaMESH.CheckList(1, g) <> Nothing And ParaMESH.CheckList(4, g) = "MetStartYear" Then
                        Exit Select
                    End If
                Next

                '***CALL ParaSAVE (MetStartDay) TO SAVE VALUE
                ParaSAVE.MetStartDay(Value)

                '***CHECKS THAT ParaMESH(MetStartDay) IS < 366 IF ParaMESH(MetStartYear) IS A LEAP-YEAR
                ParaLOG.AppendFile(ParameterCall & "# ..special condition: checking if value {" & ParaMESH.MetStartDay(0) & "} accounts for leap-year", "ParaCHECK.SpecialCheck", "ValVar")
                If ParaMESH.MetStartYear(0) Mod 4 > 0 Or (ParaMESH.MetStartYear(0) Mod 100 = 0 And ParaMESH.MetStartYear(0) Mod 400 <> 0) Then
                    If ParaMESH.MetStartDay(0) < 1 Or ParaMESH.MetStartDay(0) > 365 Then
                        ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = "The value {" & ParaMESH.MetStartDay(0) & "} must be greater than or equal to {1} and less than or equal to {365} because {" & ParaMESH.MetStartYear(0) & "} is a leap-year."
                        ParaMESH.CheckList(2, ParaMESH.CheckList.GetUpperBound(1)) = ParaMESH.MetStartDay(0)
                        ParaMESH.CheckList(3, ParaMESH.CheckList.GetUpperBound(1)) = "365"
                        ReDim Preserve ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), ParaMESH.CheckList.GetUpperBound(1) + 1)
                        Exit Sub
                    End If
                End If
            Case Is = "MetStartYear"

                '***CHECK ParaMESH(CheckList) FOR ERRONEOUS DEPENDENT PARAMETER VALUE
                ParaLOG.AppendFile(ParameterCall & "# ..special condition: checking for erroneous dependents (SimStartYear) in ParaMESH(CheckList)", "ParaCHECK.SpecialCheck", "ValVar")
                For g = 0 To ParaMESH.CheckList.GetUpperBound(1) - 1
                    If ParaMESH.CheckList(1, g) <> Nothing And ParaMESH.CheckList(4, g) = "SimStartYear" Then
                        Exit Select
                    End If
                Next

                ParaLOG.AppendFile(ParameterCall & "# ..special condition: checking for erroneous dependents (SimStopYear) in ParaMESH(CheckList)", "ParaCHECK.SpecialCheck", "ValVar")
                For g = 0 To ParaMESH.CheckList.GetUpperBound(1) - 1
                    If ParaMESH.CheckList(1, g) <> Nothing And ParaMESH.CheckList(4, g) = "SimStopYear" Then
                        Exit Select
                    End If
                Next

                '***CALL ParaSAVE(MetStartYear) TO SAVE VALUE
                ParaSAVE.MetStartYear(Value)

                '***CHECKS THAT ParaMESH(MetStartYear) IS BETWEEN ParaMESH(SimStartYear) AND ParaMESH(SimStopYear)
                ParaLOG.AppendFile(ParameterCall & "# ..special condition: checking if value {" & ParaMESH.MetStartYear(0) & "} is within the simulation start year {" & ParaMESH.SimStartYear(0) & "} and simulation stop year {" & ParaMESH.SimStopYear(0) & "}", "ParaCHECK.SpecialCheck", "ValVar")
                If ParaMESH.MetStartYear(0) < ParaMESH.SimStartYear(0) Or ParaMESH.MetStartYear(0) > ParaMESH.SimStopYear(0) Then
                    ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = "The value {" & ParaMESH.MetStartYear(0) & "} must be greater than or equal to the simulation start date {" & ParaMESH.SimStartYear(0) & "} and less than or equal to simulation stop date {" & ParaMESH.SimStopYear(0) & "}."
                    ParaMESH.CheckList(2, ParaMESH.CheckList.GetUpperBound(1)) = ParaMESH.MetStartYear(0)
                    ParaMESH.CheckList(3, ParaMESH.CheckList.GetUpperBound(1)) = ParaMESH.SimStartYear(0)
                    ReDim Preserve ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), ParaMESH.CheckList.GetUpperBound(1) + 1)
                    Exit Sub
                End If
            Case Is = "LAMN"

                '***CHECK ParaMESH(CheckList) FOR ERRONEOUS DEPENDENT PARAMETER VALUE
                ParaLOG.AppendFile(ParameterCall & "# ..special condition: checking for erroneous dependents (LAMX) in ParaMESH(CheckList)", "ParaCHECK.SpecialCheck", "ValVar")
                For g = 0 To ParaMESH.CheckList.GetUpperBound(1) - 1
                    If ParaMESH.CheckList(1, g) <> Nothing And ParaMESH.CheckList(4, g) = "LAMX" And ParaMESH.CheckList(5, g) = Index And ParaMESH.CheckList(6, g) = ColumnIndex Then
                        Exit Select
                    End If
                Next

                '***CALL ParaSAVE(LAMN) TO SAVE VALUE
                ParaSAVE.LAMN(Value, ColumnIndex, Index)

                '***CHECKS THAT ParaMESH(LAMN) < ParaMESH(LAMX)
                ParaLOG.AppendFile(ParameterCall & "# (GRU " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ") ..special condition: checking if value is less than or equal to LAMX (GRU " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ") {" & ParaMESH.LAMX(0, ColumnIndex, Index) & "}, or is equal to {0}", "ParaCHECK.SpecialCheck", "ValVar")
                If ParaMESH.LAMN(0, ColumnIndex, Index) > ParaMESH.LAMX(0, ColumnIndex, Index) And ParaMESH.LAMX(0, ColumnIndex, Index) <> 0 Then
                    ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = "The value {" & ParaMESH.LAMN(0, ColumnIndex, Index) & "} (GRU " & Index + 1 & ", Column " & ColumnIndex + 1 & ") must be less than or equal to " & ParaMESH.PrimaryParameters(3, ParaMESH.FindIndex("LAMX", ParaMESH.PrimaryParameters)) & "(LAMX) {" & ParaMESH.LAMX(0, ColumnIndex, Index) & "}, or equal to {0}."
                    ParaMESH.CheckList(2, ParaMESH.CheckList.GetUpperBound(1)) = ParaMESH.LAMN(0, ColumnIndex, Index)
                    ParaMESH.CheckList(3, ParaMESH.CheckList.GetUpperBound(1)) = ParaMESH.LAMX(0, ColumnIndex, Index)
                    ReDim Preserve ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), ParaMESH.CheckList.GetUpperBound(1) + 1)
                    Exit Sub
                End If
            Case Is = "THLQ"

                '***CHECK ParaMESH(CheckList) FOR ERRONEOUS DEPENDENT PARAMETER VALUE
                ParaLOG.AppendFile(ParameterCall & "# ..special condition: checking for erroneous dependents (TBAR) in ParaMESH(CheckList)", "ParaCHECK.SpecialCheck", "ValVar")
                For g = 0 To ParaMESH.CheckList.GetUpperBound(1) - 1
                    If ParaMESH.CheckList(1, g) <> Nothing And ParaMESH.CheckList(4, g) = "TBAR" Then
                        Exit Select
                    End If
                Next

                '***CALL ParaSAVE(THIQ) TO SAVE VALUE
                ParaSAVE.THLQ(Value, ColumnIndex, Index)

                '***CHECKS THAT ParaMESH(THLQ) = 0 IF ParaMESH(TBAR) <= 0
                ParaLOG.AppendFile(ParameterCall & "# (GRU " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ") ..special condition: if TBAR (GRU " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ") is less than or equal to {0}, THLQ (GRU " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ") must equal {0}", "ParaCHECK.SpecialCheck", "ValVar")
                If ParaMESH.TBAR(0, ColumnIndex, Index) <= 0 And ParaMESH.THLQ(0, ColumnIndex, Index) > 0 Then
                    ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = "The value {" & ParaMESH.THLQ(0, ColumnIndex, Index) & "} (GRU " & Index + 1 & ", Column " & ColumnIndex + 1 & ") must equal {0} because " & ParaMESH.PrimaryParameters(3, ParaMESH.FindIndex("TBAR", ParaMESH.PrimaryParameters)) & " (TBAR) (GRU " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ") is less than or equal to {0}."
                    ParaMESH.CheckList(2, ParaMESH.CheckList.GetUpperBound(1)) = ParaMESH.THLQ(0, ColumnIndex, Index)
                    ParaMESH.CheckList(3, ParaMESH.CheckList.GetUpperBound(1)) = "0"
                    ReDim Preserve ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), ParaMESH.CheckList.GetUpperBound(1) + 1)
                    Exit Sub
                ElseIf ParaMESH.TBAR(0, ColumnIndex, Index) > 0 And ParaMESH.THLQ(0, ColumnIndex, Index) = 0 Then
                    ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = "The value {" & ParaMESH.THLQ(0, ColumnIndex, Index) & "} (GRU " & Index + 1 & ", Column " & ColumnIndex + 1 & ") must be greater than {0} because" & ParaMESH.PrimaryParameters(3, ParaMESH.FindIndex("TBAR", ParaMESH.PrimaryParameters)) & " (TBAR) (GRU " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ") is greater than {0}."
                    ParaMESH.CheckList(2, ParaMESH.CheckList.GetUpperBound(1)) = ParaMESH.THLQ(0, ColumnIndex, Index)
                    ParaMESH.CheckList(3, ParaMESH.CheckList.GetUpperBound(1)) = ParameterArray(5, ParaMESH.FindIndex(ParameterCall, ParameterArray))
                    ReDim Preserve ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), ParaMESH.CheckList.GetUpperBound(1) + 1)
                    Exit Sub
                End If
            Case Is = "THIC"

                '***CHECK ParaMESH(CheckList) FOR ERRONEOUS DEPENDENT PARAMETER VALUE
                ParaLOG.AppendFile(ParameterCall & "# ..special condition: checking for erroneous dependents (TBAR) in ParaMESH(CheckList)", "ParaCHECK.SpecialCheck", "ValVar")
                For g = 0 To ParaMESH.CheckList.GetUpperBound(1) - 1
                    If ParaMESH.CheckList(1, g) <> Nothing And ParaMESH.CheckList(4, g) = "TBAR" Then
                        Exit Select
                    End If
                Next

                '***CALL ParaSAVE(THIC) TO SAVE VALUE
                ParaSAVE.THIC(Value, ColumnIndex, Index)

                '***CHECKS THAT ParaMESH(THIC) = 0 IF ParaMESH(TBAR) > 0
                ParaLOG.AppendFile(ParameterCall & "# (GRU " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ") ..special condition: if TBAR (GRU " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ") is greater than {0}, THIC (GRU " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ") must equal {0}", "ParaCHECK.SpecialCheck", "ValVar")
                If ParaMESH.TBAR(0, ColumnIndex, Index) > 0 And ParaMESH.THIC(0, ColumnIndex, Index) > 0 Then
                    ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = "The value {" & ParaMESH.THIC(0, ColumnIndex, Index) & "} (GRU " & Index + 1 & ", Column " & ColumnIndex + 1 & ") must equal {0} because" & ParaMESH.PrimaryParameters(3, ParaMESH.FindIndex("TBAR", ParaMESH.PrimaryParameters)) & " (TBAR) (GRU " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ") is greater than {0}."
                    ParaMESH.CheckList(2, ParaMESH.CheckList.GetUpperBound(1)) = ParaMESH.THIC(0, ColumnIndex, Index)
                    ParaMESH.CheckList(3, ParaMESH.CheckList.GetUpperBound(1)) = "0"
                    ReDim Preserve ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), ParaMESH.CheckList.GetUpperBound(1) + 1)
                    Exit Sub
                ElseIf ParaMESH.TBAR(0, ColumnIndex, Index) <= 0 And ParaMESH.THIC(0, ColumnIndex, Index) = 0 Then
                    ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = "The value {" & ParaMESH.THIC(0, ColumnIndex, Index) & "} (GRU " & Index + 1 & ", Column " & ColumnIndex + 1 & ") must be greater than {0} because" & ParaMESH.PrimaryParameters(3, ParaMESH.FindIndex("TBAR", ParaMESH.PrimaryParameters)) & "(TBAR) (GRU " & (Index + 1).ToString & ", Column " & (ColumnIndex + 1).ToString & ") is less than or equal to {0}."
                    ParaMESH.CheckList(2, ParaMESH.CheckList.GetUpperBound(1)) = ParaMESH.THIC(0, ColumnIndex, Index)
                    ParaMESH.CheckList(3, ParaMESH.CheckList.GetUpperBound(1)) = ParameterArray(5, ParaMESH.FindIndex(ParameterCall, ParameterArray))
                    ReDim Preserve ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), ParaMESH.CheckList.GetUpperBound(1) + 1)
                    Exit Sub
                End If
            Case Is = "WatflowFlag"
                'ParaLOG.AppendFile(ParameterCall & "# (Column " & (Column + 1).ToString & ", GRU " & (GRU + 1).ToString & ") ..special condition: that value of GRU exists", "ParaCHECK.SpecialCheck", "ValVar")
                'If Convert.ToInt32(ParaMESH.MDEST(0, i, GRU)) > ParaMESH.GRU(0) Then
                'Write CheckList
                'Exit Sub
                'End If

                '***CODE FROM OLD READ SUBROUTINE FOR WATFLOW(MDEST)
                'If TempArray(7).Substring(0, 5).Trim.Length = 5 Then
                'ParaMESH.MDEST(j, 0, Convert.ToInt32(TempArray(7).Substring(0, 1)) - 1) = TempArray(7).Substring(1, 1)
                'ParaMESH.FMDIV(j, 0, Convert.ToInt32(TempArray(7).Substring(0, 1)) - 1) = Convert.ToDouble(TempArray(7).Substring(2, 1)) / 10
                'ParaMESH.MDEST(j, 1, Convert.ToInt32(TempArray(7).Substring(0, 1)) - 1) = TempArray(7).Substring(3, 1)
                'ParaMESH.FMDIV(j, 1, Convert.ToInt32(TempArray(7).Substring(0, 1)) - 1) = Convert.ToDouble(TempArray(7).Substring(4, 1)) / 10
                'ParaMESH.DiversionFlag(j) = 1
                'ParaMESH.MDEST(2, 2, 7) = TempArray(7).Substring(0, 1)
                'ElseIf TempArray(7).Substring(0, 5).Trim.Length = 3 Then
                'ParaMESH.MDEST(j, 0, Convert.ToInt32(TempArray(7).Substring(0, 5).Trim.Substring(0, 1)) - 1) = TempArray(7).Substring(0, 5).Trim.Substring(1, 1)
                'ParaMESH.FMDIV(j, 0, Convert.ToInt32(TempArray(7).Substring(0, 5).Trim.Substring(0, 1)) - 1) = Convert.ToDouble(TempArray(7).Substring(0, 5).Trim.Substring(2, 1)) / 10
                'ParaMESH.DiversionFlag(j) = 1
                'ParaMESH.MDEST(2, 2, 7) = TempArray(7).Substring(0, 5).Trim.Substring(0, 1)
                'Else
                'If TempArray(7).Substring(0, 5).Trim.Length > 0 Then
                'ParaMESH.DiversionFlag(j) = 0
                'ParaMESH.MDEST(2, 2, 7) = 0
                'End If
                'End If

                '***CODE FROM OLD READ SUBROUTINE FOR WATFLOW(FIELD CAPACITY)
                'ParaLOG.AppendFile("ParaMESH is reading Line 10 of " & FileName
                'If TempArray(9).Substring(0, 5).Trim.Length = 1 Then
                'ParaMESH.ColumnCapacityFlag(j) = 0
                'ParaMESH.InfiltrationFlag(j) = 0
                'ParaMESH.RechargeFlag(j) = 0
                'Else
                'If TempArray(9).Substring(0, 5).Substring(2, 1).Trim.Length > 0 Then
                'ParaMESH.ColumnCapacityFlag(j) = TempArray(9).Substring(0, 5).Substring(2, 1)
                'End If
                'If TempArray(9).Substring(0, 5).Substring(3, 1).Trim.Length > 0 Then
                'ParaMESH.InfiltrationFlag(j) = TempArray(9).Substring(0, 5).Substring(3, 1)
                'End If
                'If TempArray(9).Substring(0, 5).Substring(4, 1).Trim.Length > 0 Then
                'ParaMESH.RechargeFlag(j) = TempArray(9).Substring(0, 5).Substring(4, 1)
                'End If
                'End If

                '***CODE FROM OLD READ SUBROUTING FOR WATFLOW(ICE-VISCOSITY)
                'ParaLOG.AppendFile("ParaMESH is reading Line 13 of " & FileName
                'If TempArray(12).Substring(0, 5).Trim = "0" Then
                'ParaMESH.IceFlag(j) = 0
                'ParaMESH.ViscosityFlag(j) = 0
                'ElseIf TempArray(12).Substring(0, 5).Trim = "1" Then
                'ParaMESH.IceFlag(j) = 1
                'ParaMESH.ViscosityFlag(j) = 0
                'ElseIf TempArray(12).Substring(0, 5).Trim = "2" Then
                'ParaMESH.IceFlag(j) = 0
                'ParaMESH.ViscosityFlag(j) = 1
                'ElseIf TempArray(12).Substring(0, 5).Trim = "3" Then
                'ParaMESH.IceFlag(j) = 1
                'ParaMESH.ViscosityFlag(j) = 1
                'Else
                'MsgBox("ParaMESH could not read Line 13: " & "Lbl_IceFlag" & " of watflow.ini.", MsgBoxStyle.OkOnly, "ParaMESH")
                'End If

                '***CODE FROM OLD READ SUBROUTINE FOR WATFLOW(GWSCALE)
                'ParaLOG.AppendFile("ParaMESH is reading Line 15 of " & FileName
                'If Convert.ToDouble(TempArray(14).Substring(0, 5).Trim) = 0 Then
                'ParaMESH.BaseflowFlag(j) = 0
                'Else
                'If TempArray(14).Substring(0, 5).Trim.Length > 0 Then
                'ParaMESH.GWSCALE(j) = TempArray(14).Substring(0, 5).Trim
                'ParaMESH.BaseflowFlag(j) = 1
                'End If
                'End If
            Case Is = "RHOS"

                '***CHECK ParaMESH(CheckList) FOR ERRONEOUS DEPENDENT PARAMETER VALUE
                ParaLOG.AppendFile(ParameterCall & "# ..special condition: checking for erroneous dependents (SNO) in ParaMESH(CheckList)", "ParaCHECK.SpecialCheck", "ValVar")
                For g = 0 To ParaMESH.CheckList.GetUpperBound(1) - 1
                    If ParaMESH.CheckList(1, g) <> Nothing And ParaMESH.CheckList(4, g) = "SNO" Then
                        Exit Select
                    End If
                Next

                '***CALL ParaSAVE(RHOS) TO SAVE VALUE
                ParaSAVE.RHOS(Value, Index)

                '***CHECKS THAT ParaMESH(RHOS) = 0 IF ParaMESH(SNO) = 0 (NO SNOW COVER EXISTS)
                ParaLOG.AppendFile(ParameterCall & "# (GRU " & (Index + 1).ToString & ") ..special condition: if SNO (GRU " & (Index + 1).ToString & ") is equal to {0}, RHOS (GRU " & (Index + 1).ToString & ") must equal {0}; no snow cover exists", "ParaCHECK.SpecialCheck", "ValVar")
                If ParaMESH.SNO(0, Index) = 0 And ParaMESH.RHOS(0, Index) > 0 Then
                    ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = "The value {" & ParaMESH.RHOS(0, Index) & "} (GRU " & Index + 1 & ") must equal {0} because " & ParaMESH.PrimaryParameters(4, ParaMESH.FindIndex("SNO", ParaMESH.PrimaryParameters)) & " (SNO) is equal to {0}; no snow cover exists."
                    ParaMESH.CheckList(2, ParaMESH.CheckList.GetUpperBound(1)) = ParaMESH.RHOS(0, Index)
                    ParaMESH.CheckList(3, ParaMESH.CheckList.GetUpperBound(1)) = "0"
                    ReDim Preserve ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), ParaMESH.CheckList.GetUpperBound(1) + 1)
                    Exit Sub
                End If
        End Select

        '***RESET FIELD 1 OF ParaMESH(CheckList) (ERROR MESSAGE) TO NOTHING
        '***ERRORNEOUS VALUES ARE IDENTIFIED BY THE POPULATED ERROR MESSAGE
        ParaMESH.CheckList(1, ParaMESH.CheckList.GetUpperBound(1)) = Nothing
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaCHECK.TrimList
    '*****************************************************************************
    'UPDATE: FEB 06 2008 (DAN)  call ParaMESH(RemoveField)
    '*****************************************************************************
    'Filters the parameter check error list ParaMESH(CheckList) for entries
    'with blank error descriptions.
    '*****************************************************************************

    Private Shared Sub TrimList()

        '***FILTER ParaMESH(CheckList) FOR ENTRIES WITH NO ERROR DESCRIPTION
        ParaLOG.AppendFile("ParaMESH is filtering blank error fields in ParaMESH(CheckList)", "ParaCHECK.TrimList")
        Do
            If ParaMESH.CheckList.Length < 1 Then
                Exit Do
            End If

            '***CONTINUE CHECKING UNTIL ALL EMPTY FIELDS REMOVED
            For g = 0 To ParaMESH.CheckList.GetUpperBound(1)
                If ParaMESH.CheckList(1, g) = Nothing Then

                    '***CALL ParaMESH(RemoveField) TO REMOVE EMPTY FIELD
                    ParaMESH.CheckList = ParaMESH.RemoveField(g, ParaMESH.CheckList)
                    Exit For
                ElseIf g = ParaMESH.CheckList.GetUpperBound(1) Then
                    Exit Do
                End If
            Next
        Loop
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaCHECK.WriteList
    '*****************************************************************************
    'UPDATE: JAN 15 2008 (DAN)  use ParaCHECK.CheckList bounds instead of 
    '                           MaxCheckForm.Arr_CheckList.RowCount to display 
    '                           MaxCheckForm
    'UPDATE: JAN 14 2008 (DAN)  isolated ParaCHECK.TrimList routine
    'UPDATE: JAN 11 2008 (DAN)  streamlining, validating process structure
    'UPDATE: JAN 11 2008 (DAN)  for IgnoreCheck approach
    'UPDATE: JAN 11 2008 (DAN)  for ParaLOG approach
    '*****************************************************************************
    'Write the parameter checklist (ParaCHECK.CheckList) to the 
    'CheckForm.Arr_CheckForm table and filters it for blank error fields.
    'Redimensions the ParaCHECK.CheckList array.
    '*****************************************************************************

    Private Shared Sub WriteList()

        '***CHECK IF AN ERROR LIST EXISTS
        ParaCHECK.TrimList()

        '***MaxCheckForm IS ONLY DISPLAYED IF AT LEAST ONE PARAMETER CHECK ERROR RECORD EXISTS
        If ParaMESH.CheckList.Length < 1 Then
            Exit Sub
        End If
        With MaxCheckForm
            .Arr_CheckList.Rows.Clear()

            '***WRITE ERROR ENTRIES TO MaxCheckForm
            ParaLOG.AppendFile("ParaMESH is populating the ParaMESH Parameter Check (ParaCHECK) summary table", "ParaCHECK.WriteList")
            For h = 0 To ParaMESH.CheckList.GetUpperBound(1)
                .Arr_CheckList.RowCount += 1
                .Arr_CheckList.Item("Grd_ParameterName", h).Value = ParaMESH.CheckList(0, h)
                .Arr_CheckList.Item("Grd_ErrorDescription", h).Value = ParaMESH.CheckList(1, h)
                .Arr_CheckList.Item("Grd_OldValue", h).Value = ParaMESH.CheckList(2, h)
                .Arr_CheckList.Item("Grd_NewValue", h).Value = ParaMESH.CheckList(3, h)
                .Arr_CheckList.Item("Grd_ParameterCall", h).Value = ParaMESH.CheckList(4, h)
                .Arr_CheckList.Item("Grd_Index", h).Value = ParaMESH.CheckList(5, h)
                .Arr_CheckList.Item("Grd_ColumnIndex", h).Value = ParaMESH.CheckList(6, h)
            Next

            '***WRITES THE NUMBER OF ENTRIES TO MaxCheckForm.Box_ErrorCount
            ParaLOG.AppendFile("ParaMESH is preparing the ParaMESH Parameter Check (ParaCHECK) summary table", "ParaCHECK.WriteList")
            If .Arr_CheckList.RowCount > 0 Then
                'If .Arr_CheckList.RowCount > 1 Then
                '.Box_ErrorCount.Text = "ParaMESH has found errors with " & .Arr_CheckList.RowCount & " parameter values."
                'Else
                '.Box_ErrorCount.Text = "ParaMESH has found an error with " & .Arr_CheckList.RowCount & " parameter value."
                'End If
                If .Visible = False Then
                    .ShowDialog()
                End If

                '***SAVE THE VALUES
                For g = 0 To .Arr_CheckList.RowCount - 1
                    If .Arr_CheckList.Item("Grd_Ignore", g).Value = False Then

                        '***SAVE THE VALUE
                        ParaMESH.SaveValue(.Arr_CheckList.Item("Grd_NewValue", g).Value, .Arr_CheckList.Item("Grd_ParameterCall", g).Value, .Arr_CheckList.Item("Grd_Index", g).Value, .Arr_CheckList.Item("Grd_ColumnIndex", g).Value)
                    Else

                        '***ADD THE PARAMETER TO THE ParaCHECK IGNORE LIST (ParaMESH.IgnoreCheck)
                        ParaLOG.AppendFile("ParaMESH is adding (" & .Arr_CheckList.Item("Grd_ParameterCall", g).Value & ") to the ParaCHECK ignore list", "ParaCHECK.Save")
                        ReDim Preserve ParaMESH.IgnoreCheck(ParaMESH.IgnoreCheck.GetUpperBound(0), ParaMESH.IgnoreCheck.GetUpperBound(1) + 1)
                        ParaMESH.IgnoreCheck(0, ParaMESH.IgnoreCheck.GetUpperBound(1)) = .Arr_CheckList.Item("Grd_ParameterCall", g).Value
                        ParaMESH.IgnoreCheck(1, ParaMESH.IgnoreCheck.GetUpperBound(1)) = .Arr_CheckList.Item("Grd_Index", g).Value
                    End If
                Next
            End If

            '***CONTINUES CHECKING VALUES IN TABLE UNTIL ALL PARAMETER CHECK ERRORS ARE CLEARED
            '***USES h OR ELSE THE VALUE (IE, FOR g) IS RESET IN THE CALLED SUBROUTINE (ParaCHECK.CheckRange)
            ReDim ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), 0)
            For h = 0 To .Arr_CheckList.RowCount - 1

                '***CALL ParaCHECK(CheckRange) TO RE-CHECK PARAMETER VALUES
                ParaCHECK.CheckRange(.Arr_CheckList.Item("Grd_NewValue", h).Value, .Arr_CheckList.Item("Grd_ParameterCall", h).Value, .Arr_CheckList.Item("Grd_Index", h).Value, .Arr_CheckList.Item("Grd_ColumnIndex", h).Value)
            Next
            ParaCHECK.WriteList()
        End With
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaCHECK.Array
    '*****************************************************************************
    'Special checking subroutine for ParaSTOR.  Checks CheckArray parameter 
    'values, where CheckArray contains parameters which are being restored or for 
    'which backup values are being called.
    '*****************************************************************************

    Public Shared Sub Array(ByVal CheckArray(,) As String)

        '***EXIT SUB IF ParaCHECK IS DISABLED IN ParaMESH.SystemParameters
        If ParaMESH.IsEnabled("ParaCHECK", ParaMESH.SystemParameters, 1) = False Then
            Exit Sub
        End If

        '***CLEAR ParaMESH(CheckList)
        ReDim ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), 0)

        '***CALL ParaCHECK.CheckRange FOR EACH ROW IN CheckArray (-1: LAST ROW IS EMPTY BY DEFINITION)
        For h = 0 To CheckArray.GetUpperBound(1) - 1

            '***CALL ParaCHECK(CheckRange) TO CHECK PARAMETER VALUES
            If CheckArray.GetUpperBound(0) > 3 Then
                ParaCHECK.CheckRange(CheckArray(0, h), CheckArray(1, h), CheckArray(2, h), CheckArray(3, h), CheckArray(4, h))
            End If
            ParaCHECK.CheckRange(CheckArray(0, h), CheckArray(1, h), CheckArray(2, h), CheckArray(3, h))
        Next

        '***CALL ParaCHECK(TrimList) TO CLEAN ParaMESH(CheckList)
        ParaCHECK.TrimList()

        '***WRITE ParaCHECK.CheckList TO MaxCheckForm
        ParaCHECK.WriteList()
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaCHECK.MiniCheck
    '*****************************************************************************
    'UPDATE: JAN 14 2008 (DAN) to accept primary (ParaMESH.PrimaryParameters) and 
    'secondary (ParaMESH.SecondaryParameters) parameter arrays.
    '*****************************************************************************
    'Checks a single parameter's value; used for new form field entry checks.
    '*****************************************************************************

    Public Shared Sub MiniCheck(ByVal Value As String, ByVal ParameterCall As String, Optional ByVal Index As Integer = -1, Optional ByVal ColumnIndex As Integer = -1)

        '***EXIT SUB IF ParaCHECK IS DISABLED IN ParaMESH.SystemParameters
        If ParaMESH.IsEnabled("ParaCHECK", ParaMESH.SystemParameters, 1) = False Then
            Exit Sub
        End If

        '***CLEAR ParaMESH(CheckList)
        ReDim ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), 0)

        '***CALL ParaCHECK(CheckRange) TO CHECK VALUE
        ParaCHECK.CheckRange(Value, ParameterCall, Index, ColumnIndex)

        '***CALL ParaCHECK(TrimList) TO CLEAN ParaMESH(CheckList)
        ParaCHECK.TrimList()

        '***ONLY CALL PromptForm IF ERROR WITH PARAMETER VALUE EXISTS
        If ParaMESH.CheckList.GetUpperBound(1) > 0 Then

            '***CALL ParaCHECK(WriteList) IF IS A SPECIAL CHECK
            '***MORE PARAMETERS MAY BE INVOLVED THAN ParameterCall; MinCheckForm WILL ONLY DISPLAY SINGLE PARAMETER VALUE
            ParaCHECK.WriteList()
            Exit Sub
        ElseIf ParaMESH.CheckList.Length > 0 Then

            '***FIND ParameterCall IN PARAMETER ARRAYS
            ParaLOG.AppendFile(ParameterCall & "# ..finding the parameter in the parameter arrays", "ParaCHECK.MiniCheck", "ValVar")
            If ParaMESH.FindIndex(ParameterCall, ParaMESH.PrimaryParameters) > -1 Then
                ParameterArray = ParaMESH.PrimaryParameters
            Else
                ParameterArray = ParaMESH.SecondaryParameters
            End If
            With MinCheckForm
                .Box_ErrorMessage.Text = "ParaMESH has found an error with the parameter: " & ParameterArray(4, ParaMESH.FindIndex(ParameterCall, ParameterArray))
                If ParameterArray(3, ParaMESH.FindIndex(ParameterCall, ParameterArray)).Length > 0 Then
                    .Box_ErrorMessage.Text &= " (" & ParameterArray(3, ParaMESH.FindIndex(ParameterCall, ParameterArray)) & ")"
                End If
                .Box_ErrorDescription.Text = ParaMESH.CheckList(1, 0)
                .Box_VariableName.Text = ParameterCall
                .Box_NewValue.Text = ParaMESH.CheckList(3, 0)
                .ShowDialog()

                '***CALL ParaCHECK(MiniCheck) TO RE-CHECK PARAMETER VALUES
                ParaCHECK.MiniCheck(.Box_NewValue.Text, ParameterCall, Index, ColumnIndex)
            End With
        Else

            '***CALL ParaSAVE(FindVariable) TO SAVE VALUE
            ParaMESH.SaveValue(Value, ParameterCall, Index, ColumnIndex)
        End If
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaCHECK.Check
    '*****************************************************************************
    'UPDATE: MAR 22 2008 (DAN)  update to include "WatflowForm"
    'UPDATE: JAN 10 2008 (DAN)  streamlining, validating process structure
    '*****************************************************************************
    'Calls either ParaCHECK.ProcessForm, ParaCHECK.GRUForm, or both for a 
    'parameter value check.  If no GRU is given, then all GRUs are checked.
    '*****************************************************************************

    Public Shared Sub Check(Optional ByVal GRU As Integer = -1)

        '***ONLY CALL ParaCHECK IF IS ENABLED IN ParaMESH.SystemParameters ARRAY
        If ParaMESH.IsEnabled("ParaCHECK", ParaMESH.SystemParameters, 1) = False Then
            Exit Sub
        End If

        '***CLEAR ParaMESH(CheckList)
        ReDim ParaMESH.CheckList(ParaMESH.CheckList.GetUpperBound(0), 0)

        '***CHECK ProcessForm
        ParaCHECK.ProcessFormSub()

        '***WatflowForm
        ParaCHECK.WatflowFormSub()

        '***CHECK GRUForm
        If GRU = -1 Then
            For GRU = 0 To ParaMESH.GRU.GetUpperBound(1)
                ParaCHECK.GRUFormSub(GRU)
            Next

            '***RESET GRU
            GRU = -1
        Else
            ParaCHECK.GRUFormSub(GRU)
        End If

        '***WRITE THE PARAMETER CHECK LIST (ParaCHECK.CheckList)
        ParaCHECK.WriteList()

        '***POPULATE ProcessForm
        ProcessForm.Populate()
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaCHECK.ProcessFormSub
    '*****************************************************************************
    'Checks values for parameters displayed by ProcessForm.
    '*****************************************************************************

    Private Shared Sub ProcessFormSub()

        '***PROJECT INFORMATION
        ParaCHECK.CheckRange(ParaMESH.TITLE(0), "TITLE")
        ParaCHECK.CheckRange(ParaMESH.NAME(0), "NAME")
        ParaCHECK.CheckRange(ParaMESH.PLACE(0), "PLACE")

        '***SITE INFORMATION
        ParaCHECK.CheckRange(ParaMESH.DEGLAT(0), "DEGLAT")
        ParaCHECK.CheckRange(ParaMESH.DEGLON(0), "DEGLON")
        ParaCHECK.CheckRange(ParaMESH.ZRFM(0), "ZRFM")
        ParaCHECK.CheckRange(ParaMESH.ZRFH(0), "ZRFH")
        ParaCHECK.CheckRange(ParaMESH.ZBLD(0), "ZBLD")
        ParaCHECK.CheckRange(ParaMESH.GC(0), "GC")
        ParaCHECK.CheckRange(ParaMESH.ILW(0), "ILW")
        ParaCHECK.CheckRange(ParaMESH.GRID(0), "GRID")

        '***RUN TIMES
        ParaCHECK.CheckRange(ParaMESH.HourlyStartDay(0), "HourlyStartDay")
        ParaCHECK.CheckRange(ParaMESH.HourlyStartYear(0), "HourlyStartYear")
        ParaCHECK.CheckRange(ParaMESH.HourlyStopDay(0), "HourlyStopDay")
        ParaCHECK.CheckRange(ParaMESH.HourlyStopYear(0), "HourlyStopYear")
        ParaCHECK.CheckRange(ParaMESH.DailyStartDay(0), "DailyStartDay")
        ParaCHECK.CheckRange(ParaMESH.DailyStartYear(0), "DailyStartYear")
        ParaCHECK.CheckRange(ParaMESH.DailyStopDay(0), "DailyStopDay")
        ParaCHECK.CheckRange(ParaMESH.DailyStopYear(0), "DailyStopYear")
        ParaCHECK.CheckRange(ParaMESH.SimStartDay(0), "SimStartDay")
        ParaCHECK.CheckRange(ParaMESH.SimStartYear(0), "SimStartYear")
        ParaCHECK.CheckRange(ParaMESH.SimStopDay(0), "SimStopDay")
        ParaCHECK.CheckRange(ParaMESH.SimStopYear(0), "SimStopYear")
        ParaCHECK.CheckRange(ParaMESH.MetStartMin(0), "MetStartMin")
        ParaCHECK.CheckRange(ParaMESH.MetStartHour(0), "MetStartHour")
        ParaCHECK.CheckRange(ParaMESH.MetStartDay(0), "MetStartDay")
        ParaCHECK.CheckRange(ParaMESH.MetStartYear(0), "MetStartYear")

        '***HYDROLOGIC PARAMETERS
        For h = 0 To ParaMESH.WF_R2.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.WF_R2(0, h), ParaMESH.SecondaryName(h, "WF_R2"), h)
        Next

        '***SECONDARY PARAMETERS
        For h = 0 To ParaMESH.ControlFlag.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.ControlFlag(0, h), ParaMESH.SecondaryName(h, "ControlFlag"), h)
        Next
        For q = 0 To ParaMESH.GridOutput.GetUpperBound(2)
            For h = 0 To ParaMESH.GridOutput.GetUpperBound(1)
                ParaCHECK.CheckRange(ParaMESH.GridOutput(0, h, q), ParaMESH.SecondaryName(h, "GridOutput"), q, h)
            Next
        Next
        For h = 0 To ParaMESH.OptionFlag.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.OptionFlag(0, h), ParaMESH.SecondaryName(h, "OptionFlag"), h)
        Next
        For h = 0 To ParaMESH.IndependentGRU.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.IndependentGRU(0, h), ParaMESH.SecondaryName(h, "IndependentGRU"), h)
        Next
        For q = 0 To ParaMESH.DependentGRU.GetUpperBound(2)
            For h = 0 To ParaMESH.DependentGRU.GetUpperBound(1)
                ParaCHECK.CheckRange(ParaMESH.DependentGRU(0, h, q), ParaMESH.SecondaryName(h, "DependentGRU"), q, h)
            Next
        Next
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaCHECK.WatflowFormSub
    '*****************************************************************************
    'Checks values for parameters displayed by WatflowForm.
    '*****************************************************************************

    Private Shared Sub WatflowFormSub()

        '***BASIN INFORMATION, SCALING FACTOR
        ParaCHECK.CheckRange(ParaMESH.BasinID(0), "BasinID")
        ParaCHECK.CheckRange(ParaMESH.ScalingFactor(0), "ScalingFactor")

        '***INITIAL GROUNDWATER
        ParaCHECK.CheckRange(ParaMESH.GWINIT(0), "GWINIT")

        '***WATFLOW FLAGS
        For h = 0 To ParaMESH.WatflowFlag.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.WatflowFlag(0, h), ParaMESH.SecondaryName(h, "WatflowFlag"), h)
        Next

        '***PONDING LIMITS
        ParaCHECK.CheckRange(ParaMESH.ZPLIMG0(0), "ZPLIMG0")
        ParaCHECK.CheckRange(ParaMESH.ZPLIMS0(0), "ZPLIMS0")
        ParaCHECK.CheckRange(ParaMESH.ZPLIMP0(0), "ZPLIMP0")
        ParaCHECK.CheckRange(ParaMESH.ZPLIMPS0(0), "ZPLIMPS0")

        '***SNOW LIMITS
        For h = 0 To ParaMESH.D100A.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.D100A(0, h), ParaMESH.SecondaryName(h, "D100A"), h)
        Next
    End Sub

    '*****************************************************************************
    'SUBROUTINE: ParaCHECK.GRUFormSub
    '*****************************************************************************
    'Checks values for parameters displayed by GRUForm.
    '*****************************************************************************

    Private Shared Sub GRUFormSub(ByVal GRU As Integer)

        '***GRU DESCRIPTION
        ParaCHECK.CheckRange(ParaMESH.GRU(0, GRU), "GRU", GRU)

        '***FIRST BLOCK
        For h = 0 To ParaMESH.FCAN.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.FCAN(0, h, GRU), ParaMESH.SecondaryName(h, "FCAN"), GRU, h, True)
        Next
        For h = 0 To ParaMESH.LNZ0.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.LNZ0(0, h, GRU), ParaMESH.SecondaryName(h, "LNZ0"), GRU, h, True)
        Next
        For h = 0 To ParaMESH.ALVC.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.ALVC(0, h, GRU), ParaMESH.SecondaryName(h, "ALVC"), GRU, h, True)
        Next
        For h = 0 To ParaMESH.ALIC.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.ALIC(0, h, GRU), ParaMESH.SecondaryName(h, "ALIC"), GRU, h, True)
        Next
        For h = 0 To ParaMESH.LAMX.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.LAMX(0, h, GRU), ParaMESH.SecondaryName(h, "LAMX"), GRU, h, True)
        Next
        For h = 0 To ParaMESH.LAMN.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.LAMN(0, h, GRU), ParaMESH.SecondaryName(h, "LAMN"), GRU, h, True)
        Next
        For h = 0 To ParaMESH.CMAS.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.CMAS(0, h, GRU), ParaMESH.SecondaryName(h, "CMAS"), GRU, h, True)
        Next
        For h = 0 To ParaMESH.ROOT.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.ROOT(0, h, GRU), ParaMESH.SecondaryName(h, "ROOT"), GRU, h, True)
        Next
        For h = 0 To ParaMESH.RSMN.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.RSMN(0, h, GRU), ParaMESH.SecondaryName(h, "RSMN"), GRU, h, True)
        Next
        For h = 0 To ParaMESH.QA50.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.QA50(0, h, GRU), ParaMESH.SecondaryName(h, "QA50"), GRU, h, True)
        Next
        For h = 0 To ParaMESH.VPDA.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.VPDA(0, h, GRU), ParaMESH.SecondaryName(h, "VPDA"), GRU, h, True)
        Next
        For h = 0 To ParaMESH.VPDB.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.VPDB(0, h, GRU), ParaMESH.SecondaryName(h, "VPDB"), GRU, h, True)
        Next
        For h = 0 To ParaMESH.PSGA.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.PSGA(0, h, GRU), ParaMESH.SecondaryName(h, "PSGA"), GRU, h, True)
        Next
        For h = 0 To ParaMESH.PSGB.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.PSGB(0, h, GRU), ParaMESH.SecondaryName(h, "PSGB"), GRU, h, True)
        Next
        ParaCHECK.CheckRange(ParaMESH.DRN(0, GRU), "DRN", GRU)
        ParaCHECK.CheckRange(ParaMESH.SDEP(0, GRU), "SDEP", GRU)
        ParaCHECK.CheckRange(ParaMESH.FARE(0, GRU), "FARE", GRU)
        ParaCHECK.CheckRange(ParaMESH.DDEN(0, GRU), "DDEN", GRU)
        ParaCHECK.CheckRange(ParaMESH.XSLP(0, GRU), "XSLP", GRU)
        ParaCHECK.CheckRange(ParaMESH.GRKF(0, GRU), "GRKF", GRU)
        ParaCHECK.CheckRange(ParaMESH.WFSF(0, GRU), "WFSF", GRU)
        ParaCHECK.CheckRange(ParaMESH.WFCI(0, GRU), "WFCI", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC01(0, GRU), "RSERVC01", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC02(0, GRU), "RSERVC02", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC03(0, GRU), "RSERVC03", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC04(0, GRU), "RSERVC04", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC05(0, GRU), "RSERVC05", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC06(0, GRU), "RSERVC06", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC07(0, GRU), "RSERVC07", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC08(0, GRU), "RSERVC08", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC09(0, GRU), "RSERVC09", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC10(0, GRU), "RSERVC10", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC11(0, GRU), "RSERVC11", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC12(0, GRU), "RSERVC12", GRU)

        '***SECOND BLOCK
        For h = 0 To ParaMESH.SAND.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.SAND(0, h, GRU), ParaMESH.SecondaryName(h, "SAND"), GRU, h, True)
        Next
        For h = 0 To ParaMESH.CLAY.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.CLAY(0, h, GRU), ParaMESH.SecondaryName(h, "CLAY"), GRU, h, True)
        Next
        For h = 0 To ParaMESH.ORGM.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.ORGM(0, h, GRU), ParaMESH.SecondaryName(h, "ORGM"), GRU, h, True)
        Next
        For h = 0 To ParaMESH.TBAR.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.TBAR(0, h, GRU), ParaMESH.SecondaryName(h, "TBAR"), GRU, h, True)
        Next
        ParaCHECK.CheckRange(ParaMESH.TCAN(0, GRU), "TCAN", GRU)
        ParaCHECK.CheckRange(ParaMESH.TSNO(0, GRU), "TSNO", GRU)
        ParaCHECK.CheckRange(ParaMESH.TPND(0, GRU), "TPND", GRU)
        ParaCHECK.CheckRange(ParaMESH.ZPND(0, GRU), "ZPND", GRU)
        For h = 0 To ParaMESH.THLQ.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.THLQ(0, h, GRU), ParaMESH.SecondaryName(h, "THLQ"), GRU, h, True)
        Next
        For h = 0 To ParaMESH.THIC.GetUpperBound(1)
            ParaCHECK.CheckRange(ParaMESH.THIC(0, h, GRU), ParaMESH.SecondaryName(h, "THIC"), GRU, h, True)
        Next
        ParaCHECK.CheckRange(ParaMESH.RCAN(0, GRU), "RCAN", GRU)
        ParaCHECK.CheckRange(ParaMESH.SCAN(0, GRU), "SCAN", GRU)
        ParaCHECK.CheckRange(ParaMESH.SNO(0, GRU), "SNO", GRU)
        ParaCHECK.CheckRange(ParaMESH.ALBS(0, GRU), "ALBS", GRU)
        ParaCHECK.CheckRange(ParaMESH.RHOS(0, GRU), "RHOS", GRU)
        ParaCHECK.CheckRange(ParaMESH.GRO(0, GRU), "GRO", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC13(0, GRU), "RSERVC13", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC14(0, GRU), "RSERVC14", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC15(0, GRU), "RSERVC15", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC16(0, GRU), "RSERVC16", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC17(0, GRU), "RSERVC17", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC18(0, GRU), "RSERVC18", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC19(0, GRU), "RSERVC19", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC20(0, GRU), "RSERVC20", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC21(0, GRU), "RSERVC21", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC22(0, GRU), "RSERVC22", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC23(0, GRU), "RSERVC23", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC24(0, GRU), "RSERVC24", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC25(0, GRU), "RSERVC25", GRU)
        ParaCHECK.CheckRange(ParaMESH.RSERVC26(0, GRU), "RSERVC26", GRU)
    End Sub
End Class
