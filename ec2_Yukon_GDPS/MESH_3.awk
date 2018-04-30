#
# AWK script to convert frames of data to R2C format from CSV
#
#   ********** FOR GEM (00,12 and 18 hour forecasts) **********
#
# Script loops through all lines in the file
#   NR = Number of lines (records)
#   NF = Number of fields parsed in each line
#
# Lines are parsed by space, " ", by default
#
# BEGIN reads each line
BEGIN { FS = " " }   {

    # First line contains basin information
    # Example: 09AB001,rain,58.70,-135.85,24,20,0.125,0.125
    if (NR == 1) {

        # Parse the line by comma, ','
        split ($1, arr, ",")

       # Grab the xcount, ycount, and date
        xcount = arr[5]+0
        ycount = arr[6]+0

        # Get the frame number (start)
        # Set to 1 if the field doesn't exist
        frame_start = arr[9]
        if (frame_start == 0) frame_start = 1
        frame_no = frame_start
    }

    # Data line
    # Example: "2017-06-06 06:00:00" "APCP" 1.894 2.3518 3.2692 2.9087 ...

    # Determine the date
    ano = substr($1, 2, 4)
    mes = substr($1, 7, 2)
    dii = substr($1, 10, 2)
    hor = substr($2, 1, 2)
    mii = substr($2, 4, 2)
    fecha = mktime(sprintf("%s %s %s %s %s 00", ano, mes, dii, hor, mii)) - 8*60*60
    dia[NR-1] = strftime("%Y/%m/%d %T.000", fecha)

    # Determine the name of the variable
    variable[NR-1] = $3

    # Flags for accumulated variables
    if ($3 ~ /DSWRF/) constant = 2; else
    if ($3 ~ /DLWRF/) constant = 2; else
    if ($3 ~ /APCP/) constant = 2; else
        constant = 1

    # Total number of records does not include the first line
    max_nr = NR-1

    # Assign the data to a variable
    for (x = 4; x <= NF; x++) {
        vector[NR-1, (x - 3)] = $x
        max_nf = x - 3
    }
}
# END runs after parsing the file
END {

    # GEM data are hourly

    # Option for fields that need to be deaccumulated
    if (constant == 2) {
        for (x = 2; x <= max_nr; x++) {
            for (y = 1; y <= max_nf; y++) {

                # If the deaccumulation results in a negative value, set the value to zero
                if (vector[x, y] < vector[(x - 1), y]) {
                    vector2[x, y] = 0.0
                } else {

                    # Deaccumulate the value and convert to an hourly rate
                    #   - DSWRF [J m-2] to [W m-2]
                    #   - DLWRF [J m-2] to [W m-2]
                    #   - APCP [kg m-2] to [kg m-2 s-1]
                    vector2[x, y] = (vector[x, y] - vector[(x - 1), y])/3600.0
                }
            }
        }

        for (x = 2; x <= max_nr; x++) {

            # Print the frame header including the frame number and date of the record
            printf(":Frame    %i  %i   \"%s\"\n", frame_no, frame_no, dia[x])

            # Write the matrix of data
            # 'x' is the record
            # 'y' is the field
            for (y = 1; y <= max_nf; y++) {
                if ((y % xcount) == 0) {
                    printf("%s\n", vector2[x, y])
                } else
                    printf("%s ", vector2[x, y])
            }

            # Close the frame
            printf ":EndFrame \n"

            # Increment the frame count
            frame_no++
        }

    # Option for fields that require no modification
    } else {
        for (x = 2; x <= max_nr; x++) {

            # Print the frame header including the frame number and date of the record
            printf(":Frame    %i  %i   \"%s\"\n", frame_no, frame_no, dia[x])

            # Write the matrix of data
            # 'x' is the record
            # 'y' is the field
            for (y = 1; y <= xcount*ycount; y++) {
                if ((y % xcount) == 0) {
                    printf("%s\n", vector[x, y])
                } else
                    printf("%s ", vector[x, y])
            }

            # Close the frame
            printf ":EndFrame \n"

            # Increment the frame count
            frame_no++
        }
    }
}
