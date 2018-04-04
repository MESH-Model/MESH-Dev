#
# AWK script to convert frames of data to R2C format from CSV
#
#   ********** FOR CaPA **********
#
# Script loops through all lines in the file
#   NR = Number of lines (records)
#   NF = Number of fields parsed in each line
#
# Lines are parsed by space, " ", by default
#
# BEGIN reads each line
BEGIN { FS = " " } {

    # First line contains basin information
    # Example: 09AB001,rain,58.70,-135.85,24,20,0.125,0.125
    if (NR == 1) {

        # Parse the line by comma, ','
        split ($1, arr, ",")

        # Grab the xcount, ycount, and date
        xcount = arr[5]+0
        ycount = arr[6]+0
    }

    # Data line
    # Example: "2017-06-06 06:00:00" "APCP" 1.894 2.3518 3.2692 2.9087 ...

    # Determine the date
    ano = substr($1, 2, 4)
    mes = substr($1, 7, 2)
    dii = substr($1, 10, 2)
    hor = substr($2, 1, 2)
    mii = substr($2, 4, 2)
    dia[NR-1] = mktime(sprintf("%s %s %s %s %s 00", ano, mes, dii, hor, mii)) - 8*60*60

    # Determine the name of the variable
    variable[NR-1] = $3

    # Total number of records does not include the first line
    max_nr = NR-1

    # Assign the data to a variable
    for (x = 4; x <= NF; x++) {
        vector[NR - 1, (x - 3)] = $x
    }
}
# END runs after parsing the file
END {

    # CaPA data are 6 hourly
    # 'p' is the number of records to create from each CaPA field
    p = (24/4)

    # 'z' is the frame number (in the file)
    z = 1
    for (x = 1; x <= max_nr; x++) {

        # Repeat the field 6 times
        for (i = 0; i < 6; i++) {
            segm = strftime("%Y/%m/%d %T.000", (dia[x] - (p - i)*60*60) )

            # Print the frame header including 'z' and the date, 'segm'
            printf(":Frame    %i  %i   \"%s\"\n", z, z, segm)

            # Write the matrix of data
            # 'x' is the records
            # 'y' is the field
            for (y = 1; y <= xcount*ycount; y++) {

                # mod(y, xcount) == 0 is the end of the line in the matrix
                # Values are divided by the time-step in seconds (e.g., 6 hourly * 3600 seconds)
                if ((y % xcount) == 0) {
                    printf("%s\n", vector[x, y]/(p*3600.0))
                }
                else
                    printf("%s ", vector[x, y]/(p*3600.0))
            }

            # Close the frame
            printf ":EndFrame \n"

            # Increment the frame count
            z = z + 1
        }
    }
}
