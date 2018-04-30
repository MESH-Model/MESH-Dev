 BEGIN { FS = " " }
{
  # Convert the MESH_output_streamflow_all.csv file into Streamflow_Daily.csv
  # This script primarly change the Julian Day and Hour stamps from the MESH
  # streamflow file into a date format that R can read to produce a plot  

  max_nr = NR
  vector[NR] = $0

  if (NR == 1) {

    split ($0, arr, ",")
    dt = systime() - 1*24*60*60
    ano = strftime("%Y", dt)

    split("31 28 31 30 31 30 31 31 30 31 30 31",m)
    yy = ano
    if (yy%4 == 0) m[2] = 29
    dd = arr[1]
    mm = 1
    while (dd > m[mm]) {
      dd -= m[mm]
      mm++
    }

    dt1 = mktime(sprintf("%s %s %s %i %i 00", yy, mm, dd, arr[2], arr[3]))
    dt2 = strftime("%Y-%m-%d %T", dt1)

  }
  # $6 and $7 print the second gauge, for the case of the 09DC002 basin
  # for all other basins $6 and $7 should print empty

} END {

  for (x = 1; x <= max_nr; x++) { 

    split (vector[x], arr, ",")

    printf ("%s,%s,%s,%s,%s\n", dt2, arr[4], arr[5], arr[6], arr[7])

    dt1 = dt1 + 30*60 
    dt2 = strftime("%Y-%m-%d %T", dt1)
  }
}
