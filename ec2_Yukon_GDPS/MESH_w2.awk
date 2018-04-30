 BEGIN { FS = "," }
{
  max_nr = NR  
  sta = $1
  vector[NR, 1] = $2
  vector[NR, 2] = $3

} END {

  for (x = 2; x <= max_nr; x++) {

      dt1 = substr(vector[x, 1], 1, 10)
      dt2 = substr(vector[x, 1], 12, 8)
      printf ("%s %s,%s\n", dt1, dt2, vector[x, 2])

       }
  }

