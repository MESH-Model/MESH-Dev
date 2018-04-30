 BEGIN { FS = "," }
{
  max_nr = NR  
  sta = $1
  vector[NR, 1] = $2
#  if (sta == "09AB001") { vector[NR, 2] = 291.42 * $3 - 8323.9 } #don't know if the rating curve is correct.
  if (sta == "09BA001") { vector[NR, 2] = $7 }
  if (sta == "09BC002") { if ($3 == "") { vector[NR, 2] = 0 } else { vector[NR, 2] = 56 * ($3+0.43) ** 2.29 } }
  if (sta == "09DC006") { if ($3 == "") { vector[NR, 2] = 0 } else { vector[NR, 2] = 102 * ($3-2.1) ** 1.78 } }
  if (sta == "10AA001") { vector[NR, 2] = $7 }

} END {

  for (x = 2; x <= max_nr; x++) {

      dt1 = substr(vector[x, 1], 1, 10)
      dt2 = substr(vector[x, 1], 12, 8)
      printf ("%s %s\n", dt1" "dt2, vector[x,2])

       }
  }

