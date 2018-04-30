 BEGIN { FS = "," }
{
  # Read the Observations File from Datamart and produce a MESH_input_streamflow.txt file
  max_nr = NR  
  sta = $1
  vector[NR, 1] = $2
  if (sta == "09AB001") { lat="3646.00" ; lon="-8100.0" ; vector[NR, 2] = 291.42 * $3 - 8323.9 }
  if (sta == "09BA001") { lat="3719.20" ; lon="-7945.0" ; vector[NR, 2] = $7 }
  if (sta == "09DC006") { lat="3815.43" ; lon="-8157.0" ; if ($3 == "") { vector[NR, 2] = 0 } else { if ($3 < 5.8) { vector[NR, 2] = 0.1973884 * ($3 - 0.1)** 4.0882516 } else { vector[NR, 2] = 100 * ($3 - 4.0)** 1.51 } }}
  if (sta == "10AA001") { lat="3604.00" ; lon="-7734.4" ; vector[NR, 2] = $7 }

} END {

  dt = systime() - 1*24*60*60
  dt2 = strftime("%Y", dt)
  dt3 = strftime("%j", dt)
  dt4 = mktime(strftime("%Y %m %d 13 00 00", dt))
  dt5 = strftime("%Y-%m-%dT%H", dt4)
  counter = 0
  
  ns = "1"
  el = ""
  ed = ""

  printf ("MESH streamflow records for %s\n", sta)
  printf ("    %s   18   18    3 %4s %4s   13    0\n", ns, dt2, dt3)
  printf ("%8s%8s %s\n", lat, lon, sta)
  printf ("%s", el)

  for (x = 1; x <= max_nr; x++) {

       if (vector[x, 1] ~ dt5){
          printf ("%s%s\n", vector[x, 2], ed)
          dt4 = dt4 + 3*60*60 
          dt5 = strftime("%Y-%m-%dT%H", dt4)
          counter = counter + 1
       }
  }
       if (counter < 18) {
       while (i < 18-counter) {
          printf ("-1.0%s\n", ed)
          i++
          }
       }  

}
