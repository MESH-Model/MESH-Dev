BEGIN { FS = " " 
}

#Editing the initial date of the data sets in MESH_parameters_CLASS.
{ 

  if ( NR == 112 ) {

     split("31 28 31 30 31 30 31 31 30 31 30 31",m)
     yy = $4
     if (yy%4 == 0) m[2] = 29
     dd = $3
     mm = 1
     while (dd > m[mm]) {
        dd -= m[mm]
        mm++ }

     dt1 = mktime(sprintf("%4i %2i %2i %2i 00 00", yy, mm, dd, $1)) 
     dt2 = dt1 +1*24*60*60
     dt3 = strftime("%Y", dt2)
     dt4 = strftime("%j", dt2)
     dt5 = strftime("%H", dt2)
     dt6 = strftime("%M", dt2)

     printf ("%10i%10i%10i%10i                                  22\n", dt5, dt6, dt4, dt3 )

  } else {
      print $0 }
}

