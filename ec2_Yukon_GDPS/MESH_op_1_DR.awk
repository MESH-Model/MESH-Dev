BEGIN { FS = " " 
}
 #FS means field separator in the input file, here, space.
{ 
#Changing the start date
  #Calculating the day from the day of year given in input file, e.g. day 167 is day 16 of June.
#  if ( NR == 39 ) {
   if ( NR == 40 ) {
     split("31 28 31 30 31 30 31 31 30 31 30 31",m) #Saving # of days in months in variable  m
     yy = $1 + 0   #year from input file (1st argument of line 39)
     if (yy%4 == 0) m[2] = 29 
     dd = $2 + 0   #day of year from input file (2nd argument from line 39)
     mm = 1
     while (dd > m[mm]) {  #loop produces day and month
        dd -= m[mm]
        mm++ }
  #Generating year, day of year and hour that are needed in the input file
     dt1 = mktime(sprintf("%s %s %s %i 00 00", yy, mm, dd, $3))  #Generates time in seconds 
     dt2 = dt1 + 24*60*60   #Adds a day to time in seconds 
     dt3 = strftime("%Y", dt2)  #Produces year
     dt4 = strftime("%j", dt2)  #Produces day of year
     dt5 = strftime("%H", dt2)  #Produces hour of day (e.g. 16 for 16:00)

        printf ("%4i%4i%4i   0 # Start year, day, hour, minute\n", dt3, dt4, dt5 ) #prints date in the input file

  } else {
#Changing the end date
#  if ( NR == 40 && $1 != 0 ) {    #This logical expression makes sure to not change line if all values are 0 already.
    if ( NR == 41 && $1 != 0 ) {   
     split("31 28 31 30 31 30 31 31 30 31 30 31",m)
     yy = $1
     if (yy%4 == 0) m[2] = 29
     dd = $2
     mm = 1
     while (dd > m[mm]) {
        dd -= m[mm]
        mm++ }

     dt1 = mktime(sprintf("%4i %2i %2i %2i 00 00", yy, mm, dd, $3)) 
     dt2 = dt1 + 24*60*60
     dt3 = strftime("%Y", dt2)
     dt4 = strftime("%j", dt2)
     dt5 = strftime("%H", dt2)

        printf ("%4i%4i%4i   0 # Stop year, day, hour, minute\n", dt3, dt4, dt5 )
  } else { 
  print $0 }
}
}
