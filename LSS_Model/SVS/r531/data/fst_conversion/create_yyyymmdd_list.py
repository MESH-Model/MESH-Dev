#!/usr/bin/python
#
# file: create_yearmonth_list.py
#
# Description:
#
#   The script generates a list of strings in format YYYYMM corresponding
#   to the calendar months within the time period defined by two dates in
#   format YYYYMMDD passed as command line arguments.
#
# Usage:
#
#   create_yearmonth_list.py  firstDate  lastDate
#
#   where 'firstDate' and 'lastDate' are strings in YYYYMMDD format
#
#
# Author: Daniel Deacu (RPN, August 2010)
#
#
from datetime import date
from datetime import timedelta
#
import calendar 
#
import sys
#
#
#
def convertDate(date_str):
#
    """ Convert the date passed as a string in format 'YYYYMMDD'

        returns a 'date' object """
#

    return date(int(date_str[0:4]), int(date_str[4:6]), int(date_str[6:8]))
#
#
#
firstDate = convertDate(sys.argv[1])
lastDate  = convertDate(sys.argv[2])
#
#currentDate = firstDate.replace(day=1) # set the day of the first month to 1
currentDate = firstDate
previousDate = currentDate - timedelta(days=1)
outline = currentDate.strftime("%Y%m%d") + previousDate.strftime("%Y%m%d")

#
yearMonthdayList = [outline]
#
#
while currentDate < lastDate:
    currentDate += timedelta(days=1) # increment by one day
    previousDate = currentDate - timedelta(days=1)

    if currentDate > lastDate: break
    outline = currentDate.strftime("%Y%m%d") + previousDate.strftime("%Y%m%d")
    yearMonthdayList.append(outline)
#
for yyyymmdd in yearMonthdayList:
    print yyyymmdd


#
# end of script
