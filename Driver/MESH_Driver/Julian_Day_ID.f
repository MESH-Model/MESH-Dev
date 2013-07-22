        subroutine Julian_Day_ID (year,day,jday_id) 
        INTEGER :: Year,day, jday_id
        INTEGER :: TEMP1
! this is a test comment
        TEMP1= YEAR-1601
        jday_id = TEMP1*365+TEMP1/4+DAY 
        RETURN
         END SUBROUTINE Julian_Day_ID
