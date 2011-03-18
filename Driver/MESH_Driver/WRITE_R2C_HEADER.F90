SUBROUTINE WRITE_R2C_HEADER(NMTEST,NR2C,NR2CFILES,GRD,GAT,GRDGAT,R2C_ATTRIBUTES, &
                            R2CFILEUNITSTART,NR2CSTATES,coordsys1,datum1,zone1,   &
                            XORIGIN,YORIGIN,XDELTA,YDELTA,XCOUNT,YCOUNT)

INTEGER      NMTEST,NR2C,NR2CFILES
INTEGER      GRD(NR2C),GAT(NR2C),GRDGAT(NR2C)
CHARACTER(*) R2C_ATTRIBUTES(NR2C,3)

INTEGER R2CFILEUNIT,R2CFILEUNITSTART,NR2CSTATES
CHARACTER*2 FN

character(10) coordsys1,datum1,zone1
real xorigin,yorigin,xdelta,ydelta
INTEGER XCOUNT,YCOUNT

!> CREATE A NEW FOLDER FOR R2C OUTPUT FILES
CALL SYSTEM('mkdir R2C_OUTPUT')

NR2CSTATES = 0
R2CFILEUNIT = R2CFILEUNITSTART
DO N = 1, NR2C
   IF(GRD(N)==1)THEN
      R2CFILEUNIT = R2CFILEUNIT + 1
      NR2CSTATES = NR2CSTATES + 1
      OPEN(R2CFILEUNIT,FILE='./R2C_OUTPUT/' // TRIM(R2C_ATTRIBUTES(N,3)) // '_GRD.r2c')
      CALL WRITE_HEADER(R2CFILEUNIT,R2C_ATTRIBUTES(N,:),0)
   ENDIF
   IF(GAT(N)==1)THEN
      DO J = 1, NMTEST
         R2CFILEUNIT = R2CFILEUNIT + 1
         NR2CSTATES = NR2CSTATES + 1
         WRITE(FN,'(I2)')J
         OPEN(R2CFILEUNIT,FILE='./R2C_OUTPUT/' // TRIM(R2C_ATTRIBUTES(N,3)) // '_GAT_CLASS_' // TRIM(ADJUSTL(FN)) // '.r2c')
         CALL WRITE_HEADER(R2CFILEUNIT,R2C_ATTRIBUTES(N,:),J)
      ENDDO
      
   ENDIF
   IF(GRDGAT(N)==1)THEN
      DO J = 1, NMTEST
         R2CFILEUNIT = R2CFILEUNIT + 1
         NR2CSTATES = NR2CSTATES + 1
         WRITE(FN,'(I2)')J
         OPEN(R2CFILEUNIT,FILE='./R2C_OUTPUT/' // TRIM(R2C_ATTRIBUTES(N,3)) // '_GRDGAT_CLASS_' // TRIM(ADJUSTL(FN)) // '.r2c')
         CALL WRITE_HEADER(R2CFILEUNIT,R2C_ATTRIBUTES(N,:),J)
      ENDDO
   ENDIF
ENDDO

END

subroutine write_header(un,varattr,nc)
!> Writes the header of R2C file

    use          area_watflood

    integer      un,nc
    character(*) varattr(3)

    character(10) ctime
    character(8)  cday
    
    write(un,3005)'########################################'
    write(un,3005)':FileType r2c  ASCII  EnSim 1.0         '
    write(un,3005)'#                                       '
    write(un,3005)'# DataType               2D Rect Cell   '
    write(un,3005)'#                                       '
    write(un,3005)':Application             EnSimHydrologic'
    write(un,3005)':Version                 2.1.23         '
    write(un,3020)':WrittenBy          ','MESH_DRIVER                             '
    call date_and_time(cday,ctime)
    write(un,3010)':CreationDate       ', &
       cday(1:4),cday(5:6),cday(7:8),ctime(1:2),ctime(3:4)
    3010  format(a20,a4,'-',a2,'-',a2,2x,a2,':',a2)
    write(un,3005)'#                                       '
    write(un,3005)'#---------------------------------------'
    write(un,3005)'#                                       '
    write(un,3020)':Name               ',varattr(3)
    write(un,3005)'#                                       '
    write(un,3004)':Projection         ',coordsys1
    if(coordsys_temp.eq.'LATLONG   ')then
    write(un,3004)':Ellipsoid          ',datum1
    endif
    if(coordsys_temp.eq.'UTM       ')then
    write(un,3004)':Ellipsoid          ',datum1
      write(un,3004)':Zone               ',zone1
    endif
    write(un,3005)'#                                       '
    write(un,3003)':xOrigin            ',xorigin
    write(un,3003)':yOrigin            ',yorigin
    write(un,3005)'#                                       '
    write(un,3005)':SourceFile            standalone MESH  '                                    
    write(un,3005)'#                                       '
    if(nc == 0)then
       write(un,3007)':AttributeName',1
    else
       write(un,3007)':AttributeName',1,' Class',nc
    endif
    write(un,3020)':AttributeUnits     ',varattr(2)  
    write(un,3005)'#                                       '
    write(un,3001)':xCount             ',xcount
    write(un,3001)':yCount             ',ycount
    write(un,3003)':xDelta             ',xdelta
    write(un,3003)':yDelta             ',ydelta
    write(un,3005)'#                                       '
    if(unit_conversion.ne.0.0)then
      write(un,3003)':UnitConverson      ',unit_conversion
    endif
    write(un,3005)'#                                       '
    write(un,3005)':endHeader                              '

return

 3000 format(a10,i5)
 3001 format(a20,i16)
 3002 format(2a20)
 3003 format(a20,f16.7)
 3004 format(a20,a10,2x,a10)
 3005 format(a40)
 3006 format(a3,a10)
 3007 format(a14,i5,a6,i5)
 3012 format(a9)
 3020 format(a20,a40)
 
      end
