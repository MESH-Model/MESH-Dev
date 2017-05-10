      subroutine gem_write_header(varid)

!     Writes the header of R2C file
!
      use gem_variable_attributes_module
      use grid_parameters_module
    
      implicit none

      integer       varid,today(3),now(3)
      external      idate, itime
    
!     Get today's data and time
      call idate(today)
      call itime(now)
    
!     Write header
      write(varid,10)today,now,                                        & 
                      trim(varname(varid)),                            &
                      trim(Projection),                                &
                      trim(Ellipsoid),                                 &
                      xOrigin,                                         &
                      yOrigin,                                         &
                      trim(attrname(varid)),                           &
                      trim(attrunit(varid)),                           &
                      xCount,                                          &
                      yCount,                                          &
                      xDelta,                                          &
                      yDelta
                 
10    format('########################################',/              &
              ':FileType r2c  ASCII  EnSim 1.0',/                      & 
              '#',/                                                    & 
              '# DataType               2D Rect Cell',/                & 
              '#',/                                                    & 
              ':Application             FORTRAN',/                     &
              ':Version                 1.0.0',/                       & 
              ':WrittenBy               MSC/HAL',/                     & 
              ':CreationDate            ',                             & 
              i2.2,'/',i2.2,'/',i4,x,i2.2,':',i2.2,':',i2.2,/          & 
              '#',/                                                    & 
              '#---------------------------------------',/             & 
              '#',/                                                    & 
              ':Name                    ',A,/                          & 
              '#',/                                                    & 
              ':Projection              ',A,/                          & 
              ':Ellipsoid               ',A,/                          & 
              '#',/                                                    & 
              ':xOrigin                 ',f9.4,/                       & 
              ':yOrigin                 ',f9.4,/                       & 
              '#',/                                                    & 
              ':SourceFile              XXXX/           X',/           & 
              '#',/                                                    & 
              ':AttributeName           ',A,/                          & 
              ':AttributeUnit           ',A,/                          & 
              '#',/                                                    & 
              ':xCount                 ',i3,/                          &
              ':yCount                 ',i3,/                          &
              ':xDelta                  ',f9.6,/                       &
              ':yDelta                  ',f9.6,/                       & 
              '#',/                                                    & 
              '#',/                                                    & 
              ':endHeader')
      return
      end
