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
      write(varid,10)today,now,                                        
     1                trim(varname(varid)),                            
     2                trim(Projection),                                
     3                trim(Ellipsoid),                                 
     4                xOrigin,                                         
     5                yOrigin,                                         
     6                trim(attrname(varid)),                           
     7                trim(attrunit(varid)),                           
     8                xCount,                                          
     9                yCount,                                          
     1                xDelta,                                          
     2                yDelta
                 
10    format('########################################',/              
     1        ':FileType r2c  ASCII  EnSim 1.0',/                       
     2        '#',/                                                     
     3        '# DataType               2D Rect Cell',/                 
     4        '#',/                                                     
     5        ':Application             FORTRAN',/                      
     6        ':Version                 1.0.0',/                        
     7        ':WrittenBy               MSC/HAL',/                      
     8        ':CreationDate            ',                              
     9        i2.2,'/',i2.2,'/',i4,x,i2.2,':',i2.2,':',i2.2,/           
     1        '#',/                                                     
     2        '#---------------------------------------',/              
     3        '#',/                                                     
     4        ':Name                    ',A,/                           
     5        '#',/                                                     
     6        ':Projection              ',A,/                           
     7        ':Ellipsoid               ',A,/                           
     8        '#',/                                                     
     9        ':xOrigin                 ',f9.4,/                        
     1        ':yOrigin                 ',f9.4,/                        
     2        '#',/                                                     
     3        ':SourceFile              XXXX/           X',/            
     4        '#',/                                                     
     5        ':AttributeName           ',A,/                           
     6        ':AttributeUnit           ',A,/                           
     7        '#',/                                                     
     8        ':xCount                 ',i3,/                          
     9        ':yCount                 ',i3,/                          
     1        ':xDelta                  ',f9.6,/                        
     2        ':yDelta                  ',f9.6,/                        
     3        '#',/                                                     
     4        '#',/                                                     
     5        ':endHeader')
      return
      end
