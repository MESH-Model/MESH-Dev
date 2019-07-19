!This is a new file for MESH-CTEM which will read in the Nitrogen inputs form the .CTN
!First Created by Stefan Sauer July 2019

!DRAFT FILE, IS NOT YET COMPILED


!Previous CLASS-CTEMN+ had the read function inside runclass36ctem()
!Newer version of CLASS-CTEM will have seperate subroutines to read input files

!This current version will accept the same way for .CTM
!The reading will go over and repeat for each GRU



      SUBROUTINE nitrogen_read(nltest,nmtest,icc,iccp1, 
     1              rnleafrow, rnstemrow,
     2              rnrootrow, rnlitrrow, 
     3              rnsomrow, snh4row, sno3row, 
     4              et0, jmax, kl, km, kmin0, rtmass0, solnh4, 
     5              rnlf0, rnsm0, rnrt0, conreal, LAI0, krubn, 
     6              kn, kni0, kdn0, kv0, nbfix0, ndep0, nfer0, 
     7              nfero0)



      IMPLICIT NONE

              INTEGER nltest  !< Number of grids (formerly MOSAIC grids)
              INTEGER nmtest  !< Number of GRUs 
              INTEGER icc     !< Number of CTEM PFTs
              INTEGER iccp1   !< Number of CTEM PFTs plus Urban landuse
              INTEGER J, M
              character*80    TITLEC1

              real, dimension(nltest,nmtest,icc) :: rnleafrow
              real, dimension(nltest,nmtest,icc) :: rnstemrow
              real, dimension(nltest,nmtest,icc) :: rnrootrow
              real, dimension(nltest,nmtest,iccp1) :: rnlitrrow
              real, dimension(nltest,nmtest,iccp1) :: rnsomrow
              real, dimension(nltest,nmtest,iccp1) :: snh4row
              real, dimension(nltest,nmtest,iccp1) :: sno3row



              REAL et0      ! characteristic ET for N uptake [mm/s]
              REAL jmax     ! High affinity maximum rate of ion uptake [-]
              REAL kl       ! low affinity root ion uptake [-]
              REAL km       ! Mihalis Menten factor for 50% of maximum ion uptake rate [-]
              REAL kmin0    ! upper limit to kmin for adequate roots and ET [1/s]
              REAL rtmass0  ! reference root mass - now a constant [gC/m2]
              REAL solNH4   ! soluability of ammonium ion [-]
              REAL rnlf0    ! ideal structual N to C ratio for new leaves [-]
              REAL rnsm0    ! ideal N to C ratio in new stem tissue [-]
              REAL rnrt0    ! ideal N to C ratio in new root tissue [-]
              REAL conreal  ! reallocation of nitrogen coefficient [-]
              REAL LAI0     ! top LAI assuming same RubiscoN content [m2/m2]
              REAL krubn    ! canopy rubisco-nitrogen decay coefficient [-]
              REAL kn       ! canopy nitrogen decay coefficient [-]
              REAL kni0     ! max. NH4 nitrifiction rate [1/s]
              REAL kdn0     ! max. NO3 denitrifiction rate [1/s]
              REAL kv0      ! max. NH4 volitization rate [1/s]
              REAL nbfix0   ! N source by biofixation at reference conditions [gN/m2/s)
              REAL ndep0    ! N source by deposition [gN/m2/yr]
              REAL nfer0    ! N source by inorganic fertilization [gN/m2/yr]
              REAL nfero0   ! N source by organic fertilization [gN/m2/yr]


              !Open Nitrogen input file
         open(unit=15,file='MESH_input_nitrogen.CTN',status='old')



          DO M=1,NMTEST !Loop over every different GRU
          ! Read from the CTN file and save within the first tile
              READ(15,*) (RNLEAFROW(1,M,J),J=1,ICC)       
              READ(15,*) (RNSTEMROW(1,M,J),J=1,ICC)       
              READ(15,*) (RNROOTROW(1,M,J),J=1,ICC)       
              READ(15,*) (RNLITRROW(1,M,J),J=1,ICCP1)       
              READ(15,*) (RNSOMROW(1,M,J),J=1,ICCP1)       
              READ(15,*) (SNH4ROW(1,M,J),J=1,ICCP1) 
              READ(15,*) (SNO3ROW(1,M,J),J=1,ICCP1)       
          ENDDO

          !Now spread the inital values of the first grid to all other grids


            !Now read in the variables which are GRU and Grid independent
              READ(15,*) TITLEC1 !Dummy variable
              READ(15,*) et0     
              READ(15,*) jmax     
              READ(15,*) kl       
              READ(15,*) km       
              READ(15,*) kmin0    
              READ(15,*) rtmass0  
              READ(15,*) solNH4  
              READ(15,*) rnlf0    
              READ(15,*) rnsm0    
              READ(15,*) rnrt0    
              READ(15,*) conreal 
              READ(15,*) LAI0     
              READ(15,*) krubn   
              READ(15,*) kn       
              READ(15,*) kni0    
              READ(15,*) kdn0     
              READ(15,*) kv0     
              READ(15,*) nbfix0  
              READ(15,*) ndep0    
              READ(15,*) nfer0    
              READ(15,*) nfero0   




        END