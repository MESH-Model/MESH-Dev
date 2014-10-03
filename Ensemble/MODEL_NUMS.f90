!-------------------------------------------------------------------------------
! Generate model numbers
!-------------------------------------------------------------------------------
subroutine MODEL_NUMS

use MODELS, only : &
  ebalm,    &! energy balance solution model
  tem,      &! turbulent exchange model
  zmsm,     &! momentum-scalar ratio model
  bsm,      &! blowing snow model
  scfm,     &! snow cover fraction model
  fsdm,     &! fresh snow density model
  scm,      &! snow compaction model
  sam,      &! snow albedo decay model
  sntcm,    &! snow thermal conductivity model
  betam,     &! soil moisture beta model
  im,       &! infiltration model
  sotcm,    &! soil thermal conductivity model
  albslm,     &! soil albedo model
  lam,      &! local advection model
  Nmod       ! Number of models model

implicit none

integer :: &
  q,        &! Model number
  qebalm,   &! energy balance solution model number
  qtem,     &! turbulent exchange model number
  qzmsm,    &! momentum-scalar ratio model number
  qbsm,     &! blowing snow model number
  qscfm,    &! snow cover fraction model number
  qfsdm,    &! fresh snow density model number
  qscm,     &! snow compaction model number
  qsam,     &! snow albedo decay model number
  qsntcm,   &! snow thermal conductivity model number
  qbetam,    &! soil moisture beta model number
  qim,      &! infiltration model number
  qsotcm,   &! soil thermal conductivity model number
  qalbslm,    &! soil albedo model number
  !qsiim,    &! soil ice impedance model number
  qlam,     &! local advection model number
  i          ! counter
  
q = 1
do qebalm = 0, 1
do qtem = 0, 1
  do qzmsm = 0, 1
   do qbsm = 0, 1
    do qscfm = 0, 1
     do qfsdm = 0, 1
      do qscm = 0, 1
       do qsam = 0, 1
        do qsntcm = 0, 1
         do qbetam = 0, 1
          do qim = 0, 1
           do qsotcm = 0, 1
            do qalbslm = 0, 1
              do qlam = 0, 1
                 ebalm(q)=   qebalm
                 tem(q)  =   qtem
                 zmsm(q) =   qzmsm
                 bsm(q)  =   qbsm
                 scfm(q) =   qscfm
                 fsdm(q) =   qfsdm
                 scm(q)  =   qscm
                 sam(q)  =   qsam
                 sntcm(q)=   qsntcm
                 betam(q) =  qbetam
                 im(q)   =   qim
                 sotcm(q)=   qsotcm
                 albslm(q) = qalbslm
                 lam(q)  =   qlam
                 q = q + 1
              end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do
   end do
  end do
 end do
end do


open(unit=907,file="model_nums.csv")
 write(907,'("ebalm,tem,zmsm,bsm,scfm,fsdm,scm,sam,sntcm,betam,im,sotcm,albslm,lam")')
 do i=1,Nmod
  write(907, '(14(I1,","))') ebalm(i),tem(i),zmsm(i),bsm(i),scfm(i),fsdm(i),scm(i),&
                      sam(i),sntcm(i),betam(i),im(i),sotcm(i),albslm(i),lam(i)
 enddo
close(unit=907)

end subroutine MODEL_NUMS
