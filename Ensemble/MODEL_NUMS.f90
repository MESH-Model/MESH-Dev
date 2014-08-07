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
  slwm,     &! snow liquid water content model
  im,       &! infiltration model
  sotcm,    &! soil thermal conductivity model
  ufcm,     &! soil unfrozen water content model
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
  qslwm,    &! snow liquid water content model number
  qim,      &! infiltration model number
  qsotcm,   &! soil thermal conductivity model number
  qufcm,    &! soil unfrozen water content model number
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
         do qslwm = 0, 1
          do qim = 0, 1
           do qsotcm = 0, 1
            do qufcm = 0, 1
              do qlam = 0, 1
                 ebalm(q)= qebalm!HOLD TO 1
                 tem(q)  = qtem
                 zmsm(q) = qzmsm
                 bsm(q)  = qbsm
                 scfm(q) = qscfm !does not fix KEN
                 fsdm(q) = qfsdm
                 scm(q)  = qscm
                 sam(q)  = qsam
                 sntcm(q)= qsntcm!HOLD TO 1
                 slwm(q) = qslwm
                 im(q)   = qim !***PROBLEM IS im(q)=1?
                 sotcm(q)= qsotcm
                 ufcm(q) = qufcm
                 lam(q)  = qlam
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
 write(907,'("ebalm,tem,zmsm,bsm,scfm,fsdm,scm,sam,sntcm,slwm,im,sotcm,ufcm,lam")')
 do i=1,Nmod
  write(907, '(14(I1,","))') ebalm(i),tem(i),zmsm(i),bsm(i),scfm(i),fsdm(i),scm(i),&
                      sam(i),sntcm(i),slwm(i),im(i),sotcm(i),ufcm(i),lam(i)
 enddo
close(unit=907)

end subroutine MODEL_NUMS
