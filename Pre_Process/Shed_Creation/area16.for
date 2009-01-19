      MODULE area16

        real, dimension(:),     allocatable :: smc,smcrag,xsta,ysta,
     *                                         temp,d,rsum,gsum,ratio,
     *                                         axx,ayy
        real, dimension(:,:),   allocatable :: p,psum,pmin,rrain,radp,
     *                                         sumr,cltr,f,f1,sto,g,sum,
     *	                                     smc_class
        real, dimension(:,:,:), allocatable :: w
        integer, dimension(:),  allocatable :: ntogo,ewg,sng,sngmin,
     *                                         ewgmin
        integer                             :: nw

        character(12), dimension(:),allocatable :: gname

      END MODULE area16




