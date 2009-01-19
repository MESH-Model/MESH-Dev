	subroutine WAT_DRAIN(mode,h,h0,ztop,xdrainh,c,grk_eff,
     +   dummy,bflow_t0,asat_t0,asat_t1)
c	
c	* April 20/07    - R. Soulis  Version 3.3.1.f
c                        et al      Version decided at UW
c								  convergence tolerance relaxed to ensure convergence
c                                     (serrtol from 1.0e-7 to 1.0e-5)
c                                   log of ratios changed to difference of logs
c								  set return values for no interflow solution
c
c	* March 23/07    - R. Soulis  Version 3.3.1.e
c								  convergence tolerance relaxed to ensure convergence
c                                     (serrtol from 1.0e-10 to 1.0e-7)
c								  initialisation separated for each mode
c                                   added mode flag to replace use of grk_eff for control
c								  mode = 0  baseflow factor at t0
c                                          1  interflow rates at t0
c								         2  saturation at end of time step
c                                          3  all of the above
c
c	* February 20/07 - R. Soulis  Version 3.3.1.d
c								  tolerances adjusted for minimum runtime
c								  see data statements
c
c	* February 14/07 - R. Soulis  Version 3.3.1.c
c								  zero grk_eff finds instantaneous flows at t0 only
c								  positive grk_eff finds average flows over delt
c
c	* February 13/07 - R. Soulis  Version 3.3.1.b
c								  correction to Newton-Raphson inflection point
c								  and reformat derivative
c								  function exav(x)=1.0-exp(-x)/x to deal with limits
c								  c factors for often used functions of c
c     
c	* February 6/07 - R. Soulis   Version 3.3.1.a
c								  calculate bottom saturation (bflow) and 
c		                          interflow (qflow) from a sloping aquifer
c	
c	This routine calculates the outflow from a tilted landscape element (tile).
c	Rather than use average saturation for the element(asat), it finds flow using 
c	the average value of bsat**c and asat**c where bsat and asat are the saturation
c	at the bottom of the layer and at the seepage face respectively. These are equivalent
c	to normalized baseflow and interflow.
c
c     Baseflow at time t0 can be determined using ks*bflow_t0*xdrainh.
c	Interfow can be determined by (asat_t0 - asat_t1)*h*thpor/delt
c	Interflow at time t0 can be determined by qflowt_t0*grk_eff*h.
c	
c	Input variables:
c		asat_t0 - average saturation at beginning of time step
c		c - Clapp-Hornberger connectivity index
c		grk_eff - ksat*modified tile slope/tile length/available porosity,(m/s)
c		h - layer thickness,(m)
c         xdrainh - fractional change in horizontal conductivity in a depth change h0
c		h0 - reference change in depth, normally 1 metre
c		ztop - depth to top of layer,(m)
c		
c	Output variables:
c		asat_t1 - average saturation at the end of time step
c		bflow_t0 - average effective saturation factor for bottom layer
c		           equal to <bsat**c>
c
c	Working variables:
c		asat - average layer saturation
c		asat_b - average layer saturation when seepage face is fully unsaturated
c		asat_c - average layer saturation when seepage face is fully saturated
c		bflow - saturation factor or normalized flow at bottom boundary
c		qflow - saturation factor or normalized flow at seepage face
c

      
	real dummy

        COMMON /CLASS1/ DELT,TFREZ


c       This is useful for testing - for example see drainmain.f
	  common/drncom/t0,t1,t_c,t_b,asat_c,asat_b,
     +    qflowu_t0,qflows_t0,qflowt_t0
c
c       error tolerance for storage calculations and maximum time to simulate flow
        data serrtol/1.0e-05/,timebig/1.0e+10/
c       data serrtol/1.0e-10/,timebig/1.0e+10/
c       data serrtol/1.0e-06/,timebig/1.0e+08/
c
c	  switches
        logical case_t0,case_t1,case_b,case_q,case_a


c       no drainage for very thin layer
	  if(asat_t0 .le.1.0e-8 .or. xdrainh .le. 1.0e-8
     +  .or. h0 .le. 1.0e-8 .or. h .le. 1.0e-8)then
          asat_t1 = asat_t0
          bflow_t0 = 0.0
	    qflowu_t0 = 0.0
		qflows_t0 = 0.0
		qflowt_t0 = 0.0
          return
	  endif
c
c       setup case switches
c       baseflow factor at t0
        case_b = mode.eq.0 .or. mode.eq.3
c       interflow factors at t0
	  case_q = mode.eq.1 .or. mode.eq.3
c       values at t0 only 
	  case_t0 = case_b .or. case_q
c       integrated values for delt
	  case_t1 = mode.eq.2 .or. mode.eq.3
c
c       c factors
        cm1 = c-1.0
	  ccm1 = c/cm1
        c2m1 = 2.0*c-1.0
c
c       xdrainh is fractional change in horizontal conductivity in a depth change h0
        xlh0 = -log(xdrainh)
        xlambda = xlh0/h0
	  xldh = xlambda*h
c
c       transition saturations
	  eps0 = exav(xldh)
        asat_c = 1.0-eps0/c     
        eps1 = exav(xldh/cm1)
        asat_b = eps1/ccm1
c
c	  protecting against supesaturation
	  asat_t00=min(1.0,asat_t0)
c       point saturation at bottom right corner
        sat0 = min(1.0,asat_t00/asat_b)
c
c*****************************************************************************************
c
c  part 1 - finds distribution of liquid water given bulk saturation (asat_t0)
c    at beginning of time step and uses it to find boundary conditions 
c    then determines effective bottom (bflow_t0)
c    and interflow (qflowt_t0) saturation factors
c
c*****************************************************************************************
c
        if(case_t0)then
c	  
c	    clear outputs
          bflow_t0= 0.0
          qflowt_t0 = 0.0
          qflowu_t0 = 0.0
	    qflows_t0 = 0.0
c		
c         determine flow factors for various saturation regimes
          bflow = 0.0 
          qflowu = 0.0
	    qflows = 0.0
          qflow_c = eps0
          qflow_b = exp(-xldh)*eps1
c
c         seepage face fully unsaturated
		if(asat_t0.le.asat_b)then
c
		  if(case_b)bflow = cm1/c2m1*(sat0**c)
            if(case_q)qflowu = (sat0**c)*qflow_b
c
c		bottom face is fully saturated
          elseif(asat_t0.ge.1.0)then
c
		  if(case_b)bflow = 1.0
	      if(case_q)qflows = qflow_c
c
c         seepage face fully saturated
          elseif(asat_t0.ge.asat_c)then
c
            if(case_b)
     +        bflow = 1.0-c/c2m1*(1.0-asat_t00)/(1.0-asat_c)*exp(-xldh)
	      if(case_q)qflows = qflow_c
c
c         find storage for partial saturation by newton-raphson
          else
c
            xlhs = xldh/ccm1
            do kount = 1,100
              delh = xldh-xlhs
	        asat_cs = delh/xldh*(1.0-exav(delh)/c)
			asat_bs = xlhs/xldh*exav(xlhs/cm1)/ccm1
              asat = asat_bs+asat_cs
              serr = asat-asat_t00
              if(abs(serr).le.serrtol)goto 500
			ds_dxlhs = (-1.0+exp(-delh)/c+exp(-xlhs/cm1)/ccm1)/xldh
              xlhs = xlhs-serr/ds_dxlhs
            enddo
c           calculate flow factors for partial saturation                         
500	      if(case_b)bflow = 1.0-c/c2m1*(exp(xlhs-xldh))
		  if(case_q)qflowu = xlhs/xldh*exp(-xlhs)*exav(xlhs/cm1)
	      if(case_q)qflows = delh/xldh*exp(-xlhs)*exav(delh)
c
	    endif ! if(asat_t0.le.asat_b)then
c
c         save bottom saturation at t0
	    bflow_t0 = max(0.0,bflow)
c
c         normalize and save lateral flows
          qflowu_t0  = qflowu/qflow_c
          qflows_t0  = qflows/qflow_c
          qflowt_t0  = qflowu_t0 + qflows_t0
c
        endif
c
c***************************************************************************************
c
c    part 2 - finds bulk saturation (asat_t1) at end time step
c
c***************************************************************************************
c	  
	  if (case_t1)then
c	  
c	    clear outputs
          asat_t1 = asat_t0
          t0 = 0.0
	    t1 = 0.0
c
c         adjust conductivities to top of current layer
          xlzt = xlambda*ztop
          grk_top = grk_eff*exp(-xlzt)
c
c         find transition times
	    t_c = 1.0/(c*grk_top)
	    t_b = t_c*exp(xldh) 
c
c         find theoretical start of recession (t0) for the current time step
          if(asat_t0.le.asat_b)then
c
	      t0 = t_b*(asat_b/asat_t0)**cm1
c
	    elseif(asat_t0.ge.asat_c)then
c
	      t0 = t_c*(1.0-asat_t0)/(1.0-asat_c)
c
          else
c
            xlhs = xldh/ccm1
            do kount = 1,100
              delh = xldh-xlhs
	        asat_cs = delh/xldh*(1.0-exav(delh)/c)
	        asat_bs = xlhs/xldh*exav(xlhs/cm1)/ccm1
              asat = asat_bs+asat_cs
              serr = asat-asat_t0
              if(abs(serr).le.serrtol)goto 600
		    ds_dxlhs = (-1.0+exp(-delh)/c+exp(-xlhs/cm1)/ccm1)/xldh
              xlhs = xlhs-serr/ds_dxlhs
            enddo
600         t0 = t_c*exp(xlhs)
c
          endif 
c
c         find end of time step 
          t1 = t0+delt
c         check for time to stop
	    if(t1.gt.timebig)return
c
c         find position on hydrograph and calulate saturation
c
c         fully saturated seepage face
          if(t1 .le. t_c)then
c
            asat_t1 = 1.0 - t1/t_c*(1.0-asat_c)
c
c         fully unsaturated seepage face
          elseif(t1 .ge. t_b)then
c
            asat_t1 = asat_b*(t_b/t1)**(1.0/cm1)
c
c         partly saturated seepage face
          else
c
            xlhs = log(t1/t_c)
            sat_eff1 = xlhs/xldh/ccm1*exav(xlhs/cm1)
            sat_eff2 = (xldh-xlhs)/xldh*(1.0-exav(xldh-xlhs)/c)
            asat_t1 = sat_eff1 + sat_eff2
c
          endif ! partly saturated seepage face
c
          asat_t1 = max(0.0,min(asat_t0,asat_t1))
		
	  endif

	  return

	  end 
c
c***************************************************************************************
c
c    function exav(x) - finds average of an exponential function exp(-x)
c                       over the interval [0,x]
c
c***************************************************************************************
c
        function exav(x)
c	  data exphuge/1.0e+9/,expbig/1.0e+8/,expsmall/1.0e-8/
	  data exphuge/1.0e+8/,expbig/1.0e+6/,expsmall/1.0e-6/
	  if(x.gt.exphuge)then
          exav = 0.0
        elseif(x.gt.expbig)then
          exav = 1.0/x
        elseif(x.gt.expsmall)then
          exav = (1.0-exp(-x))/x
        else
          exav = 1.0-x/2.0
        endif          
        return
        end
