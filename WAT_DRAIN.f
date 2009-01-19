	subroutine WAT_DRAIN(mode,H,h0,ztop,xdrainh,c,
     +   xslope,condAtSat,DD,thpor_avail,corrFactor,
     +   asat_t0,asat_t1)
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
c	Interfow can be determined by (asat_t0 - asat_t1)*H*thpor/delt
c	
c	Input variables:
c		asat_t0         average saturation at beginning of time step
c		c               Clapp-Hornberger connectivity index
c		H               layer thickness,(m)
c       xdrainh         fractional change in saturated horizontal conductivity in a depth change h0
c		h0              reference change in depth, normally 1 metre
c		ztop            depth to top of layer,(m)
c       xslope          Average overland slope 
c       DD              Drainage density (m/m^2)
c       thpor_avail     Soil porosity
c       condAtSat       Soil conductivity at saturation (m/s)
c       corrFactor      corrects condAtSat for ice and temperature effects (not mentioned in Notes on Interflow)
c		
c	Output variables:
c		asat_t1 - average saturation at the end of time step
c
c	Working variables:
c		asat - average layer saturation
c		asat_b - average layer saturation when seepage face is fully unsaturated
c		asat_c - average layer saturation when seepage face is fully saturated

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
c
c       no drainage for very thin layer
	  if(asat_t0 .le.1.0e-8 .or. xdrainh .le. 1.0e-8
     +  .or. h0 .le. 1.0e-8 .or. H .le. 1.0e-8)then
          asat_t1 = asat_t0
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
	  xldh = xlambda*H
c
c       transition saturations
        asat_c = 1.0-exav(xldh)/c
        asat_b = exav(xldh/cm1)/ccm1
c
c	  protecting against supesaturation
	  asat_t00=min(1.0,asat_t0)
c       point saturation at bottom right corner
        sat0 = min(1.0,asat_t00/asat_b)
c
c***************************************************************************************
c
c       finds bulk saturation (asat_t1) at end time step
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
c         find transition times:
c         t_c uses equation (17a) in Notes on Interflow
c         t_b uses equation (17b) in Notes on Interflow
c
c         Variables used:
c         Code            Common Name                       Notes on Interflow
c         - xslope          Average overland slope (unitless) <upper case lambda>
c         - DD              Drainage density (m/m^2)          1/L = 1/(hillslope legnth) = 2DD
c         - thpor_avail     Soil porosity (unitless)          <Theta>s
c         - condAtSat       Conductivity at saturation (m/s)  Ks
c         - condCorrFactor  corrects for ice and temperature effects (not discussed in Notes on Interflow)
c         - xLambda         decay coefficient (1/m)           <lower case lambda>
c         - zTop            depth of top of soil layer (m)    zTop

	    t_c = ((1+xslope**2)/xslope)
     +          *(thpor_avail/(2*DD*c*condAtSat*corrFactor))
     +          *exp(xLambda*zTop)

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
          
c         Use the NEWTON-RAPHSON method: find h such that asat(h) = asat_t0.
c         Then, we know h with which to find t0 = t_c*exp(h).

c         Here we use the algorithm described in "Notes on Interflow" under the
c         heading "Algorithm to estimate time as a function of bulk saturation
c         for the third case", and is explained under the previous heading "Third Case"
c         VARIABLES USED:
c         xlhs: lambda*h
c         delh: lambda*(H-h)
c         asat_cs -> Eqn. (27) with H = H-h
c         asat_bs -> Eqn. (31) with H = h
c         asat -> Eqn. (36)
c         ds_dxlhs: The derivative of asat w.r.t. xlhs -> Eqn. (37)/lambda
c         serrtol: convergence tolerance

            xlhs = xldh/ccm1  !Set h at the inflection piont H(c-1)/c
            do kount = 1,100
              delh = xldh-xlhs
	        asat_cs = 1.0-exav(delh)/c 
	        asat_bs = exav(xlhs/cm1)/ccm1
              asat = xlhs/xldh*asat_bs + delh/xldh*asat_cs
              serr = asat-asat_t0
              if(abs(serr).le.serrtol)goto 600 !if asat == asat_t0, quit
		      ds_dxlhs = (-1.0+exp(-delh)/c+exp(-xlhs/cm1)/ccm1)/xldh
              xlhs = xlhs-serr/ds_dxlhs ! New estimate of lambda*h
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
c         fully saturated seepage face: eqn(28), Notes on Interflow
          if(t1 .le. t_c)then
c
            asat_t1 = 1.0 - t1/t_c*(1.0-asat_c)
c
c         fully unsaturated seepage face: eqn(32), Notes on Interflow
          elseif(t1 .ge. t_b)then
c
            asat_t1 = asat_b*(t_b/t1)**(1.0/cm1)
c
c         partly saturated seepage face: eqn(36), Notes on Interflow
          else
c
c           ln(t1/t_c)=xlambda*h  -Notes on Interflow, eqn(34)
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
