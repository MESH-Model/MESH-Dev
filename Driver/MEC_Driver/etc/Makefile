
.SUFFIXES :

.SUFFIXES : .cdk90 .ftn .ftn90 .ptn .f .f90 .c .s .o

SHELL = /bin/sh

CPP = /lib/cpp

COMPILE = compile

DEFINE =  

FFLAGS =  

CFLAGS = -I$(ARMNLIB)/include

CPPFLAGS = -I$(ARMNLIB)/include

ASFLAGS =  

DOC =

LABASE = base

MALIB =  malib.a

IGNORE_ERRORS = set -e

MAKE = $(IGNORE_ERRORS) ; make ARCH=$(ARCH)

AR = r.ar -arch $(ARCH)

.PRECIOUS: $(MALIB)
.DEFAULT:
	collectivisme_101 $@

default: make_menu


RCOMPIL = r.compile
RBUILD  = r.build
FCOMPF = 
CCOMPF =
COMPF = 
FC = $(RCOMPIL) -arch $(ARCH) -abi $(ABI)  -defines "=$(DEFINE)" -O $(OPTIL) -optf="$(FFLAGS)" -bindir $(LCLPO) $(COMPF) $(FCOMPF) -src
CC = $(RCOMPIL) -arch $(ARCH) -abi $(ABI)  -defines "=$(DEFINE)" -O $(OPTIL) -optc="$(CFLAGS)" -bindir $(LCLPO) $(COMPF) $(CCOMPF) -src
FTNC = $(RCOMPIL) -arch $(ARCH) -abi $(ABI)  -defines "=$(DEFINE)" -optf="$(FFLAGS) $(CPPFLAGS)" -P $(COMPF) $(FCOMPF) -src
PTNC = sed 's/^[[:blank:]].*PROGRAM /      SUBROUTINE /' | sed 's/^[[:blank:]].*program /      subroutine /'  > $*.f

.ptn.o:
	rm -f $*.f
	$(FC) $< 
	
.ptn.f:
	rm -f $*.f
	$(FTNC) $<

.ftn.o:
	rm -f $*.f
	$(FC) $<
.ftn.f:
	rm -f $*.f
	$(FTNC) $<
.f90.o:
	$(FC) $<
.f.o:
	$(FC) $<
.ftn90.o:
	rm -f $*.f90
	$(FC) $<
.cdk90.o:
	$(FC) $<
.cdk90.f90:
	rm -f $*.f90
	$(FTNC) $<
.ftn90.f90:
	rm -f $*.f90
	$(FTNC) $<

.c.o:
	$(CC) $<

.s.o:
	$(AS) -c $(CPPFLAGS) $(ASFLAGS) $<

% :: RCS/%,v
	collectivisme_101 $@

FTNDECKS= \
         absdvdz3.ftn       acqui.ftn       adilwc.ftn       adw_cfl_lam.ftn \
         adw_ckbd.ftn       adw_ckbd_lam.ftn       adw_exch_1.ftn       adw_exch_2.ftn \
         adw_exch_3.ftn       adw_interp.ftn       adw_interp2.ftn       adw_main.ftn \
         adw_main_1_wnd.ftn       adw_main_2_pos.ftn       adw_main_3_int.ftn       adw_main_3_intlag.ftn \
         adw_pol0.ftn       adw_pols.ftn       adw_polw.ftn       adw_polx.ftn \
         adw_set.ftn       adw_setint.ftn       adw_trajex.ftn       adw_trajsp.ftn \
         adw_tricub.ftn       adw_tricub_lag3d.ftn       adw_trilin.ftn       adw_trilin_turbo.ftn \
         adw_vder.ftn       aggcovernat.ftn       aggcoverurb.ftn       aggveg.ftn \
         agrege1.ftn       agvgclas.ftn       agvgclas2.ftn       agvgmask.ftn \
         agvgmask2.ftn       agvgmos.ftn       atmflux.ftn       bac.ftn \
         bacp_2.ftn       baktotq2.ftn       bcastc1.ftn       bin2com.ftn \
         blcloud2.ftn       blg2.ftn       blkcol.ftn       blocstat.ftn \
         blweight2.ftn       bmf_perturb.ftn       bourge1.ftn       bubble.ftn \
         bubble_cfg.ftn       c_bloccplg.ftn       c_cplg_iml.ftn       c_cplg_surf.ftn \
         c_fillbus.ftn       c_finddim.ftn       c_getbus.ftn       c_init.ftn \
         c_init_cplfld.ftn       c_init_oasisfld.ftn       c_mtlmap.ftn       c_screen.ftn \
         c_set_coupling.ftn       c_stop_coupling.ftn       calcalb1.ftn       calccovf.ftn \
         calcdiag.ftn       calcz0.ftn       calddqr.ftn       caldiv_2.ftn \
         calomeg.ftn       calvor.ftn       calzz.ftn       cartall.ftn \
         check_parti.ftn       class300.ftn       class320.ftn       class330.ftn \
         class_const.ftn       cldoptx4.ftn       cldwin.ftn       climphs3.ftn \
         cllaij.ftn       clsgs2.ftn       co2info.ftn       coherence.ftn \
         condload.ftn       conds.ftn       conray2.ftn       conres1.ftn \
         consun1.ftn       copybus.ftn       coriol8.ftn       ctmdiag.ftn \
         datf2p.ftn       datp2f.ftn       diagno_cw_rad.ftn       diagnosurf.ftn \
         diasurf2.ftn       difuvd1.ftn       difuvd2.ftn       difuvd4.ftn \
         difuvd4n.ftn       difuvd8.ftn       difuvd9.ftn       difuvdf.ftn \
         difuvdfj.ftn       difuvt.ftn       difvef.ftn       difver6.ftn \
         digflt.ftn       doout.ftn       drag2.ftn       dtfrz.ftn \
         dtfrznew.ftn       dumpbus2.ftn       dvrtdf.ftn       dvrtef.ftn \
         dynstep.ftn       e_arak.ftn       e_arret.ftn       e_bmfrd.ftn \
         e_bmfsplitxy2.ftn       e_coefdgf.ftn       e_dateo.ftn       e_decal.ftn \
         e_dynfld.ftn       e_filtopx.ftn       e_findmod.ftn       e_gemnml.ftn \
         e_gemntr.ftn       e_geopfld.ftn       e_geopini.ftn       e_grid.ftn \
         e_infiles.ftn       e_int_ix2xu.ftn       e_int_iy2yv.ftn       e_intgeo.ftn \
         e_intpoly.ftn       e_intthm.ftn       e_intwind.ftn       e_liaccu.ftn \
         e_moclim2.ftn       e_modgl.ftn       e_modhs.ftn       e_modmg.ftn \
         e_nml2bin.ftn       e_ntrxyfil.ftn       e_pilotf.ftn       e_pregeo.ftn \
         e_premoy.ftn       e_pretopo.ftn       e_rdhint3.ftn       e_screen.ftn \
         e_setdgf.ftn       e_setintuv.ftn       e_setinuvl.ftn       e_sgrid.ftn \
         e_sortr.ftn       e_specanal.ftn       e_trinit.ftn       e_wlogntr.ftn \
         e_wrgeop.ftn       ebudget.ftn       ecmwf_ab.ftn       ecris_fst2.ftn \
         ecris_fst2m.ftn       emicrog.ftn       envirtht.ftn       equivmount.ftn \
         eturbl6.ftn       extdiag.ftn       fcpara2.ftn       fcrest5.ftn \
         feseri.ftn       ficemxp.ftn       fillagg.ftn       filter.ftn \
         flxsurf3.ftn       fomichev.ftn       force_restore.ftn       frstgss.ftn \
         ftridi6.ftn       ftridi7.ftn       gather.ftn       gefstop.ftn \
         geleyn.ftn       gem_ctrl.ftn       gem_run.ftn       gem_stop.ftn \
         gemdm.ftn       gemtim.ftn       genab.ftn       genab2.ftn \
         geneigl.ftn       geophy.ftn       gesdict.ftn       getbpm.ftn \
         getbus1.ftn       getindx.ftn       getp0.ftn       glaciers1.ftn \
         glb_bound.ftn       glbcolc.ftn       glbdist.ftn       glbpos.ftn \
         glbstat.ftn       gwd5.ftn       gwdfx95a.ftn       gz2p0.ftn \
         gzcalcul.ftn       hatoprg.ftn       hdif0.ftn       hdif_phy.ftn \
         heatcoef.ftn       height_sponge.ftn       horwavg.ftn       hspng_abc.ftn \
         hspng_drv.ftn       hspng_main.ftn       hydro1.ftn       hzd_abc.ftn \
         hzd_areal.ftn       hzd_areav.ftn       hzd_bfct.ftn       hzd_bfct_1d.ftn \
         hzd_del2.ftn       hzd_delpwr.ftn       hzd_delpwr_1d.ftn       hzd_exhrdif.ftn \
         hzd_fact.ftn       hzd_ho.ftn       hzd_ho_parite.ftn       hzd_hof.ftn \
         hzd_hof_lam.ftn       hzd_hoffld.ftn       hzd_main.ftn       hzd_nudeln.ftn \
         hzd_set.ftn       hzd_set_ops8.ftn       hzd_solfft.ftn       hzd_solfft_1d.ftn \
         hzd_solfft_lam.ftn       hzd_solmxma.ftn       hzd_solparite_2.ftn       hzd_solparite_u.ftn \
         incdatsd.ftn       inctphy.ftn       inctphy3.ftn       indata.ftn \
         indseri.ftn       inichamp2.ftn       inicover.ftn       inicoverurb.ftn \
         inifcp.ftn       inikey.ftn       inimos.ftn       iniobus.ftn \
         iniptsurf.ftn       iniras.ftn       inisoili.ftn       inisurf1.ftn \
         initial.ftn       initw2.ftn       iniveg.ftn       intchamps.ftn \
         integ2.ftn       interpveg.ftn       intodif.ftn       intozon.ftn \
         intwat3.ftn       inverse.ftn       ipig.ftn       isba2.ftn \
         juliand.ftn       kdifsimp.ftn       kfcp4.ftn       ktrsnt3.ftn \
         ktrsnt_mg.ftn       kuo2.ftn       kuostd.ftn       kuosun.ftn \
         kuosym.ftn       lacs.ftn       lacs1.ftn       liebman_2.ftn \
         lin_adjtq.ftn       lin_adjtq_ad.ftn       lin_adjtq_tl.ftn       lin_conds1.ftn \
         lin_conds1_ad.ftn       lin_conds1_tl.ftn       lin_diff_vert1.ftn       lin_diff_vert1_ad.ftn \
         lin_diff_vert1_tl.ftn       lin_difuvd1.ftn       lin_difuvd1_ad.ftn       lin_difuvd1_tl.ftn \
         lin_difuvd2.ftn       lin_difuvd2_ad.ftn       lin_difuvd2_tl.ftn       lin_difver1.ftn \
         lin_difver1_ad.ftn       lin_difver1_tl.ftn       lin_inichamp1.ftn       lin_inichamp1_tl.ftn \
         lin_kdif_simp1.ftn       lin_kdif_simp1_ad.ftn       lin_kdif_simp1_tl.ftn       lin_kdif_simp1_tr.ftn \
         lin_kuosym.ftn       lin_kuosym_ad.ftn       lin_kuosym_tl.ftn       lin_phyexe1.ftn \
         lin_phyexe1_ad.ftn       lin_phyexe1_tl.ftn       lin_precip1.ftn       lin_precip1_ad.ftn \
         lin_precip1_tl.ftn       lin_sgo1.ftn       lin_sgo1_ad.ftn       lin_sgo1_tl.ftn \
         lin_sgoflx1.ftn       lin_sgoflx1_ad.ftn       lin_sgoflx1_tl.ftn       liqwc.ftn \
         litblrad2.ftn       litozon2.ftn       llacar.ftn       lsctrol.ftn \
         mconadj2.ftn       mesahr.ftn       mesahu.ftn       mfdlesmx.ftn \
         mfodla.ftn       mfodle.ftn       mfodqa.ftn       mfodqs.ftn \
         mfoefq.ftn       mfoeic.ftn       mfoew.ftn       mfoewa.ftn \
         mfohr.ftn       mfohra.ftn       mfoqfe.ftn       mfoqsa.ftn \
         mfoqst.ftn       mfottv.ftn       mfottv_ad.ftn       mfottv_tl.ftn \
         mfottvh.ftn       mfotvht.ftn       mfotvt.ftn       mfotvt_ad.ftn \
         mfotvt_tl.ftn       mgsdict.ftn       mhraes.ftn       mhrahu.ftn \
         mhuaes.ftn       mhuahr.ftn       mirror.ftn       mixlen3.ftn \
         mixphase6.ftn       modhui.ftn       modpbus.ftn       moistke3.ftn \
         momcoef.ftn       mosavg.ftn       moscopy.ftn       moscover.ftn \
         movcar.ftn       mprecip4.ftn       mras0.ftn       msoltri6.ftn \
         mthtaw.ftn       mthtaw2.ftn       mtn_case.ftn       mtn_cfg.ftn \
         nes1s.ftn       nes2s.ftn       nes3s.ftn       nes4s.ftn \
         nesajr.ftn       nest_indata.ftn       nest_init.ftn       nest_intt.ftn \
         nestit.ftn       newrad4.ftn       ngfft.ftn       nli.ftn \
         nlip_2.ftn       ntables.ftn       nuages2.ftn       oldrad3.ftn \
         opinv6.ftn       opinv7.ftn       optriss6.ftn       optriss7.ftn \
         out_dq.ftn       out_dyn.ftn       out_padbuf.ftn       out_phy.ftn \
         out_qc0.ftn       out_sblock.ftn       out_sfile.ftn       out_sgrid.ftn \
         out_slev.ftn       out_thm.ftn       out_tracer.ftn       out_uv.ftn \
         out_vmm.ftn       ouvrstrt.ftn       ozoref2.ftn       p0vt2gz_hyb.ftn \
         p_apply.ftn       p_config.ftn       p_fcpfw.ftn       p_fillbus.ftn \
         p_inincr.ftn       p_init.ftn       p_main.ftn       p_phyexe.ftn \
         p_physlb.ftn       p_phystep.ftn       p_psetiw.ftn       p_radiaf.ftn \
         p_set.ftn       p_uvgridscal.ftn       p_vlsp.ftn       p_vmmphy.ftn \
         pdfcdate.ftn       pdfjdate.ftn       pdfjdate2.ftn       phy_ini.ftn \
         phybus_data.ftn       phycom.ftn       phydebu4.ftn       phyexe1.ftn \
         phyopt.ftn       pllwfgfw.ftn       pnm2.ftn       pntg123.ftn \
         pntozon.ftn       pospers.ftn       pre.ftn       predat.ftn \
         prelim1.ftn       prep_2.ftn       prep_cw.ftn       prep_cw_rad.ftn \
         prep_index.ftn       prepoic.ftn       prever.ftn       prgen.ftn \
         prgzvta.ftn       prism_def_var_proto.ftn       prism_enddef_proto.ftn       prism_terminate_proto.ftn \
         prodef.ftn       prof5.ftn       prsdate.ftn       qcfft8.ftn \
         qcsgs.ftn       qozon3.ftn       raddel.ftn       radfac3.ftn \
         radiaf_o.ftn       radir8.ftn       radix.ftn       rasol.ftn \
         raysol.ftn       rdmax.ftn       rdmoy.ftn       rdradf.ftn \
         rdrstrt.ftn       readdyn.ftn       readgeo.ftn       readlam.ftn \
         rhs.ftn       rhsp_2.ftn       riflux.ftn       rigrad1.ftn \
         satq.ftn       save_options.ftn       scatter.ftn       schal.ftn \
         seaice1.ftn       secajus.ftn       seralc2.ftn       serallc2.ftn \
         serdata.ftn       serdbu.ftn       serdim.ftn       serecri.ftn \
         serfin.ftn       sergdim.ftn       sergini.ftn       sergoff.ftn \
         sergout.ftn       sergset.ftn       serie.ftn       serkh.ftn \
         serpat2.ftn       serset.ftn       sersetc.ftn       sersetm.ftn \
         serwrit.ftn       serwrit2.ftn       serxst.ftn       sesahr.ftn \
         sesahu.ftn       set_class_const.ftn       set_class_dt.ftn       set_cn1.ftn \
         set_cori.ftn       set_crni.ftn       set_dcst.ftn       set_dcst_8.ftn \
         set_dia.ftn       set_dync.ftn       set_fft.ftn       set_filt.ftn \
         set_geof.ftn       set_geom.ftn       set_grid.ftn       set_intuv.ftn \
         set_level.ftn       set_list.ftn       set_mem.ftn       set_nest.ftn \
         set_opr.ftn       set_oprz.ftn       set_ops8.ftn       set_poic.ftn \
         set_poic_par.ftn       set_poic_par_u.ftn       set_pois.ftn       set_rhs.ftn \
         set_schm.ftn       set_sor.ftn       set_step.ftn       set_term.ftn \
         set_transpose.ftn       set_trig21.ftn       set_var.ftn       set_vt.ftn \
         set_vta.ftn       set_world_view.ftn       set_world_view1.ftn       set_xnbit.ftn \
         set_xst.ftn       setdsr2.ftn       setscqr.ftn       setsd.ftn \
         settriv.ftn       setvis2.ftn       sfcbus_data.ftn       sfltr.ftn \
         sfodla.ftn       sfodle.ftn       sfodqa.ftn       sfodqs.ftn \
         sfoefq.ftn       sfoew.ftn       sfoewa.ftn       sfohr.ftn \
         sfohra.ftn       sfols.ftn       sfolv.ftn       sfopop.ftn \
         sfopot.ftn       sfoqfe.ftn       sfoqsa.ftn       sfoqst.ftn \
         sfottv.ftn       sfotvt.ftn       sgamash.ftn       sgamasp.ftn \
         sgamatd.ftn       sgoflx2.ftn       sgoflx3.ftn       shallconv3.ftn \
         shalow.ftn       shraes.ftn       shrahu.ftn       shuaes.ftn \
         shuahr.ftn       shuffle.ftn       skocon.ftn       slabsym.ftn \
         slfun_tq.ftn       slfun_uv.ftn       sltop_tq.ftn       sltop_uv.ftn \
         soili.ftn       sol_abc.ftn       sol_fft8_2.ftn       sol_fft8_lam.ftn \
         sol_hcr.ftn       sol_main.ftn       sol_mxma8_2.ftn       sol_parite_2.ftn \
         sol_trsf.ftn       splitst.ftn       srequet.ftn       statf_dm.ftn \
         statfld.ftn       statfld_old.ftn       status_file.ftn       statvps1.ftn \
         stetae.ftn       stfrst4.ftn       sthtaw.ftn       sthtaw2.ftn \
         stop_world_view.ftn       stopmpi.ftn       stretch_axis2.ftn       sttlcl.ftn \
         sun7.ftn       sun_radfix1.ftn       suncos.ftn       surf_precip.ftn \
         surface.ftn       surfix.ftn       surfradcloud.ftn       t02t1.ftn \
         t12t2.ftn       t22t0.ftn       ta2t1tx.ftn       theo_cfg.ftn \
         thermco2.ftn       tkealg.ftn       tmg_init0.ftn       tmg_start0.ftn \
         tmg_stop0.ftn       tmg_terminate0.ftn       tp.ftn       tpdd.ftn \
         tpmix.ftn       tr_init.ftn       tr_vint.ftn       tracers.ftn \
         trans_clean.ftn       trapez2.ftn       tstpdyn.ftn       tttt.ftn \
         turbul.ftn       update.ftn       update3.ftn       utco2.ftn \
         uv2tdpsd.ftn       uv_acg2g.ftn       var_topo.ftn       vco2inf2.ftn \
         vdfds.ftn       vegi.ftn       verder.ftn       vertdiff.ftn \
         vgathr.ftn       vkuocon6.ftn       vrtd.ftn       vspng_abc.ftn \
         vspng_del2.ftn       vspng_drv.ftn       vspng_drv_lam.ftn       vspng_lam.ftn \
         vspng_main.ftn       vspng_set.ftn       vtap.ftn       vte_hyb2hyb.ftn \
         vte_intvertx.ftn       vte_uv2img.ftn       vte_vrtical.ftn       vterp1.ftn \
         vterp2.ftn       water.ftn       wdpers.ftn       wetcon2.ftn \
         wflux.ftn       wlog.ftn       wndth.ftn       writlam.ftn \
         wrrstrt.ftn       wrvref.ftn       xyfil.ftn       zeracc.ftn \
         zongini.ftn       zongoff.ftn       zongopr.ftn

FTN90DECKS= \
         allocate_gr_snow.ftn90       averaged_albedo_teb.ftn90       averaged_tsrad_teb.ftn90       bld_e_budget.ftn90 \
         c_oasis_get.ftn90       c_oasis_init.ftn90       c_oasis_partition.ftn90       c_oasis_put.ftn90 \
         coef_ver_interp_lin2d.ftn90       coupling_teb.ftn90       coupling_teb_orography.ftn90       default_prep_teb.ftn90 \
         default_teb.ftn90       forcing_vert_shift.ftn90       get_luout.ftn90       hor_interpol.ftn90 \
         ini_csts.ftn90       init_snow_lw.ftn90       init_teb.ftn90       initown.ftn90 \
         interp_grid1d.ftn90       interp_grid2d.ftn90       mnhget_luout.ftn90       possol.ftn90 \
         prep_hor_snow_field.ftn90       prep_hor_snow_fields.ftn90       prep_hor_teb_field.ftn90       prep_snow_gem.ftn90 \
         prep_teb.ftn90       prep_teb_gem.ftn90       road_wall_layer_e_budget.ftn90       roof_layer_e_budget.ftn90 \
         snow_cover_1layer.ftn90       snow_heat_to_t_wliq.ftn90       sunpos.ftn90       surface_aero_cond.ftn90 \
         surface_cd.ftn90       surface_ri.ftn90       teb.ftn90       town.ftn90 \
         tridiag_ground.ftn90       urban_drag.ftn90       urban_fluxes.ftn90       urban_hydro.ftn90 \
         urban_lw_coef.ftn90       urban_snow_evol.ftn90       urban_solar_abs.ftn90       vegtype_grid_to_patch_grid.ftn90 \
         vegtype_to_patch.ftn90       ver_interp_lin2d.ftn90

FDECKS= \
         APREP.f       CANADD.f       CANALB.f       CANVAP.f \
         CGROW.f       CHKWAT.f       CLASSA.f       CLASSB.f \
         CLASSBD.f       CLASSD.f       CLASSG.f       CLASSI.f \
         CLASSS.f       CLASST.f       CLASSW.f       CLASSZ.f \
         CWCALC.f       DIASURF.f       DRCOEF.f       FLXSURFZ.f \
         GATPREP.f       GRALB.f       GRDRAN.f       GRINFL.f \
         ICEBAL.f       SLDIAG.f       SNINFL.f       SNOADD.f \
         SNOALBA.f       SNOALBW.f       SNOVAP.f       SUBCAN.f \
         TFREEZ.f       TMCALC.f       TMELT.f       TNPOST.f \
         TNPREP.f       TPREP.f       TSOLVC.f       TSOLVE.f \
         TSPOST.f       TSPREP.f       TWCALC.f       WEND.f \
         WFILL.f       WFLOW.f       WPREP.f       XIT.f \
         absdvdz3.f       acqui.f       adilwc.f       adw_cfl_lam.f \
         adw_ckbd.f       adw_ckbd_lam.f       adw_exch_1.f       adw_exch_2.f \
         adw_exch_3.f       adw_interp.f       adw_interp2.f       adw_main.f \
         adw_main_1_wnd.f       adw_main_2_pos.f       adw_main_3_int.f       adw_main_3_intlag.f \
         adw_pol0.f       adw_pols.f       adw_polw.f       adw_polx.f \
         adw_set.f       adw_setint.f       adw_trajex.f       adw_trajsp.f \
         adw_tricub.f       adw_tricub_lag3d.f       adw_trilin.f       adw_trilin_turbo.f \
         adw_vder.f       aggcovernat.f       aggcoverurb.f       aggveg.f \
         agrege1.f       agvgclas.f       agvgclas2.f       agvgmask.f \
         agvgmask2.f       agvgmos.f       atmflux.f       bac.f \
         bacp_2.f       baktotq2.f       bcastc1.f       bin2com.f \
         blcloud2.f       blg2.f       blkcol.f       blocstat.f \
         blweight2.f       bmf_perturb.f       bourge1.f       bubble.f \
         bubble_cfg.f       c_bloccplg.f       c_cplg_iml.f       c_cplg_surf.f \
         c_fillbus.f       c_finddim.f       c_getbus.f       c_init.f \
         c_init_cplfld.f       c_init_oasisfld.f       c_mtlmap.f       c_screen.f \
         c_set_coupling.f       c_stop_coupling.f       calcalb1.f       calccovf.f \
         calcdiag.f       calcz0.f       calddqr.f       caldiv_2.f \
         calomeg.f       calvor.f       calzz.f       cartall.f \
         check_parti.f       class300.f       class320.f       class330.f \
         class_const.f       cldoptx4.f       cldwin.f       climphs3.f \
         cllaij.f       clsgs2.f       co2info.f       coherence.f \
         condload.f       conds.f       conray2.f       conres1.f \
         consun1.f       copybus.f       coriol8.f       ctmdiag.f \
         datf2p.f       datp2f.f       diagno_cw_rad.f       diagnosurf.f \
         diasurf2.f       difuvd1.f       difuvd2.f       difuvd4.f \
         difuvd4n.f       difuvd8.f       difuvd9.f       difuvdf.f \
         difuvdfj.f       difuvt.f       difvef.f       difver6.f \
         digflt.f       doout.f       drag2.f       dtfrz.f \
         dtfrznew.f       dumpbus2.f       dvrtdf.f       dvrtef.f \
         dynstep.f       e_arak.f       e_arret.f       e_bmfrd.f \
         e_bmfsplitxy2.f       e_coefdgf.f       e_dateo.f       e_decal.f \
         e_dynfld.f       e_filtopx.f       e_findmod.f       e_gemnml.f \
         e_gemntr.f       e_geopfld.f       e_geopini.f       e_grid.f \
         e_infiles.f       e_int_ix2xu.f       e_int_iy2yv.f       e_intgeo.f \
         e_intpoly.f       e_intthm.f       e_intwind.f       e_liaccu.f \
         e_moclim2.f       e_modgl.f       e_modhs.f       e_modmg.f \
         e_nml2bin.f       e_ntrxyfil.f       e_pilotf.f       e_pregeo.f \
         e_premoy.f       e_pretopo.f       e_rdhint3.f       e_screen.f \
         e_setdgf.f       e_setintuv.f       e_setinuvl.f       e_sgrid.f \
         e_sortr.f       e_specanal.f       e_trinit.f       e_wlogntr.f \
         e_wrgeop.f       ebudget.f       ecmwf_ab.f       ecris_fst2.f \
         ecris_fst2m.f       emicrog.f       envirtht.f       equivmount.f \
         eturbl6.f       extdiag.f       fcpara2.f       fcrest5.f \
         feseri.f       ficemxp.f       fillagg.f       filter.f \
         flxsurf3.f       fomichev.f       force_restore.f       frstgss.f \
         ftridi6.f       ftridi7.f       gather.f       gefstop.f \
         geleyn.f       gem_ctrl.f       gem_run.f       gem_stop.f \
         gemdm.f       gemtim.f       genab.f       genab2.f \
         geneigl.f       geophy.f       gesdict.f       getbpm.f \
         getbus1.f       getindx.f       getp0.f       glaciers1.f \
         glb_bound.f       glbcolc.f       glbdist.f       glbpos.f \
         glbstat.f       gwd5.f       gwdfx95a.f       gz2p0.f \
         gzcalcul.f       hatoprg.f       hdif0.f       hdif_phy.f \
         heatcoef.f       height_sponge.f       horwavg.f       hspng_abc.f \
         hspng_drv.f       hspng_main.f       hydro1.f       hzd_abc.f \
         hzd_areal.f       hzd_areav.f       hzd_bfct.f       hzd_bfct_1d.f \
         hzd_del2.f       hzd_delpwr.f       hzd_delpwr_1d.f       hzd_exhrdif.f \
         hzd_fact.f       hzd_ho.f       hzd_ho_parite.f       hzd_hof.f \
         hzd_hof_lam.f       hzd_hoffld.f       hzd_main.f       hzd_nudeln.f \
         hzd_set.f       hzd_set_ops8.f       hzd_solfft.f       hzd_solfft_1d.f \
         hzd_solfft_lam.f       hzd_solmxma.f       hzd_solparite_2.f       hzd_solparite_u.f \
         incdatsd.f       inctphy.f       inctphy3.f       indata.f \
         indseri.f       inichamp2.f       inicover.f       inicoverurb.f \
         inifcp.f       inikey.f       inimos.f       iniobus.f \
         iniptsurf.f       iniras.f       inisoili.f       inisurf1.f \
         initial.f       initw2.f       iniveg.f       intchamps.f \
         integ2.f       interpveg.f       intodif.f       intozon.f \
         intwat3.f       inverse.f       ipig.f       isba2.f \
         juliand.f       kdifsimp.f       kfcp4.f       ktrsnt3.f \
         ktrsnt_mg.f       kuo2.f       kuostd.f       kuosun.f \
         kuosym.f       lacs.f       lacs1.f       liebman_2.f \
         lin_adjtq.f       lin_adjtq_ad.f       lin_adjtq_tl.f       lin_conds1.f \
         lin_conds1_ad.f       lin_conds1_tl.f       lin_diff_vert1.f       lin_diff_vert1_ad.f \
         lin_diff_vert1_tl.f       lin_difuvd1.f       lin_difuvd1_ad.f       lin_difuvd1_tl.f \
         lin_difuvd2.f       lin_difuvd2_ad.f       lin_difuvd2_tl.f       lin_difver1.f \
         lin_difver1_ad.f       lin_difver1_tl.f       lin_inichamp1.f       lin_inichamp1_tl.f \
         lin_kdif_simp1.f       lin_kdif_simp1_ad.f       lin_kdif_simp1_tl.f       lin_kdif_simp1_tr.f \
         lin_kuosym.f       lin_kuosym_ad.f       lin_kuosym_tl.f       lin_phyexe1.f \
         lin_phyexe1_ad.f       lin_phyexe1_tl.f       lin_precip1.f       lin_precip1_ad.f \
         lin_precip1_tl.f       lin_sgo1.f       lin_sgo1_ad.f       lin_sgo1_tl.f \
         lin_sgoflx1.f       lin_sgoflx1_ad.f       lin_sgoflx1_tl.f       liqwc.f \
         litblrad2.f       litozon2.f       llacar.f       lsctrol.f \
         mconadj2.f       mesahr.f       mesahu.f       mfdlesmx.f \
         mfodla.f       mfodle.f       mfodqa.f       mfodqs.f \
         mfoefq.f       mfoeic.f       mfoew.f       mfoewa.f \
         mfohr.f       mfohra.f       mfoqfe.f       mfoqsa.f \
         mfoqst.f       mfottv.f       mfottv_ad.f       mfottv_tl.f \
         mfottvh.f       mfotvht.f       mfotvt.f       mfotvt_ad.f \
         mfotvt_tl.f       mgsdict.f       mhraes.f       mhrahu.f \
         mhuaes.f       mhuahr.f       mirror.f       mixlen3.f \
         mixphase6.f       modhui.f       modpbus.f       moistke3.f \
         momcoef.f       mosavg.f       moscopy.f       moscover.f \
         movcar.f       mprecip4.f       mras0.f       msoltri6.f \
         mthtaw.f       mthtaw2.f       mtn_case.f       mtn_cfg.f \
         nes1s.f       nes2s.f       nes3s.f       nes4s.f \
         nesajr.f       nest_indata.f       nest_init.f       nest_intt.f \
         nestit.f       newrad4.f       ngfft.f       nli.f \
         nlip_2.f       ntables.f       nuages2.f       oldrad3.f \
         opinv6.f       opinv7.f       optriss6.f       optriss7.f \
         out_dq.f       out_dyn.f       out_padbuf.f       out_phy.f \
         out_qc0.f       out_sblock.f       out_sfile.f       out_sgrid.f \
         out_slev.f       out_thm.f       out_tracer.f       out_uv.f \
         out_vmm.f       ouvrstrt.f       ozoref2.f       p0vt2gz_hyb.f \
         p_apply.f       p_config.f       p_fcpfw.f       p_fillbus.f \
         p_inincr.f       p_init.f       p_main.f       p_phyexe.f \
         p_physlb.f       p_phystep.f       p_psetiw.f       p_radiaf.f \
         p_set.f       p_uvgridscal.f       p_vlsp.f       p_vmmphy.f \
         pdfcdate.f       pdfjdate.f       pdfjdate2.f       phy_ini.f \
         phybus_data.f       phycom.f       phydebu4.f       phyexe1.f \
         phyopt.f       pllwfgfw.f       pnm2.f       pntg123.f \
         pntozon.f       pospers.f       pre.f       predat.f \
         prelim1.f       prep_2.f       prep_cw.f       prep_cw_rad.f \
         prep_index.f       prepoic.f       prever.f       prgen.f \
         prgzvta.f       prism_def_var_proto.f       prism_enddef_proto.f       prism_terminate_proto.f \
         prodef.f       prof5.f       prsdate.f       qcfft8.f \
         qcsgs.f       qozon3.f       raddel.f       radfac3.f \
         radiaf_o.f       radir8.f       radix.f       rasol.f \
         raysol.f       rdmax.f       rdmoy.f       rdradf.f \
         rdrstrt.f       readdyn.f       readgeo.f       readlam.f \
         rhs.f       rhsp_2.f       riflux.f       rigrad1.f \
         satq.f       save_options.f       scatter.f       schal.f \
         seaice1.f       secajus.f       seralc2.f       serallc2.f \
         serdata.f       serdbu.f       serdim.f       serecri.f \
         serfin.f       sergdim.f       sergini.f       sergoff.f \
         sergout.f       sergset.f       serie.f       serkh.f \
         serpat2.f       serset.f       sersetc.f       sersetm.f \
         serwrit.f       serwrit2.f       serxst.f       sesahr.f \
         sesahu.f       set_class_const.f       set_class_dt.f       set_cn1.f \
         set_cori.f       set_crni.f       set_dcst.f       set_dcst_8.f \
         set_dia.f       set_dync.f       set_fft.f       set_filt.f \
         set_geof.f       set_geom.f       set_grid.f       set_intuv.f \
         set_level.f       set_list.f       set_mem.f       set_nest.f \
         set_opr.f       set_oprz.f       set_ops8.f       set_poic.f \
         set_poic_par.f       set_poic_par_u.f       set_pois.f       set_rhs.f \
         set_schm.f       set_sor.f       set_step.f       set_term.f \
         set_transpose.f       set_trig21.f       set_var.f       set_vt.f \
         set_vta.f       set_world_view.f       set_world_view1.f       set_xnbit.f \
         set_xst.f       setdsr2.f       setscqr.f       setsd.f \
         settriv.f       setvis2.f       sfcbus_data.f       sfltr.f \
         sfodla.f       sfodle.f       sfodqa.f       sfodqs.f \
         sfoefq.f       sfoew.f       sfoewa.f       sfohr.f \
         sfohra.f       sfols.f       sfolv.f       sfopop.f \
         sfopot.f       sfoqfe.f       sfoqsa.f       sfoqst.f \
         sfottv.f       sfotvt.f       sgamash.f       sgamasp.f \
         sgamatd.f       sgoflx2.f       sgoflx3.f       shallconv3.f \
         shalow.f       shraes.f       shrahu.f       shuaes.f \
         shuahr.f       shuffle.f       skocon.f       slabsym.f \
         slfun_tq.f       slfun_uv.f       sltop_tq.f       sltop_uv.f \
         soili.f       sol_abc.f       sol_fft8_2.f       sol_fft8_lam.f \
         sol_hcr.f       sol_main.f       sol_mxma8_2.f       sol_parite_2.f \
         sol_trsf.f       splitst.f       srequet.f       statf_dm.f \
         statfld.f       statfld_old.f       status_file.f       statvps1.f \
         stetae.f       stfrst4.f       sthtaw.f       sthtaw2.f \
         stop_world_view.f       stopmpi.f       stretch_axis2.f       sttlcl.f \
         sun7.f       sun_radfix1.f       suncos.f       surf_precip.f \
         surface.f       surfix.f       surfradcloud.f       t02t1.f \
         t12t2.f       t22t0.f       ta2t1tx.f       theo_cfg.f \
         thermco2.f       tkealg.f       tmg_init0.f       tmg_start0.f \
         tmg_stop0.f       tmg_terminate0.f       tp.f       tpdd.f \
         tpmix.f       tr_init.f       tr_vint.f       tracers.f \
         trans_clean.f       trapez2.f       tstpdyn.f       tttt.f \
         turbul.f       update.f       update3.f       utco2.f \
         uv2tdpsd.f       uv_acg2g.f       var_topo.f       vco2inf2.f \
         vdfds.f       vegi.f       verder.f       vertdiff.f \
         vgathr.f       vkuocon6.f       vrtd.f       vspng_abc.f \
         vspng_del2.f       vspng_drv.f       vspng_drv_lam.f       vspng_lam.f \
         vspng_main.f       vspng_set.f       vtap.f       vte_hyb2hyb.f \
         vte_intvertx.f       vte_uv2img.f       vte_vrtical.f       vterp1.f \
         vterp2.f       water.f       wdpers.f       wetcon2.f \
         wflux.f       wlog.f       wndth.f       writlam.f \
         wrrstrt.f       wrvref.f       xyfil.f       zeracc.f \
         zongini.f       zongoff.f       zongopr.f

F90DECKS= \
         allocate_gr_snow.f90       averaged_albedo_teb.f90       averaged_tsrad_teb.f90       bld_e_budget.f90 \
         c_oasis_get.f90       c_oasis_init.f90       c_oasis_partition.f90       c_oasis_put.f90 \
         coef_ver_interp_lin2d.f90       coupling_teb.f90       coupling_teb_orography.f90       default_prep_teb.f90 \
         default_teb.f90       forcing_vert_shift.f90       get_luout.f90       hor_interpol.f90 \
         ini_csts.f90       init_snow_lw.f90       init_teb.f90       initown.f90 \
         interp_grid1d.f90       interp_grid2d.f90       mnhget_luout.f90       possol.f90 \
         prep_hor_snow_field.f90       prep_hor_snow_fields.f90       prep_hor_teb_field.f90       prep_snow_gem.f90 \
         prep_teb.f90       prep_teb_gem.f90       road_wall_layer_e_budget.f90       roof_layer_e_budget.f90 \
         snow_cover_1layer.f90       snow_heat_to_t_wliq.f90       sunpos.f90       surface_aero_cond.f90 \
         surface_cd.f90       surface_ri.f90       teb.f90       town.f90 \
         tridiag_ground.f90       urban_drag.f90       urban_fluxes.f90       urban_hydro.f90 \
         urban_lw_coef.f90       urban_snow_evol.f90       urban_solar_abs.f90       vegtype_grid_to_patch_grid.f90 \
         vegtype_to_patch.f90       ver_interp_lin2d.f90

OBJECTS= \
         APREP.o       CANADD.o       CANALB.o       CANVAP.o \
         CGROW.o       CHKWAT.o       CLASSA.o       CLASSB.o \
         CLASSBD.o       CLASSD.o       CLASSG.o       CLASSI.o \
         CLASSS.o       CLASST.o       CLASSW.o       CLASSZ.o \
         CWCALC.o       DIASURF.o       DRCOEF.o       FLXSURFZ.o \
         GATPREP.o       GRALB.o       GRDRAN.o       GRINFL.o \
         ICEBAL.o       SLDIAG.o       SNINFL.o       SNOADD.o \
         SNOALBA.o       SNOALBW.o       SNOVAP.o       SUBCAN.o \
         TFREEZ.o       TMCALC.o       TMELT.o       TNPOST.o \
         TNPREP.o       TPREP.o       TSOLVC.o       TSOLVE.o \
         TSPOST.o       TSPREP.o       TWCALC.o       WEND.o \
         WFILL.o       WFLOW.o       WPREP.o       XIT.o \
         absdvdz3.o       acqui.o       adilwc.o       adw_cfl_lam.o \
         adw_ckbd.o       adw_ckbd_lam.o       adw_exch_1.o       adw_exch_2.o \
         adw_exch_3.o       adw_interp.o       adw_interp2.o       adw_main.o \
         adw_main_1_wnd.o       adw_main_2_pos.o       adw_main_3_int.o       adw_main_3_intlag.o \
         adw_pol0.o       adw_pols.o       adw_polw.o       adw_polx.o \
         adw_set.o       adw_setint.o       adw_trajex.o       adw_trajsp.o \
         adw_tricub.o       adw_tricub_lag3d.o       adw_trilin.o       adw_trilin_turbo.o \
         adw_vder.o       aggcovernat.o       aggcoverurb.o       aggveg.o \
         agrege1.o       agvgclas.o       agvgclas2.o       agvgmask.o \
         agvgmask2.o       agvgmos.o       allocate_gr_snow.o       atmflux.o \
         averaged_albedo_teb.o       averaged_tsrad_teb.o       bac.o       bacp_2.o \
         baktotq2.o       bcastc1.o       bin2com.o       blcloud2.o \
         bld_e_budget.o       blg2.o       blkcol.o       blocstat.o \
         blweight2.o       bmf_perturb.o       bourge1.o       bubble.o \
         bubble_cfg.o       c_bloccplg.o       c_cplg_iml.o       c_cplg_surf.o \
         c_fillbus.o       c_finddim.o       c_getbus.o       c_init.o \
         c_init_cplfld.o       c_init_oasisfld.o       c_mtlmap.o       c_oasis_get.o \
         c_oasis_init.o       c_oasis_partition.o       c_oasis_put.o       c_screen.o \
         c_set_coupling.o       c_stop_coupling.o       calcalb1.o       calccovf.o \
         calcdiag.o       calcz0.o       calddqr.o       caldiv_2.o \
         calomeg.o       calvor.o       calzz.o       cartall.o \
         check_parti.o       class300.o       class320.o       class330.o \
         class_const.o       cldoptx4.o       cldwin.o       climphs3.o \
         cllaij.o       clsgs2.o       co2info.o       coef_ver_interp_lin2d.o \
         coherence.o       condload.o       conds.o       conray2.o \
         conres1.o       consun1.o       copybus.o       coriol8.o \
         coupling_teb.o       coupling_teb_orography.o       ctmdiag.o       datf2p.o \
         datp2f.o       default_prep_teb.o       default_teb.o       diagno_cw_rad.o \
         diagnosurf.o       diasurf2.o       difuvd1.o       difuvd2.o \
         difuvd4.o       difuvd4n.o       difuvd8.o       difuvd9.o \
         difuvdf.o       difuvdfj.o       difuvt.o       difvef.o \
         difver6.o       digflt.o       doout.o       drag2.o \
         dtfrz.o       dtfrznew.o       dumpbus2.o       dvrtdf.o \
         dvrtef.o       dynstep.o       e_arak.o       e_arret.o \
         e_bmfrd.o       e_bmfsplitxy2.o       e_coefdgf.o       e_dateo.o \
         e_decal.o       e_dynfld.o       e_filtopx.o       e_findmod.o \
         e_gemnml.o       e_gemntr.o       e_geopfld.o       e_geopini.o \
         e_grid.o       e_infiles.o       e_int_ix2xu.o       e_int_iy2yv.o \
         e_intgeo.o       e_intpoly.o       e_intthm.o       e_intwind.o \
         e_liaccu.o       e_moclim2.o       e_modgl.o       e_modhs.o \
         e_modmg.o       e_nml2bin.o       e_ntrxyfil.o       e_pilotf.o \
         e_pregeo.o       e_premoy.o       e_pretopo.o       e_rdhint3.o \
         e_screen.o       e_setdgf.o       e_setintuv.o       e_setinuvl.o \
         e_sgrid.o       e_sortr.o       e_specanal.o       e_trinit.o \
         e_wlogntr.o       e_wrgeop.o       ebudget.o       ecmwf_ab.o \
         ecris_fst2.o       ecris_fst2m.o       emicrog.o       envirtht.o \
         equivmount.o       eturbl6.o       extdiag.o       fcpara2.o \
         fcrest5.o       feseri.o       ficemxp.o       fillagg.o \
         filter.o       flxsurf3.o       fomichev.o       force_restore.o \
         forcing_vert_shift.o       frstgss.o       ftridi6.o       ftridi7.o \
         gather.o       gefstop.o       geleyn.o       gem_ctrl.o \
         gem_run.o       gem_stop.o       gemdm.o       gemtim.o \
         genab.o       genab2.o       geneigl.o       geophy.o \
         gesdict.o       get_luout.o       getbpm.o       getbus1.o \
         getindx.o       getp0.o       glaciers1.o       glb_bound.o \
         glbcolc.o       glbdist.o       glbpos.o       glbstat.o \
         gwd5.o       gwdfx95a.o       gz2p0.o       gzcalcul.o \
         hatoprg.o       hdif0.o       hdif_phy.o       heatcoef.o \
         height_sponge.o       hor_interpol.o       horwavg.o       hspng_abc.o \
         hspng_drv.o       hspng_main.o       hydro1.o       hzd_abc.o \
         hzd_areal.o       hzd_areav.o       hzd_bfct.o       hzd_bfct_1d.o \
         hzd_del2.o       hzd_delpwr.o       hzd_delpwr_1d.o       hzd_exhrdif.o \
         hzd_fact.o       hzd_ho.o       hzd_ho_parite.o       hzd_hof.o \
         hzd_hof_lam.o       hzd_hoffld.o       hzd_main.o       hzd_nudeln.o \
         hzd_set.o       hzd_set_ops8.o       hzd_solfft.o       hzd_solfft_1d.o \
         hzd_solfft_lam.o       hzd_solmxma.o       hzd_solparite_2.o       hzd_solparite_u.o \
         incdatsd.o       inctphy.o       inctphy3.o       indata.o \
         indseri.o       ini_csts.o       inichamp2.o       inicover.o \
         inicoverurb.o       inifcp.o       inikey.o       inimos.o \
         iniobus.o       iniptsurf.o       iniras.o       inisoili.o \
         inisurf1.o       init_snow_lw.o       init_teb.o       initial.o \
         initown.o       initw2.o       iniveg.o       intchamps.o \
         integ2.o       interp_grid1d.o       interp_grid2d.o       interpveg.o \
         intodif.o       intozon.o       intwat3.o       inverse.o \
         ipig.o       isba2.o       juliand.o       kdifsimp.o \
         kfcp4.o       ktrsnt3.o       ktrsnt_mg.o       kuo2.o \
         kuostd.o       kuosun.o       kuosym.o       lacs.o \
         lacs1.o       liebman_2.o       lin_adjtq.o       lin_adjtq_ad.o \
         lin_adjtq_tl.o       lin_conds1.o       lin_conds1_ad.o       lin_conds1_tl.o \
         lin_diff_vert1.o       lin_diff_vert1_ad.o       lin_diff_vert1_tl.o       lin_difuvd1.o \
         lin_difuvd1_ad.o       lin_difuvd1_tl.o       lin_difuvd2.o       lin_difuvd2_ad.o \
         lin_difuvd2_tl.o       lin_difver1.o       lin_difver1_ad.o       lin_difver1_tl.o \
         lin_inichamp1.o       lin_inichamp1_tl.o       lin_kdif_simp1.o       lin_kdif_simp1_ad.o \
         lin_kdif_simp1_tl.o       lin_kdif_simp1_tr.o       lin_kuosym.o       lin_kuosym_ad.o \
         lin_kuosym_tl.o       lin_phyexe1.o       lin_phyexe1_ad.o       lin_phyexe1_tl.o \
         lin_precip1.o       lin_precip1_ad.o       lin_precip1_tl.o       lin_sgo1.o \
         lin_sgo1_ad.o       lin_sgo1_tl.o       lin_sgoflx1.o       lin_sgoflx1_ad.o \
         lin_sgoflx1_tl.o       liqwc.o       litblrad2.o       litozon2.o \
         llacar.o       lsctrol.o       mconadj2.o       mesahr.o \
         mesahu.o       mfdlesmx.o       mfodla.o       mfodle.o \
         mfodqa.o       mfodqs.o       mfoefq.o       mfoeic.o \
         mfoew.o       mfoewa.o       mfohr.o       mfohra.o \
         mfoqfe.o       mfoqsa.o       mfoqst.o       mfottv.o \
         mfottv_ad.o       mfottv_tl.o       mfottvh.o       mfotvht.o \
         mfotvt.o       mfotvt_ad.o       mfotvt_tl.o       mgsdict.o \
         mhraes.o       mhrahu.o       mhuaes.o       mhuahr.o \
         mirror.o       mixlen3.o       mixphase6.o       mnhget_luout.o \
         modd_atm_cst.o       modd_csts.o       modd_data_cover_par.o       modd_isba.o \
         modd_prep.o       modd_prep_snow.o       modd_prep_teb.o       modd_snow_par.o \
         modd_surf_par.o       modd_teb.o       modd_teb_grid.o       modd_town.o \
         modd_type_date.o       modd_type_snow.o       modd_ver_interp_lin.o       mode_snow3l.o \
         mode_surf_snow_frac.o       mode_thermos.o       modhui.o       modi_allocate_gr_snow.o \
         modi_averaged_albedo_teb.o       modi_averaged_tsrad_teb.o       modi_bld_e_budget.o       modi_coef_ver_interp_lin2d.o \
         modi_coupling_teb.o       modi_coupling_teb_orography.o       modi_default_prep_teb.o       modi_default_teb.o \
         modi_forcing_vert_shift.o       modi_get_luout.o       modi_hor_interpol.o       modi_ini_csts.o \
         modi_init_snow_lw.o       modi_init_teb.o       modi_interp_grid1d.o       modi_interp_grid2d.o \
         modi_mnhget_luout.o       modi_possol.o       modi_prep_hor_snow_field.o       modi_prep_hor_snow_fields.o \
         modi_prep_hor_teb_field.o       modi_prep_snow_gem.o       modi_prep_teb.o       modi_prep_teb_gem.o \
         modi_road_wall_layer_e_budget.o       modi_roof_layer_e_budget.o       modi_snow_cover_1layer.o       modi_snow_heat_to_t_wliq.o \
         modi_sunpos.o       modi_surface_aero_cond.o       modi_surface_cd.o       modi_surface_ri.o \
         modi_teb.o       modi_tridiag_ground.o       modi_urban_drag.o       modi_urban_fluxes.o \
         modi_urban_hydro.o       modi_urban_lw_coef.o       modi_urban_snow_evol.o       modi_urban_solar_abs.o \
         modi_vegtype_grid_to_patch_grid.o       modi_vegtype_to_patch.o       modi_ver_interp_lin2d.o       modpbus.o \
         moistke3.o       momcoef.o       mosavg.o       moscopy.o \
         moscover.o       movcar.o       mprecip4.o       mras0.o \
         msoltri6.o       mthtaw.o       mthtaw2.o       mtn_case.o \
         mtn_cfg.o       nes1s.o       nes2s.o       nes3s.o \
         nes4s.o       nesajr.o       nest_indata.o       nest_init.o \
         nest_intt.o       nestit.o       newrad4.o       ngfft.o \
         nli.o       nlip_2.o       ntables.o       nuages2.o \
         oldrad3.o       opinv6.o       opinv7.o       optriss6.o \
         optriss7.o       out_dq.o       out_dyn.o       out_padbuf.o \
         out_phy.o       out_qc0.o       out_sblock.o       out_sfile.o \
         out_sgrid.o       out_slev.o       out_thm.o       out_tracer.o \
         out_uv.o       out_vmm.o       ouvrstrt.o       ozoref2.o \
         p0vt2gz_hyb.o       p_apply.o       p_config.o       p_fcpfw.o \
         p_fillbus.o       p_inincr.o       p_init.o       p_main.o \
         p_phyexe.o       p_physlb.o       p_phystep.o       p_psetiw.o \
         p_radiaf.o       p_set.o       p_uvgridscal.o       p_vlsp.o \
         p_vmmphy.o       pdfcdate.o       pdfjdate.o       pdfjdate2.o \
         phy_ini.o       phybus_data.o       phycom.o       phydebu4.o \
         phyexe1.o       phyopt.o       pllwfgfw.o       pnm2.o \
         pntg123.o       pntozon.o       pospers.o       possol.o \
         pre.o       predat.o       prelim1.o       prep_2.o \
         prep_cw.o       prep_cw_rad.o       prep_hor_snow_field.o       prep_hor_snow_fields.o \
         prep_hor_teb_field.o       prep_index.o       prep_snow_gem.o       prep_teb.o \
         prep_teb_gem.o       prepoic.o       prever.o       prgen.o \
         prgzvta.o       prism_def_var_proto.o       prism_enddef_proto.o       prism_terminate_proto.o \
         prodef.o       prof5.o       prsdate.o       qcfft8.o \
         qcsgs.o       qozon3.o       raddel.o       radfac3.o \
         radiaf_o.o       radir8.o       radix.o       rasol.o \
         raysol.o       rdmax.o       rdmoy.o       rdradf.o \
         rdrstrt.o       readdyn.o       readgeo.o       readlam.o \
         rhs.o       rhsp_2.o       riflux.o       rigrad1.o \
         road_wall_layer_e_budget.o       roof_layer_e_budget.o       satq.o       save_options.o \
         scatter.o       schal.o       seaice1.o       secajus.o \
         seralc2.o       serallc2.o       serdata.o       serdbu.o \
         serdim.o       serecri.o       serfin.o       sergdim.o \
         sergini.o       sergoff.o       sergout.o       sergset.o \
         serie.o       serkh.o       serpat2.o       serset.o \
         sersetc.o       sersetm.o       serwrit.o       serwrit2.o \
         serxst.o       sesahr.o       sesahu.o       set_class_const.o \
         set_class_dt.o       set_cn1.o       set_cori.o       set_crni.o \
         set_dcst.o       set_dcst_8.o       set_dia.o       set_dync.o \
         set_fft.o       set_filt.o       set_geof.o       set_geom.o \
         set_grid.o       set_intuv.o       set_level.o       set_list.o \
         set_mem.o       set_nest.o       set_opr.o       set_oprz.o \
         set_ops8.o       set_poic.o       set_poic_par.o       set_poic_par_u.o \
         set_pois.o       set_rhs.o       set_schm.o       set_sor.o \
         set_step.o       set_term.o       set_transpose.o       set_trig21.o \
         set_var.o       set_vt.o       set_vta.o       set_world_view.o \
         set_world_view1.o       set_xnbit.o       set_xst.o       setdsr2.o \
         setscqr.o       setsd.o       settriv.o       setvis2.o \
         sfcbus_data.o       sfltr.o       sfodla.o       sfodle.o \
         sfodqa.o       sfodqs.o       sfoefq.o       sfoew.o \
         sfoewa.o       sfohr.o       sfohra.o       sfols.o \
         sfolv.o       sfopop.o       sfopot.o       sfoqfe.o \
         sfoqsa.o       sfoqst.o       sfottv.o       sfotvt.o \
         sgamash.o       sgamasp.o       sgamatd.o       sgoflx2.o \
         sgoflx3.o       shallconv3.o       shalow.o       shraes.o \
         shrahu.o       shuaes.o       shuahr.o       shuffle.o \
         skocon.o       slabsym.o       slfun_tq.o       slfun_uv.o \
         sltop_tq.o       sltop_uv.o       snow_cover_1layer.o       snow_heat_to_t_wliq.o \
         soili.o       sol_abc.o       sol_fft8_2.o       sol_fft8_lam.o \
         sol_hcr.o       sol_main.o       sol_mxma8_2.o       sol_parite_2.o \
         sol_trsf.o       splitst.o       srequet.o       statf_dm.o \
         statfld.o       statfld_old.o       status_file.o       statvps1.o \
         stetae.o       stfrst4.o       sthtaw.o       sthtaw2.o \
         stop_world_view.o       stopmpi.o       stretch_axis2.o       sttlcl.o \
         sun7.o       sun_radfix1.o       suncos.o       sunpos.o \
         surf_precip.o       surface.o       surface_aero_cond.o       surface_cd.o \
         surface_ri.o       surfix.o       surfradcloud.o       t02t1.o \
         t12t2.o       t22t0.o       ta2t1tx.o       teb.o \
         theo_cfg.o       thermco2.o       tkealg.o       tmg_init0.o \
         tmg_start0.o       tmg_stop0.o       tmg_terminate0.o       town.o \
         tp.o       tpdd.o       tpmix.o       tr_init.o \
         tr_vint.o       tracers.o       trans_clean.o       trapez2.o \
         tridiag_ground.o       tstpdyn.o       tttt.o       turbul.o \
         update.o       update3.o       urban_drag.o       urban_fluxes.o \
         urban_hydro.o       urban_lw_coef.o       urban_snow_evol.o       urban_solar_abs.o \
         utco2.o       uv2tdpsd.o       uv_acg2g.o       v4d_prof.o \
         v4dz.o       var_topo.o       vco2inf2.o       vdfds.o \
         vegi.o       vegtype_grid_to_patch_grid.o       vegtype_to_patch.o       ver_interp_lin2d.o \
         verder.o       vertdiff.o       vgathr.o       vkuocon6.o \
         vrtd.o       vspng_abc.o       vspng_del2.o       vspng_drv.o \
         vspng_drv_lam.o       vspng_lam.o       vspng_main.o       vspng_set.o \
         vtap.o       vte_hyb2hyb.o       vte_intvertx.o       vte_uv2img.o \
         vte_vrtical.o       vterp1.o       vterp2.o       water.o \
         wdpers.o       wetcon2.o       wflux.o       wlog.o \
         wndth.o       writlam.o       wrrstrt.o       wrvref.o \
         xyfil.o       zeracc.o       zongini.o       zongoff.o \
         zongopr.o

COMDECKS= \
         acmcon.cdk       acq.cdk       acq_int.cdk       adw.cdk \
         bmf.cdk       bubble.cdk       buses.cdk       busind.cdk \
         busind_tr.cdk       c_cplg.cdk       c_nml.cdk       class_com.cdk \
         classlvls.cdk       cldopt_mode.cdk       clefcon.cdk       comphy.cdk \
         comras.cdk       consphy.cdk       cori.cdk       cst_lis.cdk \
         cstv.cdk       dcst.cdk       deriv_stabfunc.cdk       dimout.cdk \
         dimsurf.cdk       dintern.cdk       dzcond.cdk       dzsedi.cdk \
         e_anal.cdk       e_cdate.cdk       e_dgf.cdk       e_dim.cdk \
         e_fu.cdk       e_geoerr.cdk       e_geol.cdk       e_geoma.cdk \
         e_geomg.cdk       e_grids.cdk       e_intuv.cdk       e_inuvl.cdk \
         e_nml.cdk       e_option.cdk       e_schm.cdk       e_topo.cdk \
         e_tr.cdk       eigv.cdk       fft.cdk       filename.cdk \
         fintern.cdk       fintern90.cdk       flatscp.cdk       fmroft.cdk \
         geomg.cdk       geomn.cdk       glb_ld.cdk       glb_pil.cdk \
         grd.cdk       grid.cdk       hblen.cdk       hgc.cdk \
         himin.cdk       hscap.cdk       hspng.cdk       hybdim.cdk \
         hzd.cdk       icelvls.cdk       impnone.cdk       ind.cdk \
         indx_sfc.cdk       init.cdk       initcph.cdk       intuv.cdk \
         inuvl.cdk       isbapar.cdk       lctl.cdk       ldnh.cdk \
         leads.cdk       level.cdk       locbus.cdk       lun.cdk \
         machcon.cdk       macro.cdk       maxlev.cdk       mem.cdk \
         modpr1.cdk       modpr2.cdk       modtp1.cdk       mosaic.cdk \
         mountains.cdk       mtn.cdk       mult.cdk       nbvarsurf.cdk \
         nclassvg.cdk       nest.cdk       nl.cdk       nml.cdk \
         nocld.cdk       obus.cdk       offc.cdk       offline.cdk \
         opr.cdk       options.cdk       orh.cdk       out.cdk \
         out1.cdk       out2.cdk       out3.cdk       outd.cdk \
         outp.cdk       ozopnt.cdk       p4d_simp.cdk       p_bus.cdk \
         p_clim.cdk       p_cond.cdk       p_cond2.cdk       p_geobus.cdk \
         p_geobusid.cdk       p_geof.cdk       p_geol.cdk       p_gwd.cdk \
         p_nml.cdk       p_nml2.cdk       p_out.cdk       p_pbl.cdk \
         p_pbl2.cdk       p_pset.cdk       p_rad.cdk       p_serg.cdk \
         p_str.cdk       p_zong.cdk       path.cdk       phybus.cdk \
         pilot.cdk       pres.cdk       ptopo.cdk       radclcons.cdk \
         raddata.cdk       radparam.cdk       radpnt.cdk       rasdata.cdk \
         rhsc.cdk       ribcom.cdk       rstr.cdk       scfrst.cdk \
         schm.cdk       sedipara.cdk       sercmdk.cdk       setsor.cdk \
         sfcbus.cdk       small_sedimentation_dt.cdk       snoh0.cdk       sol.cdk \
         solcons.cdk       stabfunc.cdk       step.cdk       stk.cdk \
         surfacepar.cdk       surfcon.cdk       surfcon_ini.cdk       tables.cdk \
         tebcst.cdk       theo.cdk       theonml.cdk       time0.cdk \
         timestep.cdk       tr3d.cdk       trp.cdk       tuiles.cdk \
         tuiles_red.cdk       v4dcfl.cdk       v4dg.cdk       v4dj.cdk \
         v4dm.cdk       v4dr.cdk       valcon.cdk       vamin.cdk \
         version.cdk       vrtd.cdk       vspng.cdk       vt0.cdk \
         vt1.cdk       vt1m.cdk       vt2.cdk       vta.cdk \
         vth.cdk       vtopo.cdk       vtx.cdk       workspc.cdk \
         xptsurf.cdk       xst.cdk       xstb.cdk       zblen.cdk \
         zuzt.cdk

COMDECKS90= \
         modd_atm_cst.cdk90       modd_csts.cdk90       modd_data_cover_par.cdk90       modd_isba.cdk90 \
         modd_prep.cdk90       modd_prep_snow.cdk90       modd_prep_teb.cdk90       modd_snow_par.cdk90 \
         modd_surf_par.cdk90       modd_teb.cdk90       modd_teb_grid.cdk90       modd_town.cdk90 \
         modd_type_date.cdk90       modd_type_snow.cdk90       modd_ver_interp_lin.cdk90       mode_snow3l.cdk90 \
         mode_surf_snow_frac.cdk90       mode_thermos.cdk90       modi_allocate_gr_snow.cdk90       modi_averaged_albedo_teb.cdk90 \
         modi_averaged_tsrad_teb.cdk90       modi_bld_e_budget.cdk90       modi_coef_ver_interp_lin2d.cdk90       modi_coupling_teb.cdk90 \
         modi_coupling_teb_orography.cdk90       modi_default_prep_teb.cdk90       modi_default_teb.cdk90       modi_forcing_vert_shift.cdk90 \
         modi_get_luout.cdk90       modi_hor_interpol.cdk90       modi_ini_csts.cdk90       modi_init_snow_lw.cdk90 \
         modi_init_teb.cdk90       modi_interp_grid1d.cdk90       modi_interp_grid2d.cdk90       modi_mnhget_luout.cdk90 \
         modi_possol.cdk90       modi_prep_hor_snow_field.cdk90       modi_prep_hor_snow_fields.cdk90       modi_prep_hor_teb_field.cdk90 \
         modi_prep_snow_gem.cdk90       modi_prep_teb.cdk90       modi_prep_teb_gem.cdk90       modi_road_wall_layer_e_budget.cdk90 \
         modi_roof_layer_e_budget.cdk90       modi_snow_cover_1layer.cdk90       modi_snow_heat_to_t_wliq.cdk90       modi_sunpos.cdk90 \
         modi_surface_aero_cond.cdk90       modi_surface_cd.cdk90       modi_surface_ri.cdk90       modi_teb.cdk90 \
         modi_tridiag_ground.cdk90       modi_urban_drag.cdk90       modi_urban_fluxes.cdk90       modi_urban_hydro.cdk90 \
         modi_urban_lw_coef.cdk90       modi_urban_snow_evol.cdk90       modi_urban_solar_abs.cdk90       modi_vegtype_grid_to_patch_grid.cdk90 \
         modi_vegtype_to_patch.cdk90       modi_ver_interp_lin2d.cdk90       v4d_prof.cdk90       v4dz.cdk90

HDECKS= \
         phy_macros_f.h



FICHIERS = $(FDECKS) $(CDECKS) $(ASDECKS) $(HDECKS) $(F90DECKS) $(COMDECKS90)

absdvdz3.f: absdvdz3.ftn 	clefcon.cdk 	consphy.cdk 	impnone.cdk 
acqui.f: acqui.ftn 	acq.cdk 	acq_int.cdk 	bmf.cdk \
	cstv.cdk 	dcst.cdk 	geomg.cdk 	glb_ld.cdk \
	glb_pil.cdk 	grd.cdk 	hblen.cdk 	ind.cdk \
	lun.cdk 	pres.cdk 	ptopo.cdk 	schm.cdk \
	vtopo.cdk 
adilwc.f: adilwc.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 
adw_cfl_lam.f: adw_cfl_lam.ftn 	adw.cdk 	glb_ld.cdk 	ptopo.cdk 
adw_ckbd.f: adw_ckbd.ftn 	adw.cdk 	geomg.cdk 	glb_ld.cdk \
	impnone.cdk 
adw_ckbd_lam.f: adw_ckbd_lam.ftn 	adw.cdk 	geomg.cdk 	glb_ld.cdk \
	impnone.cdk 	ptopo.cdk 
adw_exch_1.f: adw_exch_1.ftn 	adw.cdk 	geomg.cdk 	glb_ld.cdk \
	impnone.cdk 	ptopo.cdk 
adw_exch_2.f: adw_exch_2.ftn 	glb_ld.cdk 	impnone.cdk 
adw_exch_3.f: adw_exch_3.ftn 	adw.cdk 	impnone.cdk 
adw_interp.f: adw_interp.ftn 	adw.cdk 	glb_ld.cdk 
adw_interp2.f: adw_interp2.ftn 	adw.cdk 	glb_ld.cdk 
adw_main.f: adw_main.ftn 	adw.cdk 	glb_ld.cdk 	impnone.cdk \
	lun.cdk 	orh.cdk 	ptopo.cdk 	schm.cdk \
	v4dg.cdk 	v4dr.cdk 
adw_main_1_wnd.f: adw_main_1_wnd.ftn 	adw.cdk 	glb_ld.cdk 	glb_pil.cdk \
	inuvl.cdk 	lun.cdk 	vth.cdk 
adw_main_2_pos.f: adw_main_2_pos.ftn 	adw.cdk 	cstv.cdk 	dcst.cdk \
	geomg.cdk 	glb_ld.cdk 	glb_pil.cdk 	lctl.cdk \
	lun.cdk 	v4dg.cdk 	vt1.cdk 	vth.cdk 
adw_main_3_int.f: adw_main_3_int.ftn 	adw.cdk 	cstv.cdk 	geomg.cdk \
	glb_ld.cdk 	glb_pil.cdk 	lctl.cdk 	lun.cdk \
	offc.cdk 	orh.cdk 	ptopo.cdk 	rhsc.cdk \
	schm.cdk 	step.cdk 	tr3d.cdk 	v4dg.cdk \
	vt0.cdk 	vt1.cdk 
adw_main_3_intlag.f: adw_main_3_intlag.ftn 	adw.cdk 	cstv.cdk 	geomg.cdk \
	glb_ld.cdk 	glb_pil.cdk 	lctl.cdk 	lun.cdk \
	offc.cdk 	orh.cdk 	ptopo.cdk 	rhsc.cdk \
	schm.cdk 	step.cdk 	tr3d.cdk 	v4dg.cdk \
	vt0.cdk 	vt1.cdk 
adw_pol0.f: adw_pol0.ftn 	impnone.cdk 
adw_pols.f: adw_pols.ftn 	impnone.cdk 
adw_polw.f: adw_polw.ftn 	impnone.cdk 
adw_polx.f: adw_polx.ftn 	adw.cdk 	impnone.cdk 
adw_set.f: adw_set.ftn 	adw.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	impnone.cdk 	schm.cdk 
adw_setint.f: adw_setint.ftn 	adw.cdk 	glb_ld.cdk 
adw_trajex.f: adw_trajex.ftn 	adw.cdk 	dcst.cdk 	glb_ld.cdk \
	impnone.cdk 
adw_trajsp.f: adw_trajsp.ftn 	adw.cdk 	dcst.cdk 	glb_ld.cdk 
adw_tricub.f: adw_tricub.ftn 	adw.cdk 	glb_ld.cdk 	impnone.cdk 
adw_tricub_lag3d.f: adw_tricub_lag3d.ftn 	adw.cdk 	glb_ld.cdk 
adw_trilin.f: adw_trilin.ftn 	adw.cdk 	glb_ld.cdk 
adw_trilin_turbo.f: adw_trilin_turbo.ftn 	adw.cdk 	glb_ld.cdk 	v4dcfl.cdk 
adw_vder.f: adw_vder.ftn 	adw.cdk 	geomg.cdk 	glb_ld.cdk \
	impnone.cdk 	schm.cdk 
aggcovernat.f: aggcovernat.ftn 	impnone.cdk 	surfacepar.cdk 
aggcoverurb.f: aggcoverurb.ftn 	impnone.cdk 	surfacepar.cdk 
aggveg.f: aggveg.ftn 	impnone.cdk 	surfacepar.cdk 
agrege1.f: agrege1.ftn 	dimsurf.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	nbvarsurf.cdk 	options.cdk 	phy_macros_f.h \
	sfcbus.cdk 
agvgclas.f: agvgclas.ftn 	impnone.cdk 	surfacepar.cdk 
agvgclas2.f: agvgclas2.ftn 	impnone.cdk 	surfacepar.cdk 
agvgmask.f: agvgmask.ftn 	impnone.cdk 
agvgmask2.f: agvgmask2.ftn 	impnone.cdk 
agvgmos.f: agvgmos.ftn 	classlvls.cdk 	impnone.cdk 	nclassvg.cdk \
	phy_macros_f.h 	phybus.cdk 	surfacepar.cdk 	tuiles.cdk 
allocate_gr_snow.f90: allocate_gr_snow.ftn90 	modd_surf_par.o  \
	modd_type_snow.o 
atmflux.f: atmflux.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 
averaged_albedo_teb.f90: averaged_albedo_teb.ftn90 	modd_snow_par.o  \
	modd_type_snow.o  	mode_surf_snow_frac.o  \
	modi_urban_solar_abs.o 
averaged_tsrad_teb.f90: averaged_tsrad_teb.ftn90 	modd_csts.o  \
	modd_snow_par.o  	modd_type_snow.o  \
	mode_surf_snow_frac.o  	modi_urban_lw_coef.o 
bac.f: bac.ftn 	glb_ld.cdk 	impnone.cdk 	lctl.cdk \
	lun.cdk 	nl.cdk 	p_geof.cdk 	rhsc.cdk \
	schm.cdk 	vt0.cdk 	vtx.cdk 
bacp_2.f: bacp_2.ftn 	cori.cdk 	cstv.cdk 	dcst.cdk \
	geomg.cdk 	glb_ld.cdk 	orh.cdk 	schm.cdk 
baktotq2.f: baktotq2.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
bcastc1.f: bcastc1.ftn 	adw.cdk 	bubble.cdk 	cori.cdk \
	cstv.cdk 	dimout.cdk 	eigv.cdk 	geomg.cdk \
	glb_ld.cdk 	glb_pil.cdk 	grd.cdk 	hblen.cdk \
	hgc.cdk 	hspng.cdk 	hzd.cdk 	init.cdk \
	lctl.cdk 	level.cdk 	mem.cdk 	mtn.cdk \
	offc.cdk 	offline.cdk 	out1.cdk 	out3.cdk \
	p_geol.cdk 	path.cdk 	pilot.cdk 	pres.cdk \
	ptopo.cdk 	schm.cdk 	step.cdk 	theo.cdk \
	tr3d.cdk 	v4dg.cdk 	v4dj.cdk 	v4dm.cdk \
	v4dr.cdk  	v4dz.o 	vrtd.cdk \
	vspng.cdk 	vtopo.cdk 	xst.cdk 	zblen.cdk 
bin2com.f: bin2com.ftn 	glb_ld.cdk 	grd.cdk 	hblen.cdk \
	hgc.cdk 	lun.cdk 	offline.cdk 	out1.cdk \
	p_geol.cdk 	p_pbl.cdk 	pilot.cdk 	schm.cdk \
	tr3d.cdk 
blcloud2.f: blcloud2.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
bld_e_budget.f90: bld_e_budget.ftn90 	modd_csts.o 
blg2.f: blg2.ftn 	impnone.cdk 	maxlev.cdk 
blkcol.f: blkcol.ftn 	out.cdk 
blocstat.f: blocstat.ftn 	adw.cdk 	glb_ld.cdk 	impnone.cdk \
	lctl.cdk 	ptopo.cdk 	step.cdk 	tr3d.cdk \
	v4dg.cdk 	vt1.cdk 
blweight2.f: blweight2.ftn 	impnone.cdk 	phy_macros_f.h 
bmf_perturb.f: bmf_perturb.ftn 	e_anal.cdk 
bourge1.f: bourge1.ftn 	consphy.cdk 	impnone.cdk 
bubble.f: bubble.ftn 	acq.cdk 	acq_int.cdk 	bubble.cdk \
	cstv.cdk 	dcst.cdk 	geomg.cdk 	glb_ld.cdk \
	grd.cdk 	ind.cdk 	lun.cdk 	out1.cdk \
	pres.cdk 	ptopo.cdk 	theo.cdk 	tr3d.cdk \
	vt1.cdk 
bubble_cfg.f: bubble_cfg.ftn 	adw.cdk 	bubble.cdk 	cstv.cdk \
	eigv.cdk 	geomg.cdk 	glb_ld.cdk 	glb_pil.cdk \
	grd.cdk 	hblen.cdk 	hgc.cdk 	hybdim.cdk \
	hzd.cdk 	init.cdk 	lctl.cdk 	lun.cdk \
	mtn.cdk 	offc.cdk 	out1.cdk 	p_geol.cdk \
	pilot.cdk 	pres.cdk 	ptopo.cdk 	schm.cdk \
	step.cdk 	theo.cdk 	theonml.cdk 	tr3d.cdk \
	vspng.cdk 	vtopo.cdk 	zblen.cdk 
c_bloccplg.f: c_bloccplg.ftn 	glb_ld.cdk 	impnone.cdk 	lctl.cdk \
	lun.cdk 	step.cdk 	vt1.cdk 
c_cplg_iml.f: c_cplg_iml.ftn 	impnone.cdk 	lun.cdk 
c_cplg_surf.f: c_cplg_surf.ftn 	c_cplg.cdk 	glb_ld.cdk 	impnone.cdk \
	ptopo.cdk 
c_fillbus.f: c_fillbus.ftn 	busind.cdk 	c_cplg.cdk 	glb_ld.cdk \
	p_bus.cdk 
c_finddim.f: c_finddim.ftn 	impnone.cdk 
c_getbus.f: c_getbus.ftn 	busind.cdk 	c_cplg.cdk 	glb_ld.cdk \
	p_bus.cdk 
c_init.f: c_init.ftn 	impnone.cdk 
c_init_cplfld.f: c_init_cplfld.ftn 	c_cplg.cdk 	c_nml.cdk 	glb_ld.cdk \
	impnone.cdk 	lun.cdk 	p_pbl.cdk 	path.cdk \
	ptopo.cdk 	schm.cdk 
c_init_oasisfld.f: c_init_oasisfld.ftn 	c_cplg.cdk 	impnone.cdk 
c_mtlmap.f: c_mtlmap.ftn	 
c_oasis_get.f90: c_oasis_get.ftn90	 
c_oasis_init.f90: c_oasis_init.ftn90	 
c_oasis_partition.f90: c_oasis_partition.ftn90	 
c_oasis_put.f90: c_oasis_put.ftn90	 
c_screen.f: c_screen.ftn 	impnone.cdk 	lun.cdk 
c_set_coupling.f: c_set_coupling.ftn 	c_cplg.cdk 	cstv.cdk 	geomn.cdk \
	glb_ld.cdk 	impnone.cdk 	ptopo.cdk 	rstr.cdk 
c_stop_coupling.f: c_stop_coupling.ftn 	c_cplg.cdk 	impnone.cdk 
calcalb1.f: calcalb1.ftn 	impnone.cdk 	maxlev.cdk 	options.cdk 
calccovf.f: calccovf.ftn 	impnone.cdk 	maxlev.cdk 	nclassvg.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 	tebcst.cdk 
calcdiag.f: calcdiag.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	options.cdk 	phy_macros_f.h 	phybus.cdk 
calcz0.f: calcz0.ftn 	consphy.cdk 	impnone.cdk 
calddqr.f: calddqr.ftn 	geomg.cdk 	glb_ld.cdk 	impnone.cdk \
	intuv.cdk 	inuvl.cdk 
caldiv_2.f: caldiv_2.ftn 	geomg.cdk 	glb_ld.cdk 
calomeg.f: calomeg.ftn 	geomg.cdk 	glb_ld.cdk 	impnone.cdk 
calvor.f: calvor.ftn 	geomg.cdk 	glb_ld.cdk 	impnone.cdk \
	intuv.cdk 	inuvl.cdk 	schm.cdk 
calzz.f: calzz.ftn 	cstv.cdk 	dcst.cdk 	glb_ld.cdk \
	lctl.cdk 
cartall.f: cartall.ftn 	impnone.cdk 
check_parti.f: check_parti.ftn 	impnone.cdk 	lun.cdk 
class300.f: class300.ftn 	classlvls.cdk 	consphy.cdk 	dimsurf.cdk \
	impnone.cdk 	indx_sfc.cdk 	locbus.cdk 	maxlev.cdk \
	nbvarsurf.cdk 	options.cdk 	phy_macros_f.h 	sfcbus.cdk \
	xptsurf.cdk 	zuzt.cdk 
class320.f: class320.ftn	 
class330.f: class330.ftn 	classlvls.cdk 	consphy.cdk 	dimsurf.cdk \
	impnone.cdk 	indx_sfc.cdk 	locbus.cdk 	maxlev.cdk \
	mosaic.cdk 	nbvarsurf.cdk 	nclassvg.cdk 	options.cdk \
	phy_macros_f.h 	sfcbus.cdk 	tuiles_red.cdk 	xptsurf.cdk \
	zuzt.cdk 
class_const.f: class_const.ftn 	class_com.cdk 
cldoptx4.f: cldoptx4.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
cldwin.f: cldwin.ftn 	comphy.cdk 	consphy.cdk 	dintern.cdk \
	fintern.cdk 	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
climphs3.f: climphs3.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	options.cdk 	phy_macros_f.h 	phybus.cdk \
	surfacepar.cdk 
cllaij.f: cllaij.ftn 	impnone.cdk 	xst.cdk 
clsgs2.f: clsgs2.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
co2info.f: co2info.ftn 	impnone.cdk 
coef_ver_interp_lin2d.f90: coef_ver_interp_lin2d.ftn90 	modd_ver_interp_lin.o 
coherence.f: coherence.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	isbapar.cdk 	maxlev.cdk 	options.cdk 	phy_macros_f.h \
	phybus.cdk 	surfacepar.cdk 
condload.f: condload.ftn 	impnone.cdk 
conds.f: conds.ftn 	comphy.cdk 	consphy.cdk 	dintern.cdk \
	fintern.cdk 	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
conray2.f: conray2.ftn 	consphy.cdk 	impnone.cdk 	valcon.cdk 
conres1.f: conres1.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
consun1.f: consun1.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	fmroft.cdk 	impnone.cdk 	phy_macros_f.h 
copybus.f: copybus.ftn 	dimsurf.cdk 	impnone.cdk 	indx_sfc.cdk \
	nbvarsurf.cdk 	phy_macros_f.h 	sfcbus.cdk 
coriol8.f: coriol8.ftn 	dcst.cdk 	glb_ld.cdk 	lun.cdk \
	p_geol.cdk 	schm.cdk 
coupling_teb.f90: coupling_teb.ftn90 	modd_csts.o  \
	modd_surf_par.o  	modd_teb.o  \
	modd_town.o  	modd_type_snow.o  \
	modi_teb.o 
coupling_teb_orography.f90: coupling_teb_orography.ftn90 	modd_teb.o  \
	modd_type_snow.o  	modi_coupling_teb.o  \
	modi_forcing_vert_shift.o 
ctmdiag.f: ctmdiag.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	phy_macros_f.h 	phybus.cdk 	ribcom.cdk 	vamin.cdk 
datf2p.f: datf2p.ftn 	impnone.cdk 
datp2f.f: datp2f.ftn 	impnone.cdk 
default_prep_teb.f90: default_prep_teb.ftn90 	modd_csts.o  \
	modd_prep_teb.o  	modd_surf_par.o  \
	modd_teb.o  	modd_type_snow.o 
default_teb.f90: default_teb.ftn90 	modd_surf_par.o 
diagno_cw_rad.f: diagno_cw_rad.ftn 	impnone.cdk 	phy_macros_f.h 	phybus.cdk 
diagnosurf.f: diagnosurf.ftn 	impnone.cdk 	indx_sfc.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 
diasurf2.f: diasurf2.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h \
	stabfunc.cdk 	surfcon.cdk 
difuvd1.f: difuvd1.ftn 	impnone.cdk 
difuvd2.f: difuvd2.ftn 	impnone.cdk 
difuvd4.f: difuvd4.ftn 	impnone.cdk 
difuvd4n.f: difuvd4n.ftn 	impnone.cdk 
difuvd8.f: difuvd8.ftn 	impnone.cdk 
difuvd9.f: difuvd9.ftn 	impnone.cdk 
difuvdf.f: difuvdf.ftn 	impnone.cdk 
difuvdfj.f: difuvdfj.ftn 	impnone.cdk 	phy_macros_f.h 
difuvt.f: difuvt.ftn	 
difvef.f: difvef.ftn 	impnone.cdk 
difver6.f: difver6.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	options.cdk 	phy_macros_f.h 	phybus.cdk \
	stk.cdk 
digflt.f: digflt.ftn 	glb_ld.cdk 	impnone.cdk 	init.cdk \
	lctl.cdk 	lun.cdk 	mem.cdk 	p_bus.cdk \
	rstr.cdk 	schm.cdk 	tr3d.cdk 	vt1.cdk \
	vta.cdk 	vtx.cdk 
doout.f: doout.ftn 	dimout.cdk 	init.cdk 	lctl.cdk \
	outd.cdk 	outp.cdk 	rstr.cdk 	timestep.cdk \
	v4dg.cdk 
drag2.f: drag2.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
dtfrz.f: dtfrz.ftn 	impnone.cdk 
dtfrznew.f: dtfrznew.ftn 	impnone.cdk 
dumpbus2.f: dumpbus2.ftn 	buses.cdk 	impnone.cdk 
dvrtdf.f: dvrtdf.ftn 	impnone.cdk 
dvrtef.f: dvrtef.ftn 	impnone.cdk 
dynstep.f: dynstep.ftn 	glb_ld.cdk 	init.cdk 	lctl.cdk \
	lun.cdk 	orh.cdk 	pilot.cdk 	rstr.cdk \
	schm.cdk 	step.cdk 	vtopo.cdk 
e_arak.f: e_arak.ftn 	dcst.cdk 	e_geomg.cdk 	e_grids.cdk \
	e_intuv.cdk 	e_inuvl.cdk 	e_schm.cdk 	impnone.cdk 
e_arret.f: e_arret.ftn 	e_fu.cdk 	impnone.cdk 
e_bmfrd.f: e_bmfrd.ftn 	e_anal.cdk 	e_fu.cdk 	e_grids.cdk \
	e_schm.cdk 	e_topo.cdk 	pilot.cdk 
e_bmfsplitxy2.f: e_bmfsplitxy2.ftn 	bmf.cdk 	impnone.cdk 
e_coefdgf.f: e_coefdgf.ftn 	dcst.cdk 	impnone.cdk 
e_dateo.f: e_dateo.ftn 	bmf.cdk 	e_cdate.cdk 	e_fu.cdk \
	e_grids.cdk 	out1.cdk 	pilot.cdk 
e_decal.f: e_decal.ftn 	impnone.cdk 
e_dynfld.f: e_dynfld.ftn 	bmf.cdk 	e_anal.cdk 	e_cdate.cdk \
	e_fu.cdk 	e_grids.cdk 	e_schm.cdk 	e_topo.cdk \
	hblen.cdk 	out1.cdk 	pilot.cdk 	ptopo.cdk 
e_filtopx.f: e_filtopx.ftn 	e_dim.cdk 	e_geomg.cdk 	e_grids.cdk \
	e_topo.cdk 	impnone.cdk 
e_findmod.f: e_findmod.ftn 	impnone.cdk 
e_gemnml.f: e_gemnml.ftn 	cst_lis.cdk 	dcst.cdk 	e_anal.cdk \
	e_geol.cdk 	e_grids.cdk 	e_nml.cdk 	e_schm.cdk \
	e_topo.cdk 	e_tr.cdk 	grd.cdk 	hblen.cdk \
	offline.cdk 	out1.cdk 	p_pbl.cdk 	pilot.cdk \
	ptopo.cdk 
e_gemntr.f: e_gemntr.ftn 	e_geoerr.cdk 	e_schm.cdk 	p_bus.cdk \
	version.cdk 
e_geopfld.f: e_geopfld.ftn 	e_anal.cdk 	e_cdate.cdk 	e_fu.cdk \
	e_grids.cdk 	e_schm.cdk 	e_topo.cdk 	hgc.cdk \
	p_bus.cdk 	p_geobus.cdk 	p_geobusid.cdk 
e_geopini.f: e_geopini.ftn 	e_fu.cdk 	e_geol.cdk 	glb_ld.cdk \
	p_bus.cdk 	p_geobus.cdk 	p_geobusid.cdk 	p_pbl.cdk 
e_grid.f: e_grid.ftn 	e_grids.cdk 	grd.cdk 	hgc.cdk 
e_infiles.f: e_infiles.ftn 	filename.cdk 
e_int_ix2xu.f: e_int_ix2xu.ftn 	e_grids.cdk 	e_intuv.cdk 	e_inuvl.cdk \
	e_schm.cdk 	impnone.cdk 
e_int_iy2yv.f: e_int_iy2yv.ftn 	e_grids.cdk 	e_intuv.cdk 	e_inuvl.cdk \
	e_schm.cdk 	impnone.cdk 
e_intgeo.f: e_intgeo.ftn 	e_cdate.cdk 	e_fu.cdk 	e_geoerr.cdk \
	e_grids.cdk 	e_schm.cdk 	hgc.cdk 	p_bus.cdk 
e_intpoly.f: e_intpoly.ftn 	impnone.cdk 
e_intthm.f: e_intthm.ftn 	dcst.cdk 	e_anal.cdk 	e_fu.cdk \
	e_grids.cdk 	e_option.cdk 	e_schm.cdk 	e_tr.cdk \
	hgc.cdk 
e_intwind.f: e_intwind.ftn 	bmf.cdk 	dcst.cdk 	e_anal.cdk \
	e_cdate.cdk 	e_fu.cdk 	e_grids.cdk 	e_option.cdk \
	e_schm.cdk 	e_topo.cdk 	grd.cdk 	hgc.cdk 
e_liaccu.f: e_liaccu.ftn 	e_anal.cdk 	e_fu.cdk 	e_grids.cdk \
	e_schm.cdk 	e_topo.cdk 	offline.cdk 	pilot.cdk 
e_moclim2.f: e_moclim2.ftn 	impnone.cdk 
e_modgl.f: e_modgl.ftn 	dcst.cdk 	e_geol.cdk 	e_grids.cdk \
	impnone.cdk 	p_bus.cdk 	p_geobus.cdk 	p_geobusid.cdk 
e_modhs.f: e_modhs.ftn 	dcst.cdk 	e_geol.cdk 	e_grids.cdk \
	impnone.cdk 	p_bus.cdk 	p_geobus.cdk 	p_geobusid.cdk 
e_modmg.f: e_modmg.ftn 	dcst.cdk 	e_geol.cdk 	e_grids.cdk \
	impnone.cdk 	p_bus.cdk 	p_geobus.cdk 	p_geobusid.cdk 
e_nml2bin.f: e_nml2bin.ftn 	dimout.cdk 	e_grids.cdk 	e_schm.cdk \
	e_tr.cdk 	grd.cdk 	hblen.cdk 	hgc.cdk \
	offline.cdk 	out1.cdk 	p_pbl.cdk 	pilot.cdk \
	ptopo.cdk 
e_ntrxyfil.f: e_ntrxyfil.ftn 	dcst.cdk 	e_geomg.cdk 	e_grids.cdk \
	impnone.cdk 
e_pilotf.f: e_pilotf.ftn 	e_fu.cdk 	filename.cdk 
e_pregeo.f: e_pregeo.ftn 	dcst.cdk 	e_geol.cdk 	e_grids.cdk \
	hgc.cdk 	p_bus.cdk 	p_geobus.cdk 	p_geobusid.cdk \
	p_pbl.cdk 
e_premoy.f: e_premoy.ftn 	impnone.cdk 
e_pretopo.f: e_pretopo.ftn 	dcst.cdk 	e_dim.cdk 	e_geoma.cdk \
	e_geomg.cdk 	e_grids.cdk 	e_topo.cdk 
e_rdhint3.f: e_rdhint3.ftn 	e_anal.cdk 	e_cdate.cdk 	e_grids.cdk \
	hgc.cdk 	impnone.cdk 
e_screen.f: e_screen.ftn 	impnone.cdk 
e_setdgf.f: e_setdgf.ftn 	e_dgf.cdk 	e_grids.cdk 	e_topo.cdk \
	impnone.cdk 
e_setintuv.f: e_setintuv.ftn 	dcst.cdk 	e_geomg.cdk 	e_grids.cdk \
	e_intuv.cdk 	e_schm.cdk 	impnone.cdk 
e_setinuvl.f: e_setinuvl.ftn 	dcst.cdk 	e_geomg.cdk 	e_grids.cdk \
	e_inuvl.cdk 	impnone.cdk 
e_sgrid.f: e_sgrid.ftn	 
e_sortr.f: e_sortr.ftn 	impnone.cdk 
e_specanal.f: e_specanal.ftn 	bmf.cdk 	e_anal.cdk 	e_cdate.cdk \
	e_fu.cdk 	e_grids.cdk 	e_option.cdk 	e_schm.cdk \
	out1.cdk 	ptopo.cdk 
e_trinit.f: e_trinit.ftn 	e_tr.cdk 
e_wlogntr.f: e_wlogntr.ftn 	e_option.cdk 	impnone.cdk 	out1.cdk 
e_wrgeop.f: e_wrgeop.ftn 	e_grids.cdk 	impnone.cdk 	lun.cdk \
	p_bus.cdk 	p_geobus.cdk 	p_geobusid.cdk 
ebudget.f: ebudget.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
ecmwf_ab.f: ecmwf_ab.ftn 	impnone.cdk 
ecris_fst2.f: ecris_fst2.ftn 	out.cdk 
ecris_fst2m.f: ecris_fst2m.ftn 	out.cdk 
emicrog.f: emicrog.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 	sedipara.cdk 
envirtht.f: envirtht.ftn 	impnone.cdk 
equivmount.f: equivmount.ftn 	impnone.cdk 	phy_macros_f.h 
eturbl6.f: eturbl6.ftn 	clefcon.cdk 	consphy.cdk 	impnone.cdk \
	machcon.cdk 	maxlev.cdk 	options.cdk 	phy_macros_f.h \
	surfcon.cdk 
extdiag.f: extdiag.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 	sercmdk.cdk 
fcpara2.f: fcpara2.ftn 	impnone.cdk 	phy_macros_f.h 
fcrest5.f: fcrest5.ftn 	clefcon.cdk 	consphy.cdk 	dintern.cdk \
	fintern.cdk 	fintern90.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	scfrst.cdk 	surfcon.cdk 
feseri.f: feseri.ftn 	consphy.cdk 	impnone.cdk 
ficemxp.f: ficemxp.ftn 	impnone.cdk 
fillagg.f: fillagg.ftn 	dimsurf.cdk 	impnone.cdk 	indx_sfc.cdk \
	nbvarsurf.cdk 	phy_macros_f.h 	sfcbus.cdk 	xptsurf.cdk 
filter.f: filter.ftn 	geomg.cdk 	glb_ld.cdk 	impnone.cdk 
flxsurf3.f: flxsurf3.ftn 	consphy.cdk 	impnone.cdk 	stabfunc.cdk \
	surfcon.cdk 
fomichev.f: fomichev.ftn 	impnone.cdk 
force_restore.f: force_restore.ftn 	clefcon.cdk 	consphy.cdk 	dimsurf.cdk \
	dintern.cdk 	fintern.cdk 	fintern90.cdk 	impnone.cdk \
	indx_sfc.cdk 	locbus.cdk 	machcon.cdk 	maxlev.cdk \
	nbvarsurf.cdk 	options.cdk 	phy_macros_f.h 	scfrst.cdk \
	sfcbus.cdk 	surfcon.cdk 	vamin.cdk 	xptsurf.cdk \
	zuzt.cdk 
forcing_vert_shift.f90: forcing_vert_shift.ftn90 	modd_atm_cst.o  \
	modd_csts.o  	mode_thermos.o 
frstgss.f: frstgss.ftn 	cori.cdk 	glb_ld.cdk 	impnone.cdk \
	lun.cdk 	schm.cdk 	vt0.cdk 	vt1.cdk 
ftridi6.f: ftridi6.ftn 	impnone.cdk 
ftridi7.f: ftridi7.ftn 	impnone.cdk 
gather.f: gather.ftn 	impnone.cdk 
gefstop.f: gefstop.ftn 	impnone.cdk 	lun.cdk 
geleyn.f: geleyn.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 
gem_ctrl.f: gem_ctrl.ftn 	impnone.cdk 	init.cdk 	lctl.cdk \
	lun.cdk 	rstr.cdk 	step.cdk 	v4dg.cdk 
gem_run.f: gem_run.ftn 	glb_ld.cdk 	hzd.cdk 	init.cdk \
	lctl.cdk 	lun.cdk 	p_clim.cdk 	rstr.cdk \
	schm.cdk 	step.cdk 	v4dg.cdk 	vrtd.cdk 
gem_stop.f: gem_stop.ftn 	lun.cdk 
gemdm.f: gemdm.ftn 	lctl.cdk 	lun.cdk 	mult.cdk \
	step.cdk 	v4dg.cdk 
gemtim.f: gemtim.ftn	 
genab.f: genab.ftn 	impnone.cdk 
genab2.f: genab2.ftn	 
geneigl.f: geneigl.ftn 	lun.cdk 
geophy.f: geophy.ftn 	impnone.cdk 
gesdict.f: gesdict.ftn 	buses.cdk 	impnone.cdk 
get_luout.f90: get_luout.ftn90	 
getbpm.f: getbpm.ftn 	impnone.cdk 
getbus1.f: getbus1.ftn 	buses.cdk 	impnone.cdk 
getindx.f: getindx.ftn 	impnone.cdk 	p_bus.cdk 
getp0.f: getp0.ftn 	dcst.cdk 	impnone.cdk 
glaciers1.f: glaciers1.ftn 	consphy.cdk 	dimsurf.cdk 	dintern.cdk \
	fintern.cdk 	fintern90.cdk 	impnone.cdk 	indx_sfc.cdk \
	locbus.cdk 	maxlev.cdk 	nbvarsurf.cdk 	options.cdk \
	phy_macros_f.h 	sfcbus.cdk 	snoh0.cdk 	vamin.cdk \
	xptsurf.cdk 	zuzt.cdk 
glb_bound.f: glb_bound.ftn 	glb_ld.cdk 	lun.cdk 	schm.cdk \
	theo.cdk 	zblen.cdk 
glbcolc.f: glbcolc.ftn 	impnone.cdk 
glbdist.f: glbdist.ftn 	glb_ld.cdk 	impnone.cdk 
glbpos.f: glbpos.ftn 	glb_ld.cdk 	impnone.cdk 	lun.cdk \
	ptopo.cdk 
glbstat.f: glbstat.ftn 	glb_ld.cdk 	impnone.cdk 	lctl.cdk \
	ptopo.cdk 
gwd5.f: gwd5.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 
gwdfx95a.f: gwdfx95a.ftn 	impnone.cdk 	phy_macros_f.h 
gz2p0.f: gz2p0.ftn 	dcst.cdk 	geomg.cdk 	glb_ld.cdk \
	impnone.cdk 	lun.cdk 
gzcalcul.f: gzcalcul.ftn 	impnone.cdk 
hatoprg.f: hatoprg.ftn 	glb_ld.cdk 	impnone.cdk 
hdif0.f: hdif0.ftn 	glb_ld.cdk 	lctl.cdk 	v4dg.cdk 
hdif_phy.f: hdif_phy.ftn 	glb_ld.cdk 	lctl.cdk 	schm.cdk \
	v4dg.cdk 
heatcoef.f: heatcoef.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
height_sponge.f: height_sponge.ftn 	dcst.cdk 	geomg.cdk 	glb_ld.cdk \
	glb_pil.cdk 	lun.cdk 	mtn.cdk 	nest.cdk \
	p_geof.cdk 	schm.cdk 	vt0.cdk 	zblen.cdk 
hor_interpol.f90: hor_interpol.ftn90 	modd_prep.o 
horwavg.f: horwavg.ftn 	dcst.cdk 	geomg.cdk 	glb_ld.cdk \
	impnone.cdk 	ptopo.cdk 
hspng_abc.f: hspng_abc.ftn 	glb_ld.cdk 	grd.cdk 	hspng.cdk \
	impnone.cdk 	ldnh.cdk 	trp.cdk 
hspng_drv.f: hspng_drv.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	hspng.cdk 	hzd.cdk 	impnone.cdk \
	ldnh.cdk 	lun.cdk 	opr.cdk 	ptopo.cdk \
	schm.cdk 	trp.cdk 
hspng_main.f: hspng_main.ftn 	cstv.cdk 	geomg.cdk 	glb_ld.cdk \
	hspng.cdk 	impnone.cdk 	lun.cdk 	p_geof.cdk \
	schm.cdk 	vt0.cdk 	vt1.cdk 	vtx.cdk 
hydro1.f: hydro1.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	isbapar.cdk 	phy_macros_f.h \
	surfacepar.cdk 
hzd_abc.f: hzd_abc.ftn 	glb_ld.cdk 	grd.cdk 	hzd.cdk \
	impnone.cdk 	trp.cdk 
hzd_areal.f: hzd_areal.ftn 	glb_ld.cdk 	glb_pil.cdk 	impnone.cdk 
hzd_areav.f: hzd_areav.ftn 	impnone.cdk 
hzd_bfct.f: hzd_bfct.ftn 	glb_ld.cdk 	glb_pil.cdk 	impnone.cdk 
hzd_bfct_1d.f: hzd_bfct_1d.ftn 	glb_ld.cdk 	glb_pil.cdk 	impnone.cdk 
hzd_del2.f: hzd_del2.ftn 	glb_ld.cdk 	ptopo.cdk 
hzd_delpwr.f: hzd_delpwr.ftn 	glb_ld.cdk 	glb_pil.cdk 	impnone.cdk 
hzd_delpwr_1d.f: hzd_delpwr_1d.ftn 	glb_ld.cdk 	glb_pil.cdk 	impnone.cdk 
hzd_exhrdif.f: hzd_exhrdif.ftn 	glb_ld.cdk 	hzd.cdk 	impnone.cdk 
hzd_fact.f: hzd_fact.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	hzd.cdk 	ldnh.cdk 	lun.cdk \
	opr.cdk 	ptopo.cdk 	schm.cdk 	trp.cdk 
hzd_ho.f: hzd_ho.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	glb_pil.cdk 	hzd.cdk 	ldnh.cdk \
	lun.cdk 	opr.cdk 	ptopo.cdk 	schm.cdk \
	trp.cdk 
hzd_ho_parite.f: hzd_ho_parite.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	hzd.cdk 	ldnh.cdk 	lun.cdk \
	opr.cdk 	ptopo.cdk 	schm.cdk 	trp.cdk 
hzd_hof.f: hzd_hof.ftn 	cstv.cdk 	dcst.cdk 	fft.cdk \
	geomg.cdk 	glb_ld.cdk 	hzd.cdk 	ldnh.cdk \
	lun.cdk 	opr.cdk 	ptopo.cdk 	schm.cdk \
	trp.cdk 
hzd_hof_lam.f: hzd_hof_lam.ftn 	cstv.cdk 	dcst.cdk 	fft.cdk \
	geomg.cdk 	glb_ld.cdk 	hzd.cdk 	impnone.cdk \
	ldnh.cdk 	lun.cdk 	opr.cdk 	ptopo.cdk \
	schm.cdk 	trp.cdk 
hzd_hoffld.f: hzd_hoffld.ftn 	fft.cdk 	geomg.cdk 	glb_ld.cdk \
	hzd.cdk 	lun.cdk 	opr.cdk 	ptopo.cdk \
	trp.cdk 
hzd_main.f: hzd_main.ftn 	cstv.cdk 	eigv.cdk 	fft.cdk \
	geomg.cdk 	glb_ld.cdk 	grd.cdk 	hzd.cdk \
	lctl.cdk 	lun.cdk 	p_geof.cdk 	schm.cdk \
	vt0.cdk 	vt1.cdk 	vtx.cdk 
hzd_nudeln.f: hzd_nudeln.ftn 	glb_ld.cdk 
hzd_set.f: hzd_set.ftn 	cstv.cdk 	dcst.cdk 	eigv.cdk \
	fft.cdk 	geomg.cdk 	glb_ld.cdk 	glb_pil.cdk \
	hspng.cdk 	hzd.cdk 	impnone.cdk 	lun.cdk \
	opr.cdk 	ptopo.cdk 	trp.cdk 	vspng.cdk 
hzd_set_ops8.f: hzd_set_ops8.ftn 	impnone.cdk 
hzd_solfft.f: hzd_solfft.ftn 	ptopo.cdk 
hzd_solfft_1d.f: hzd_solfft_1d.ftn 	ptopo.cdk 
hzd_solfft_lam.f: hzd_solfft_lam.ftn 	glb_ld.cdk 	glb_pil.cdk 	impnone.cdk \
	ptopo.cdk 
hzd_solmxma.f: hzd_solmxma.ftn 	glb_ld.cdk 	glb_pil.cdk 	ptopo.cdk 
hzd_solparite_2.f: hzd_solparite_2.ftn 	ptopo.cdk 
hzd_solparite_u.f: hzd_solparite_u.ftn 	ptopo.cdk 
incdatsd.f: incdatsd.ftn	 
inctphy.f: inctphy.ftn 	consphy.cdk 	impnone.cdk 
inctphy3.f: inctphy3.ftn 	consphy.cdk 	impnone.cdk 
indata.f: indata.ftn 	bmf.cdk 	glb_ld.cdk 	ind.cdk \
	lctl.cdk 	out1.cdk 	p_geof.cdk 	schm.cdk \
	theo.cdk 	v4dg.cdk 	vt1.cdk 	vtopo.cdk \
	vtx.cdk 
indseri.f: indseri.ftn 	impnone.cdk 
ini_csts.f90: ini_csts.ftn90 	modd_csts.o 
inichamp2.f: inichamp2.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	options.cdk 	phy_macros_f.h 	phybus.cdk 
inicover.f: inicover.ftn 	classlvls.cdk 	impnone.cdk 	maxlev.cdk \
	mosaic.cdk 	nclassvg.cdk 	options.cdk 	phy_macros_f.h \
	phybus.cdk 	tuiles.cdk 
inicoverurb.f: inicoverurb.ftn 	impnone.cdk 	maxlev.cdk 	nclassvg.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 	tebcst.cdk 
inifcp.f: inifcp.ftn 	impnone.cdk 
inikey.f: inikey.ftn 	busind.cdk 	c_cplg.cdk 	glb_ld.cdk \
	p_cond.cdk 	schm.cdk 	tr3d.cdk 
inimos.f: inimos.ftn 	classlvls.cdk 	impnone.cdk 	maxlev.cdk \
	mosaic.cdk 	nclassvg.cdk 	options.cdk 	phy_macros_f.h \
	phybus.cdk 	tuiles.cdk 
iniobus.f: iniobus.ftn 	dcst.cdk 	dimout.cdk 	glb_ld.cdk \
	glb_pil.cdk 	impnone.cdk 	lctl.cdk 	lun.cdk \
	obus.cdk 	outp.cdk 	p_bus.cdk 
iniptsurf.f: iniptsurf.ftn 	buses.cdk 	dimsurf.cdk 	impnone.cdk \
	indx_sfc.cdk 	maxlev.cdk 	nbvarsurf.cdk 	options.cdk \
	phy_macros_f.h 	sfcbus.cdk 
iniras.f: iniras.ftn 	impnone.cdk 	rasdata.cdk 
inisoili.f: inisoili.ftn 	impnone.cdk 	phy_macros_f.h 	phybus.cdk 
inisurf1.f: inisurf1.ftn 	classlvls.cdk 	consphy.cdk 	himin.cdk \
	icelvls.cdk 	impnone.cdk 	indx_sfc.cdk 	isbapar.cdk \
	leads.cdk 	maxlev.cdk 	mosaic.cdk 	nclassvg.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 	surfacepar.cdk 
init_snow_lw.f90: init_snow_lw.ftn90 	modd_csts.o  \
	modd_snow_par.o  	modd_surf_par.o  \
	modd_type_snow.o  	mode_snow3l.o 
init_teb.f90: init_teb.ftn90 	modd_snow_par.o  \
	modd_surf_par.o  	modd_teb.o  \
	modd_town.o  	modd_type_snow.o  \
	modi_averaged_albedo_teb.o  	modi_averaged_tsrad_teb.o  \
	modi_default_teb.o  	modi_init_snow_lw.o 
initial.f: initial.ftn 	cstv.cdk 	dcst.cdk 	glb_ld.cdk \
	impnone.cdk 	init.cdk 	lctl.cdk 	lun.cdk \
	rstr.cdk 	schm.cdk 	step.cdk 
initown.f90: initown.ftn90 	impnone.cdk 	indx_sfc.cdk 	maxlev.cdk \
 	modd_csts.o  	modd_prep.o \
 	modd_surf_par.o  	modd_teb.o \
 	modd_teb_grid.o  	modd_town.o \
 	modd_type_snow.o  	modi_allocate_gr_snow.o \
 	modi_ini_csts.o  	modi_init_teb.o \
 	modi_possol.o  	modi_prep_teb.o \
 	modi_sunpos.o 	options.cdk 	phy_macros_f.h \
	phybus.cdk 	tebcst.cdk 
initw2.f: initw2.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	intuv.cdk 	inuvl.cdk 	schm.cdk 
iniveg.f: iniveg.ftn 	classlvls.cdk 	impnone.cdk 	maxlev.cdk \
	nclassvg.cdk 	options.cdk 	phy_macros_f.h 	phybus.cdk 
intchamps.f: intchamps.ftn 	impnone.cdk 
integ2.f: integ2.ftn 	impnone.cdk 
interp_grid1d.f90: interp_grid1d.ftn90 	modd_surf_par.o  \
	modi_coef_ver_interp_lin2d.o  	modi_ver_interp_lin2d.o 
interp_grid2d.f90: interp_grid2d.ftn90 	modd_surf_par.o  \
	modi_coef_ver_interp_lin2d.o  	modi_ver_interp_lin2d.o 
interpveg.f: interpveg.ftn 	impnone.cdk 
intodif.f: intodif.ftn 	impnone.cdk 
intozon.f: intozon.ftn 	impnone.cdk 	ozopnt.cdk 	raddata.cdk \
	radparam.cdk 
intwat3.f: intwat3.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
inverse.f: inverse.ftn 	impnone.cdk 
ipig.f: ipig.ftn 	impnone.cdk 
isba2.f: isba2.ftn 	clefcon.cdk 	consphy.cdk 	dimsurf.cdk \
	impnone.cdk 	indx_sfc.cdk 	locbus.cdk 	maxlev.cdk \
	nbvarsurf.cdk 	options.cdk 	phy_macros_f.h 	sfcbus.cdk \
	xptsurf.cdk 	zuzt.cdk 
juliand.f: juliand.ftn 	impnone.cdk 
kdifsimp.f: kdifsimp.ftn 	consphy.cdk 	impnone.cdk 
kfcp4.f: kfcp4.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 
ktrsnt3.f: ktrsnt3.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
ktrsnt_mg.f: ktrsnt_mg.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	maxlev.cdk 	options.cdk \
	phy_macros_f.h 
kuo2.f: kuo2.ftn 	comphy.cdk 	consphy.cdk 	dintern.cdk \
	fintern.cdk 	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
kuostd.f: kuostd.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
kuosun.f: kuosun.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
kuosym.f: kuosym.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
lacs.f: lacs.ftn 	consphy.cdk 	impnone.cdk 	leads.cdk \
	phy_macros_f.h 	phybus.cdk 	surfacepar.cdk 
lacs1.f: lacs1.ftn 	consphy.cdk 	impnone.cdk 	leads.cdk \
	phy_macros_f.h 	phybus.cdk 	surfacepar.cdk 
liebman_2.f: liebman_2.ftn 	impnone.cdk 
lin_adjtq.f: lin_adjtq.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
lin_adjtq_ad.f: lin_adjtq_ad.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
lin_adjtq_tl.f: lin_adjtq_tl.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
lin_conds1.f: lin_conds1.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
lin_conds1_ad.f: lin_conds1_ad.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
lin_conds1_tl.f: lin_conds1_tl.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
lin_diff_vert1.f: lin_diff_vert1.ftn 	impnone.cdk 
lin_diff_vert1_ad.f: lin_diff_vert1_ad.ftn 	impnone.cdk 
lin_diff_vert1_tl.f: lin_diff_vert1_tl.ftn 	impnone.cdk 
lin_difuvd1.f: lin_difuvd1.ftn	 
lin_difuvd1_ad.f: lin_difuvd1_ad.ftn	 
lin_difuvd1_tl.f: lin_difuvd1_tl.ftn	 
lin_difuvd2.f: lin_difuvd2.ftn	 
lin_difuvd2_ad.f: lin_difuvd2_ad.ftn	 
lin_difuvd2_tl.f: lin_difuvd2_tl.ftn	 
lin_difver1.f: lin_difver1.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	options.cdk 	phy_macros_f.h 	phybus.cdk \
	stk.cdk 
lin_difver1_ad.f: lin_difver1_ad.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	options.cdk 	phy_macros_f.h 	phybus.cdk \
	stk.cdk 
lin_difver1_tl.f: lin_difver1_tl.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	options.cdk 	phy_macros_f.h 	phybus.cdk \
	stk.cdk 
lin_inichamp1.f: lin_inichamp1.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	options.cdk 	phy_macros_f.h 	phybus.cdk 
lin_inichamp1_tl.f: lin_inichamp1_tl.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	options.cdk 	phy_macros_f.h 	phybus.cdk 
lin_kdif_simp1.f: lin_kdif_simp1.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	indx_sfc.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 	surfcon.cdk \
	zuzt.cdk 
lin_kdif_simp1_ad.f: lin_kdif_simp1_ad.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	indx_sfc.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 	zuzt.cdk 
lin_kdif_simp1_tl.f: lin_kdif_simp1_tl.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	indx_sfc.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 	zuzt.cdk 
lin_kdif_simp1_tr.f: lin_kdif_simp1_tr.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	indx_sfc.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 	surfcon.cdk \
	zuzt.cdk 
lin_kuosym.f: lin_kuosym.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
lin_kuosym_ad.f: lin_kuosym_ad.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
lin_kuosym_tl.f: lin_kuosym_tl.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
lin_phyexe1.f: lin_phyexe1.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 	workspc.cdk 
lin_phyexe1_ad.f: lin_phyexe1_ad.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 	workspc.cdk 
lin_phyexe1_tl.f: lin_phyexe1_tl.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 	workspc.cdk 
lin_precip1.f: lin_precip1.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 
lin_precip1_ad.f: lin_precip1_ad.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 
lin_precip1_tl.f: lin_precip1_tl.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 
lin_sgo1.f: lin_sgo1.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 
lin_sgo1_ad.f: lin_sgo1_ad.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 
lin_sgo1_tl.f: lin_sgo1_tl.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 
lin_sgoflx1.f: lin_sgoflx1.ftn 	impnone.cdk 	phy_macros_f.h 
lin_sgoflx1_ad.f: lin_sgoflx1_ad.ftn 	impnone.cdk 	phy_macros_f.h 
lin_sgoflx1_tl.f: lin_sgoflx1_tl.ftn 	impnone.cdk 	phy_macros_f.h 
liqwc.f: liqwc.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 
litblrad2.f: litblrad2.ftn 	consphy.cdk 	impnone.cdk 	ozopnt.cdk \
	phy_macros_f.h 	raddata.cdk 	radparam.cdk 	radpnt.cdk 
litozon2.f: litozon2.ftn 	impnone.cdk 	ozopnt.cdk 	raddata.cdk \
	radparam.cdk 
llacar.f: llacar.ftn 	impnone.cdk 
lsctrol.f: lsctrol.ftn 	impnone.cdk 	phy_macros_f.h 
mconadj2.f: mconadj2.ftn 	acmcon.cdk 	consphy.cdk 	dintern.cdk \
	fintern.cdk 	fintern90.cdk 	flatscp.cdk 	impnone.cdk \
	phy_macros_f.h 
mesahr.f: mesahr.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	phy_macros_f.h 
mesahu.f: mesahu.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr2.cdk \
	phy_macros_f.h 
mfdlesmx.f: mfdlesmx.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
mfodla.f: mfodla.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
mfodle.f: mfodle.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
mfodqa.f: mfodqa.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr2.cdk \
	phy_macros_f.h 
mfodqs.f: mfodqs.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr2.cdk \
	phy_macros_f.h 
mfoefq.f: mfoefq.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr2.cdk \
	phy_macros_f.h 
mfoeic.f: mfoeic.ftn 	consphy.cdk 	impnone.cdk 	initcph.cdk \
	phy_macros_f.h 
mfoew.f: mfoew.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
mfoewa.f: mfoewa.ftn 	consphy.cdk 	impnone.cdk 	initcph.cdk \
	phy_macros_f.h 
mfohr.f: mfohr.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr2.cdk \
	phy_macros_f.h 
mfohra.f: mfohra.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr2.cdk \
	phy_macros_f.h 
mfoqfe.f: mfoqfe.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr2.cdk \
	phy_macros_f.h 
mfoqsa.f: mfoqsa.ftn 	consphy.cdk 	impnone.cdk 	initcph.cdk \
	modpr2.cdk 	phy_macros_f.h 
mfoqst.f: mfoqst.ftn 	consphy.cdk 	impnone.cdk 	initcph.cdk \
	modpr2.cdk 	phy_macros_f.h 
mfottv.f: mfottv.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
mfottv_ad.f: mfottv_ad.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
mfottv_tl.f: mfottv_tl.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
mfottvh.f: mfottvh.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
mfotvht.f: mfotvht.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
mfotvt.f: mfotvt.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
mfotvt_ad.f: mfotvt_ad.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
mfotvt_tl.f: mfotvt_tl.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
mgsdict.f: mgsdict.ftn 	impnone.cdk 	p_bus.cdk 
mhraes.f: mhraes.ftn 	consphy.cdk 	impnone.cdk 	initcph.cdk \
	phy_macros_f.h 
mhrahu.f: mhrahu.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr2.cdk \
	phy_macros_f.h 
mhuaes.f: mhuaes.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr2.cdk \
	phy_macros_f.h 
mhuahr.f: mhuahr.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr2.cdk \
	phy_macros_f.h 
mirror.f: mirror.ftn 	glb_ld.cdk 	lun.cdk 	schm.cdk \
	tr3d.cdk 	vt0.cdk 
mixlen3.f: mixlen3.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 	scfrst.cdk 
mixphase6.f: mixphase6.ftn 	consphy.cdk 	dzcond.cdk 	impnone.cdk \
	maxlev.cdk 	options.cdk 	phy_macros_f.h 
mnhget_luout.f90: mnhget_luout.ftn90	 
modd_atm_cst.f90: modd_atm_cst.cdk90  
modd_csts.f90: modd_csts.cdk90                                
modd_data_cover_par.f90: modd_data_cover_par.cdk90    
modd_isba.f90: modd_isba.cdk90   	modd_type_date.o   	modd_type_snow.o    
modd_prep.f90: modd_prep.cdk90          
modd_prep_snow.f90: modd_prep_snow.cdk90   	modd_prep.o   	modd_snow_par.o   
modd_prep_teb.f90: modd_prep_teb.cdk90    
modd_snow_par.f90: modd_snow_par.cdk90               
modd_surf_par.f90: modd_surf_par.cdk90                    
modd_teb.f90: modd_teb.cdk90         	modd_type_snow.o     
modd_teb_grid.f90: modd_teb_grid.cdk90      
modd_town.f90: modd_town.cdk90          
modd_type_date.f90: modd_type_date.cdk90     
modd_type_snow.f90: modd_type_snow.cdk90                           
modd_ver_interp_lin.f90: modd_ver_interp_lin.cdk90  
mode_snow3l.f90: mode_snow3l.cdk90    	modd_csts.o   	modd_snow_par.o   	modd_surf_par.o   
mode_surf_snow_frac.f90: mode_surf_snow_frac.cdk90     	modd_snow_par.o   
mode_thermos.f90: mode_thermos.cdk90    	modd_csts.o        
modhui.f: modhui.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr2.cdk \
	phy_macros_f.h 
modi_allocate_gr_snow.f90: modi_allocate_gr_snow.cdk90     	modd_type_snow.o    
modi_averaged_albedo_teb.f90: modi_averaged_albedo_teb.cdk90    	modd_type_snow.o 
modi_averaged_tsrad_teb.f90: modi_averaged_tsrad_teb.cdk90    	modd_type_snow.o 
modi_bld_e_budget.f90: modi_bld_e_budget.cdk90  
modi_coef_ver_interp_lin2d.f90: modi_coef_ver_interp_lin2d.cdk90   
modi_coupling_teb.f90: modi_coupling_teb.cdk90  
modi_coupling_teb_orography.f90: modi_coupling_teb_orography.cdk90   
modi_default_prep_teb.f90: modi_default_prep_teb.cdk90  
modi_default_teb.f90: modi_default_teb.cdk90  
modi_forcing_vert_shift.f90: modi_forcing_vert_shift.cdk90  
modi_get_luout.f90: modi_get_luout.cdk90  
modi_hor_interpol.f90: modi_hor_interpol.cdk90   
modi_ini_csts.f90: modi_ini_csts.cdk90     
modi_init_snow_lw.f90: modi_init_snow_lw.cdk90    	modd_type_snow.o 
modi_init_teb.f90: modi_init_teb.cdk90   
modi_interp_grid1d.f90: modi_interp_grid1d.cdk90  
modi_interp_grid2d.f90: modi_interp_grid2d.cdk90   
modi_mnhget_luout.f90: modi_mnhget_luout.cdk90 
modi_possol.f90: modi_possol.cdk90     
modi_prep_hor_snow_field.f90: modi_prep_hor_snow_field.cdk90   	modd_type_snow.o  
modi_prep_hor_snow_fields.f90: modi_prep_hor_snow_fields.cdk90   	modd_type_snow.o  
modi_prep_hor_teb_field.f90: modi_prep_hor_teb_field.cdk90  
modi_prep_snow_gem.f90: modi_prep_snow_gem.cdk90  
modi_prep_teb.f90: modi_prep_teb.cdk90   
modi_prep_teb_gem.f90: modi_prep_teb_gem.cdk90  
modi_road_wall_layer_e_budget.f90: modi_road_wall_layer_e_budget.cdk90  
modi_roof_layer_e_budget.f90: modi_roof_layer_e_budget.cdk90  
modi_snow_cover_1layer.f90: modi_snow_cover_1layer.cdk90  
modi_snow_heat_to_t_wliq.f90: modi_snow_heat_to_t_wliq.cdk90  
modi_sunpos.f90: modi_sunpos.cdk90     
modi_surface_aero_cond.f90: modi_surface_aero_cond.cdk90   
modi_surface_cd.f90: modi_surface_cd.cdk90  
modi_surface_ri.f90: modi_surface_ri.cdk90   
modi_teb.f90: modi_teb.cdk90  
modi_tridiag_ground.f90: modi_tridiag_ground.cdk90   
modi_urban_drag.f90: modi_urban_drag.cdk90  
modi_urban_fluxes.f90: modi_urban_fluxes.cdk90  
modi_urban_hydro.f90: modi_urban_hydro.cdk90  
modi_urban_lw_coef.f90: modi_urban_lw_coef.cdk90     
modi_urban_snow_evol.f90: modi_urban_snow_evol.cdk90  
modi_urban_solar_abs.f90: modi_urban_solar_abs.cdk90   
modi_vegtype_grid_to_patch_grid.f90: modi_vegtype_grid_to_patch_grid.cdk90   	modd_isba.o   	modd_type_date.o   	modd_type_snow.o  
modi_vegtype_to_patch.f90: modi_vegtype_to_patch.cdk90  
modi_ver_interp_lin2d.f90: modi_ver_interp_lin2d.cdk90   
modpbus.f: modpbus.ftn 	dimsurf.cdk 	impnone.cdk 	indx_sfc.cdk \
	nbvarsurf.cdk 	phy_macros_f.h 	phybus.cdk 
moistke3.f: moistke3.ftn 	clefcon.cdk 	consphy.cdk 	impnone.cdk \
	machcon.cdk 	maxlev.cdk 	options.cdk 	phy_macros_f.h \
	surfcon.cdk 
momcoef.f: momcoef.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
mosavg.f: mosavg.ftn 	dimsurf.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	nbvarsurf.cdk 	options.cdk 	phy_macros_f.h \
	sfcbus.cdk 
moscopy.f: moscopy.ftn 	dimsurf.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	nbvarsurf.cdk 	options.cdk 	phy_macros_f.h \
	sfcbus.cdk 
moscover.f: moscover.ftn 	classlvls.cdk 	impnone.cdk 	nclassvg.cdk \
	tuiles.cdk 
movcar.f: movcar.ftn 	impnone.cdk 
mprecip4.f: mprecip4.ftn 	acmcon.cdk 	consphy.cdk 	dintern.cdk \
	fintern.cdk 	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
mras0.f: mras0.ftn 	comras.cdk 	impnone.cdk 	phy_macros_f.h \
	rasdata.cdk 	tables.cdk 
msoltri6.f: msoltri6.ftn 	ptopo.cdk 
mthtaw.f: mthtaw.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	flatscp.cdk 	impnone.cdk 	initcph.cdk \
	modpr2.cdk 	phy_macros_f.h 
mthtaw2.f: mthtaw2.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr2.cdk \
	phy_macros_f.h 
mtn_case.f: mtn_case.ftn 	acq.cdk 	acq_int.cdk 	cstv.cdk \
	dcst.cdk 	geomg.cdk 	glb_ld.cdk 	glb_pil.cdk \
	grd.cdk 	ind.cdk 	lun.cdk 	mtn.cdk \
	out1.cdk 	pres.cdk 	ptopo.cdk 	theo.cdk \
	tr3d.cdk 	vt1.cdk 	vtopo.cdk 	zblen.cdk 
mtn_cfg.f: mtn_cfg.ftn 	adw.cdk 	bubble.cdk 	cstv.cdk \
	dcst.cdk 	eigv.cdk 	geomg.cdk 	glb_ld.cdk \
	glb_pil.cdk 	grd.cdk 	hblen.cdk 	hgc.cdk \
	hybdim.cdk 	hzd.cdk 	init.cdk 	lctl.cdk \
	lun.cdk 	mtn.cdk 	offc.cdk 	out1.cdk \
	p_geol.cdk 	pilot.cdk 	pres.cdk 	ptopo.cdk \
	schm.cdk 	step.cdk 	theo.cdk 	theonml.cdk \
	tr3d.cdk 	vspng.cdk 	vtopo.cdk 	zblen.cdk 
nes1s.f: nes1s.ftn 	glb_ld.cdk 	hblen.cdk 
nes2s.f: nes2s.ftn 	glb_ld.cdk 	hblen.cdk 
nes3s.f: nes3s.ftn 	glb_ld.cdk 	hblen.cdk 
nes4s.f: nes4s.ftn 	glb_ld.cdk 	hblen.cdk 
nesajr.f: nesajr.ftn 	glb_ld.cdk 
nest_indata.f: nest_indata.ftn 	glb_ld.cdk 	ind.cdk 	nest.cdk \
	schm.cdk 	v4dg.cdk 
nest_init.f: nest_init.ftn 	glb_ld.cdk 	nest.cdk 	schm.cdk \
	tr3d.cdk 	v4dg.cdk 	vt1.cdk 
nest_intt.f: nest_intt.ftn 	bmf.cdk 	cstv.cdk 	glb_ld.cdk \
	lctl.cdk 	nest.cdk 	offline.cdk 	pilot.cdk \
	ptopo.cdk 	schm.cdk 	tr3d.cdk 	v4dg.cdk 
nestit.f: nestit.ftn 	glb_ld.cdk 	hblen.cdk 	lun.cdk \
	nest.cdk 	schm.cdk 	tr3d.cdk 	vt0.cdk 
newrad4.f: newrad4.ftn 	clefcon.cdk 	consphy.cdk 	impnone.cdk \
	indx_sfc.cdk 	maxlev.cdk 	nocld.cdk 	options.cdk \
	ozopnt.cdk 	phy_macros_f.h 	phybus.cdk 	raddata.cdk \
	radparam.cdk 	radpnt.cdk 	solcons.cdk 	stk.cdk 
ngfft.f: ngfft.ftn 	impnone.cdk 
nli.f: nli.ftn 	cori.cdk 	glb_ld.cdk 	impnone.cdk \
	lctl.cdk 	lun.cdk 	nl.cdk 	p_geof.cdk \
	ptopo.cdk 	rhsc.cdk 	schm.cdk 	vt0.cdk \
	vtx.cdk 
nlip_2.f: nlip_2.ftn 	cori.cdk 	cstv.cdk 	dcst.cdk \
	geomg.cdk 	glb_ld.cdk 	intuv.cdk 	inuvl.cdk \
	ptopo.cdk 	schm.cdk 
ntables.f: ntables.ftn 	impnone.cdk 
nuages2.f: nuages2.ftn 	clefcon.cdk 	consphy.cdk 	dintern.cdk \
	fintern.cdk 	fintern90.cdk 	impnone.cdk 	nocld.cdk \
	phy_macros_f.h 
oldrad3.f: oldrad3.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	phy_macros_f.h 	phybus.cdk 	solcons.cdk \
	stk.cdk 
opinv6.f: opinv6.ftn 	impnone.cdk 
opinv7.f: opinv7.ftn 	impnone.cdk 
optriss6.f: optriss6.ftn 	impnone.cdk 
optriss7.f: optriss7.ftn 	impnone.cdk 
out_dq.f: out_dq.ftn 	dcst.cdk 	dimout.cdk 	geomg.cdk \
	glb_ld.cdk 	lctl.cdk 	level.cdk 	lun.cdk \
	out.cdk 	out3.cdk 	outd.cdk 
out_dyn.f: out_dyn.ftn 	cstv.cdk 	dcst.cdk 	dimout.cdk \
	geomg.cdk 	geomn.cdk 	glb_ld.cdk 	grd.cdk \
	grid.cdk 	init.cdk 	lctl.cdk 	level.cdk \
	lun.cdk 	out3.cdk 	outd.cdk 	pres.cdk \
	ptopo.cdk 	rstr.cdk 	schm.cdk 	tr3d.cdk \
 	v4d_prof.o 	v4dg.cdk 	vt0.cdk \
	vt1.cdk 	vt1m.cdk 
out_padbuf.f: out_padbuf.ftn 	out.cdk 
out_phy.f: out_phy.ftn 	dcst.cdk 	dimout.cdk 	geomg.cdk \
	geomn.cdk 	glb_ld.cdk 	glb_pil.cdk 	grd.cdk \
	grid.cdk 	init.cdk 	level.cdk 	obus.cdk \
	out.cdk 	out3.cdk 	outp.cdk 	p_bus.cdk \
	ptopo.cdk 	rstr.cdk  	v4d_prof.o \
	v4dg.cdk 
out_qc0.f: out_qc0.ftn 	cstv.cdk 	dcst.cdk 	dimout.cdk \
	geomg.cdk 	geomn.cdk 	glb_ld.cdk 	grd.cdk \
	grid.cdk 	init.cdk 	lctl.cdk 	level.cdk \
	lun.cdk 	out.cdk 	out3.cdk 	outd.cdk \
	pres.cdk 	ptopo.cdk 	rstr.cdk 	schm.cdk \
	tr3d.cdk 	vt1.cdk 
out_sblock.f: out_sblock.ftn 	out.cdk 
out_sfile.f: out_sfile.ftn 	out.cdk 
out_sgrid.f: out_sgrid.ftn 	out.cdk 
out_slev.f: out_slev.ftn 	out.cdk 
out_thm.f: out_thm.ftn 	dcst.cdk 	dimout.cdk 	geomg.cdk \
	geomn.cdk 	glb_ld.cdk 	grd.cdk 	lctl.cdk \
	level.cdk 	out.cdk 	out3.cdk 	outd.cdk \
	ptopo.cdk 	schm.cdk 	v4dg.cdk 
out_tracer.f: out_tracer.ftn 	dimout.cdk 	geomg.cdk 	glb_ld.cdk \
	lun.cdk 	out.cdk 	out3.cdk 	outd.cdk \
	tr3d.cdk 
out_uv.f: out_uv.ftn 	dcst.cdk 	dimout.cdk 	geomg.cdk \
	glb_ld.cdk 	lctl.cdk 	level.cdk 	out.cdk \
	out3.cdk 	outd.cdk 	v4dg.cdk 
out_vmm.f: out_vmm.ftn 	dimout.cdk 	geomg.cdk 	geomn.cdk \
	glb_ld.cdk 	grid.cdk 	init.cdk 	lctl.cdk \
	lun.cdk 	orh.cdk 	out.cdk 	out3.cdk \
	outd.cdk 	p_geof.cdk 	rhsc.cdk 	rstr.cdk \
	vt0.cdk 	vt1.cdk 	vt2.cdk 	vta.cdk \
	vth.cdk 	vtx.cdk 
ouvrstrt.f: ouvrstrt.ftn 	impnone.cdk 	lun.cdk 	rstr.cdk 
ozoref2.f: ozoref2.ftn 	consphy.cdk 	impnone.cdk 
p0vt2gz_hyb.f: p0vt2gz_hyb.ftn 	dcst.cdk 	geomg.cdk 	glb_ld.cdk \
	lun.cdk 
p_apply.f: p_apply.ftn 	busind.cdk 	cstv.cdk 	dcst.cdk \
	geomg.cdk 	glb_ld.cdk 	hblen.cdk 	intuv.cdk \
	inuvl.cdk 	lun.cdk 	p_cond.cdk 	schm.cdk \
	v4dg.cdk 	vt0.cdk 	vt1.cdk 
p_config.f: p_config.ftn 	busind.cdk 	c_cplg.cdk 	cstv.cdk \
	dcst.cdk 	geomg.cdk 	glb_ld.cdk 	lctl.cdk \
	lun.cdk 	p4d_simp.cdk 	p_bus.cdk 	p_clim.cdk \
	p_cond.cdk 	p_cond2.cdk 	p_gwd.cdk 	p_out.cdk \
	p_pbl.cdk 	p_pbl2.cdk 	p_pset.cdk 	p_rad.cdk \
	p_str.cdk 	pres.cdk 	schm.cdk 	v4dg.cdk 
p_fcpfw.f: p_fcpfw.ftn 	glb_ld.cdk 	impnone.cdk 	lun.cdk \
	p_cond.cdk 
p_fillbus.f: p_fillbus.ftn 	busind.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	p_bus.cdk 	p_cond.cdk 	p_geol.cdk \
	p_pbl.cdk 	schm.cdk 	tr3d.cdk 
p_inincr.f: p_inincr.ftn 	busind.cdk 	cstv.cdk 	dcst.cdk \
	glb_ld.cdk 	lctl.cdk 	lun.cdk 	mem.cdk \
	out1.cdk 	p_bus.cdk 	p_pbl.cdk 	p_pbl2.cdk \
	p_pset.cdk 	path.cdk 	ptopo.cdk 	rstr.cdk 
p_init.f: p_init.ftn 	busind.cdk 	dimout.cdk 	glb_ld.cdk \
	impnone.cdk 	lun.cdk 	mem.cdk 	p_bus.cdk \
	schm.cdk 
p_main.f: p_main.ftn 	c_cplg.cdk 	cstv.cdk 	dimout.cdk \
	glb_ld.cdk 	impnone.cdk 	init.cdk 	lctl.cdk \
	out2.cdk 	p_bus.cdk 	rstr.cdk 	v4dg.cdk \
	xst.cdk 
p_phyexe.f: p_phyexe.ftn 	glb_ld.cdk 	impnone.cdk 	lctl.cdk \
	lun.cdk 	p_pset.cdk 	ptopo.cdk 
p_physlb.f: p_physlb.ftn 	busind.cdk 	busind_tr.cdk 	c_cplg.cdk \
	cstv.cdk 	dcst.cdk 	dimout.cdk 	geomg.cdk \
	glb_ld.cdk 	lun.cdk 	macro.cdk 	mem.cdk \
	mult.cdk 	obus.cdk 	p_bus.cdk 	p_clim.cdk \
	p_pbl.cdk 	p_pset.cdk 	ptopo.cdk 	v4dg.cdk 
p_phystep.f: p_phystep.ftn 	busind.cdk 	dimout.cdk 	glb_ld.cdk \
	lun.cdk 	macro.cdk 	mem.cdk 	p4d_simp.cdk \
	p_bus.cdk 	p_pbl.cdk 	ptopo.cdk 	schm.cdk \
	v4dg.cdk 
p_psetiw.f: p_psetiw.ftn 	glb_ld.cdk 	impnone.cdk 	lctl.cdk \
	lun.cdk 	p_pset.cdk 
p_radiaf.f: p_radiaf.ftn 	impnone.cdk 	ptopo.cdk 
p_set.f: p_set.ftn 	dcst.cdk 	glb_ld.cdk 	grd.cdk \
	lun.cdk 	mem.cdk 	mult.cdk 	out1.cdk \
	p4d_simp.cdk 	p_clim.cdk 	p_cond.cdk 	p_cond2.cdk \
	p_gwd.cdk 	p_nml.cdk 	p_nml2.cdk 	p_out.cdk \
	p_pbl.cdk 	p_pbl2.cdk 	p_pset.cdk 	p_rad.cdk \
	p_serg.cdk 	p_str.cdk 	p_zong.cdk 	path.cdk \
	ptopo.cdk 	schm.cdk 	xst.cdk 
p_uvgridscal.f: p_uvgridscal.ftn 	glb_ld.cdk 	impnone.cdk 	intuv.cdk \
	inuvl.cdk 	schm.cdk 
p_vlsp.f: p_vlsp.ftn 	geomg.cdk 	glb_ld.cdk 	grd.cdk \
	impnone.cdk 	lun.cdk 	p_geol.cdk 	p_pbl.cdk 
p_vmmphy.f: p_vmmphy.ftn 	busind.cdk 	cstv.cdk 	dcst.cdk \
	geomg.cdk 	glb_ld.cdk 	inuvl.cdk 	p_bus.cdk \
	p_cond.cdk 	p_geof.cdk 	schm.cdk 	vt0.cdk \
	vt1.cdk 
pdfcdate.f: pdfcdate.ftn	 
pdfjdate.f: pdfjdate.ftn	 
pdfjdate2.f: pdfjdate2.ftn	 
phy_ini.f: phy_ini.ftn 	classlvls.cdk 	dimsurf.cdk 	icelvls.cdk \
	impnone.cdk 	indx_sfc.cdk 	maxlev.cdk 	mosaic.cdk \
	nbvarsurf.cdk 	nclassvg.cdk 	options.cdk 	phy_macros_f.h \
	phybus.cdk 
phybus_data.f: phybus_data.ftn 	phy_macros_f.h 	phybus.cdk 
phycom.f: phycom.ftn 	dzcond.cdk 	hscap.cdk 	impnone.cdk \
	leads.cdk 	mountains.cdk 
phydebu4.f: phydebu4.ftn 	acmcon.cdk 	buses.cdk 	cldopt_mode.cdk \
	clefcon.cdk 	comphy.cdk 	consphy.cdk 	dimsurf.cdk \
	dzsedi.cdk 	impnone.cdk 	indx_sfc.cdk 	isbapar.cdk \
	machcon.cdk 	maxlev.cdk 	mountains.cdk 	nbvarsurf.cdk \
	options.cdk 	scfrst.cdk 	sedipara.cdk 	small_sedimentation_dt.cdk \
	surfacepar.cdk 	surfcon.cdk 	surfcon_ini.cdk 	tables.cdk \
	workspc.cdk 
phyexe1.f: phyexe1.ftn 	consphy.cdk 	dimsurf.cdk 	impnone.cdk \
	indx_sfc.cdk 	maxlev.cdk 	nbvarsurf.cdk 	options.cdk \
	phy_macros_f.h 	phybus.cdk 	workspc.cdk 
phyopt.f: phyopt.ftn 	dzsedi.cdk 	indx_sfc.cdk 	maxlev.cdk \
	mosaic.cdk 	options.cdk 	phy_macros_f.h 
pllwfgfw.f: pllwfgfw.ftn	 
pnm2.f: pnm2.ftn 	dcst.cdk 	glb_ld.cdk 	impnone.cdk 
pntg123.f: pntg123.ftn 	impnone.cdk 	radparam.cdk 	radpnt.cdk 
pntozon.f: pntozon.ftn 	impnone.cdk 	ozopnt.cdk 
pospers.f: pospers.ftn 	geomg.cdk 	glb_ld.cdk 	impnone.cdk \
	vth.cdk 
possol.f90: possol.ftn90 	modd_csts.o 
pre.f: pre.ftn 	geomg.cdk 	glb_ld.cdk 	impnone.cdk \
	lctl.cdk 	lun.cdk 	p_geof.cdk 	rhsc.cdk \
	schm.cdk 	vt1.cdk 
predat.f: predat.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	hzd.cdk 	ind.cdk 	lctl.cdk \
	lun.cdk 	pres.cdk 	schm.cdk 	v4dg.cdk \
	vt1.cdk 	vtopo.cdk 
prelim1.f: prelim1.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 
prep_2.f: prep_2.ftn 	adw.cdk 	cori.cdk 	cstv.cdk \
	dcst.cdk 	geomg.cdk 	glb_ld.cdk 	grd.cdk \
	intuv.cdk 	inuvl.cdk 	lun.cdk 	offc.cdk \
	schm.cdk 
prep_cw.f: prep_cw.ftn 	impnone.cdk 	maxlev.cdk 	options.cdk \
	phy_macros_f.h 	phybus.cdk 
prep_cw_rad.f: prep_cw_rad.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	nocld.cdk 	options.cdk 	phy_macros_f.h \
	phybus.cdk 
prep_hor_snow_field.f90: prep_hor_snow_field.ftn90 	modd_data_cover_par.o  \
	modd_isba.o  	modd_prep.o  \
	modd_prep_snow.o  	modd_snow_par.o  \
	modd_surf_par.o  	modd_type_date.o  \
	modd_type_snow.o  	modi_hor_interpol.o  \
	modi_interp_grid2d.o  	modi_prep_snow_gem.o  \
	modi_snow_heat_to_t_wliq.o  	modi_vegtype_grid_to_patch_grid.o 
prep_hor_snow_fields.f90: prep_hor_snow_fields.ftn90 	modd_csts.o  \
	modd_prep.o  	modd_snow_par.o  \
	modd_surf_par.o  	modd_type_snow.o  \
	mode_snow3l.o  	modi_allocate_gr_snow.o  \
	modi_prep_hor_snow_field.o 
prep_hor_teb_field.f90: prep_hor_teb_field.ftn90 	modd_prep.o  \
	modd_prep_teb.o  	modd_teb.o  \
	modd_teb_grid.o  	modd_type_snow.o  \
	modi_get_luout.o  	modi_hor_interpol.o  \
	modi_interp_grid2d.o  	modi_prep_hor_snow_fields.o  \
	modi_prep_teb_gem.o 
prep_index.f: prep_index.ftn	 
prep_snow_gem.f90: prep_snow_gem.ftn90 	modd_prep.o  \
	modd_prep_snow.o  	modd_snow_par.o  \
	modd_town.o 
prep_teb.f90: prep_teb.ftn90 	modd_teb.o  \
	modd_type_snow.o  	modi_default_prep_teb.o  \
	modi_prep_hor_teb_field.o 
prep_teb_gem.f90: prep_teb_gem.ftn90 	modd_csts.o  \
	modd_prep.o  	modd_prep_teb.o  \
	modd_surf_par.o  	modd_town.o  \
	modi_interp_grid1d.o 
prepoic.f: prepoic.ftn 	impnone.cdk 
prever.f: prever.ftn 	cstv.cdk 	dcst.cdk 	impnone.cdk \
	schm.cdk 
prgen.f: prgen.ftn 	glb_ld.cdk 	impnone.cdk 
prgzvta.f: prgzvta.ftn 	dcst.cdk 	glb_ld.cdk 	impnone.cdk 
prism_def_var_proto.f: prism_def_var_proto.ftn	 
prism_enddef_proto.f: prism_enddef_proto.ftn	 
prism_terminate_proto.f: prism_terminate_proto.ftn	 
prodef.f: prodef.ftn 	impnone.cdk 
prof5.f: prof5.ftn 	impnone.cdk 
prsdate.f: prsdate.ftn	 
qcfft8.f: qcfft8.ftn	 
qcsgs.f: qcsgs.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
qozon3.f: qozon3.ftn 	consphy.cdk 	impnone.cdk 
raddel.f: raddel.ftn 	impnone.cdk 
radfac3.f: radfac3.ftn 	impnone.cdk 
radiaf_o.f: radiaf_o.ftn 	impnone.cdk 
radir8.f: radir8.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 
radix.f: radix.ftn 	clefcon.cdk 	consphy.cdk 	impnone.cdk \
	phy_macros_f.h 
rasol.f: rasol.ftn 	clefcon.cdk 	consphy.cdk 	impnone.cdk \
	phy_macros_f.h 	valcon.cdk 
raysol.f: raysol.ftn 	clefcon.cdk 	consphy.cdk 	impnone.cdk 
rdmax.f: rdmax.ftn 	impnone.cdk 
rdmoy.f: rdmoy.ftn 	impnone.cdk 
rdradf.f: rdradf.ftn 	impnone.cdk 	radparam.cdk 	radpnt.cdk 
rdrstrt.f: rdrstrt.ftn 	c_cplg.cdk 	glb_ld.cdk 	impnone.cdk \
	init.cdk 	lctl.cdk 	lun.cdk 	mem.cdk \
	p_bus.cdk 	pilot.cdk 	pres.cdk 	rstr.cdk \
	schm.cdk 
readdyn.f: readdyn.ftn 	acq.cdk 	acq_int.cdk 	bmf.cdk \
	cstv.cdk 	dcst.cdk 	geomg.cdk 	glb_ld.cdk \
	grd.cdk 	ind.cdk 	lctl.cdk 	lun.cdk \
	out1.cdk 	pres.cdk 	ptopo.cdk 	schm.cdk \
	vtopo.cdk 
readgeo.f: readgeo.ftn 	c_cplg.cdk 	glb_ld.cdk 	impnone.cdk \
	lun.cdk 	p_bus.cdk 	p_geobus.cdk 	p_geobusid.cdk \
	ptopo.cdk 
readlam.f: readlam.ftn 	glb_ld.cdk 	impnone.cdk 	ptopo.cdk 
rhs.f: rhs.ftn 	glb_ld.cdk 	impnone.cdk 	lun.cdk \
	nest.cdk 	orh.cdk 	p_geof.cdk 	rhsc.cdk \
	schm.cdk 	vt1.cdk 
rhsp_2.f: rhsp_2.ftn 	cori.cdk 	cstv.cdk 	dcst.cdk \
	geomg.cdk 	glb_ld.cdk 	intuv.cdk 	inuvl.cdk \
	offc.cdk 	schm.cdk 
riflux.f: riflux.ftn 	clefcon.cdk 	impnone.cdk 
rigrad1.f: rigrad1.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 
road_wall_layer_e_budget.f90: road_wall_layer_e_budget.ftn90 	modd_csts.o  \
	mode_thermos.o  	modi_tridiag_ground.o  \
	modi_urban_lw_coef.o 
roof_layer_e_budget.f90: roof_layer_e_budget.ftn90 	modd_csts.o  \
	mode_thermos.o  	modi_tridiag_ground.o 
satq.f: satq.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 
save_options.f: save_options.ftn 	cldopt_mode.cdk 	consphy.cdk 	dzsedi.cdk \
	impnone.cdk 	maxlev.cdk 	options.cdk 	sedipara.cdk \
	small_sedimentation_dt.cdk 
scatter.f: scatter.ftn 	impnone.cdk 
schal.f: schal.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
seaice1.f: seaice1.ftn 	consphy.cdk 	dimsurf.cdk 	dintern.cdk \
	fintern.cdk 	fintern90.cdk 	himin.cdk 	icelvls.cdk \
	impnone.cdk 	indx_sfc.cdk 	locbus.cdk 	maxlev.cdk \
	nbvarsurf.cdk 	options.cdk 	phy_macros_f.h 	sfcbus.cdk \
	snoh0.cdk 	vamin.cdk 	xptsurf.cdk 	zuzt.cdk 
secajus.f: secajus.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
seralc2.f: seralc2.ftn 	impnone.cdk 	sercmdk.cdk 
serallc2.f: serallc2.ftn 	impnone.cdk 	sercmdk.cdk 
serdata.f: serdata.ftn 	sercmdk.cdk 
serdbu.f: serdbu.ftn 	impnone.cdk 	sercmdk.cdk 
serdim.f: serdim.ftn 	impnone.cdk 	sercmdk.cdk 
serecri.f: serecri.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 
serfin.f: serfin.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
sergdim.f: sergdim.ftn 	impnone.cdk 
sergini.f: sergini.ftn 	busind.cdk 	dcst.cdk 	glb_ld.cdk \
	impnone.cdk 	lun.cdk 	mem.cdk 	p_bus.cdk \
	p_serg.cdk 	ptopo.cdk 	rstr.cdk 	xst.cdk 
sergoff.f: sergoff.ftn 	impnone.cdk 	init.cdk 	lctl.cdk \
	lun.cdk 	p_serg.cdk 	rstr.cdk 	xst.cdk 
sergout.f: sergout.ftn 	dcst.cdk 	dimout.cdk 	geomg.cdk \
	glb_ld.cdk 	hgc.cdk 	impnone.cdk 	lctl.cdk \
	out1.cdk 	p_cond.cdk 	p_serg.cdk 	ptopo.cdk \
	xst.cdk 	xstb.cdk 
sergset.f: sergset.ftn 	glb_ld.cdk 	impnone.cdk 	lctl.cdk \
	lun.cdk 	p_bus.cdk 	p_geol.cdk 	p_serg.cdk \
	ptopo.cdk 	rstr.cdk 	xst.cdk 	xstb.cdk 
serie.f: serie.ftn 	impnone.cdk 	maxlev.cdk 	phy_macros_f.h \
	sercmdk.cdk 
serkh.f: serkh.ftn 	impnone.cdk 	sercmdk.cdk 
serpat2.f: serpat2.ftn 	impnone.cdk 	sercmdk.cdk 
serset.f: serset.ftn 	impnone.cdk 	sercmdk.cdk 
sersetc.f: sersetc.ftn 	impnone.cdk 	sercmdk.cdk 
sersetm.f: sersetm.ftn 	impnone.cdk 	sercmdk.cdk 
serwrit.f: serwrit.ftn	 
serwrit2.f: serwrit2.ftn 	impnone.cdk 	sercmdk.cdk 
serxst.f: serxst.ftn 	impnone.cdk 	sercmdk.cdk 
sesahr.f: sesahr.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sesahu.f: sesahu.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr1.cdk 
set_class_const.f: set_class_const.ftn 	consphy.cdk 	impnone.cdk 	surfcon.cdk 
set_class_dt.f: set_class_dt.ftn 	class_com.cdk 
set_cn1.f: set_cn1.ftn 	dimout.cdk 	glb_ld.cdk 	init.cdk \
	lun.cdk 	rstr.cdk 	schm.cdk 
set_cori.f: set_cori.ftn 	cori.cdk 	geomg.cdk 	glb_ld.cdk \
	grd.cdk 	impnone.cdk 	lun.cdk 
set_crni.f: set_crni.ftn 	glb_ld.cdk 	impnone.cdk 	lun.cdk \
	orh.cdk 	schm.cdk 
set_dcst.f: set_dcst.ftn	 
set_dcst_8.f: set_dcst_8.ftn	 
set_dia.f: set_dia.ftn 	glb_ld.cdk 	impnone.cdk 	lun.cdk \
	p_serg.cdk 	p_zong.cdk 
set_dync.f: set_dync.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	grd.cdk 	lun.cdk 	pres.cdk \
	schm.cdk 
set_fft.f: set_fft.ftn 	dcst.cdk 	fft.cdk 	glb_ld.cdk \
	glb_pil.cdk 	grd.cdk 	impnone.cdk 	lun.cdk 
set_filt.f: set_filt.ftn 	dimout.cdk 	glb_ld.cdk 	impnone.cdk \
	lun.cdk 	out3.cdk 
set_geof.f: set_geof.ftn 	glb_ld.cdk 	lun.cdk 	p_geof.cdk \
	vtopo.cdk 
set_geom.f: set_geom.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	geomn.cdk 	glb_ld.cdk 	glb_pil.cdk 	grd.cdk \
	hgc.cdk 	lun.cdk 	p_geof.cdk 	ptopo.cdk \
	schm.cdk 	xst.cdk 
set_grid.f: set_grid.ftn 	dimout.cdk 	glb_ld.cdk 	glb_pil.cdk \
	grd.cdk 	grid.cdk 	impnone.cdk 	lun.cdk \
	p_geol.cdk 
set_intuv.f: set_intuv.ftn 	dcst.cdk 	geomg.cdk 	glb_ld.cdk \
	impnone.cdk 	intuv.cdk 	inuvl.cdk 	lun.cdk \
	ptopo.cdk 
set_level.f: set_level.ftn 	dimout.cdk 	geomg.cdk 	glb_ld.cdk \
	impnone.cdk 	level.cdk 	lun.cdk 
set_list.f: set_list.ftn 	dimout.cdk 	glb_ld.cdk 	impnone.cdk \
	init.cdk 	lun.cdk 	rhsc.cdk 	rstr.cdk \
	setsor.cdk 	tr3d.cdk 	vt0.cdk 	vt1.cdk \
	vt2.cdk 	vta.cdk 	vth.cdk 	vtx.cdk 
set_mem.f: set_mem.ftn 	glb_ld.cdk 	impnone.cdk 	lun.cdk \
	mem.cdk 	schm.cdk 
set_nest.f: set_nest.ftn 	glb_ld.cdk 	impnone.cdk 	lun.cdk \
	nest.cdk 	pilot.cdk 	schm.cdk 	tr3d.cdk 
set_opr.f: set_opr.ftn 	cstv.cdk 	dcst.cdk 	eigv.cdk \
	fft.cdk 	geomg.cdk 	glb_ld.cdk 	glb_pil.cdk \
	impnone.cdk 	lun.cdk 	opr.cdk 	schm.cdk \
	sol.cdk 	trp.cdk 
set_oprz.f: set_oprz.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	impnone.cdk 	opr.cdk 	schm.cdk \
	sol.cdk 	trp.cdk 
set_ops8.f: set_ops8.ftn 	impnone.cdk 
set_poic.f: set_poic.ftn 	dcst.cdk 	glb_ld.cdk 	glb_pil.cdk \
	impnone.cdk 	lun.cdk 
set_poic_par.f: set_poic_par.ftn 	dcst.cdk 	impnone.cdk 
set_poic_par_u.f: set_poic_par_u.ftn 	dcst.cdk 	impnone.cdk 
set_pois.f: set_pois.ftn 	impnone.cdk 
set_rhs.f: set_rhs.ftn 	glb_ld.cdk 	impnone.cdk 	lun.cdk \
	rhsc.cdk 
set_schm.f: set_schm.ftn 	cstv.cdk 	dcst.cdk 	glb_ld.cdk \
	lun.cdk 	offc.cdk 	schm.cdk 
set_sor.f: set_sor.ftn 	cstv.cdk 	dimout.cdk 	geomg.cdk \
	glb_ld.cdk 	grid.cdk 	hgc.cdk 	impnone.cdk \
	lctl.cdk 	level.cdk 	lun.cdk 	orh.cdk \
	out1.cdk 	out3.cdk 	outd.cdk 	outp.cdk \
	p_bus.cdk 	p_geof.cdk 	pilot.cdk 	ptopo.cdk \
	rhsc.cdk 	step.cdk 	timestep.cdk 	vt0.cdk \
	vt1.cdk 	vt2.cdk 	vta.cdk 	vth.cdk \
	vtx.cdk 
set_step.f: set_step.ftn 	cstv.cdk 	dimout.cdk 	glb_ld.cdk \
	impnone.cdk 	lun.cdk 	step.cdk 	timestep.cdk 
set_term.f: set_term.ftn 	impnone.cdk 	lun.cdk 	p_serg.cdk \
	ptopo.cdk 	schm.cdk 
set_transpose.f: set_transpose.ftn 	glb_ld.cdk 	impnone.cdk 	ldnh.cdk \
	lun.cdk 	ptopo.cdk 	schm.cdk 	trp.cdk \
	vspng.cdk 
set_trig21.f: set_trig21.ftn	 
set_var.f: set_var.ftn 	dimout.cdk 	glb_ld.cdk 	grid.cdk \
	impnone.cdk 	level.cdk 	lun.cdk 	out3.cdk \
	outd.cdk 	outp.cdk 	p_bus.cdk 	setsor.cdk \
	timestep.cdk 
set_vt.f: set_vt.ftn 	glb_ld.cdk 	impnone.cdk 	lun.cdk \
	schm.cdk 	tr3d.cdk 	vt0.cdk 	vt1.cdk \
	vt2.cdk 	vth.cdk 	vtx.cdk 
set_vta.f: set_vta.ftn 	glb_ld.cdk 	impnone.cdk 	lun.cdk \
	schm.cdk 	tr3d.cdk 	vta.cdk 
set_world_view.f: set_world_view.ftn 	cst_lis.cdk 	cstv.cdk 	dcst.cdk \
	dimout.cdk 	glb_ld.cdk 	glb_pil.cdk 	grd.cdk \
	grid.cdk 	hblen.cdk 	hgc.cdk 	level.cdk \
	lun.cdk 	path.cdk 	pilot.cdk 	pres.cdk \
	ptopo.cdk 
set_world_view1.f: set_world_view1.ftn 	adw.cdk 	c_cplg.cdk 	cori.cdk \
	cstv.cdk 	dimout.cdk 	eigv.cdk 	geomg.cdk \
	glb_ld.cdk 	glb_pil.cdk 	grd.cdk 	grid.cdk \
	hspng.cdk 	hybdim.cdk 	hzd.cdk 	init.cdk \
	lctl.cdk 	level.cdk 	lun.cdk 	mem.cdk \
	nml.cdk 	offc.cdk 	offline.cdk 	out1.cdk \
	out3.cdk 	path.cdk 	pilot.cdk 	pres.cdk \
	ptopo.cdk 	schm.cdk 	setsor.cdk 	step.cdk \
	theo.cdk 	tr3d.cdk 	v4dg.cdk 	v4dj.cdk \
	v4dm.cdk 	v4dr.cdk  	v4dz.o \
	version.cdk 	vrtd.cdk 	vspng.cdk 	vtopo.cdk \
	xst.cdk 	zblen.cdk 
set_xnbit.f: set_xnbit.ftn 	dimout.cdk 	glb_ld.cdk 	impnone.cdk \
	lun.cdk 	out3.cdk 
set_xst.f: set_xst.ftn 	dcst.cdk 	geomn.cdk 	glb_ld.cdk \
	grd.cdk 	lun.cdk 	p_geol.cdk 	xst.cdk 
setdsr2.f: setdsr2.ftn 	impnone.cdk 
setscqr.f: setscqr.ftn	 
setsd.f: setsd.ftn 	impnone.cdk 
settriv.f: settriv.ftn 	impnone.cdk 
setvis2.f: setvis2.ftn 	consphy.cdk 	impnone.cdk 
sfcbus_data.f: sfcbus_data.ftn 	phy_macros_f.h 	sfcbus.cdk 
sfltr.f: sfltr.ftn 	impnone.cdk 
sfodla.f: sfodla.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfodle.f: sfodle.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfodqa.f: sfodqa.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfodqs.f: sfodqs.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfoefq.f: sfoefq.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfoew.f: sfoew.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfoewa.f: sfoewa.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfohr.f: sfohr.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfohra.f: sfohra.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfols.f: sfols.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfolv.f: sfolv.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfopop.f: sfopop.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfopot.f: sfopot.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfoqfe.f: sfoqfe.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfoqsa.f: sfoqsa.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfoqst.f: sfoqst.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfottv.f: sfottv.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfotvt.f: sfotvt.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sgamash.f: sgamash.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sgamasp.f: sgamasp.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sgamatd.f: sgamatd.ftn 	consphy.cdk 	impnone.cdk 	initcph.cdk 
sgoflx2.f: sgoflx2.ftn 	impnone.cdk 	phy_macros_f.h 
sgoflx3.f: sgoflx3.ftn 	impnone.cdk 	phy_macros_f.h 
shallconv3.f: shallconv3.ftn 	impnone.cdk 	indx_sfc.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 
shalow.f: shalow.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 
shraes.f: shraes.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
shrahu.f: shrahu.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr1.cdk 
shuaes.f: shuaes.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr1.cdk \
	modtp1.cdk 
shuahr.f: shuahr.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr1.cdk \
	modtp1.cdk 
shuffle.f: shuffle.ftn 	impnone.cdk 	lun.cdk 	setsor.cdk 
skocon.f: skocon.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	fmroft.cdk 	impnone.cdk 	phy_macros_f.h 
slabsym.f: slabsym.ftn 	glb_ld.cdk 	lun.cdk 	schm.cdk \
	tr3d.cdk 	vt0.cdk 
slfun_tq.f: slfun_tq.ftn 	phy_macros_f.h 	stabfunc.cdk 	surfcon_ini.cdk 
slfun_uv.f: slfun_uv.ftn 	phy_macros_f.h 	stabfunc.cdk 	surfcon_ini.cdk 
sltop_tq.f: sltop_tq.ftn 	deriv_stabfunc.cdk 	phy_macros_f.h 	surfcon_ini.cdk 
sltop_uv.f: sltop_uv.ftn 	deriv_stabfunc.cdk 	phy_macros_f.h 	surfcon_ini.cdk 
snow_cover_1layer.f90: snow_cover_1layer.ftn90 	modd_csts.o  \
	modd_snow_par.o  	modd_surf_par.o  \
	mode_thermos.o  	modi_surface_aero_cond.o  \
	modi_surface_ri.o 
snow_heat_to_t_wliq.f90: snow_heat_to_t_wliq.ftn90 	modd_csts.o  \
	modd_snow_par.o  	modd_surf_par.o  \
	mode_snow3l.o 
soili.f: soili.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
sol_abc.f: sol_abc.ftn 	glb_ld.cdk 	glb_pil.cdk 	impnone.cdk 
sol_fft8_2.f: sol_fft8_2.ftn 	impnone.cdk 	ptopo.cdk 
sol_fft8_lam.f: sol_fft8_lam.ftn 	glb_ld.cdk 	glb_pil.cdk 	impnone.cdk \
	ptopo.cdk 
sol_hcr.f: sol_hcr.ftn 	cstv.cdk 	eigv.cdk 	fft.cdk \
	glb_ld.cdk 	glb_pil.cdk 	impnone.cdk 	lctl.cdk \
	ldnh.cdk 	lun.cdk 	opr.cdk 	ptopo.cdk \
	schm.cdk 	sol.cdk 	trp.cdk 
sol_main.f: sol_main.ftn 	fft.cdk 	glb_ld.cdk 	impnone.cdk \
	lctl.cdk 	ldnh.cdk 	lun.cdk 	opr.cdk \
	ptopo.cdk 	rhsc.cdk 	trp.cdk 	vtx.cdk 
sol_mxma8_2.f: sol_mxma8_2.ftn 	glb_ld.cdk 	glb_pil.cdk 	ptopo.cdk 
sol_parite_2.f: sol_parite_2.ftn 	impnone.cdk 	ptopo.cdk 
sol_trsf.f: sol_trsf.ftn 	glb_ld.cdk 	impnone.cdk 
splitst.f: splitst.ftn 	impnone.cdk 
srequet.f: srequet.ftn 	dimout.cdk 	grid.cdk 	impnone.cdk \
	level.cdk 	lun.cdk 	out3.cdk 	outd.cdk \
	outp.cdk 	timestep.cdk 
statf_dm.f: statf_dm.ftn 	glb_ld.cdk 	lun.cdk 	ptopo.cdk 
statfld.f: statfld.ftn 	glb_ld.cdk 	impnone.cdk 	lctl.cdk \
	lun.cdk 
statfld_old.f: statfld_old.ftn 	impnone.cdk 	lun.cdk 
status_file.f: status_file.ftn	 
statvps1.f: statvps1.ftn 	buses.cdk 	impnone.cdk 
stetae.f: stetae.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
stfrst4.f: stfrst4.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	hscap.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	scfrst.cdk 
sthtaw.f: sthtaw.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	flatscp.cdk 	impnone.cdk 	initcph.cdk \
	modpr1.cdk 
sthtaw2.f: sthtaw2.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr1.cdk 
stop_world_view.f: stop_world_view.ftn 	lctl.cdk 	lun.cdk 	path.cdk \
	ptopo.cdk 	version.cdk 
stopmpi.f: stopmpi.ftn	 
stretch_axis2.f: stretch_axis2.ftn	 
sttlcl.f: sttlcl.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sun7.f: sun7.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
sun_radfix1.f: sun_radfix1.ftn 	impnone.cdk 
suncos.f: suncos.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
sunpos.f90: sunpos.ftn90 	modd_csts.o 
surf_precip.f: surf_precip.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 
surface.f: surface.ftn 	consphy.cdk 	dimsurf.cdk 	impnone.cdk \
	indx_sfc.cdk 	maxlev.cdk 	nbvarsurf.cdk 	options.cdk \
	phy_macros_f.h 	phybus.cdk 	stk.cdk 	surfacepar.cdk \
	zuzt.cdk 
surface_aero_cond.f90: surface_aero_cond.ftn90 	modd_csts.o  \
	mode_thermos.o 
surface_cd.f90: surface_cd.ftn90 	modd_csts.o  \
	mode_thermos.o 
surface_ri.f90: surface_ri.ftn90 	modd_csts.o 
surfix.f: surfix.ftn 	glb_ld.cdk 	impnone.cdk 	vt0.cdk \
	vt1.cdk 
surfradcloud.f: surfradcloud.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	options.cdk 	phy_macros_f.h 	phybus.cdk \
	radclcons.cdk 	solcons.cdk 
t02t1.f: t02t1.ftn 	glb_ld.cdk 	impnone.cdk 	schm.cdk \
	tr3d.cdk 	vt0.cdk 	vt1.cdk 
t12t2.f: t12t2.ftn 	glb_ld.cdk 	impnone.cdk 	schm.cdk \
	tr3d.cdk 	vt1.cdk 	vt2.cdk 
t22t0.f: t22t0.ftn 	glb_ld.cdk 	impnone.cdk 	schm.cdk \
	tr3d.cdk 	vt0.cdk 	vt2.cdk 
ta2t1tx.f: ta2t1tx.ftn 	glb_ld.cdk 	impnone.cdk 	lun.cdk \
	mem.cdk 	nest.cdk 	p_bus.cdk 	schm.cdk \
	tr3d.cdk 	vt1.cdk 	vta.cdk 	vtx.cdk 
teb.f90: teb.ftn90 	modd_csts.o  \
	modd_snow_par.o  	mode_surf_snow_frac.o  \
	mode_thermos.o  	modi_bld_e_budget.o  \
	modi_road_wall_layer_e_budget.o  	modi_roof_layer_e_budget.o  \
	modi_urban_drag.o  	modi_urban_fluxes.o  \
	modi_urban_hydro.o  	modi_urban_snow_evol.o  \
	modi_urban_solar_abs.o 
theo_cfg.f: theo_cfg.ftn 	adw.cdk 	bubble.cdk 	cstv.cdk \
	eigv.cdk 	geomg.cdk 	glb_ld.cdk 	glb_pil.cdk \
	grd.cdk 	hblen.cdk 	hgc.cdk 	hybdim.cdk \
	hzd.cdk 	init.cdk 	lctl.cdk 	lun.cdk \
	mtn.cdk 	offc.cdk 	out1.cdk 	p_geol.cdk \
	pilot.cdk 	pres.cdk 	ptopo.cdk 	schm.cdk \
	step.cdk 	theo.cdk 	theonml.cdk 	tr3d.cdk \
	vspng.cdk 	vtopo.cdk 	zblen.cdk 
thermco2.f: thermco2.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
tkealg.f: tkealg.ftn 	impnone.cdk 	machcon.cdk 	phy_macros_f.h 
tmg_init0.f: tmg_init0.ftn 	time0.cdk 
tmg_start0.f: tmg_start0.ftn 	time0.cdk 
tmg_stop0.f: tmg_stop0.ftn 	time0.cdk 
tmg_terminate0.f: tmg_terminate0.ftn 	time0.cdk 
town.f90: town.ftn90 	dimsurf.cdk 	impnone.cdk 	indx_sfc.cdk \
	locbus.cdk 	maxlev.cdk  	modd_csts.o \
 	modd_surf_par.o  	modd_teb.o \
 	modd_teb_grid.o  	modd_town.o \
 	modd_type_snow.o  	modi_allocate_gr_snow.o \
 	modi_coupling_teb_orography.o  	modi_ini_csts.o \
 	modi_possol.o  	modi_sunpos.o \
	nbvarsurf.cdk 	options.cdk 	phy_macros_f.h 	sfcbus.cdk \
	tebcst.cdk 	xptsurf.cdk 
tp.f: tp.ftn 	impnone.cdk 
tpdd.f: tpdd.ftn 	impnone.cdk 
tpmix.f: tpmix.ftn 	impnone.cdk 
tr_init.f: tr_init.ftn 	glb_ld.cdk 	nest.cdk 	tr3d.cdk \
	vt1.cdk 
tr_vint.f: tr_vint.ftn 	acq.cdk 	geomg.cdk 	glb_ld.cdk \
	schm.cdk 
tracers.f: tracers.ftn 	busind.cdk 	lun.cdk 	tr3d.cdk 
trans_clean.f: trans_clean.ftn 	lctl.cdk 	path.cdk 	ptopo.cdk \
	rstr.cdk 	step.cdk 
trapez2.f: trapez2.ftn 	impnone.cdk 
tridiag_ground.f90: tridiag_ground.ftn90	 
tstpdyn.f: tstpdyn.ftn 	cstv.cdk 	glb_ld.cdk 	lun.cdk \
	nl.cdk 	orh.cdk 	schm.cdk 	sol.cdk \
	v4dg.cdk 	v4dr.cdk 
tttt.f: tttt.ftn 	impnone.cdk 
turbul.f: turbul.ftn 	clefcon.cdk 	consphy.cdk 	dintern.cdk \
	fintern.cdk 	fintern90.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	options.cdk 	phy_macros_f.h 	phybus.cdk \
	stk.cdk 	surfcon.cdk 
update.f: update.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
update3.f: update3.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 
urban_drag.f90: urban_drag.ftn90 	modd_csts.o  \
	modd_town.o  	mode_thermos.o  \
	modi_surface_aero_cond.o  	modi_surface_cd.o  \
	modi_surface_ri.o 
urban_fluxes.f90: urban_fluxes.ftn90 	modd_csts.o  \
	modi_urban_lw_coef.o 
urban_hydro.f90: urban_hydro.ftn90 	modd_csts.o 
urban_lw_coef.f90: urban_lw_coef.ftn90 	modd_csts.o 
urban_snow_evol.f90: urban_snow_evol.ftn90 	modd_csts.o  \
	modd_snow_par.o  	mode_surf_snow_frac.o  \
	modi_snow_cover_1layer.o  	modi_urban_lw_coef.o 
urban_solar_abs.f90: urban_solar_abs.ftn90 	modd_csts.o 
utco2.f: utco2.ftn 	impnone.cdk 
uv2tdpsd.f: uv2tdpsd.ftn 	geomg.cdk 	glb_ld.cdk 	intuv.cdk 
uv_acg2g.f: uv_acg2g.ftn 	glb_ld.cdk 	impnone.cdk 	intuv.cdk \
	inuvl.cdk 	schm.cdk 
v4d_prof.f90: v4d_prof.cdk90    
v4dz.f90: v4dz.cdk90    
var_topo.f: var_topo.ftn 	dcst.cdk 	glb_ld.cdk 	lctl.cdk \
	p_geof.cdk 	vt1.cdk 	vtopo.cdk 
vco2inf2.f: vco2inf2.ftn 	impnone.cdk 	phy_macros_f.h 
vdfds.f: vdfds.ftn 	impnone.cdk 
vegi.f: vegi.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
vegtype_grid_to_patch_grid.f90: vegtype_grid_to_patch_grid.ftn90 	modd_data_cover_par.o  \
	modd_isba.o  	modd_surf_par.o  \
	modd_type_date.o  	modd_type_snow.o  \
	modi_vegtype_to_patch.o 
vegtype_to_patch.f90: vegtype_to_patch.ftn90 	modd_data_cover_par.o 
ver_interp_lin2d.f90: ver_interp_lin2d.ftn90	 
verder.f: verder.ftn 	glb_ld.cdk 	impnone.cdk 
vertdiff.f: vertdiff.ftn 	impnone.cdk 	phy_macros_f.h 
vgathr.f: vgathr.ftn 	impnone.cdk 
vkuocon6.f: vkuocon6.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	mountains.cdk 	nocld.cdk 	options.cdk 	phy_macros_f.h \
	phybus.cdk 
vrtd.f: vrtd.ftn 	cstv.cdk 	dcst.cdk 	glb_ld.cdk \
	impnone.cdk 	intuv.cdk 	schm.cdk 	vrtd.cdk \
	vt1.cdk 
vspng_abc.f: vspng_abc.ftn 	glb_ld.cdk 	ptopo.cdk 	trp.cdk \
	vspng.cdk 
vspng_del2.f: vspng_del2.ftn 	glb_ld.cdk 	impnone.cdk 	ldnh.cdk \
	ptopo.cdk 	trp.cdk 	vspng.cdk 
vspng_drv.f: vspng_drv.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	hzd.cdk 	impnone.cdk 	opr.cdk \
	schm.cdk 	trp.cdk 	vspng.cdk 
vspng_drv_lam.f: vspng_drv_lam.ftn 	cstv.cdk 	geomg.cdk 	glb_ld.cdk \
	impnone.cdk 	schm.cdk 	vspng.cdk 
vspng_lam.f: vspng_lam.ftn 	glb_ld.cdk 	glb_pil.cdk 	impnone.cdk \
	vspng.cdk 
vspng_main.f: vspng_main.ftn 	cstv.cdk 	geomg.cdk 	glb_ld.cdk \
	impnone.cdk 	lun.cdk 	p_geof.cdk 	schm.cdk \
	vspng.cdk 	vt0.cdk 	vt1.cdk 	vtx.cdk 
vspng_set.f: vspng_set.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	impnone.cdk 	lun.cdk 	vspng.cdk 
vtap.f: vtap.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	impnone.cdk 	ind.cdk 	lun.cdk \
	schm.cdk 
vte_hyb2hyb.f: vte_hyb2hyb.ftn 	dcst.cdk 	impnone.cdk 
vte_intvertx.f: vte_intvertx.ftn 	dcst.cdk 	impnone.cdk 
vte_uv2img.f: vte_uv2img.ftn 	dcst.cdk 	impnone.cdk 
vte_vrtical.f: vte_vrtical.ftn 	geomg.cdk 	glb_ld.cdk 
vterp1.f: vterp1.ftn 	impnone.cdk 
vterp2.f: vterp2.ftn	 
water.f: water.ftn 	clefcon.cdk 	consphy.cdk 	dimsurf.cdk \
	dintern.cdk 	fintern.cdk 	fintern90.cdk 	impnone.cdk \
	indx_sfc.cdk 	locbus.cdk 	maxlev.cdk 	nbvarsurf.cdk \
	options.cdk 	phy_macros_f.h 	sfcbus.cdk 	vamin.cdk \
	xptsurf.cdk 	zuzt.cdk 
wdpers.f: wdpers.ftn 	glb_ld.cdk 	hzd.cdk 	impnone.cdk \
	lctl.cdk 	lun.cdk 	rhsc.cdk 	vt1.cdk \
	vth.cdk 
wetcon2.f: wetcon2.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
wflux.f: wflux.ftn 	impnone.cdk 
wlog.f: wlog.ftn 	cstv.cdk 	dimout.cdk 	glb_ld.cdk \
	impnone.cdk 	lctl.cdk 	out1.cdk 	out3.cdk \
	ptopo.cdk 
wndth.f: wndth.ftn 	glb_ld.cdk 	hzd.cdk 	impnone.cdk \
	lun.cdk 	vt0.cdk 	vt1.cdk 	vth.cdk 
writlam.f: writlam.ftn 	glb_ld.cdk 	impnone.cdk 	ptopo.cdk 
wrrstrt.f: wrrstrt.ftn 	c_cplg.cdk 	glb_ld.cdk 	impnone.cdk \
	init.cdk 	lctl.cdk 	lun.cdk 	mem.cdk \
	p_bus.cdk 	pilot.cdk 	pres.cdk 	rstr.cdk \
	schm.cdk 
wrvref.f: wrvref.ftn 	grd.cdk 	out.cdk 	pres.cdk 
xyfil.f: xyfil.ftn 	dcst.cdk 	glb_ld.cdk 	impnone.cdk 
zeracc.f: zeracc.ftn 	buses.cdk 	impnone.cdk 
zongini.f: zongini.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	geomn.cdk 	glb_ld.cdk 	grd.cdk 	impnone.cdk \
	lun.cdk 	out1.cdk 	p_geol.cdk 	p_pbl.cdk \
	p_zong.cdk 	ptopo.cdk 	rstr.cdk 
zongoff.f: zongoff.ftn 	impnone.cdk 	init.cdk 	lctl.cdk \
	lun.cdk 	p_zong.cdk 	rstr.cdk 
zongopr.f: zongopr.ftn 	glb_ld.cdk 	impnone.cdk 	lctl.cdk \
	lun.cdk 


absdvdz3.o: absdvdz3.ftn 	clefcon.cdk 	consphy.cdk 	impnone.cdk 
acqui.o: acqui.ftn 	acq.cdk 	acq_int.cdk 	bmf.cdk \
	cstv.cdk 	dcst.cdk 	geomg.cdk 	glb_ld.cdk \
	glb_pil.cdk 	grd.cdk 	hblen.cdk 	ind.cdk \
	lun.cdk 	pres.cdk 	ptopo.cdk 	schm.cdk \
	vtopo.cdk 
adilwc.o: adilwc.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 
adw_cfl_lam.o: adw_cfl_lam.ftn 	adw.cdk 	glb_ld.cdk 	ptopo.cdk 
adw_ckbd.o: adw_ckbd.ftn 	adw.cdk 	geomg.cdk 	glb_ld.cdk \
	impnone.cdk 
adw_ckbd_lam.o: adw_ckbd_lam.ftn 	adw.cdk 	geomg.cdk 	glb_ld.cdk \
	impnone.cdk 	ptopo.cdk 
adw_exch_1.o: adw_exch_1.ftn 	adw.cdk 	geomg.cdk 	glb_ld.cdk \
	impnone.cdk 	ptopo.cdk 
adw_exch_2.o: adw_exch_2.ftn 	glb_ld.cdk 	impnone.cdk 
adw_exch_3.o: adw_exch_3.ftn 	adw.cdk 	impnone.cdk 
adw_interp.o: adw_interp.ftn 	adw.cdk 	glb_ld.cdk 
adw_interp2.o: adw_interp2.ftn 	adw.cdk 	glb_ld.cdk 
adw_main.o: adw_main.ftn 	adw.cdk 	glb_ld.cdk 	impnone.cdk \
	lun.cdk 	orh.cdk 	ptopo.cdk 	schm.cdk \
	v4dg.cdk 	v4dr.cdk 
adw_main_1_wnd.o: adw_main_1_wnd.ftn 	adw.cdk 	glb_ld.cdk 	glb_pil.cdk \
	inuvl.cdk 	lun.cdk 	vth.cdk 
adw_main_2_pos.o: adw_main_2_pos.ftn 	adw.cdk 	cstv.cdk 	dcst.cdk \
	geomg.cdk 	glb_ld.cdk 	glb_pil.cdk 	lctl.cdk \
	lun.cdk 	v4dg.cdk 	vt1.cdk 	vth.cdk 
adw_main_3_int.o: adw_main_3_int.ftn 	adw.cdk 	cstv.cdk 	geomg.cdk \
	glb_ld.cdk 	glb_pil.cdk 	lctl.cdk 	lun.cdk \
	offc.cdk 	orh.cdk 	ptopo.cdk 	rhsc.cdk \
	schm.cdk 	step.cdk 	tr3d.cdk 	v4dg.cdk \
	vt0.cdk 	vt1.cdk 
adw_main_3_intlag.o: adw_main_3_intlag.ftn 	adw.cdk 	cstv.cdk 	geomg.cdk \
	glb_ld.cdk 	glb_pil.cdk 	lctl.cdk 	lun.cdk \
	offc.cdk 	orh.cdk 	ptopo.cdk 	rhsc.cdk \
	schm.cdk 	step.cdk 	tr3d.cdk 	v4dg.cdk \
	vt0.cdk 	vt1.cdk 
adw_pol0.o: adw_pol0.ftn 	impnone.cdk 
adw_pols.o: adw_pols.ftn 	impnone.cdk 
adw_polw.o: adw_polw.ftn 	impnone.cdk 
adw_polx.o: adw_polx.ftn 	adw.cdk 	impnone.cdk 
adw_set.o: adw_set.ftn 	adw.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	impnone.cdk 	schm.cdk 
adw_setint.o: adw_setint.ftn 	adw.cdk 	glb_ld.cdk 
adw_trajex.o: adw_trajex.ftn 	adw.cdk 	dcst.cdk 	glb_ld.cdk \
	impnone.cdk 
adw_trajsp.o: adw_trajsp.ftn 	adw.cdk 	dcst.cdk 	glb_ld.cdk 
adw_tricub.o: adw_tricub.ftn 	adw.cdk 	glb_ld.cdk 	impnone.cdk 
adw_tricub_lag3d.o: adw_tricub_lag3d.ftn 	adw.cdk 	glb_ld.cdk 
adw_trilin.o: adw_trilin.ftn 	adw.cdk 	glb_ld.cdk 
adw_trilin_turbo.o: adw_trilin_turbo.ftn 	adw.cdk 	glb_ld.cdk 	v4dcfl.cdk 
adw_vder.o: adw_vder.ftn 	adw.cdk 	geomg.cdk 	glb_ld.cdk \
	impnone.cdk 	schm.cdk 
aggcovernat.o: aggcovernat.ftn 	impnone.cdk 	surfacepar.cdk 
aggcoverurb.o: aggcoverurb.ftn 	impnone.cdk 	surfacepar.cdk 
aggveg.o: aggveg.ftn 	impnone.cdk 	surfacepar.cdk 
agrege1.o: agrege1.ftn 	dimsurf.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	nbvarsurf.cdk 	options.cdk 	phy_macros_f.h \
	sfcbus.cdk 
agvgclas.o: agvgclas.ftn 	impnone.cdk 	surfacepar.cdk 
agvgclas2.o: agvgclas2.ftn 	impnone.cdk 	surfacepar.cdk 
agvgmask.o: agvgmask.ftn 	impnone.cdk 
agvgmask2.o: agvgmask2.ftn 	impnone.cdk 
agvgmos.o: agvgmos.ftn 	classlvls.cdk 	impnone.cdk 	nclassvg.cdk \
	phy_macros_f.h 	phybus.cdk 	surfacepar.cdk 	tuiles.cdk 
allocate_gr_snow.o: allocate_gr_snow.ftn90 	modd_surf_par.o  \
	modd_type_snow.o 
atmflux.o: atmflux.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 
averaged_albedo_teb.o: averaged_albedo_teb.ftn90 	modd_snow_par.o  \
	modd_type_snow.o  	mode_surf_snow_frac.o  \
	modi_urban_solar_abs.o 
averaged_tsrad_teb.o: averaged_tsrad_teb.ftn90 	modd_csts.o  \
	modd_snow_par.o  	modd_type_snow.o  \
	mode_surf_snow_frac.o  	modi_urban_lw_coef.o 
bac.o: bac.ftn 	glb_ld.cdk 	impnone.cdk 	lctl.cdk \
	lun.cdk 	nl.cdk 	p_geof.cdk 	rhsc.cdk \
	schm.cdk 	vt0.cdk 	vtx.cdk 
bacp_2.o: bacp_2.ftn 	cori.cdk 	cstv.cdk 	dcst.cdk \
	geomg.cdk 	glb_ld.cdk 	orh.cdk 	schm.cdk 
baktotq2.o: baktotq2.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
bcastc1.o: bcastc1.ftn 	adw.cdk 	bubble.cdk 	cori.cdk \
	cstv.cdk 	dimout.cdk 	eigv.cdk 	geomg.cdk \
	glb_ld.cdk 	glb_pil.cdk 	grd.cdk 	hblen.cdk \
	hgc.cdk 	hspng.cdk 	hzd.cdk 	init.cdk \
	lctl.cdk 	level.cdk 	mem.cdk 	mtn.cdk \
	offc.cdk 	offline.cdk 	out1.cdk 	out3.cdk \
	p_geol.cdk 	path.cdk 	pilot.cdk 	pres.cdk \
	ptopo.cdk 	schm.cdk 	step.cdk 	theo.cdk \
	tr3d.cdk 	v4dg.cdk 	v4dj.cdk 	v4dm.cdk \
	v4dr.cdk  	v4dz.o 	vrtd.cdk \
	vspng.cdk 	vtopo.cdk 	xst.cdk 	zblen.cdk 
bin2com.o: bin2com.ftn 	glb_ld.cdk 	grd.cdk 	hblen.cdk \
	hgc.cdk 	lun.cdk 	offline.cdk 	out1.cdk \
	p_geol.cdk 	p_pbl.cdk 	pilot.cdk 	schm.cdk \
	tr3d.cdk 
blcloud2.o: blcloud2.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
bld_e_budget.o: bld_e_budget.ftn90 	modd_csts.o 
blg2.o: blg2.ftn 	impnone.cdk 	maxlev.cdk 
blkcol.o: blkcol.ftn 	out.cdk 
blocstat.o: blocstat.ftn 	adw.cdk 	glb_ld.cdk 	impnone.cdk \
	lctl.cdk 	ptopo.cdk 	step.cdk 	tr3d.cdk \
	v4dg.cdk 	vt1.cdk 
blweight2.o: blweight2.ftn 	impnone.cdk 	phy_macros_f.h 
bmf_perturb.o: bmf_perturb.ftn 	e_anal.cdk 
bourge1.o: bourge1.ftn 	consphy.cdk 	impnone.cdk 
bubble.o: bubble.ftn 	acq.cdk 	acq_int.cdk 	bubble.cdk \
	cstv.cdk 	dcst.cdk 	geomg.cdk 	glb_ld.cdk \
	grd.cdk 	ind.cdk 	lun.cdk 	out1.cdk \
	pres.cdk 	ptopo.cdk 	theo.cdk 	tr3d.cdk \
	vt1.cdk 
bubble_cfg.o: bubble_cfg.ftn 	adw.cdk 	bubble.cdk 	cstv.cdk \
	eigv.cdk 	geomg.cdk 	glb_ld.cdk 	glb_pil.cdk \
	grd.cdk 	hblen.cdk 	hgc.cdk 	hybdim.cdk \
	hzd.cdk 	init.cdk 	lctl.cdk 	lun.cdk \
	mtn.cdk 	offc.cdk 	out1.cdk 	p_geol.cdk \
	pilot.cdk 	pres.cdk 	ptopo.cdk 	schm.cdk \
	step.cdk 	theo.cdk 	theonml.cdk 	tr3d.cdk \
	vspng.cdk 	vtopo.cdk 	zblen.cdk 
c_bloccplg.o: c_bloccplg.ftn 	glb_ld.cdk 	impnone.cdk 	lctl.cdk \
	lun.cdk 	step.cdk 	vt1.cdk 
c_cplg_iml.o: c_cplg_iml.ftn 	impnone.cdk 	lun.cdk 
c_cplg_surf.o: c_cplg_surf.ftn 	c_cplg.cdk 	glb_ld.cdk 	impnone.cdk \
	ptopo.cdk 
c_fillbus.o: c_fillbus.ftn 	busind.cdk 	c_cplg.cdk 	glb_ld.cdk \
	p_bus.cdk 
c_finddim.o: c_finddim.ftn 	impnone.cdk 
c_getbus.o: c_getbus.ftn 	busind.cdk 	c_cplg.cdk 	glb_ld.cdk \
	p_bus.cdk 
c_init.o: c_init.ftn 	impnone.cdk 
c_init_cplfld.o: c_init_cplfld.ftn 	c_cplg.cdk 	c_nml.cdk 	glb_ld.cdk \
	impnone.cdk 	lun.cdk 	p_pbl.cdk 	path.cdk \
	ptopo.cdk 	schm.cdk 
c_init_oasisfld.o: c_init_oasisfld.ftn 	c_cplg.cdk 	impnone.cdk 
c_mtlmap.o: c_mtlmap.ftn	 
c_oasis_get.o: c_oasis_get.ftn90	 
c_oasis_init.o: c_oasis_init.ftn90	 
c_oasis_partition.o: c_oasis_partition.ftn90	 
c_oasis_put.o: c_oasis_put.ftn90	 
c_screen.o: c_screen.ftn 	impnone.cdk 	lun.cdk 
c_set_coupling.o: c_set_coupling.ftn 	c_cplg.cdk 	cstv.cdk 	geomn.cdk \
	glb_ld.cdk 	impnone.cdk 	ptopo.cdk 	rstr.cdk 
c_stop_coupling.o: c_stop_coupling.ftn 	c_cplg.cdk 	impnone.cdk 
calcalb1.o: calcalb1.ftn 	impnone.cdk 	maxlev.cdk 	options.cdk 
calccovf.o: calccovf.ftn 	impnone.cdk 	maxlev.cdk 	nclassvg.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 	tebcst.cdk 
calcdiag.o: calcdiag.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	options.cdk 	phy_macros_f.h 	phybus.cdk 
calcz0.o: calcz0.ftn 	consphy.cdk 	impnone.cdk 
calddqr.o: calddqr.ftn 	geomg.cdk 	glb_ld.cdk 	impnone.cdk \
	intuv.cdk 	inuvl.cdk 
caldiv_2.o: caldiv_2.ftn 	geomg.cdk 	glb_ld.cdk 
calomeg.o: calomeg.ftn 	geomg.cdk 	glb_ld.cdk 	impnone.cdk 
calvor.o: calvor.ftn 	geomg.cdk 	glb_ld.cdk 	impnone.cdk \
	intuv.cdk 	inuvl.cdk 	schm.cdk 
calzz.o: calzz.ftn 	cstv.cdk 	dcst.cdk 	glb_ld.cdk \
	lctl.cdk 
cartall.o: cartall.ftn 	impnone.cdk 
check_parti.o: check_parti.ftn 	impnone.cdk 	lun.cdk 
class300.o: class300.ftn 	classlvls.cdk 	consphy.cdk 	dimsurf.cdk \
	impnone.cdk 	indx_sfc.cdk 	locbus.cdk 	maxlev.cdk \
	nbvarsurf.cdk 	options.cdk 	phy_macros_f.h 	sfcbus.cdk \
	xptsurf.cdk 	zuzt.cdk 
class320.o: class320.ftn	 
class330.o: class330.ftn 	classlvls.cdk 	consphy.cdk 	dimsurf.cdk \
	impnone.cdk 	indx_sfc.cdk 	locbus.cdk 	maxlev.cdk \
	mosaic.cdk 	nbvarsurf.cdk 	nclassvg.cdk 	options.cdk \
	phy_macros_f.h 	sfcbus.cdk 	tuiles_red.cdk 	xptsurf.cdk \
	zuzt.cdk 
class_const.o: class_const.ftn 	class_com.cdk 
cldoptx4.o: cldoptx4.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
cldwin.o: cldwin.ftn 	comphy.cdk 	consphy.cdk 	dintern.cdk \
	fintern.cdk 	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
climphs3.o: climphs3.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	options.cdk 	phy_macros_f.h 	phybus.cdk \
	surfacepar.cdk 
cllaij.o: cllaij.ftn 	impnone.cdk 	xst.cdk 
clsgs2.o: clsgs2.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
co2info.o: co2info.ftn 	impnone.cdk 
coef_ver_interp_lin2d.o: coef_ver_interp_lin2d.ftn90 	modd_ver_interp_lin.o 
coherence.o: coherence.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	isbapar.cdk 	maxlev.cdk 	options.cdk 	phy_macros_f.h \
	phybus.cdk 	surfacepar.cdk 
condload.o: condload.ftn 	impnone.cdk 
conds.o: conds.ftn 	comphy.cdk 	consphy.cdk 	dintern.cdk \
	fintern.cdk 	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
conray2.o: conray2.ftn 	consphy.cdk 	impnone.cdk 	valcon.cdk 
conres1.o: conres1.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
consun1.o: consun1.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	fmroft.cdk 	impnone.cdk 	phy_macros_f.h 
copybus.o: copybus.ftn 	dimsurf.cdk 	impnone.cdk 	indx_sfc.cdk \
	nbvarsurf.cdk 	phy_macros_f.h 	sfcbus.cdk 
coriol8.o: coriol8.ftn 	dcst.cdk 	glb_ld.cdk 	lun.cdk \
	p_geol.cdk 	schm.cdk 
coupling_teb.o: coupling_teb.ftn90 	modd_csts.o  \
	modd_surf_par.o  	modd_teb.o  \
	modd_town.o  	modd_type_snow.o  \
	modi_teb.o 
coupling_teb_orography.o: coupling_teb_orography.ftn90 	modd_teb.o  \
	modd_type_snow.o  	modi_coupling_teb.o  \
	modi_forcing_vert_shift.o 
ctmdiag.o: ctmdiag.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	phy_macros_f.h 	phybus.cdk 	ribcom.cdk 	vamin.cdk 
datf2p.o: datf2p.ftn 	impnone.cdk 
datp2f.o: datp2f.ftn 	impnone.cdk 
default_prep_teb.o: default_prep_teb.ftn90 	modd_csts.o  \
	modd_prep_teb.o  	modd_surf_par.o  \
	modd_teb.o  	modd_type_snow.o 
default_teb.o: default_teb.ftn90 	modd_surf_par.o 
diagno_cw_rad.o: diagno_cw_rad.ftn 	impnone.cdk 	phy_macros_f.h 	phybus.cdk 
diagnosurf.o: diagnosurf.ftn 	impnone.cdk 	indx_sfc.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 
diasurf2.o: diasurf2.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h \
	stabfunc.cdk 	surfcon.cdk 
difuvd1.o: difuvd1.ftn 	impnone.cdk 
difuvd2.o: difuvd2.ftn 	impnone.cdk 
difuvd4.o: difuvd4.ftn 	impnone.cdk 
difuvd4n.o: difuvd4n.ftn 	impnone.cdk 
difuvd8.o: difuvd8.ftn 	impnone.cdk 
difuvd9.o: difuvd9.ftn 	impnone.cdk 
difuvdf.o: difuvdf.ftn 	impnone.cdk 
difuvdfj.o: difuvdfj.ftn 	impnone.cdk 	phy_macros_f.h 
difuvt.o: difuvt.ftn	 
difvef.o: difvef.ftn 	impnone.cdk 
difver6.o: difver6.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	options.cdk 	phy_macros_f.h 	phybus.cdk \
	stk.cdk 
digflt.o: digflt.ftn 	glb_ld.cdk 	impnone.cdk 	init.cdk \
	lctl.cdk 	lun.cdk 	mem.cdk 	p_bus.cdk \
	rstr.cdk 	schm.cdk 	tr3d.cdk 	vt1.cdk \
	vta.cdk 	vtx.cdk 
doout.o: doout.ftn 	dimout.cdk 	init.cdk 	lctl.cdk \
	outd.cdk 	outp.cdk 	rstr.cdk 	timestep.cdk \
	v4dg.cdk 
drag2.o: drag2.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
dtfrz.o: dtfrz.ftn 	impnone.cdk 
dtfrznew.o: dtfrznew.ftn 	impnone.cdk 
dumpbus2.o: dumpbus2.ftn 	buses.cdk 	impnone.cdk 
dvrtdf.o: dvrtdf.ftn 	impnone.cdk 
dvrtef.o: dvrtef.ftn 	impnone.cdk 
dynstep.o: dynstep.ftn 	glb_ld.cdk 	init.cdk 	lctl.cdk \
	lun.cdk 	orh.cdk 	pilot.cdk 	rstr.cdk \
	schm.cdk 	step.cdk 	vtopo.cdk 
e_arak.o: e_arak.ftn 	dcst.cdk 	e_geomg.cdk 	e_grids.cdk \
	e_intuv.cdk 	e_inuvl.cdk 	e_schm.cdk 	impnone.cdk 
e_arret.o: e_arret.ftn 	e_fu.cdk 	impnone.cdk 
e_bmfrd.o: e_bmfrd.ftn 	e_anal.cdk 	e_fu.cdk 	e_grids.cdk \
	e_schm.cdk 	e_topo.cdk 	pilot.cdk 
e_bmfsplitxy2.o: e_bmfsplitxy2.ftn 	bmf.cdk 	impnone.cdk 
e_coefdgf.o: e_coefdgf.ftn 	dcst.cdk 	impnone.cdk 
e_dateo.o: e_dateo.ftn 	bmf.cdk 	e_cdate.cdk 	e_fu.cdk \
	e_grids.cdk 	out1.cdk 	pilot.cdk 
e_decal.o: e_decal.ftn 	impnone.cdk 
e_dynfld.o: e_dynfld.ftn 	bmf.cdk 	e_anal.cdk 	e_cdate.cdk \
	e_fu.cdk 	e_grids.cdk 	e_schm.cdk 	e_topo.cdk \
	hblen.cdk 	out1.cdk 	pilot.cdk 	ptopo.cdk 
e_filtopx.o: e_filtopx.ftn 	e_dim.cdk 	e_geomg.cdk 	e_grids.cdk \
	e_topo.cdk 	impnone.cdk 
e_findmod.o: e_findmod.ftn 	impnone.cdk 
e_gemnml.o: e_gemnml.ftn 	cst_lis.cdk 	dcst.cdk 	e_anal.cdk \
	e_geol.cdk 	e_grids.cdk 	e_nml.cdk 	e_schm.cdk \
	e_topo.cdk 	e_tr.cdk 	grd.cdk 	hblen.cdk \
	offline.cdk 	out1.cdk 	p_pbl.cdk 	pilot.cdk \
	ptopo.cdk 
e_gemntr.o: e_gemntr.ftn 	e_geoerr.cdk 	e_schm.cdk 	p_bus.cdk \
	version.cdk 
e_geopfld.o: e_geopfld.ftn 	e_anal.cdk 	e_cdate.cdk 	e_fu.cdk \
	e_grids.cdk 	e_schm.cdk 	e_topo.cdk 	hgc.cdk \
	p_bus.cdk 	p_geobus.cdk 	p_geobusid.cdk 
e_geopini.o: e_geopini.ftn 	e_fu.cdk 	e_geol.cdk 	glb_ld.cdk \
	p_bus.cdk 	p_geobus.cdk 	p_geobusid.cdk 	p_pbl.cdk 
e_grid.o: e_grid.ftn 	e_grids.cdk 	grd.cdk 	hgc.cdk 
e_infiles.o: e_infiles.ftn 	filename.cdk 
e_int_ix2xu.o: e_int_ix2xu.ftn 	e_grids.cdk 	e_intuv.cdk 	e_inuvl.cdk \
	e_schm.cdk 	impnone.cdk 
e_int_iy2yv.o: e_int_iy2yv.ftn 	e_grids.cdk 	e_intuv.cdk 	e_inuvl.cdk \
	e_schm.cdk 	impnone.cdk 
e_intgeo.o: e_intgeo.ftn 	e_cdate.cdk 	e_fu.cdk 	e_geoerr.cdk \
	e_grids.cdk 	e_schm.cdk 	hgc.cdk 	p_bus.cdk 
e_intpoly.o: e_intpoly.ftn 	impnone.cdk 
e_intthm.o: e_intthm.ftn 	dcst.cdk 	e_anal.cdk 	e_fu.cdk \
	e_grids.cdk 	e_option.cdk 	e_schm.cdk 	e_tr.cdk \
	hgc.cdk 
e_intwind.o: e_intwind.ftn 	bmf.cdk 	dcst.cdk 	e_anal.cdk \
	e_cdate.cdk 	e_fu.cdk 	e_grids.cdk 	e_option.cdk \
	e_schm.cdk 	e_topo.cdk 	grd.cdk 	hgc.cdk 
e_liaccu.o: e_liaccu.ftn 	e_anal.cdk 	e_fu.cdk 	e_grids.cdk \
	e_schm.cdk 	e_topo.cdk 	offline.cdk 	pilot.cdk 
e_moclim2.o: e_moclim2.ftn 	impnone.cdk 
e_modgl.o: e_modgl.ftn 	dcst.cdk 	e_geol.cdk 	e_grids.cdk \
	impnone.cdk 	p_bus.cdk 	p_geobus.cdk 	p_geobusid.cdk 
e_modhs.o: e_modhs.ftn 	dcst.cdk 	e_geol.cdk 	e_grids.cdk \
	impnone.cdk 	p_bus.cdk 	p_geobus.cdk 	p_geobusid.cdk 
e_modmg.o: e_modmg.ftn 	dcst.cdk 	e_geol.cdk 	e_grids.cdk \
	impnone.cdk 	p_bus.cdk 	p_geobus.cdk 	p_geobusid.cdk 
e_nml2bin.o: e_nml2bin.ftn 	dimout.cdk 	e_grids.cdk 	e_schm.cdk \
	e_tr.cdk 	grd.cdk 	hblen.cdk 	hgc.cdk \
	offline.cdk 	out1.cdk 	p_pbl.cdk 	pilot.cdk \
	ptopo.cdk 
e_ntrxyfil.o: e_ntrxyfil.ftn 	dcst.cdk 	e_geomg.cdk 	e_grids.cdk \
	impnone.cdk 
e_pilotf.o: e_pilotf.ftn 	e_fu.cdk 	filename.cdk 
e_pregeo.o: e_pregeo.ftn 	dcst.cdk 	e_geol.cdk 	e_grids.cdk \
	hgc.cdk 	p_bus.cdk 	p_geobus.cdk 	p_geobusid.cdk \
	p_pbl.cdk 
e_premoy.o: e_premoy.ftn 	impnone.cdk 
e_pretopo.o: e_pretopo.ftn 	dcst.cdk 	e_dim.cdk 	e_geoma.cdk \
	e_geomg.cdk 	e_grids.cdk 	e_topo.cdk 
e_rdhint3.o: e_rdhint3.ftn 	e_anal.cdk 	e_cdate.cdk 	e_grids.cdk \
	hgc.cdk 	impnone.cdk 
e_screen.o: e_screen.ftn 	impnone.cdk 
e_setdgf.o: e_setdgf.ftn 	e_dgf.cdk 	e_grids.cdk 	e_topo.cdk \
	impnone.cdk 
e_setintuv.o: e_setintuv.ftn 	dcst.cdk 	e_geomg.cdk 	e_grids.cdk \
	e_intuv.cdk 	e_schm.cdk 	impnone.cdk 
e_setinuvl.o: e_setinuvl.ftn 	dcst.cdk 	e_geomg.cdk 	e_grids.cdk \
	e_inuvl.cdk 	impnone.cdk 
e_sgrid.o: e_sgrid.ftn	 
e_sortr.o: e_sortr.ftn 	impnone.cdk 
e_specanal.o: e_specanal.ftn 	bmf.cdk 	e_anal.cdk 	e_cdate.cdk \
	e_fu.cdk 	e_grids.cdk 	e_option.cdk 	e_schm.cdk \
	out1.cdk 	ptopo.cdk 
e_trinit.o: e_trinit.ftn 	e_tr.cdk 
e_wlogntr.o: e_wlogntr.ftn 	e_option.cdk 	impnone.cdk 	out1.cdk 
e_wrgeop.o: e_wrgeop.ftn 	e_grids.cdk 	impnone.cdk 	lun.cdk \
	p_bus.cdk 	p_geobus.cdk 	p_geobusid.cdk 
ebudget.o: ebudget.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
ecmwf_ab.o: ecmwf_ab.ftn 	impnone.cdk 
ecris_fst2.o: ecris_fst2.ftn 	out.cdk 
ecris_fst2m.o: ecris_fst2m.ftn 	out.cdk 
emicrog.o: emicrog.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 	sedipara.cdk 
envirtht.o: envirtht.ftn 	impnone.cdk 
equivmount.o: equivmount.ftn 	impnone.cdk 	phy_macros_f.h 
eturbl6.o: eturbl6.ftn 	clefcon.cdk 	consphy.cdk 	impnone.cdk \
	machcon.cdk 	maxlev.cdk 	options.cdk 	phy_macros_f.h \
	surfcon.cdk 
extdiag.o: extdiag.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 	sercmdk.cdk 
fcpara2.o: fcpara2.ftn 	impnone.cdk 	phy_macros_f.h 
fcrest5.o: fcrest5.ftn 	clefcon.cdk 	consphy.cdk 	dintern.cdk \
	fintern.cdk 	fintern90.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	scfrst.cdk 	surfcon.cdk 
feseri.o: feseri.ftn 	consphy.cdk 	impnone.cdk 
ficemxp.o: ficemxp.ftn 	impnone.cdk 
fillagg.o: fillagg.ftn 	dimsurf.cdk 	impnone.cdk 	indx_sfc.cdk \
	nbvarsurf.cdk 	phy_macros_f.h 	sfcbus.cdk 	xptsurf.cdk 
filter.o: filter.ftn 	geomg.cdk 	glb_ld.cdk 	impnone.cdk 
flxsurf3.o: flxsurf3.ftn 	consphy.cdk 	impnone.cdk 	stabfunc.cdk \
	surfcon.cdk 
fomichev.o: fomichev.ftn 	impnone.cdk 
force_restore.o: force_restore.ftn 	clefcon.cdk 	consphy.cdk 	dimsurf.cdk \
	dintern.cdk 	fintern.cdk 	fintern90.cdk 	impnone.cdk \
	indx_sfc.cdk 	locbus.cdk 	machcon.cdk 	maxlev.cdk \
	nbvarsurf.cdk 	options.cdk 	phy_macros_f.h 	scfrst.cdk \
	sfcbus.cdk 	surfcon.cdk 	vamin.cdk 	xptsurf.cdk \
	zuzt.cdk 
forcing_vert_shift.o: forcing_vert_shift.ftn90 	modd_atm_cst.o  \
	modd_csts.o  	mode_thermos.o 
frstgss.o: frstgss.ftn 	cori.cdk 	glb_ld.cdk 	impnone.cdk \
	lun.cdk 	schm.cdk 	vt0.cdk 	vt1.cdk 
ftridi6.o: ftridi6.ftn 	impnone.cdk 
ftridi7.o: ftridi7.ftn 	impnone.cdk 
gather.o: gather.ftn 	impnone.cdk 
gefstop.o: gefstop.ftn 	impnone.cdk 	lun.cdk 
geleyn.o: geleyn.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 
gem_ctrl.o: gem_ctrl.ftn 	impnone.cdk 	init.cdk 	lctl.cdk \
	lun.cdk 	rstr.cdk 	step.cdk 	v4dg.cdk 
gem_run.o: gem_run.ftn 	glb_ld.cdk 	hzd.cdk 	init.cdk \
	lctl.cdk 	lun.cdk 	p_clim.cdk 	rstr.cdk \
	schm.cdk 	step.cdk 	v4dg.cdk 	vrtd.cdk 
gem_stop.o: gem_stop.ftn 	lun.cdk 
gemdm.o: gemdm.ftn 	lctl.cdk 	lun.cdk 	mult.cdk \
	step.cdk 	v4dg.cdk 
gemtim.o: gemtim.ftn	 
genab.o: genab.ftn 	impnone.cdk 
genab2.o: genab2.ftn	 
geneigl.o: geneigl.ftn 	lun.cdk 
geophy.o: geophy.ftn 	impnone.cdk 
gesdict.o: gesdict.ftn 	buses.cdk 	impnone.cdk 
get_luout.o: get_luout.ftn90	 
getbpm.o: getbpm.ftn 	impnone.cdk 
getbus1.o: getbus1.ftn 	buses.cdk 	impnone.cdk 
getindx.o: getindx.ftn 	impnone.cdk 	p_bus.cdk 
getp0.o: getp0.ftn 	dcst.cdk 	impnone.cdk 
glaciers1.o: glaciers1.ftn 	consphy.cdk 	dimsurf.cdk 	dintern.cdk \
	fintern.cdk 	fintern90.cdk 	impnone.cdk 	indx_sfc.cdk \
	locbus.cdk 	maxlev.cdk 	nbvarsurf.cdk 	options.cdk \
	phy_macros_f.h 	sfcbus.cdk 	snoh0.cdk 	vamin.cdk \
	xptsurf.cdk 	zuzt.cdk 
glb_bound.o: glb_bound.ftn 	glb_ld.cdk 	lun.cdk 	schm.cdk \
	theo.cdk 	zblen.cdk 
glbcolc.o: glbcolc.ftn 	impnone.cdk 
glbdist.o: glbdist.ftn 	glb_ld.cdk 	impnone.cdk 
glbpos.o: glbpos.ftn 	glb_ld.cdk 	impnone.cdk 	lun.cdk \
	ptopo.cdk 
glbstat.o: glbstat.ftn 	glb_ld.cdk 	impnone.cdk 	lctl.cdk \
	ptopo.cdk 
gwd5.o: gwd5.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 
gwdfx95a.o: gwdfx95a.ftn 	impnone.cdk 	phy_macros_f.h 
gz2p0.o: gz2p0.ftn 	dcst.cdk 	geomg.cdk 	glb_ld.cdk \
	impnone.cdk 	lun.cdk 
gzcalcul.o: gzcalcul.ftn 	impnone.cdk 
hatoprg.o: hatoprg.ftn 	glb_ld.cdk 	impnone.cdk 
hdif0.o: hdif0.ftn 	glb_ld.cdk 	lctl.cdk 	v4dg.cdk 
hdif_phy.o: hdif_phy.ftn 	glb_ld.cdk 	lctl.cdk 	schm.cdk \
	v4dg.cdk 
heatcoef.o: heatcoef.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
height_sponge.o: height_sponge.ftn 	dcst.cdk 	geomg.cdk 	glb_ld.cdk \
	glb_pil.cdk 	lun.cdk 	mtn.cdk 	nest.cdk \
	p_geof.cdk 	schm.cdk 	vt0.cdk 	zblen.cdk 
hor_interpol.o: hor_interpol.ftn90 	modd_prep.o 
horwavg.o: horwavg.ftn 	dcst.cdk 	geomg.cdk 	glb_ld.cdk \
	impnone.cdk 	ptopo.cdk 
hspng_abc.o: hspng_abc.ftn 	glb_ld.cdk 	grd.cdk 	hspng.cdk \
	impnone.cdk 	ldnh.cdk 	trp.cdk 
hspng_drv.o: hspng_drv.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	hspng.cdk 	hzd.cdk 	impnone.cdk \
	ldnh.cdk 	lun.cdk 	opr.cdk 	ptopo.cdk \
	schm.cdk 	trp.cdk 
hspng_main.o: hspng_main.ftn 	cstv.cdk 	geomg.cdk 	glb_ld.cdk \
	hspng.cdk 	impnone.cdk 	lun.cdk 	p_geof.cdk \
	schm.cdk 	vt0.cdk 	vt1.cdk 	vtx.cdk 
hydro1.o: hydro1.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	isbapar.cdk 	phy_macros_f.h \
	surfacepar.cdk 
hzd_abc.o: hzd_abc.ftn 	glb_ld.cdk 	grd.cdk 	hzd.cdk \
	impnone.cdk 	trp.cdk 
hzd_areal.o: hzd_areal.ftn 	glb_ld.cdk 	glb_pil.cdk 	impnone.cdk 
hzd_areav.o: hzd_areav.ftn 	impnone.cdk 
hzd_bfct.o: hzd_bfct.ftn 	glb_ld.cdk 	glb_pil.cdk 	impnone.cdk 
hzd_bfct_1d.o: hzd_bfct_1d.ftn 	glb_ld.cdk 	glb_pil.cdk 	impnone.cdk 
hzd_del2.o: hzd_del2.ftn 	glb_ld.cdk 	ptopo.cdk 
hzd_delpwr.o: hzd_delpwr.ftn 	glb_ld.cdk 	glb_pil.cdk 	impnone.cdk 
hzd_delpwr_1d.o: hzd_delpwr_1d.ftn 	glb_ld.cdk 	glb_pil.cdk 	impnone.cdk 
hzd_exhrdif.o: hzd_exhrdif.ftn 	glb_ld.cdk 	hzd.cdk 	impnone.cdk 
hzd_fact.o: hzd_fact.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	hzd.cdk 	ldnh.cdk 	lun.cdk \
	opr.cdk 	ptopo.cdk 	schm.cdk 	trp.cdk 
hzd_ho.o: hzd_ho.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	glb_pil.cdk 	hzd.cdk 	ldnh.cdk \
	lun.cdk 	opr.cdk 	ptopo.cdk 	schm.cdk \
	trp.cdk 
hzd_ho_parite.o: hzd_ho_parite.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	hzd.cdk 	ldnh.cdk 	lun.cdk \
	opr.cdk 	ptopo.cdk 	schm.cdk 	trp.cdk 
hzd_hof.o: hzd_hof.ftn 	cstv.cdk 	dcst.cdk 	fft.cdk \
	geomg.cdk 	glb_ld.cdk 	hzd.cdk 	ldnh.cdk \
	lun.cdk 	opr.cdk 	ptopo.cdk 	schm.cdk \
	trp.cdk 
hzd_hof_lam.o: hzd_hof_lam.ftn 	cstv.cdk 	dcst.cdk 	fft.cdk \
	geomg.cdk 	glb_ld.cdk 	hzd.cdk 	impnone.cdk \
	ldnh.cdk 	lun.cdk 	opr.cdk 	ptopo.cdk \
	schm.cdk 	trp.cdk 
hzd_hoffld.o: hzd_hoffld.ftn 	fft.cdk 	geomg.cdk 	glb_ld.cdk \
	hzd.cdk 	lun.cdk 	opr.cdk 	ptopo.cdk \
	trp.cdk 
hzd_main.o: hzd_main.ftn 	cstv.cdk 	eigv.cdk 	fft.cdk \
	geomg.cdk 	glb_ld.cdk 	grd.cdk 	hzd.cdk \
	lctl.cdk 	lun.cdk 	p_geof.cdk 	schm.cdk \
	vt0.cdk 	vt1.cdk 	vtx.cdk 
hzd_nudeln.o: hzd_nudeln.ftn 	glb_ld.cdk 
hzd_set.o: hzd_set.ftn 	cstv.cdk 	dcst.cdk 	eigv.cdk \
	fft.cdk 	geomg.cdk 	glb_ld.cdk 	glb_pil.cdk \
	hspng.cdk 	hzd.cdk 	impnone.cdk 	lun.cdk \
	opr.cdk 	ptopo.cdk 	trp.cdk 	vspng.cdk 
hzd_set_ops8.o: hzd_set_ops8.ftn 	impnone.cdk 
hzd_solfft.o: hzd_solfft.ftn 	ptopo.cdk 
hzd_solfft_1d.o: hzd_solfft_1d.ftn 	ptopo.cdk 
hzd_solfft_lam.o: hzd_solfft_lam.ftn 	glb_ld.cdk 	glb_pil.cdk 	impnone.cdk \
	ptopo.cdk 
hzd_solmxma.o: hzd_solmxma.ftn 	glb_ld.cdk 	glb_pil.cdk 	ptopo.cdk 
hzd_solparite_2.o: hzd_solparite_2.ftn 	ptopo.cdk 
hzd_solparite_u.o: hzd_solparite_u.ftn 	ptopo.cdk 
incdatsd.o: incdatsd.ftn	 
inctphy.o: inctphy.ftn 	consphy.cdk 	impnone.cdk 
inctphy3.o: inctphy3.ftn 	consphy.cdk 	impnone.cdk 
indata.o: indata.ftn 	bmf.cdk 	glb_ld.cdk 	ind.cdk \
	lctl.cdk 	out1.cdk 	p_geof.cdk 	schm.cdk \
	theo.cdk 	v4dg.cdk 	vt1.cdk 	vtopo.cdk \
	vtx.cdk 
indseri.o: indseri.ftn 	impnone.cdk 
ini_csts.o: ini_csts.ftn90 	modd_csts.o 
inichamp2.o: inichamp2.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	options.cdk 	phy_macros_f.h 	phybus.cdk 
inicover.o: inicover.ftn 	classlvls.cdk 	impnone.cdk 	maxlev.cdk \
	mosaic.cdk 	nclassvg.cdk 	options.cdk 	phy_macros_f.h \
	phybus.cdk 	tuiles.cdk 
inicoverurb.o: inicoverurb.ftn 	impnone.cdk 	maxlev.cdk 	nclassvg.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 	tebcst.cdk 
inifcp.o: inifcp.ftn 	impnone.cdk 
inikey.o: inikey.ftn 	busind.cdk 	c_cplg.cdk 	glb_ld.cdk \
	p_cond.cdk 	schm.cdk 	tr3d.cdk 
inimos.o: inimos.ftn 	classlvls.cdk 	impnone.cdk 	maxlev.cdk \
	mosaic.cdk 	nclassvg.cdk 	options.cdk 	phy_macros_f.h \
	phybus.cdk 	tuiles.cdk 
iniobus.o: iniobus.ftn 	dcst.cdk 	dimout.cdk 	glb_ld.cdk \
	glb_pil.cdk 	impnone.cdk 	lctl.cdk 	lun.cdk \
	obus.cdk 	outp.cdk 	p_bus.cdk 
iniptsurf.o: iniptsurf.ftn 	buses.cdk 	dimsurf.cdk 	impnone.cdk \
	indx_sfc.cdk 	maxlev.cdk 	nbvarsurf.cdk 	options.cdk \
	phy_macros_f.h 	sfcbus.cdk 
iniras.o: iniras.ftn 	impnone.cdk 	rasdata.cdk 
inisoili.o: inisoili.ftn 	impnone.cdk 	phy_macros_f.h 	phybus.cdk 
inisurf1.o: inisurf1.ftn 	classlvls.cdk 	consphy.cdk 	himin.cdk \
	icelvls.cdk 	impnone.cdk 	indx_sfc.cdk 	isbapar.cdk \
	leads.cdk 	maxlev.cdk 	mosaic.cdk 	nclassvg.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 	surfacepar.cdk 
init_snow_lw.o: init_snow_lw.ftn90 	modd_csts.o  \
	modd_snow_par.o  	modd_surf_par.o  \
	modd_type_snow.o  	mode_snow3l.o 
init_teb.o: init_teb.ftn90 	modd_snow_par.o  \
	modd_surf_par.o  	modd_teb.o  \
	modd_town.o  	modd_type_snow.o  \
	modi_averaged_albedo_teb.o  	modi_averaged_tsrad_teb.o  \
	modi_default_teb.o  	modi_init_snow_lw.o 
initial.o: initial.ftn 	cstv.cdk 	dcst.cdk 	glb_ld.cdk \
	impnone.cdk 	init.cdk 	lctl.cdk 	lun.cdk \
	rstr.cdk 	schm.cdk 	step.cdk 
initown.o: initown.f90 	modd_csts.o  	modd_prep.o \
 	modd_surf_par.o  	modd_teb.o \
 	modd_teb_grid.o  	modd_town.o \
 	modd_type_snow.o  	modi_allocate_gr_snow.o \
 	modi_ini_csts.o  	modi_init_teb.o \
 	modi_possol.o  	modi_prep_teb.o \
 	modi_sunpos.o 
initown.o: initown.ftn90 	impnone.cdk 	indx_sfc.cdk 	maxlev.cdk \
 	modd_csts.o  	modd_prep.o \
 	modd_surf_par.o  	modd_teb.o \
 	modd_teb_grid.o  	modd_town.o \
 	modd_type_snow.o  	modi_allocate_gr_snow.o \
 	modi_ini_csts.o  	modi_init_teb.o \
 	modi_possol.o  	modi_prep_teb.o \
 	modi_sunpos.o 	options.cdk 	phy_macros_f.h \
	phybus.cdk 	tebcst.cdk 
initw2.o: initw2.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	intuv.cdk 	inuvl.cdk 	schm.cdk 
iniveg.o: iniveg.ftn 	classlvls.cdk 	impnone.cdk 	maxlev.cdk \
	nclassvg.cdk 	options.cdk 	phy_macros_f.h 	phybus.cdk 
intchamps.o: intchamps.ftn 	impnone.cdk 
integ2.o: integ2.ftn 	impnone.cdk 
interp_grid1d.o: interp_grid1d.ftn90 	modd_surf_par.o  \
	modi_coef_ver_interp_lin2d.o  	modi_ver_interp_lin2d.o 
interp_grid2d.o: interp_grid2d.ftn90 	modd_surf_par.o  \
	modi_coef_ver_interp_lin2d.o  	modi_ver_interp_lin2d.o 
interpveg.o: interpveg.ftn 	impnone.cdk 
intodif.o: intodif.ftn 	impnone.cdk 
intozon.o: intozon.ftn 	impnone.cdk 	ozopnt.cdk 	raddata.cdk \
	radparam.cdk 
intwat3.o: intwat3.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
inverse.o: inverse.ftn 	impnone.cdk 
ipig.o: ipig.ftn 	impnone.cdk 
isba2.o: isba2.ftn 	clefcon.cdk 	consphy.cdk 	dimsurf.cdk \
	impnone.cdk 	indx_sfc.cdk 	locbus.cdk 	maxlev.cdk \
	nbvarsurf.cdk 	options.cdk 	phy_macros_f.h 	sfcbus.cdk \
	xptsurf.cdk 	zuzt.cdk 
juliand.o: juliand.ftn 	impnone.cdk 
kdifsimp.o: kdifsimp.ftn 	consphy.cdk 	impnone.cdk 
kfcp4.o: kfcp4.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 
ktrsnt3.o: ktrsnt3.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
ktrsnt_mg.o: ktrsnt_mg.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	maxlev.cdk 	options.cdk \
	phy_macros_f.h 
kuo2.o: kuo2.ftn 	comphy.cdk 	consphy.cdk 	dintern.cdk \
	fintern.cdk 	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
kuostd.o: kuostd.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
kuosun.o: kuosun.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
kuosym.o: kuosym.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
lacs.o: lacs.ftn 	consphy.cdk 	impnone.cdk 	leads.cdk \
	phy_macros_f.h 	phybus.cdk 	surfacepar.cdk 
lacs1.o: lacs1.ftn 	consphy.cdk 	impnone.cdk 	leads.cdk \
	phy_macros_f.h 	phybus.cdk 	surfacepar.cdk 
liebman_2.o: liebman_2.ftn 	impnone.cdk 
lin_adjtq.o: lin_adjtq.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
lin_adjtq_ad.o: lin_adjtq_ad.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
lin_adjtq_tl.o: lin_adjtq_tl.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
lin_conds1.o: lin_conds1.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
lin_conds1_ad.o: lin_conds1_ad.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
lin_conds1_tl.o: lin_conds1_tl.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
lin_diff_vert1.o: lin_diff_vert1.ftn 	impnone.cdk 
lin_diff_vert1_ad.o: lin_diff_vert1_ad.ftn 	impnone.cdk 
lin_diff_vert1_tl.o: lin_diff_vert1_tl.ftn 	impnone.cdk 
lin_difuvd1.o: lin_difuvd1.ftn	 
lin_difuvd1_ad.o: lin_difuvd1_ad.ftn	 
lin_difuvd1_tl.o: lin_difuvd1_tl.ftn	 
lin_difuvd2.o: lin_difuvd2.ftn	 
lin_difuvd2_ad.o: lin_difuvd2_ad.ftn	 
lin_difuvd2_tl.o: lin_difuvd2_tl.ftn	 
lin_difver1.o: lin_difver1.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	options.cdk 	phy_macros_f.h 	phybus.cdk \
	stk.cdk 
lin_difver1_ad.o: lin_difver1_ad.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	options.cdk 	phy_macros_f.h 	phybus.cdk \
	stk.cdk 
lin_difver1_tl.o: lin_difver1_tl.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	options.cdk 	phy_macros_f.h 	phybus.cdk \
	stk.cdk 
lin_inichamp1.o: lin_inichamp1.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	options.cdk 	phy_macros_f.h 	phybus.cdk 
lin_inichamp1_tl.o: lin_inichamp1_tl.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	options.cdk 	phy_macros_f.h 	phybus.cdk 
lin_kdif_simp1.o: lin_kdif_simp1.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	indx_sfc.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 	surfcon.cdk \
	zuzt.cdk 
lin_kdif_simp1_ad.o: lin_kdif_simp1_ad.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	indx_sfc.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 	zuzt.cdk 
lin_kdif_simp1_tl.o: lin_kdif_simp1_tl.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	indx_sfc.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 	zuzt.cdk 
lin_kdif_simp1_tr.o: lin_kdif_simp1_tr.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	indx_sfc.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 	surfcon.cdk \
	zuzt.cdk 
lin_kuosym.o: lin_kuosym.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
lin_kuosym_ad.o: lin_kuosym_ad.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
lin_kuosym_tl.o: lin_kuosym_tl.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
lin_phyexe1.o: lin_phyexe1.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 	workspc.cdk 
lin_phyexe1_ad.o: lin_phyexe1_ad.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 	workspc.cdk 
lin_phyexe1_tl.o: lin_phyexe1_tl.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 	workspc.cdk 
lin_precip1.o: lin_precip1.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 
lin_precip1_ad.o: lin_precip1_ad.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 
lin_precip1_tl.o: lin_precip1_tl.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 
lin_sgo1.o: lin_sgo1.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 
lin_sgo1_ad.o: lin_sgo1_ad.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 
lin_sgo1_tl.o: lin_sgo1_tl.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 
lin_sgoflx1.o: lin_sgoflx1.ftn 	impnone.cdk 	phy_macros_f.h 
lin_sgoflx1_ad.o: lin_sgoflx1_ad.ftn 	impnone.cdk 	phy_macros_f.h 
lin_sgoflx1_tl.o: lin_sgoflx1_tl.ftn 	impnone.cdk 	phy_macros_f.h 
liqwc.o: liqwc.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 
litblrad2.o: litblrad2.ftn 	consphy.cdk 	impnone.cdk 	ozopnt.cdk \
	phy_macros_f.h 	raddata.cdk 	radparam.cdk 	radpnt.cdk 
litozon2.o: litozon2.ftn 	impnone.cdk 	ozopnt.cdk 	raddata.cdk \
	radparam.cdk 
llacar.o: llacar.ftn 	impnone.cdk 
lsctrol.o: lsctrol.ftn 	impnone.cdk 	phy_macros_f.h 
mconadj2.o: mconadj2.ftn 	acmcon.cdk 	consphy.cdk 	dintern.cdk \
	fintern.cdk 	fintern90.cdk 	flatscp.cdk 	impnone.cdk \
	phy_macros_f.h 
mesahr.o: mesahr.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	phy_macros_f.h 
mesahu.o: mesahu.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr2.cdk \
	phy_macros_f.h 
mfdlesmx.o: mfdlesmx.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
mfodla.o: mfodla.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
mfodle.o: mfodle.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
mfodqa.o: mfodqa.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr2.cdk \
	phy_macros_f.h 
mfodqs.o: mfodqs.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr2.cdk \
	phy_macros_f.h 
mfoefq.o: mfoefq.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr2.cdk \
	phy_macros_f.h 
mfoeic.o: mfoeic.ftn 	consphy.cdk 	impnone.cdk 	initcph.cdk \
	phy_macros_f.h 
mfoew.o: mfoew.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
mfoewa.o: mfoewa.ftn 	consphy.cdk 	impnone.cdk 	initcph.cdk \
	phy_macros_f.h 
mfohr.o: mfohr.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr2.cdk \
	phy_macros_f.h 
mfohra.o: mfohra.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr2.cdk \
	phy_macros_f.h 
mfoqfe.o: mfoqfe.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr2.cdk \
	phy_macros_f.h 
mfoqsa.o: mfoqsa.ftn 	consphy.cdk 	impnone.cdk 	initcph.cdk \
	modpr2.cdk 	phy_macros_f.h 
mfoqst.o: mfoqst.ftn 	consphy.cdk 	impnone.cdk 	initcph.cdk \
	modpr2.cdk 	phy_macros_f.h 
mfottv.o: mfottv.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
mfottv_ad.o: mfottv_ad.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
mfottv_tl.o: mfottv_tl.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
mfottvh.o: mfottvh.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
mfotvht.o: mfotvht.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
mfotvt.o: mfotvt.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
mfotvt_ad.o: mfotvt_ad.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
mfotvt_tl.o: mfotvt_tl.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
mgsdict.o: mgsdict.ftn 	impnone.cdk 	p_bus.cdk 
mhraes.o: mhraes.ftn 	consphy.cdk 	impnone.cdk 	initcph.cdk \
	phy_macros_f.h 
mhrahu.o: mhrahu.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr2.cdk \
	phy_macros_f.h 
mhuaes.o: mhuaes.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr2.cdk \
	phy_macros_f.h 
mhuahr.o: mhuahr.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr2.cdk \
	phy_macros_f.h 
mirror.o: mirror.ftn 	glb_ld.cdk 	lun.cdk 	schm.cdk \
	tr3d.cdk 	vt0.cdk 
mixlen3.o: mixlen3.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 	scfrst.cdk 
mixphase6.o: mixphase6.ftn 	consphy.cdk 	dzcond.cdk 	impnone.cdk \
	maxlev.cdk 	options.cdk 	phy_macros_f.h 
mnhget_luout.o: mnhget_luout.ftn90	 
modd_atm_cst.o: modd_atm_cst.cdk90  
modd_csts.o: modd_csts.cdk90                                
modd_data_cover_par.o: modd_data_cover_par.cdk90    
modd_isba.o: modd_isba.cdk90   	modd_type_date.o   	modd_type_snow.o    
modd_prep.o: modd_prep.cdk90          
modd_prep_snow.o: modd_prep_snow.cdk90   	modd_prep.o   	modd_snow_par.o   
modd_prep_teb.o: modd_prep_teb.cdk90    
modd_snow_par.o: modd_snow_par.cdk90               
modd_surf_par.o: modd_surf_par.cdk90                    
modd_teb.o: modd_teb.cdk90         	modd_type_snow.o     
modd_teb_grid.o: modd_teb_grid.cdk90      
modd_town.o: modd_town.cdk90          
modd_type_date.o: modd_type_date.cdk90     
modd_type_snow.o: modd_type_snow.cdk90                           
modd_ver_interp_lin.o: modd_ver_interp_lin.cdk90  
mode_snow3l.o: mode_snow3l.cdk90    	modd_csts.o   	modd_snow_par.o   	modd_surf_par.o   
mode_surf_snow_frac.o: mode_surf_snow_frac.cdk90     	modd_snow_par.o   
mode_thermos.o: mode_thermos.cdk90    	modd_csts.o        
modhui.o: modhui.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr2.cdk \
	phy_macros_f.h 
modi_allocate_gr_snow.o: modi_allocate_gr_snow.cdk90     	modd_type_snow.o    
modi_averaged_albedo_teb.o: modi_averaged_albedo_teb.cdk90    	modd_type_snow.o 
modi_averaged_tsrad_teb.o: modi_averaged_tsrad_teb.cdk90    	modd_type_snow.o 
modi_bld_e_budget.o: modi_bld_e_budget.cdk90  
modi_coef_ver_interp_lin2d.o: modi_coef_ver_interp_lin2d.cdk90   
modi_coupling_teb.o: modi_coupling_teb.cdk90  
modi_coupling_teb_orography.o: modi_coupling_teb_orography.cdk90   
modi_default_prep_teb.o: modi_default_prep_teb.cdk90  
modi_default_teb.o: modi_default_teb.cdk90  
modi_forcing_vert_shift.o: modi_forcing_vert_shift.cdk90  
modi_get_luout.o: modi_get_luout.cdk90  
modi_hor_interpol.o: modi_hor_interpol.cdk90   
modi_ini_csts.o: modi_ini_csts.cdk90     
modi_init_snow_lw.o: modi_init_snow_lw.cdk90    	modd_type_snow.o 
modi_init_teb.o: modi_init_teb.cdk90   
modi_interp_grid1d.o: modi_interp_grid1d.cdk90  
modi_interp_grid2d.o: modi_interp_grid2d.cdk90   
modi_mnhget_luout.o: modi_mnhget_luout.cdk90 
modi_possol.o: modi_possol.cdk90     
modi_prep_hor_snow_field.o: modi_prep_hor_snow_field.cdk90   	modd_type_snow.o  
modi_prep_hor_snow_fields.o: modi_prep_hor_snow_fields.cdk90   	modd_type_snow.o  
modi_prep_hor_teb_field.o: modi_prep_hor_teb_field.cdk90  
modi_prep_snow_gem.o: modi_prep_snow_gem.cdk90  
modi_prep_teb.o: modi_prep_teb.cdk90   
modi_prep_teb_gem.o: modi_prep_teb_gem.cdk90  
modi_road_wall_layer_e_budget.o: modi_road_wall_layer_e_budget.cdk90  
modi_roof_layer_e_budget.o: modi_roof_layer_e_budget.cdk90  
modi_snow_cover_1layer.o: modi_snow_cover_1layer.cdk90  
modi_snow_heat_to_t_wliq.o: modi_snow_heat_to_t_wliq.cdk90  
modi_sunpos.o: modi_sunpos.cdk90     
modi_surface_aero_cond.o: modi_surface_aero_cond.cdk90   
modi_surface_cd.o: modi_surface_cd.cdk90  
modi_surface_ri.o: modi_surface_ri.cdk90   
modi_teb.o: modi_teb.cdk90  
modi_tridiag_ground.o: modi_tridiag_ground.cdk90   
modi_urban_drag.o: modi_urban_drag.cdk90  
modi_urban_fluxes.o: modi_urban_fluxes.cdk90  
modi_urban_hydro.o: modi_urban_hydro.cdk90  
modi_urban_lw_coef.o: modi_urban_lw_coef.cdk90     
modi_urban_snow_evol.o: modi_urban_snow_evol.cdk90  
modi_urban_solar_abs.o: modi_urban_solar_abs.cdk90   
modi_vegtype_grid_to_patch_grid.o: modi_vegtype_grid_to_patch_grid.cdk90   	modd_isba.o   	modd_type_date.o   	modd_type_snow.o  
modi_vegtype_to_patch.o: modi_vegtype_to_patch.cdk90  
modi_ver_interp_lin2d.o: modi_ver_interp_lin2d.cdk90   
modpbus.o: modpbus.ftn 	dimsurf.cdk 	impnone.cdk 	indx_sfc.cdk \
	nbvarsurf.cdk 	phy_macros_f.h 	phybus.cdk 
moistke3.o: moistke3.ftn 	clefcon.cdk 	consphy.cdk 	impnone.cdk \
	machcon.cdk 	maxlev.cdk 	options.cdk 	phy_macros_f.h \
	surfcon.cdk 
momcoef.o: momcoef.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
mosavg.o: mosavg.ftn 	dimsurf.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	nbvarsurf.cdk 	options.cdk 	phy_macros_f.h \
	sfcbus.cdk 
moscopy.o: moscopy.ftn 	dimsurf.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	nbvarsurf.cdk 	options.cdk 	phy_macros_f.h \
	sfcbus.cdk 
moscover.o: moscover.ftn 	classlvls.cdk 	impnone.cdk 	nclassvg.cdk \
	tuiles.cdk 
movcar.o: movcar.ftn 	impnone.cdk 
mprecip4.o: mprecip4.ftn 	acmcon.cdk 	consphy.cdk 	dintern.cdk \
	fintern.cdk 	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
mras0.o: mras0.ftn 	comras.cdk 	impnone.cdk 	phy_macros_f.h \
	rasdata.cdk 	tables.cdk 
msoltri6.o: msoltri6.ftn 	ptopo.cdk 
mthtaw.o: mthtaw.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	flatscp.cdk 	impnone.cdk 	initcph.cdk \
	modpr2.cdk 	phy_macros_f.h 
mthtaw2.o: mthtaw2.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr2.cdk \
	phy_macros_f.h 
mtn_case.o: mtn_case.ftn 	acq.cdk 	acq_int.cdk 	cstv.cdk \
	dcst.cdk 	geomg.cdk 	glb_ld.cdk 	glb_pil.cdk \
	grd.cdk 	ind.cdk 	lun.cdk 	mtn.cdk \
	out1.cdk 	pres.cdk 	ptopo.cdk 	theo.cdk \
	tr3d.cdk 	vt1.cdk 	vtopo.cdk 	zblen.cdk 
mtn_cfg.o: mtn_cfg.ftn 	adw.cdk 	bubble.cdk 	cstv.cdk \
	dcst.cdk 	eigv.cdk 	geomg.cdk 	glb_ld.cdk \
	glb_pil.cdk 	grd.cdk 	hblen.cdk 	hgc.cdk \
	hybdim.cdk 	hzd.cdk 	init.cdk 	lctl.cdk \
	lun.cdk 	mtn.cdk 	offc.cdk 	out1.cdk \
	p_geol.cdk 	pilot.cdk 	pres.cdk 	ptopo.cdk \
	schm.cdk 	step.cdk 	theo.cdk 	theonml.cdk \
	tr3d.cdk 	vspng.cdk 	vtopo.cdk 	zblen.cdk 
nes1s.o: nes1s.ftn 	glb_ld.cdk 	hblen.cdk 
nes2s.o: nes2s.ftn 	glb_ld.cdk 	hblen.cdk 
nes3s.o: nes3s.ftn 	glb_ld.cdk 	hblen.cdk 
nes4s.o: nes4s.ftn 	glb_ld.cdk 	hblen.cdk 
nesajr.o: nesajr.ftn 	glb_ld.cdk 
nest_indata.o: nest_indata.ftn 	glb_ld.cdk 	ind.cdk 	nest.cdk \
	schm.cdk 	v4dg.cdk 
nest_init.o: nest_init.ftn 	glb_ld.cdk 	nest.cdk 	schm.cdk \
	tr3d.cdk 	v4dg.cdk 	vt1.cdk 
nest_intt.o: nest_intt.ftn 	bmf.cdk 	cstv.cdk 	glb_ld.cdk \
	lctl.cdk 	nest.cdk 	offline.cdk 	pilot.cdk \
	ptopo.cdk 	schm.cdk 	tr3d.cdk 	v4dg.cdk 
nestit.o: nestit.ftn 	glb_ld.cdk 	hblen.cdk 	lun.cdk \
	nest.cdk 	schm.cdk 	tr3d.cdk 	vt0.cdk 
newrad4.o: newrad4.ftn 	clefcon.cdk 	consphy.cdk 	impnone.cdk \
	indx_sfc.cdk 	maxlev.cdk 	nocld.cdk 	options.cdk \
	ozopnt.cdk 	phy_macros_f.h 	phybus.cdk 	raddata.cdk \
	radparam.cdk 	radpnt.cdk 	solcons.cdk 	stk.cdk 
ngfft.o: ngfft.ftn 	impnone.cdk 
nli.o: nli.ftn 	cori.cdk 	glb_ld.cdk 	impnone.cdk \
	lctl.cdk 	lun.cdk 	nl.cdk 	p_geof.cdk \
	ptopo.cdk 	rhsc.cdk 	schm.cdk 	vt0.cdk \
	vtx.cdk 
nlip_2.o: nlip_2.ftn 	cori.cdk 	cstv.cdk 	dcst.cdk \
	geomg.cdk 	glb_ld.cdk 	intuv.cdk 	inuvl.cdk \
	ptopo.cdk 	schm.cdk 
ntables.o: ntables.ftn 	impnone.cdk 
nuages2.o: nuages2.ftn 	clefcon.cdk 	consphy.cdk 	dintern.cdk \
	fintern.cdk 	fintern90.cdk 	impnone.cdk 	nocld.cdk \
	phy_macros_f.h 
oldrad3.o: oldrad3.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	phy_macros_f.h 	phybus.cdk 	solcons.cdk \
	stk.cdk 
opinv6.o: opinv6.ftn 	impnone.cdk 
opinv7.o: opinv7.ftn 	impnone.cdk 
optriss6.o: optriss6.ftn 	impnone.cdk 
optriss7.o: optriss7.ftn 	impnone.cdk 
out_dq.o: out_dq.ftn 	dcst.cdk 	dimout.cdk 	geomg.cdk \
	glb_ld.cdk 	lctl.cdk 	level.cdk 	lun.cdk \
	out.cdk 	out3.cdk 	outd.cdk 
out_dyn.o: out_dyn.ftn 	cstv.cdk 	dcst.cdk 	dimout.cdk \
	geomg.cdk 	geomn.cdk 	glb_ld.cdk 	grd.cdk \
	grid.cdk 	init.cdk 	lctl.cdk 	level.cdk \
	lun.cdk 	out3.cdk 	outd.cdk 	pres.cdk \
	ptopo.cdk 	rstr.cdk 	schm.cdk 	tr3d.cdk \
 	v4d_prof.o 	v4dg.cdk 	vt0.cdk \
	vt1.cdk 	vt1m.cdk 
out_padbuf.o: out_padbuf.ftn 	out.cdk 
out_phy.o: out_phy.f 	v4d_prof.o 
out_phy.o: out_phy.ftn 	dcst.cdk 	dimout.cdk 	geomg.cdk \
	geomn.cdk 	glb_ld.cdk 	glb_pil.cdk 	grd.cdk \
	grid.cdk 	init.cdk 	level.cdk 	obus.cdk \
	out.cdk 	out3.cdk 	outp.cdk 	p_bus.cdk \
	ptopo.cdk 	rstr.cdk  	v4d_prof.o \
	v4dg.cdk 
out_qc0.o: out_qc0.ftn 	cstv.cdk 	dcst.cdk 	dimout.cdk \
	geomg.cdk 	geomn.cdk 	glb_ld.cdk 	grd.cdk \
	grid.cdk 	init.cdk 	lctl.cdk 	level.cdk \
	lun.cdk 	out.cdk 	out3.cdk 	outd.cdk \
	pres.cdk 	ptopo.cdk 	rstr.cdk 	schm.cdk \
	tr3d.cdk 	vt1.cdk 
out_sblock.o: out_sblock.ftn 	out.cdk 
out_sfile.o: out_sfile.ftn 	out.cdk 
out_sgrid.o: out_sgrid.ftn 	out.cdk 
out_slev.o: out_slev.ftn 	out.cdk 
out_thm.o: out_thm.ftn 	dcst.cdk 	dimout.cdk 	geomg.cdk \
	geomn.cdk 	glb_ld.cdk 	grd.cdk 	lctl.cdk \
	level.cdk 	out.cdk 	out3.cdk 	outd.cdk \
	ptopo.cdk 	schm.cdk 	v4dg.cdk 
out_tracer.o: out_tracer.ftn 	dimout.cdk 	geomg.cdk 	glb_ld.cdk \
	lun.cdk 	out.cdk 	out3.cdk 	outd.cdk \
	tr3d.cdk 
out_uv.o: out_uv.ftn 	dcst.cdk 	dimout.cdk 	geomg.cdk \
	glb_ld.cdk 	lctl.cdk 	level.cdk 	out.cdk \
	out3.cdk 	outd.cdk 	v4dg.cdk 
out_vmm.o: out_vmm.ftn 	dimout.cdk 	geomg.cdk 	geomn.cdk \
	glb_ld.cdk 	grid.cdk 	init.cdk 	lctl.cdk \
	lun.cdk 	orh.cdk 	out.cdk 	out3.cdk \
	outd.cdk 	p_geof.cdk 	rhsc.cdk 	rstr.cdk \
	vt0.cdk 	vt1.cdk 	vt2.cdk 	vta.cdk \
	vth.cdk 	vtx.cdk 
ouvrstrt.o: ouvrstrt.ftn 	impnone.cdk 	lun.cdk 	rstr.cdk 
ozoref2.o: ozoref2.ftn 	consphy.cdk 	impnone.cdk 
p0vt2gz_hyb.o: p0vt2gz_hyb.ftn 	dcst.cdk 	geomg.cdk 	glb_ld.cdk \
	lun.cdk 
p_apply.o: p_apply.ftn 	busind.cdk 	cstv.cdk 	dcst.cdk \
	geomg.cdk 	glb_ld.cdk 	hblen.cdk 	intuv.cdk \
	inuvl.cdk 	lun.cdk 	p_cond.cdk 	schm.cdk \
	v4dg.cdk 	vt0.cdk 	vt1.cdk 
p_config.o: p_config.ftn 	busind.cdk 	c_cplg.cdk 	cstv.cdk \
	dcst.cdk 	geomg.cdk 	glb_ld.cdk 	lctl.cdk \
	lun.cdk 	p4d_simp.cdk 	p_bus.cdk 	p_clim.cdk \
	p_cond.cdk 	p_cond2.cdk 	p_gwd.cdk 	p_out.cdk \
	p_pbl.cdk 	p_pbl2.cdk 	p_pset.cdk 	p_rad.cdk \
	p_str.cdk 	pres.cdk 	schm.cdk 	v4dg.cdk 
p_fcpfw.o: p_fcpfw.ftn 	glb_ld.cdk 	impnone.cdk 	lun.cdk \
	p_cond.cdk 
p_fillbus.o: p_fillbus.ftn 	busind.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	p_bus.cdk 	p_cond.cdk 	p_geol.cdk \
	p_pbl.cdk 	schm.cdk 	tr3d.cdk 
p_inincr.o: p_inincr.ftn 	busind.cdk 	cstv.cdk 	dcst.cdk \
	glb_ld.cdk 	lctl.cdk 	lun.cdk 	mem.cdk \
	out1.cdk 	p_bus.cdk 	p_pbl.cdk 	p_pbl2.cdk \
	p_pset.cdk 	path.cdk 	ptopo.cdk 	rstr.cdk 
p_init.o: p_init.ftn 	busind.cdk 	dimout.cdk 	glb_ld.cdk \
	impnone.cdk 	lun.cdk 	mem.cdk 	p_bus.cdk \
	schm.cdk 
p_main.o: p_main.ftn 	c_cplg.cdk 	cstv.cdk 	dimout.cdk \
	glb_ld.cdk 	impnone.cdk 	init.cdk 	lctl.cdk \
	out2.cdk 	p_bus.cdk 	rstr.cdk 	v4dg.cdk \
	xst.cdk 
p_phyexe.o: p_phyexe.ftn 	glb_ld.cdk 	impnone.cdk 	lctl.cdk \
	lun.cdk 	p_pset.cdk 	ptopo.cdk 
p_physlb.o: p_physlb.ftn 	busind.cdk 	busind_tr.cdk 	c_cplg.cdk \
	cstv.cdk 	dcst.cdk 	dimout.cdk 	geomg.cdk \
	glb_ld.cdk 	lun.cdk 	macro.cdk 	mem.cdk \
	mult.cdk 	obus.cdk 	p_bus.cdk 	p_clim.cdk \
	p_pbl.cdk 	p_pset.cdk 	ptopo.cdk 	v4dg.cdk 
p_phystep.o: p_phystep.ftn 	busind.cdk 	dimout.cdk 	glb_ld.cdk \
	lun.cdk 	macro.cdk 	mem.cdk 	p4d_simp.cdk \
	p_bus.cdk 	p_pbl.cdk 	ptopo.cdk 	schm.cdk \
	v4dg.cdk 
p_psetiw.o: p_psetiw.ftn 	glb_ld.cdk 	impnone.cdk 	lctl.cdk \
	lun.cdk 	p_pset.cdk 
p_radiaf.o: p_radiaf.ftn 	impnone.cdk 	ptopo.cdk 
p_set.o: p_set.ftn 	dcst.cdk 	glb_ld.cdk 	grd.cdk \
	lun.cdk 	mem.cdk 	mult.cdk 	out1.cdk \
	p4d_simp.cdk 	p_clim.cdk 	p_cond.cdk 	p_cond2.cdk \
	p_gwd.cdk 	p_nml.cdk 	p_nml2.cdk 	p_out.cdk \
	p_pbl.cdk 	p_pbl2.cdk 	p_pset.cdk 	p_rad.cdk \
	p_serg.cdk 	p_str.cdk 	p_zong.cdk 	path.cdk \
	ptopo.cdk 	schm.cdk 	xst.cdk 
p_uvgridscal.o: p_uvgridscal.ftn 	glb_ld.cdk 	impnone.cdk 	intuv.cdk \
	inuvl.cdk 	schm.cdk 
p_vlsp.o: p_vlsp.ftn 	geomg.cdk 	glb_ld.cdk 	grd.cdk \
	impnone.cdk 	lun.cdk 	p_geol.cdk 	p_pbl.cdk 
p_vmmphy.o: p_vmmphy.ftn 	busind.cdk 	cstv.cdk 	dcst.cdk \
	geomg.cdk 	glb_ld.cdk 	inuvl.cdk 	p_bus.cdk \
	p_cond.cdk 	p_geof.cdk 	schm.cdk 	vt0.cdk \
	vt1.cdk 
pdfcdate.o: pdfcdate.ftn	 
pdfjdate.o: pdfjdate.ftn	 
pdfjdate2.o: pdfjdate2.ftn	 
phy_ini.o: phy_ini.ftn 	classlvls.cdk 	dimsurf.cdk 	icelvls.cdk \
	impnone.cdk 	indx_sfc.cdk 	maxlev.cdk 	mosaic.cdk \
	nbvarsurf.cdk 	nclassvg.cdk 	options.cdk 	phy_macros_f.h \
	phybus.cdk 
phybus_data.o: phybus_data.ftn 	phy_macros_f.h 	phybus.cdk 
phycom.o: phycom.ftn 	dzcond.cdk 	hscap.cdk 	impnone.cdk \
	leads.cdk 	mountains.cdk 
phydebu4.o: phydebu4.ftn 	acmcon.cdk 	buses.cdk 	cldopt_mode.cdk \
	clefcon.cdk 	comphy.cdk 	consphy.cdk 	dimsurf.cdk \
	dzsedi.cdk 	impnone.cdk 	indx_sfc.cdk 	isbapar.cdk \
	machcon.cdk 	maxlev.cdk 	mountains.cdk 	nbvarsurf.cdk \
	options.cdk 	scfrst.cdk 	sedipara.cdk 	small_sedimentation_dt.cdk \
	surfacepar.cdk 	surfcon.cdk 	surfcon_ini.cdk 	tables.cdk \
	workspc.cdk 
phyexe1.o: phyexe1.ftn 	consphy.cdk 	dimsurf.cdk 	impnone.cdk \
	indx_sfc.cdk 	maxlev.cdk 	nbvarsurf.cdk 	options.cdk \
	phy_macros_f.h 	phybus.cdk 	workspc.cdk 
phyopt.o: phyopt.ftn 	dzsedi.cdk 	indx_sfc.cdk 	maxlev.cdk \
	mosaic.cdk 	options.cdk 	phy_macros_f.h 
pllwfgfw.o: pllwfgfw.ftn	 
pnm2.o: pnm2.ftn 	dcst.cdk 	glb_ld.cdk 	impnone.cdk 
pntg123.o: pntg123.ftn 	impnone.cdk 	radparam.cdk 	radpnt.cdk 
pntozon.o: pntozon.ftn 	impnone.cdk 	ozopnt.cdk 
pospers.o: pospers.ftn 	geomg.cdk 	glb_ld.cdk 	impnone.cdk \
	vth.cdk 
possol.o: possol.ftn90 	modd_csts.o 
pre.o: pre.ftn 	geomg.cdk 	glb_ld.cdk 	impnone.cdk \
	lctl.cdk 	lun.cdk 	p_geof.cdk 	rhsc.cdk \
	schm.cdk 	vt1.cdk 
predat.o: predat.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	hzd.cdk 	ind.cdk 	lctl.cdk \
	lun.cdk 	pres.cdk 	schm.cdk 	v4dg.cdk \
	vt1.cdk 	vtopo.cdk 
prelim1.o: prelim1.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 
prep_2.o: prep_2.ftn 	adw.cdk 	cori.cdk 	cstv.cdk \
	dcst.cdk 	geomg.cdk 	glb_ld.cdk 	grd.cdk \
	intuv.cdk 	inuvl.cdk 	lun.cdk 	offc.cdk \
	schm.cdk 
prep_cw.o: prep_cw.ftn 	impnone.cdk 	maxlev.cdk 	options.cdk \
	phy_macros_f.h 	phybus.cdk 
prep_cw_rad.o: prep_cw_rad.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	nocld.cdk 	options.cdk 	phy_macros_f.h \
	phybus.cdk 
prep_hor_snow_field.o: prep_hor_snow_field.ftn90 	modd_data_cover_par.o  \
	modd_isba.o  	modd_prep.o  \
	modd_prep_snow.o  	modd_snow_par.o  \
	modd_surf_par.o  	modd_type_date.o  \
	modd_type_snow.o  	modi_hor_interpol.o  \
	modi_interp_grid2d.o  	modi_prep_snow_gem.o  \
	modi_snow_heat_to_t_wliq.o  	modi_vegtype_grid_to_patch_grid.o 
prep_hor_snow_fields.o: prep_hor_snow_fields.ftn90 	modd_csts.o  \
	modd_prep.o  	modd_snow_par.o  \
	modd_surf_par.o  	modd_type_snow.o  \
	mode_snow3l.o  	modi_allocate_gr_snow.o  \
	modi_prep_hor_snow_field.o 
prep_hor_teb_field.o: prep_hor_teb_field.ftn90 	modd_prep.o  \
	modd_prep_teb.o  	modd_teb.o  \
	modd_teb_grid.o  	modd_type_snow.o  \
	modi_get_luout.o  	modi_hor_interpol.o  \
	modi_interp_grid2d.o  	modi_prep_hor_snow_fields.o  \
	modi_prep_teb_gem.o 
prep_index.o: prep_index.ftn	 
prep_snow_gem.o: prep_snow_gem.ftn90 	modd_prep.o  \
	modd_prep_snow.o  	modd_snow_par.o  \
	modd_town.o 
prep_teb.o: prep_teb.ftn90 	modd_teb.o  \
	modd_type_snow.o  	modi_default_prep_teb.o  \
	modi_prep_hor_teb_field.o 
prep_teb_gem.o: prep_teb_gem.ftn90 	modd_csts.o  \
	modd_prep.o  	modd_prep_teb.o  \
	modd_surf_par.o  	modd_town.o  \
	modi_interp_grid1d.o 
prepoic.o: prepoic.ftn 	impnone.cdk 
prever.o: prever.ftn 	cstv.cdk 	dcst.cdk 	impnone.cdk \
	schm.cdk 
prgen.o: prgen.ftn 	glb_ld.cdk 	impnone.cdk 
prgzvta.o: prgzvta.ftn 	dcst.cdk 	glb_ld.cdk 	impnone.cdk 
prism_def_var_proto.o: prism_def_var_proto.ftn	 
prism_enddef_proto.o: prism_enddef_proto.ftn	 
prism_terminate_proto.o: prism_terminate_proto.ftn	 
prodef.o: prodef.ftn 	impnone.cdk 
prof5.o: prof5.ftn 	impnone.cdk 
prsdate.o: prsdate.ftn	 
qcfft8.o: qcfft8.ftn	 
qcsgs.o: qcsgs.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
qozon3.o: qozon3.ftn 	consphy.cdk 	impnone.cdk 
raddel.o: raddel.ftn 	impnone.cdk 
radfac3.o: radfac3.ftn 	impnone.cdk 
radiaf_o.o: radiaf_o.ftn 	impnone.cdk 
radir8.o: radir8.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 
radix.o: radix.ftn 	clefcon.cdk 	consphy.cdk 	impnone.cdk \
	phy_macros_f.h 
rasol.o: rasol.ftn 	clefcon.cdk 	consphy.cdk 	impnone.cdk \
	phy_macros_f.h 	valcon.cdk 
raysol.o: raysol.ftn 	clefcon.cdk 	consphy.cdk 	impnone.cdk 
rdmax.o: rdmax.ftn 	impnone.cdk 
rdmoy.o: rdmoy.ftn 	impnone.cdk 
rdradf.o: rdradf.ftn 	impnone.cdk 	radparam.cdk 	radpnt.cdk 
rdrstrt.o: rdrstrt.ftn 	c_cplg.cdk 	glb_ld.cdk 	impnone.cdk \
	init.cdk 	lctl.cdk 	lun.cdk 	mem.cdk \
	p_bus.cdk 	pilot.cdk 	pres.cdk 	rstr.cdk \
	schm.cdk 
readdyn.o: readdyn.ftn 	acq.cdk 	acq_int.cdk 	bmf.cdk \
	cstv.cdk 	dcst.cdk 	geomg.cdk 	glb_ld.cdk \
	grd.cdk 	ind.cdk 	lctl.cdk 	lun.cdk \
	out1.cdk 	pres.cdk 	ptopo.cdk 	schm.cdk \
	vtopo.cdk 
readgeo.o: readgeo.ftn 	c_cplg.cdk 	glb_ld.cdk 	impnone.cdk \
	lun.cdk 	p_bus.cdk 	p_geobus.cdk 	p_geobusid.cdk \
	ptopo.cdk 
readlam.o: readlam.ftn 	glb_ld.cdk 	impnone.cdk 	ptopo.cdk 
rhs.o: rhs.ftn 	glb_ld.cdk 	impnone.cdk 	lun.cdk \
	nest.cdk 	orh.cdk 	p_geof.cdk 	rhsc.cdk \
	schm.cdk 	vt1.cdk 
rhsp_2.o: rhsp_2.ftn 	cori.cdk 	cstv.cdk 	dcst.cdk \
	geomg.cdk 	glb_ld.cdk 	intuv.cdk 	inuvl.cdk \
	offc.cdk 	schm.cdk 
riflux.o: riflux.ftn 	clefcon.cdk 	impnone.cdk 
rigrad1.o: rigrad1.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 
road_wall_layer_e_budget.o: road_wall_layer_e_budget.ftn90 	modd_csts.o  \
	mode_thermos.o  	modi_tridiag_ground.o  \
	modi_urban_lw_coef.o 
roof_layer_e_budget.o: roof_layer_e_budget.ftn90 	modd_csts.o  \
	mode_thermos.o  	modi_tridiag_ground.o 
satq.o: satq.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 
save_options.o: save_options.ftn 	cldopt_mode.cdk 	consphy.cdk 	dzsedi.cdk \
	impnone.cdk 	maxlev.cdk 	options.cdk 	sedipara.cdk \
	small_sedimentation_dt.cdk 
scatter.o: scatter.ftn 	impnone.cdk 
schal.o: schal.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
seaice1.o: seaice1.ftn 	consphy.cdk 	dimsurf.cdk 	dintern.cdk \
	fintern.cdk 	fintern90.cdk 	himin.cdk 	icelvls.cdk \
	impnone.cdk 	indx_sfc.cdk 	locbus.cdk 	maxlev.cdk \
	nbvarsurf.cdk 	options.cdk 	phy_macros_f.h 	sfcbus.cdk \
	snoh0.cdk 	vamin.cdk 	xptsurf.cdk 	zuzt.cdk 
secajus.o: secajus.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
seralc2.o: seralc2.ftn 	impnone.cdk 	sercmdk.cdk 
serallc2.o: serallc2.ftn 	impnone.cdk 	sercmdk.cdk 
serdata.o: serdata.ftn 	sercmdk.cdk 
serdbu.o: serdbu.ftn 	impnone.cdk 	sercmdk.cdk 
serdim.o: serdim.ftn 	impnone.cdk 	sercmdk.cdk 
serecri.o: serecri.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 
serfin.o: serfin.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
sergdim.o: sergdim.ftn 	impnone.cdk 
sergini.o: sergini.ftn 	busind.cdk 	dcst.cdk 	glb_ld.cdk \
	impnone.cdk 	lun.cdk 	mem.cdk 	p_bus.cdk \
	p_serg.cdk 	ptopo.cdk 	rstr.cdk 	xst.cdk 
sergoff.o: sergoff.ftn 	impnone.cdk 	init.cdk 	lctl.cdk \
	lun.cdk 	p_serg.cdk 	rstr.cdk 	xst.cdk 
sergout.o: sergout.ftn 	dcst.cdk 	dimout.cdk 	geomg.cdk \
	glb_ld.cdk 	hgc.cdk 	impnone.cdk 	lctl.cdk \
	out1.cdk 	p_cond.cdk 	p_serg.cdk 	ptopo.cdk \
	xst.cdk 	xstb.cdk 
sergset.o: sergset.ftn 	glb_ld.cdk 	impnone.cdk 	lctl.cdk \
	lun.cdk 	p_bus.cdk 	p_geol.cdk 	p_serg.cdk \
	ptopo.cdk 	rstr.cdk 	xst.cdk 	xstb.cdk 
serie.o: serie.ftn 	impnone.cdk 	maxlev.cdk 	phy_macros_f.h \
	sercmdk.cdk 
serkh.o: serkh.ftn 	impnone.cdk 	sercmdk.cdk 
serpat2.o: serpat2.ftn 	impnone.cdk 	sercmdk.cdk 
serset.o: serset.ftn 	impnone.cdk 	sercmdk.cdk 
sersetc.o: sersetc.ftn 	impnone.cdk 	sercmdk.cdk 
sersetm.o: sersetm.ftn 	impnone.cdk 	sercmdk.cdk 
serwrit.o: serwrit.ftn	 
serwrit2.o: serwrit2.ftn 	impnone.cdk 	sercmdk.cdk 
serxst.o: serxst.ftn 	impnone.cdk 	sercmdk.cdk 
sesahr.o: sesahr.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sesahu.o: sesahu.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr1.cdk 
set_class_const.o: set_class_const.ftn 	consphy.cdk 	impnone.cdk 	surfcon.cdk 
set_class_dt.o: set_class_dt.ftn 	class_com.cdk 
set_cn1.o: set_cn1.ftn 	dimout.cdk 	glb_ld.cdk 	init.cdk \
	lun.cdk 	rstr.cdk 	schm.cdk 
set_cori.o: set_cori.ftn 	cori.cdk 	geomg.cdk 	glb_ld.cdk \
	grd.cdk 	impnone.cdk 	lun.cdk 
set_crni.o: set_crni.ftn 	glb_ld.cdk 	impnone.cdk 	lun.cdk \
	orh.cdk 	schm.cdk 
set_dcst.o: set_dcst.ftn	 
set_dcst_8.o: set_dcst_8.ftn	 
set_dia.o: set_dia.ftn 	glb_ld.cdk 	impnone.cdk 	lun.cdk \
	p_serg.cdk 	p_zong.cdk 
set_dync.o: set_dync.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	grd.cdk 	lun.cdk 	pres.cdk \
	schm.cdk 
set_fft.o: set_fft.ftn 	dcst.cdk 	fft.cdk 	glb_ld.cdk \
	glb_pil.cdk 	grd.cdk 	impnone.cdk 	lun.cdk 
set_filt.o: set_filt.ftn 	dimout.cdk 	glb_ld.cdk 	impnone.cdk \
	lun.cdk 	out3.cdk 
set_geof.o: set_geof.ftn 	glb_ld.cdk 	lun.cdk 	p_geof.cdk \
	vtopo.cdk 
set_geom.o: set_geom.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	geomn.cdk 	glb_ld.cdk 	glb_pil.cdk 	grd.cdk \
	hgc.cdk 	lun.cdk 	p_geof.cdk 	ptopo.cdk \
	schm.cdk 	xst.cdk 
set_grid.o: set_grid.ftn 	dimout.cdk 	glb_ld.cdk 	glb_pil.cdk \
	grd.cdk 	grid.cdk 	impnone.cdk 	lun.cdk \
	p_geol.cdk 
set_intuv.o: set_intuv.ftn 	dcst.cdk 	geomg.cdk 	glb_ld.cdk \
	impnone.cdk 	intuv.cdk 	inuvl.cdk 	lun.cdk \
	ptopo.cdk 
set_level.o: set_level.ftn 	dimout.cdk 	geomg.cdk 	glb_ld.cdk \
	impnone.cdk 	level.cdk 	lun.cdk 
set_list.o: set_list.ftn 	dimout.cdk 	glb_ld.cdk 	impnone.cdk \
	init.cdk 	lun.cdk 	rhsc.cdk 	rstr.cdk \
	setsor.cdk 	tr3d.cdk 	vt0.cdk 	vt1.cdk \
	vt2.cdk 	vta.cdk 	vth.cdk 	vtx.cdk 
set_mem.o: set_mem.ftn 	glb_ld.cdk 	impnone.cdk 	lun.cdk \
	mem.cdk 	schm.cdk 
set_nest.o: set_nest.ftn 	glb_ld.cdk 	impnone.cdk 	lun.cdk \
	nest.cdk 	pilot.cdk 	schm.cdk 	tr3d.cdk 
set_opr.o: set_opr.ftn 	cstv.cdk 	dcst.cdk 	eigv.cdk \
	fft.cdk 	geomg.cdk 	glb_ld.cdk 	glb_pil.cdk \
	impnone.cdk 	lun.cdk 	opr.cdk 	schm.cdk \
	sol.cdk 	trp.cdk 
set_oprz.o: set_oprz.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	impnone.cdk 	opr.cdk 	schm.cdk \
	sol.cdk 	trp.cdk 
set_ops8.o: set_ops8.ftn 	impnone.cdk 
set_poic.o: set_poic.ftn 	dcst.cdk 	glb_ld.cdk 	glb_pil.cdk \
	impnone.cdk 	lun.cdk 
set_poic_par.o: set_poic_par.ftn 	dcst.cdk 	impnone.cdk 
set_poic_par_u.o: set_poic_par_u.ftn 	dcst.cdk 	impnone.cdk 
set_pois.o: set_pois.ftn 	impnone.cdk 
set_rhs.o: set_rhs.ftn 	glb_ld.cdk 	impnone.cdk 	lun.cdk \
	rhsc.cdk 
set_schm.o: set_schm.ftn 	cstv.cdk 	dcst.cdk 	glb_ld.cdk \
	lun.cdk 	offc.cdk 	schm.cdk 
set_sor.o: set_sor.ftn 	cstv.cdk 	dimout.cdk 	geomg.cdk \
	glb_ld.cdk 	grid.cdk 	hgc.cdk 	impnone.cdk \
	lctl.cdk 	level.cdk 	lun.cdk 	orh.cdk \
	out1.cdk 	out3.cdk 	outd.cdk 	outp.cdk \
	p_bus.cdk 	p_geof.cdk 	pilot.cdk 	ptopo.cdk \
	rhsc.cdk 	step.cdk 	timestep.cdk 	vt0.cdk \
	vt1.cdk 	vt2.cdk 	vta.cdk 	vth.cdk \
	vtx.cdk 
set_step.o: set_step.ftn 	cstv.cdk 	dimout.cdk 	glb_ld.cdk \
	impnone.cdk 	lun.cdk 	step.cdk 	timestep.cdk 
set_term.o: set_term.ftn 	impnone.cdk 	lun.cdk 	p_serg.cdk \
	ptopo.cdk 	schm.cdk 
set_transpose.o: set_transpose.ftn 	glb_ld.cdk 	impnone.cdk 	ldnh.cdk \
	lun.cdk 	ptopo.cdk 	schm.cdk 	trp.cdk \
	vspng.cdk 
set_trig21.o: set_trig21.ftn	 
set_var.o: set_var.ftn 	dimout.cdk 	glb_ld.cdk 	grid.cdk \
	impnone.cdk 	level.cdk 	lun.cdk 	out3.cdk \
	outd.cdk 	outp.cdk 	p_bus.cdk 	setsor.cdk \
	timestep.cdk 
set_vt.o: set_vt.ftn 	glb_ld.cdk 	impnone.cdk 	lun.cdk \
	schm.cdk 	tr3d.cdk 	vt0.cdk 	vt1.cdk \
	vt2.cdk 	vth.cdk 	vtx.cdk 
set_vta.o: set_vta.ftn 	glb_ld.cdk 	impnone.cdk 	lun.cdk \
	schm.cdk 	tr3d.cdk 	vta.cdk 
set_world_view.o: set_world_view.ftn 	cst_lis.cdk 	cstv.cdk 	dcst.cdk \
	dimout.cdk 	glb_ld.cdk 	glb_pil.cdk 	grd.cdk \
	grid.cdk 	hblen.cdk 	hgc.cdk 	level.cdk \
	lun.cdk 	path.cdk 	pilot.cdk 	pres.cdk \
	ptopo.cdk 
set_world_view1.o: set_world_view1.f 	v4dz.o 
set_world_view1.o: set_world_view1.ftn 	adw.cdk 	c_cplg.cdk 	cori.cdk \
	cstv.cdk 	dimout.cdk 	eigv.cdk 	geomg.cdk \
	glb_ld.cdk 	glb_pil.cdk 	grd.cdk 	grid.cdk \
	hspng.cdk 	hybdim.cdk 	hzd.cdk 	init.cdk \
	lctl.cdk 	level.cdk 	lun.cdk 	mem.cdk \
	nml.cdk 	offc.cdk 	offline.cdk 	out1.cdk \
	out3.cdk 	path.cdk 	pilot.cdk 	pres.cdk \
	ptopo.cdk 	schm.cdk 	setsor.cdk 	step.cdk \
	theo.cdk 	tr3d.cdk 	v4dg.cdk 	v4dj.cdk \
	v4dm.cdk 	v4dr.cdk  	v4dz.o \
	version.cdk 	vrtd.cdk 	vspng.cdk 	vtopo.cdk \
	xst.cdk 	zblen.cdk 
set_xnbit.o: set_xnbit.ftn 	dimout.cdk 	glb_ld.cdk 	impnone.cdk \
	lun.cdk 	out3.cdk 
set_xst.o: set_xst.ftn 	dcst.cdk 	geomn.cdk 	glb_ld.cdk \
	grd.cdk 	lun.cdk 	p_geol.cdk 	xst.cdk 
setdsr2.o: setdsr2.ftn 	impnone.cdk 
setscqr.o: setscqr.ftn	 
setsd.o: setsd.ftn 	impnone.cdk 
settriv.o: settriv.ftn 	impnone.cdk 
setvis2.o: setvis2.ftn 	consphy.cdk 	impnone.cdk 
sfcbus_data.o: sfcbus_data.ftn 	phy_macros_f.h 	sfcbus.cdk 
sfltr.o: sfltr.ftn 	impnone.cdk 
sfodla.o: sfodla.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfodle.o: sfodle.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfodqa.o: sfodqa.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfodqs.o: sfodqs.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfoefq.o: sfoefq.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfoew.o: sfoew.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfoewa.o: sfoewa.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfohr.o: sfohr.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfohra.o: sfohra.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfols.o: sfols.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfolv.o: sfolv.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfopop.o: sfopop.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfopot.o: sfopot.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfoqfe.o: sfoqfe.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfoqsa.o: sfoqsa.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfoqst.o: sfoqst.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfottv.o: sfottv.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sfotvt.o: sfotvt.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sgamash.o: sgamash.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sgamasp.o: sgamasp.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sgamatd.o: sgamatd.ftn 	consphy.cdk 	impnone.cdk 	initcph.cdk 
sgoflx2.o: sgoflx2.ftn 	impnone.cdk 	phy_macros_f.h 
sgoflx3.o: sgoflx3.ftn 	impnone.cdk 	phy_macros_f.h 
shallconv3.o: shallconv3.ftn 	impnone.cdk 	indx_sfc.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 	phybus.cdk 
shalow.o: shalow.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 
shraes.o: shraes.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
shrahu.o: shrahu.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr1.cdk 
shuaes.o: shuaes.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr1.cdk \
	modtp1.cdk 
shuahr.o: shuahr.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr1.cdk \
	modtp1.cdk 
shuffle.o: shuffle.ftn 	impnone.cdk 	lun.cdk 	setsor.cdk 
skocon.o: skocon.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	fmroft.cdk 	impnone.cdk 	phy_macros_f.h 
slabsym.o: slabsym.ftn 	glb_ld.cdk 	lun.cdk 	schm.cdk \
	tr3d.cdk 	vt0.cdk 
slfun_tq.o: slfun_tq.ftn 	phy_macros_f.h 	stabfunc.cdk 	surfcon_ini.cdk 
slfun_uv.o: slfun_uv.ftn 	phy_macros_f.h 	stabfunc.cdk 	surfcon_ini.cdk 
sltop_tq.o: sltop_tq.ftn 	deriv_stabfunc.cdk 	phy_macros_f.h 	surfcon_ini.cdk 
sltop_uv.o: sltop_uv.ftn 	deriv_stabfunc.cdk 	phy_macros_f.h 	surfcon_ini.cdk 
snow_cover_1layer.o: snow_cover_1layer.ftn90 	modd_csts.o  \
	modd_snow_par.o  	modd_surf_par.o  \
	mode_thermos.o  	modi_surface_aero_cond.o  \
	modi_surface_ri.o 
snow_heat_to_t_wliq.o: snow_heat_to_t_wliq.ftn90 	modd_csts.o  \
	modd_snow_par.o  	modd_surf_par.o  \
	mode_snow3l.o 
soili.o: soili.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
sol_abc.o: sol_abc.ftn 	glb_ld.cdk 	glb_pil.cdk 	impnone.cdk 
sol_fft8_2.o: sol_fft8_2.ftn 	impnone.cdk 	ptopo.cdk 
sol_fft8_lam.o: sol_fft8_lam.ftn 	glb_ld.cdk 	glb_pil.cdk 	impnone.cdk \
	ptopo.cdk 
sol_hcr.o: sol_hcr.ftn 	cstv.cdk 	eigv.cdk 	fft.cdk \
	glb_ld.cdk 	glb_pil.cdk 	impnone.cdk 	lctl.cdk \
	ldnh.cdk 	lun.cdk 	opr.cdk 	ptopo.cdk \
	schm.cdk 	sol.cdk 	trp.cdk 
sol_main.o: sol_main.ftn 	fft.cdk 	glb_ld.cdk 	impnone.cdk \
	lctl.cdk 	ldnh.cdk 	lun.cdk 	opr.cdk \
	ptopo.cdk 	rhsc.cdk 	trp.cdk 	vtx.cdk 
sol_mxma8_2.o: sol_mxma8_2.ftn 	glb_ld.cdk 	glb_pil.cdk 	ptopo.cdk 
sol_parite_2.o: sol_parite_2.ftn 	impnone.cdk 	ptopo.cdk 
sol_trsf.o: sol_trsf.ftn 	glb_ld.cdk 	impnone.cdk 
splitst.o: splitst.ftn 	impnone.cdk 
srequet.o: srequet.ftn 	dimout.cdk 	grid.cdk 	impnone.cdk \
	level.cdk 	lun.cdk 	out3.cdk 	outd.cdk \
	outp.cdk 	timestep.cdk 
statf_dm.o: statf_dm.ftn 	glb_ld.cdk 	lun.cdk 	ptopo.cdk 
statfld.o: statfld.ftn 	glb_ld.cdk 	impnone.cdk 	lctl.cdk \
	lun.cdk 
statfld_old.o: statfld_old.ftn 	impnone.cdk 	lun.cdk 
status_file.o: status_file.ftn	 
statvps1.o: statvps1.ftn 	buses.cdk 	impnone.cdk 
stetae.o: stetae.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
stfrst4.o: stfrst4.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	hscap.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	scfrst.cdk 
sthtaw.o: sthtaw.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	flatscp.cdk 	impnone.cdk 	initcph.cdk \
	modpr1.cdk 
sthtaw2.o: sthtaw2.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 	modpr1.cdk 
stop_world_view.o: stop_world_view.ftn 	lctl.cdk 	lun.cdk 	path.cdk \
	ptopo.cdk 	version.cdk 
stopmpi.o: stopmpi.ftn	 
stretch_axis2.o: stretch_axis2.ftn	 
sttlcl.o: sttlcl.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	initcph.cdk 
sun7.o: sun7.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
sun_radfix1.o: sun_radfix1.ftn 	impnone.cdk 
suncos.o: suncos.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
sunpos.o: sunpos.ftn90 	modd_csts.o 
surf_precip.o: surf_precip.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 	phy_macros_f.h 
surface.o: surface.ftn 	consphy.cdk 	dimsurf.cdk 	impnone.cdk \
	indx_sfc.cdk 	maxlev.cdk 	nbvarsurf.cdk 	options.cdk \
	phy_macros_f.h 	phybus.cdk 	stk.cdk 	surfacepar.cdk \
	zuzt.cdk 
surface_aero_cond.o: surface_aero_cond.ftn90 	modd_csts.o  \
	mode_thermos.o 
surface_cd.o: surface_cd.ftn90 	modd_csts.o  \
	mode_thermos.o 
surface_ri.o: surface_ri.ftn90 	modd_csts.o 
surfix.o: surfix.ftn 	glb_ld.cdk 	impnone.cdk 	vt0.cdk \
	vt1.cdk 
surfradcloud.o: surfradcloud.ftn 	consphy.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	options.cdk 	phy_macros_f.h 	phybus.cdk \
	radclcons.cdk 	solcons.cdk 
t02t1.o: t02t1.ftn 	glb_ld.cdk 	impnone.cdk 	schm.cdk \
	tr3d.cdk 	vt0.cdk 	vt1.cdk 
t12t2.o: t12t2.ftn 	glb_ld.cdk 	impnone.cdk 	schm.cdk \
	tr3d.cdk 	vt1.cdk 	vt2.cdk 
t22t0.o: t22t0.ftn 	glb_ld.cdk 	impnone.cdk 	schm.cdk \
	tr3d.cdk 	vt0.cdk 	vt2.cdk 
ta2t1tx.o: ta2t1tx.ftn 	glb_ld.cdk 	impnone.cdk 	lun.cdk \
	mem.cdk 	nest.cdk 	p_bus.cdk 	schm.cdk \
	tr3d.cdk 	vt1.cdk 	vta.cdk 	vtx.cdk 
teb.o: teb.ftn90 	modd_csts.o  \
	modd_snow_par.o  	mode_surf_snow_frac.o  \
	mode_thermos.o  	modi_bld_e_budget.o  \
	modi_road_wall_layer_e_budget.o  	modi_roof_layer_e_budget.o  \
	modi_urban_drag.o  	modi_urban_fluxes.o  \
	modi_urban_hydro.o  	modi_urban_snow_evol.o  \
	modi_urban_solar_abs.o 
theo_cfg.o: theo_cfg.ftn 	adw.cdk 	bubble.cdk 	cstv.cdk \
	eigv.cdk 	geomg.cdk 	glb_ld.cdk 	glb_pil.cdk \
	grd.cdk 	hblen.cdk 	hgc.cdk 	hybdim.cdk \
	hzd.cdk 	init.cdk 	lctl.cdk 	lun.cdk \
	mtn.cdk 	offc.cdk 	out1.cdk 	p_geol.cdk \
	pilot.cdk 	pres.cdk 	ptopo.cdk 	schm.cdk \
	step.cdk 	theo.cdk 	theonml.cdk 	tr3d.cdk \
	vspng.cdk 	vtopo.cdk 	zblen.cdk 
thermco2.o: thermco2.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
tkealg.o: tkealg.ftn 	impnone.cdk 	machcon.cdk 	phy_macros_f.h 
tmg_init0.o: tmg_init0.ftn 	time0.cdk 
tmg_start0.o: tmg_start0.ftn 	time0.cdk 
tmg_stop0.o: tmg_stop0.ftn 	time0.cdk 
tmg_terminate0.o: tmg_terminate0.ftn 	time0.cdk 
town.o: town.f90 	modd_csts.o  	modd_surf_par.o \
 	modd_teb.o  	modd_teb_grid.o \
 	modd_town.o  	modd_type_snow.o \
 	modi_allocate_gr_snow.o  	modi_coupling_teb_orography.o \
 	modi_ini_csts.o  	modi_possol.o \
 	modi_sunpos.o 
town.o: town.ftn90 	dimsurf.cdk 	impnone.cdk 	indx_sfc.cdk \
	locbus.cdk 	maxlev.cdk  	modd_csts.o \
 	modd_surf_par.o  	modd_teb.o \
 	modd_teb_grid.o  	modd_town.o \
 	modd_type_snow.o  	modi_allocate_gr_snow.o \
 	modi_coupling_teb_orography.o  	modi_ini_csts.o \
 	modi_possol.o  	modi_sunpos.o \
	nbvarsurf.cdk 	options.cdk 	phy_macros_f.h 	sfcbus.cdk \
	tebcst.cdk 	xptsurf.cdk 
tp.o: tp.ftn 	impnone.cdk 
tpdd.o: tpdd.ftn 	impnone.cdk 
tpmix.o: tpmix.ftn 	impnone.cdk 
tr_init.o: tr_init.ftn 	glb_ld.cdk 	nest.cdk 	tr3d.cdk \
	vt1.cdk 
tr_vint.o: tr_vint.ftn 	acq.cdk 	geomg.cdk 	glb_ld.cdk \
	schm.cdk 
tracers.o: tracers.ftn 	busind.cdk 	lun.cdk 	tr3d.cdk 
trans_clean.o: trans_clean.ftn 	lctl.cdk 	path.cdk 	ptopo.cdk \
	rstr.cdk 	step.cdk 
trapez2.o: trapez2.ftn 	impnone.cdk 
tridiag_ground.o: tridiag_ground.ftn90	 
tstpdyn.o: tstpdyn.ftn 	cstv.cdk 	glb_ld.cdk 	lun.cdk \
	nl.cdk 	orh.cdk 	schm.cdk 	sol.cdk \
	v4dg.cdk 	v4dr.cdk 
tttt.o: tttt.ftn 	impnone.cdk 
turbul.o: turbul.ftn 	clefcon.cdk 	consphy.cdk 	dintern.cdk \
	fintern.cdk 	fintern90.cdk 	impnone.cdk 	indx_sfc.cdk \
	maxlev.cdk 	options.cdk 	phy_macros_f.h 	phybus.cdk \
	stk.cdk 	surfcon.cdk 
update.o: update.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
update3.o: update3.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	options.cdk 
urban_drag.o: urban_drag.ftn90 	modd_csts.o  \
	modd_town.o  	mode_thermos.o  \
	modi_surface_aero_cond.o  	modi_surface_cd.o  \
	modi_surface_ri.o 
urban_fluxes.o: urban_fluxes.ftn90 	modd_csts.o  \
	modi_urban_lw_coef.o 
urban_hydro.o: urban_hydro.ftn90 	modd_csts.o 
urban_lw_coef.o: urban_lw_coef.ftn90 	modd_csts.o 
urban_snow_evol.o: urban_snow_evol.ftn90 	modd_csts.o  \
	modd_snow_par.o  	mode_surf_snow_frac.o  \
	modi_snow_cover_1layer.o  	modi_urban_lw_coef.o 
urban_solar_abs.o: urban_solar_abs.ftn90 	modd_csts.o 
utco2.o: utco2.ftn 	impnone.cdk 
uv2tdpsd.o: uv2tdpsd.ftn 	geomg.cdk 	glb_ld.cdk 	intuv.cdk 
uv_acg2g.o: uv_acg2g.ftn 	glb_ld.cdk 	impnone.cdk 	intuv.cdk \
	inuvl.cdk 	schm.cdk 
v4d_prof.o: v4d_prof.cdk90    
v4dz.o: v4dz.cdk90    
var_topo.o: var_topo.ftn 	dcst.cdk 	glb_ld.cdk 	lctl.cdk \
	p_geof.cdk 	vt1.cdk 	vtopo.cdk 
vco2inf2.o: vco2inf2.ftn 	impnone.cdk 	phy_macros_f.h 
vdfds.o: vdfds.ftn 	impnone.cdk 
vegi.o: vegi.ftn 	consphy.cdk 	dintern.cdk 	fintern.cdk \
	fintern90.cdk 	impnone.cdk 	phy_macros_f.h 
vegtype_grid_to_patch_grid.o: vegtype_grid_to_patch_grid.ftn90 	modd_data_cover_par.o  \
	modd_isba.o  	modd_surf_par.o  \
	modd_type_date.o  	modd_type_snow.o  \
	modi_vegtype_to_patch.o 
vegtype_to_patch.o: vegtype_to_patch.ftn90 	modd_data_cover_par.o 
ver_interp_lin2d.o: ver_interp_lin2d.ftn90	 
verder.o: verder.ftn 	glb_ld.cdk 	impnone.cdk 
vertdiff.o: vertdiff.ftn 	impnone.cdk 	phy_macros_f.h 
vgathr.o: vgathr.ftn 	impnone.cdk 
vkuocon6.o: vkuocon6.ftn 	consphy.cdk 	impnone.cdk 	maxlev.cdk \
	mountains.cdk 	nocld.cdk 	options.cdk 	phy_macros_f.h \
	phybus.cdk 
vrtd.o: vrtd.ftn 	cstv.cdk 	dcst.cdk 	glb_ld.cdk \
	impnone.cdk 	intuv.cdk 	schm.cdk 	vrtd.cdk \
	vt1.cdk 
vspng_abc.o: vspng_abc.ftn 	glb_ld.cdk 	ptopo.cdk 	trp.cdk \
	vspng.cdk 
vspng_del2.o: vspng_del2.ftn 	glb_ld.cdk 	impnone.cdk 	ldnh.cdk \
	ptopo.cdk 	trp.cdk 	vspng.cdk 
vspng_drv.o: vspng_drv.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	hzd.cdk 	impnone.cdk 	opr.cdk \
	schm.cdk 	trp.cdk 	vspng.cdk 
vspng_drv_lam.o: vspng_drv_lam.ftn 	cstv.cdk 	geomg.cdk 	glb_ld.cdk \
	impnone.cdk 	schm.cdk 	vspng.cdk 
vspng_lam.o: vspng_lam.ftn 	glb_ld.cdk 	glb_pil.cdk 	impnone.cdk \
	vspng.cdk 
vspng_main.o: vspng_main.ftn 	cstv.cdk 	geomg.cdk 	glb_ld.cdk \
	impnone.cdk 	lun.cdk 	p_geof.cdk 	schm.cdk \
	vspng.cdk 	vt0.cdk 	vt1.cdk 	vtx.cdk 
vspng_set.o: vspng_set.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	impnone.cdk 	lun.cdk 	vspng.cdk 
vtap.o: vtap.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	glb_ld.cdk 	impnone.cdk 	ind.cdk 	lun.cdk \
	schm.cdk 
vte_hyb2hyb.o: vte_hyb2hyb.ftn 	dcst.cdk 	impnone.cdk 
vte_intvertx.o: vte_intvertx.ftn 	dcst.cdk 	impnone.cdk 
vte_uv2img.o: vte_uv2img.ftn 	dcst.cdk 	impnone.cdk 
vte_vrtical.o: vte_vrtical.ftn 	geomg.cdk 	glb_ld.cdk 
vterp1.o: vterp1.ftn 	impnone.cdk 
vterp2.o: vterp2.ftn	 
water.o: water.ftn 	clefcon.cdk 	consphy.cdk 	dimsurf.cdk \
	dintern.cdk 	fintern.cdk 	fintern90.cdk 	impnone.cdk \
	indx_sfc.cdk 	locbus.cdk 	maxlev.cdk 	nbvarsurf.cdk \
	options.cdk 	phy_macros_f.h 	sfcbus.cdk 	vamin.cdk \
	xptsurf.cdk 	zuzt.cdk 
wdpers.o: wdpers.ftn 	glb_ld.cdk 	hzd.cdk 	impnone.cdk \
	lctl.cdk 	lun.cdk 	rhsc.cdk 	vt1.cdk \
	vth.cdk 
wetcon2.o: wetcon2.ftn 	consphy.cdk 	impnone.cdk 	phy_macros_f.h 
wflux.o: wflux.ftn 	impnone.cdk 
wlog.o: wlog.ftn 	cstv.cdk 	dimout.cdk 	glb_ld.cdk \
	impnone.cdk 	lctl.cdk 	out1.cdk 	out3.cdk \
	ptopo.cdk 
wndth.o: wndth.ftn 	glb_ld.cdk 	hzd.cdk 	impnone.cdk \
	lun.cdk 	vt0.cdk 	vt1.cdk 	vth.cdk 
writlam.o: writlam.ftn 	glb_ld.cdk 	impnone.cdk 	ptopo.cdk 
wrrstrt.o: wrrstrt.ftn 	c_cplg.cdk 	glb_ld.cdk 	impnone.cdk \
	init.cdk 	lctl.cdk 	lun.cdk 	mem.cdk \
	p_bus.cdk 	pilot.cdk 	pres.cdk 	rstr.cdk \
	schm.cdk 
wrvref.o: wrvref.ftn 	grd.cdk 	out.cdk 	pres.cdk 
xyfil.o: xyfil.ftn 	dcst.cdk 	glb_ld.cdk 	impnone.cdk 
zeracc.o: zeracc.ftn 	buses.cdk 	impnone.cdk 
zongini.o: zongini.ftn 	cstv.cdk 	dcst.cdk 	geomg.cdk \
	geomn.cdk 	glb_ld.cdk 	grd.cdk 	impnone.cdk \
	lun.cdk 	out1.cdk 	p_geol.cdk 	p_pbl.cdk \
	p_zong.cdk 	ptopo.cdk 	rstr.cdk 
zongoff.o: zongoff.ftn 	impnone.cdk 	init.cdk 	lctl.cdk \
	lun.cdk 	p_zong.cdk 	rstr.cdk 
zongopr.o: zongopr.ftn 	glb_ld.cdk 	impnone.cdk 	lctl.cdk \
	lun.cdk 


make_menu:
	$(MAKE)	"DEFINE=$(DEFINE)" "CPPFLAGS=$(CPPFLAGS)" \
        "CFLAGS=$(CFLAGS)" "FFLAGS=$(FFLAGS)" \
        "ASFLAGS=$(ASFLAGS)" "COMPILE=$(COMPILE)" "DOC=$(DOC)" `fait_le_menu2`

sortirtout:
	extrait_tout

#LISTE DES CIBLES DU FICHIER recettes

obj: $(OBJECTS) 
#Produire les fichiers objets (.o) pour tous les fichiers

full: $(FICHIERS)
#Inclure tous les fichiers .f dans le fichier $(COMPILE) et sortir le reste des fichiers
	-if [ "*.f" != "`echo *.f`" ] ; \
	then \
	cat *.f  > $(COMPILE) ;\
	fi ; \
	if [ "*.f90" != "`echo *.f90`" ] ; \
	then \
	cat *.f90  >> $(COMPILE)90.f90 ;\
	fi


compil:
#Inclure tous les fichers affectes par les dernieres\nmodifications dans le fichier $COMPILE
	-rm $(COMPILE) $(COMPILE)90.f90
	-if [ "*.cdk" != "`echo *.cdk`" ] ; \
	then \
	for i in *.cdk; \
	do \
	if [ -w $$i ] ; \
	then fn=$${i%.cdk}; \
	$(MAKE) -f make_cdk $$fn.acdk; \
	fi; \
	done \
	fi; \
	if [ "*.h" != "`echo *.h`" ] ; \
	then \
	for i in *.h; \
	do \
	if [ -w $$i ]; \
	then fn=$${i%.h};\
	$(MAKE) -f make_cdk $$fn.ah; \
	fi; \
	done \
	fi; \
	if [ "*.cdk90" != "`echo *.cdk90`" ] ; \
	then \
	for i in *.cdk90; \
	do \
	if [ -w $$i ]; \
	then fn=$${i%.h};\
	$(MAKE) -f make_cdk $$fn.acdk90; \
	fi; \
	done \
	fi; \
	if [ "*.[fp]tn" != "`echo *.[fp]tn`" ] ; \
	then \
	for i in *.[fp]tn; \
	do \
	fn=$${i%.[fp]tn}; \
	$(MAKE) "DEFINE=$(DEFINE)" $$fn.f; \
	done \
	fi ; \
	if [ "*.cdk90" != "`echo *.cdk90`" ] ; \
	then \
	for i in *.cdk90; \
	do \
	fn=$${i%.cdk90}; \
	$(MAKE) "DEFINE=$(DEFINE)" $$fn.o; \
	done \
	fi ; \	
	if [ "*.ftn90" != "`echo *.ftn90`" ] ; \
	then \
	for i in *.ftn90; \
	do \
	fn=$${i%.ftn90}; \
	$(MAKE) "DEFINE=$(DEFINE)" $$fn.f90; \
	done \
	fi ; \
	if [ "*.f" != "`echo *.f`" ] ;\
	then \
	cat *.f >> $(COMPILE) ;\
	fi ; \
	if [ "*.f90" != "`echo *.f90`" ] ;\
	then \
	cat *.f90 >> $(COMPILE)90.f90 ;\
	fi

compexp: sortirtout
#Inclure tous les modules de l experience en cours dans\nle fichier $COMPILE
	-rm $(COMPILE) $(COMPILE)90.f90
	-if [ "*.cdk" != "`echo *.cdk`" ] ; \
	then \
	for i in *.cdk; \
	do \
	fn=$${i%.cdk}; \
	$(MAKE) -f make_cdk $$fn.acdk; \
	done \
	fi; \
	if [ "*.h" != "`echo *.h`" ] ; \
	then \
	for i in *.h; \
	do \
	fn=$${i%.h}; \
	$(MAKE) -f make_cdk $$fn.ah; \
	done \
	fi; \
	if [ "*.[fp]tn" != "`echo *.[fp]tn`" ] ; \
	then \
	for i in *.[fp]tn; \
	do \
	fn=$${i%.[fp]tn}; \
	$(MAKE) "DEFINE=$(DEFINE)" "FFLAGS=$(FFLAGS)" $$fn.f; \
	done \
	fi ; \
	if [ "*.cdk90" != "`echo *.cdk90`" ] ; \
	then \
	for i in *.cdk90; \
	do \
	fn=$${i%.cdk90}; \
	$(MAKE) "DEFINE=$(DEFINE)" "FFLAGS=$(FFLAGS)" $$fn.o; \
	$(MAKE) -f make_cdk $$fn.acdk90; \
	done \
	fi ; \	
	if [ "*.ftn90" != "`echo *.ftn90`" ] ; \
	then \
	for i in *.ftn90; \
	do \
	fn=$${i%.ftn90}; \
	$(MAKE) "DEFINE=$(DEFINE)" "FFLAGS=$(FFLAGS)" $$fn.f90; \
	done \
	fi ; \
	if [ "*.f" != "`echo *.f`" ] ;\
	then \
	cat *.f >> $(COMPILE) ;\
	fi ; \
	if [ "*.f90" != "`echo *.f90`" ] ;\
	then \
	cat *.f90 >> $(COMPILE)90.f90 ;\
	fi

objloc:
#Produire les objets (.o) de tous les fichiers affectes\npar les dernieres modifications
	-if [ "*.cdk" != "`echo *.cdk`" ] ; \
	then \
	for i in *.cdk; \
	do \
	if [ -w $$i ] ; \
	then fn=$${i%.cdk}; \
	$(MAKE) -f make_cdk $$fn.acdk; \
	fi; \
	done \
	fi; \
	if [ "*.cdk90" != "`echo *.cdk90`" ] ; \
	then \
	for i in *.cdk90; \
	do \
	if [ -w $$i ] ; \
	then fn=$${i%.cdk90};\
	$(MAKE) -f make_cdk $$fn.acdk90; \
	fi; \
	done; \
	for i in *.cdk90; \
	do \
	fn=$${i%.cdk90};\
	$(MAKE) "DEFINE=$(DEFINE)" "FFLAGS=$(FFLAGS)" $$fn.o; \
	done \
	fi; \
	if [ "*.h" != "`echo *.h`" ] ; \
	then \
	for i in *.h; \
	do \
	if [ -w $$i ] ; \
	then fn=$${i%.h}; \
	$(MAKE) -f make_cdk $$fn.ah; \
	fi; \
	done \
	fi; \
	if [ "*.[fcs]" != "`echo *.[fcs]`" ] ; \
	then \
	for i in *.[fcs]; \
	do \
	fn=$${i%.[fcs]}; \
	$(MAKE) "FFLAGS=$(FFLAGS)" "CFLAGS=$(CFLAGS)" "ASFLAGS=$(ASFLAGS)" $$fn.o; \
	done \
	fi; \
	if [ "*.f90" != "`echo *.f90`" ] ; \
	then \
	for i in *.f90; \
	do \
	fn=$${i%.f90}; \
	$(MAKE) "FFLAGS=$(FFLAGS)" "CFLAGS=$(CFLAGS)" "ASFLAGS=$(ASFLAGS)" $$fn.o; \
	done \
	fi; \
	if [ "*.[fp]tn" != "`echo *.[fp]tn`" ] ; \
	then \
	for i in *.[fp]tn; \
	do \
	fn=$${i%.[fp]tn}; \
	$(MAKE) "DEFINE=$(DEFINE)" "FFLAGS=$(FFLAGS)" $$fn.o; \
	done \
	fi ; \
	if [ "*.ftn90" != "`echo *.ftn90`" ] ; \
	then \
	for i in *.ftn90; \
	do \
	fn=$${i%.ftn90}; \
	$(MAKE) "DEFINE=$(DEFINE)" "FFLAGS=$(FFLAGS)" $$fn.o; \
	done \
	fi


objexp: sortirtout
#Produire les fichiers objets de tous les modules de\nl experience en cours
	-if [ "*.cdk" != "`echo *.cdk`" ] ; \
	then \
	for i in *.cdk; \
	do \
	fn=$${i%.cdk}; \
	$(MAKE) -f make_cdk $$fn.acdk; \
	done \
	fi; \
	if [ "*.cdk90" != "`echo *.cdk90`" ] ; \
	then \
	for i in *.cdk90; \
	do \
	fn=$${i%.cdk90}; \
	$(MAKE) "FFLAGS=$(FFLAGS)" "DEFINE=$(DEFINE)"  $$fn.o; \
	$(MAKE) -f make_cdk $$fn.acdk90; \
	done \
	fi;\
	if [ "*.h" != "`echo *.h`" ] ; \
	then \
	for i in *.h; \
	do \
	fn=$${i%.h}; \
	$(MAKE) -f make_cdk $$fn.ah; \
	done \
	fi; \
	if [ "*.[fcs]" != "`echo *.[fcs]`" ] ; \
	then \
	for i in *.[fcs]; \
	do \
	fn=$${i%.[fcs]}; \
	$(MAKE) "FFLAGS=$(FFLAGS)" "CFLAGS=$(CFLAGS)" "ASFLAGS=$(ASFLAGS)" "CPPFLAGS=$(CPPFLAGS)" $$fn.o; \
	done \
	fi; \
	if [ "*.f90" != "`echo *.f90`" ] ; \
	then \
	for i in *.f90; \
	do \
	fn=$${i%.f90}; \
	$(MAKE) "FFLAGS=$(FFLAGS)" "CFLAGS=$(CFLAGS)" "ASFLAGS=$(ASFLAGS)" "CPPFLAGS=$(CPPFLAGS)" $$fn.o; \
	done \
	fi; \
	if [ "*.[fp]tn" != "`echo *.[fp]tn`" ] ; \
	then \
	for i in *.[fp]tn; \
	do \
	fn=$${i%.[fp]tn}; \
	$(MAKE) "FFLAGS=$(FFLAGS)" "DEFINE=$(DEFINE)"  $$fn.o; \
	done \
	fi ; \
	if [ "*.ftn90" != "`echo *.ftn90`" ] ; \
	then \
	for i in *.ftn90; \
	do \
	fn=$${i%.ftn90}; \
	$(MAKE) "FFLAGS=$(FFLAGS)" "DEFINE=$(DEFINE)"  $$fn.o; \
	done \
	fi



qcompil:
#Inclure uniquement les fichiers .f .f90 modifies dans le fichier $(COMPILE)
	-rm $(COMPILE) $(COMPILE)90.f90
	-if [ "*.[fp]tn" != "`echo *.[fp]tn`" ] ;\
	then \
	for i in *.[fp]tn; \
	do \
	fn=$${i%.[fp]tn}; \
	$(MAKE) "DEFINE=$(DEFINE)" $$fn.f; \
	done \
	fi ;\
	if [ "*.f" != "`echo *.f`" ] ;\
	then \
	cat *.f > $(COMPILE) ;\
	fi ; \
	if [ "*.cdk90" != "`echo *.cdk90`" ] ;\
	then \
	for i in *.cdk90; \
	do \
	fn=$${i%.cdk90}; \
	$(MAKE) "DEFINE=$(DEFINE)" $$fn.o; \
	done \
	fi ;\
	if [ "*.f90" != "`echo *.f90`" ] ;\
	then \
	cat *.f90 >> $(COMPILE)90.f90 ;\
	fi ; \
	if [ "*.ftn90" != "`echo *.ftn90`" ] ;\
	then \
	for i in *.ftn90; \
	do \
	fn=$${i%.ftn90}; \
	$(MAKE) "DEFINE=$(DEFINE)" $$fn.f90; \
	done \
	fi ;\
	if [ "*.f90" != "`echo *.f90`" ] ;\
	then \
	cat *.f90 >> $(COMPILE)90.f90 ;\
	fi

qobj:
#Compiler uniquement les fichiers  modifies.
	-if [ "*.[fcs]" != "`echo *.[fcs]`" ] ;\
	then \
	for i in *.[fcs]; \
	do \
	fn=$${i%.[fcs]}; \
	$(MAKE) "FFLAGS=$(FFLAGS)" "CFLAGS=$(CFLAGS)" "ASFLAGS=$(ASFLAGS)" $$fn.o; \
	done \
	fi ;\
	if [ "*.[fp]tn" != "`echo *.[fp]tn`" ] ;\
	then \
	for i in *.[fp]tn; \
	do \
	fn=$${i%.[fp]tn}; \
	$(MAKE) "DEFINE=$(DEFINE)" $$fn.o; \
	done \
	fi ; \
	if [ "*.cdk90" != "`echo *.cdk90`" ] ;\
	then \
	for i in *.cdk90; \
	do \
	fn=$${i%.cdk90}; \
	$(MAKE) "FFLAGS=$(FFLAGS)" "CFLAGS=$(CFLAGS)" "ASFLAGS=$(ASFLAGS)" $$fn.o; \
	done \
	fi ;\
	if [ "*.f90" != "`echo *.f90`" ] ;\
	then \
	for i in *.f90; \
	do \
	fn=$${i%.f90}; \
	$(MAKE) "FFLAGS=$(FFLAGS)" "CFLAGS=$(CFLAGS)" "ASFLAGS=$(ASFLAGS)" $$fn.o; \
	done \
	fi ;\
	if [ "*.ftn90" != "`echo *.ftn90`" ] ;\
	then \
	for i in *.ftn90; \
	do \
	fn=$${i%.ftn90}; \
	$(MAKE) "DEFINE=$(DEFINE)" $$fn.o; \
	done \
	fi

genlib: $(OBJECTS)
#Creer une programmatheque ayant pour nom $MALIB et incluant TOUS les fichiers objets
	$(AR) rv $(MALIB) $(OBJECTS)

majlib: objloc
#Mise a jour de la programmatheque $MALIB a partir de tous les fichers .o affectes par les dernieres modifications
	$(AR) rv $(MALIB) *.o

qmajlib: qobj
#Mise a jour de la programmatheque $MALIB a partir de tous les fichers .o presents dans le repertoire courant
	$(AR) rv $(MALIB) *.o

libexp: sortirtout objexp
#Mettre tous les objets de l experience en cours dans la programmatheque $MALIB
	$(AR) rv $(MALIB) *.o


clean:
#Faire le grand menage. On enleve tous les fichiers sources inutiles et les .o 
	-if [ "*.[chfs]" != "`echo *.[chfs]`" ] ; \
	then 	for i in *.[chfs]; \
	do \
	if (r.ucanrm $$i)  ; \
	then rm -f $$i; \
	fi; \
	done \
	fi; \
	if [ "*.cdk" != "`echo *.cdk`" ] ; \
	then 	for i in *.cdk; \
	do \
	if (r.ucanrm  $$i) ; \
	then rm -f $$i; \
	fi; \
	done \
	fi; \
	if [ "*.txt" != "`echo *.txt`" ] ; \
	then 	for i in *.txt; \
	do \
	if (r.ucanrm  $$i) ; \
	then rm -f $$i; \
	fi; \
	done \
	fi; \
	if [ "*.*_sh" != "`echo *.*_sh`" ] ; \
	then \
	for i in *.*_sh; \
	do \
	if (r.ucanrm  $$i) ; \
	then rm -f $$i; \
	fi; \
	done \
	fi; \
	if [ "*.[fp]tn" != "`echo *.[fp]tn`" ] ; \
	then \
	for i in *.[fp]tn; \
	do \
	if (r.ucanrm $$i) ; \
	then rm -f $$i; \
	fi; \
	done \
	fi ; \
	rm *.o
	-if [ "*.f90" != "`echo *.f90`" ] ; \
	then \
	for i in *.f90; \
	do \
	if (r.ucanrm $$i)  ; \
	then rm -f $$i; \
	fi; \
	done \
	fi; \
	if [ "*.cdk90" != "`echo *.cdk90`" ] ; \
	then \
	for i in *.cdk90; \
	do \
	if (r.ucanrm  $$i) ; \
	then rm -f $$i; \
	fi; \
	done \
	fi ; \
	if [ "*.ftn90" != "`echo *.ftn90`" ] ; \
	then \
	for i in *.ftn90; \
	do \
	if (r.ucanrm $$i) ; \
	then rm -f $$i; \
	fi; \
	done \
	fi
	rm *.o *.mod

qclean:
#Faire un petit menage. On enleve tous les .o et les .f produits a partir de .ftn/.ptn
	-if [ "*.[fp]tn" != "`echo *.[fp]tn`" ] ; \
	then \
	for i in *.[fp]tn ; \
	do \
	fn=$${i%.[fp]tn}; \
	rm -f $$fn.f; \
	done \
	fi ; \
	if [ "*.ftn90" != "`echo *.ftn90`" ] ; \
	then \
	for i in *.ftn90 ; \
	do \
	fn=$${i%.ftn90};\
	rm -f $$fn.f90; \
	done \
	fi 
	rm *.o

#Descriptions des recettes personnelles
 
#Descriptions du fichier /usr/local/env/armnlib/modeles/GEMDM_shared/v_3.2.2/RCS/.recettes
 
# 
#   Common recipes for models
#
include $(MODEL_PATH)/Makefile_$(ARCH)
AR          = r.ar -arch $(ARCH)
PHY_VERSION = 4.4
PHY         = phy
PATCH       = 
OPTF        =
#
LIBMODEL= $(ARMNLIB)/modeles/$(FULL_MODEL)/v_$(MODEL_VERSION)/lib/$(ARCH)$(ABI)
LESLIB  = $(LIBMODEL) $(ARMNLIB)/modeles/PHY/v_$(PHY_VERSION)/lib/$(ARCH)$(ABI)
LCLPO   = malib$(ARCH)
mainntr = main$(MODEL)ntr_$(ARCH)_${MODEL_VERSION}.Abs
maindm  = main$(MODEL)dm_$(ARCH)_${MODEL_VERSION}.Abs
RCOMPIL = r.compile $(MPI) $(OMP)
#
rmpo: 
	/bin/rm -f $(LCLPO)/*.o
#
libdyn: rmpo $(OBJECTS)
#
geophy: geophy.o
	makebidon geophy
	$(RBUILD) -obj $(LCLPO)/geophy.o $(LCLPO)/bidon.o -arch $(ARCH) -abi $(ABI) -opt "=$(LFLAGS)" -o geophy.Abs -librmn $(RMN)
	/bin/rm -f $(LCLPO)/geophy.o $(LCLPO)/bidon.o
#


 
#Descriptions du fichier /usr/local/env/armnlib/modeles/GEMDM_shared/v_3.2.2/RCS/.cibles
 
# 
#   Targets to load gemdm
#
gemntr:
	makebidon gemntr;\
	cd $(LCLPO);\
	$(RBUILD) -obj *.o -libpath $(LESLIB) -arch $(ARCH) -abi $(ABI) -mpi -optf "=$(LFLAGS)" -o $(mainntr) $(OMP) -libappl "gem gem_4dvar_stubs $(PHY) $(PATCH) prof_003 modulopt $(COMM) rtools lapack blas massvp4 $(IBM_LD)" -librmn $(RMN) -libpriv $(PRIV) ;\
	/bin/cp $(mainntr) $(PWD)/$(mainntr) ; /bin/rm -f $(mainntr)* bidon.o

gemdm:
	makebidon gemdm;\
	cd $(LCLPO);\
	$(RBUILD) -obj *.o -libpath $(LESLIB) -arch $(ARCH) -abi $(ABI) -mpi -optf "=$(LFLAGS)" -o $(maindm) $(OMP) -libappl "gem gem_4dvar_stubs $(PHY) $(PATCH) prof_003 modulopt $(COMM) rtools lapack blas massvp4 $(IBM_LD)" -librmn $(RMN) -libpriv $(PRIV) ;\
	/bin/cp $(maindm) $(PWD)/$(maindm) ; /bin/rm -f $(maindm)* bidon.o

gem4d:
	makebidon gemdm;\
	cd $(LCLPO);\
	$(RBUILD) -obj *.o -libpath $(LESLIB) -arch $(ARCH) -abi $(ABI) -mpi -optf "=$(LFLAGS)" -o $(maindm) $(OMP) -libappl "gem gem_4dvar $(PHY) $(PATCH) prof_003 modulopt $(COMM) rtools lapack blas massvp4 $(IBM_LD)" -librmn $(RMN) -libpriv $(PRIV) ;\
	/bin/cp $(maindm) $(PWD)/$(maindm) ; /bin/rm -f $(maindm)* bidon.o

gem: gemntr gemdm

gemntr_nompi:
	makebidon gemntr;\
	cd $(LCLPO);\
	$(RBUILD) -obj *.o -libpath $(LESLIB) -arch $(ARCH) -abi $(ABI) -optf "=$(LFLAGS)" -o $(mainntr) $(OMP) -libappl "gem gem_4dvar_stubs $(PHY) $(PATCH) prof_003 modulopt $(COMM) rtools blas massvp4 $(IBM_LD)" -librmn $(RMN) -libpriv $(PRIV) -codebeta 'rpn_comm_stubs';\
	/bin/cp $(mainntr) $(PWD)/$(mainntr) ; /bin/rm -f $(mainntr)* bidon.o

gemdm_nompi:
	makebidon gemdm;\
	cd $(LCLPO);\
	$(RBUILD) -obj *.o -libpath $(LESLIB) -arch $(ARCH) -abi $(ABI) -optf "=$(LFLAGS)" -o $(maindm) $(OMP) -libappl "gem gem_4dvar_stubs $(PHY) $(PATCH) prof_003 modulopt $(COMM) rtools lapack blas massvp4 $(IBM_LD)" -librmn $(RMN) -libpriv $(PRIV) -codebeta 'rpn_comm_stubs';\
	/bin/cp $(maindm) $(PWD)/$(maindm) ; /bin/rm -f $(maindm)* bidon.o

gem_nompi: gemntr_nompi gemdm_nompi
#


 
include mes_recettes
