/***********************************************************************

 HiSIM (Hiroshima University STARC IGFET Model)
 Copyright (C) 2000-2016 Hiroshima University and STARC
 Copyright (C) 2016-2019 Hiroshima University
 HiSIM_HV (High-Voltage Model)
 Copyright (C) 2007-2016 Hiroshima University and STARC
 Copyright (C) 2016-2019 Hiroshima University

 MODEL NAME : HiSIM_HV 
 ( VERSION : 2  SUBVERSION : 5  REVISION : 0 )
 Model Parameter 'VERSION' : 2.50
 FILE : HSMHV_temp_eval_dio.inc 

 Date : 2019.04.26

 released by Hiroshima University

***********************************************************************/
//
////////////////////////////////////////////////////////////////
//
//
//
//The HiSIM_HV standard has been supported by the members of 
//Silicon Integration Initiative's Compact Model Coalition. A 
//link to the most recent version of this standard can be found 
//at:
//
//http://www.si2.org/cmc
//
////////////////////////////////////////////////////////////////
//



T0 = Tratio * Tratio ;


// for substrate-drain junction diode. //
`HSMHV_Idio_args1(js,     UC_JS0D,    XTID,  UC_NJD)
`HSMHV_Idio_args1(jssw,   UC_JS0SWD,  XTID,  NJSWD)
`HSMHV_Idio_args1(jsswg,  JS0SWGD,    XTID,  NJSWGD)
`HSMHV_Idio_args1(js2,    UC_JS0D,    XTI2D, UC_NJD)
`HSMHV_Idio_args1(jssw2,  UC_JS0SWD,  XTI2D, NJSWD)
`HSMHV_Idio_args1(jsswg2, JS0SWGD,    XTI2D, NJSWGD)

if ( CODIO > 0 ) begin //new model:
   `HSMHV_Idio_args2(isbd_btm, isbd2_btm, isbd_sws, isbd2_sws, isbd_swg, isbd2_swg, PD, AD)
end else begin //backward compatible:
   `HSMHV_Idio_args2_compat(isbd_btm, isbd2_btm, isbd_sws, isbd2_sws, isbd_swg, isbd2_swg, PD, AD)
end
isbd = isbd_btm + isbd_sws + isbd_swg ;

if( isbd > 0 ) begin
   T2 = isbd + `Small ; //once removed in Geoffrey01_Small_20160309, but necessary for backward compatibility
   vbdt = UC_NJD / beta * ln( UC_VDIFFJD * T0 / T2 + 1.0 ) ;
   exptempd = exp (( Tratio - 1.0 ) * CTEMPD ) ;
   jd_nvtm_invd = 1.0 / ( UC_NJD / beta ) ;
   jd_expcd = exp (vbdt * jd_nvtm_invd ) ;
end


// for substrate-source junction diode. //
`HSMHV_Idio_args1(js,     UC_JS0S,    XTIS,  UC_NJS)
`HSMHV_Idio_args1(jssw,   UC_JS0SWS,  XTIS,  NJSWS)
`HSMHV_Idio_args1(jsswg,  JS0SWGS,    XTIS,  NJSWGS)
`HSMHV_Idio_args1(js2,    UC_JS0S,    XTI2S, UC_NJS)
`HSMHV_Idio_args1(jssw2,  UC_JS0SWS,  XTI2S, NJSWS)
`HSMHV_Idio_args1(jsswg2, JS0SWGS,    XTI2S, NJSWGS)

if ( CODIO > 0 ) begin //new model:
   `HSMHV_Idio_args2(isbs_btm, isbs2_btm, isbs_sws, isbs2_sws, isbs_swg, isbs2_swg, PS, AS)
end else begin //backward compatible:
   `HSMHV_Idio_args2_compat(isbs_btm, isbs2_btm, isbs_sws, isbs2_sws, isbs_swg, isbs2_swg, PS, AS)
end
isbs = isbs_btm + isbs_sws + isbs_swg ;

if( isbs > 0 ) begin
   T3 = isbs + `Small ; //once removed in Geoffrey01_Small_20160309, but necessary for backward compatibility
   vbst = UC_NJS / beta * ln( UC_VDIFFJS * T0 / T3 + 1.0 ) ;
   exptemps = exp (( Tratio - 1.0 ) * CTEMPS ) ;
   jd_nvtm_invs = 1.0 / ( UC_NJS / beta ) ;
   jd_expcs = exp (vbst * jd_nvtm_invs ) ;
end


//-----------------------------------------------------------*
//* Charges and Capacitances.
//*-----------------//
      
// Drain Bulk Junction //
`HSMHV_Qdio_args1(czbd, czbdsw, czbdswg, PD, AD, CJD, TCJBD, CJSWD, TCJBDSW, CJSWGD, TCJBDSWG, "D")
`HSMHV_Qdio_args2(pzbd, pzbdsw, pzbdswg, PD, AD, PBD, TPBBD, PBSWD, TPBBDSW, PBSWGD, TPBBDSWG, "D")

// Source Bulk Junction //
`HSMHV_Qdio_args1(czbs, czbssw, czbsswg, PS, AS, CJS, TCJBS, CJSWS, TCJBSSW, CJSWGS, TCJBSSWG, "S")
`HSMHV_Qdio_args2(pzbs, pzbssw, pzbsswg, PS, AS, PBS, TPBBS, PBSWS, TPBBSSW, PBSWGS, TPBBSSWG, "S")

// end of HSMHVtemp_eval_dio.h //
