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
 FILE : HSMHV_eval_rdrifts.inc 

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

           
begin : HSMHV_eval_rdrifts 
if( flg_rs == 0 ) begin
    // does nothing; no resistance on source side;  node sp shall be collapsed.
end
else begin //  CORS == 1; source side resistance in consideration

    //-----------------------------------------------------------*
    //* Temperature dependent constants.
    //*-----------------//
    if( COSELFHEAT > 0 && UC_RTH0 != 0.0 ) begin
              
`include "HSMHV_temp_eval_rdris.inc"  // internally updates UC_RDRBB_s
    end else begin
      TTEMP = $temperature + DTEMP ;
    end
           
    WeffLD_nf = weff_ld * NF ;  // Used for Rsd = Rsd / WeffLD_nf ;
            //Ldrifte_s = LDRIFT1S + LDRIFT2S ; // compatibility btw rdrift and rdrifts
            Ldrifte_s = LDRIFTS; // (new feature)
    Novers = UC_NOVERS ;

            Mu0_s   = RRDRMUES * Rdrmuele;
            Vmaxe_s = RRDRVMAXS * Rdrvmaxwe * Rdrvmaxle + `Small;
`ifdef __DEBUG_RDRIFTS__
            $write("debug_eval_rdrifts_0[ %g %g %g %g ]\n",Vmaxe_s, RRDRVMAXS, MKS_RDRVMAXS,RRDRMUES); /
`endif

            Edri_s     = Vsps  / Ldrifte_s ;
            Vdri_s     = Mu0_s  * Edri_s ;
    //-----------------------------------------------------------*
    //* Mu : mobility
    //*-----------------//
    if( Vsps >= 0 ) begin
      T1       = Vdri_s / Vmaxe_s ;
    end else  begin
      T1       = - Vdri_s / Vmaxe_s ;
    end
`ifdef __DEBUG_RDRIFTS__
    $write("debug_eval_rdrifts_a[ %g %g %g  %g %g ]\n", Vsps, Vdri_s, Vmaxe_s, Edri_s, Mu0_s);
`endif
    if( T1 == 0.0 ) begin
      T2 = 0.0 ;  
      T4 = 1.0 ;  
    end else begin
      if( 1.0e0 - `epsm10 <= UC_RDRBB_s && UC_RDRBB_s <= 1.0e0 + `epsm10 )  begin
        T3 = 1.0e0 ;
      end else if( 2.0e0 - `epsm10 <= UC_RDRBB_s && UC_RDRBB_s <= 2.0e0 + `epsm10 )  begin
        T3 = T1 ;
      end else  begin
        T3 = `Fn_Pow( T1 , UC_RDRBB_s - 1.0e0 ) ;
      end
      T2 = T1 * T3 ;
      T4 = 1.0e0 + T2 ;
    end
    if( 1.0e0 - `epsm10 <= UC_RDRBB_s && UC_RDRBB_s <= 1.0e0 + `epsm10 )  begin
      T5 = 1.0 / T4 ;
    end else if( 2.0e0 - `epsm10 <= UC_RDRBB_s && UC_RDRBB_s <= 2.0e0 + `epsm10 )  begin
      T5 = 1.0 / sqrt( T4 ) ;
    end else  begin
      T6 = `Fn_Pow( T4 , ( - 1.0e0 / UC_RDRBB_s - 1.0e0 ) ) ;
      T5 = T4 * T6 ;
    end
    Mu_s   = Mu0_s * T5 ;  // velocity saturation accounted
`ifdef __DEBUG_RDRIFTS__
    $write("debug_eval_rdrifts_b[ %g %g %g ]\n", Mu_s, Mu0_s, T5);
`endif             
    //-----------------------------------------------------------*
    //* Carr : carrier density
    //*-----------------//
    Carr_s = Novers;
    Xov_s = Xmax_s;
    //-----------------------------------------------------------*
    //* Rd : drift resistance
    //*-----------------//
//drain:    T1 = `C_QE / ( Ldrifte + RDRDL1 );
    T1 = `C_QE / ( Ldrifte_s );
    GD_s = T1 * Xov_s * Mu_s * Carr_s ;
`ifdef __DEBUG_RDRIFTS__
$write("debug_eval_rdrifts_c[ %g %g %g %g %g  %g %g %g  %g ]\n", GD_s, T1, Xov_s, Mu_s, Carr_s, Mu0_s, Vdri_s, Vmaxe_s, Edri_s);
`endif

`ifdef REPLACE_CLIPPING_WITH_SMOOTHING //revised for continuity (Rsd)
     `Fn_SL_CP(GD_s, GD_s, 0, `Small, 2, T0)
`else
    if( GD_s <= 0 ) begin
      GD_s = `Small ;
    end
`endif

    Rsd = 1 / GD_s ;
    // Weff dependence of the resistances //
    Rsd = Rsd  /  WeffLD_nf ;

    // Sheet resistances are added. //
    //drain: Rdd = Rdd + Rd0 ;
    Rsd = Rsd + Rs0 ;

    // Re-stamps for hsmhvnoi.c //
    //drain: if ( Rdd  > `Res_min && COTHRML != 0 ) drainConductance = Mfactor / Rdd ;
    //drain: else drainConductance = 0.0 ;
    if ( Rsd  > `Res_min && COTHRML != 0 ) sourceConductance = Mfactor / Rsd ;
    else sourceConductance = 0.0 ;

    // Clamping to Res_min //
    if(Rsd < `Res_min) begin Rsd = `Res_min ; end
    Rsde = Rsd / Mfactor ;   // Rsd to have been evaluated in HSMHV_eval_rdrifts.inc beforehand.
    //drain: if(Rdd < `Res_min) begin Rdd = `Res_min ; end
    //drain: Rdde = Rdd / Mfactor ;

end //  if ( flg_rs == 0)
end //  HSMHV_eval_rdrifts
 
