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
 FILE : HSMHV_eval_rdrift.inc 

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

           
//---------------------------------------------------*
//* "smoothUpper" using a power function
//*-----------------*/
`define Fn_SU_POW( y , x , xmax , delta ) begin \
 if( (x) == 0 || (delta) <= 0 ) begin \
   y = x ; \
 end else begin \
   TMF3 = (x) < 0 ? -1 : 1 ; \
   TMF4 = TMF3 * (x) ; \
   TMF1 = 1.0  + pow( TMF4 / (xmax) , (delta) ) ; \
   TMF2 = pow( TMF1, 1/(delta) ) ; \
   y    = TMF3 * TMF4 / TMF2 ; \
 end \
end
//---------------------------------------------------*

 begin : HSMHV_eval_rdrift 
    real Vds, Vbs , Mu , Vmaxe ;
//  real Carr1 , Carr2 ;

if ( flg_rd == 0 ) begin
   // does nothing; drain side resistance zero; node dp shall be collapsed beforehand.
end
else begin
//  getting biases 
    Vds = Vdsi ; Vbs = Vbsi ;

    //-----------------------------------------------------------*
    //* Temperature dependent constants.
    //*-----------------//
    if( COSELFHEAT > 0 && UC_RTH0 != 0.0 ) begin
              
`include "HSMHV_temp_eval_rdri.inc"
    end else begin
      TTEMP = $temperature + DTEMP ;
    end
           
    WeffLD_nf = weff_ld * NF ;
    Ldrifte   = LDRIFT1 + LDRIFT2;
    //Ldrifte = LDRIFT; // in future
    Rd_Xldld  = UC_XLDLD + `Small2 ;
    Noverd    = UC_NOVER ;
    T1 = 1.0e0 + Vbs*(RDRMUEBS1 - RDRMUEBS2 * Vbs); // 20181017
    `Fn_SZ( RdrmueVbs,  T1, 0.1, T2)                // 20181017
    Mu0 = RRDRMUE  * Rdrmuele  * RdrmueVbs ;        // 20181017

    Vmaxe = RRDRVMAX * Rdrvmaxwe * Rdrvmaxle + `Small ;
    Cx    = UC_RDRCX ;
    Car   = RDRCAR ;

    // Check the value after L , W and T dependence
    T1 = Mu0 * `C_m2cm_p2 ;
    T2 = Vmaxe * `C_m2cm ;
    `RANGECHECK(T1, 1e2 , 3e3 , "RDRMUE@L,T dependence")
    `RANGECHECK(T2, 1e6 , 100e6  , "RDRVMAX@L,W,T dependence ")
            
    //-----------------------------------------------------------*
    // Modified bias introduced to realize symmetry at Vddp=0.
    //-----------------//
    if(Vddp < 0) begin
      `Fn_SymAdd( Vzadd , -Vddp / 2 , VZADD0 , T2 ) 
      if( Vzadd < `ps_conv ) begin
        Vzadd = `ps_conv ;
      end
      Vddpz = Vddp - 2 * Vzadd ;
    end else begin
      `Fn_SymAdd( Vzadd ,  Vddp / 2 , VZADD0 , T2 ) 
      if( Vzadd < `ps_conv ) begin
        Vzadd = `ps_conv ;
      end
      Vddpz = Vddp + 2 * Vzadd ;
    end

    Edri = Vddpz / Ldrifte ;
    Vdri = Mu0   * Edri ;
               
    //-----------------------------------------------------------*
    //* Mu : mobility
    //*-----------------//
    if( Vddp >= 0 ) begin
      T1 = Vdri / Vmaxe ;
    end else  begin
      T1 = - Vdri / Vmaxe ;
    end
    if( T1 == 0.0 ) begin
      T2 = 0.0 ;  
      T4 = 1.0 ;  
    end else begin
      if( 1.0e0 - `epsm10 <= UC_RDRBB && UC_RDRBB <= 1.0e0 + `epsm10 )  begin
        T3 = 1.0e0 ;
      end else if( 2.0e0 - `epsm10 <= UC_RDRBB && UC_RDRBB <= 2.0e0 + `epsm10 )  begin
        T3 = T1 ;
      end else  begin
        T3 = `Fn_Pow( T1 , UC_RDRBB - 1.0e0 ) ;
      end
      T2 = T1 * T3 ;
      T4 = 1.0e0 + T2 ;
    end
    if( 1.0e0 - `epsm10 <= UC_RDRBB && UC_RDRBB <= 1.0e0 + `epsm10 )  begin
      T5 = 1.0 / T4 ;
    end else if( 2.0e0 - `epsm10 <= UC_RDRBB && UC_RDRBB <= 2.0e0 + `epsm10 )  begin
      T5 = 1.0 / sqrt( T4 ) ;
    end else  begin
      T6 = `Fn_Pow( T4 , ( - 1.0e0 / UC_RDRBB - 1.0e0 ) ) ;
      T5 = T4 * T6 ;
    end
    Mu = Mu0 * T5 ;
    //-----------------------------------------------------------*
    //* Carr : carrier density
    //*-----------------//
    T4 = 1.0e0 + T1 ;
    T5 = 1.0 / T4 ;
    T4 = 1.0 + Car * ( 1.0 - T5 ) * Vddpz /  ( Ldrifte - RDRDL2 ) ;
    `Fn_SL( T5, T4, 0.0, 1e-3, T0 )
    Carr1 = Noverd * T5 ;
    Carr2 = RD_QbuLD / `C_QE * RDRQOVER ;
    `Fn_SU( Carr2, Carr2, Carr1, Carr1*1e-3, T0)
    Carr = Carr1 - Carr2 ;
    if( NDRIDLT > 0 && NDRILIM > 1.0 ) begin
//    `Fn_SU_POW( T2 , Mu , Mu0*RDRMUELIM , RDRMUEDLT )
//kiku      `Fn_SU_CP( T2 , Carr , Noverd*NDRILIM , Noverd*NDRILIM/2 , NDRIDLT , T0 )
      `Fn_SU_CP( T2 , Carr , Noverd*NDRILIM , Noverd*NDRIDLT , NDRIPW , T0 )
      Carr = T2 ;
    end
`ifdef _AGING_
    //        COFIXAG  =1(new)  =0(old)
    Vdseeff = COFIXAG ? AVddp : Vtraplx_prv ; 
    DVddp = TRAPDVDDP + DVddp * lexp( - Vdseeff / TRAPDLX ) ;
    `ifdef _DEBUG_
      $write("DVddp= %g Vddpz = %g Vdseeff = %g TRAPDLX=%g \n",DVddp, Vddpz, Vdseeff, TRAPDLX);
    `endif
    Carr = Carr + Noverd * DVddp ; 
`endif

    //-----------------------------------------------------------*
    //* Xov : depth of the current flow
    //*-----------------//
    T0 = -RD_Ps0LD ;
    `Fn_SZ( T0 , T0 , 1.0e-2 , T9 ) 
    T0 = T0 + `epsm10 ;
    Wdepl  = sqrt ( kdep * T0 ) ;
    T2 = Vds - Vbs + VBI ;
    `Fn_SZ( T2 , T2 , 1.0e-2 , T9 ) 
    T2 = T2 + `epsm10 ;
    Wjunc0 = sqrt ( kjunc * T2 ) ;
    `Fn_SU( Wjunc, Wjunc0, Rd_Xldld, 10e-3*Rd_Xldld, T0 )
    Wrdrdjunc = RDRDJUNC + `Small ;
    Xov = Xmax * ( 1.0 - Cx * ( Wdepl / Wrdrdjunc + Wjunc / Rd_Xldld ));
    `Fn_SZ( Xov , Xov , (1.0 - UC_RDRCX) * Xmax / 100 , T9 ) 
     //-----------------------------------------------------------*
    //* Rd : drift resistance
    //*-----------------//
    T1 = `C_QE / ( Ldrifte + RDRDL1 );
    GD = T1 * Xov * Mu * Carr ;

`ifdef REPLACE_CLIPPING_WITH_SMOOTHING //revised for continuity (Rdd)
     `Fn_SL_CP(GD, GD, 0, `Small, 2, T0)
`else
    if( GD <= 0 ) begin
      GD = `Small ;
    end
`endif

    Rdd  = 1 / GD ;

    // Weff dependence of the resistances //
    Rdd = Rdd / WeffLD_nf ;

    `Fn_SU_CP( Rdd, Rdd, 1e6, 1e3, 2, T0 )

    /* subnode modeling */
    if( COSUBNODE == 1 &&
      ( UC_NOVER * ( `NSUBSUB + UC_NOVER ) ) > 0 ) begin
      ddriftld = `DDRIFT - Wdep ;
      Rdd = Rdd * Ldrift0 / ddriftld ;
    end

    // Sheet resistances are added. //
    Rdd = Rdd + Rd0 ;

    // Re-stamps for hsmhvnoi.c //
    if ( Rdd > `Res_min && COTHRML != 0 ) drainConductance = Mfactor / Rdd ;
    else drainConductance = 0.0 ;

    if(Rdd < `Res_min) begin Rdd = `Res_min ; end
    // Clamping to Res_min //
    Rdde = Rdd / Mfactor ;

 end //  if ( flg_rd == 0)
 end //  HSMHV_eval_rdrift 
         
