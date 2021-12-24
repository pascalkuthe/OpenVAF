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
 FILE : vaFiles/HSMHV_depmos3.inc 

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




 begin : HSMHV_depmos3

   real W_bsub0, W_bsubL ;
   real Vds_maxb0, Vds_maxbL ;
   real phi_s0_DEP, phi_sL_DEP , Vbi_DEP ;
   real phi_b0_DEP, phi_bL_DEP ;
   real phi_j0_DEP, phi_jL_DEP ;
   real phi_s0_DEP_ini , phi_sL_DEP_ini ;
//
    real Q_s0, Q_sL ;
//
   real PF1 , PF11 , PF12 , PF2 , PF21 , PF22 ;
   real PDJ , PDJI , PJI11 , PJI12 , PJI21 , PJI22 , dPs , dPb ;
//
   real q_Ndepm_esi, Edri ;
   real Ey_acc , Vbsc ;
//
   real Q_n0_cur , Q_nL_cur , Q_n0_sym ; 
   real sum_Q_nx_cur ;
   real Q_n0 , Q_nL , Qn_res ;
//   
   real Q_s0_dPs, Q_sL_dPs ;
   real Q_s0_dPb, Q_sL_dPb ;
   real NdepmpNsub_inv1, NdepmpNsub ;
//
   real Tn , N_res ; 
   real q_Ndepm , q_Ndepm_esi_Cox_inv2 , C2_q_Ndepm_esi_Cox_inv2 ; 
   real C_2ESIpq_Ndepm ; 
   real phib_ref, phib_ref_dPs , phib_ref_dPb , phi_j0_DEP_dPb , phi_jL_DEP_dPb ;
//
   real VgpDEP_dlt;
   integer VgpDEP_pw;
   real Vgpz , Vgp_res ;
   real Vgp_res_raw; // without Fn_SU_CP (ceiling by 0.0)  
   real Ws, Tnp ;
   real Tnp0 , phi_j0_DEP0 , W_bsub00 , wDEPSUBSL ;
   real Ps0_res , Eeff_res , W_res_leak ;

   real phi_vsat , Q_sat , Q_sat_dPs , Vgpsat; 
   real Vdssat_ini , Vdssat1d , Vdssat ; // , Vdssat_res ;
   real u, usqr;
   real Pb0DEP , Vds_res0 , Vds_res0_sym , Vds_resz , Edri2 ;
   real depvbs , Ps0DEP0 ; //, Ps0DEP1 ;
   //real Qn_drift , real Idd_drift , Qn_delta ;

    
  `define C_PHI_1_MINIMUM (1e-7)
  `define C_leak_delta 1e-4
  `define C_sum_Q_nx_cur_delta (`VgVt_Small*`Cox_Small)

  `define CO_WK_VDSATRES1VARNT 0    // "-", 0, 1, "VDSATRES 1varNT(1) or analytical(0); depmos3") // 20170929                                                                MPIcc
  `define CO_WK_QNRESWRES      0    // "-", 0, 2, "W_res(0) or W_res_qnres(1) for calculation of Qn_res ; further, W_res replaced by W_res_qnres (2) depmos3") // 20170831   MPIcc
  `define WK_IACC              1.0  // "-", 0, 4.0,"Ids_acc enabler; depmos3")                                                                                       MPRco
  `define WK_IRES              1.0  // "-", 0, 4.0,"Ids_res enabler; depmos3")                                                                                       MPRco
  `define WK_ILEAK             1.0  // "-", 0, 4.0,"Ids_leak enabler; depmos3")                                                                                      MPRco
  `define WK_CO_DEPMUE         1    // "-", "DEPMUE enabler depmos3") // 20170902                                                                                            MPIsw
  `define WK_CO_DEPMUEPH       1    // "-", "DEPMUEPH enabler depmos3") // 20170902                                                                                  MPIsw
  `define WK_CO_NEWVGPRES      0    // "-", 0,2, "Disregard dVth etc for Vgpres (1) furthermore Vgs (2) instead of Vgsz depmos3") // 20170902                        MPIcc
  `define WK_CO_MUECB          1    // "-", "MUECB enabler depmos3") // 20170920                                                                                             MPIsw
  `define WK_CO_MUEPH          1    // "-", "MUEPH enabler depmos3") // 20170920                                                                                             MPIsw
  `define WK_CO_MUESR          1    // "-", "MUESR enabler depmos3") // 20170920                                                                                             MPIsw
  `define WK_CO_DVGPACC        1    // "-", "Vgp acc dVgp on(1)  off(0) depmos3") // 20171002                                                                        MPIsw
  `define WK_CO_VDSSATACC_1VARNT 1  // "-", "Vdssat for Iacc; 1varNT(1) or analytical(0); depmos3") // 20171004                                                      MPIsw
  `define WK_CO_SMOOTHCHARGE_IDD 1  // "-","Smooth Q_n0, Q_nL for IDD using DEPPS (1); depmos3")// 20171006                                                          MPIsw
  `define WK_DVTHRES_MULT          1    // "-", -0.5, 1.5, "dVth multiplier for Vgp_res; depmos3") // 20171015                                                                 MPRco
  `define WK_CO_UWE20171213        0    //"-","Calc Wres 20171213 Uwe depmos3")                                                                                                MPIsw
   // Constants 
     Vbi_DEP = Vbipn ;
     q_Ndepm = `C_QE * UC_NDEPM ;
     q_Ndepm_esi = `C_QE * UC_NDEPM * `C_ESI ;
     C_2ESIpq_Ndepm = 2.0 *`C_ESI/q_Ndepm ;
     NdepmpNsub  = UC_NDEPM / EF_NSUBC ;
     NdepmpNsub_inv1  = 1.0 / (1.0 + NdepmpNsub ) ;
     q_Ndepm_esi_Cox_inv2 = q_Ndepm_esi / ( Cox * Cox ) ;
     C2_q_Ndepm_esi_Cox_inv2 = 2.0 / q_Ndepm_esi_Cox_inv2 ;
     VgpDEP_pw  = 2 ;

     UC_CLM2 = (($param_given(CLM2)) ? UC_CLM2 : 5.0e9/(TNDEP * NDEPM)); 
     `Fn_SL_CP(UC_CLM2, UC_CLM2, 2.0, 0.1, 2, T0)

    // Initialization for hidden states
     phi_s0_DEP = 0.0 ;
     phi_sL_DEP = 0.0 ;
     Q_s0       = 0.0 ;
     Q_sL       = 0.0 ;
     Ps0DEP0    = 0.0 ;
     Vgp_res_raw = 0.0 ;

     Vbsc = Vbscl ; // Change Vbs vars to Vbsc(internal)

     //---------------------------------------------------*
     //* start of Ps0 calculation. (label)
     //*-----------------//
     Vgp = Vgs - Vfb + `WK_CO_DVGPACC*(dVth - dPpg) ; // 20171002
     //---------------------------------------------------*
     //* initial potential Ps0 , Pb0 and etc calculated.
     //*------------------//
     `Fn_SU( phi_s0_DEP_ini , Vgp , 0.3 , 0.01 , T0 )
     `Fn_SL( phi_s0_DEP_ini , phi_s0_DEP_ini , Vbsc - Vbi_DEP , 0.01 , T0 )
     phi_s0_DEP = phi_s0_DEP_ini ; 

     Vds_maxb0 = 0.0 ;
     phi_b0_DEP  = Vds_maxb0 ;
     phi_j0_DEP  = NdepmpNsub_inv1 * ( Vbsz - Vbi_DEP) ; 
     phi_j0_DEP0 = NdepmpNsub_inv1 * (      - Vbi_DEP) ;

     `Fn_SL_CP(T1, (- phi_j0_DEP) , 0, 1e-3, 2, T3) 
     W_bsub0  = sqrt(C_2ESIpq_Ndepm * T1 ) ;  // update for Tnp
     `Fn_SL_CP(T1, (- phi_j0_DEP0), 0, 1e-3, 2, T3) 
     W_bsub00 = sqrt(C_2ESIpq_Ndepm * T1 ) ;  // update for Tnp0

     // Intends to vary a landing of W_res to its maximum with Vds
     // Vds dependence of TNDEP
     // imported from depmos2  
     if( TNDEPV != 0.0 ) begin
       T1 = UC_DEPTHN * ( 1 - TNDEPV * Vds ) ;
       T2 = 1e-3 * UC_DEPTHN ;
       `Fn_SL(        T1 , T1 , 0.1 * UC_DEPTHN , T2 , T0 )
       `Fn_SU( UC_DEPTHN , T1 , 2.0 * UC_DEPTHN , T2 , T0 )
     end

     // calc. Tn and Tnp , wDEPSUBSL
     Tn   = UC_DEPTHN ;
     Tnp  = UC_DEPTHN - W_bsub0 ;
     Tnp0 = UC_DEPTHN - W_bsub00 ;
     `Fn_SL_CP(Tnp , Tnp , TNDEPMIN, TNDEPMIN*1e-1, 1, T0)  
     `Fn_SL_CP(Tnp0, Tnp0,        0, TNDEPMIN*1e-1, 1, T0)  
     wDEPSUBSL = (DEPSUBSL - DEPSUBSL0) * (Tnp / Tnp0) + DEPSUBSL0 ;

     /*                               */
     /* solve poisson at source side  */
     /*                               */

     flg_conv = 0 ;
     phi_j0_DEP_dPb = NdepmpNsub_inv1 * NdepmpNsub ;

     for ( lp_s0 = 1 ; lp_s0 <= `lp_se_max  ; lp_s0 = lp_s0 + 1 ) begin  // {

       phi_j0_DEP = NdepmpNsub_inv1 * (NdepmpNsub * phi_b0_DEP + Vbsc - Vbi_DEP) ;

       T1 = phi_b0_DEP - phi_j0_DEP ;
       `Fn_SL_CP(T1, T1, 0, 1e-3, 2, T2)
       W_bsub0 = sqrt(C_2ESIpq_Ndepm * T1) ;
       `Fn_SU_CP( W_bsub0 , W_bsub0 , Tn , 1e-12, 2 , T3 )
       T3 = T2 * T3 ;  

       phib_ref = phi_s0_DEP + (Tn*Tn + W_bsub0 * (W_bsub0 - 2 * Tn)) / C_2ESIpq_Ndepm ;
       phib_ref_dPs = 1.0 ;
       phib_ref_dPb = (T3 - Tn /W_bsub0 * T3 ) * (1.0 - phi_j0_DEP_dPb) ;

       `Fn_SU_CP( phib_ref , phib_ref , Vds_maxb0 , DEPFDPD, 4 , T0 )
       phib_ref_dPs = phib_ref_dPs * T0 ;
       phib_ref_dPb = phib_ref_dPb * T0 ;

       T5 = beta * ( phi_s0_DEP - phi_b0_DEP );
       T6 = exp( T5 ) ;
       T10 = T6 - 1.0 - T5 ;
       if( T5 >= `C_PHI_1_MINIMUM ) begin
         flg_zone = -1 ;
         Q_s0 = - cnst0 * sqrt( T10 ) ;
         T11 = 0.5 * cnst0 * cnst0 * beta / Q_s0 ;
         Q_s0_dPs = T11 * (   T6 - 1 ) ;
         Q_s0_dPb = T11 * ( - T6 + 1 ) ;
       end else if ( T5 < -`C_PHI_1_MINIMUM) begin //  T5 < -`C_PHI_1_MINIMUM
         flg_zone = 1 ;
         T8 = exp( - beta * (phi_s0_DEP - DEPRBR * Vbsc )) ;
         T9 = exp( - beta * (phi_b0_DEP - DEPRBR * Vbsc )) ;
         Q_s0 =  cnst0 * sqrt( T10 + cnst1 * ( T8 - T9 + T5 ) ) ;
         T11 = 0.5 * cnst0 * cnst0 * beta / Q_s0 ;
         Q_s0_dPs = T11 * (   T6 - 1 + cnst1 * ( - T8 + 1) ) ;
         Q_s0_dPb = T11 * ( - T6 + 1 + cnst1 * (   T9 - 1) ) ;
       end else if( T5 > 0 ) begin //  `C_PHI_1_MINIMUM > T5 > 0
         u = 0.5 * (1.0 + T5 * `C_1o3);
         usqr = sqrt(u);
         Q_s0 = -cnst0 * sqrt(u) * T5;
         Q_s0_dPs = -cnst0 *beta* ( usqr + T5/12.0/usqr );
         Q_s0_dPb = -Q_s0_dPs;
       end else begin              //   -`C_PHI_1_MINIMUM < T5 < 0
         u = 0.5 * (1.0 + T5 * `C_1o3);
         usqr = sqrt(u);
         Q_s0 = -cnst0 * sqrt(u) * T5;
         Q_s0_dPs = -cnst0 *beta* ( usqr + T5/12.0/usqr );
         Q_s0_dPb = -Q_s0_dPs;
       end // else: !if( T5 > 0 )

       if(flg_conv) begin
         lp_s0 = `lp_se_max + 1 ; // exit Newton Loop
       end else begin
         PF1  = Cox * (Vgp - phi_s0_DEP) + Q_s0 ;
         PF11 = - Cox + Q_s0_dPs ;
         PF12 = Q_s0_dPb ;

         PF2  = phi_b0_DEP - phib_ref ;
         PF21 = - phib_ref_dPs ;
         PF22 = 1.0 - phib_ref_dPb ;
   
         PDJ  = PF11 * PF22 - PF12 * PF21;
         PDJI = ( PDJ > 0.0 ) ? 1.0 / (PDJ + `Small) : 1.0 / (PDJ - `Small) ;
         PJI11 = PF22;
         PJI12 = - PF12;
         PJI21 = - PF21;
         PJI22 = PF11;
         dPs = -PDJI * (PJI11 * PF1 + PJI12 * PF2);
         dPb = -PDJI * (PJI21 * PF1 + PJI22 * PF2);
         T1 = abs(dPs);
         T1 = (T1 < abs(dPb)) ? abs(dPb) : T1 ;
         if (T1 > `dP_max) begin
           dPs = dPs * ( `dP_max / T1 );
           dPb = dPb * ( `dP_max / T1 );
         end
         if(T1 < `ps_conv2) begin
           flg_conv = 1 ; // break
         end

         phi_s0_DEP = phi_s0_DEP + dPs ;
         phi_b0_DEP = phi_b0_DEP + dPb ;

       end // End of flg_conv=1

     end // End of Newton Loop }

     if( flg_conv == 0 ) begin
       $write( "*** warning(HiSIM_HV(%m)): Went Over Iteration Maximum(Ps0)\n" ) ;
       $write( " Vbse   = %7.3f Vdse = %7.3f Vgse = %7.3f\n" ,Vbse , Vdse , Vgse ) ;
     end

     T5 = beta * ( phi_s0_DEP - phi_b0_DEP );
     T6 = exp( T5 );
     T10 = T6 - 1.0 - T5 + 1e-15 ;
     Q_n0 = ( T5 > 0 ) ? -cnst0 * sqrt( T10 ) : cnst0 * sqrt( T10 ) ; 

     if (`WK_CO_SMOOTHCHARGE_IDD == 1 ) begin
     `Fn_SL_CP( T2 , (phi_s0_DEP - Vds_maxb0) , 0.0, DEPPS, 6 , T0 )
     T4 = exp(beta * T2) - 1.0 - beta * T2 + 1e-15 ;
     Q_n0_cur = - cnst0 * sqrt(T4) ;

     end else begin
        Q_n0_cur = Q_n0;
     end
        
    
     //-----------------------------------------------------------*
     //* Start point of Psl(= Ps0 + Pds) calculation.(label)
     //*-----------------//

// start_of_Psl:
     if ( `WK_CO_VDSSATACC_1VARNT == 1 ) begin // 20171004
       // calculate Vdssat by 1D Newton
       Vgpsat = Vgp ;
       T4 = 1.0e0 + C2_q_Ndepm_esi_Cox_inv2 * ( Vgpsat ) ;
       T3 = (T4 > 0) ?  sqrt(T4) : -sqrt(-T4) ;
       Vdssat_ini = Vgpsat + q_Ndepm_esi_Cox_inv2 * ( 1.0 - T3 ) + DEPVSATA ; // 20171004 revert
       phi_vsat = Vdssat_ini ;
  
       flg_conv = 0 ;
  
       for ( lp_s0 = 1 ; lp_s0 <= `lp_se_max  ; lp_s0 = lp_s0 + 1 ) begin  // {
  
         T1 = - beta * phi_vsat ;
         T2 = exp(T1) ;
         T4 = sqrt(2.0*q_Ndepm_esi/beta) ;
         T10 = T2 - T1 - 1 ;
         Q_sat = T4 * sqrt( T10 + 1e-15 ) ;
         if( T1 > 0 ) Q_sat = - Q_sat ;
         T11 = 0.5 * T4 * T4 * beta / Q_sat ; 
         Q_sat_dPs = T11 * ( -T2 + 1 ) ;
  
         if(flg_conv) begin
           lp_s0 = `lp_se_max + 1 ; // exit Newton Loop
         end else begin
           PF1 = -Cox * (Vgpsat - phi_vsat + DEPVSATA ) + Q_sat ; // 20171004 revert
           PF11 = Cox + Q_sat_dPs ;
           dPs  = - PF1 / PF11 ;
  
           if(abs(dPs) < `ps_conv2) begin
              flg_conv = 1 ;
           end else if(dPs > `dP_max) begin
              dPs = `dP_max ; 
           end else if(dPs < - `dP_max) begin
              dPs = - `dP_max ; 
           end
  
           phi_vsat = phi_vsat + dPs ;
  
         end //  flg_conv == 0
       end // End of phi_vsat loop
  
       Vdssat1d = phi_vsat ;  
       // Smoothing for small Vds 
       `Fn_SZ(Vdssat , Vdssat1d , DEPQF, T0 )
        
     end else begin // analytical
       //T10 = `Small ; redundant?
       //T4 = 1.0e0 + C2_q_Ndepm_esi_Cox_inv2 * ( Vgp -  T10 ) ; // redundant?
       T4 = 1.0e0 + C2_q_Ndepm_esi_Cox_inv2 *  Vgp ;
       T5 = 1.0e0 + C2_q_Ndepm_esi_Cox_inv2 ;
       `Fn_SL_CP( T4 , T4 , 0, T5 , 2 , T0 )
       T3 = sqrt(T4) ;
       T10 = Vgp + q_Ndepm_esi_Cox_inv2 * ( 1.0 - T3 )  ;
       `Fn_SL_CP( Vdssat , T10 , 0.0 , DEPQF , 2 , T0 )
     end // else: !if( `WK_CO_VDSSATACC_1VARNT == 1 )
        
     //---------------------------------------------------*
     // calculate Vdseff //
     //*-----------------//
     Vdsorg = Vds ;
     // Smoothing for small Vds 
     //`Fn_SZ(Vdssat , Vdssat1d , DEPQF, T0 )
     T1 = Vds / Vdssat ;
     T2 = `Fn_Pow( T1 , DDLTe ) ;
     T3 = 1.0 + T2 ;
     T4 = `Fn_Pow( T3 , 1.0 / DDLTe ) ;
     Vdseff = Vds / T4 ;
     Vds = Vdseff ;


     //---------------------------------------------------*
     //* start of Psl calculation. (label)
     //*-----------------//

     if( Vds < 0.0 ) begin
 
       phi_sL_DEP = phi_s0_DEP ;
       phi_jL_DEP = phi_j0_DEP ;
       phi_bL_DEP = phi_b0_DEP ;

       Q_nL = Q_n0 ;
       Q_nL_cur = Q_n0_cur ;

     end else begin

       //---------------------------------------------------*
       //* initial potential Psl calculated.
       //*------------------//
       Vds_maxbL  = Vds ;

       `Fn_SU( phi_sL_DEP_ini , Vgp , phi_s0_DEP + Vds_maxbL , 0.01 , T0 )
       `Fn_SL( phi_sL_DEP_ini , phi_sL_DEP_ini , Vbsc - Vbi_DEP , 0.01 , T0 )

       phi_bL_DEP = Vds_maxbL ;
       phi_sL_DEP = phi_sL_DEP_ini ;

       /*                              */
       /* solve poisson  at drain side */
       /*                              */

       flg_conv = 0 ;
       phi_jL_DEP_dPb = NdepmpNsub_inv1 * NdepmpNsub ;

       for ( lp_sl = 1 ; lp_sl <= `lp_se_max  ; lp_sl = lp_sl + 1 ) begin  // {

         phi_jL_DEP  = NdepmpNsub_inv1 * (NdepmpNsub * phi_bL_DEP + Vbsc - Vbi_DEP) ;

         T1 = phi_bL_DEP - phi_jL_DEP ;
         `Fn_SL_CP(  T1, T1, 0, 1e-3, 2, T2)  
         W_bsubL = sqrt(C_2ESIpq_Ndepm * T1 ) ;                     
         `Fn_SU_CP( W_bsubL , W_bsubL , Tn , 1e-12, 2 , T3 )
         T3 = T2 * T3 ; 

         phib_ref = phi_sL_DEP + (Tn*Tn + W_bsubL * (W_bsubL - 2 * Tn)) / C_2ESIpq_Ndepm ;
         phib_ref_dPs = 1.0 ;
         phib_ref_dPb = (T3 - Tn /W_bsubL * T3 ) * (1.0 - phi_jL_DEP_dPb) ;

         `Fn_SU_CP( phib_ref , phib_ref , Vds_maxbL , DEPFDPD , 4 , T0 )
         phib_ref_dPs = phib_ref_dPs * T0 ;
         phib_ref_dPb = phib_ref_dPb * T0 ;

         T5 = beta * ( phi_sL_DEP - phi_bL_DEP );
         T6 = exp( T5 ) ;
         T10 = T6 - 1.0 - T5;
         if( T5 >= `C_PHI_1_MINIMUM ) begin
           flg_zone = -1 ;
           Q_sL = - cnst0 * sqrt( T10 ) ;
           T11 = 0.5 * cnst0 * cnst0 * beta / Q_sL ;
           Q_sL_dPs = T11 * (  T6 - 1);
           Q_sL_dPb = T11 * ( -T6 + 1);
         end else if ( T5 < -`C_PHI_1_MINIMUM) begin //  T5 < -`C_PHI_1_MINIMUM
           flg_zone = 1 ;
           T8 = exp( - beta * (phi_sL_DEP - DEPRBR * Vbsc) ) ;
           T9 = exp( - beta * (phi_bL_DEP - DEPRBR * Vbsc) ) ;
           Q_sL =  cnst0 * sqrt( T10 + cnst1 * ( T8 - T9 + T5 ) ) ;
           T11 = 0.5 * cnst0 * cnst0 * beta / Q_sL ;
           Q_sL_dPs = T11 * (   T6 - 1 + cnst1 * ( - T8 + 1) ) ;
           Q_sL_dPb = T11 * ( - T6 + 1 + cnst1 * (   T9 - 1) ) ;
         end else if( T5 > 0 ) begin                 //  `C_PHI_1_MINIMUM > T5 > 0
           u = 0.5 * (1.0 + T5 * `C_1o3);
           usqr = sqrt(u);
           Q_sL = -cnst0 * sqrt(u) * T5;
           Q_sL_dPs = -cnst0 *beta* ( usqr + T5/12.0/usqr );
           Q_sL_dPb = -Q_sL_dPs;
         end else begin
           u = 0.5 * (1.0 + T5 * `C_1o3);
           usqr = sqrt(u);
           Q_sL = -cnst0 * sqrt(u) * T5;
           Q_sL_dPs = -cnst0 *beta* ( usqr + T5/12.0/usqr );
           Q_sL_dPb = -Q_sL_dPs;
         end

         if(flg_conv) begin
           lp_sl = `lp_se_max + 1 ; // exit Newton Loop
         end else begin
           PF1  = Cox * (Vgp - phi_sL_DEP) + Q_sL ;
           PF11 = - Cox + Q_sL_dPs ;
           PF12 = Q_sL_dPb ;

           PF2  = phi_bL_DEP - phib_ref ;
           PF21 = - phib_ref_dPs ;
           PF22 = 1.0 - phib_ref_dPb ;

           PDJ  = PF11 * PF22 - PF12 * PF21;
           PDJI = ( PDJ > 0.0 ) ? 1.0 / (PDJ + `Small) : 1.0 / (PDJ - `Small) ;
           PJI11 = PF22;
           PJI12 = - PF12;
           PJI21 = - PF21;
           PJI22 = PF11;
           dPs = -PDJI * (PJI11 * PF1 + PJI12 * PF2);
           dPb = -PDJI * (PJI21 * PF1 + PJI22 * PF2);
           T1 = abs(dPs);
           T1 = (T1 < abs(dPb)) ? abs(dPb) : T1 ;
           if (T1 > `dP_max) begin
              dPs = dPs * ( `dP_max / T1 );
              dPb = dPb * ( `dP_max / T1 );
           end
           if(T1 < `ps_conv2) begin
              flg_conv = 1 ; // break
           end

           phi_sL_DEP = phi_sL_DEP + dPs ;
           phi_bL_DEP = phi_bL_DEP + dPb ;

         end // End of flg_conv=1

       end // End of Newton Loop }

       if( flg_conv == 0 ) begin
         $write( "*** warning(HiSIM_HV(%m)): Went Over Iteration Maximum(PsL)\n" ) ;
         $write( " Vbse   = %7.3f Vdse = %7.3f Vgse = %7.3f\n" ,Vbse , Vdse , Vgse ) ;
       end

       T5 = beta * ( phi_sL_DEP - phi_bL_DEP );
       T6 = exp( T5 ) ;
       T10 = T6 - 1.0 - T5 + 1e-15 ;
       Q_nL = ( phi_sL_DEP > phi_bL_DEP ) ? -cnst0 * sqrt( T10 ) : cnst0 * sqrt( T10 ) ; 

       if (`WK_CO_SMOOTHCHARGE_IDD == 1 ) begin
         `Fn_SL_CP( T2 , (phi_sL_DEP - Vds_maxbL) , 0.0, DEPPS, 6 , T0 )
         T4 = exp(beta * T2) - 1.0 - beta * T2 + 1e-15 ;
         Q_nL_cur = - cnst0 * sqrt(T4) ;
       end else begin
         Q_nL_cur = Q_nL; 
       end
        
     end // Vds > 0

     //---------------------------------------------------*
     //* Assign Pds.
     //*-----------------//
     Ps0 = phi_s0_DEP ;
     Psl = phi_sL_DEP ;
     Pds = phi_sL_DEP - phi_s0_DEP ;

     //-----------------------------------------------------------*
     //* Modified potential and Q_n0 for symmetry.
     //*-----------------//
     T1 = ( Vds - Pds ) / 2 ;
     `Fn_SymAdd( Pzadd , T1 , PZADD0*0.1 , T2 )
`ifdef REPLACE_CLIPPING_WITH_SMOOTHING //revised for continuity (Qn0)
     `Fn_SL_CP(Pzadd, Pzadd, `epsm10, `epsm10, 2, T0)
`else
     if( Pzadd < `epsm10 ) begin
         Pzadd = `epsm10 ;
     end
`endif
     Ps0z = Ps0 + Pzadd ;

     // Q_n0_sym for Symmetry
     //`Fn_SZ( T2 , (Ps0z - Vds_maxb0) , DEPPS, T0 )
     `Fn_SL_CP( T2, (Ps0z - Vds_maxb0), 0.0, DEPPS, 6, T0)
     T4 = exp(beta * T2) - 1.0 - beta * T2 + 1e-15 ;
     Q_n0_sym = - cnst0 * sqrt(T4) ;
     

     //---------------------------------------------------*
     // Calc. Idd
     //*-----------------//

`ifdef REPLACE_CLIPPING_WITH_SMOOTHING  
     `Fn_SZ(sum_Q_nx_cur, - (Q_nL_cur + Q_n0_cur), `C_sum_Q_nx_cur_delta, T1)
     sum_Q_nx_cur = - sum_Q_nx_cur;
`else
     sum_Q_nx_cur = (Q_nL_cur + Q_n0_cur);
     if( - sum_Q_nx_cur < `epsm10 )  sum_Q_nx_cur = 0.0;
`endif
     Idd = - beta * sum_Q_nx_cur / 2.0 * Pds ;

     //---------- End of Idd ------------*

     Qn0 = - Q_n0_sym ;
     Lch = Leff ;

     // Vdseff //
     Vds = Vdsorg;

     //-----------------------------------------------------------*
     //* Channel Length Modulation. Lred: \Delta L
     //*-----------------//
     if( UC_CLM2 < `epsm10 && UC_CLM3 < `epsm10  ) begin 
       Lred = 0.0e0 ;
       Psdl = Psl ;
`ifdef REPLACE_CLIPPING_WITH_SMOOTHING //revised for continuity (Psdl)
       `Fn_SU_CP(Psdl, Psdl, (Ps0 + Vds - `epsm10), `epsm10, 2, T0)
`else
       if( Psdl > Ps0 + Vds - `epsm10 ) begin
           Psdl = Ps0 + Vds - `epsm10 ;
       end
`endif
     end else begin
       //T8 = Psl - Vbs ;
       T8 = Psl - Vbsc ;
       //`Fn_SZ(T8, T8,  1e-3, T0)      
       `Fn_SL_CP(T8, T8, 0.2, (-Vbsc+0.8), 1, T0)
       T9 = C_2ESIpq_Ndepm*(T8); 
       Wd = sqrt(T9);
       T0 = 1.0 / Wd ;
       T1 = Qn0 * T0 ;
       T2 = UC_CLM3 * T1 ;
       T3 = UC_CLM3 * T0 ;
       T5 = UC_CLM2 * q_Ndepm + T2 ;
       T1 = 1.0 / T5 ;
       T4 = `C_ESI * T1 ;
       T1 = (1.0e0 - UC_CLM1) ;
       Psdl = UC_CLM1 * (Vds + Ps0z) + T1 * Psl ;
`ifdef REPLACE_CLIPPING_WITH_SMOOTHING //revised for continuity (Psdl and Lred)
       `Fn_SU_CP(Psdl, Psdl, (Ps0z + Vds - `epsm10), `epsm10, 2, T0)
`else
       if( Psdl > Ps0z + Vds - `epsm10 ) begin
           Psdl = Ps0z + Vds - `epsm10 ;
       end
`endif
       T6 = Psdl - Psl ;
       T3 = beta * Qn0 ;
       T1 = 1.0 / T3 ;
       T5 = Idd * T1 ; //  Iaccum only; no contribution of Ires-based Idd
       T10 = q_Ndepm / `C_ESI ;
       T1 = 1.0e5 ;
       T2 = 1.0 / Leff ;
       T11 = (2.0 * T5 + 2.0 * T10 * T6 * T4 + T1 * T4) * T2 ;
       T7 = T11 * T4 ;
       T11 = 4.0 * (2.0 * T10 * T6 + T1) ;
       T8 = T11 * T4 * T4 ;
       T9 = sqrt (T7 * T7 + T8);
       Lred = 0.5 * (- T7 + T9) ;
        
     //---------------------------------------------------*
     //* Modify Lred for symmetry.
     //*-----------------//
       T1 = Lred ;
       Lred = FMDVDS * T1 ;
     end        
          
     // CLM5 & CLM6 //
     Lred = Lred * clmmod ;
 
     Lch = Lch - Lred ;

     //-----------------------------------------------------------*
     //* Muun : surface universal mobility for Iacc.  (CGS unit)
     //*-----------------//
     T2 = ninv_o_esi / `C_m2cm ; // [m/F]*(1e-2) = [cm/F]*(1e-4)
     T0 = Ninvde ;
     Pdsz = sqrt(Pds * Pds + VZADD0) - sqrt(VZADD0) ;
     T4 = 1.0 + (Pdsz) * T0 ;   // Pdsz for DC symmetry

     T5 = T2 * Qn0 ;  // Note Qn0 in [C/m2]=[C/cm2]*(1e4);  [cm/F]*(1e-4)*[C/cm2]*(1e4) = [C/F]][1/cm]
     T3 = T5 / T4 ;   // [C/F][1/cm]/[1] = [V/cm]
     Eeff = T3 ;      // [V/cm]
     T8 = `Fn_Pow( Eeff , MUEPH0 ) ;
     T6 = `Fn_Pow( Eeff , muesr ) ;
     T9 = `C_QE * `C_m2cm_p2 ; //  [C]*(1e4)
     Rns = Qn0 / T9 ;  // [C/m2]/[C]/(1e4) = [1/cm2]

     T2 = UC_MUECB0 ;
     T1 = `WK_CO_MUECB* 1.0e0 / ( T2 + UC_MUECB1 * T4 * Rns / 1.0e11 )
        + `WK_CO_MUEPH* mphn0 * T8 + `WK_CO_MUESR*T6 / UC_MUESR1 ;     // [cm2/V/s][1/cm2]/[1/cm2]=[cm2/V/s]

     Muun = 1.0e0 / T1 ;


     //  Change to MKS unit //
     Muun = Muun / `C_m2cm_p2 ;  // [cm2/V/s] -> [m2/V/s]

     //-----------------------------------------------------------*
     //* Mu : mobility
     //*-----------------//
     T2 = beta * (Qn0 + `Small) * Lch ;
     T1 = 1.0e0 / T2 ;
     TY = Idd * T1 ;
     T2 = 0.2 * Vmaxe / Muun ;
     Ey = sqrt( TY * TY + T2 * T2 ) ;
     T4 = 1.0 / Ey ;
     Em = Muun * Ey ;
     T1 = Em / Vmaxe ;
     // note: bb = 2 (electron) ;1 (hole) //
     if( 1.0e0 - `epsm10 <= BB && BB <= 1.0e0 + `epsm10 ) begin
       T2 = T1;
     end else if( 2.0e0 - `epsm10 <= BB && BB <= 2.0e0 + `epsm10 ) begin
       T2 = T1*T1;
     end else begin
       T2 = `Fn_Pow( T1 , BB ) ;
     end
     T4 = 1.0e0 + T2 ;
     if( 1.0e0 - `epsm10 <= BB && BB <= 1.0e0 + `epsm10 ) begin
       T5 = 1.0 / T4 ;
     end else if( 2.0e0 - `epsm10 <= BB && BB <= 2.0e0 + `epsm10 ) begin
       T5 = 1.0 / sqrt( T4 ) ;
     end else begin
       T5 = `Fn_Pow( T4 , ( - 1.0e0 / BB ) ) ;
     end
     Mu = Muun * T5 ;
     Mu_acc = Mu ;
     Ey_acc = Ey ;

 begin : Calc_W_res_with_DD_mode
    //1D Newton specific for calculating W_res

    real dphi_sb , c_sb , Q_s0 , lp_ws , Vgp_ws , Vgp_leak ;

    Vgp_ws = 0.0 ; //protection against hidden state
    W_res_leak = 0.0 ;
    W_res = 0.0 ;
    Ws = 0.0 ;
    Q_s0 = 0.0 ;
    Vgpz    = Vgsz - Vfb + dVth - dPpg ;
    if ( `WK_CO_NEWVGPRES == 0 ) begin 
      Vgp_res = Vgpz - DEPDVFBC ; // original
    end else if ( `WK_CO_NEWVGPRES == 1) begin
      Vgp_res = Vgsz - Vfb - DEPDVFBC ; // 20170902
    end else begin
      Vgp_res = Vgp - DEPDVFBC ; // 20171002
    end

    if( Tnp == 0 ) begin
      Ps0_res = 0.0 ;
    end else begin

      Ps0DEP  = Ps0 ;
      depvbs  = DEPVLEAK*Vbsc ;
      Vgp_leak = (Vfb + DEPDVFBC) - 3.0 ; // Vgp_leak for W_res_leak

      for ( lp_ws = 1 ; lp_ws <= 2 ; lp_ws = lp_ws + 1 ) begin 
        case (lp_ws)
        1: begin // 
             depvbs = DEPVLEAK*Vbsc ;
             Ps0DEP = depvbs - 1 ;
             Vgp_ws = Vgp_leak ;
             Vgp_res_raw = Vgp_leak;
           end
        2: begin // 
             depvbs = DEPVLEAK*Vbsc - 0.1 ;
             Ps0DEP = Ps0 ;
             Vgp_ws = Vgp_res ;
             Vgp_res_raw = Vgp_res;   
           end
        endcase // case (lp_ws)

        flg_conv = 0 ;

        for ( lp_s0 = 1 ; lp_s0 <= `lp_se_max ; lp_s0 = lp_s0 + 1 ) begin

          T1 = beta * Ps0DEP ;
          T2 = exp(T1) ;
          if( Ps0DEP >= 0.0 ) begin
            Q_s0 = - cnst0 * sqrt(T2 - 1.0 - T1 + 1e-15) ;
            Q_s0_dPs = 0.5 * cnst0 * cnst0 / Q_s0 * (beta * T2 - beta ) ;
          end else begin
            T3 = exp( - beta * (Ps0DEP - depvbs) ) ; 
            T4 = exp(   beta *           depvbs  ) ; 
            Q_s0 = cnst0 * sqrt(T2 - 1.0 - T1 + cnst1 * (T3 - T4) + 1e-15) ;
            T5 = 0.5 * cnst0 * cnst0 / Q_s0 ;
            Q_s0_dPs = T5 * (beta * T2 - beta + cnst1 * ( - beta * T3) ) ;
          end

          if(flg_conv) begin
            lp_s0 = `lp_se_max + 1 ; // exit Newton Loop
          end else begin
            PF1  = Cox * (Vgp_ws - Ps0DEP) + Q_s0 ;
            PF11 = - Cox + Q_s0_dPs ;
            dPs  = - PF1 / PF11 ;
            if(abs(dPs) < `ps_conv2*100) begin
              flg_conv = 1 ;
            end else if(dPs > `dP_max) begin
              dPs = `dP_max ;
            end else if(dPs < - `dP_max) begin
              dPs = - `dP_max ;
            end

            Ps0DEP = Ps0DEP + dPs ;

          end // End of flg_conv=1

        end // End of Newton Loop


        if( flg_conv == 0 ) begin
          $write( "*** warning(HiSIM_HV(%m)): Went Over Iteration Maximum(Pres)\n" ) ;
          $write( " Vbse   = %7.3f Vdse = %7.3f Vgse = %7.3f\n" ,Vbse , Vdse , Vgse ) ;
        end

        case (lp_ws)
        1: begin // 
             Ps0DEP0 = Ps0DEP ;
           end
        2: begin // 
             `Fn_SL_CP( Ps0DEP , Ps0DEP , Ps0DEP0 , 0.2 , 2 , T0 ) 
           end
        endcase // case (lp_ws)
        Ps0_res = Ps0DEP ; // save to Ps0_res; 
        // As all sorts of charge were taken into account for
        // the solution of Poisson equation, Ps0_res does not serve right for estimation of Vdssat where mobile carriers
        // are depleted. A separate iteration loop will be setup for this purpose although it should look alike to the codes for the
        // Vdssat1d calculation.  Vgp_res and Vgp can be different by a small amount due to Vgs or Vgsz and due to dVth-dPpg.  

        // calc. the pseudo Pb0DEP in FD condition 
        VgpDEP_dlt = `Fn_Max(1E-6,DEPVGPSL) ; 
        `Fn_SU_CP( Ps0DEP , Ps0_res , 0.0 , VgpDEP_dlt , VgpDEP_pw , T0 )  // for Wres
        Ps0DEP = - Ps0DEP ; 
        dphi_sb = q_Ndepm * Tnp * Tnp / 2.0 / `C_ESI ; 
        T0 = wDEPSUBSL * sqrt(2.0 * beta * dphi_sb) ; 
        T1 = (exp(T0) + exp(-T0)) / 2 ; //  greater or equal to 1; positive
        if ( abs(T0) > 1e-4 ) begin
          c_sb = ln(T1) / dphi_sb ;  // positive
        end else begin
          // ln(cosh(T0)) = (1/2)*T0^2-(1/12)*T0^4; dphi_sb 
          //              = (1/2)*(1/beta)*(1/DEPSUBSL)^2 * T0^2; to avoid 0/0 indefinite evaluation
          c_sb = wDEPSUBSL*wDEPSUBSL*beta*(1-`C_1o6*T0*T0);
        end
        TX = c_sb * Ps0DEP ;
        if ( TX > 500 ) begin // bypass ln(1+exp(big))
          Pb0DEP = Ps0DEP - dphi_sb;
        end else begin
          T0 = exp(- c_sb * dphi_sb) ; 
          if (abs(TX) > 1E-8) begin
            `Fn_DExp(T1,TX,T3)
            T1 = T1 * T0 ;
            T2 = T1 - T0 ; 
          end else begin
             T1 = (1 + TX) * T0 ;
             T2 = TX * (1 + TX / 2) * T0 ;
          end
          if (abs(T2) > 1E-8) begin
             Pb0DEP = ln(1 + T2) / c_sb ; 
          end else begin
             Pb0DEP = T2 / c_sb ;
          end
        end

        // callc. Ws and Wres
        T2 = Ps0DEP - Pb0DEP ;
        if (`WK_CO_UWE20171213 == 0) begin  // original implementation as of 20171212 
          Ws = ( T2 < 0 ) ? - sqrt(-C_2ESIpq_Ndepm*T2) : sqrt(C_2ESIpq_Ndepm*T2) ; // Ws
        end else begin
          if ( T2 < 0 ) begin
             T3 = beta * T2;
             Ws = -sqrt((C_2ESIpq_Ndepm*beta_inv)*(exp(T3)-T3-1.0));
          end
          else begin
             T3 = -beta* T2;
             Ws =  sqrt((C_2ESIpq_Ndepm*beta_inv)*(exp(T3)-T3-1.0));
          end  
        end
        W_res = Tnp - Ws ; // equivalent to Tn - ( W_bsub0 + W_b0_new ) ;
        `Fn_SL_CP( W_res  , W_res  , 0 , 1e-16 , 2 , T0 )  
         
        if( lp_ws == 1 ) begin
           W_res_leak = W_res ;
        end

      end // End of for ( lp_ws=1-2 )
//=======
//        // callc. Ws and Wres
//        T2 = Ps0DEP - Pb0DEP ;
//        Ws = ( T2 < 0 ) ? - sqrt(-C_2ESIpq_Ndepm*T2) : sqrt(C_2ESIpq_Ndepm*T2) ; // Ws
//        W_res = Tnp - Ws ; // equivalent to Tn - ( W_bsub0 + W_b0_new ) ;
//        `Fn_SL_CP( W_res  , W_res  , 1e-16 , 1e-16 , 2 , T0 )  
//         
//        if( lp_ws == 1 ) begin
//           W_res_leak = W_res ;
//        end
//
//      end // End of for ( lp_ws=1-2 )
//
//      if (`CO_WK_NEWWRESSCHEME == 1) begin // 20171003 kiku, HSMHV_depmos3_Wres_new.inc
//       begin : W_res_new
//        real Ws_org , Ws_dlt , Q_s0_max , Q_s0_smo ;
//      
//        //T1 = beta * 0.05 ; // beta * Ps0DEP
//        T1 = beta * `WK_WSDLT_PS0MAX ; // beta * Ps0DEP 20171004
//        T2 = exp(T1) ;
//        Q_s0_max = cnst0 * sqrt(T2 - 1.0 - T1 + 1e-15) ;
//        //`Fn_SL_CP( Q_s0_smo  , Q_s0_res  , -Q_s0_max , Q_s0_max , 1 , T0 ) // Q_s0_res local within HSMHV_depmos3. Use this
//        // when block W_res_new will be placed outside the block Calc_W_res_with_DD_mode.   
//        //`Fn_SL_CP( Q_s0_smo  , Q_s0  , -Q_s0_max , Q_s0_max , 1 , T0 ) // Q_s0 local within Calc_W_res_with_DD_mode.
//        `Fn_SL_CP( Q_s0_smo  , Q_s0  , -Q_s0_max , Q_s0_max* `WK_QS0MAX_DLT , 1 , T0 ) // Q_s0 local within Calc_W_res_with_DD_mode.
//        //`Fn_Sigmoid( T1, (Vgp_res+0.5)*10 )
//        `Fn_Sigmoid( T1, (Vgp_res+ `WK_WSDLT_VGPRESSHIFT)*`WK_WSDLT_VGPRESMULT ) // 20171004
//        Ws_org = Ws ;
//        Ws_dlt = Ws_org * T1 * 0.1 ;
//        Ws = Q_s0_smo / q_Ndepm ;
//        `Fn_SU_CP( Ws  , (Ws*1.05)  , Ws_org ,   Ws_dlt , 2 , T0 )
//      
//        W_res = Tnp - Ws  ;
//        `Fn_SL_CP( W_res  , W_res  , 1e-16 , 1e-16 , 2 , T0 )
//>>>>>>> 1.12
//
//       end // block: W_res_new
//      end // if (`CO_WK_NEWWRESSCHEME == 1)
 
    end // else: !if( Tnp == 0 )
 
 end // block: Calc_W_res_with_DD_mode

     //-----------------------------------------------------------*
     //*  Vds for resistor region current. 
     //*-----------------//

  if (`CO_WK_VDSATRES1VARNT == 0) begin  // 20170929
  begin :  Calc_Vdsatres_analytical // 20170929 import from hisim2 300b5
     Vds_res = Vdsorg ;
       T10 = ( Vbsc + beta_inv ) * DEPVSATR  ;
       T4 = 1.0e0 + C2_q_Ndepm_esi_Cox_inv2 * (Vgp - T10) ;
       T5 = 1.0e0 + C2_q_Ndepm_esi_Cox_inv2 ;
       `Fn_SL_CP( T4 , T4 , 0, T5 , 2 , T0 )
       T3 = sqrt(T4) ;
       T10 = Vgp + q_Ndepm_esi_Cox_inv2 * ( 1.0e0 - T3 )  ;
       `Fn_SL_CP(  Vdssat_res , T10 , UC_DEPLEAK, DEPQFRES, 2 , T0 )
  end   
   
  end else begin
  begin :  Calc_Vdsatres_1varNT  // 20170929
     // Vgp_res can be slightly different from Vgp for acc.
     // calculate Vdssat by 1D Newton
     Vgpsat = Vgp_res ;
     T4 = 1.0e0 + C2_q_Ndepm_esi_Cox_inv2 * ( Vgpsat ) ;
     T3 = (T4 > 0) ?  sqrt(T4) : -sqrt(-T4) ;
     Vdssat_ini = Vgpsat + q_Ndepm_esi_Cox_inv2 * ( 1.0 - T3 ) ;
     phi_vsat = Vdssat_ini ;

     flg_conv = 0 ;

     for ( lp_s0 = 1 ; lp_s0 <= `lp_se_max  ; lp_s0 = lp_s0 + 1 ) begin  // {

       T1 = - beta * phi_vsat ;
       T2 = exp(T1) ;
       T4 = sqrt(2.0*q_Ndepm_esi/beta) ;
       T10 = T2 - T1 - 1 ;
       Q_sat = T4 * sqrt( T10 + 1e-15 ) ;
       if( T1 > 0 ) Q_sat = - Q_sat ;
       T11 = 0.5 * T4 * T4 * beta / Q_sat ; 
       Q_sat_dPs = T11 * ( -T2 + 1 ) ;

       if(flg_conv) begin
         lp_s0 = `lp_se_max + 1 ; // exit Newton Loop
       end else begin
         PF1 = -Cox * (Vgpsat - phi_vsat) + Q_sat ;
         PF11 = Cox + Q_sat_dPs ;
         dPs  = - PF1 / PF11 ;

         if(abs(dPs) < `ps_conv2) begin
            flg_conv = 1 ;
         end else if(dPs > `dP_max) begin
            dPs = `dP_max ; 
         end else if(dPs < - `dP_max) begin
            dPs = - `dP_max ; 
         end

         phi_vsat = phi_vsat + dPs ;

       end //  flg_conv == 0
     end // End of phi_vsat loop
  end // block: Calc_Vdsatres_1varNT 20170929
    
    
     //Vdssat1d = phi_vsat ;
     Ps0_res = phi_vsat; // 20170929
    
    
     Vds_res = Vdsorg ;
     `Fn_SZ( Vdssat_res , Ps0_res , DEPQFRES, T0 )
  end // else: !if(`CO_WK_VDSATRES1VARNT == 0)

    
    
     T1 = Vds_res / Vdssat_res ;
     T2 = `Fn_Pow( T1 , DEPDDLT-1.0 ) ;
     T3 = 1.0 + T2 * T1 ;
     T4 = `Fn_Pow( T3 , 1.0 / DEPDDLT-1.0 ) ;
     T6 = T4 * T3  ;

     `Fn_SU_CP( Vds_res0 , Vdsorg , UC_DEPLEAK, UC_DEPLEAK*0.5 , 2 , T0 )
     Vds_res = Vds_res / T6 + Vds_res0 ;
    
    
     // Vds_res0_sym for Symmetry
     T4 = Vds_res0 * Vds_res0 * Vds_res0 ;
     T0 = T4 + `C_leak_delta ;
     Vds_res0_sym = T4 / T0 ;

     if ( COVDSRES == -1) begin // Vdsres=Vds as in hisim2 300
        Vds_res0_sym = 0.0;
        Vds_res = Vdsorg;
     end
     else begin        
        if ( COVDSRES == 2) begin // 20180429
           Vds_res = Vdsorg;
           Vds_res0 = 0.0;
           Vds_res0_sym = 0.0;

           T1 = Vgp_res_raw - UC_DEPLEAK;
           Vdssat_res= 0.5*( T1 + ln(cosh(T1)) + ln(2.0) ) + UC_DEPLEAK;
         end
         else if ( COVDSRES == 3) begin // 20180526
              Vdssat_res = ln(1+exp(Vgp_res_raw - UC_DEPLEAK)) +UC_DEPLEAK;
         end 
      
           T1 = Vds_res / Vdssat_res ;
           T2 = `Fn_Pow( T1 , DEPDDLT-1.0 ) ;
           T3 = 1.0 + T2 * T1 ;
           T4 = `Fn_Pow( T3 , 1.0 / DEPDDLT-1.0 ) ;
           T6 = T4 * T3  ;
           Vds_res = Vds_res / T6 + Vds_res0 ;
     end // else: !if( COVDSRES == -1)

     //  resistor region universal mobility for Ires.  (CGS unit)

     Qn_res = W_res * q_Ndepm ; // (1/m^2) Used for Coulomb scat mobility for resistor
    
     T9 = `C_QE * `C_m2cm_p2 ;  //
     Rns = Qn_res / T9 ;        // (1/cm^2)
    
     // calc. Eeff,res 
     // Ninvdec,hRES for high-field correction for universal mobility; DEPNINVDC,H
     // Vds_resz for DC symmetry; 
     Vds_resz = sqrt(Vds_res * Vds_res + VZADD0) - sqrt(VZADD0) ; 
     T4 = 1.0 + Vds_resz * NinvdecRES ;   
     T5 = 1.0 + Vds_resz * NinvdehRES ;
     if ( $param_given(DEPPB0)) begin
        Eeff_res = ( DEPPB0 - phi_b0_DEP ) / (`C_m2cm *UC_DEPTHN) / T5; // (HiSIM2 3.0.0 compatible)
     end
     else begin // when DEPPB0 is not supplied.
        Eeff_res = Qn_res / `C_ESI / T5 ; // (same as HiSIM2 3.1.0 or later)
     end   

     T8 = `Fn_Pow( Eeff_res, DEPMUEPH0 ); // phonon scat mobility
     T1 = `WK_CO_DEPMUE * 1.0e0 / (UC_DEPMUE0 + UC_DEPMUE1 * T4 * Rns / 1.0e10 + `Small ) + `WK_CO_DEPMUEPH * depmphn0 * T8;
     Muun = 1.0 / T1 ;
     //  Change to MKS unit //
     Muun = Muun / `C_m2cm_p2 ; // 2nd term revived. 

     // for Symmetry */
     Edri = Vds_res  / (Leff+DEPRDRDL1) ; 
     `Fn_Sym4( T0 , Vds_res , 0.01 )  /* Edri for Symmetry */
     Edri2 = T0 / (Leff-DEPRDRDL2) ; 
     T1 = Muun * Edri2 / UC_DEPVMAX;
     T2 = `Fn_Pow(T1,DEPBB) ;
     T3 = 1.0 + T2 ;
     T4 = `Fn_Pow(T3,1.0 / DEPBB) ;
     Mu_res = Muun / T4 ;
     //  Modification of NDEPM and Qn_dep
     N_res  = UC_NDEPM * ( 1.0 + DEPCAR * Edri  * (1.0 - 1.0 / ( 1.0 + Muun * Edri  / UC_DEPVMAX ))) ;

     //-----------------------------------------------------------*
     //* Ids0: Ids_acc + Ids_res + Ires_leak .
     //*-----------------//
     T1 = W_res * `C_QE  * N_res ;
     T2 = pow( Weff / Leff , UC_DEPWLP ) * NF ;
     Ids_res   = weff_nf * T1 * Mu_res  * Edri ;
     Ires_leak = T2 * W_res_leak * DEPJLEAK * Vds_res0_sym ;

     betaWL = weff_nf * beta_inv / Lch ;
     Ids_acc =  betaWL * Idd * Mu_acc ;
     Ids0 = `WK_IACC*Ids_acc + `WK_IRES*Ids_res + `WK_ILEAK*Ires_leak ;

     // Vdseff //
     Vds = Vdsorg;

     //-----------------------------------------------------------*
     //* Adding parasitic components to the channel current.
     //*-----------------//
     if( PTL != 0 ) begin
       T1 =  0.5 * ( Vds - Pds ) ;
       `Fn_SymAdd( T6 , T1 , 0.01 , T2 )
       T1 = 1.1 - ( phi_s0_DEP + T6 );
       `Fn_SZ( T2 , T1 , 0.05 , T0 )
       T2 = T2 + `Small ;
       T0 = beta * ptl0 ;
       T3 = Cox * T0 ;
       T0 = pow( T2 , PTP ) ;
       T9 = T3 * T0 ;
       T4 = 1.0 + Vdsz * PT2 ;
       T0 = pt40 ;
       T5 = phi_s0_DEP + T6 - Vbsz ;
       T4 = T4 + Vdsz * T0 * T5 ;
       T6 = T9 * T4 ;
       T9 = T6 ;
     end else begin
       T9 = 0.0 ;
     end
     if( GDL != 0 )begin
       T1 = beta * gdl0 ;
       T2 = Cox * T1 ;
       T8     = T2 * Vdsz ;
     end else begin
       T8 = 0.0 ;
     end
     if(( T9 + T8 ) > 0.0 ) begin
       Idd1 = Pds * ( T9 + T8 ) ;
       Ids0 = Ids0 + betaWL * Idd1 * Mu ;
     end 
       Ids = Ids0 ;

     /* charge calculation */

     //---------------------------------------------------*
     //* Qbu : -Qb in unit area.
     //*-----------------//
     Qbu = - 0.5 * (Q_s0 - Q_n0 + Q_sL - Q_nL);
     Qiu = - 0.5 * (Q_n0 + Q_nL );
     Qdrat = 0.5;

     //---------------------------------------------------*
     //set flg_noqi & Qiu_noi
     //---------------------------------------------------*
     Qiu_noi = - 0.5 * (Q_n0 + Q_nL) ;
     Qn0 = -Q_n0 ;
     Ey = Ey_acc ;
     if( Qn0 < `Small || Qiu < `Small ) begin
`ifdef REPLACE_CLIPPING_WITH_SMOOTHING //removed for continuity (Qn0)
`else
       Qn0 = `Small ;
`endif
       flg_noqi = 1 ;
     end

//`include "dbgprn3.cnt"

 end // HSMHV_depmos3

