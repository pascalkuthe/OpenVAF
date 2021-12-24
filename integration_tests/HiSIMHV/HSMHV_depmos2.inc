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
 FILE : vaFiles/HSMHV_depmos2.inc 

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
 begin : HSMHV_depmos2
   //*------------------//
   // 
   real W_b0, W_bL, W_sub0, W_subL ;
   real Tnp ;
   real Wbsub , W_bsub0, W_bsubL ;
   real kjuncpn_Vbs_Vbi , phib_ref , phib_ref_dPs , phib_ref_dPb ; 
   real phi_j0_DEP_dPb , phi_jL_DEP_dPb ;
   real Q_b0_dep_dPb, Q_bL_dep_dPb, Q_b0_dep_dPd, Q_bL_dep_dPd, Q_sub0_dep_dPd, Q_subL_dep_dPd ;
   real sm_delta ;

   real Vds_maxb0, Vds_maxbL ;
   real phi_s0_DEP, phi_sL_DEP , Vbi_DEP ;
   real phi_b0_DEP, phi_bL_DEP ;
   real phi_j0_DEP, phi_jL_DEP ;
   real phi_s0_DEP_ini, phi_sL_DEP_ini ;

   real Q_s0, Q_sL, Q_sub0, Q_subL ;
   real Q_b0_dep, Q_sub0_dep, Q_bL_dep, Q_subL_dep ;

   real PF1 , PF11 , PF12 , PF2 , PF21 , PF22 ;
   real PDJ , PDJI , PJI11 , PJI12 , PJI21 , PJI22 , dPs , dPb ;

   real q_Ndepm_esi, Edri ;
   real Ey_acc , Vbsc , phi_j0 ;

   real Q_n0_cur , Q_nL_cur , Q_n0_sym ; 
   real Q_n0 , Q_nL , Qn_drift ;
   real Q_s0_dep, Q_sL_dep ;
   
   real Q_s0_dPs, Q_sL_dPs ;
   real Q_s0_dPb, Q_sL_dPb ;
   real NdepmpNsub_inv1, NdepmpNsub ;

   real Tn2 ; 
   real q_Nsub, q_Ndepm , q_Ndepm_esi_Cox_inv2 , C2_q_Ndepm_esi_Cox_inv2 ; 
   real C_2ESIpq_Ndepm ; 
   real C_2ESIpq_Nsub ; 
   real DEPQFN_dlt , Ps_delta ;
   real VFBOFFSET , VgpDEP_dlt;
   integer VgpDEP_pw ;
   real Vgp_res , vfb_res ;

   // Constants 
     Vbi_DEP = Vbipn ;
     q_Ndepm = `C_QE * UC_NDEPM ;
     q_Ndepm_esi = `C_QE * UC_NDEPM * `C_ESI ;
     q_Nsub = `C_QE * EF_NSUBC ;
     Tn2    = UC_DEPTHN * UC_DEPTHN ;
     C_2ESIpq_Ndepm = 2.0 *`C_ESI/q_Ndepm ;
     C_2ESIpq_Nsub  = 2.0 * `C_ESI / q_Nsub  ;
     NdepmpNsub  = UC_NDEPM / EF_NSUBC ;
     NdepmpNsub_inv1  = 1.0 / (1.0 + NdepmpNsub ) ;
     q_Ndepm_esi_Cox_inv2 = q_Ndepm_esi / ( Cox * Cox ) ;
     C2_q_Ndepm_esi_Cox_inv2 = 2.0 / q_Ndepm_esi_Cox_inv2 ;
     DEPQFN_dlt = 4 ;
     Ps_delta   = 0.1 ;  
     VFBOFFSET  = 0.1 ;
     VgpDEP_dlt = Pb2n + DEPVGPSL ;
     VgpDEP_pw  = 3 ;

    // Initialization for hidden states
     phi_s0_DEP = 0.0 ;
     phi_sL_DEP = 0.0 ;
     Q_s0       = 0.0 ;
     Q_sL       = 0.0 ;
     Q_s0_dep   = 0.0 ;
     Q_sL_dep   = 0.0 ;
     Q_b0_dep   = 0.0 ;
     Q_bL_dep   = 0.0 ;
     Q_sub0_dep = 0.0 ;
     Q_subL_dep = 0.0 ;
     phib_ref   = 0.0 ;
     Tnp        = 0.0 ;

     Vbsc = Vbscl ; // Change Vbs vars to Vbsc(internal)

     //---------------------------------------------------*
     //* start of Ps0 calculation. (label)
     //*-----------------//
     Vgp = Vgp + `epsm10 * 1e8 ; // Avoid the numerical noise

     // Calc. Wb of P-N junction 
     kjuncpn = C_2ESIpq_Ndepm * EF_NSUBC / ( UC_NDEPM + EF_NSUBC ) ;
     T1 = Vbi_DEP - Vbsz ;  /* Vbs for Symmetry */
     `Fn_SL_CP( T2 , T1 , 0.0 , 0.1, 4 , T0 )
     kjuncpn_Vbs_Vbi = kjuncpn * T2 ;
     W_bsub0 = sqrt( kjuncpn_Vbs_Vbi ) ;

     // Vds dependence of TNDEP
     if( TNDEPV != 0.0 ) begin
       T1 = UC_DEPTHN * ( 1 - TNDEPV * Vds ) ;
       T2 = 1e-3 * UC_DEPTHN ;
       `Fn_SL(        T1 , T1 , 0.1 * UC_DEPTHN , T2 , T0 )
       `Fn_SU( UC_DEPTHN , T1 , 2.0 * UC_DEPTHN , T2 , T0 )
     end

     Vds_maxb0 = 0.0 ;

     /* fully depleted case */
     if( W_bsub0 > UC_DEPTHN ) begin
       Wbsub = UC_DEPTHN ;
     end else begin
       Wbsub = W_bsub0 ;
     end

     //---------------------------------------------------*
     //* initial potential Ps0 calculated.
     //*------------------//

     `Fn_SU( phi_s0_DEP_ini , Vgp , 0.3 , 0.01 , T0 )
     `Fn_SL( phi_s0_DEP_ini , phi_s0_DEP_ini , Vbsc - Vbi_DEP , 0.01 , T0 )

     /* Calc. phi_j0 & Tnp */
     phi_j0 = - Vbi_DEP * EF_NSUBC / (EF_NSUBC + UC_NDEPM) ;
     Tnp = UC_DEPTHN - Wbsub + 1e-15 ;

     /*                               */
     /* solve poisson at source side  */
     /*                               */

     flg_conv = 0 ;
     sm_delta = 0.2;

     phi_s0_DEP = phi_s0_DEP_ini ;
     phi_b0_DEP = Vds_maxb0 ;
     phi_j0_DEP = phi_j0 ;


     for ( lp_s0 = 1 ; lp_s0 <= `lp_se_max  ; lp_s0 = lp_s0 + 1 ) begin

       phi_j0_DEP = NdepmpNsub_inv1 * (NdepmpNsub * phi_b0_DEP + Vbsc - Vbi_DEP) ;
       phi_j0_DEP_dPb = NdepmpNsub_inv1 * NdepmpNsub ;

       T1 = phi_b0_DEP - phi_j0_DEP ;
       `Fn_SL_CP( T2 , T1 , 0.0 , 0.1, 2 , T0 )
       W_b0 = sqrt(C_2ESIpq_Ndepm * T2 ) ;
       `Fn_SU_CP( W_b0 , W_b0 , UC_DEPTHN , 1e-8, 2 , T3 )
       Q_b0_dep = W_b0 * q_Ndepm ;
       Q_b0_dep_dPb = `C_ESI / W_b0 * T0 * T3 ;
       Q_b0_dep_dPd = - `C_ESI / W_b0 * T0 * T3 ;

       if(COFIXCSS==0) begin 
         phib_ref = ( Tn2 + kjuncpn_Vbs_Vbi - 2.0 * UC_DEPTHN * Wbsub ) / C_2ESIpq_Ndepm + phi_s0_DEP ;
         phib_ref_dPs = 1.0 ;
         phib_ref_dPb = 0.0 ;
       end else begin
         phib_ref = phi_s0_DEP + (Tn2 + W_b0 * (W_b0 - 2 * UC_DEPTHN)) / C_2ESIpq_Ndepm ;
         phib_ref_dPs = 1.0 ;
         phib_ref_dPb = (T0 - UC_DEPTHN /W_b0 * T0) * (1 - phi_j0_DEP_dPb) ;
       end

       `Fn_SU_CP( phib_ref , phib_ref , Vds_maxb0 , sm_delta , 4 , T0 )
       phib_ref_dPs = phib_ref_dPs * T0 ;
       phib_ref_dPb = phib_ref_dPb * T0 ;


       T1 = phi_j0_DEP - Vbsc + Vbi_DEP ;
       `Fn_SL_CP( T2 , T1 , 0.0 , 0.1, 2 , T3 )
       W_sub0 = sqrt(C_2ESIpq_Nsub * T2 ) ;
       Q_sub0_dep = - W_sub0 * q_Nsub ;
       Q_sub0_dep_dPd = - `C_ESI / W_sub0 * T3;

       T1 = beta * (phi_s0_DEP - phi_b0_DEP) ;
       T2 = exp(T1) ;
       if( phi_s0_DEP >= phi_b0_DEP ) begin
         Q_s0 = - cnst0 * sqrt(T2 - 1.0 - T1 + 1e-15) ;
         Q_s0_dPs = 0.5 * cnst0 * cnst0 / Q_s0 * (beta * T2 - beta ) ;
         Q_s0_dPb = - Q_s0_dPs ;
       end else begin
         T3 = exp( - beta * (phi_s0_DEP - Vbsc)) ;
         T4 = exp( - beta * (phi_b0_DEP - Vbsc)) ;
         Q_s0 = cnst0 * sqrt(T2 - 1.0 - T1 + cnst1 * (T3 - T4) + 1e-15) ;
         T5 = 0.5 * cnst0 * cnst0 / Q_s0 ;
         Q_s0_dPs = T5 * (beta * T2 - beta + cnst1 * ( - beta * T3) ) ;
         Q_s0_dPb = T5 * ( - beta * T2 + beta + cnst1 * beta * T4 ) ;
       end

       if(flg_conv) begin
         lp_s0 = `lp_se_max + 1 ; // exit Newton Loop
       end else begin
         PF1 = Cox * (Vgp - phi_s0_DEP) + Q_s0 + (Q_b0_dep + Q_sub0_dep) ;
         PF11 = - Cox + Q_s0_dPs ;
         PF12 = Q_s0_dPb + (Q_b0_dep_dPb + Q_b0_dep_dPd * phi_j0_DEP_dPb + Q_sub0_dep_dPd * phi_j0_DEP_dPb) ;
         PF2 = phi_b0_DEP - phib_ref ; 
         PF21 = - phib_ref_dPs ;
         PF22 = 1.0 - phib_ref_dPb ;
         PDJ = PF11 * PF22 - PF12 * PF21;
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
         if(T1 < `ps_conv) begin
           flg_conv = 1 ; // break
         end

         phi_s0_DEP = phi_s0_DEP + dPs ;
         phi_b0_DEP = phi_b0_DEP + dPb ;

       end // End of flg_conv=1

     end // End of Newton Loop

     if( flg_conv == 0 ) begin
       $write( "*** warning(HiSIM_HV(%m)): Went Over Iteration Maximum(Ps0)\n" ) ;
       $write( " Vbse   = %7.3f Vdse = %7.3f Vgse = %7.3f\n" ,Vbse , Vdse , Vgse ) ;
     end

     T1 = beta * (phi_s0_DEP - phi_b0_DEP) ;
     T2 = exp(T1) ;
     if( phi_s0_DEP >= phi_b0_DEP ) begin
       Q_n0 = Q_s0 ;
       Q_s0_dep = 0.0 ;
       Q_sub0 = 0.0 ;
     end else begin
       Q_n0 = 0.0 ;
       Q_s0_dep = cnst0 * sqrt(T2 - 1.0e0 - T1 + 1e-15) ;
       if( W_bsub0 > UC_DEPTHN ) begin
         Q_sub0 = 0.0 ;
       end else begin
         T3 = cnst0 * sqrt( - T1
            + cnst1 * (exp( - beta * (phi_s0_DEP - Vbsc)) - exp( - beta * (phi_b0_DEP - Vbsc)))) ;
         Q_sub0 = T3 - cnst0 * sqrt( - T1)  ;
       end 
     end // (phi_s0_DEP >= phi_b0_DEP)

     `Fn_SL_CP( T2 , (phi_s0_DEP - Vds_maxb0) , 0.0, Ps_delta, 4 , T0 )
     T4 = exp(beta * T2) - 1.0 - beta * T2 + 1e-15 ;
     Q_n0_cur = - cnst0 * sqrt(T4) ;

     //-----------------------------------------------------------*
     //* Start point of Psl(= Ps0 + Pds) calculation.(label)
     //*-----------------//

// start_of_Psl:

     // Vdseff(begin) //
     Vdsorg = Vds ;

     if( Vds > 1e-6 ) begin
       T10 = `Small ;
       T4 = 1.0e0 + C2_q_Ndepm_esi_Cox_inv2 * ( Vgp -  T10 ) ;
       T5 = 1.0e0 + C2_q_Ndepm_esi_Cox_inv2 ;
       `Fn_SL_CP( T4 , T4 , 0, T5 , 2 , T0 )
       T3 = sqrt(T4) ;
       T10 = Vgp + q_Ndepm_esi_Cox_inv2 * ( 1.0 - T3 )  ;
       `Fn_SL_CP( T10 , T10 , 0.0 , 1 , 2 , T0 )
       T1 = Vds / T10 ;
       T2 = `Fn_Pow( T1 , DDLTe-1.0 ) ;
       T3 = 1.0 + T2 * T1 ;
       T4 = `Fn_Pow( T3 , 1.0 / DDLTe-1.0 ) ;
       T6 = T4 * T3  ;
       Vdseff = Vds / T6 ;
       Vds = Vdseff ;
     end else begin
       Vdseff = Vds ;
     end

     //---------------------------------------------------*
     //* start of Psl calculation. (label)
     //*-----------------//

     if( Vds < 0.0 ) begin
 
       phi_sL_DEP = phi_s0_DEP ;
       phi_jL_DEP = phi_j0_DEP ;
       phi_bL_DEP = phi_b0_DEP ;

       Q_subL = Q_sub0 ;
       Q_nL = Q_n0 ;
       Q_nL_cur = Q_n0_cur ;
       Q_bL_dep = Q_b0_dep ;
       Q_subL_dep = Q_sub0_dep ;

     end else begin

       W_bsubL = W_bsub0 ;

       Vds_maxbL = Vds ;

       //---------------------------------------------------*
       //* initial potential Psl calculated.
       //*------------------//

       `Fn_SU( phi_sL_DEP_ini , Vgp , phi_s0_DEP + Vds_maxbL , 0.01 , T0 )
       `Fn_SL( phi_sL_DEP_ini , phi_sL_DEP_ini , Vbsc - Vbi_DEP , 0.01 , T0 )


       /* Calc. Psl_lim */
       fac1 = cnst0 * Cox_inv ;
       fac1p2 = fac1 * fac1 ;
       TX = 1.0e0 + 4.0e0 * ( beta * ( Vgp - Vbsc ) - 1.0e0 ) / ( fac1p2 * beta2 ) ;
       TX = `Fn_Max( TX , `epsm10 ) ;
       Psl_lim = Vgp + fac1p2 * beta * 0.5 * ( 1.0e0 - sqrt( TX ) ) ;

       /*                              */
       /* solve poisson  at drain side */
       /*                              */

       flg_conv = 0 ;

       phi_sL_DEP = phi_sL_DEP_ini ;
       phi_bL_DEP = Vds_maxbL ;

       for ( lp_s0 = 1 ; lp_s0 <= `lp_se_max ; lp_s0 = lp_s0 + 1 ) begin

         phi_jL_DEP  = NdepmpNsub_inv1 * (NdepmpNsub * phi_bL_DEP + Vbsc - Vbi_DEP) ;
         phi_jL_DEP_dPb = NdepmpNsub_inv1 * NdepmpNsub ;

         T1 = phi_bL_DEP - phi_jL_DEP ;
         `Fn_SL_CP( T2 , T1 , 0.0 , 0.1, 2 , T0 )
         W_bL = sqrt(C_2ESIpq_Ndepm * (T2) ) ;
         `Fn_SU_CP( W_bL , W_bL , UC_DEPTHN , 1e-8, 2 , T3 )
         Q_bL_dep = W_bL * q_Ndepm ;
         Q_bL_dep_dPb = `C_ESI / W_bL * T0 * T3 ;
         Q_bL_dep_dPd = - `C_ESI / W_bL * T0 * T3 ;

         if(COFIXCSS==0) begin 
           phib_ref = ( Tn2 + kjuncpn_Vbs_Vbi - 2.0 * UC_DEPTHN * Wbsub ) / C_2ESIpq_Ndepm + phi_sL_DEP ;
           phib_ref_dPs = 1.0 ;
           phib_ref_dPb = 0.0 ;
         end else begin
           phib_ref = phi_sL_DEP + (Tn2 + W_bL * (W_bL - 2 * UC_DEPTHN)) / C_2ESIpq_Ndepm ;
           phib_ref_dPs = 1.0 ;
           phib_ref_dPb = (T0 - UC_DEPTHN /W_bL * T0) * (1 - phi_jL_DEP_dPb) ;
         end
         `Fn_SU_CP( phib_ref , phib_ref , Vds_maxbL , sm_delta , 4 , T0 )
         phib_ref_dPs = phib_ref_dPs * T0 ;
         phib_ref_dPb = phib_ref_dPb * T0 ;

         T1 = phi_jL_DEP - Vbsc + Vbi_DEP ;
         `Fn_SL_CP( T2 , T1 , 0.0 , 0.1, 2 , T3 )
         W_subL = sqrt(C_2ESIpq_Nsub * T2 ) ;
         Q_subL_dep = - W_subL * q_Nsub ;
         Q_subL_dep_dPd = - `C_ESI / W_subL * T3 ;

         T1 = beta * (phi_sL_DEP - phi_bL_DEP) ;
         T2 = exp(T1) ;
         if( phi_sL_DEP >= phi_bL_DEP ) begin
           Q_sL = - cnst0 * sqrt(T2 - 1.0 - T1 + 1e-15) ;    
           Q_sL_dPs = 0.5 * cnst0 * cnst0 / Q_sL * (beta * T2 - beta) ;
           Q_sL_dPb = - Q_sL_dPs ;
         end else begin
           T3 = exp( - beta * (phi_sL_DEP - Vbsc)) ;
           T4 = exp( - beta * (phi_bL_DEP - Vbsc)) ;
           Q_sL = cnst0 * sqrt(T2 - 1.0 - T1 + cnst1 * (T3 - T4) + 1e-15) ;
           T5 = 0.5 * cnst0 * cnst0 / Q_sL ;
           Q_sL_dPs = T5 * (beta * T2 - beta + cnst1 * ( - beta * T3) ) ;
           Q_sL_dPb = T5 * ( - beta * T2 + beta + cnst1 * beta * T4 ) ;
         end
        
         if(flg_conv) begin
           lp_s0 = `lp_se_max + 1 ; // exit Newton Loop
         end else begin
           PF1 = Cox * (Vgp - phi_sL_DEP) + Q_sL + (Q_bL_dep + Q_subL_dep) ;
           PF11 = - Cox + Q_sL_dPs ;
           PF12 = Q_sL_dPb + (Q_bL_dep_dPb + Q_bL_dep_dPd * phi_jL_DEP_dPb + Q_subL_dep_dPd * phi_jL_DEP_dPb) ;
           PF2 = phi_bL_DEP - phib_ref ; 
           PF21 = - phib_ref_dPs ;
           PF22 = 1.0  - phib_ref_dPb ;
           PDJ = PF11 * PF22 - PF12 * PF21;
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
           if(T1 < `ps_conv) begin
              flg_conv = 1 ; // break
           end

           phi_sL_DEP = phi_sL_DEP + dPs ;
           phi_bL_DEP = phi_bL_DEP + dPb ;

         end // End of flg_conv=1

       end // End of newton loop

       if( flg_conv == 0 ) begin
         $write( "*** warning(HiSIM_HV(%m)): Went Over Iteration Maximum(Psl)\n" ) ;
         $write( " Vbse   = %7.3f Vdse = %7.3f Vgse = %7.3f\n" ,Vbse , Vdse , Vgse ) ;
       end

       T1 = beta * (phi_sL_DEP - phi_bL_DEP) ;
       T2 = exp(T1) ;
       if( phi_sL_DEP >= phi_bL_DEP ) begin
         Q_nL = Q_sL ;
         Q_sL_dep = 0.0 ;
         Q_subL = 0.0 ;
       end else begin
         Q_nL = 0.0 ;
         Q_sL_dep = cnst0 * sqrt(T2 - 1.0 - T1 + 1e-15) ;
         if( W_bsubL > UC_DEPTHN ) begin
           Q_subL = 0.0 ;
         end else begin
           T3 = cnst0 * sqrt( - T1
              + cnst1 * (exp( - beta * (phi_sL_DEP - Vbsc)) - exp( - beta * (phi_bL_DEP - Vbsc))) ) ;
           Q_subL = T3 - cnst0 * sqrt( - T1)  ;
         end
       end // ( phi_sL_DEP >= phi_bL_DEP )

     `Fn_SL_CP( T2 , (phi_sL_DEP - Vds_maxbL) , 0.0, Ps_delta, 4 , T0 )
     T4 = exp(beta * T2) - 1.0 - beta * T2 + 1e-15 ;
     Q_nL_cur = - cnst0 * sqrt(T4) ;

     end

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
     // Q_n0_Sym
     `Fn_SL_CP( T2 , (Ps0z - Vds_maxb0) , 0.0, Ps_delta, 4 , T0 )
     T4 = exp(beta * T2) - 1.0 - beta * T2 + 1e-15 ;
     Q_n0_sym = - cnst0 * sqrt(T4) ;

     //---------------------------------------------------*
     // Calc. the resister width of source and drain side
     //*-----------------//

 begin : Calc_W_res_with_DD_mode

     real Pb0DEP , dphi_sb , c_sb , Q_s0 , Q_s0_dPs ;

     if( W_bsub0 > UC_DEPTHN ) begin
        Ws = Tnp ;  // Ids_res = 0
     end else begin

        `Fn_SU_CP( Ps0DEP , Ps0 , 0.0 , 0.1 , VgpDEP_pw , T0 )
        vfb_res = Vgs - Vgp - (UC_VFBC - DEPVFBC - VFBOFFSET) ;
        Vgp_res = Vgs - vfb_res ;
        `Fn_SU_CP( Vgp_res , Vgp_res , 0.0 , VgpDEP_dlt , VgpDEP_pw , T0 )
        flg_conv = 0;

        for ( lp_s0 = 1 ; lp_s0 <= `lp_se_max ; lp_s0 = lp_s0 + 1 ) begin

          T1 = beta * Ps0DEP ;
          T2 = exp(T1) ;
          if( Ps0DEP >= 0.0 ) begin
            Q_s0 = - cnst0 * sqrt(T2 - 1.0 - T1 + 1e-15) ;
            Q_s0_dPs = 0.5 * cnst0 * cnst0 / Q_s0 * (beta * T2 - beta ) ;
          end else begin
            T3 = exp( - beta * (Ps0DEP - Vbsc)) ;
            T4 = exp( - beta * (       - Vbsc)) ;
            Q_s0 = cnst0 * sqrt(T2 - 1.0 - T1 + cnst1 * (T3 - T4) + 1e-15) ;
            T5 = 0.5 * cnst0 * cnst0 / Q_s0 ;
            Q_s0_dPs = T5 * (beta * T2 - beta + cnst1 * ( - beta * T3) ) ;
          end

          if(flg_conv) begin
            lp_s0 = `lp_se_max + 1 ; // exit Newton Loop
          end else begin
            PF1  = Cox * (Vgp_res - Ps0DEP) + Q_s0 ;
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

        Ps0DEP = - Ps0DEP ;
        dphi_sb = q_Ndepm * Tnp * Tnp / 2.0 / `C_ESI ;
        T0 = DEPSUBSL * sqrt(2.0 * beta * dphi_sb) ; 
        T1 = (exp(T0) + exp(-T0)) / 2 ;
        c_sb = ln(T1) / dphi_sb ; 
        TX = c_sb * Ps0DEP ; 
        T0 = exp(- c_sb * dphi_sb) ; 
        if (abs(TX) > 1E-8) begin
           T1 = exp(TX) * T0 ; 
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

        if ( (2.0 * `C_ESI * (Ps0DEP - Pb0DEP) / q_Ndepm ) <= 0) begin
           Ws = 0.0;
        end else begin
           Ws = sqrt(2.0 * `C_ESI * ( Ps0DEP - Pb0DEP) / q_Ndepm ) ;
        end
        
        Ws = (Ws > Tnp) ? Tnp : Ws ;
        
     end // ( W_bsub0 < UC_DEPTHN ) 
 end // Calc_W_res_with_DD_mode

     if(Ws < Tnp ) begin 
       W_res = Tnp - Ws ;         
     end else begin 
       W_res = 0.0 ;
     end 

     //---------------------------------------------------*
     // Calc. Idd
     //*-----------------//
     Qn_drift = - (Q_n0_cur + Q_nL_cur) ;
     if( Pds < 0.0 ) begin // take care of numerical noise //
       Pds = 0.0 ;
       phi_sL_DEP = phi_s0_DEP ;
       Idd = 0.0 ;
     end else begin
       Idd = beta * (Qn_drift) / 2.0 * Pds ;
       Idd = (Idd < 0.0) ? 0.0 : Idd ; 
     end

     Qn0 = - Q_n0_sym ;
     Lch = Leff ;

     //-----------------------------------------------------------*
     //* Muun : surface universal mobility.  (CGS unit)
     //*-----------------//
     T2 = ninv_o_esi / `C_m2cm ;
     T0 = Ninvde ;
     Pdsz = sqrt(Pds * Pds + VZADD0) - sqrt(VZADD0) ;
     T4 = 1.0 + (Pdsz) * T0 ;   // for DC symmetry

     T5 = T2 * Qn0 ;
     T3 = T5 / T4 ;
     Eeff = T3 ;
     T5 = `Fn_Pow( Eeff , MUEPH0 - 1.0e0 ) ;
     T8 = T5 * Eeff ;
     T7 = `Fn_Pow( Eeff , muesr - 1.0e0 ) ;
     T6 = T7 * Eeff ;
     T9 = `C_QE * `C_m2cm_p2 ;
     Rns = Qn0 / T9 ;

     T2 = UC_MUECB0 ;
     T1 = 1.0e0 / ( T2 + UC_MUECB1 * Rns / 1.0e11 )
        + mphn0 * T8 + T6 / UC_MUESR1 ;

     Muun = 1.0e0 / T1 ;

     //  Change to MKS unit //
     Muun = Muun / `C_m2cm_p2 ;

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
       T3 = 1.0e0 ;
     end else if( 2.0e0 - `epsm10 <= BB && BB <= 2.0e0 + `epsm10 ) begin
       T3 = T1 ;
     end else begin
       T3 = `Fn_Pow( T1 , BB - 1.0e0 ) ;
     end
     T2 = T1 * T3 ;
     T4 = 1.0e0 + T2 ;
     if( 1.0e0 - `epsm10 <= BB && BB <= 1.0e0 + `epsm10 ) begin
       T5 = 1.0 / T4 ;
     end else if( 2.0e0 - `epsm10 <= BB && BB <= 2.0e0 + `epsm10 ) begin
       T5 = 1.0 / sqrt( T4 ) ;
     end else begin
       T6 = `Fn_Pow( T4 , ( - 1.0e0 / BB - 1.0e0 ) ) ;
       T5 = T4 * T6 ;
     end
     Mu = Muun * T5 ;
     Mu_acc = Mu ;
     Ey_acc = Ey ;

     //-----------------------------------------------------------*
     //*  Vds for resistor region current. 
     //*-----------------//
     Vds_res = Vdsorg ;
     if( Vdsorg > 1e-6 ) begin
       T10 = ( Vbsc + beta_inv ) * DEPVSATR  ;
       T4 = 1.0e0 + C2_q_Ndepm_esi_Cox_inv2 * (Vgp - T10) ;
       T5 = 1.0e0 + C2_q_Ndepm_esi_Cox_inv2 ;
       `Fn_SL_CP( T4 , T4 , 0, T5 , 2 , T0 )
       T3 = sqrt(T4) ;
       T10 = Vgp + q_Ndepm_esi_Cox_inv2 * ( 1.0e0 - T3 )  ;
       `Fn_SL_CP( T10 , T10 , UC_DEPLEAK, DEPQFN_dlt, 2 , T0 )
       T1 = Vds_res / T10 ;
       T2 = `Fn_Pow( T1 , DEPDDLT-1.0 ) ;
       T3 = 1.0 + T2 * T1 ;
       T4 = `Fn_Pow( T3 , 1.0 / DEPDDLT-1.0 ) ;
       T6 = T4 * T3  ;
       Vds_res = Vds_res / T6 ;
     end

     //-----------------------------------------------------------*
     //*  resistor region universal mobility.  (CGS unit)
     //*-----------------//

     T1 = Vgs - Vbsc ;
     `Fn_SL_CP( T1 , T1 , 0 , 1.0 , 2 , T0 )
     Eeff = T1 / UC_DEPTHN ;
     T5 = `Fn_Pow( Eeff , DEPMUEA1 - 1.0e0 ) ;
     T8 = T5 * Eeff ;
     T2 = UC_DEPMUE0 + `Small ;
     T1 = 1.0e0 / T2 + T8 / UC_DEPMUE2 ;
     Muun = 1.0 / T1 ;

     //  Change to MKS unit //
     Muun = Muun / `C_m2cm_p2 ;

     Edri = Vds_res / Lch ;
     `Fn_Sym4( T1 , Vds_res , 0.1 )  /* Edri for Symmetry */
     T1 = T1 / Lch ;
     T1 = Muun * T1 / UC_DEPVMAX ;
     T2 = `Fn_Pow(T1,DEPBB) ;
     T3 = 1.0 + T2 ;
     T4 = `Fn_Pow(T3,1.0 / DEPBB) ;

     Mu_res = Muun / T4 ;

     //  Modification of NDEPM and Qn_dep
     Qn_res  = - W_res * `C_QE  * UC_NDEPM ;
     Ids_res = weff_nf * ( - Qn_res) * Mu_res * Edri ;

     //-----------------------------------------------------------*
     //* Ids: channel current.
     //*-----------------//

     betaWL = weff_nf * beta_inv / Lch ;
     Ids_acc = betaWL * Idd * Mu_acc ;
     Ids0 = Ids_acc + Ids_res ;

     // Vdseff //
     Vds = Vdsorg;

     //-----------------------------------------------------------*
     //* Adding parasitic components to the channel current.
     //*-----------------//
     if( PTL != 0 )begin
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

     if( flg_rsrd == 2 || flg_rsrd == 3 )begin
       if( RD20 > 0.0 )begin
         T4 = Rd23e ; 
         T1 = UC_RD24 * ( Vgse-RD25 ) ; 
         `Fn_SL( T2 , T1 , T4 , `delta_rd , T0 )
         T3 = T4 * ( RD20 + 1.0 ) ;
         `Fn_SU( T7 , T2 , T3 , `delta_rd , T0 )
       end else begin
         T7 = Rd23e;
       end
        
       if(Vdse >= 0.0) begin
          Vdse_eff = Vdse ;
       end else begin
          Vdse_eff = 0.0 ;
       end
       // smoothing of Ra for Vdse_eff close to zero //
       // ... smoothing parameter is Ra_N            //
       if(Vdse_eff < `Ra_N * `Small2) begin
         Ra_alpha = pow( `Ra_N + 1.0 , RD21 - 1.0 )
                  * (`Ra_N + 1.0 - 0.5 * RD21 * `Ra_N)
                  * pow( `Small2,RD21 );
         Ra_beta = 0.5 * RD21
                 * pow( `Ra_N + 1.0 , RD21 - 1.0 ) / `Ra_N
                 * pow( `Small2, RD21 - 2.0 );
         T1 = Ra_alpha + Ra_beta * Vdse_eff * Vdse_eff;
       end else begin
         T1 = pow( Vdse_eff + `Small2 , RD21 ) ;
       end
       T9 = pow( Vdse_eff + `Small2 , RD22D ) ; 
       Ra = ( T7 * T1 + Vbse * UC_RD22 * T9 ) / weff_nf ;
       T0 = Ra * Ids0 ;
       T1 = Vds + `Small2 ;
       T2 = 1.0 / T1 ; 
       T3 = 1.0 + T0 * T2 ;
       T4 = 1.0 / T3 ; 
       Ids = Ids0 * T4 ;
     end else begin
       Ids = Ids0 ;
       Ra = 0.0 ;
     end

     /* charge calculation */

     //---------------------------------------------------*
     //* Qbu : -Qb in unit area.
     //*-----------------//
     Qbu = - 0.5 * (Q_sub0 + Q_subL + Q_sub0_dep + Q_subL_dep ) ;
     Qiu = - 0.5 * (Q_n0 + Q_nL + Q_s0_dep + Q_sL_dep + Q_b0_dep + Q_bL_dep) ;
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

`ifdef dbgprn3
`include "dbgprn3.cnt"
`endif


 end // HSMHV_depmos2

