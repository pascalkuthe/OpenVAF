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
 FILE : vaFiles/HSMHV_eval_dio.inc 

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
 
     
  //-----------------------------------------------------------*
  //* Temperature dependent constants.
  //*-----------------//
  if( COSELFHEAT > 0 && UC_RTH0 != 0.0 ) begin
        
`include "HSMHV_temp_eval_dio.inc"
  end else begin
    TTEMP = $temperature + DTEMP;
  end
        
  //-----------------------------------------------------------*
  //* Cbsj, Cbdj: node-base S/D biases.
  //*-----------------//
        
  // ibd //
  T10 = CVBD * jd_nvtm_invd ;
  T9 = CISBD * exptempd ;
  `HSMHV_Idio(Ibd_btm, vbd_jct,  isbd_btm, isbd2_btm,  vbdt, jd_expcd,  T10, T9, jd_nvtm_invd, UC_CISBKD, DIVXD)
  `HSMHV_Idio(Ibd_sws, vbd_jct,  isbd_sws, isbd2_sws,  vbdt, jd_expcd,  T10, T9, jd_nvtm_invd, UC_CISBKD, DIVXD)
  if ( CODIO > 0 ) begin //new model:
     `HSMHV_Idio(Ibd_swg, vbdi_jct, isbd_swg, isbd2_swg,  vbdt, jd_expcd,  T10, T9, jd_nvtm_invd, UC_CISBKD, DIVXD)
  end else begin //backward compatible:
     Ibd_swg = 0.0 ;
  end

  // ibs //
  T10 = CVBS * jd_nvtm_invs ;
  T9 = CISBS * exptemps ;
  `HSMHV_Idio(Ibs_btm, vbs_jct,  isbs_btm, isbs2_btm,  vbst, jd_expcs,  T10, T9, jd_nvtm_invs, UC_CISBKS, DIVXS)
  `HSMHV_Idio(Ibs_sws, vbs_jct,  isbs_sws, isbs2_sws,  vbst, jd_expcs,  T10, T9, jd_nvtm_invs, UC_CISBKS, DIVXS)
  if ( CODIO > 0 ) begin //new model:
     `HSMHV_Idio(Ibs_swg, vbsi_jct, isbs_swg, isbs2_swg,  vbst, jd_expcs,  T10, T9, jd_nvtm_invs, UC_CISBKS, DIVXS)
  end else begin //backward compatible:
     Ibs_swg = 0.0 ;
  end

  //-----------------------------------------------------------*
  //* Charges and Capacitances.
  //*-----------------//
  //  charge storage elements
  //*  bulk-drain and bulk-source depletion capacitances
  //*  czbd : zero bias drain junction capacitance
  //*  czbs : zero bias source junction capacitance
  //*  czbdsw:zero bias drain junction sidewall capacitance
  //*  czbssw:zero bias source junction sidewall capacitance
  ////
      
  // Drain Bulk Junction //
  `HSMHV_Qdio(Qbd_btm, Capbd_btm, vbd_jct,  czbd,    pzbd,    MJD)
  `HSMHV_Qdio(Qbd_sws, Capbd_sws, vbd_jct,  czbdsw,  pzbdsw,  MJSWD)
  if ( CODIO > 0 ) begin //new model:
     `HSMHV_Qdio(Qbd_swg, Capbd_swg, vbdi_jct, czbdswg, pzbdswg, MJSWGD)
  end else begin //backward compatible:
     `HSMHV_Qdio(Qbd_swg, Capbd_swg, vbd_jct,  czbdswg, pzbdswg, MJSWGD)
  end

  // Source Bulk Junction //
  `HSMHV_Qdio(Qbs_btm, Capbs_btm, vbs_jct,  czbs,    pzbs,    MJS)
  `HSMHV_Qdio(Qbs_sws, Capbs_sws, vbs_jct,  czbssw,  pzbssw,  MJSWS)
  if ( CODIO > 0 ) begin //new model:
     `HSMHV_Qdio(Qbs_swg, Capbs_swg, vbsi_jct, czbsswg, pzbsswg, MJSWGS)
  end else begin //backward compatible:
     `HSMHV_Qdio(Qbs_swg, Capbs_swg, vbs_jct,  czbsswg, pzbsswg, MJSWGS)
  end


  //---------------------------------------------------*
  //* Junction diode.
  //*-----------------//
  Ibs = Mfactor * (Ibs_btm + Ibs_sws) ;
  Ibd = Mfactor * (Ibd_btm + Ibd_sws) ;
  if ( CODIO > 0 ) begin //new model:
     Ibsi = Mfactor * Ibs_swg ;
     Ibdi = Mfactor * Ibd_swg ;
     Qbs = Mfactor * (Qbs_btm + Qbs_sws) ;
     Qbd = Mfactor * (Qbd_btm + Qbd_sws) ;
     Qbsi = Mfactor * Qbs_swg ;
     Qbdi = Mfactor * Qbd_swg ;
     Capbs = Mfactor * (Capbs_btm + Capbs_sws) ;
     Capbd = Mfactor * (Capbd_btm + Capbd_sws) ;
     Capbsi = Mfactor * Capbs_swg ;
     Capbdi = Mfactor * Capbd_swg ;
  end else begin //backward compatible:
     Ibsi = 0.0 ;
     Ibdi = 0.0 ;
     Qbs = Mfactor * (Qbs_btm + Qbs_sws + Qbs_swg) ;
     Qbd = Mfactor * (Qbd_btm + Qbd_sws + Qbd_swg) ;
     Capbs = Mfactor * (Capbs_btm + Capbs_sws + Capbs_swg) ;
     Capbd = Mfactor * (Capbd_btm + Capbd_sws + Capbd_swg) ;
     Qbsi = 0.0 ;
     Qbdi = 0.0 ;
     Capbsi = 0.0 ;
     Capbdi = 0.0 ;
  end












  //---------------------------------------------------*
  //* Reverse recovery model imported from Diode_CMC 2.0.0 (23/Aug/2016).
  //*-----------------//

//imported from DIODE_CMC_InitModel.include:
         //diode reverse recovery model
         NDI_i    = NDIBOT / `C_cm2m_p3;  //UC_NOVER
         NJL      = UC_NJD;
         MUEN_i   = `C_MUEN / `C_m2cm_p2;  // [cm2/V/sec] -> [m2/V/sec]
         MUEP_i   = `C_MUEP / `C_m2cm_p2;  // [cm2/V/sec] -> [m2/V/sec]
         JUNCDLT  = 1e-3; //Smoothness of depletion width around zero bias (minor) [V]
         //// Temperature-dependent variables
         // Intrinsic Carrier Density:
         `HSMHV_Idio_args1(Nin_dio,     `C_Nin0_dio,    XTID,  UC_NJD)  //diverted from js to Nin_dio
         // Minority Carrier Density:
         Pn0 = Nin_dio * Nin_dio / NDI_i;
         // Ambipolar Diffusion Length:
         T1 = pow(Tratio, -`C_MUETMP); // temperature dependence of phonon scattering
         Dn = (MUEN_i * T1) * beta_inv;
         Dp = (MUEP_i * T1) * beta_inv;
         Da = 2*Dn*Dp/(Dn+Dp);
         T2 = pow(Tratio, TAUT); // temperature dependence of tau
         tau_HL = TAU * T2;
         La = sqrt(tau_HL * Da);
         // High-Injection Threshold Voltages:
         V_HA = (NJL * beta_inv) * ln( NDI_i / Pn0 );
         V_HK = (NJL * beta_inv) * ( ln( NDI_i / Pn0 ) + WI/La );
         //End of diode reverse recovery model



//imported from diode_cmc.va:
        /////////////////////////////////////////////////////////////////////////////
        //
        //  Recovery charge (calculations for ddt() contribs)
        //
        /////////////////////////////////////////////////////////////////////////////

        // diode reverse recovery model
        begin : evaluateRecoveryCharge

            if ( CORECOVERY > 0 ) begin

                //// High-Injection part //////////////////////////////////////////////////////

                // High-Injection Coefficient for the N+/N- Junction:
                    nj_K = UC_NJD;

                // Hole Density at the P+/N- Junction:
                exp_A = exp( vbd_jct * jd_nvtm_invd );

                // Hole Density at the N+/N- Junction:
                if ( vbd_jct - ( V_HK - V_HA ) > 0 ) begin
                    exp_K = exp( beta * ( vbd_jct / nj_K - (V_HK-V_HA) / nj_K ) );
                end else begin
                    exp_K = 1;
                end


                //// Carrier-distribution-based recovery model ////////////////////////////////

                /////////////////////////////////////////////////////////
                //// Injected Charge from the P+/N- Junction (Anode Side)
                /////////////////////////////////////////////////////////

                // Modification of exp_A (especially for the high-injection condition):
                if (INJ2 == 0 || vbd_jct < V_HA) begin
                    exp_A2 = exp_A * INJ1;
                end else begin
                    exp_A2 = exp_A * INJ1 * exp( - INJ2*(vbd_jct-V_HA)*(vbd_jct-V_HA) * exp(INJT*ln(1/Tratio)) );
                end
                exp_A2 = ( exp_A2 > `C_EXPCEIL ?  `C_EXPCEIL : exp_A2); // safety valve

                // Hole Density at the P+/N- Junction:
                p_nA = Pn0 * exp_A2;

                // Excess Charge Density (Quasi-Static):
                q_pexA = `C_QE * AD * ( p_nA - Pn0 );

                //// Nonquasi-Static (NQS) Effect Model Formulation
                if ( NQS > 0.0 ) begin
                    Q_qs_A = q_pexA * `QN_INJ_SCALING;        //input: QS charge density (scaled)
                    Q_nqs_A = `NQS_CAPACITOR * V(charge_A);   //output: NQS charge dens. (scaled)
                    Inqs0_A = (Q_nqs_A - Q_qs_A ) / NQS;      //DC current to the charge node
                    // Injected Charge Density (Nonquasi-Static):
                    q_pexA_nqs = Q_nqs_A / `QN_INJ_SCALING;   //(rescaled)
                end else begin
                    Q_qs_A = q_pexA;     //input: QS charge density (unscaled)
                    q_pexA_nqs = Q_qs_A; //(unscaled)
                end

                /////////////////////////////////////////////////////////
                //// Injected Charge from the N+/N- Junction (Cathode Side)
                /////////////////////////////////////////////////////////

                // Modification of exp_A (especially for the high-injection condition):
                if (INJ2 == 0 || vbd_jct < V_HK) begin
                    exp_K2 = exp_K * INJ1;
                end else begin
                    exp_K2 = exp_K * INJ1 * exp( - INJ2*(vbd_jct-V_HK)*(vbd_jct-V_HK) * exp(INJT*ln(1/Tratio)) );
                end
                exp_K2 = ( exp_K2 > `C_EXPCEIL ?  `C_EXPCEIL : exp_K2); // safety valve

                // Hole Density at the N+/N- Junction:
                p_nK = Pn0 * exp_K2;

                // Excess Charge Density (Quasi-Static):
                q_pexK = `C_QE * AD * (p_nK - Pn0);

                //// Nonquasi-Static (NQS) Effect Model Formulation
                if ( NQS > 0.0 ) begin
                    Q_qs_K = q_pexK * `QN_INJ_SCALING;        //input: QS charge density (scaled)
                    Q_nqs_K = `NQS_CAPACITOR * V(charge_K);   //output: NQS charge dens. (scaled)
                    Inqs0_K = (Q_nqs_K - Q_qs_K ) / NQS;      //DC current to the charge node
                    // Injected Charge Density (Nonquasi-Static):
                    q_pexK_nqs = Q_nqs_K / `QN_INJ_SCALING;   //(rescaled)
                end else begin
                    Q_qs_K = q_pexK;     //input: QS charge density (unscaled)
                    q_pexK_nqs = Q_qs_K; //(unscaled)
                end

                /////////////////////////////////////////////////////////
                //// Depletion part
                /////////////////////////////////////////////////////////

                //// At the P+/N- Junction:
                Vjunc_A = PBD - vbd_jct ;
                //Vjunc_A = VBIRBOT_i - vbd_jct ; //to use the existing parameter in diode_cmc but results will change
                `Fn_SZ( Vjunc_A, Vjunc_A, JUNCDLT, T0 )
                W_depA = sqrt (2 * `C_ESI * Vjunc_A / (`C_QE * NDI_i));
                //W_depA = sqrt (2 * EPSSI * Vjunc_A / (`C_QE * NDI_i)); //to use the existing parameter in diode_cmc but results will change

                // Punch through effect:
                `Fn_SU( W_depA, W_depA, WI, `C_PUNCH, T0 )

                // NQS Effect on W_depA (temporary model):
                if ( DEPNQS > 0.0 ) begin
                    W_qs_A = W_depA * `DEP_SCALING;            //input: QS value (scaled)
                    W_nqs_A = `DEPNQS_CAPACITOR * V(depl_A);   //output: NQS value (scaled)
                    IWnqs0_A = (W_nqs_A - W_qs_A ) / DEPNQS;   //DC current to the NQS node
                    // Depletion Width (Nonquasi-Static):
                    W_depA_nqs = W_nqs_A / `DEP_SCALING;       //(rescaled)
                end else begin
                    W_qs_A = W_depA;     //input: QS value (unscaled)
                    W_depA_nqs = W_qs_A; //(unscaled)
                end


                /////////////////////////////////////////////////////////
                //// Final Equations
                /////////////////////////////////////////////////////////
                //----- Analytical Integration Based on the Carrier Distribution:
                // basic majority-carrier charge:
                Q_n0 = - (NDI_i * AD * `C_QE) * WI;
                // excess charge (Anode side):
                Q_nexA_nqs = La * q_pexA_nqs * ( exp( - WI/La ) - exp( - W_depA_nqs/La ) );
                // excess charge (Cathode side):
                Q_nexK_nqs = La * q_pexK_nqs * ( exp( - (WI - W_depA_nqs)/La ) - 1.0 );
                // depletion charge:
//              Q_depA = + (NDI_i * AD * `C_QE) * W_depA_nqs;

                //// Final total electron charge:
                Qrr = - ( Q_n0 + Q_nexA_nqs + Q_nexK_nqs );

                /////////////////////////////////////
                //connection with original Diode_cmc:
                /////////////////////////////////////
	        Qbd = Qbd + Mfactor * Qrr;

            end //End of if ( CORECOVERY > 0 )

        end // evaluateRecoveryCharge
        // End of diode reverse recovery model



        /////////////////////////////////////////////////////////////////////////////
        //
        //  NQS nodes contribs for the recovery model
        //
        /////////////////////////////////////////////////////////////////////////////

        // diode reverse recovery model
        begin : loadRecovery
            if ( CORECOVERY > 0 && NQS > 0.0 ) begin
                I(charge_A) <+ Inqs0_A + ddt(Q_nqs_A);
                I(charge_K) <+ Inqs0_K + ddt(Q_nqs_K);
            end else begin
                V(charge_A) <+ 0.0;
                V(charge_K) <+ 0.0;
            end
            if ( CORECOVERY > 0 && DEPNQS > 0.0 ) begin
                I(depl_A) <+ IWnqs0_A + ddt(W_nqs_A);
            end else begin
                V(depl_A) <+ 0.0;
            end
        end // loadRecovery
        // End of diode reverse recovery model






//  End of HSMHVevalDio //
