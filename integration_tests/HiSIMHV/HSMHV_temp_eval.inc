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
 FILE : HSMHV_temp_eval.inc 

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


    TTEMP = $temperature + DTEMP;
    TTEMP0 = TTEMP ;
    // Self heating //
    TTEMP = TTEMP + deltemp ;
    Tdiff0 =   TTEMP0 - KTNOM ;
    Tdiff0_2 = TTEMP0 * TTEMP0 - KTNOM * KTNOM ;
    Tdiff =    TTEMP  - KTNOM ;
    Tdiff_2 =  TTEMP  * TTEMP  - KTNOM * KTNOM ;
    Tratio = TTEMP / KTNOM ;
    log_Tratio = ln(Tratio) ;
           
    // Band gap //
    Eg   = egtnom - UC_BGTMP1 * Tdiff - UC_BGTMP2 * Tdiff_2 ;
    sqrt_eg = sqrt( Eg ) ;
    T1 = 1.0 / TTEMP ;
    T2 = 1.0 / KTNOM ;
    T3 = egtnom + EGIG
       + IGTEMP2 * ( T1 - T2 )
       + IGTEMP3 * ( T1 * T1 - T2 * T2 ) ;
    Egp12 = sqrt ( T3 ) ;
    Egp32 = T3 * Egp12 ;
          
    // Inverse of the thermal voltage //
    beta   = `C_QE / (`C_KB * TTEMP) ;
    beta_inv = 1.0 / beta ;
    beta2 = beta * beta ;
    betatnom = `C_QE / (`C_KB * KTNOM) ;
             
    // Intrinsic carrier concentration //
    Nin   = `C_Nin0 * exp(log_Tratio*1.5e0)
          * exp(- Eg / 2.0e0 * beta + egtnom / 2.0e0 * betatnom) ;
          
    // Phonon Scattering(temperature-dependent part) //
    T1 = exp(log_Tratio*UC_MUETMP) ;
    mphn0 = T1 / mueph ;
          
    
    // constant for depletion MOS  //
    if( UC_CODEP != 0 && UC_CODEP < 3 ) begin
      cnst0 = sqrt ( 2.0 * `C_ESI * `C_QE * UC_NDEPM * beta_inv ) ;
      cnst1 = Nin * Nin / UC_NDEPM / UC_NDEPM ;
      Pb2n =  2.0e0 * beta_inv * ln (UC_NDEPM / Nin) ;
      Vbipn = beta_inv * ln(UC_NDEPM * EF_NSUBC /Nin /Nin) ;
      T1 = exp(log_Tratio*DEPMUETMP) ;
      depmphn0 = T1 / UC_DEPMUEPH1 ;
      T0 = 1.8 + 0.4 * Tratio + 0.1 * Tratio * Tratio - DEPVTMP * ( 1.0 - Tratio ) ;
      UC_DEPVMAX = UC_DEPVMAX / T0 ;
      `MINRESET(UC_DEPVMAX , 1E3 , "DEPVMAX@T dependence ")
      // Temp. dependence of DEPMUE0 & DEPMUE2
      UC_DEPMUE0 = UC_DEPMUE0 / pow (Tratio, DEPMUE0TMP) ; // Coulomb(CODEP=1,2)
      UC_DEPMUE2 = UC_DEPMUE2 / pow (Tratio, DEPMUE2TMP) ; // Phonon(CODEP=2)
    end else if ( UC_CODEP == 3) begin
      cnst0 = sqrt ( 2.0 * `C_ESI * `C_QE * UC_NDEPM * beta_inv ) ;
      cnst1 = Nin * Nin / UC_NDEPM / UC_NDEPM ;
      Pb2n =  2.0e0 * beta_inv * ln (UC_NDEPM / Nin) ;
      Vbipn = beta_inv * ln(UC_NDEPM * EF_NSUBC /Nin /Nin) ;
      T1 = exp(log_Tratio*DEPMUETMP) ; // revive 20170322
      depmphn0 = T1 / UC_DEPMUEPH1 ;   // revive 20170322
      T0 = 1.8 + 0.4 * Tratio + 0.1 * Tratio * Tratio - DEPVTMP * ( 1.0 - Tratio ) ;
      UC_DEPVMAX = UC_DEPVMAX / T0 ;
      `MINRESET(UC_DEPVMAX , 1E3 , "DEPVMAX@T dependence ")
      // Temp. dependence of DEPMUE0 & DEPMUE2
      UC_DEPMUE0 = UC_DEPMUE0 / pow (Tratio, DEPMUE0TMP) ; // Coulomb scat mobility for resistor (CODEP=3)
      UC_DEPWLP  = DEPWLP + DEPWLPT * ( Tratio - 1.0 ) ;
    end else begin
      Pb2n =  0.0 ;
//    Vbipn = 0.0 ;
      Vbipn = beta_inv * ln(UC_NJUNC/Nin * Nsub/Nin); //  All conc here in 1/m^3.
      depmphn0 = 0.0 ;
    end

    // Pocket Overlap(temperature-dependent part) //
    ptovr = ptovr0 * beta_inv ;
          
    // Velocity Temperature Dependence //
    T1 = TTEMP  / KTNOM ;
    T0 = 1.8 + 0.4 * T1 + 0.1 * T1 * T1 - UC_VTMP * (1.0 - T1) ;
    if( COTEMP != 2 ) begin // without deltemp(COTEMP=0,1,3) //
      VMAXeff = vmax0 * UC_VMAX
              / T0 * ( 1.0 + VMAXT1 * Tdiff0 + VMAXT2 * Tdiff0_2 ) ;
    end else begin // with deltemp(COTEMP=2) //
      VMAXeff = vmax0 * UC_VMAX
              / T0 * ( 1.0 + VMAXT1 * Tdiff + VMAXT2 * Tdiff_2 ) ;
    end
    `MINCHECK(VMAXeff , 1E3 , "VMAXeff@T dependence ")

    if( COTEMP != 2 ) begin // without deltemp(COTEMP=0,1,3) //
      Ninvde = ninvd0 * ( 1.0 + NINVDT1 * Tdiff0 + NINVDT2 * Tdiff0_2 ) ;
      T1 = 1.0 + DEPNINVDT1 * Tdiff0 + DEPNINVDT2 * Tdiff0_2  ;
      NinvdecRES = ninvd0cRES * T1 ; // for Mu_res
      NinvdehRES = ninvd0hRES * T1 ; // for Mu_res
    end else begin // with deltemp(COTEMP=2) //
      Ninvde = ninvd0 * ( 1.0 + NINVDT1 * Tdiff + NINVDT2 * Tdiff_2 ) ;
      T1 = 1.0 + DEPNINVDT1 * Tdiff + DEPNINVDT2 * Tdiff_2  ; 
      NinvdecRES = ninvd0cRES * T1 ; // for Mu_res
      NinvdehRES = ninvd0hRES * T1 ; // for Mu_res
    end
    `MINRESET(Ninvde     , 0 , "NINVD@T dependence ")
    `MINRESET(NinvdecRES , 0 , "NINVDCRES@T dependence ")
    `MINRESET(NinvdehRES , 0 , "NINVDHRES@T dependence ")
           
    // Temperature Dependence of RTH0 //
    if( COSELFHEAT ) begin
      RTH = ( UC_RTH0 + RTHTEMP1 * Tdiff0 + RTHTEMP2 * Tdiff0_2  ) * rthtemp0 ;
      `MINRESET(RTH , `C_RTH_MIN , "RTH@T dependence ")
    end
        
    // Temperature Dependence of POWRAT //
    T2 = UC_POWRAT + PRATTEMP1 * Tdiff0 + PRATTEMP2 * Tdiff0_2  ;
    `Fn_SL( T2 , T2 , 0 , 0.05 , T0 )
    `Fn_SU( powratio , T2 , 1 , 0.05 , T0 )
       
    // 2 phi_B(temperature-dependent) //
    // @temp, with pocket //
    Pb2 =  2.0e0 * beta_inv * ln (Nsub / Nin) ;
        
    // Depletion Width //
    T1 = 2.0e0 * `C_ESI / `C_QE ;
    wdpl = sqrt ( T1 / Nsub ) ;
    wdplp = sqrt( T1 / ( EF_NSUBP ) ) ;
          
    if( UC_CODEP == 0 ) begin
      // Coefficient of the F function for bulk charge //
      cnst0 = sqrt ( 2.0 * `C_ESI * `C_QE * Nsub * beta_inv ) ;
          
      // cnst1: n_{p0} / p_{p0} //
      T1 = Nin / Nsub ;
      cnst1 = T1 * T1 ;
    end
    if( UC_CODEP == 0 ) begin 
      if( UC_NOVER != 0.0 ) begin
        cnst0over = cnst0 * sqrt( UC_NOVER / Nsub ) ;
      end
      if( UC_NOVERS != 0.0 ) begin
        cnst0overs = cnst0 * sqrt( UC_NOVERS / Nsub ) ;
      end
    end else begin
      if( UC_NOVER != 0.0 ) begin
        cnst0over = cnst0 * sqrt( UC_NOVER / UC_NDEPM ) ;
      end
      if( UC_NOVERS != 0.0 ) begin
        cnst0overs = cnst0 * sqrt( UC_NOVERS / UC_NDEPM ) ;
      end
    end
              
    // temperature-dependent resistance model //
    if( UC_CORDRIFT == 0 ) begin 
      // drain side //
      if( UC_RD > 0.0 ) begin
        T2 = Rdtemp0
           * ( LDRIFT1 * UC_RDSLP1 * `C_m2um   + UC_RDICT1 )
           * ( LDRIFT2 * RDSLP2 * `C_m2um   + RDICT2 ) ;
         
        if( COTEMP == 1 ) begin // without deltemp(COTEMP=1) //
          Rde = ( UC_RD + MKS_RDTEMP1 * Tdiff0 + MKS_RDTEMP2 * Tdiff0_2 ) * T2 ;
          `Fn_SL( Rde, Rde, `C_rdtemp_min * UC_RD, `C_rdtemp_dlt * UC_RD, T0 )
        end else begin // with deltemp(COTEMP=0,2,3) //
          Rde = ( UC_RD + MKS_RDTEMP1 * Tdiff + MKS_RDTEMP2 * Tdiff_2 ) * T2 ;
          `Fn_SL( Rde, Rde, `C_rdtemp_min * UC_RD, `C_rdtemp_dlt * UC_RD, T0 )
        end
      end else begin
        Rde = 0.0 ;
      end
      // source side(asymmetric case) //
      if( UC_RS > 0.0 ) begin
        T2 = Rdtemp0
           * ( LDRIFT1S * UC_RDSLP1 * `C_m2um   + UC_RDICT1 )
           * ( LDRIFT2S * RDSLP2 * `C_m2um   + RDICT2 ) ;
         
        if( COTEMP == 1 ) begin // without deltemp(COTEMP=1) //
          Rse = ( UC_RS + MKS_RDTEMP1 * Tdiff0 + MKS_RDTEMP2 * Tdiff0_2 ) * T2 ;
          `Fn_SL( Rse, Rse, `C_rdtemp_min * UC_RS, `C_rdtemp_dlt * UC_RS, T0 )
        end else begin // with deltemp(COTEMP=0,2,3) //
          Rse = ( UC_RS + MKS_RDTEMP1 * Tdiff + MKS_RDTEMP2 * Tdiff_2 ) * T2 ;
          `Fn_SL( Rse, Rse, `C_rdtemp_min * UC_RS, `C_rdtemp_dlt * UC_RS, T0 )
        end
      end else begin
        Rse = 0.0 ;
      end
      if( UC_RDVD > 0.0 ) begin
        T4 = Rdvdtemp0 * ( LDRIFT1 * UC_RDSLP1 * `C_m2um  + UC_RDICT1 )
           * ( LDRIFT2 * RDSLP2 * `C_m2um  + RDICT2 ) ;
        T1 = ( 1 -  UC_RDOV13 ) * LOVERLD * `C_m2um ;
        `Fn_SZ( T2  , RDOV12 , `rdsz_dlt , T0 )
        T8 = - RDOV11 / T2 ;
        T3 = ( T8 * LOVERLD * `C_m2um  + 1.0 + RDOV11 ) ;
        `Fn_SL( T5 , T3 * T4 , T4 , 10.0e-3 , T6 ) 
        `Fn_SU( T7  , T5    , T4 * ( RDOV11 + 1.0) , 50.0e-6 , T6 ) 
        `Fn_SL( T2  , T7 + T1 * T4  , 0, 50.0e-6 , T6 ) 
         
        if( COTEMP == 0 || COTEMP == 1 ) begin // without deltemp(COTEMP=0,1) //
          Rdvde = ( UC_RDVD + MKS_RDVDTEMP1 * Tdiff0 + MKS_RDVDTEMP2 * Tdiff0_2 ) * T2 ;
          `Fn_SL( Rdvde, Rdvde, `C_rdtemp_min * UC_RDVD, `C_rdtemp_dlt * UC_RDVD, T0 )
        end else begin // with deltemp(COTEMP=2,3) //
          Rdvde = ( UC_RDVD + MKS_RDVDTEMP1 * Tdiff + MKS_RDVDTEMP2 * Tdiff_2 ) * T2 ;
          `Fn_SL( Rdvde, Rdvde, `C_rdtemp_min * UC_RDVD, `C_rdtemp_dlt * UC_RDVD, T0 )
        end
            
        T4 = Rdvdtemp0 * ( LDRIFT1S * UC_RDSLP1 * `C_m2um  + UC_RDICT1 )
           * ( LDRIFT2S * RDSLP2 * `C_m2um  + RDICT2 ) ;
        T1 = ( 1 -  UC_RDOV13 ) * LOVERS * `C_m2um ;
        T3 = ( T8 * LOVERS * `C_m2um + 1.0 + RDOV11 ) ;
        `Fn_SL( T5 , T3 * T4 , T4 , 10.0e-3 , T6 ) 
        `Fn_SU( T7  , T5    , T4 * ( RDOV11 + 1.0) , 50.0e-6 , T6 ) 
        `Fn_SL( T2  , T7 + T1 * T4  , 0, 50.0e-6 , T6 ) 
         
        if( COTEMP == 0 || COTEMP == 1 ) begin // without deltemp(COTEMP=0,1) //
          Rsvde = ( UC_RDVD + MKS_RDVDTEMP1 * Tdiff0 + MKS_RDVDTEMP2 * Tdiff0_2 ) * T2 ;
          `Fn_SL( Rsvde, Rsvde, `C_rdtemp_min * UC_RDVD, `C_rdtemp_dlt * UC_RDVD, T0 )
        end else begin // with deltemp(COTEMP=2,3) //
          Rsvde = ( UC_RDVD + MKS_RDVDTEMP1 * Tdiff + MKS_RDVDTEMP2 * Tdiff_2 ) * T2 ;
          `Fn_SL( Rsvde, Rsvde, `C_rdtemp_min * UC_RDVD, `C_rdtemp_dlt * UC_RDVD, T0 )
        end
      end else begin
        Rdvde = 0.0 ;
        Rsvde = 0.0 ;
      end
    end  // CORDRIFT=0
      
    // costi0 and costi1 for STI transistor model(temperature-dependent part) //
    costi0 = costi00 * sqrt(beta_inv) ;
    costi0_p2 = costi0 * costi0 ;
    costi1 = Nin * Nin * nsti_p2 ;

    // Temperature dependence of Breakdown Model //
    HBDCeff = HBDC + HBDCTMP * Tdiff ;

    // Temperature dependence for Isub Model //
    UC_SUBTMP  = SUBTMP ;
    `RANGERESET(UC_SUBTMP ,    0.0 ,       5.0e-3 , "SUBTMP"   )

`ifdef _AGING_
    // Temperature dependence for aging model //
    Nt0s_temp  = TRAPTEMP1 * Tdiff + TRAPTEMP2 * Tdiff_2 ; 
`endif           
    // end of HSMHVtemp_eval.h //
