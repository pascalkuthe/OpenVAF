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
 FILE : HSMHV_iterativePs0.inc

 Date : 2019.04.26

 released by Hiroshima University

***********************************************************************/

  begin : HSM2_iterativePs0

     real Vbscl , phi_s0 ;
     real dphi_sb , c_sb , phi_0 , Chib , phi_b , phi_b_dPss , fb_dPss ;
       
     //-----------------------------------------------------------*
     //Solve Poisson's equation
     //----------------//
     Vbscl = dphi_vds ;
       
     //-----------------------------------------------------------*
     //* Accumulation zone. (zone-A)
     //* - evaluate basic characteristics and exit from this part.
     //*-----------------//
     Vgs_fb = Vfb - dVth + dPpg + Vbscl + VFBPT ; 
     if( Vgs < Vgs_fb ) begin
         flg_zone = -1 ; 
         
         //---------------------------------------------------*
         //* Evaluation of Ps0.
         //* - Psa : Analytical solution of
         //*             Cox( Vgp - Psa ) = cnst0 * Qacc
         //*         where Qacc is the 3-degree series of(fdep)^{1/2}.
         //*         The unknown  is transformed to Chi=beta(Ps0-Vbs).
         //* - Ps0_min : |Ps0_min| when Vbs=0.
         //*-----------------//
         // Ps0_min: approx. solution of Poisson equation at Vgs_min //
         //          ( easy to improve, if necessary  )              //
         //    Ps0_min = Eg - Pb2 ;
         Ps0_min = 2.0 * beta_inv * ln (-Vgs_min/fac1) ;
         TX = beta * ( Vgp - Vbscl ) ;
         T1 = 1.0 / ( beta * cnst0 ) ;
         TY = T1 * Cox ;
         Ac41 = 2.0 + 3.0 * `C_SQRT_2 * TY ;
         Ac4 = 8.0 * Ac41 * Ac41 * Ac41 ;
         T4 = ( TX - 2.0 ) ;
         T5 = 9.0 * TY * T4 ;
         Ac31 = 7.0 * `C_SQRT_2 - T5 ;
         Ac3 = Ac31 * Ac31 ;
         if( Ac4 < Ac3 * 1.0e-8 ) begin
           Ac1 = -7.0 * `C_SQRT_2 + Ac31 + 0.5 * Ac4 / Ac31 + T5 ;
         end else begin
           Ac2 = sqrt( Ac4 + Ac3 ) ;
           Ac1 = -7.0 * `C_SQRT_2 + Ac2 + T5 ;
         end
         Acd = `Fn_Pow( Ac1 , `C_1o3 ) ;
         Acn = -4.0 * `C_SQRT_2 - 12.0 * TY + 2.0 * Acd + `C_SQRT_2 * Acd * Acd ;
         T1 = 1.0 / Acd ;
         Chi = Acn * T1 ;
         Psa = Chi * beta_inv + Vbscl ;
         T1 = Psa - Vbscl ;
         T2 = T1 / Ps0_min ;
         T3 = sqrt( 1.0 + ( T2 * T2 ) ) ;
         Ps0 = T1 / T3 + Vbscl ;
         
     end else begin //if( Vgs < Vgs_fb ) begin
       
       //---------------------------------------------------*
       //* Calculation of Ps0. (beginning of Newton loop)
       //* - Fs0 : Fs0 = 0 is the equation to be solved.
       //* - dPs0 : correction value.
       //*-----------------//
       exp_bVbsVds = exp( beta * ( Vbscl - PSLIMPT ) ) ;
       flg_conv = 0 ;
       
       phi_s0 = Ps0_ini ;
       
       dphi_sb = q_Nsub * t_SUB * t_SUB / 2.0 / `C_ESI ;
       T0 = sqrt(2.0 * beta * dphi_sb) ;
       T1 = (lexp(T0) + lexp(-T0)) / 2 ;
       c_sb = ln(T1) / dphi_sb ;
       
       for ( lp_s0 = 1 ; lp_s0 <= lp_s0_max + 1 ; lp_s0 = lp_s0 + 1 ) begin
         phi_0 = phi_s0 - Vbscl ;
         Chi = beta * phi_0 ;

         TY = c_sb * (phi_0 - dphi_sb) ;
         if( TY < `large_arg ) begin
           T1 = exp(TY) ;
           T0 = exp(- c_sb * dphi_sb) ;
           T2 = T1 - T0 ;
           phi_b = ln(1 + T2) / c_sb ;
           phi_b_dPss = T1 / (1 + T2) ;
         end else begin
           phi_b = phi_0 - dphi_sb ;
           phi_b_dPss = 1 ;
         end
         Chib = beta * phi_b ;
         
         if (abs(Chi) < 1E-16) begin
           T0 = sqrt((1 - phi_b_dPss * phi_b_dPss) / 2) ;
           fb = Chi * T0 ;
           fb_dPss = beta * T0 ;
           if (Chi < 0) begin
             fb = -fb ;
             fb_dPss = -fb_dPss ;
           end
         end else if (abs(Chi) < 5E-3) begin
           
           T0 = Chi * Chi / 2 * (1 - Chi / 3 * (1 - Chi / 4 * (1 - Chi / 5))) ;
           T1 = Chi * (1 - Chi / 2 * (1 - Chi / 3 * (1 - Chi / 4))) ;
           T2 = Chib * Chib / 2 * (1 - Chib / 3 * (1 - Chib / 4 * (1 - Chib / 5))) ;
           T3 = Chib * (1 - Chib / 2 * (1 - Chib / 3 * (1 - Chib / 4))) ;
           fb = sqrt(T0 - T2) ;
           fb_dPss = beta * 0.5 * (T1 - phi_b_dPss * T3) / fb ;
         end else begin
           //-------------------------------------------*
           //* zone-D3.  (phi_s0)
           //*-----------------//
           
           T0 = exp(-Chi) ;
           T1 = exp(-Chib) ;
           fb = sqrt(Chi - Chib + (T0 - T1)) ;
           fb_dPss = beta * 0.5 * (1 - T0 - phi_b_dPss * (1 - T1)) / fb ;
         end // check nest level. ok. //if (abs(Chi) < 1E-8) begin
         
         if (flg_conv == 1 && Chi < 0.0) begin
           flg_zone = -1 ;
         end
         
         if (Chi < 0) begin
           fs02 = -fb ;
           fs02_dPs0 = -fb_dPss ;
         end else if (Chi < 1E-7) begin
           fs02 = fb ;
           fs02_dPs0 = fb_dPss ;
         end else begin
           Rho = beta * ( phi_s0 - PSLIMPT ) ;
           exp_Rho = exp( Rho ) ;
           fs01 = cnst1 * ( exp_Rho - exp_bVbsVds * (Chi + 1) ) ;
           fs01_dPs0 = cnst1 * beta * ( exp_Rho - exp_bVbsVds ) ;
           fs02 = sqrt(fb * fb + fs01) ;
           fs02_dPs0 = 0.5 * (2 * fb_dPss * fb + fs01_dPs0) / fs02 ;
         end
         
         // Revised Fs0 //
         Fs0 = - Vgp + phi_s0 + fac1 * fs02 ;
         Fs0_dPs0 = 1.0 + fac1 * fs02_dPs0 ;
         if( flg_conv == 1 ) begin  
           lp_s0 = lp_s0_max+1 ; // break
         end else begin
           dPs0 = - Fs0 / Fs0_dPs0 ;
           
           //-------------------------------------------*
           //* Update Ps0 .
           //* - clamped to Vbscl if Ps0 < Vbscl .
           //*-----------------//
           dPlim = 0.5*`dP_max*(1.0 + `Fn_Max(1.0e0,abs(phi_s0))) ;
           if ( abs( dPs0 ) > dPlim ) dPs0 = dPlim * `Fn_Sgn( dPs0 ) ;
           phi_s0 = phi_s0 + dPs0 ;
           
           //-------------------------------------------*
           //* Check convergence.
           //* NOTE: This condition may be too rigid.
           //*-----------------//
           if( abs( dPs0 ) <= `ps_conv && abs( Fs0 ) <= `gs_conv ) begin
             flg_conv = 1 ;
           end
           
         end  
       end // end of phi_s0 Newton loop //
       
       Ps0 = phi_s0 ;
       
      end // end of if( Vgs < Vgs_fb )

  end // HSMHV_iterativePs0
