/***********************************************************************

 HiSIM (Hiroshima University STARC IGFET Model)
 Copyright (C) 2000-2016 Hiroshima University and STARC
 Copyright (C) 2016-2019 Hiroshima University

 MODEL NAME : HiSIM2
 ( VERSION : 3  SUBVERSION : 1  REVISION : 1 )
 Model Parameter 'VERSION' : 3.11
 FILE : HSM2_depmos.inc

 Date : 2019.04.04

 released by Hiroshima University
 
***********************************************************************/
//
////////////////////////////////////////////////////////////////
//
//Licensed under the Educational Community License, Version 2.0 
//(the "License"); you may not use this file except in 
//compliance with the License. 
//
//You may obtain a copy of the License at: 
//
//	http://opensource.org/licenses/ECL-2.0
//
//Unless required by applicable law or agreed to in writing, 
//software distributed under the License is distributed on an 
//"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, 
//either express or implied. See the License for the specific 
//language governing permissions and limitations under the 
//License.
//
//
//The HiSIM_SOI standard has been supported by the members of 
//Silicon Integration Initiative's Compact Model Coalition. A 
//link to the most recent version of this standard can be found 
//at:
//
//http://www.si2.org/cmc
//
////////////////////////////////////////////////////////////////

   //---------------------------------------------------*
 begin : HSM2_depmos  
   //*------------------//
   // 
   real W_b0, W_bL, W_s0, W_sL, W_sub0, W_subL ;
   real W_bsub0, W_bsubL ;
   real vthn, afact, bfact, cfact, afact2, afact3  ;
   real phi_s0_DEP, phi_sL_DEP , Vbi_DEP ;
   real phi_j0_DEP, phi_jL_DEP, Vgp0, Psbmax ;
   real y0 ;

   real phi_s0_DEP_old, phi_b0_DEP_old, phi_bL_DEP_old, phi_j0_DEP_old, phi_jL_DEP_old ;
   real phi_sL_DEP_old ;
   real Q_s0, Q_sL, Q_sub0, Q_subL ;
   integer depmode ;

   real phi_s0_DEP_ini, phi_sL_DEP_ini, phi_b0_DEP_ini ;
   real phi_b0_DEP,  phi_bL_DEP ;

   real y1, y2 ;
   real y11, y12, y21, y22, dety ;
   real rev11, rev12, rev21, rev22 ;
   real Qn_res0 ;
   real Q_b0_dep, Q_sub0_dep, Q_bL_dep, Q_subL_dep ;

   real W_res0 ;

   real q_Ndepm_esi, Idd_drift,Idd_diffu,Qn_bac ;
   real Mu_bac ;

   real dydPsm ;

   real Vgp1, Vgp0old, Vds_maxb0, Vds_maxbL, phi_j0_DEP_acc, phi_jL_DEP_acc ;
   real Q_n0_cur, Q_nL_cur ;
   real Q_n0 , Q_nL , Qn_delta ;
   real Qn_drift  ;
   real Q_s0_dep, Q_sL_dep ;
   
   real sm_delta ;
   real phib_ref, phib_ref_dPs, phib_ref_dPd ;
   real Q_s0_dPs, Q_sL_dPs, Q_s0_dPb, Q_sL_dPb ;
   real Q_b0_dep_dPb, Q_bL_dep_dPb, Q_b0_dep_dPd, Q_bL_dep_dPd, Q_sub0_dep_dPd, Q_subL_dep_dPd ;
   real phi_j0_DEP_dPb, phi_jL_DEP_dPb ;
   real NdepmpNsub_inv1, NdepmpNsub ;
   real Vgpp , Vdseff0 ;


   real C_QE2, C_ESI2, Tn2 ; 
   real q_Nsub, Ndepm2, q_Ndepm ; 
   real C_2ESIpq_Ndepm, C_2ESIpq_Ndepm_inv , C_2ESI_q_Ndepm ; 
   real C_2ESIpq_Nsub , C_2ESIpq_Nsub_inv ; 
   real ps_conv3 , ps_conv23 ; 
   real Ids_bac, Edri ;
   real Ey_suf , Vbsc ;

// for Vdseff calculation
`define DEPQFN1    2.0
`define DEPQFN3    0.3
`define DEPQFN_dlt 2.0

// for Current Smoothing
`define Ps_delta   0.06
`define Ps_delta0  0.08

   // Constants 
    Vbi_DEP = Vbipn ;
    q_Ndepm = `C_QE * UC_NDEPM ;
    Ndepm2  = UC_NDEPM * UC_NDEPM ;
    q_Ndepm_esi = `C_QE * UC_NDEPM * `C_ESI ;
    q_Nsub = `C_QE * EF_NSUBC ;
    C_QE2  = `C_QE * `C_QE ;
    C_ESI2 = `C_ESI * `C_ESI ;
    Tn2    = UC_DEPTHN * UC_DEPTHN ;
    C_2ESIpq_Ndepm = 2.0 *`C_ESI/q_Ndepm ;
    C_2ESIpq_Ndepm_inv = q_Ndepm / (2.0 * `C_ESI) ;
    C_2ESI_q_Ndepm = 2.0 * `C_ESI * q_Ndepm ;
    C_2ESIpq_Nsub  = 2.0 * `C_ESI / q_Nsub  ;
    C_2ESIpq_Nsub_inv  = q_Nsub / (2.0 * `C_ESI) ;
    NdepmpNsub  = UC_NDEPM / EF_NSUBC ;
    NdepmpNsub_inv1  = 1.0 / (1.0 + NdepmpNsub ) ;
    ps_conv3  = `ps_conv * 1000.0 ;
    ps_conv23 = `ps_conv2 * 1000.0 ;
    qnsub_esi = q_Ndepm_esi ;

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

    Vbsc = Vbs ; // Change Vbs vars to Vbsc(internal)

     //---------------------------------------------------*
     //* start of Ps0 calculation. (label)
     //*-----------------//

     //---------------------------------------------------*
     //* region judgement  
     //*------------------//

       Vgp = Vgp + `epsm10 * 1.0e7 ;


       afact = Cox * Cox / cnst0 / cnst0 ;
       afact2 = afact / Nin / Nin * Ndepm2 ;
       W_bsub0 = sqrt(C_2ESIpq_Ndepm * EF_NSUBC / (EF_NSUBC + UC_NDEPM) * ( - Vbsc + Vbi_DEP)) ;

      /* fully depleted case */
       if( W_bsub0 > UC_DEPTHN ) begin
         
         Vgp0 = 0.0;

         W_b0 = UC_DEPTHN ;
         phi_b0_DEP = 0.0 ;
         phi_j0_DEP = phi_b0_DEP - C_2ESIpq_Ndepm_inv * W_b0 * W_b0 ;
         Vds_maxb0 = 0.0 ;

         Vgp0old = Vgp0 ;
         phi_j0_DEP_old = phi_j0_DEP ;

         for ( lp_s0 = 1 ; lp_s0 <= `lp_se_max + 1 ; lp_s0 = lp_s0 + 1 ) begin

           W_b0 = sqrt(C_2ESIpq_Ndepm * (phi_b0_DEP - phi_j0_DEP) ) ;
           `Fn_SU_CP( W_b0 , W_b0 , UC_DEPTHN , 1e-8, 2 , T0 )
           W_sub0 = sqrt(C_2ESIpq_Nsub * (phi_j0_DEP - Vbsc + Vbi_DEP) ) ;

           Q_b0_dep = W_b0 * q_Ndepm ;
           Q_b0_dep_dPd = - `C_ESI / W_b0 * T0 ;
           Q_sub0_dep = - W_sub0 * q_Nsub ;
           Q_sub0_dep_dPd = - `C_ESI / W_sub0 ;

           y1 = Cox * (Vgp0 - phi_b0_DEP) + Q_b0_dep + Q_sub0_dep ;
           y11 = Cox ;
           y12 = Q_b0_dep_dPd + Q_sub0_dep_dPd ;

           y2 = phi_j0_DEP - NdepmpNsub_inv1 * (NdepmpNsub * phi_b0_DEP + Vbsc - Vbi_DEP) ;
           y21 = 0.0 ;
           y22 = 1.0 ;

           dety = y11 * y22 - y21 * y12;
           rev11 = (y22) / dety ;
           rev12 = ( - y12) / dety ;
           rev21 = ( - y21) / dety ;
           rev22 = (y11) / dety ;

           if( abs( rev11 * y1 + rev12 * y2 ) > 0.5 ) begin 
             Vgp0 = Vgp0 - 0.5 * `Fn_Sgn( rev11 * y1 + rev12 * y2 ) ;
             phi_j0_DEP = phi_j0_DEP - 0.5 * `Fn_Sgn( rev21 * y1 + rev22 * y2 ) ;
           end else begin
             Vgp0 = Vgp0 - ( rev11 * y1 + rev12 * y2 ) ;
             phi_j0_DEP = phi_j0_DEP - ( rev21 * y1 + rev22 * y2 ) ;
           end
 
           if( abs(Vgp0 - Vgp0old) <= `ps_conv &&
                 abs(phi_j0_DEP - phi_j0_DEP_old) <= `ps_conv ) lp_s0=`lp_se_max + 1 ;

           Vgp0old = Vgp0 ;
           phi_j0_DEP_old = phi_j0_DEP ;
         end
         phi_j0_DEP_acc = phi_j0_DEP ;

         W_sub0 = UC_DEPTHN * NdepmpNsub ;
         phi_j0_DEP = C_2ESIpq_Nsub_inv * W_sub0 * W_sub0 + Vbsc - Vbi_DEP ;
         phi_b0_DEP = phi_j0_DEP + C_2ESIpq_Ndepm_inv * Tn2 ;
         phi_s0_DEP = phi_b0_DEP ;
         Psbmax = phi_b0_DEP ;
         Vgp1 = phi_b0_DEP ;
         if( Vgp > Vgp0 ) begin
           depmode = 1 ;
         end else if(Vgp > Vgp1 ) begin 
           depmode = 3 ;
         end else begin
           depmode = 2 ;
         end           

      /* else */
       end else begin
         Vgp0 = 0.0 ;
         Vgp1 = Vgp0 ;
         Psbmax = 0.0 ;
         Vds_maxb0 = Vgp0 ;
         W_b0 = W_bsub0 ;
         W_sub0 = W_b0 * NdepmpNsub ;
         phi_j0_DEP = C_2ESIpq_Nsub_inv * W_sub0 * W_sub0 + Vbsc - Vbi_DEP ;
         phi_b0_DEP = C_2ESIpq_Ndepm_inv * W_b0 * W_b0 + phi_j0_DEP ;
         phi_j0_DEP_acc = phi_j0_DEP ;
         if( Vgp > Vgp0 ) begin
           depmode = 1 ;
         end else begin
           depmode = 2 ;
         end
       end

       T1 = C_2ESI_q_Ndepm * ( Psbmax - ( - Pb2n + Vbsc)) ;
       if ( T1 > 0.0 ) begin
         vthn = - Pb2n + Vbsc - sqrt(T1) / Cox ;
       end else begin
         vthn = - Pb2n + Vbsc ;
       end

     //---------------------------------------------------*
     //* initial potential Ps0,Pb0,Pj0 calculated.
     //*------------------//

      /* accumulation region */
       if( Vgp > Vgp0 ) begin
         phi_j0_DEP = phi_j0_DEP_acc ;
         phi_b0_DEP = 0.0 ;
         phi_s0_DEP_ini = ln(afact * Vgp * Vgp) / (beta + 2.0 / Vgp) + phi_b0_DEP ;
         if( phi_s0_DEP_ini < Vds_maxb0 + ps_conv23 ) phi_s0_DEP_ini = Vds_maxb0 + ps_conv23 ;

      /* fully depleted region */
       end else if( Vgp > Vgp1 ) begin

         phi_s0_DEP_ini = phi_s0_DEP ;

      /* depletion and inversion region */
       end else begin

        /* depletion region */
         if( Vgp > vthn ) begin
           bfact = - 2.0 * afact * Vgp + beta ;
           cfact = afact * Vgp * Vgp - beta * phi_b0_DEP ;
           phi_b0_DEP_old = phi_b0_DEP ;

           phi_s0_DEP_ini = ( - bfact + sqrt(bfact * bfact - 4.0 * afact * cfact)) / 2.0 / afact ;
           if( phi_s0_DEP_ini > Psbmax - ps_conv3 ) phi_s0_DEP_ini = Psbmax - ps_conv3 ;

           W_s0 = sqrt(C_2ESIpq_Ndepm * (phi_b0_DEP - phi_s0_DEP_ini) ) ;
           W_b0 = sqrt(C_2ESIpq_Ndepm * (phi_b0_DEP - phi_j0_DEP) ) ;

           if( W_s0 + W_b0 > UC_DEPTHN ) begin
             for ( lp_s0 = 1 ; lp_s0 <= `lp_se_max + 1 ; lp_s0 = lp_s0 + 1 ) begin

               y0 = W_s0 + W_b0 - UC_DEPTHN ;

               dydPsm = `C_ESI / q_Ndepm / W_s0 + `C_ESI / q_Ndepm * ( 1.0 - (NdepmpNsub) / ( 1.0 + (NdepmpNsub))) / W_b0 ;

               if( abs(y0 / dydPsm) > 0.5 ) begin
                 phi_b0_DEP = phi_b0_DEP - 0.5 * `Fn_Sgn(y0 / dydPsm) ;
               end else begin
                 phi_b0_DEP = phi_b0_DEP - y0 / dydPsm ;
               end       
               if( (phi_b0_DEP - Vbsc + Vbi_DEP) < `epsm10 ) phi_b0_DEP=Vbsc - Vbi_DEP + `epsm10 ;

               cfact = afact * Vgp * Vgp - beta * phi_b0_DEP ;
               T1 = bfact * bfact - 4.0 * afact * cfact ;
               if( T1 > 0.0 ) begin
                 phi_s0_DEP_ini = ( - bfact + sqrt(T1)) / 2.0 / afact ;
               end else begin
                 phi_s0_DEP_ini = ( - bfact) / 2.0 / afact ;
               end

               if( phi_s0_DEP_ini > Psbmax ) phi_s0_DEP_ini = Psbmax ;
               if( phi_s0_DEP_ini > phi_b0_DEP ) begin
                 phi_s0_DEP_ini = phi_b0_DEP - ps_conv23 ;
                 lp_s0=`lp_se_max + 1 ;
               end

               W_s0 = sqrt(C_2ESIpq_Ndepm * (phi_b0_DEP - phi_s0_DEP_ini) ) ;
               phi_j0_DEP = ( NdepmpNsub * phi_b0_DEP + Vbsc - Vbi_DEP) / (1.0 + NdepmpNsub) ;
               W_b0 = sqrt(C_2ESIpq_Ndepm * (phi_b0_DEP - phi_j0_DEP) ) ;

              if( abs(phi_b0_DEP - phi_b0_DEP_old) <= 1.0e-8 ) lp_s0=`lp_se_max + 1 ;
               phi_b0_DEP_old = phi_b0_DEP ;
             end
           end

         end else begin

           afact3 = afact2 / exp(beta * Vbsc) ;
           phi_b0_DEP_old = phi_b0_DEP ;
           phi_s0_DEP_ini = ln(afact3 * Vgp * Vgp) / ( - beta + 2.0 / Vgp) ;
           W_s0 = sqrt(C_2ESIpq_Ndepm * (phi_b0_DEP - phi_s0_DEP_ini) ) ;
           W_b0 = sqrt(C_2ESIpq_Ndepm * (phi_b0_DEP - phi_j0_DEP) ) ;
           if( W_s0 + W_b0 >  UC_DEPTHN ) begin
             for ( lp_s0 = 1 ; lp_s0 <= lp_s0_max + 1 ; lp_s0 = lp_s0 + 1 ) begin

               y0 = W_s0 + W_b0 - UC_DEPTHN ;
               dydPsm = `C_ESI / q_Ndepm / W_s0 + `C_ESI / q_Ndepm * ( 1.0 - (NdepmpNsub) / ( 1.0 + (NdepmpNsub))) / W_b0 ;

               if( abs(y0 / dydPsm) > 0.5 ) begin
                 phi_b0_DEP = phi_b0_DEP - 0.5 * `Fn_Sgn(y0 / dydPsm) ;
               end else begin
                 phi_b0_DEP = phi_b0_DEP - y0 / dydPsm ;
               end
               if( (phi_b0_DEP - Vbsc + Vbi_DEP) < `epsm10 ) phi_b0_DEP=Vbsc - Vbi_DEP + `epsm10 ;

               W_s0 = sqrt(C_2ESIpq_Ndepm * (phi_b0_DEP - phi_s0_DEP_ini) ) ;
               phi_j0_DEP = ( NdepmpNsub * phi_b0_DEP + Vbsc - Vbi_DEP) / (1.0 + NdepmpNsub) ;
               W_b0 = sqrt(C_2ESIpq_Ndepm * (phi_b0_DEP - phi_j0_DEP) ) ;

               if( abs(phi_b0_DEP - phi_b0_DEP_old) <= 1.0e-5 ) lp_s0=lp_s0_max + 1 ;
               phi_b0_DEP_old = phi_b0_DEP ;
             end

           end
         end // end of phi_b0_DEP loop //

       end

       phi_b0_DEP_ini = phi_b0_DEP ;

       /*               */
       /* solve poisson at source side  */
       /*               */

       sm_delta = 0.12;

       flg_conv = 0 ;

       phi_s0_DEP = phi_s0_DEP_ini ;
       phi_b0_DEP = phi_b0_DEP_ini ;

       phi_s0_DEP_old = phi_s0_DEP ;
       phi_b0_DEP_old = phi_b0_DEP ;

       for ( lp_s0 = 1 ; lp_s0 <= `lp_se_max + 1 ; lp_s0 = lp_s0 + 1 ) begin

         phi_j0_DEP  = NdepmpNsub_inv1 * (NdepmpNsub * phi_b0_DEP + Vbsc - Vbi_DEP) ;
         phi_j0_DEP_dPb = NdepmpNsub_inv1 * NdepmpNsub ;

         T1 = phi_b0_DEP - phi_j0_DEP ;
         `Fn_SL_CP( T2 , T1 , 0.0 , 0.1, 2 , T0 )
         W_b0 = sqrt(C_2ESIpq_Ndepm * (T2) ) ;
         `Fn_SU_CP( W_b0 , W_b0 , UC_DEPTHN , 1e-8, 2 , T3 )

         W_sub0 = sqrt(C_2ESIpq_Nsub * (phi_j0_DEP - Vbsc + Vbi_DEP) ) ;
         Q_b0_dep = W_b0 * q_Ndepm ;
         Q_b0_dep_dPb = `C_ESI / W_b0 * T0 * T3 ;
         Q_b0_dep_dPd = - `C_ESI / W_b0 * T0 * T3 ;
         Q_sub0_dep = - W_sub0 * q_Nsub ;
         Q_sub0_dep_dPd = - `C_ESI / W_sub0 ;

         T1 = 8.0 * q_Ndepm_esi * Tn2 ;
         phib_ref = (4.0 * phi_j0_DEP * phi_j0_DEP * C_ESI2 - 8.0 * phi_j0_DEP * C_ESI2 * phi_s0_DEP
               + 4.0 * C_ESI2 * phi_s0_DEP * phi_s0_DEP
               + 4.0 * phi_j0_DEP * q_Ndepm_esi * Tn2
               + 4.0 * phi_s0_DEP * q_Ndepm_esi * Tn2
               + Ndepm2 * C_QE2 * Tn2 * Tn2) / T1 ;
         phib_ref_dPs = ( - 8.0 * phi_j0_DEP * C_ESI2 + 4.0 * C_ESI2 * phi_s0_DEP * 2.0
               + 4.0 * q_Ndepm_esi * Tn2) / T1 ;
         phib_ref_dPd = (4.0 * phi_j0_DEP * C_ESI2 * 2.0 - 8.0 * C_ESI2 * phi_s0_DEP
               + 4.0 * q_Ndepm_esi * Tn2) / T1 ;

         T1 = beta * (phi_s0_DEP - phi_b0_DEP) ;
         T2 = exp(T1) ;
         if( phi_s0_DEP >= phi_b0_DEP ) begin
           Q_s0 = - cnst0 * sqrt(T2 - 1.0 - T1 + 1e-15) ;
           Q_s0_dPs = 0.5 * cnst0 * cnst0 / Q_s0 * (beta * T2 - beta ) ;
           Q_s0_dPb = - Q_s0_dPs ;
         end else begin
           T3 = exp( - beta * (phi_s0_DEP - Vbsc)) ;
           T4 = exp( - beta * (phi_b0_DEP - Vbsc)) ;
           Q_s0 = cnst0 * sqrt(T2 - 1.0 - T1 + 1e-15 + cnst1 * (T3 - T4) ) ;
           T5 = 0.5 * cnst0 * cnst0 / Q_s0 ;
           Q_s0_dPs = T5 * (beta * T2 - beta + cnst1 * ( - beta * T3) ) ;
           Q_s0_dPb = T5 * ( - beta * T2 + beta + cnst1 * beta * T4 ) ;
         end

         `Fn_SU_CP( T1 , phib_ref , Vds_maxb0 , sm_delta, 4 , T0 )

         y1 = phi_b0_DEP - T1 ; 
         y11 = - phib_ref_dPs * T0 ;
         y12 = 1.0 - phib_ref_dPd * phi_j0_DEP_dPb * T0 ;

         y2 = Cox * (Vgp - phi_s0_DEP) + Q_s0 + Q_b0_dep + Q_sub0_dep ;
         y21 = - Cox + Q_s0_dPs ;
         y22 = Q_s0_dPb + Q_b0_dep_dPb + Q_b0_dep_dPd * phi_j0_DEP_dPb + Q_sub0_dep_dPd * phi_j0_DEP_dPb ;

         dety = y11 * y22 - y21 * y12;
         rev11 = (y22) / dety ;
         rev12 = ( - y12) / dety ;
         rev21 = ( - y21) / dety ;
         rev22 = (y11) / dety ;
         if( abs( rev21 * y1 + rev22 * y2 ) > 0.5 ) begin
           phi_s0_DEP = phi_s0_DEP - 0.5 * `Fn_Sgn( rev11 * y1 + rev12 * y2 ) ;
           phi_b0_DEP = phi_b0_DEP - 0.5 * `Fn_Sgn( rev21 * y1 + rev22 * y2 ) ;
         end else begin
           phi_s0_DEP = phi_s0_DEP - ( rev11 * y1 + rev12 * y2 ) ;
           phi_b0_DEP = phi_b0_DEP - ( rev21 * y1 + rev22 * y2 ) ;
         end

         if( abs(phi_s0_DEP - phi_s0_DEP_old) <= `ps_conv &&  abs(phi_b0_DEP - phi_b0_DEP_old) <= `ps_conv ) begin
               lp_s0=`lp_se_max + 1 ;
               flg_conv = 1 ;
         end

         phi_s0_DEP_old = phi_s0_DEP ;
         phi_b0_DEP_old = phi_b0_DEP ;

       end

       if( flg_conv == 0 ) begin
         $write( "*** warning(HiSIM(%m)): Went Over Iteration Maximum(Ps0)\n") ;
         $write( " Vbse   = %7.3f Vdse = %7.3f Vgse = %7.3f\n" ,Vbse , Vdse , Vgse ) ;
       end

       if( W_bsub0 > UC_DEPTHN && depmode !=2 ) begin
         `Fn_SU_CP(phi_b0_DEP , phi_b0_DEP , phi_s0_DEP , 0.04, 2 , T1 )
       end 

       phi_j0_DEP  = NdepmpNsub_inv1 * (NdepmpNsub * phi_b0_DEP + Vbsc - Vbi_DEP) ;

       T1 = beta * (phi_s0_DEP - phi_b0_DEP) ;
       T2 = exp(T1) ;
       if( phi_s0_DEP >= phi_b0_DEP ) begin
         Q_s0 = - cnst0 * sqrt(T2 - 1.0e0 - T1 + 1e-15 ) ;
         Q_n0 = Q_s0 ;

         Q_s0_dep = 0.0 ;
         Q_sub0 = 0.0 ;

         W_b0 = sqrt(C_2ESIpq_Ndepm * (phi_b0_DEP - phi_j0_DEP) ) ;
         `Fn_SU_CP( W_b0 , W_b0 , UC_DEPTHN , 5e-8, 2 , T3 )
         W_sub0 = sqrt(C_2ESIpq_Nsub * (phi_j0_DEP - Vbsc + Vbi_DEP) ) ;
         Q_b0_dep = W_b0 * q_Ndepm ;
         Q_sub0_dep = - W_sub0 * q_Nsub ;
       end else begin
         T3 = exp( - beta * (phi_s0_DEP - Vbsc)) ;
         T4 = exp( - beta * (phi_b0_DEP - Vbsc)) ;
         Q_s0 = cnst0 * sqrt(T2 - 1.0 - T1 + cnst1 * (T3 - T4) + 1e-15) ;

         if( W_bsub0 > UC_DEPTHN && depmode !=2 ) begin
           Q_sub0 = 0.0 ;
           Q_s0_dep = 0.0 ;
         end else begin
           T3 = cnst0 * sqrt( - T1
               + cnst1 * (exp( - beta * (phi_s0_DEP - Vbsc)) - exp( - beta * (phi_b0_DEP - Vbsc)))) ;
           Q_sub0 = T3 - cnst0 * sqrt( - T1)  ;
           Q_s0_dep = cnst0 * sqrt(T2 - 1.0e0 - T1 + 1e-15) ;
         end

         Q_n0 = 0.0 ;

         T1 = phi_b0_DEP - phi_j0_DEP ;
         `Fn_SL_CP( T2 , T1 , 0.0 , 0.1, 2 , T0 )
         W_b0 = sqrt(C_2ESIpq_Ndepm * (T2) ) ;
         `Fn_SU_CP( W_b0 , W_b0 , UC_DEPTHN , 5e-8, 2 , T3 )
         W_sub0 = sqrt(C_2ESIpq_Nsub * (phi_j0_DEP - Vbsc + Vbi_DEP) ) ;
         Q_b0_dep = W_b0 * q_Ndepm ;
         Q_sub0_dep = - W_sub0 * q_Nsub ;

       end // (phi_s0_DEP >= phi_b0_DEP)

       T1 = phi_b0_DEP - phi_j0_DEP ;
       `Fn_SL_CP( T2 , T1 , 0.0 , 0.1, 2 , T0 )
       W_b0 = sqrt(C_2ESIpq_Ndepm * (T2) ) ;
       `Fn_SU_CP( W_b0 , W_b0 , UC_DEPTHN , 1e-8, 2 , T3 )

       T1 = phi_b0_DEP - phi_s0_DEP ;
//     `Fn_SL_CP( T2 , T1 , 0.0 , 0.05, 2 , T0 )
       `Fn_SL_CP( T2 , T1 , 0.0 , 0.1, 2 , T0 )
       W_s0 = sqrt(C_2ESIpq_Ndepm * (T2) ) ;

       T1 = UC_DEPTHN - W_b0 - W_s0 ;
       `Fn_SL_CP( W_res0 , T1 , 1e-20 , 1.0e-14, 2 , T0 )

       Qn_res0 = - W_res0 * q_Ndepm ;

       if( W_bsub0 > UC_DEPTHN && depmode !=2 ) begin
         `Fn_SU_CP(T2 , phi_s0_DEP , Vds_maxb0 , 0.8, 2 , T1 )
       end else begin
         `Fn_SU_CP( T2 , phib_ref , Vds_maxb0 , 0.8, 2 , T0 )
       end

       Qn_bac = - `C_QE * UC_NDEPM * exp(beta * (T2 - Vds_maxb0)) * W_b0 ;


       `Fn_SL_CP( T2 , (phi_s0_DEP - Vds_maxb0) , 0.0, `Ps_delta, 2 , T0 )
       T4 = exp(beta * T2) - 1.0 - beta * T2 + 1e-15 ;
       Q_n0_cur = - cnst0 * sqrt(T4) ;

       T4 = exp(beta * (`Ps_delta0)) - 1.0 - beta * (`Ps_delta0) ;
       Qn_delta = cnst0 * sqrt(T4) ;

     //-----------------------------------------------------------*
     //* Start point of Psl(= Ps0 + Pds) calculation.(label)
     //*-----------------//

// start_of_Psl:

       // Vdseff(begin) //
       Vdsorg = Vds ;

       
       if( Vds > 1e-3 ) begin
         T2 = q_Ndepm_esi / ( Cox * Cox ) ;
         T0 = Vgp + `DEPQFN1 - beta_inv - Vbsz ;
         T4 = 1.0e0 + 2.0e0 / T2 * T0 ;
         `Fn_SL_CP( T9 , T4 , 0 , `DEPQFN_dlt , 2 , T0 )
         T9 = T9 + `Small ;
         T3 = sqrt( T9 ) ;
         T4 = T2 * ( 1.0e0 - T3 ) ;
         T10 = Vgp + `DEPQFN1 + T4 ;

         `Fn_SL_CP( T10 , T10 , `DEPQFN3, 0.20, 4 , T0 )

         T1 = Vds / T10 ;
         T2 = `Fn_Pow( T1 , DDLTe-1.0e0 ) ;
         T7 = T2 * T1 ;
         T3 = 1.0 + T7 ;
         T4 = `Fn_Pow( T3 , 1.0 / DDLTe-1.0 ) ;
         T6 = T4 * T3  ;
         Vdseff = Vds / T6 ;

         `Fn_SL_CP( Vgpp , Vgp , 0 , 0.5, 2 , T0 )
 
         T1 = Vgpp * 0.8 ;
         `Fn_SU_CP( Vds , Vdseff , Vgpp , T1, 2 , T0 )

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
         Q_bL_dep = Q_b0_dep ;
         Q_subL_dep = Q_sub0_dep ;
         Q_nL_cur = Q_n0_cur ;

       end else begin

         W_bsubL = sqrt(C_2ESIpq_Ndepm * EF_NSUBC / (EF_NSUBC + UC_NDEPM) * (Vds - Vbsc + Vbi_DEP)) ;

       //---------------------------------------------------*
       //* region judgement  
       //*------------------//

        /* fully depleted case */
         if( W_bsubL > UC_DEPTHN ) begin
         
           Vgp0 = Vds ;
           W_bL = UC_DEPTHN ;
           phi_bL_DEP = Vds ;
           Vds_maxbL = Vds ;
           phi_jL_DEP = phi_bL_DEP - C_2ESIpq_Ndepm_inv * W_bL * W_bL ;

           Vgp0old = Vgp0 ;
           phi_jL_DEP_old = phi_jL_DEP ; 

           Q_bL_dep = W_bL * q_Ndepm ;

           for ( lp_s0 = 1 ; lp_s0 <= `lp_se_max + 1 ; lp_s0 = lp_s0 + 1 ) begin

             W_bL = sqrt(C_2ESIpq_Ndepm * (phi_bL_DEP - phi_jL_DEP) ) ;
             `Fn_SU_CP( W_bL , W_bL , UC_DEPTHN , 1e-8, 2 , T0 )
             W_subL = sqrt(C_2ESIpq_Nsub * (phi_jL_DEP - Vbsc + Vbi_DEP) ) ; 

             Q_bL_dep = W_bL * q_Ndepm ;
             Q_bL_dep_dPd = - `C_ESI / W_bL * T0 ;
             Q_subL_dep = - W_subL * q_Nsub ;
             Q_subL_dep_dPd = - `C_ESI / W_subL ;

             y1 = Cox * (Vgp0 - phi_bL_DEP) + Q_bL_dep + Q_subL_dep ;
             y11 = Cox ;
             y12 = Q_bL_dep_dPd + Q_subL_dep_dPd ;

             y2 = phi_jL_DEP - NdepmpNsub_inv1 * (NdepmpNsub * phi_bL_DEP + Vbsc - Vbi_DEP) ;
             y21 = 0.0 ;
             y22 = 1.0 ;

             dety = y11 * y22 - y21 * y12;
             rev11 = (y22) / dety ;
             rev12 = ( - y12) / dety ;
             rev21 = ( - y21) / dety ;
             rev22 = (y11) / dety ;

             if( abs( rev11 * y1 + rev12 * y2 ) > 0.5 ) begin
               Vgp0 = Vgp0 - 0.5 * `Fn_Sgn( rev11 * y1 + rev12 * y2 ) ;
               phi_jL_DEP = phi_jL_DEP - 0.5 * `Fn_Sgn( rev21 * y1 + rev22 * y2 ) ;
             end else begin
               Vgp0 = Vgp0 - ( rev11 * y1 + rev12 * y2 ) ;
               phi_jL_DEP = phi_jL_DEP - ( rev21 * y1 + rev22 * y2 ) ;
             end

             if( abs(Vgp0 - Vgp0old) <= `ps_conv &&
                 abs(phi_jL_DEP - phi_jL_DEP_old) <= `ps_conv ) lp_s0=`lp_se_max + 1 ;
  
             Vgp0old = Vgp0 ;
             phi_jL_DEP_old = phi_jL_DEP ;
           end
           phi_jL_DEP_acc = phi_jL_DEP ;

           W_subL = UC_DEPTHN * NdepmpNsub ;
           phi_jL_DEP = C_2ESIpq_Nsub_inv * W_subL * W_subL + Vbsc - Vbi_DEP ;
           phi_bL_DEP = phi_jL_DEP + C_2ESIpq_Ndepm_inv * Tn2 ;
           phi_sL_DEP = phi_bL_DEP ;
           Psbmax = phi_bL_DEP ;
           Vgp1 = phi_bL_DEP ;
           if( Vgp > Vgp0 ) begin
             depmode = 1 ;
           end else if(Vgp > Vgp1 ) begin
             depmode = 3 ;
           end else begin
             depmode = 2 ;
           end

        /* else */
         end else begin
           Vgp0 = Vds ;
           Vgp1 = Vgp0 ; 
           Psbmax = Vds ;
           Vds_maxbL = Vds ;
           W_bL = W_bsubL ;
           W_subL = W_bL * NdepmpNsub ;
           phi_jL_DEP = C_2ESIpq_Nsub_inv * W_subL * W_subL + Vbsc - Vbi_DEP ;
           phi_bL_DEP = C_2ESIpq_Ndepm_inv * W_bL * W_bL + phi_jL_DEP ;
           phi_jL_DEP_acc = phi_jL_DEP ;
           if( Vgp > Vgp0 ) begin
             depmode = 1 ;
           end else begin
             depmode = 2 ;
           end

         end
 
         T1 = C_2ESI_q_Ndepm * ( Psbmax - ( - Pb2n + Vbsc)) ;
         if ( T1 > 0.0 ) begin
           vthn = - Pb2n + Vbsc - sqrt(T1) / Cox ;
         end else begin
           vthn = - Pb2n + Vbsc ;
         end

       //---------------------------------------------------*
       //* initial potential Psl,Pbl,Pjl calculated.
       //*------------------//


        /* accumulation region */
         if( Vgp > Vgp0 ) begin
           phi_jL_DEP = phi_jL_DEP_acc ;
           phi_bL_DEP = Vds ; 
           phi_sL_DEP_ini = ln(afact * Vgp * Vgp) / (beta + 2.0 / Vgp) + Vds ;

           if( phi_sL_DEP_ini < Vds_maxbL + ps_conv23 ) phi_sL_DEP_ini = Vds_maxbL + ps_conv23 ;

        /* fully depleted region */
         end else if( Vgp > Vgp1 ) begin

           phi_sL_DEP_ini = phi_sL_DEP ;

        /* depletion & inversion */

         end else begin

          /* depletion */
           if( Vgp > vthn ) begin
             bfact = - 2.0 * afact * Vgp + beta ;
             cfact = afact * Vgp * Vgp - beta * phi_bL_DEP ;
             phi_bL_DEP_old = phi_bL_DEP ;
             phi_sL_DEP_ini = ( - bfact + sqrt(bfact * bfact - 4.0 * afact * cfact)) / 2.0 / afact ;
             if( phi_sL_DEP_ini > Psbmax - ps_conv23 ) phi_sL_DEP_ini = Psbmax - ps_conv23 ;
             W_sL = sqrt(C_2ESIpq_Ndepm * (phi_bL_DEP - phi_sL_DEP_ini) ) ;
             W_bL = sqrt(C_2ESIpq_Ndepm * (phi_bL_DEP - phi_jL_DEP) ) ;

             if( W_sL + W_bL > UC_DEPTHN ) begin
               for ( lp_s0 = 1 ; lp_s0 <= `lp_se_max + 1 ; lp_s0 = lp_s0 + 1 ) begin

                 y0 = W_sL + W_bL - UC_DEPTHN ;

                 dydPsm = `C_ESI / q_Ndepm / W_sL + `C_ESI / q_Ndepm * ( 1.0 - (NdepmpNsub) / ( 1.0 + (NdepmpNsub))) / W_bL ;

                 if( abs(y0 / dydPsm) > 0.5 ) begin
                   phi_bL_DEP = phi_bL_DEP - 0.5 * `Fn_Sgn(y0 / dydPsm) ;
                 end else begin
                   phi_bL_DEP = phi_bL_DEP - y0 / dydPsm ;
                 end

                 if( (phi_bL_DEP - Vbsc + Vbi_DEP) < `epsm10 ) phi_bL_DEP=Vbsc - Vbi_DEP + `epsm10 ;

                 cfact = afact * Vgp * Vgp - beta * phi_bL_DEP ;
                 T1 = bfact * bfact - 4.0 * afact * cfact ;
                 if( T1 > 0.0 ) begin
                   phi_sL_DEP_ini = ( - bfact + sqrt(T1)) / 2.0 / afact ;
                 end else begin
                   phi_sL_DEP_ini = ( - bfact) / 2.0 / afact ;
                 end

                 if( phi_sL_DEP_ini > Psbmax ) phi_sL_DEP_ini = Psbmax ;
                 if( phi_sL_DEP_ini > phi_bL_DEP ) begin
                   phi_sL_DEP_ini = phi_bL_DEP - ps_conv23 ;
                   lp_s0=`lp_se_max + 1 ;
                 end
                 W_sL = sqrt(C_2ESIpq_Ndepm * (phi_bL_DEP - phi_sL_DEP_ini) ) ;
                 phi_jL_DEP = ( NdepmpNsub * phi_bL_DEP + Vbsc - Vbi_DEP) / (1.0 + NdepmpNsub) ;
                 W_bL = sqrt(C_2ESIpq_Ndepm * (phi_bL_DEP - phi_jL_DEP) ) ;

                 if( abs(phi_bL_DEP - phi_bL_DEP_old) <= 1.0e-8 ) lp_s0=`lp_se_max + 1 ;
                 phi_bL_DEP_old = phi_bL_DEP ;
               end
             end

          /* inversion */
           end else begin

             phi_bL_DEP = phi_b0_DEP ;
             phi_jL_DEP = phi_j0_DEP ;
             phi_sL_DEP_ini = phi_s0_DEP ;

           end

         end

         phi_b0_DEP_ini = phi_bL_DEP ;

         /*               */
         /* solve poisson  at drain side */
         /*               */

         flg_conv = 0 ;

         phi_sL_DEP = phi_sL_DEP_ini ;
         phi_bL_DEP = phi_b0_DEP_ini ;

         phi_sL_DEP_old = phi_sL_DEP ;
         phi_bL_DEP_old = phi_bL_DEP ;

         for ( lp_s0 = 1 ; lp_s0 <= `lp_se_max + 1 ; lp_s0 = lp_s0 + 1 ) begin

           phi_jL_DEP  = NdepmpNsub_inv1 * (NdepmpNsub * phi_bL_DEP + Vbsc - Vbi_DEP) ;
           phi_jL_DEP_dPb = NdepmpNsub_inv1 * NdepmpNsub ;

           T1 = phi_bL_DEP - phi_jL_DEP ;
           `Fn_SL_CP( T2 , T1 , 0.0 , 0.1, 2 , T0 )
           W_bL = sqrt(C_2ESIpq_Ndepm * (T2) ) ;
           `Fn_SU_CP( W_bL , W_bL , UC_DEPTHN , 1e-8, 2 , T3 )

           W_subL = sqrt(C_2ESIpq_Nsub * (phi_jL_DEP - Vbsc + Vbi_DEP) ) ;
           Q_bL_dep = W_bL * q_Ndepm ;
           Q_bL_dep_dPb = `C_ESI / W_bL * T0 * T3 ;
           Q_bL_dep_dPd = - `C_ESI / W_bL * T0 * T3 ;
           Q_subL_dep = - W_subL * q_Nsub ;
           Q_subL_dep_dPd = - `C_ESI / W_subL ;

           T1 = 8.0 * q_Ndepm_esi * Tn2 ;
           phib_ref = (4.0 * phi_jL_DEP * phi_jL_DEP * C_ESI2 - 8.0 * phi_jL_DEP * C_ESI2 * phi_sL_DEP
               + 4.0 * C_ESI2 * phi_sL_DEP * phi_sL_DEP
               + 4.0 * phi_jL_DEP * q_Ndepm_esi * Tn2
               + 4.0 * phi_sL_DEP * q_Ndepm_esi * Tn2
               + Ndepm2 * C_QE2 * Tn2 * Tn2) / T1 ;
           phib_ref_dPs = ( - 8.0 * phi_jL_DEP * C_ESI2 + 4.0 * C_ESI2 * phi_sL_DEP * 2.0
               + 4.0 * q_Ndepm_esi * Tn2) / T1 ;
           phib_ref_dPd = (4.0 * phi_jL_DEP * C_ESI2 * 2.0 - 8.0 * C_ESI2 * phi_sL_DEP
               + 4.0 * q_Ndepm_esi * Tn2) / T1 ;

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
        
          `Fn_SU_CP( T1 , phib_ref , Vds_maxbL , sm_delta, 4 , T0 )

           y1 = phi_bL_DEP - T1 ;
           y11 = - phib_ref_dPs * T0 ;
           y12 = 1.0 - phib_ref_dPd * phi_jL_DEP_dPb * T0 ;

           y2 = Cox * (Vgp - phi_sL_DEP) + Q_sL + Q_bL_dep + Q_subL_dep ;
           y21 = - Cox + Q_sL_dPs ;
           y22 = Q_sL_dPb + Q_bL_dep_dPb + Q_bL_dep_dPd * phi_jL_DEP_dPb + Q_subL_dep_dPd * phi_jL_DEP_dPb ;

           dety = y11 * y22 - y21 * y12;
           rev11 = (y22) / dety ;
           rev12 = ( - y12) / dety ;
           rev21 = ( - y21) / dety ;
           rev22 = (y11) / dety ;
           if( abs( rev21 * y1 + rev22 * y2 ) > 0.2 ) begin
             phi_sL_DEP = phi_sL_DEP - 0.2 * `Fn_Sgn( rev11 * y1 + rev12 * y2 ) ;
             phi_bL_DEP = phi_bL_DEP - 0.2 * `Fn_Sgn( rev21 * y1 + rev22 * y2 ) ;
           end else begin
             phi_sL_DEP = phi_sL_DEP - ( rev11 * y1 + rev12 * y2 ) ;
             phi_bL_DEP = phi_bL_DEP - ( rev21 * y1 + rev22 * y2 ) ;
           end

           if( abs(phi_sL_DEP - phi_sL_DEP_old) <= `ps_conv &&  abs(phi_bL_DEP - phi_bL_DEP_old) <= `ps_conv ) begin
               lp_s0=`lp_se_max + 1 ;
               flg_conv = 1 ;
           end

           phi_sL_DEP_old = phi_sL_DEP ;
           phi_bL_DEP_old = phi_bL_DEP ;

         end

         if( flg_conv == 0 ) begin
           $write( "*** warning(HiSIM(%m)): Went Over Iteration Maximum(Psl)\n") ;
           $write( " Vbse   = %7.3f Vdse = %7.3f Vgse = %7.3f\n" ,Vbse , Vdse , Vgse ) ;
         end

         if( W_bsubL > UC_DEPTHN && depmode !=2 ) begin
           `Fn_SU_CP(phi_bL_DEP , phi_bL_DEP , phi_sL_DEP , 0.04, 2 , T1 )
         end

         phi_jL_DEP  = NdepmpNsub_inv1 * (NdepmpNsub * phi_bL_DEP + Vbsc - Vbi_DEP) ;

         T1 = beta * (phi_sL_DEP - phi_bL_DEP) ;
         T2 = exp(T1) ;
         if( phi_sL_DEP >= phi_bL_DEP ) begin
           Q_sL = - cnst0 * sqrt(T2 - 1.0 - T1 + 1e-15 ) ;
           Q_nL = Q_sL ;

           Q_sL_dep = 0.0 ;
           Q_subL = 0.0 ;

           W_bL = sqrt(C_2ESIpq_Ndepm * (phi_bL_DEP - phi_jL_DEP) ) ;
           `Fn_SU_CP( W_bL , W_bL , UC_DEPTHN , 1e-8, 2 , T3 )
           W_subL = sqrt(C_2ESIpq_Nsub * (phi_jL_DEP - Vbsc + Vbi_DEP) ) ;
           Q_bL_dep = W_bL * q_Ndepm ;
           Q_subL_dep = - W_subL * q_Nsub ;

         end else begin
           T3 = exp( - beta * (phi_sL_DEP - Vbsc)) ;
           T4 = exp( - beta * (phi_bL_DEP - Vbsc)) ;
           Q_sL = cnst0 * sqrt(T2 - 1.0 - T1 + cnst1 * (T3 - T4) + 1e-15) ;

           if( W_bsubL > UC_DEPTHN && depmode !=2 ) begin
             Q_subL = 0.0 ;
             Q_sL_dep = 0.0 ;
           end else begin
             T3 = cnst0 * sqrt( - T1
                 + cnst1 * (exp( - beta * (phi_sL_DEP - Vbsc)) - exp( - beta * (phi_bL_DEP - Vbsc))) ) ;
             Q_subL = T3 - cnst0 * sqrt( - T1)  ;
             Q_sL_dep = cnst0 * sqrt(T2 - 1.0 - T1 + 1e-15) ;
           end
           Q_nL = 0.0 ;

           T1 = phi_bL_DEP - phi_jL_DEP ;
           `Fn_SL_CP( T2 , T1 , 0.0 , 0.1, 2 , T0 )
           W_bL = sqrt(C_2ESIpq_Ndepm * (T2) ) ;
           `Fn_SU_CP( W_bL , W_bL , UC_DEPTHN , 1e-8, 2 , T3 )
           W_subL = sqrt(C_2ESIpq_Nsub * (phi_jL_DEP - Vbsc + Vbi_DEP) ) ;
           Q_bL_dep = W_bL * q_Ndepm ;
           Q_subL_dep = - W_subL * q_Nsub ;

         end


         `Fn_SL_CP( T2 , (phi_sL_DEP - Vds_maxbL) , 0.0, `Ps_delta , 2 , T0 )
         T4 = exp(beta * T2) - 1.0 - beta * T2 + 1e-15 ;
         Q_nL_cur = - cnst0 * sqrt(T4) ;

       end

     //---------------------------------------------------*
     //* Assign Pds.
     //*-----------------//
       Ps0 = phi_s0_DEP ;
       Psl = phi_sL_DEP ;
       Pds = phi_sL_DEP - phi_s0_DEP + `epsm10 ;

       if( Pds < 0.0 ) begin // take care of numerical noise //
         Pds = 0.0 ;
         phi_sL_DEP = phi_s0_DEP ;
         Idd = 0.0 ;
       end else begin

         T2 = - (Q_s0 + Q_sL) ;
         `Fn_SL_CP( Qn_drift , T2 , 0.0, Qn_delta , 2 , T0 )
         Idd_drift  =   beta * (Qn_drift) / 2.0 * Pds ;
         Idd_diffu  = - ( - Q_nL_cur + Q_n0_cur);

         Idd = Idd_drift + Idd_diffu ;
         Idd = (Idd < 0.0) ? 0.0 : Idd ; 

       end

       Qiu = - Q_n0_cur ;

       Lch = Leff ;
     //-----------------------------------------------------------*
     //* Muun : surface universal mobility.  (CGS unit)
     //*-----------------//
       T2 = ninv_o_esi / `C_m2cm ;
       T0 = Ninvde ;
       Pdsz = sqrt(Pds * Pds + VZADD0) - sqrt(VZADD0) ;
       T4 = 1.0 + (Pdsz) * T0 ;   // for DC symmetry

       T5 =  T2 * Qiu ;
       T3 = T5 / T4 ;
       Eeff = T3 ;
       T5 = `Fn_Pow( Eeff , MUEPH0 - 1.0e0 ) ;
       T8 = T5 * Eeff ;
       T7 = `Fn_Pow( Eeff , muesr - 1.0e0 ) ;
       T6 = T7 * Eeff ;
       T9 = `C_QE * `C_m2cm_p2 ;
       Rns = Qiu / T9 ;

       T1 = 1.0e0 / ( UC_MUECB0 + UC_MUECB1 * Rns / 1.0e11 )
          + mphn0 * T8 + T6 / UC_MUESR1 ;

       Muun = 1.0e0 / T1 ;

     //  Change to MKS unit //
       Muun = Muun / `C_m2cm_p2 ;
    
     //-----------------------------------------------------------*
     //* Mu : mobility
     //*-----------------//
       T2 = beta * (Qiu + `Small) * Lch ;
       T1 = 1.0e0 / T2 ;
       TY = Idd * T1 ;
       T2 = 0.2 * Vmaxe / Muun ;
       Ey = sqrt( TY * TY + T2 * T2 ) ;
       T4 = 1.0 / Ey ;
       Em = Muun * Ey ;
       T1  = Em / Vmaxe ;
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

       Ey_suf = Ey ;
     //-----------------------------------------------------------*
     //*  resistor region current.  (CGS unit)
     //*-----------------//

       if( Vdsorg > 1e-3 ) begin

         T2 = q_Ndepm_esi / ( Cox * Cox ) ;
         T0 = Vgp + UC_DEPVDSEF1 - beta_inv - Vbsz ;
         T4 = 1.0e0 + 2.0e0 / T2 * T0 ;
         `Fn_SL_CP( T9 , T4 , 0 , `DEPQFN_dlt , 2 , T0 )

         T9 = T9 + `Small ;
         T3 = sqrt( T9 ) ;
         T4 = T2 * ( 1.0e0 - T3 ) ;
         T10 = Vgp + UC_DEPVDSEF1 + T4 ;
         T10 = T10 * UC_DEPVDSEF2 ;

         `Fn_SL_CP( T10 , T10 , UC_DEPLEAK, 1.00, 4 , T0 )

         T1 = Vdsorg / T10 ;
         T2 = `Fn_Pow( T1 , DDLTe-1.0e0 ) ;
         T7 = T2 * T1 ;
         T3 = 1.0 + T7 ;
         T4 = `Fn_Pow( T3 , 1.0 / DDLTe-1.0 ) ;
         T6 = T4 * T3  ;
         Vdseff0 = Vdsorg / T6 ;

       end else begin
         Vdseff0 = Vdsorg ;
       end


     //-----------------------------------------------------------*
     //*  resistor region universal mobility.  (CGS unit)
     //*-----------------//
       T0 = Ninvde ;
       Pdsz = sqrt(Pds * Pds + VZADD0) - sqrt(VZADD0) ;
       T4 = 1.0 + (Pdsz) * T0 ;   // for DC symmetry

       Qiu = - Qn_res0 ;
       T5 =  Qiu ;
       T3 = T5 / T4 ;
       Eeff = T3 ;
       T5 = `Fn_Pow( Eeff , DEPMUEPH0 - 1.0e0 ) ;
       T8 = T5 * Eeff ;
       T9 = `C_QE * `C_m2cm_p2 ;
       Rns = Qiu / T9 ;

       T1 = 1.0e0 / ( UC_DEPMUE0 + UC_DEPMUE1 * Rns / 1.0e11 )
          + depmphn0 * T8  ;

       Muun = 1.0e0 / T1 ;

     //  Change to MKS unit //
       Muun = Muun / `C_m2cm_p2 ;

       Edri = Vdseff0 / Lch ;

       T1 = Muun * Edri / UC_DEPVMAX ;
       T2 = `Fn_Pow(T1,DEPBB) ;
       T3 = 1.0 + T2 ;
       T4 = `Fn_Pow(T3,1.0 / DEPBB) ;

       Mu_res = Muun / T4 ;
       Ids_res = weff_nf * ( - Qn_res0) * Mu_res * Edri ;

     //-----------------------------------------------------------*
     //*  back region universal mobility.  (CGS unit)
     //*-----------------//

       T0 = Ninvde ;
       Pdsz = sqrt(Pds * Pds + VZADD0) - sqrt(VZADD0) ;
       T4 = 1.0 + (Pdsz) * T0 ;   // for DC symmetry

       Qiu = - Qn_bac ;
       T5 =  Qiu ;
       T3 = T5 / T4 ;
       Eeff = T3 ;
       T5 = `Fn_Pow( Eeff , DEPMUEPH0 - 1.0e0 ) ;
       T8 = T5 * Eeff ;
       T9 = `C_QE * `C_m2cm_p2 ;
       Rns = Qiu / T9 ;

       T1 = 1.0e0 / ( UC_DEPMUEBACK0 + UC_DEPMUEBACK1 * Rns / 1.0e11 )
          + depmphn0 * T8  ;

       Muun = 1.0e0 / T1 ;

     //  Change to MKS unit //
       Muun = Muun / `C_m2cm_p2 ;

       Edri = Vdseff0 / Lch ;

       T1 = Muun * Edri / UC_DEPVMAX ;
       T2 = `Fn_Pow(T1,DEPBB) ;
       T3 = 1.0 + T2 ;
       T4 = `Fn_Pow(T3,1.0 / DEPBB) ;

       Mu_bac = Muun / T4 ;
       Ids_bac = weff_nf * ( - Qn_bac) * Mu_bac * Edri ;

     //-----------------------------------------------------------*
     //* Ids: channel current.
     //*-----------------//

       betaWL = weff_nf * beta_inv / Lch ;
       Ids0 = betaWL * Idd * Mu + Ids_res + Ids_bac ;


     // Vdseff //
       Vds = Vdsorg;

       //-----------------------------------------------------------*
       //* Adding parasitic components to the channel current.
       //*-----------------//
       if( PTL != 0 )begin
         T1 =  0.5 * ( Vds - Pds ) ;
         `Fn_SymAdd( T6 , T1 , 0.01 , T2 )
         T1     = 1.1 - ( phi_s0_DEP + T6 );
         `Fn_SZ( T2 , T1 , 0.05 , T0 )
         T2 = T2 + `Small ;
         T0 = beta * ptl0 ;
         T3 = Cox * T0 ;
         T0 = pow( T2 , PTP ) ;
         T9     = T3 * T0 ;
         T4 = 1.0 + Vdsz * PT2 ;
         T0 = pt40 ;
         T5 = phi_s0_DEP + T6 - Vbsz ;
         T4 = T4 + Vdsz * T0 * T5 ;
         T6     = T9 * T4 ;
         T9     = T6 ;
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

       // (note: rpock procedure was removed. (2006.04.20) //
       if( flg_rsrd == 2 ) begin
         Rdd  = Rde ;
         T0 = Rdd * Ids0 ;
         T1 = Vds + `Small ;
         T2 = 1.0 / T1 ;
         T3 = 1.0 + T0 * T2 ;
         T4 = 1.0 / T3 ;
         Ids = Ids0 * T4 ;
       end else begin
         Ids = Ids0 ;
       end

       /* charge calculation */

      //---------------------------------------------------*
      //* Qbu : -Qb in unit area.
      //*-----------------//
       Qbu = - 0.5 * (Q_sub0 + Q_subL + Q_sub0_dep + Q_subL_dep ) ;
       Qiu = - 0.5 * (Q_n0 + Q_nL + Q_s0_dep + Q_sL_dep + Q_b0_dep + Q_bL_dep) ;
       Qdrat = 0.5;

      //* Qiu for Noise calc.
       Qiu_noi = - 0.5 * (Q_n0 + Q_nL) ;

      //-------------------------------------------------*
      //* set flg_noqi , Qn0 , Ey for noise calc.
      //*-----------------//
       Qn0 = - Q_n0 ;
       if( Qn0 < `Small || Qiu < `Small ) begin 
         Qn0 = `Small ;
         flg_noqi = 1; 
       end

       Ey = Ey_suf ;

 end // HSM2_depmos
