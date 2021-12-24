/***********************************************************************

 HiSIM (Hiroshima University STARC IGFET Model)
 Copyright (C) 2000-2016 Hiroshima University and STARC
 Copyright (C) 2016-2019 Hiroshima University

 MODEL NAME : HiSIM2
 ( VERSION : 3  SUBVERSION : 1  REVISION : 1 )
 Model Parameter 'VERSION' : 3.11
 FILE : HSM2_eval_newPT.inc

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

 begin : HSM2_eval_newPT
// Use this module when COPT > 0 and MUPT > 0.0.
`define C_PHI__MINIMUM (1e-7) 

      real Ec , T1w , wk ;
      real Ps0 , t_SUB , Vgp , wk_Vth ;       // protect external Ps0 and beta
      real conpt00 , conpt0 ;
      real sq1npt  , Qn0npt ;

      real wfactor; // accounts for diminished influence exerted by gate; in aid of depletion width
      real wQbu, Wdep, delta;    // depletion width in association with Ps0//   

      t_SUB = 10e-6 ;
      Vgp   = Vgs - Vfb + dVth - dPpg - VFBPT ; 
      wk_Vth = Vth + VFBPT ;              

        Ec = sqrt( 2 * `C_QE * (Vbipn - Vbs)/`C_ESI* Nsub * UC_NJUNC/(Nsub+UC_NJUNC)); //  
        // Potential build-up (lowering potential barrier seen from source) due to Vds.        
        wk = Ec * Leff;                        // [V] positive
        dphi_vds = -0.25 * wk*wk / (Vds + wk); // [V] negative

`include "HSM2_analyticalPs0.inc"   // analyticalPs0   
        if(COPSPT==1) begin
           Ps0_ini = Ps0 ;
`include "HSM2_iterativePs0.inc"    // iterativePs0   
        end

        // From Ps0 here, depletion width will be estimated.
        T5 = -beta *(Ps0 - dphi_vds);
        T6 = exp(T5);
        T7= T6 - 1.0 - T5;
        if (T5 >= `C_PHI__MINIMUM) begin 
           Qbu = -cnst0*sqrt(T7); // sign inverted; accumulation side
        end else begin
           Qbu =  cnst0*sqrt(T7); // sign inverted; depletion, inversion side
        end
        `Fn_SZtemp(wQbu, Qbu, 1e-6) 
        Wdep = wQbu / (`C_QE * Nsub) ;
        T1 = Wdep - XJPT ;
        delta = Wdep*0.01;
        `Fn_SZtemp(T2, T1, delta)  // protection against negative T1 ( i.e. Wdep < XJPT)
        wfactor = T2/Wdep*T2/Wdep; // area ratio between small and large triangles

        phim = (Ps0 - dphi_vds) * wfactor + dphi_vds; // measured from source
        TY = exp(beta*phim) - exp(beta*(phim-Vds));   

          // calculate NPTA based on STI model
          conpt00 = sqrt (2.0 * `C_QE * UC_NJUNC * `C_ESI );
          conpt0 = conpt00 * sqrt(beta_inv) ;
          T1w = beta * (phim - dphi_vds) ;
          `Fn_SL_CP( T1 , T1w , 0 , 0.2*beta , 1 , T0 )
          sq1npt = sqrt (T1 + `epsm10);
          Qn0npt = conpt0 * sq1npt ;
          WK_JNPT_A = 2.0 * beta_inv / Leff * Qn0npt * MUPT ;

        IdsPT1 = WK_JNPT_A * weff_nf * TY ;    // thermoionic emission-like
        Ids    = Idsorg + IdsPT1 ;


`ifdef dbgpt
   Vbspt = Vbs ;
   Ps0pt = Ps0 ;
   Ecpt  = Ecpt ;
   Vgppt = Vgp ;
   TYpt  = TY ;
   wkpt  = wk ;
   Wdeppt = Wdep ;
`endif

 end // HSM2_eval_newPT
