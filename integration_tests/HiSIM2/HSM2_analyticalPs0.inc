/***********************************************************************

 HiSIM (Hiroshima University STARC IGFET Model)
 Copyright (C) 2000-2016 Hiroshima University and STARC
 Copyright (C) 2016-2019 Hiroshima University

 MODEL NAME : HiSIM2
 ( VERSION : 3  SUBVERSION : 1  REVISION : 1 )
 Model Parameter 'VERSION' : 3.11
 FILE : HSM2_analyticalPs0.inc

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

 begin : HSM2_analyticalPs0

    real Vbscl , Vth ; // different from outside

         Vbscl = dphi_vds ;   
         Vth   = wk_Vth ;
  
         //-----------------------------------------------------------*
         //* Initial guess for Ps0. (start)
         //*-----------------//
         //---------------------------------------------------*
         //* Ps0_iniA: solution of subthreshold equation assuming zone-D1/D2.
         //*-----------------//
         TX = 1.0e0 + 4.0e0 * ( beta * ( Vgp - Vbscl ) - 1.0e0 ) / ( fac1p2 * beta2 ) ;
         TX = `Fn_Max( TX , `epsm10 ) ;
         Ps0_iniA = Vgp + fac1p2 * beta * 0.5 * ( 1.0e0 - sqrt( TX ) ) ;
                
         // use analytical value in subthreshold region. //
//       if( Vgs <( Vfb + Vth ) * 0.5 ) begin
//           flg_pprv = 0 ;
//       end

//       if( 1 ) begin
                        
         //---------------------------------------------------*
         //* Analytical initial guess.
         //*-----------------//
         //-------------------------------------------*
         //* Common part.
         //*-----------------//
         Chi = beta * ( Ps0_iniA - Vbscl ) ;
         if( Chi < `znbd3 ) begin
           //-----------------------------------*
           //* zone-D1/D2
           //* - Ps0_ini is the analytical solution of Qs=Qb0 with
           //*   Qb0 being approximated to 3-degree polynomial.
           //*-----------------//
           TY = beta * ( Vgp - Vbscl ) ;
           T1 = 1.0e0 / ( `cn_nc3 * beta * fac1 ) ;
           T2 = 81.0 + 3.0 * T1 ;
           T3 = -2916.0 - 81.0 * T1 + 27.0 * T1 * TY ;
           T4 = 1458.0 - 81.0 * ( 54.0 + T1 ) + 27.0 * T1 * TY ;
           T4 = T4 * T4 ;
           T5 = `Fn_Pow( T3 + sqrt( 4 * T2 * T2 * T2 + T4 ) , `C_1o3 ) ;
           TX = 3.0 - ( `C_2p_1o3 * T2 ) / ( 3.0 * T5 )
              + 1 / ( 3.0 * `C_2p_1o3 ) * T5 ;
           Ps0_iniA = TX * beta_inv + Vbscl ;
           Ps0_ini = Ps0_iniA ;
         end else if( Vgs <= Vth ) begin
           //-----------------------------------*
           //* Accumulation & Weak inversion zone.
           //*-----------------//
           Ps0_ini = Ps0_iniA ;
         end else begin
           //-----------------------------------*
           //* Strong inversion zone.
           //* - Ps0_iniB : upper bound.
           //*-----------------//
           T1 = 1.0 / cnst1 / cnstCoxi ;
           T2 = T1 * Vgp * Vgp ;
           T3 = beta + 2.0 / Vgp ;
           Ps0_iniB = ln ( T2 ) / T3 + PSLIMPT ;
           `Fn_SUtemp( Ps0_ini , Ps0_iniA, Ps0_iniB, `c_ps0ini_2)
         end

         TX = Vbscl + `ps_conv / 2 ;
         if ( Ps0_ini < TX ) Ps0_ini = TX ;
          
         //---------------------------------------------------*
         //* Assign initial guess.
         //*-----------------//
         Ps0 = Ps0_ini ;
         
         // Initial guess for Ps0. (end) //

 end // block: HSM2_analyticalPs0
