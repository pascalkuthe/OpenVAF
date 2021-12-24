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
 FILE : HSMHV_macrosAndDefs.inc 

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


    `define ALIAS(alias,paramName) aliasparam alias = paramName;
    `define OPP(nam,uni,des)               (*units=uni,                             desc=des*)           real    nam;
    `define MPRnb(nam,def,uni,        des) (*units=uni,                  desc=des*) parameter real    nam=def;
    `define MPRcc(nam,def,uni,lwr,upr,des) (*units=uni,                  desc=des*) parameter real    nam=def from[lwr:upr];
    `define MPRoo(nam,def,uni,lwr,upr,des) (*units=uni,                  desc=des*) parameter real    nam=def from(lwr:upr);
    `define MPRco(nam,def,uni,lwr,upr,des) (*units=uni,                  desc=des*) parameter real    nam=def from[lwr:upr);
    `define MPRoc(nam,def,uni,lwr,upr,des) (*units=uni,                  desc=des*) parameter real    nam=def from(lwr:upr];
    `define MPIcc(nam,def,uni,lwr,upr,des) (*units=uni,                  desc=des*) parameter integer nam=def from[lwr:upr];
    `define MPInb(nam,def,uni,        des) (*units=uni,                  desc=des*) parameter integer nam=def;
    `define MPIsw(nam,def,uni,        des) (*units=uni,                  desc=des*) parameter integer nam=def from[  0:  1];
    `define MPIty(nam,def,uni,        des) (*units=uni,                  desc=des*) parameter integer nam=def from[ -1:  1] exclude 0;
    `define IPRnb(nam,def,uni,        des) (*units=uni, type="instance", desc=des*) parameter real    nam=def;
    `define IPRcc(nam,def,uni,lwr,upr,des) (*units=uni, type="instance", desc=des*) parameter real    nam=def from[lwr:upr];
    `define IPRoo(nam,def,uni,lwr,upr,des) (*units=uni, type="instance", desc=des*) parameter real    nam=def from(lwr:upr);
    `define IPRco(nam,def,uni,lwr,upr,des) (*units=uni, type="instance", desc=des*) parameter real    nam=def from[lwr:upr);
    `define IPRoc(nam,def,uni,lwr,upr,des) (*units=uni, type="instance", desc=des*) parameter real    nam=def from(lwr:upr];
    `define IPInb(nam,def,uni,        des) (*units=uni, type="instance", desc=des*) parameter integer nam=def;
    `define IPIsw(nam,def,uni,        des) (*units=uni, type="instance", desc=des*) parameter integer nam=def from[  0:  1];
    `define BPRnb(nam,def,uni,        des) (*units=uni, type="instance", desc=des*) parameter real    nam=def;
    `define BPIcc(nam,def,uni,lwr,upr,des) (*units=uni, type="instance", desc=des*) parameter integer nam=def from[lwr:upr];
    `define BPRcc(nam,def,uni,lwr,upr,des) (*units=uni, type="instance", desc=des*) parameter real    nam=def from[lwr:upr];
    `define BPRoo(nam,def,uni,lwr,upr,des) (*units=uni, type="instance", desc=des*) parameter real    nam=def from(lwr:upr);
    `define BPRco(nam,def,uni,lwr,upr,des) (*units=uni, type="instance", desc=des*) parameter real    nam=def from[lwr:upr);
    `define BPRoc(nam,def,uni,lwr,upr,des) (*units=uni, type="instance", desc=des*) parameter real    nam=def from(lwr:upr];
    `define BPInb(nam,def,uni,        des) (*units=uni, type="instance", desc=des*) parameter integer nam=def;
    `define BPIsw(nam,def,uni,        des) (*units=uni, type="instance", desc=des*) parameter integer nam=def from[  0:  1];
    `define BPIty(nam,def,uni,        des) (*units=uni, type="instance", desc=des*) parameter integer nam=def from[ -1:  1] exclude 0;

    `define IPM
    `define TESTGIVEN(parameter) $param_given(parameter)
    `define GMIN \
        begin \
            if($param_given(GMIN)) \
                gminMod  = GMIN; \
            else \
                gminMod  = $simparam("gmin",1.0e-12); \
        end
    `define MFACTOR_DEC mMod = $mfactor;
    `define MFACTOR_USE 1.0
//

`define C_RTH_MIN     0.0001
`define Kflic         1
`define Kwhite        1

// For Debugging print
//`define DEBUG 1
`ifdef DEBUG
`define DEBUG_PRINT(xName, x) $strobe("%s = %g", xName, 1.0*x)
`else
`define DEBUG_PRINT(xName, x)
`endif

`define NMOS         1                          // MOS type 
`define PMOS         -1                         // 
`define HiSIM_NORMAL_MODE 1                     // device working mode 
`define HiSIM_REVERSE_MODE -1                   // 

`define SUBVERSION ( VERSION*10 % 10 )

//---------------------------------------------------* 
//* Numerical constants. (macro) 
//*-----------------*/
`ifdef  _FLOAT_H
`define C_EPS_M      (DBL_EPSILON)              // machine epsilon 
`else  
`define C_EPS_M      (2.2204460492503131e-16)   // 
`endif 
`define MAX_EXP      5.834617425e14             // 
`define MIN_EXP      1.713908431e-15            // 
`define EXP_THR      34.0                       // 

`define C_SQRT_2     (1.414213562373095e+00)    // sqrt(2) 
`define C_1oSQRT2    (0.7071067811865475)       // 1/sqrt(2)
`define C_1o3        (3.333333333333333e-01)    // 1/3 
`define C_2o3        (6.666666666666667e-01)    // 2/3
`define C_1o6        (1.666666666666667e-01)    // 1/6
`define C_2p_1o3     (1.259921049894873e+00)   // 2^(1/3)
`define C_Pi         (3.141592653589793)       // Pi
`define C_Pio2       (1.570796326794897)       //
`define C_Pio4       (0.7853981633974483)       // 
`define C_m2cm_p2    (1.0e4)                    // Unit change 
`define C_m2um       (1.0e6)                    // 
//---------------------------------------------------* 
//* Physical constants/properties. (macro) 
//*-----------------*/
`define C_QE         (1.6021918e-19)            // Elemental charge 
`define C_KB         (1.3806226e-23)            // Boltzmann constant 
`define C_ESI        (1.034943e-10)             // Permittivity of Si, SiO2 and vacuum 
`define C_EOX        (3.453133e-11)             // 
`define C_VAC        (8.8541878e-12)            // 
`define C_T300       (300e+00)                  // Room temperature constants 
`define C_b300       (3.868283e+01)             // 
`define C_Nin0       (1.04e+16)                 // Intrinsic carrier density at 300K 
//---------------------------------------------------* 
//* Functions. (macro)  Take care of the arguments.
//*-----------------*/
`define Fn_Sqr(x)   ( (x)*(x) ) // x^2 */
`define Fn_Max(x,y) ( (x) >= (y) ? (x) : (y) ) // max[x,y] */
`define Fn_Min(x,y) ( (x) <= (y) ? (x) : (y) ) // min[x,y] */
`define Fn_Sgn(x)   ( (x) >= 0  ?  (1) : (-1) )    // sign[x] */
//===========================================================* 
//* pow
//*=================*/
`ifdef  POW_TO_EXP_AND_LOG
`define Fn_Pow( x , y ) ((x==0.0) ? 0.0 : exp((y) * ln( x ) ) )
`else
`define Fn_Pow( x , y ) ((x==0.0) ? 0.0 : pow( x , y ))
`endif 
`define C_m2cm_p3    (1.0e6)                    // 
`define C_m2cm       (1.0e2)                    // 
`define C_cm2m_p3    (1.0e-6)                   // 
`define C_m2cm_p1o2  (1.0e1)                    // 
//***********************************************************************/
`define C_rdtemp_min 5.0e-3                     // 
`define C_rdtemp_dlt 1.0e-2                     // 
`define vth_dlt      1.0e-3                 
`define cclmmdf      1.0e-1                 
`define C_cm2m       1.0e-2                 
`define qme_dlt      1.0e-9 * `C_cm2m       
`define rdsl2_dlt    10.0e-3 * `C_cm2m      
`define rdsu2_dlt    50.0e-6 * `C_cm2m      
`define rdsz_dlt     0.1e-3 * `C_cm2m       
`define qme2_dlt     5.0e-2                 
`define eef_dlt      1.0e-2 / `C_cm2m       
`define sti2_dlt     2.0e-3                 
`define pol_dlt      5.0e-2                 
`define psia_dlt     1.0e-3                 
`define psia2_dlt    5.0e-3                 
`define psisti_dlt   5.0e-3                 
`define lp_s0_max    40                     
`define lp_sl_max    40                     
`define lp_se_max    150
`define dP_max       0.1e0                  
`define ps_conv      1.0e-12                
`define ps_conv2     1.0e-10
`define gs_conv      1.0e-8                 
`define znbd3        3.0e0                  
`define znbd5        5.0e0                  
`define cn_nc3       `C_SQRT_2 / 108e0      
`define cn_nc51      0.707106781186548      // sqrt(2)/2 */
`define cn_nc52      -0.117851130197758     // -sqrt(2)/12 */
`define cn_nc53      0.0178800506338833     // (187 - 112*sqrt(2))/1600 */
`define cn_nc54      -0.00163730162779191   // (-131 + 88*sqrt(2))/4000 */
`define cn_nc55      6.36964918866352e-5    // (1509-1040*sqrt(2))/600000 */
`define cn_im53      2.9693154855770998e-1   
`define cn_im54      -7.0536542840097616e-2   
`define cn_im55      6.1152888951331797e-3   
`define c_ps0ini_2   8.0e-4                 
`define c_pslini_1   0.3e0                  
`define c_pslini_2   3.0e-2                 
`define VgVt_Small   1.0e-12                
`define epsm10       10.0e0 * `C_EPS_M      
`define epsm100      500.0e0 * `C_EPS_M
`define Small        1.0e-25                
`define Small2       1e-12                  // for Ra(Vdse) dependence */
`define c_exp_2      7.38905609893065       
`define large_arg    60                     
`define pol_b        1.0                    
`define igate_dlt    1.0e-2 / `C_cm2m       
`define gidlvds_dlt  1.0e-5                 
`define gidla        100.0                  
`define c_sqrt_15    3.872983346207417e0    // sqrt(15) */
`define Cox_Small    1.0e-6                 
`define c_16o135     1.185185185185185e-1   // 16/135 */
`define delta_rd     10e-3 * `C_cm2m        
`define Ra_N         20.0                   // smoothing parameter for Ra */
`define Res_min      RMIN                 
`define RDVSUB       RDVSUB                 
`define RDVDSUB      RDVDSUB                
`define DDRIFT       DDRIFT                 
`define VBISUB       VBISUB                 
`define NSUBSUB      MKS_NSUBSUB            
`define dlt_rd23     1.0e-6 / `C_m2cm       
`define tau_min      1.0e-15       
`define dQdrat        ( 1.0e-8 )
`define inf          1e+300
`define VXBGMTDLT    ( 0.5     )


//*-----------------*/
`define C_IDD_MIN    1.0e-15                    // 
`define C_VDS_MIN    1.0E-6
`define C_sub_delta  0.1 
`define C_sub_delta2 1.0e-9 
`define C_gidl_delta 0.5                        // 
//===========================================================* 
//* Exp() for PGD.
//* - ExpLim(-3)=0
//*=================*/
`define Fn_ExpLim( y , x , dx ) begin \
    if((x) < -3.0 ) begin \
      dx = 0.0 ; \
      y = 0.0 ; \
    end else if((x) < 0.0 ) begin \
      dx =  1.0 + (x) * ( 2 * (1.0/3.0) + (x) * 3 * (1.0/27.0) )  ; \
      y = 1.0 + (x) * ( 1.0 + (x) * ( (1.0/3.0) + (x) * (1.0/27.0) ) ) ; \
    end else begin \
      dx =  1.0 + (x) * ( 2 * (1.0/3.0) + (x) * ( 3 * 0.0402052934513951 \
                + (x) * 4 * 0.148148111111111 ) ) ; \
      y = 1.0 + (x) * ( 1.0 + (x) * ( (1.0/3.0) + (x) * ( 0.0402052934513951 \
              + (x) * 0.148148111111111 ) ) ) ; \
    end \
end
//===========================================================* 
//* Macro Functions for ceiling/flooring/symmetrization.
//*=================*/
//---------------------------------------------------* 
//* smoothUpper: ceiling.
//*      y = xmax - 0.5 ( arg + sqrt( arg^2 + 4 xmax delta ) )
//*    arg = xmax - x - delta
//*-----------------*/
`define Fn_SU( y , x , xmax , delta , dx ) begin \
    TMF1 = ( xmax ) - ( x ) - ( delta ) ; \
    TMF2 = 4.0 * ( xmax ) * ( delta) ; \
    TMF2 = TMF2 > 0.0 ?  TMF2 : - ( TMF2 ) ; \
    TMF2 = sqrt ( TMF1 * TMF1 + TMF2 ) ; \
    dx = 0.5 * ( 1.0 + TMF1 / TMF2 ) ; \
    y = ( xmax ) - 0.5 * ( TMF1 + TMF2 ) ; \
  end
`define Fn_SU2( y , x , xmax , delta , dy_dx , dy_dxmax ) begin \
    TMF1 = ( xmax ) - ( x ) - ( delta ) ; \
    TMF2 = 4.0 * ( xmax ) * ( delta) ; \
    TMF2 = TMF2 > 0.0 ?  TMF2 : - ( TMF2 ) ; \
    TMF2 = sqrt ( TMF1 * TMF1 + TMF2 ) ; \
    dy_dx = 0.5 * ( 1.0 + TMF1 / TMF2 ) ; \
    dy_dxmax = 0.5 * ( 1.0 - ( TMF1 + 2.0 * delta ) / TMF2 ) ; \
    y = ( xmax ) - 0.5 * ( TMF1 + TMF2 ) ; \
  end
`define Fn_SUtemp( y , x , xmax , delta ) begin \
    TMF1 = ( xmax ) - ( x ) - ( delta ) ; \
    TMF2 = 4.0 * ( xmax ) * ( delta) ; \
    TMF2 = TMF2 > 0.0 ?  TMF2 : - ( TMF2 ) ; \
    TMF2 = sqrt ( TMF1 * TMF1 + TMF2 ) ; \
    y = ( xmax ) - 0.5 * ( TMF1 + TMF2 ) ; \
  end
//---------------------------------------------------* 
//* smoothLower: flooring.
//*      y = xmin + 0.5 ( arg + sqrt( arg^2 + 4 xmin delta ) )
//*    arg = x - xmin - delta
//*-----------------*/
`define Fn_SL( y , x , xmin , delta , dx ) begin \
    TMF1 = ( x ) - ( xmin ) - ( delta ) ; \
    TMF2 = 4.0 * ( xmin ) * ( delta ) ; \
    TMF2 = TMF2 > 0.0 ?  TMF2 : - ( TMF2 ) ; \
    TMF2 = sqrt ( TMF1 * TMF1 + TMF2 ) ; \
    dx = 0.5 * ( 1.0 + TMF1 / TMF2 ) ; \
    y = ( xmin ) + 0.5 * ( TMF1 + TMF2 ) ; \
  end
`define Fn_SL2( y , x , xmin , delta , dy_dx, dy_dxmin ) begin \
    TMF1 = ( x ) - ( xmin ) - ( delta ) ; \
    TMF2 = 4.0 * ( xmin ) * ( delta ) ; \
    TMF2 = TMF2 > 0.0 ?  TMF2 : - ( TMF2 ) ; \
    TMF2 = sqrt ( TMF1 * TMF1 + TMF2 ) ; \
    dy_dx = 0.5 * ( 1.0 + TMF1 / TMF2 ) ; \
    dy_dxmin = 0.5 * ( 1.0 - ( TMF1 - 2.0 * delta ) / TMF2 ) ; \
    y = ( xmin ) + 0.5 * ( TMF1 + TMF2 ) ; \
  end
`define Fn_SLtemp( y , x , xmin , delta ) begin \
    TMF1 = ( x ) - ( xmin ) - ( delta ) ; \
    TMF2 = 4.0 * ( xmin ) * ( delta ) ; \
    TMF2 = TMF2 > 0.0 ?  TMF2 : - ( TMF2 ) ; \
    TMF2 = sqrt ( TMF1 * TMF1 + TMF2 ) ; \
    y = ( xmin ) + 0.5 * ( TMF1 + TMF2 ) ; \
  end
//---------------------------------------------------* 
//* smoothZero: flooring to zero.
//*      y = 0.5 ( x + sqrt( x^2 + 4 delta^2 ) )
//*-----------------*/
`define Fn_SZ( y , x , delta , dx ) begin \
    TMF2 = sqrt ( ( x ) *  ( x ) + 4.0 * ( delta ) * ( delta) ) ; \
    dx = 0.5 * ( 1.0 + ( x ) / TMF2 ) ; \
    y = 0.5 * ( ( x ) + TMF2 ) ; \
    if( y < 0.0 ) begin y=0.0; dx=0.0; end \
  end
`define Fn_SZtemp( y , x , delta ) begin \
    TMF1 = sqrt ( ( x ) *  ( x ) + 4.0 * ( delta ) * ( delta) ) ; \
    y = 0.5 * ( ( x ) + TMF1 ) ; \
  end
//---------------------------------------------------* 
//* CeilingPow: ceiling for positive x, flooring for negative x.
//*      y = x * xmax / ( x^{2m} + xmax^{2m} )^{1/(2m)}
//* note: 
//*   - xmax has to be positive.
//*   - -xmax < y < xmax.
//*   - dy/dx|_{x=0} = 1.
//*-----------------*/
`define Fn_CP( y , x , xmax , pw , dx ) begin \
  x2 = (x) * (x) ; \
  xmax2 = (xmax) * (xmax) ; \
  xp = 1.0  ;\
  xmp = 1.0 ; \
  m0 = 0 ;\
  mm =0; \
  arg = 0.0 ;\
  dnm =0.0; \
  for ( m0 = 0 ; m0 < pw ; m0 = m0 + 1 ) begin xp = xp * x2 ; xmp = xmp * xmax2 ; end \
  arg = xp + xmp ; \
  dnm = arg ; \
  if( pw == 1 || pw == 2 || pw == 4 || pw == 8 ) begin \
    if ( pw == 1 ) begin mm = 1 ; \
    end else if ( pw == 2 ) begin mm = 2 ; \
    end else if ( pw == 4 ) begin mm = 3 ; \
    end else if ( pw == 8 ) begin mm = 4 ; end \
    for ( m0 = 0 ; m0 < mm ; m0 = m0 + 1 ) begin dnm = sqrt( dnm ) ; end \
  end else begin dnm = `Fn_Pow( dnm , 1.0 / ( 2.0 * (pw) ) ) ; end \
  dnm = 1.0 / dnm ; \
  y = (x) * (xmax) * dnm ; \
  dx = (xmax) * xmp * dnm / arg ; \
end

`define Fn_SU_CP( y , x , xmax , delta , pw , dx ) begin \
 if(x > xmax - (delta) && delta >= 0.0) begin \
   TMF1 = x - (xmax) + delta ; \
   `Fn_CP( TMF0 , TMF1 , delta , pw , dx )  \
   y = xmax - (delta) + TMF0 ; \
   dx = dx ; \
 end else begin \
   y = x ; \
   dx = 1.0 ; \
 end \
end

`define Fn_SU_CP2( y , x , xmax , delta , pw , dx , dxmax ) begin \
 if(x > xmax - (delta) && delta >= 0.0) { \
   TMF1 = x - (xmax) + delta ; \
   Fn_CP( TMF0 , TMF1 , delta , pw , dx )  \
   y = xmax - (delta) + TMF0 ; \
   dx = dx ; \
   dxmax = 1.0-dx ; \
 end else begin \
   y = x ; \
   dx = 1.0 ; \
   dxmax = 0.0 ; \
 end \
end

`define Fn_SL_CP( y , x , xmin , delta , pw , dx ) begin \
 if(x < xmin + delta && delta >= 0.0) begin \
   TMF1 = xmin + delta - x ; \
   `Fn_CP( TMF0 , TMF1 , delta , pw , dx )  \
   y = xmin + delta - TMF0 ; \
   dx = dx ; \
 end else begin \
   y = x ; \
   dx = 1.0 ; \
 end \
end


//---------------------------------------------------* 
//* Declining function using a polynomial.
//*-----------------*/
`define Fn_DclPoly4( y , x , dx ) begin \
  TMF2 = (x) * (x) ; \
  TMF3 = TMF2 * (x) ; \
  TMF4 = TMF2 * TMF2 ; \
  y = 1.0 / ( 1.0 + (x) + TMF2 + TMF3 + TMF4 ) ; \
  dx = - ( 1.0 + 2.0 * (x) + 3.0 * TMF2 + 4.0 * TMF3 )  * y * y  ; \
end 
`define Fn_DclPoly6( y , x , dx ) begin \
  TMF2 = (x) * (x) ; \
  TMF3 = TMF2 * (x) ; \
  TMF4 = TMF2 * TMF2 ; \
  TMF5 = TMF2 * TMF3 ; \
  TMF6 = TMF3 * TMF3 ; \
  y = 1.0 / ( 1.0 + (x) + TMF2 + TMF3 + TMF4 + TMF5 + TMF6 ) ; \
  dx = - ( 1.0 + 2.0 * (x) + 3.0 * TMF2 + 4.0 * TMF3 \
         + 5.0 * TMF4 + 6.0 * TMF5 )  * y * y  ; \
end
//---------------------------------------------------* 
//* "smoothUpper" using a polynomial
//*-----------------*/
`define Fn_SUPoly4( y , x , xmax , dx ) begin \
 TMF1 = (x) / (xmax) ; \
 `Fn_DclPoly4( y , TMF1 , dx )  \
 y = (xmax) * ( 1.0 - y ) ; \
 dx = - dx ; \
end
`define Fn_SUPoly4m( y , x , xmax , dx , dxmax ) begin \
 TMF1 = (x) / (xmax) ; \
 `Fn_DclPoly4( TMF0 , TMF1 , dx )  \
 y = (xmax) * ( 1.0 - TMF0 ) ; \
 dxmax = 1.0 - TMF0 + TMF1 * dx ; \
 dx = - dx ; \
end
`define Fn_SUPoly6m( y , x , xmax , dx , dxmax ) begin \
 TMF1 = (x) / (xmax) ; \
 `Fn_DclPoly6( TMF0 , TMF1 , dx )  \
 y = (xmax) * ( 1.0 - TMF0 ) ; \
 dxmax = 1.0 - TMF0 + TMF1 * dx ; \
 dx = - dx ; \
end
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
//* SymAdd: evaluate additional term for symmetry.
//*-----------------*/
`define Fn_SymAdd( y , x , add0 , dx ) \
begin \
    TMF1 = 2.0 * ( x ) / ( add0 ) ; \
    TMF2 = 1.0 + TMF1 * ( (1.0/2) + TMF1 * ( (1.0/6) \
               + TMF1 * ( (1.0/24) + TMF1 * ( (1.0/120) \
               + TMF1 * ( (1.0/720) + TMF1 * (1.0/5040) ) ) ) ) ) ; \
    TMF3 = (1.0/2) + TMF1 * ( (1.0/3) \
               + TMF1 * ( (1.0/8) + TMF1 * ( (1.0/30) \
               + TMF1 * ( (1.0/144) + TMF1 * (1.0/840) ) ) ) ) ; \
    y = (add0) / TMF2 ; \
    dx = - 2.0 * TMF3 / ( TMF2 * TMF2 ) ; \
end
//---------------------------------------------------*
//* Sym4: 4th order additional term for symmetry.
//*-----------------*/
`define nSym4 2
`define Fn_Sym4( y , x , add0 ) \
 begin \
     TMF1 = pow( (x) , `nSym4 ) ; \
     TMF2 = pow( (add0) , `nSym4 ) ; \
     y = pow( (TMF1+TMF2) , 1.0/`nSym4 ) - pow(TMF2,1.0/`nSym4) ; \
 end

//===========================================================*
//* Sigmoid function
//*=================*/
`define Fn_Sigmoid( y , x ) y = 1.0 / ( 1.0 + exp(- (x)) );


`define BINNING(UC_PARAM, PARAM, LPARAM, WPARAM, PPARAM) UC_PARAM = PARAM \
  + LPARAM / Lbin + WPARAM / Wbin \
  + PPARAM / LWbin ;

`define RANGECHECK(param, min, max, pname)          \
  if( COERRREP && ((param) <(min) ||(param) >(max)) ) begin \
    $write("warning(HiSIM_HV(%m)): (%s = %g) range [%g , %g].\n", \
           pname, (param), (min*1.0), (max*1.0) );            \
  end
`define RANGERESET(param, min, max, pname)              \
  if( COERRREP && ((param) >(max)) ) begin   \
    $write("reset(HiSIM_HV(%m)): (%s = %g to %g) range [%g , %g].\n", \
           pname, (param), (max*1.0), (min*1.0), (max*1.0) );     \
  end \
  if( COERRREP && ((param) <(min)) ) begin   \
    $write("reset(HiSIM_HV(%m)): (%s = %g to %g) range [%g , %g].\n", \
           pname, (param), (min*1.0), (min*1.0), (max*1.0) );     \
  end \
  if((param) <(min) ) begin  param  =(min); end    \
  if((param) >(max) ) begin  param  =(max); end            
`define MINCHECK(param, min, pname)                     \
  if( COERRREP && ((param) <(min)) ) begin   \
    $write("warning(HiSIM_HV(%m)): (%s = %g) range [%g , %g].\n", \
           pname, (param), (min*1.0), (min*1.0) );                   \
  end
`define MINRESET(param, min, pname)                     \
  if( COERRREP && ((param) <(min)) ) begin   \
    $write("reset(HiSIM_HV(%m)): (%s = %g to %g) range [%g , %g].\n",    \
           pname, (param), (min*1.0), (min*1.0), (min*1.0) );            \
  end \
  if((param) <(min) ) begin  param  =(min); end 

`define RANGECHECKFoo(param, min, max, pname)          \
  if( ((param) <= (min) || (param) >= (max)) ) begin \
    $write("fatal(HiSIM_HV(%m)): (%s = %g) range (%g , %g).\n", \
           pname, (param), (min*1.0), (max*1.0) );            \
    FATAL_flag = 1; \
  end
`define RANGECHECKFco(param, min, max, pname)          \
  if( ((param) <  (min) || (param) >= (max)) ) begin \
    $write("fatal(HiSIM_HV(%m)): (%s = %g) range [%g , %g).\n", \
           pname, (param), (min*1.0), (max*1.0) );            \
    FATAL_flag = 1; \
  end
`define RANGECHECKFoc(param, min, max, pname)          \
  if( ((param) <= (min) || (param) >  (max)) ) begin \
    $write("fatal(HiSIM_HV(%m)): (%s = %g) range (%g , %g].\n", \
           pname, (param), (min*1.0), (max*1.0) );            \
    FATAL_flag = 1; \
  end
`define RANGECHECKFcc(param, min, max, pname)          \
  if( ((param) <  (min) || (param) >  (max)) ) begin \
    $write("fatal(HiSIM_HV(%m)): (%s = %g) range [%g , %g].\n", \
           pname, (param), (min*1.0), (max*1.0) );            \
    FATAL_flag = 1; \
  end

//// limited exp function (lexp)
//`define CEXP_THRES 80.0
//`define CMAX_EXP 5.540622384e+34
//`define CMIN_EXP 1.804851387e-35
//analog function real lexp;
//       input x; 
//       real x;
//       begin
//         if(x > `CEXP_THRES) begin
//                 lexp = `CMAX_EXP * ( 1.0 + (x) -`CEXP_THRES);
//         end else if(x < -`CEXP_THRES) begin
//                 lexp = `CMIN_EXP;
//         end else begin
//                 lexp = exp(x);
//         end
//       end
//endfunction

//===========================================================*
//* Exp() for large input value.
//*=================*/
`define D_EXP_THRESHOLD (500.0)                //exp(710)=inf
`define D_MAX_EXP       (1.403592217853e+217)  //exp(500.0)
`define C_UNIT_EXP_ARG  (60.0)                 //exp(60.0) may touch the Verilog-A limit
`define C_UNIT_EXP_RES  (1.14200738981568e+26) //exp(60.0)
`define Fn_DExp( y , x , dx) begin \
  if ((x) >= `D_EXP_THRESHOLD) begin \
    y  = `D_MAX_EXP * (1 + (x) - `D_EXP_THRESHOLD); \
    dx = `D_MAX_EXP ; \
  end else begin \
    TMF1 = (x); \
    y = 1.0; \
    while (TMF1 >= `C_UNIT_EXP_ARG ) begin \
      y = y * `C_UNIT_EXP_RES; \
      TMF1 = TMF1 - `C_UNIT_EXP_ARG; \
    end \
    y = y * exp(TMF1); dx = y; \
  end \
end

//---------------------------------------------------* 
//* Macros for aging model.
//*-----------------*/
`define SU_TRAP_delta 1e-3
`define Fn_calc_TRAP( N00 , N01 , N02, N03, N04, ps , pds ) begin \
    efn_ec = ps - ( - keT *ln(2.5e25/Nsub)+pds) - EG0 ; \
    TMF4 = lexp(efn_ec / MKS_TRAPES1 ) ; \
    y1 = N01 * TMF4 ; \
    `Fn_SLtemp( y1 , y1 , 1 , 1 ) \
    TMF4 = lexp(efn_ec / TRAPES2 ) ; \
    y2 = N02 * TMF4 ; \
    `Fn_SLtemp( y2 , y2 , 1 , 1 ) \
    TMF5 = log(y1) ;  \
    TMF4 = lexp(efn_ec / TRAPESLIM ) ; \
    y6 = N04 * TMF4 ;  TMF8 = log(y6) ; \
    if( CODEGES0 ) begin \
      TMF4 = lexp(efn_ec / TRAPES0 ) ; \
      y3 = N03 * TMF4 ; \
      `Fn_SLtemp( y3 , y3 , 1 , 1 ) \
      TMF8 = log(y3) ; \
      `Fn_SUtemp( TMF6 , TMF8 , TMF5 , `SU_TRAP_delta )  \
    end else begin \
      `Fn_SUtemp( TMF6 , TMF5 , TMF8 , `SU_TRAP_delta*0.1 )  \
    end \
    y4 = pow(10,TMF6) ; \
    y5 = y4 * lexp( - Vtraplx_prv / TRAPLX ) ; \
    TMF5 = log(y5) ; TMF6 = log(y2) ; \
    `Fn_SLtemp( TMF7 , TMF6 , TMF5 , `SU_TRAP_delta*0.1 )  \
    N00 = pow(10,TMF7) ; \
 end

`define Fn_calcNKT(NtK,gc,es) begin \
    delta = ( `C_Pi * keT ) / es ; \
    NtK   = abs( gc * es * ( delta / sin( delta ))) ; \
 end

//---------------------------------------------------* 
//* Macros for body diode model.
//*-----------------*/
`define HSMHV_Idio_args1(js, JS0, XTI, NJ) begin \
  js = JS0 * exp((egtnom * betatnom - Eg * beta + XTI * log_Tratio) / NJ) ; \
end

`define HSMHV_Idio_args2(is_btm, is2_btm, is_sws, is2_sws, is_swg, is2_swg, PD, AD) begin \
  if( PD > weff_nf ) begin \
     is_btm  = AD * js  ; \
     is2_btm = AD * js2 ; \
     is_sws  = (PD - weff_nf) * jssw  ; \
     is2_sws = (PD - weff_nf) * jssw2 ; \
     is_swg  = weff_nf * jsswg ; \
     is2_swg = weff_nf * jsswg2 ; \
  end else begin \
     is_btm  = AD * js  ; \
     is2_btm = AD * js2 ; \
     is_sws  = 0.0 ; \
     is2_sws = 0.0 ; \
     is_swg  = PD * jsswg ; \
     is2_swg = PD * jsswg2 ; \
  end \
end

`define HSMHV_Idio_args2_compat(is_btm, is2_btm, is_sws, is2_sws, is_swg, is2_swg, PD, AD) begin \
  is_btm  = AD * js  ; \
  is2_btm = AD * js2 ; \
  is_sws  = PD * jssw  ; \
  is2_sws = PD * jssw2 ; \
  is_swg  = 0.0 ; \
  is2_swg = 0.0 ; \
end

`define HSMHV_Qdio_args1(czbd, czbdsw, czbdswg, PD, AD, CJD, TCJBD, CJSWD, TCJBDSW, CJSWGD, TCJBDSWG, name) begin \
  czbd = CJD * AD * ( 1.0 + TCJBD * Tdiff) ; \
  if(PD > weff_nf) begin \
    czbdsw  = CJSWD  * ( PD - weff_nf ) * ( 1.0 + TCJBDSW  * Tdiff) ; \
    czbdswg = CJSWGD *        weff_nf   * ( 1.0 + TCJBDSWG * Tdiff) ; \
  end else begin \
    czbdsw  = 0.0 ; \
    czbdswg = CJSWGD *   PD             * ( 1.0 + TCJBDSWG * Tdiff) ; \
  end \
  if(czbd < 0.0) begin \
    if(COERRREP) begin \
      $write("reset(HiSIM_HV(%m)): The model parameter TCJB%s=%g is too large.  Effective total CJ%s is reset from %g to 0 (F).\n", name, TCJBD, name, czbd); \
    end \
    czbd  = 0.0 ; \
  end \
  if(czbdsw < 0.0) begin \
    if(COERRREP) begin \
      $write("reset(HiSIM_HV(%m)): The model parameter TCJB%sSW=%g is too large.  Effective total CJSW%s is reset from %g to 0 (F).\n", name, TCJBDSW, name, czbdsw); \
    end \
    czbdsw  = 0.0 ; \
  end \
  if(czbdswg < 0.0) begin \
    if(COERRREP) begin \
      $write("reset(HiSIM_HV(%m)): The model parameter TCJB%sSWG=%g is too large.  Effective total CJSWG%s is reset from %g to 0 (F).\n", name, TCJBDSWG, name, czbdswg); \
    end \
    czbdswg  = 0.0 ; \
  end \
end

`define HSMHV_Qdio_args2(pzbd, pzbdsw, pzbdswg, PD, AD, PBD, TPBBD, PBSWD, TPBBDSW, PBSWGD, TPBBDSWG, name) begin \
  pzbd    = PBD    - TPBBD    * Tdiff ; \
  pzbdsw  = PBSWD  - TPBBDSW  * Tdiff ; \
  pzbdswg = PBSWGD - TPBBDSWG * Tdiff ; \
  if(pzbd < 0.01 && AD > 0.0) begin \
    if(COERRREP) begin \
      $write("reset(HiSIM_HV(%m)): The model parameter TPBB%s=%g is too large.  Effective PB%s is reset from %g to 0.01 (V).\n", name, TPBBD, name, pzbd); \
    end \
    pzbd  = 0.01 ; \
  end \
  if(pzbdsw < 0.01 && PD > weff_nf) begin \
    if(COERRREP) begin \
      $write("reset(HiSIM_HV(%m)): The model parameter TPBB%sSW=%g is too large.  Effective PBSW%s is reset from %g to 0.01 (V).\n", name, TPBBDSW, name, pzbdsw); \
    end \
    pzbdsw  = 0.01 ; \
  end \
  if(pzbdswg < 0.01 && PD > 0.0) begin \
    if(COERRREP) begin \
      $write("reset(HiSIM_HV(%m)): The model parameter TPBB%sSWG=%g is too large.  Effective PBSWG%s is reset from %g to 0.01 (V).\n", name, TPBBDSWG, name, pzbdswg); \
    end \
    pzbdswg  = 0.01 ; \
  end \
end

`define HSMHV_Idio(Idio, Vdio, is, is2,  vt_X, jd_expc_X,  T10_X, T9_X, jd_nvtm_inv_X, UC_CISBK_X, DIVX_X) begin \
  if( is > 0 ) begin \
    T0 = is2 * T9_X ; \
    TX = - Vdio * T10_X ; \
    T2 = exp ( TX ); \
    T3 = T2 ; \
    if( Vdio < vt_X ) begin \
      TX = Vdio * jd_nvtm_inv_X ; \
      if( TX < - 3*`EXP_THR ) begin \
        T1 = 0.0 ; \
      end else begin \
        T1 = exp ( TX ) ; \
      end \
      Idio = is * (T1 - 1.0) + T0 * (T2 - 1.0) + UC_CISBK_X * (T3 - 1.0); \
    end else begin \
      T1 = jd_expc_X ; \
      T4 = is * jd_nvtm_inv_X * T1 ; \
      Idio = is * (T1 - 1.0) + T4 * (Vdio - vt_X) + T0 * (T2 - 1.0) + UC_CISBK_X * (T3 - 1.0) ; \
    end \
  end else begin \
    Idio = 0.0 ; \
  end \
  T12 = DIVX_X * is2 ; \
  Idio = Idio + T12 * Vdio ; \
end

`define HSMHV_Qdio(Qdio, Capdio, Vdio, cz, PB, MJ) begin \
  if(cz > 0.0) begin \
    if(Vdio < 0.0) begin \
      arg = 1.0 - Vdio / PB ; \
      if(MJ == 0.5) sarg = 1.0 / sqrt(arg) ; \
      else sarg = `Fn_Pow( arg , -MJ ) ; \
      Qdio = PB * cz * (1.0 - arg * sarg) / (1.0 - MJ) ; \
      Capdio = cz * sarg ; \
    end else begin \
      T1 = cz ; \
      T2 = cz * MJ / PB ; \
      Qdio = Vdio * (T1 + Vdio * 0.5 * T2) ; \
      Capdio = T1 + Vdio * T2 ; \
    end \
  end else begin \
    Qdio = 0.0 ; \
    Capdio = 0.0 ; \
  end \
end


//imported from DIODE_CMC_macrodefs.include:
//diode reverse recovery model
`define C_Nin0_dio   (1.45e+16)                 // Intrinsic carrier density at 300K (m^-3)
`define C_MUEP       (500)                      // Hole mobility (cm^2/V/sec)
`define C_MUEN       (1450)                     // Electron mob. (cm^2/V/sec)
`define C_EXPCEIL    (1e20)                     // safety valve against carrier conc explosion (-)
`define C_PUNCH      (1.0e-7)                   // Delta for punch-through effect of depletion (m)
`define QN_INJ_SCALING   (NQS)                  // pre/post-scaling for node voltages of charge_A and charge_K
`define NQS_CAPACITOR    (NQS)                  // scaling for charge_A and charge_K
`define DEP_SCALING      (DEPNQS)               // pre/post-scaling for node voltage of depl_A
`define DEPNQS_CAPACITOR (DEPNQS)               // scaling for depl_A
`define C_MUETMP     (1.5)                      // temperature dependence of phonon scattering (-)
//End of diode reverse recovery model


//End of File: HSMHV_macrosAndDefs.inc
