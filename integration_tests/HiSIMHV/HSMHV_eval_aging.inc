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
 FILE : HSMHV_eval_aging.inc

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


begin : HSMHV_eval_aging

   real gc1deg0 , e1deg0 ;
   real gc_time1 , gc_time2 , gc_time , e_time1 , e_time2 , Isub_fac ;
`ifdef _AGING_DEBUG_
   real NtK1 ;
`endif 

// Macro definition
`define C2p72_GCTIME1     2.72/TRAPGCTIME1
`define C2p72_GCTIME1_dlt 2.72/TRAPGCTIME1*1e-3
`define C2p72_ESTIME1     2.72/TRAPESTIME1
`define C2p72_ESTIME1_dlt 2.72/TRAPESTIME1*1e-3
`define C2p72_DTIME1      2.72/TRAPDTIME1
`define C2p72_DTIME1_dlt  2.72/TRAPDTIME1*1e-3
`define IsubT_min         2.72
`define IsubpW_min        1e-6

`define Fn_calcGC1ES1(gc1deg,e1deg,isub)  begin \
    T2 = isub / Weff ; \
    `Fn_Sigmoid( Isub_fac , (T2-`IsubpW_min)/`IsubpW_min*15 ) \
    `Fn_SLtemp( T2 , T2 , `C2p72_GCTIME1 , `C2p72_GCTIME1_dlt )  \
    T3 = T2  * TRAPGCTIME1 ;  \
    `Fn_SLtemp( T4 , (T2 * DEGTIME) , `IsubT_min , 1e-3 )  \
    `Fn_SLtemp( T5 , (T2 * TRAPGCTIME2) , `IsubT_min , 1e-3 )  \
    gc_time  = ln(T4) ; \
    gc_time1 = ln(T3) ; \
    gc_time2 = ln(T5) ; \
    T8 = (gc_time - gc_time2)/gc_time1 ; \
    T5 = - 0.5 * T8 * T8 ; \
    `Fn_DExp( T6 , T5 , T0 )   \
    gc1deg = gc1deg0 + MKS_GC1MAX / gc_time1 * T6 * Isub_fac ; \
    T2 = isub / Weff ; \
    `Fn_SLtemp( T2 , T2 , `C2p72_ESTIME1 , `C2p72_ESTIME1_dlt )  \
    T3 = T2  * TRAPESTIME1 ;  \
    `Fn_SLtemp( T5 , (T2 * TRAPESTIME2) , `IsubT_min , 1e-3 )  \
    e_time1 = ln(T3) ; \
    e_time2 = ln(T5) ; \
    T8 = (gc_time - e_time2)/e_time1 ; \
    T5 = - 0.5 * T8 * T8 ; \
    `Fn_DExp( T6 , T5 , T0 )   \
    e1deg = e1deg0 + TRAPES1MAX / e_time1 * T6 * Isub_fac ; \
    `Fn_SU( e1deg, e1deg, 1.0, 1e-2, T0 ) \
 end

`define Fn_calcGC1ES1_drift(DVddp ,ids) begin \
    T2 = ids / Weff ; \
    `Fn_SLtemp( T2 , T2 , `C2p72_DTIME1 , `C2p72_DTIME1_dlt )  \
    T3 = T2  * TRAPDTIME1 ;  \
    `Fn_SLtemp( T4 , (T2 * DEGTIME) , `IsubT_min , 1e-3 )  \
    `Fn_SLtemp( T5 , (T2 * TRAPDTIME2) , `IsubT_min , 1e-3 )  \
    gc_time  = ln(T4) ; \
    gc_time1 = ln(T3) ; \
    gc_time2 = ln(T5) ; \
    T8 = (gc_time - gc_time2)/gc_time1 ; \
    T5 = - 0.5 * T8 * T8 ; \
    `Fn_DExp( T6 , T5, T0 )   \
    DVddp = TRAPD1MAX / gc_time1 * T6 ; \
 end

   // Initilization variables
   gc1deg0   = TRAPGC1 / `C_cm2m_p3 ; 
   e1deg0    = TRAPES1 ; 
   abstime   = ($abstime - `pretime) + `Small ;
   Isub_trap = 0.0 ;

   if( CODEG == 1 && TRAPGC1MAX > 0 ) begin

     if( CODEGSTEP == 0 ) begin // DC simulation
       // DC simulation with Isub
       Isub_trap = Isub ; 
       Ids_trap  = Ids ; 
     end else begin
       // transient simulation with idt( Isuba )
       Isub_trap = V(idtag1) / abstime ;  // idt( Isuba  );
       Ids_trap  = V(idtag2) / abstime ;  // idt( Ids );
     end

     //-----------------------------------------------------------*
     //* Calculation of trap density gc1, e1 and DVddp
     //*-----------------//
     `Fn_calcGC1ES1(gc1,e1,Isub_trap)
     `Fn_calcGC1ES1_drift(DVddp, Ids_trap) 

      if ( TRAPA > 0 && ($abstime > pretime0 || CODEGSTEP==0) ) begin
         if( TRAPBTI > 0.0 ) begin
           tcycle = (CODEGSTEP == 1) ? V(idtag3) / abstime : 1 ;
           ontime = abstime * tcycle ;
           Vgsonave = (CODEGSTEP == 1 && ontime > 0 ) ? V(idtag4) / ontime : Vgs ;
           ts_bti = tcycle * DEGTIME ;
           exp_time = 1.0 - lexp( -ts_bti / TRAPBTI ) ;
         end else begin
           exp_time = 1.0 ;
         end
//       Eox = ( Vgp - Pb2 ) / TOX ;
         Vgpon = Vgsonave - Vfb ; // Vfb = VFBC + dVth_bti ; calc. in main module
         Eox = ( Vgpon - Pb2 ) / TOX ; 
         dVth_bti = TRAPA * lexp( TRAPB * Eox ) * exp_time ;
`ifdef _AGING_DEBUG_
  $write(" %M abstime %g ts_bti %g tcycle %g exp_time %g dVth_bti %g TRAPA %g TRAPB %g Eox %g T1 %g \n",
  abstime , ts_bti , tcycle , exp_time , dVth_bti , TRAPA , TRAPB , Eox , T1 );
`endif
      end else begin
         dVth_bti = 0.0 ;
      end

`ifdef _AGING_DEBUG_
       // check the convergency of Ntrap 
       `Fn_calcNKT(NtK1,gc1,e1)
       T1 = (NtK1 <= 0 ) ? 0.0 : abs(NtK1 - NtK10) /NtK1 * 100 ;
       $write("%M Isub = %g Isub_trap = %g gc1 %g e1 %g NtK10 %g NtK1 %g Err: %g \n",
       Isub , Isub_trap, gc1, e1 , NtK10, NtK1,T1 );
       $write("%m abstime = %g DVddp = %g <Ids> = %g Ids = %g Vgse = %g Vdse = %g\n",
       abstime , DVddp, Ids_trap , Ids , Vgse , Vdse) ;
`endif

   end // (CODEG == 1 )


end // HSMHV_eval_aging.inc

