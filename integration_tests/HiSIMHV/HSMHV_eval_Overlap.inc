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
 FILE : HSMHS_eval_Overlap.inc 

 Date : 2019.04.26

 released by Hiroshima University

***********************************************************************/

     //-----------------------------------*
     //* Surface potential model
     //*------------------------//
     flg_COOVLPS = 0; flg_COOVLP = 0;  
     i_over = 0; flg_calcqover = 0;
     flg_never_reach_vfbover = 0;
     for (i_over=1;i_over<=4;i_over=i_over+1) begin 
       flg_calcqover = 0 ;
       case (i_over) // calc. the terminal biases and etc.
       1: if( COOVLPS == 1 && LOVERS > 0.0 && UC_NOVERS > 0.0 ) begin
            //-------------------------------------*
            //* Source side Qovs( V(gP,bP), V(sP,bP) )
            //*-----------------//
            flg_calcqover = 1 ;
            flg_COOVLPS = 1;
            Vgbgmt = Vgsi- Vbsi;
            Vxbgmt = - Vbsi;
            Nover_func = UC_NOVERS  ;
            Lover_func = LOVERS ;
            Wdep_func  = 0.0;
            cnst0over_func = cnst0overs ;
	    Cox0_func = Cox0; //added

          end // flg_COOVLPS = 1;
       2: if( COOVLPS == 1 && LOVERS > 0.0 && UC_NOVERS > 0.0 && UC_CVDSOVER != 0.0 && COTRENCH != 1 ) begin
            //--------------------------------------*
            //* Source side Qovsext( V(gP,bP), V(s,bP) )
            //*-----------------//
            flg_calcqover = 1 ;
            Vgbgmt = Vgsei - Vbsei ;
            Vxbgmt = - Vbsei ;
          end
       3: if( COOVLP == 1 && LOVERLD > 0.0 && UC_NOVER > 0.0 ) begin
            //--------------------------------------*
            //* Drain side Qovd( V(gP,bP), V(dP,bP) )
            //*-----------------//
            flg_calcqover = 1 ;
            flg_COOVLP = 1;
            Vgbgmt = Vgsi - Vbsi ;
            Vxbgmt = Vdsi - Vbsi ;
            Nover_func = UC_NOVER ;
            Lover_func = LOVERLD  + WTRENCH * COTRENCH ;
            Wdep_func  = Wdep ;
            cnst0over_func = cnst0over ;
	    Cox0_func = Coxb0; //added
            Lover_func = - Lover_func ;

            begin : HSMHVeval_loverLim
`include "HSMHV_eval_loverLim.inc"
            end

          end // flg_COOVLP = 1;
       4: if( COOVLP == 1 && LOVERLD > 0.0 && UC_NOVER > 0.0 && UC_CVDSOVER != 0.0 && COTRENCH != 1 ) begin
            //--------------------------------------*
            //* Drain side Qovdext( V(gP,bP), V(d,bP) )
            //*-----------------//
            flg_calcqover = 1 ;
            Vgbgmt = Vgsei - Vbsei ;
            Vxbgmt = Vdsei - Vbsei ;
          end
       endcase // end of case

       if(flg_calcqover) begin 
         begin : HSMHVevalQover_qov
`include "HSMHV_eval_qover.inc"
         end  // HSMHVevalQover_qov

         case (i_over)  // calc. the total overlap charges
         1: begin
              if ( COTRENCH == 1) begin
               T4 = weffcv_nf * Lover_func;
              end else begin
               T4 = weffcv_nf * Lover_func * ( 1 - UC_CVDSOVER ) ;
              end
              Qovs  = T4 * QsuLD ;
              QbsLD = T4 * QbuLD ;
              RS_Ps0LD = Ps0LD; RS_QbuLD = QbuLD; // OPP and actual uses
            end // COOVLPS != 0 end //
         2: begin 
              T4 = weffcv_nf * Lover_func * UC_CVDSOVER ;
              Qovsext =  T4 * QsuLD ;
              QbsLDext = T4 * QbuLD ;
            end
         3: begin
              if( COTRENCH == 1 ) begin
                T4 = weffcv_nf * Lover_func ;
              end else begin
                T4 = weffcv_nf * Lover_func * ( 1 - UC_CVDSOVER ) ;
              end
              RD_Ps0LD = Ps0LD ;
	      if ( RDRQOVER == 0 ) begin //original:
              RD_QbuLD = QbuLD ;
	      end
              // saves for the trench part
              Qovd  = T4 * ( QsuLD ) ;
              QbdLD = T4 * ( QbuLD ) ;
              Qbd_qs = QbdLD ; //for CONQSOV=1
            end // 
         4: begin 
              T4 = weffcv_nf * Lover_func * UC_CVDSOVER ;
              Qovdext =  T4 * QsuLD ;
              QbdLDext = T4 * QbuLD ;
            end

         endcase // end of case
       end // flg_calcqover == 1
     end // enf of loop for overlap charges

     //
     // Additional calculation for the another version of RD_QbuLD:
     //
     if ( RDRQOVER > 0 ) begin //trial 1
       flg_never_reach_vfbover = 1;
       if( COOVLP == 1 && LOVERLD > 0.0 && UC_NOVER > 0.0 ) begin
          Vgbgmt = Vgsi - Vbsi ;
          Vxbgmt = Vdsi - Vbsi ;
          Nover_func = UC_NOVER ;
          Lover_func = 0.0 ; //not used in this additional calculation
          Wdep_func  = Wdep ;
          cnst0over_func = cnst0over ;
          Cox0_func = Coxb0; //added
          begin : HSMHVevalQover_qov_rdr
`include "HSMHV_eval_qover.inc"
          end
          RD_QbuLD = QbuLD ;
       end // flg_COOVLP = 1;
     end //

     //-----------------------------------*
     //* Simplified model
     //* Source side 
     //*-----------------//
     if( COOVLPS == 1 && LOVERS > 0.0 && UC_NOVERS == 0.0 ) begin
       flg_COOVLPS = 1 ;
       cov_slp = MKS_OVSLP ;
       cov_mag = OVMAG ;
       T1 = Cox0 * weffcv_nf ;
       if(mode == `HiSIM_NORMAL_MODE) begin
          T4 = cov_slp * T1 * ( cov_mag + Vgs ) ;
          T5 = LOVERS * T1 ;
          T9 = 1.2e0 - Ps0 ;
       end else begin
          T4 = cov_slp * T1 * ( cov_mag + Vgs - Vds ) ;
          T5 = LOVERLD * T1 ;
          T9 = 1.2e0 + Vds - Psl ;
       end
       Qgos = Vgs * T5 - T4 * T9 ;
     end 
     //-----------------------------------*
     //* Simplified model
     //* Drain side
     //*-----------------//
     if( COOVLP == 1 && LOVERLD > 0.0 && UC_NOVER == 0.0 ) begin
       flg_COOVLP = 1 ;
       cov_slp = MKS_OVSLP ;
       cov_mag = OVMAG ;
       T1 = Coxb0 * weffcv_nf ;
       if(mode == `HiSIM_NORMAL_MODE) begin
          T4 = cov_slp * T1 * ( cov_mag + Vgs - Vds ) ;
          T5 = LOVERLD * T1 ;
          T9 = 1.2e0 + Vds - Psl ;
       end else begin
          T4 = cov_slp * T1 * ( cov_mag + Vgs ) ;
          T5 = LOVERS * T1 ;
          T9 = 1.2e0 - Ps0 ;
       end
       Qgod = ( Vgs - Vds ) * T5 - T4 * T9 ;
     end 

     //--------------------------------------*
     //* Additional constant capacitance model
     //* Source side 
     //*-----------------//
//     Cgos = - Cox0 * LOVERS * weffcv_nf ;
//     Qgos = - Cgos * Vgsi ;
     if( CGSO_GIVEN ) begin
        Cgsoe = UC_CGSO * ( - weffcv_nf ) ;
     end else begin
	if( flg_COOVLPS == 0 ) begin
           Cgsoe = - Cox0 * LOVERS * weffcv_nf ; 
	end
     end
     Qgso  = - Cgsoe * Vgsei ;

     //--------------------------------------*
     //* Additional constant capacitance model
     //* Drain side
     //*-----------------//
//     Cgod = - Cox0 * LOVERLD  * weffcv_nf ; 
//     Cgod = Cgod - Coxb0 * UC_WTRENCH * weffcv_nf ; 
//     Qgod = - Cgod * (Vgsi - Vdsi) ;
     if( CGDO_GIVEN ) begin
        Cgdoe = UC_CGDO * ( - weffcv_nf ) ;
     end else begin
	if( flg_COOVLP == 0 ) begin
           Cgdoe = - Coxb0 * LOVERLD * weffcv_nf ; 
	end
     end
     Qgdo  = - Cgdoe * (Vgsei - Vdsei) ;

     //------------------------------------------------*
     //* Additional Vds and Pds depend capacitance model
     //*-----------------//
     if(mode == `HiSIM_NORMAL_MODE) begin
        QodAD =   QOVADD * ( Vds - Pds ) ;
        Qovd_ADD  = T4 * ( QodAD ) ;
        QbdLD_ADD = T4 * ( QodAD ) ;
     end else begin
        QodAD = - QOVADD * ( Vds - Pds ) ;
        Qovs_ADD  = T4 * ( QodAD ) ;
        QbsLD_ADD = T4 * ( QodAD ) ;
     end

     //-------------------------------------------*
     //* Gate/Bulk overlap charge: Qgbo
     //*-----------------//
     Cgbo_loc = - UC_CGBO * Lgate ;
     Qgbo = - Cgbo_loc * (Vgsi -Vbsi) ;
