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
 FILE : HSMHV_temp_eval_rdris.inc 

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

            
      // Phonon Scattering(temperature-dependent part) //
      T1 =  `Fn_Pow( Tratio, RDRMUETMPS ) ;
      RRDRMUES = MKS_RDRMUES / T1 ;
             
      // Velocity Temperature Dependence //
      T0 = 1.8 + 0.4 * Tratio + 0.1 * Tratio * Tratio - RDRVTMPS * ( 1.0 - Tratio ) ;
      RRDRVMAXS = MKS_RDRVMAXS / T0 ;
              
      //Toshiba model //
      UC_RDRBB_s = UC_RDRBB_s+RDRBBTMPS*(TTEMP-KTNOM) ;

      // end of HSMHVtemp_eval_rdri.h //
