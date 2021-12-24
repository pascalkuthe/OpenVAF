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
 FILE : HSMHV_eval_loverLim.inc 

 Date : 2019.04.26

 released by Hiroshima University

***********************************************************************/

    // calc. bias dependent Lover for 2.4.0
    if( Lover_func < 0.0 && QOVJUNC > 0 && COTRENCH == 1 ) begin
      Lover_func = - Lover_func ; // now, Lover_func positive.
      T1 = LOVERLD ;
      Vxb_lim = (T1 * T1 / kjunc) - VBI ;
      if( OLMDLT > 0 ) begin
        `Fn_SU_POW( Vxbgmt , Vxbgmt , Vxb_lim , OLMDLT )
        `Fn_SZ( T2 , (Vxbgmt + VBI) , 0.1 , T9 )
        Wjunc0 = sqrt ( kjunc * T2 ) * QOVJUNC ;
        Lover_func = Lover_func - Wjunc0 ;
      end
    end // if ( Lover_func < 0.0 && QOVJUNC > 0 && COTRENCH == 1 )
