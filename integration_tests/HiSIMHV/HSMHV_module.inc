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
 FILE : HSMHV_module.inc 

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


// Branch Definitions

   branch(sb,s ) BRsbs  ;
   branch(db,d ) BRdbd  ;
   branch(dp,sp) BRdpsp ;
   branch(dp,bp) BRdpbp ;
   branch(sp,bp) BRspbp ;
   branch(gp,sp) BRgpsp ;
   branch(gp,dp) BRgpdp ;
   branch(gp,bp) BRgpbp ;
   branch(g ,gp) BRggp  ;
   branch(gp,s ) BRgps  ;
   branch(gp,d ) BRgpd  ;
   branch(d ,dp) BRddp  ;
   branch(d ,s ) BRds   ;
   branch(sp,s ) BRsps  ;
   branch(bp,sp) BRbpsp ;
   branch(bp,dp) BRbpdp ;
   branch(bp,d ) BRbpd  ;
   branch(bp,s ) BRbps  ;
   branch(sb,bp) BRsbbp ;
   branch(db,bp) BRdbbp ;
   branch(b ,bp) BRbbp  ;
   branch(b ,sb) BRbsb  ;
   branch(b ,db) BRbdb  ;
   branch(d ,bp) BRdbp  ;
   branch(s ,bp) BRsbp  ;

//
// Parameter definitions

//******* Device Parameters *******//
`IPRco( L         , 2.0E-6     , "m"                 ,  1n , inf , "Gate length ")
`IPRco( W         , 5.0E-6     , "m"                 ,  1n , inf , "Gate width ")
`IPRco( NRD       , 1.0        , "-"                 ,   0 , inf , "Number of squares in drain ")
`IPRco( NRS       , 1.0        , "-"                 ,   0 , inf , "Number of squares in source ")
`IPRco( NGCON     , 1.0        , "-"                 ,   1 , inf , "Number of gate contacts ")
`IPRnb( XGW       , 0E0        , "m"                             , "Distance from gate contact to channel edge ")
`IPRnb( XGL       , 0E0        , "m"                             , "Offset of gate length due to variation in patterning ")
`IPRco( NF        , 1.0        , "-"                 ,   1 , inf , "Number of fingers ")
`IPRco( SA        , 0          , "m"                 ,   0 , inf , "Distance from STI edge to Gate edge  ")
`IPRco( SB        , 0          , "m"                 ,   0 , inf , "Distance from STI edge to Gate edge  ")
`IPRco( SD        , 0          , "m"                 ,   0 , inf , "Distance from Gate edge to Gate edge  ")
`IPRnb( DTEMP     , 0          , "K"                             , "Device temperature change")
`IPM
`IPRoo( NSUBCDFM  , 3E17       , "1/cm^3"            ,   0 , inf , "Substrate impurity concentration for DFM")

`ifdef COEXTDIO 
       /* for Junction Diode Model */
`else 
`IPRco( AD        , 0.0        , "m^2"               ,   0 , inf , "Drain area ")
`IPRco( AS        , 0.0        , "m^2"               ,   0 , inf , "Source area ")
`IPRco( PD        , 0.0        , "m"                 ,   0 , inf , "Drain perimeter ")
`IPRco( PS        , 0.0        , "m"                 ,   0 , inf , "Source perimeter ")
`endif /* End of COEXTDIO */

//******* Model Flags *******//
`MPIcc( CORSRD    , 3          , "-"                 ,  0  ,   3 , "Handling of Rs and Rd ") //HV1
`MPIcc( CORS      , 0          , "-"                 ,  0  ,   1 , "source side resistance OFF(0)/ON(1)")
`MPIcc( CORD      , 1          , "-"                 ,  0  ,   1 , "drain  side resistance OFF(0)/ON(1)")
`MPIsw( COIPRV    , 0          , "-"                             , "Use ids_prv as initial guess of Ids(internal flag) ")
`MPIsw( COPPRV    , 0          , "-"                             , "not supported ")
`MPIcc( COADOV    , 1          , "-"                 ,  0  ,   2 , "Add overlap to intrinsic (1)  No intrinsic (2) No overlap (0) ") //20171005
`MPIsw( COISUB    , 0          , "-"                             , "Calculate isub ")
`MPIsw( COIIGS    , 0          , "-"                             , "Calculate igate ")
`MPIsw( COGIDL    , 0          , "-"                             , "Calculate igidl ")
`MPIsw( COFLICK   , 0          , "-"                             , "Calculate 1/f noise ")
`MPIsw( COISTI    , 0          , "-"                             , "Calculate STI ")
`MPIsw( CONQS     , 0          , "-"                             , "Calculate in nqs mode or qs mode ")
`MPIsw( CONQSOV   , 0          , "-"                             , "Calculate in nqs mode or qs mode ")
`MPIsw( COTHRML   , 0          , "-"                             , "Calculate thermal noise ")
`MPIsw( COIGN     , 0          , "-"                             , "Calculate induced gate noise ")
`MPIsw( CODFM     , 0          , "-"                             , "Calculation of model for DFM ")
`MPIcc( COQOVSM   , 1          , "-"                 ,   0 ,   2 , "select smoothing method of Qover ")
`MPIsw( COSYM     , 0          , "-"                             , "Model selector for symmetry device ")
`MPIsw( COOVLP    , 1          , "-"                             , "Calculate overlap charge on the drain side ")
`MPIsw( COOVLPS   , (COSYM ? COOVLP : 0) , "-"                   , "Calculate overlap charge on the source side ")
`MPIcc( COTEMP    , 0          , "-"                 ,   0 ,   3 , "Model flag for temperature dependence ")
`MPIsw( CORDRIFT  , 1          , "-"                             , "selector for drift resistance ")
`MPIsw( COERRREP  , 1          , "-"                             , "selector for error report ")
`MPIcc( CODEP     , 0          , "-"                 ,   0 ,   3 , "selector for depletion device ")
`MPIcc( COVDSRES  , 2          , "-"                 ,  -1 ,   3 , "Vdssatres, analytical Vdsres model switch: (-1) Vdsres=Vds; (2) Vdsatres  ln(cosh) equation  (3) Vdssatres ln(1+exp) depmos3")
`MPIsw( CODDLT    , 1          , "-"                             , "selector for DDLT model ")
`MPIcc( COHBD     , 0          , "-"                 ,  -1 ,   1 , "selector for Hard Breakdown Model ") 
`MPIsw( COSNP     , 0          , "-"                             , "Calculate snapback (bjt effects) ")
`MPInb( INFO      , 0          , "-"                             , "")
`MPIsw( CODIO     , 0          , "-"                             , "selector for updated diode model ")
`MPIcc( COFIXCSS  , 0          , "-"                 ,   0 ,   1 , "Selector for Css problem ")
`MPIcc( COFIXAG   , 0          , "-"                 ,   0 ,   1 , "Selector for Aging problem 0=240 1=250 ")
`MPIcc( COOVJUNC  , 0          , "-"                 ,   0 ,   1 , "Selector for Wjunc,ov model(0: phis,over, 1: Vds) ")

`ifdef _AGING_
`BPIsw( CODEG     , 0          , "-"                             , "Selector for Aging model ")
`BPIsw( CODEGES0  , 0          , "-"                             , "Selector for TRAPES0/TRAPGC0 ")
`BPIcc( CODEGSTEP , 0          , "-"                 ,   0 ,   2 , "Selector for Aging model ")
`endif

//*** model and instance parameter ***
`BPIsw( CORG      , 0          , "-"                             , "Activate gate resistance(1) or not(0) ")
`BPIsw( CORBNET   , 0          , "-"                             , "Activate body resistance(1) or not(0) ")
`BPIcc( COSELFHEAT, 0          , "-"                 ,   0 ,   2 , "selector for self heating model ")
`BPIsw( COSUBNODE , 0          , "-"                             , "Switch tempNode to subNode ")
`BPIsw( COTRENCH  , 0          , "-"                             , "Trench overlap capacitance (1)") // 2.4.0
`BPRco( RBPB      , 50.0E0     , "ohm"               ,   0 , inf , "Substrate resistance network")
`BPRco( RBPD      , 50.0E0     , "ohm"               ,   0 , inf , "Substrate resistance network")
`BPRco( RBPS      , 50.0E0     , "ohm"               ,   0 , inf , "Substrate resistance network")
`BPRnb( RDOV13    , 1.0        , "-"                             , "Dependence coeff. for overlap length ")            //HV1
`BPRnb( RDSLP1    , 0.0        , "-"                             , "LDRIFT1 dependence of resistance for CORSRD=1")    //HV1
`BPRnb( RDVG11    , 0.0        , "-"                             , "Vgs dependence of RD for CORSRD=1,3")              //HV1
`BPRnb( RDICT1    , 1.0        , "-"                             , "LDRIFT1 dependence of resistance for CORSRD=1")    //HV1  
`BPRco( LOVERLD   , 1.0E-6     , "m"                 ,   0 , inf , "Overlap length on drain side  ")
`BPRco( WTRENCH   , 1.0E-6     , "m"                 ,   0 , 10E-6,"Overlap length on drain side, trench bottom")
`BPRco( LOVER     , (COSYM ? LOVERLD : 30E-9) , "m"  ,   0 , inf , "Overlap length of the source region, if LOVERS is not determined.")
`BPRco( LOVERS    , (COSYM ? LOVERLD : LOVER) , "m"  ,   0 , inf , "Overlap length on source side  ")
`BPRco( LDRIFT1   , 1.0E-6     , "m"                 ,   0 , inf , "Parameter for drift region length-1  ")
`BPRco( LDRIFT2   , 1.0E-6     , "m"                 ,   0 , inf , "Parameter for drift region length-2  ")
`BPRco( LDRIFT1S  , (COSYM ? LDRIFT1 :  0.0) , "m"   ,   0 , inf , "Parameter for drift region length-1 on source side ")
`BPRco( LDRIFT2S  , (COSYM ? LDRIFT2 : 1E-6) , "m"   ,   0 , inf , "Parameter for drift region length-2 on source side ")
`BPRco( LDRIFTS   , (LDRIFT1S+LDRIFT2S) , "m"        ,   0 , inf , "Parameter for drift region on source side ")
`BPRnb( SUBLD1    , 0.0        , "-"                             , "Parameter for impact-ionization current in the drift region  ")
`BPRnb( SUBLD2    , 0.0        , "m^-1*V^3/2"                    , "Parameter for impact-ionization current in the drift region  ")
`BPRco( RD        , 0.0        , "ohm*m"             ,   0 , inf , "Drain contact resistance   ")  //HV1
`BPRco( RS        , (COSYM ? RD : 0) , "ohm*m"       ,   0 , inf , "Source contact resistance  ")  //HV1
`BPRoo( NPEXT     , 5.0E17     , "1/cm^3"            ,   0 , inf , "Pocket extension ")
`BPRnb( VOVER     , 0.3        , "-"                             , "Parameter for overshoot  ")
`BPRoo( FALPH     , 1.0        , ""                  ,   0 , inf , "Parameter for 1/f noise ")
`BPRco( CGBO      , 0.0        , "F/m"               ,   0 , inf , "Cgbo ")
`BPRco( RTH0      , 0.1        , "Kcm/W"             ,   0 , inf , "Thermal resistance ")
`BPRnb( POWRAT    , 1.0        , ""                              , "Thermal dissipation")
`BPRnb( RDVD      , 7.0E-2     , "ohm*cm/V"                      , "Vds dependence of RD for CORSRD=1,3")   //HV1
`BPRnb( RD23      , 5E-3       , ""                              , "modification of RD for CORSRD=2,3")     //HV1
`BPRnb( RD24      , 0.0        , ""                              , "Vgs dependence of RD for CORSRD=2,3")   //HV1
`BPRnb( RDVB      , 0.0        , ""                              , "Vbs dependence of RD for CORSRD=1,3")   //HV1
`BPRco( CVDSOVER  , 0.0        , ""                  ,   0 , inf , "Vds drop along the overlap ")

//******* Technology Model Parameters *******//
`MPIty( TYPE      , 1          , ""                              , "mosfet TYPE: 1=NMOS and -1=PMOS")
`MPRnb( VERSION   , 2.50       , ""                              , "Model version ")
`MPRoo( VMAX      , 1.0E7      , "cm/s"              ,   0 , inf , "Saturation velocity  ")
`MPRnb( VMAXT1    , 0.0        , "-"                             , "Saturation velocity coeff.  ")
`MPRnb( VMAXT2    , 0.0        , "-"                             , "Saturation velocity coeff.  ")
`MPRnb( BGTMP1    , 90.25E-6   , "V/K"                           , "First order temp. coeff. for band gap  ")
`MPRnb( BGTMP2    , 1.0E-7     , "V/K^2"                         , "Second order temp. coeff. for band gap  ")
`MPRnb( EG0       , 1.1785E0   , ""                              , "Bandgap")
`MPRoo( TOX       , 7E-9       , "m"                 ,   0 , inf , "Oxide thickness  ")
`MPRnb( TOXB      , TOX        , "m"                             , "Oxide thickness of gate-drain overlap region")
`MPRco( XLD       , 0          , "m"                 ,   0 , inf , "Lateral diffusion of S/D under the gate  ")
`MPRnb( RDOV11    , 0.0        , ""                              , "Dependence coeff. for overlap length ")           //HV1
`MPRnb( RDOV12    , 1.0        , ""                              , "Dependence coeff. for overlap length ")           //HV1
`MPRnb( RDSLP2    , 1.0        , ""                              , "LDRIFT2 dependence of resistance for CORSRD=1")   //HV1
`MPRnb( RDICT2    , 0.0        , ""                              , "LDRIFT2 dependence of resistance for CORSRD=1")   //HV1   
`MPRnb( SUBLD1L   , 0.0        , "um^subld1l"                    , "Impact-ionization current in the drift region  ")
`MPRnb( SUBLD1LP  , 1.0        , "-"                             , "Impact-ionization current in the drift region  ")
`MPRnb( XPDV      , 0.0        , "m^-1"                          , "Impact-ionization current in the drift region  ")
`MPRnb( XPVDTH    , 0.0        , "V"                             , "Impact-ionization current in the drift region  ")
`MPRnb( XPVDTHG   , 0.0        , "V^-1"                          , "Impact-ionization current in the drift region  ")
`MPRco( DDLTMAX   , 10         , ""                  ,   1 , inf , "Smoothing coefficient for Vds")
`MPRco( DDLTSLP   , (`SUBVERSION<3 ?  0 : 10) , ""   ,   0 , inf , "Lgate dependence of smoothing coefficient")
`MPRnb( DDLTICT   , (`SUBVERSION<3 ? 10 :  0) , ""               , "Lgate dependence of smoothing coefficient")
`MPRnb( VFBOVER   , -0.5       , ""                              , "flat-band voltage in overlap region")
`MPRco( NOVER     , 3E16       , ""                  ,   0 , inf , "Impurity concentration in overlap region")
`MPRco( NOVERS    , (COSYM ? NOVER : 1E17) , ""      ,   0 , inf , "Impurity concentration in overlap region at source side")
`MPRcc( OLMDLT    , 5.0        , "-"                  ,  0,  100 , "smooth delta for Wjuncld ceiling upto Lover_func (COTRENCH=1)") // 2.4.0 
`MPRnb( XWD       , 0.0        , "m"                             , "Lateral diffusion along the width dir.  ")
`MPRnb( XWDC      , XWD        , "m"                             , "Lateral diffusion along the width dir. for capacitance  ")
`MPRnb( XL        , 0.0        , "m"                             , "Gate length offset due to mask/etch effect  ")
`MPRnb( XW        , 0.0        , "m"                             , "Gate width offset due to mask/etch effect  ")
`MPRco( SAREF     , 1E-6       , "m"                 ,   0 , inf , "Reference distance from STI edge to Gate edge  ")
`MPRco( SBREF     , 1E-6       , "m"                 ,   0 , inf , "Reference distance from STI edge to Gate edge  ")
`MPRnb( LL        , 0.0        , ""                              , "Gate length parameter ")
`MPRnb( LLD       , 0.0        , ""                              , "Gate length parameter ")
`MPRnb( LLN       , 0.0        , ""                              , "Gate length parameter ")
`MPRnb( WL        , 0.0        , ""                              , "Gate width parameter ")
`MPRnb( WL1       , 0.0        , ""                              , "Gate width parameter ")
`MPRnb( WL1P      , 1.0        , ""                              , "Gate width parameter ")
`MPRnb( WL2       , 0.0        , ""                              , "Gate width parameter ")
`MPRnb( WL2P      , 1.0        , ""                              , "Gate width parameter ")
`MPRnb( WLD       , 0.0        , ""                              , "Gate width parameter ")
`MPRnb( WLN       , 0.0        , ""                              , "Gate width parameter ")
`MPRco( RSH       , 0.0        , "ohm"               ,   0 , inf , "drain diffusion sheet  resistance   ")
`MPRco( RSHS      , RSH        , "ohm"               ,   0 , inf , "Source diffusion sheet  resistance   ")
`MPRco( RSHG      , 0.0        , ""                  ,   0 , inf , "Gate-electrode sheet resistance ")
`MPRco( XQY       , 0.0        , "m"                 ,   0 , inf , "Distance from drain junction to maximum electric field point ")
`MPRnb( XQY1      , 0.0        , "F m^XQY2"                      , "Vbs dependence of Qy ")
`MPRnb( XQY2      , 2.0        , ""                              , "Lgate dependence of Qy ")
`MPRnb( VFBC      , (CODEP!=0 ? -0.2 : -1.0) , "V"               , "Constant part of Vfb  ")
`MPRnb( VBI       , 1.1        , "V"                             , "Built-in potential  ")
`MPRoo( NSUBC     , (CODEP!=0 ? 5e16 : 3E17) , "1/cm^3", 0 , inf , "Constant part of Nsub  ")
`MPRoo( PARL2     , 10.0E-9    , "m"                , -inf ,   L , "Under diffusion  ")
`MPRcc( LP       , (CODEP!=0 ? 0 : 1.5e-8 ) , "m" ,      0 ,   L , "Length of pocket potential  ")
`MPRoo( NSUBP    , (CODEP!=0 ? 1e17 : 1e18) , "1/cm^3" , 0 , inf , "Maximum pocket concentration ")
`MPRnb( NSUBP0    , 0.0        , ""                              , "Pocket implant parameter ")
`MPRnb( NSUBWP    , 1.0        , ""                              , "Pocket implant parameter ")
`MPRnb( SCP1      , 0.0        , "-"                             , "Parameter for pocket  ")
`MPRnb( SCP2      , 0.0        , "1/V"                           , "Parameter for pocket  ")
`MPRnb( SCP3      , 0.0        , "m/V"                           , "Parameter for pocket  ")
`MPRnb( SC1       , 0.0        , "-"                             , "Parameter for SCE  ")
`MPRnb( SC2       , 0.0        , "1/V"                           , "Parameter for SCE  ")
`MPRnb( SC3       , 0.0        , "m/V"                           , "Parameter for SCE  ")
`MPRnb( SC4       , 0.0        , "1/V"                           , "Parameter for SCE  ")
`MPRnb( PGD1      , 0.0        , "V"                             , "Parameter for gate-poly depletion  ")
`MPRnb( PGD2      , 1.0        , "V"                             , "Parameter for gate-poly depletion  ")
`MPRnb( PGD4      , 0.0        , "-"                             , "Parameter for gate-poly depletion  ")
`MPRnb( NDEP      , 1.0        , "-"                             , "Coeff. of Qbm for Eeff  ")
`MPRnb( NDEPL     , 0.0        , "-"                             , "Coeff. of Qbm for Eeff  ")
`MPRnb( NDEPLP    , 1.0        , "-"                             , "Coeff. of Qbm for Eeff  ")
`MPRnb( NINV      , 0.5        , "-"                             , "Coeff. of Qnm for Eeff  ")
`MPRnb( MUECB0    , 1.0E3     , "cm^2/Vs"                        , "Const. part of coulomb scattering  ")
`MPRnb( MUECB1    , 100.0     , "cm^2/Vs"                        , "Coeff. for coulomb scattering  ")
`MPRnb( MUEPH0    , 300.0E-3   , "-"                             , "Power of Eeff for phonon scattering  ")
`MPRoo( MUEPH1    , (TYPE>0 ? 20E3 : 9E3) , ""       ,   0 , inf , "Phonon scattering ")
`MPRnb( MUEPHW    , 0.0        , "-"                             , "Phonon related mobility reduction ")
`MPRnb( MUEPWP    , 1.0        , "-"                             , "Phonon scattering parameter ")
`MPRnb( MUEPHL    , 0.0        , "-"                             , "Phonon scattering parameter ")
`MPRnb( MUEPLP    , 1.0        , "-"                             , "Phonon scattering parameter ")
`MPRnb( MUEEFB    , 0.0        , "1/V"                           , "Vbs dependence of the phonon scattering ")
`MPRnb( MUEPHS    , 0.0        , "-"                             , "Mobility modification due to small size")
`MPRnb( MUEPSP    , 1.0        , "-"                             , "Mobility modification due to small size")
`MPRnb( VTMP      , 0.0        , "-"                             , "Temperature dependence of saturation velocity")
`MPRnb( WVTH0     , 0.0        , "V"                              , "Threshold voltage shift")
`MPRnb( MUESR0    , 2.0        , "-"                             , "Power of Eeff for S.R. scattering  ")
`MPRoo( MUESR1    , (CODEP!=0 ? 5E15 : 6e14 ) , "-"  ,   0 , inf , "Coeff. for S.R. scattering  ")
`MPRnb( MUESRL    , 0.0        , "-"                             , "Surface roughness parameter ")
`MPRnb( MUESRW    , 0.0        , "-"                             , "Change of surface roughness related mobility ")
`MPRnb( MUESWP    , 1.0        , "-"                             , "Change of surface roughness related mobility ")
`MPRnb( MUESLP    , 1.0        , "-"                             , "Surface roughness parameter ")
`MPRnb( MUETMP    , 1.5        , "-"                             , "Parameter for mobility  ")
`MPRco( BB        , (TYPE>0 ? 2 : 1) , "-"           , 0.1 , inf , "Empirical mobility model coefficient  ")
`MPRnb( SUB1      , 10         , "1/V"                           , "Parameter for Isub  ")
`MPRnb( SUB2      , 25         , "V"                             , "Parameter for Isub  ")
`MPRnb( SVGS      , 0.8E0      , ""                              , "Coefficient for Vg of Psislsat ")
`MPRnb( SVBS      , 0.5E0      , ""                              , "Coefficient for Vbs of Psislsat ")
`MPRnb( SVBSL     , 0E0        , ""                              , "Lgate dependence of SVBS ")
`MPRnb( SVBSLP    , 1.0        , ""                              , "Lgate dependence of SVBS ")
`MPRnb( SVDS      , 0.8E0      , ""                              , "Substrate current dependence on Vds ")
`MPRco( SLG       , 30E-9      , ""                  ,   0 , inf , "Substrate current dependence on Lgate ")
`MPRnb( SUB1SNP   , SUB1       , "1/V"                           , "Parameter for Isub (snapback) ")
`MPRnb( SUB2SNP   , 0.6*SUB2   , "V"                             , "Parameter for Isub (snapback) ")
`MPRnb( SVDSSNP   , SVDS       , ""                              , "Substrate current dependence on Vds (snapback)")
//`MPRnb( SVGSSNP   , SVGS       , ""                              , "Coefficient for Vg of Psislsat (snapback)")
//`MPRnb( SVBSSNP   , 0.5E0      , ""                              , "Coefficient for Vbs of Psislsat(snapback)")
`MPRnb( SUB1L     , 2.5E-3     , ""                              , "Lgate dependence of SUB1")
`MPRnb( SUB1LP    , 1.0        , ""                              , "Lgate dependence SUB1 ")
`MPRnb( SUB2L     , 2E-6       , ""                              , "Lgate dependence of SUB2")
`MPRnb( SUBTMP    , 0          , ""                              , "Temperature dependence of Isub  ")
`MPRnb( FN1       , 50E0       , ""                              , "Coefficient of Fowler-Nordheim-current contribution")
`MPRnb( FN2       , 170E-6     , ""                              , "Coefficient of Fowler-Nordheim-current contribution")
`MPRnb( FN3       , 0E0        , ""                              , "Coefficient of Fowler-Nordheim-current contribution")
`MPRnb( FVBS      , 12E-3      , ""                              , "Vbs dependence of Fowler-Nordheim current")
`MPRnb( SVGSL     , 0.0        , ""                              , "Lgate dependence of SVGS ")
`MPRnb( SVGSLP    , 1.0        , ""                              , "Lgate dependence of SVGS ")
`MPRnb( SVGSW     , 0.0        , ""                              , "Wgate dependence of SVGS ")
`MPRnb( SVGSWP    , 1.0        , ""                              , "Wgate dependence of SVGS ")
`MPRnb( SLGL      , 0.0        , ""                              , "Substrate current dependence on Lgate ")
`MPRnb( SLGLP     , 1.0        , ""                              , "Substrate current dependence on Lgate ")
`MPRoo( NSTI      , 5.0E17     , "1/cm^3"            ,   0 , inf , "Parameter for STI  ")
`MPRnb( WSTI      , 0.0        , "m"                             , "Parameter for STI  ")
`MPRnb( WSTIL     , 0.0        , "-"                             , "Parameter for STI  ")
`MPRnb( WSTILP    , 1.0        , "-"                             , "Parameter for STI  ")
`MPRnb( WSTIW     , 0.0        , "-"                             , "Parameter for STI  ")
`MPRnb( WSTIWP    , 1.0        , "-"                             , "Parameter for STI  ")
`MPRnb( SCSTI1    , 0.0        , "-"                             , "Parameter for STI  ")
`MPRnb( SCSTI2    , 0.0        , "1/V"                           , "Parameter for STI  ")
`MPRnb( VTHSTI    , 0.0        , ""                              , "Parameter for STI ")
`MPRnb( VDSTI     , 0.0        , "-"                             , "Parameter for STI  ")
`MPRco( MUESTI1   , 0.0        , ""                  ,   0 , inf , "STI Stress mobility parameter ")
`MPRoo( MUESTI2   , 0.0        , ""                  ,  -1 , inf , "STI Stress mobility parameter ")
`MPRnb( MUESTI3   , 1.0        , ""                              , "STI Stress mobility parameter ")
`MPRco( NSUBPSTI1 , 0.0        , ""                  ,   0 , inf , "STI Stress pocket implant parameter ")
`MPRoo( NSUBPSTI2 , 0.0        , ""                  ,  -1 , inf , "STI Stress pocket implant parameter ")
`MPRnb( NSUBPSTI3 , 1.0        , ""                              , "STI Stress pocket implant parameter ")
`MPRoo( LPEXT     , 1.0E-50    , ""                  ,   0 , inf , "Pocket extension ")
`MPRnb( SCP21     , 0.0        , ""                              , "Short-channel-effect modification for small Vds")
`MPRnb( SCP22     , 0.0        , ""                              , "Short-channel-effect modification for small Vds")
`MPRnb( BS1       , 0.0        , ""                              , "Body-coefficient modification due to impurity profile")
`MPRnb( BS2       , 0.9        , ""                              , "Body-coefficient modification due to impurity profile")
`MPRco( TPOLY     , 200E-9     , "m"                 ,   0 , inf , "Height of poly gate on the source side ")
`MPRnb( CLM1      , 50E-3      , "-"                             , "Parameter for CLM  ")
`MPRnb( CLM2      , 2.0        , "1/m"                           , "Parameter for CLM  ")
`MPRnb( CLM3      , 1.0        , "-"                             , "Parameter for CLM  ")
`MPRnb( CLM5      , 1.0        , "-"                             , "Parameter for CLM  ")
`MPRnb( CLM6      , 0.0        , "-"                             , "Parameter for CLM  ")
`MPRnb( VOVERP    , 0.3        , "-"                             , "Parameter for overshoot  ")
`MPRnb( WFC       , 0.0        , "F/m"                           , "Parameter for narrow channel effect  ")
`MPRnb( NSUBCW    , 0.0        , "-"                             , "Parameter for narrow channel effect  ")
`MPRnb( NSUBCWP   , 1.0        , "-"                             , "Parameter for narrow channel effect  ")
`MPRnb( QME1      , 0.0        , "mV"                            , "Parameter for quantum effect  ")
`MPRnb( QME2      , 2.0        , "V"                             , "Parameter for quantum effect  ")
`MPRnb( QME3      , 0.0        , "m"                             , "Parameter for quantum effect  ")
`MPRnb( VOVERS    , 0.0        , "-"                             , "Parameter for overshoot  ")
`MPRnb( VOVERSP   , 0.0        , "-"                             , "Parameter for overshoot  ")
`MPRnb( GIDL1     , 2E0        , "-"                             , "Parameter for GIDL  ")
`MPRnb( GIDL2     , 3E7        , "-"                             , "Parameter for GIDL  ")
`MPRnb( GIDL3     , 0.9E0      , "-"                             , "Parameter for GIDL  ")
`MPRnb( GIDL4     , 0.0        , "-"                             , "Parameter for GIDL  ")
`MPRnb( GIDL5     , 0.2E0      , "-"                             , "Parameter for GIDL  ")
`MPRnb( GLEAK1    , 50E0       , "A*V^(-3/2)/C"                  , "Parameter for gate current  ")
`MPRnb( GLEAK2    , 10E6       , "V^(-1/2)/m "                   , "Parameter for gate current  ")
`MPRnb( GLEAK3    , 60E-3      , "-"                             , "Parameter for gate current  ")
`MPRnb( GLEAK4    , 4E0        , "1/m"                           , "Parameter for gate current  ")
`MPRoo( GLEAK5    , 7.5E3      , "V/m"               ,   0 , inf , "Parameter for gate current  ")
`MPRnb( GLEAK6    , 250E-3     , "V"                             , "Parameter for gate current  ")
`MPRnb( GLEAK7    , 1E-6       , "m^2"                           , "Parameter for gate current  ")
`MPRnb( GLPART1   , 0.5        , "-"                             , "Parameter for gate current  ")
`MPRnb( GLKSD1    , 1.0E-15    , "A*m/V^2"                       , "Parameter for gate current  ")
`MPRnb( GLKSD2    , 1E3        , "1/(V*m)"                       , "Parameter for gate current  ")
`MPRnb( GLKSD3    , -1E3       , "1/m"                           , "Parameter for gate current  ")
`MPRnb( GLKB1     , 5E-16      , "A/V^2"                         , "Parameter for gate current  ")
`MPRnb( GLKB2     , 1E0        , "m/V"                           , "Parameter for gate current  ")
`MPRnb( GLKB3     , 0E0        , "V"                             , "Parameter for gate current  ")
`MPRnb( EGIG      , 0E0        , "V"                             , "Parameter for gate current  ")
`MPRnb( IGTEMP2   , 0E0        , "V*K"                           , "Parameter for gate current  ")
`MPRnb( IGTEMP3   , 0E0        , "V*K^2"                         , "Parameter for gate current  ")
`MPRoo( VZADD0    , 10.0E-3    , "V"                 ,   0 , inf , "Vzadd at Vds=0   ")
`MPRoo( PZADD0    , 5.0E-3     , "V"                 ,   0 , inf , "Pzadd at Vds=0   ")
`MPRnb( NFTRP     , 10E9       , ""                              , "Ratio of trap density to attenuation coefficient")
`MPRnb( NFALP     , 1.0E-19    , ""                              , "Contribution of the mobility fluctuation")
`MPRnb( CIT       , 0E0        , ""                              , "capacitance caused by the interface trapped carriers")
`MPRoo( KAPPA     , 3.90E0     , "-"                 ,   0 , inf , "Dielectric constant for high-k stacked gate ")
`MPRco( CGDO      , 0          , "F/m"               ,   0 , inf , "Cgso ")
`MPRco( CGSO      , (COSYM ? CGDO : 0) , "F/m"       ,   0 , inf , "Cgso ")
`MPRoo( DLY1      , 100.0E-12  , "-"                 ,   0 , inf , "Parameter for transit time  ")
`MPRnb( DLY2      , 0.7E0      , "-"                             , "Parameter for transit time  ")
`MPRoo( DLY3      , 0.8E-6     , "s/F"               ,   0 , inf , "Parameter for transforming bulk charge  ")
`MPRco( DLYOV     , 0.8E-4     , "cm^2/A"            ,   0 , inf , "Parameter for transforming overlap charge  ")
`MPRoo( TNOM      , 27.0       , "K"             , -273.15 , inf , "Nominal temperature  ")
`MPRnb( OVSLP     , 2.1E-7     , ""                              , "Coefficient for overlap capacitance")
`MPRnb( OVMAG     , 0.6        , ""                              , "Coefficient for overlap capacitance")
`MPRcc( GBMIN     , 1.0E-12    , ""                  ,   0 , 1E4 , "Minimum Substrate resistance network")
`MPRnb( IBPC1     , 0.0        , "V/A"                           , "Parameter for impact-ionization induced bulk potential change ")
`MPRnb( IBPC1L    , 0.0        , "-"                             , "Parameter for impact-ionization induced bulk potential change ")
`MPRnb( IBPC1LP   , -1.0       , "-"                             , "Parameter for impact-ionization induced bulk potential change ")
`MPRnb( IBPC2     , 0.0        , "1/V"                           , "Parameter for impact-ionization induced bulk potential change ")
`MPRnb( MPHDFM    , -0.3       , ""                              , "NSUBCDFM dependence of phonon scattering for DFM ")
`MPRnb( PTL       , 0.0        , "V"                             , "Strength of punchthrough effect")
`MPRnb( PTP       , 3.5        , "-"                             , "Strength of punchthrough effect")
`MPRnb( PT2       , 0.0        , "1/V"                           , "Vds dependence of punchthrough effect")
`MPRnb( PTLP      , 1.0        , "-"                             , "Channel-length dependence of punchthrough effect")
`MPRnb( GDL       , 0.0        , "-"                             , "Strength of high-field effect")
`MPRnb( GDLP      , 0.0        , "-"                             , "Channel-length dependence of high-field effect")
`MPRnb( GDLD      , 0.0        , "m"                             , "Channel-length dependence of high-field effect")
`MPRnb( PT4       , 0.0        , ""                              , "Vbs dependence of punchthrough effect")
`MPRnb( PT4P      , 1.0        , ""                              , "Vbs dependence of punchthrough effect")
`MPRnb( RDVG12    , 100.0      , ""                              , "Vgs dependence of RD for CORSRD=1,3") //HV1
`MPRco( CTH0      , 1.0E-7     , "Ws/(Kcm)"          ,   0 , inf , "Thermal capacitance ")
`MPRco( XLDLD     , 1.0E-6     , "m"                 ,   0 , inf , "Lateral diffusion of Drain under the gate  ")
`MPRnb( XWDLD     , XWD        , ""                              , "Widening of drift width")
`MPRnb( RD20      , 0.0        , ""                              , "RD23 boundary for CORSRD=2,3")                        //HV1
`MPRnb( RD21      , 1.0        , ""                              , "Vds dependence of RD for CORSRD=2,3")           //HV1
`BPRnb( RD22      , 0.0        , ""                              , "Vbs dependence of RD for CORSRD=2,3")           //HV1
`MPRnb( RD22D     , 0.0        , ""                              , "Vbs dependence of RD for CORSRD=2,3 with large Vds")  //HV1
`MPRnb( RD25      , 0.0        , ""                              , "Vgs dependence of RD for CORSRD=2,3")           //HV1
`MPRnb( RDVDL     , 0.0        , ""                              , "Lgate dependence of RD for CORSRD=1,3")                 //HV1
`MPRnb( RDVDLP    , 1.0        , ""                              , "Lgate dependence of RD for CORSRD=1,3")                 //HV1
`MPRnb( RDVDS     , 0.0        , ""                              , "Small size dependence of RD for CORSRD=1,3")    //HV1
`MPRnb( RDVDSP    , 1.0        , ""                              , "Small size dependence of RD for CORSRD=1,3")    //HV1
`MPRnb( RD23L     , 0.0        , ""                              , "Lgate dependence of RD23 boundary for CORSRD=2,3")      //HV1
`MPRnb( RD23LP    , 1.0        , ""                              , "Lgate dependence of RD23 boundary for CORSRD=2,3")      //HV1
`MPRnb( RD23S     , 0.0        , ""                              , "Small size dependence of RD23 for CORSRD=2,3")          //HV1
`MPRnb( RD23SP    , 1.0        , ""                              , "Small size dependence of RD23 for CORSRD=2,3")          //HV1
`MPRnb( RDS       , 0.0        , ""                              , "Small size dependence of RD for CORSRD=1,3")    //HV1
`MPRnb( RDSP      , 1.0        , ""                              , "Small size dependence of RD for CORSRD=1,3")          //HV1     
`MPRnb( RDTEMP1   , 0.0        , "ohm*cm/K"                      , "Temperature-dependence of Rd ")
`MPRnb( RDTEMP2   , 0.0        , "ohm*cm/K^2"                    , "Temperature-dependence of Rd ")
`MPRnb( RDVDTEMP1 , 0.0        , "ohm*cm/V/K"                    , "Temperature-dependence of RDVD ")                     //HV1
`MPRnb( RDVDTEMP2 , 0.0        , "ohm*cm/V/K^2"                  , "Temperature-dependence of RDVD ")                     //HV1
`MPRnb( RTH0W     , 0.0        , "-"                             , "Width-dependence of RTH0 ")
`MPRnb( RTH0WP    , 1.0        , "-"                             , "Width-dependence of RTH0 ")
`MPRnb( RTH0L     , 0.0        , "-"                             , "Length dependence of RTH0 ")
`MPRnb( RTH0LP    , 1.0        , "-"                             , "Length dependence of RTH0 ")
`MPRnb( NINVD     , 0.0        , "1/V"                           , "Modification of Vdse dependence on Eeff  ")
`MPRnb( NINVDL    , 0.0        , "-"                             , "Coeff of modification of Vdse dependence on Eeff  ")
`MPRnb( NINVDLP   , 1.0        , "-"                             , "Coeff of modification of Vdse dependence on Eeff  ")
`MPRnb( NINVDW    , 0.0        , "-"                             , "Coeff of modification of Vdse dependence on Eeff  ")
`MPRnb( NINVDWP   , 1.0        , "-"                             , "Coeff of modification of Vdse dependence on Eeff  ")
`MPRnb( NINVDT1   , 0.0        , "-"                             , "Coeff of modification of Vdse dependence on Eeff  ")
`MPRnb( NINVDT2   , 0.0        , "-"                             , "Coeff of modification of Vdse dependence on Eeff  ")
`MPRnb( VBSMIN    , -10.5      , "V"                             , "Minimum back bias voltage to be treated in hsmhveval ")
`MPRnb( RTH0NF    , 0.0        , "-"                             , "Nf-dependence of RTH0 ")
`MPRnb( RTHTEMP1  , 0.0        , "m/W"                           , "Thermal resistance ")
`MPRnb( RTHTEMP2  , 0.0        , "m/W/K"                         , "Thermal resistance ")
`MPRnb( PRATTEMP1 , 0.0        , "1/K"                           , "Temperature dependence of thermal dissipation")
`MPRnb( PRATTEMP2 , 0.0        , "1/k^2"                         , "Temperature dependence of thermal dissipation")
`MPRnb( RDVSUB    , 1.0        , ""                              , "Model parameter for the substrate effect ")
`MPRnb( RDVDSUB   , 0.3        , ""                              , "Model parameter for the substrate effect ")
`MPRoo( DDRIFT    , 1.0E-6     , ""                  ,   0 , inf , "Model parameter for the substrate effect ")
`MPRnb( VBISUB    , 0.7        , ""                              , "Model parameter for the substrate effect ")
`MPRoo( NSUBSUB   , 1.0E15     , ""                  ,   0 , inf , "Model parameter for the substrate effect ")
`MPRoo( SHEMAXDLT , 0.1        , ""                  ,   0 , inf , "Delta for the SHEMAX smoothing")

// Model parameters for depletion MOSFET 
`MPRoo( NDEPM     , ((CODEP < 3) ? 1E17:4E16) ,"1/cm^3" , 0 ,inf , "Impurity concentration of the surface layer (resistor) (CODEP=1,2,3)")
//20170913 compatibility to hisim2
`MPRnb( NDEPML        , 0.0        , "-"                          , "Lgate dependence of impurity concentration of the surface N^- layer" )
`MPRnb( NDEPMLP       , 1.0        , "-"                          , "Lgate dependence of impurity concentration of the surface N^- layer" )
//
`MPRoo( TNDEP     , ((CODEP < 3) ? 200n:300n) ,"m"      , 0 ,inf , "Thickness of the surface layer (CODEP=1,2,3)")
`MPRoo( TNDEPMIN  , (TNDEP*1u) , "m"        , (TNDEP*1n) , TNDEP , "minumum thickness of the surface layer (CODEP=3)")
`MPRnb( TNDEPV    , 0          , "1/V"                           , "Vds dependence of TNDEP (CODEP=2,3)") 
`MPRnb( DEPMUE0   , ((CODEP < 3) ? 1e3:1E8) , "cm^2/V/s"         , "Coulomb scattering in the resistor part (CODEP=1,2,3)")
`MPRnb( DEPMUE0L      , 0.0        , "-"                          , "Lgate dependence of the Coulomb scattering in the resistor region" )
`MPRnb( DEPMUE0LP     , 1.0        , "-"                          , "Lgate dependence of the Coulomb scattering in the resistor region" )
`MPRnb( DEPMUE1   , ((CODEP < 3) ?   0:100) , "cm^2/V/s"         , "Coulomb scattering in the resistor part (CODEP=1,  3)")
`MPRnb( DEPMUE1L      , 0.0        , "-"                          , "Lgate dependence of the Phonon scattering in the resistor region" )
`MPRnb( DEPMUE1LP     , 1.0        , "-"                          , "Lgate dependence of the Phonon scattering in the resistor region" )
`MPRoo( DEPMUE2   , 1e3        , "cm^2/V/s"          ,   0 , inf , "Coulomb scattering of the resistor part (CODEP=2)")
`MPRnb( DEPMUEA1  , 0.0        , "-"                             , "Modification of mures (CODEP=2)")
`MPRnb( DEPMUEBACK0 , 100      , "cm^2/V/s"                      , "Coulomb scattering in the back part (CODEP=1)")
`MPRnb( DEPMUEBACK1 , 0        , "cm^2/V/s"                      , "Coulomb scattering in the back part (CODEP=1)")
`MPRnb( DEPMUEBACK0L  , 0.0        , "-"                          , "Lgate dependence of the Coulomb scattering in the back region" ) //CODEP=1
`MPRnb( DEPMUEBACK0LP , 1.0        , "-"                          , "Lgate dependence of the Coulomb scattering in the back region" ) //CODEP=1
`MPRnb( DEPMUEBACK1L  , 0.0        , "-"                          , "Lgate dependence of the Phonon scattering in the back region" ) //CODEP=1
`MPRnb( DEPMUEBACK1LP , 1.0        , "-"                          , "Lgate dependence of the Phonon scattering in the back region" ) //CODEP=1
`MPRnb( DEPLEAK   , ((CODEP < 3) ? 0.5:0.1) , "V"                , "leakage current coefficient (CODEP=1,2,3)")
`MPRnb( DEPLEAKL      , 0.0        , "-"                          , "Lgate dependence of leakage current coefficient" )
`MPRnb( DEPLEAKLP     , 1.0        , "-"                          , "Lgate dependence of leakage current coefficient" )
`MPRco( DEPJLEAK  , 0          , "A/m2"              ,   0 , inf , "current density of the leak part (CODEP=3)")
`MPRnb( DEPWLP    , 0          , "A/m2"                          , "Coeff of the leak part (CODEP=3)")
`MPRnb( DEPWLPT   , 0          , "-"                             , "Temperature dependence of DEPWLP (CODEP=3)")
`MPRnb( DEPETA    , 0.0        , "-"                             , "Vds dependence of the threshold voltage shift (CODEP=1)")
`MPRoo( DEPVMAX   , ((CODEP < 3) ? 3e7:1e7) , "cm/s" ,   0 , inf , "Saturation velocity in the resistor part (CODEP=1,2,3)")
`MPRnb( DEPVMAXL      , 0.0        , "-"                          , "Lgate dependence of the saturation velocity in the resistor region" )
`MPRnb( DEPVMAXLP     , 1.0        , "-"                          , "Lgate dependence of the saturation velocity in the resistor region" )
`MPRnb( DEPVDSEF1 ,  2.0       , "V"            , "Effective drain potential coefficient-1 in the resistor part (CODEP=1)")
`MPRnb( DEPVDSEF2 ,  0.5       , "-"            , "Effective drain potential coefficient-2 in the resistor part (CODEP=1)")
`MPRnb( DEPVDSEF1L    , 0.0        , "-"                          , "Lgate dependence of the effective drain potential coefficient" ) //CODEP=1
`MPRnb( DEPVDSEF1LP   , 1.0        , "-"                          , "Lgate dependence of the effective drain potential coefficient" ) //CODEP=1
`MPRnb( DEPVDSEF2L    , 0.0        , "-"                          , "Lgate dependence of the effective drain potential coefficient" ) //CODEP=1
`MPRnb( DEPVDSEF2LP   , 1.0        , "-"                          , "Lgate dependence of the effective drain potential coefficient" ) //CODEP=1
`MPRnb( DEPMUEPH0 , ((CODEP < 3) ? 0.3:  0), "-"                 , "Phonon scattering in the resistor part (CODEP=1,3)")
`MPRoo( DEPMUEPH1 , ((CODEP < 3) ? 5e3:400), "cm^2/V/s", 0 , inf , "Phonon scattering in the resistor part (CODEP=1,3)")
`MPRco( DEPBB     , ((CODEP < 3) ? 1.0:2.0), "-"    , 0.01 , inf , "High-field-mobility degradation in the resistor part (CODEP=1,2,3)")
`MPRnb( DEPVTMP   , 0.0        , "-"                             , "Temperature dependence of DEPVMAX (CODEP=1,2,3)")
`MPRnb( DEPMUETMP , 1.5        , "-"                             , "Temperature dependence of the phonon scattering in the resistor part (CODEP=1,2,3)")
`MPRnb( DEPMUE0TMP , 0.0       , "-"                             , "Temperature dependence of DEPMUE0 (CODEP=1,3)")  //CODEP = 1, 3 Coulomb scat mobility
`MPRnb( DEPMUE2TMP , 0.0       , "-"                             , "temperature dependence of DEPMUE2 (CODEP=2)")    //CODEP = 2
`MPRcc( DEPDDLT   , ((CODEP < 3) ? 3.0:1.0) , "-"    , 0.1 ,  20 , "Smoothing coefficient for Vds,res (CODEP=2,3)") // 20171127 lower limit
`MPRnb( DEPNINVDC ,  100.0     , "1/V"                           , "Modification of Vdse dependence on Eeff (for Mu_res) (CODEP=3)") 
`MPRnb( DEPNINVDH ,  10.0      , "1/V"                           , "Modification of Vdse dependence on Eeff (for Mu_res) (CODEP=3)") 
`MPRnb( DEPNINVDL  , 0.0       , "-"                             , "Coeff of modification of Vdse dependence on Eeff; Mu_res (CODEP=3)")
`MPRnb( DEPNINVDLP , 1.0       , "-"                             , "Coeff of modification of Vdse dependence on Eeff; Mu_res (CODEP=3)")
`MPRnb( DEPNINVDW  , 0.0       , "-"                             , "Coeff of modification of Vdse dependence on Eeff; Mu_res (CODEP=3)")
`MPRnb( DEPNINVDWP , 1.0       , "-"                             , "Coeff of modification of Vdse dependence on Eeff; Mu_res (CODEP=3)")
`MPRnb( DEPNINVDT1 , 0.0       , "-"                             , "Coeff of modification of Vdse dependence on Eeff; Mu_res (CODEP=3)")
`MPRnb( DEPNINVDT2 , 0.0       , "-"                             , "Coeff of modification of Vdse dependence on Eeff; Mu_res (CODEP=3)")
`MPRnb( DEPVFBC   , VFBC       , "V"                             , "Flat-band voltage of the resistor part (CODEP=2)")          //CODEP = 2
`MPRnb( DEPDVFBC  , 0.1        , "V"                             , "delta of Flat-band voltage of the resistor part (CODEP=3)") //CODEP = 3
`MPRco( DEPSUBSL  , 2.0        , "-"                , 1e-8 , inf , "Factor of the sub-threshold sloop (CODEP=2,3)")             //CODEP = 2, 3
`MPRco( DEPSUBSL0 , DEPSUBSL   , "-"                , 1e-8 , inf , "Factor of the sub-threshold sloop (CODEP=2,3)")             //CODEP = 3
`MPRnb( DEPVSATR  , 0.0        , "-"                             , "Vbs dependence of subVds,sat of the resistor part (CODEP=2)") //CODEP = 2
`MPRnb( DEPVSATA  , 0.0        , "V", " Vdssat shift depmos3") // 20171004
`MPRcc( DEPRBR    , 1          , "-"                , 0    ,   1 , "parameter for resistance effect along substrate (for minority carrier) (CODEP=3)") //CODEP = 3
`MPRcc( DEPVLEAK    , 1          , "-"           , -0.5    ,   1 , "Vbs dependence of resistance leak current(CODEP=3)") //CODEP = 3
`MPRnb( DEPCAR    , 0.0        , "m/V"                           , "coefficient for Ndrift,res (CODEP=3)")            //CODEP = 3
`MPRnb( DEPRDRDL1 , 0.0        , "m"                             , "RDRDL1 for surface layer (CODEP=3)")              //CODEP = 3
`MPRnb( DEPRDRDL2 , 0.0        , "m"                             , "Pinch-off length in resistor layer (CODEP=3)")    //CODEP = 3
`MPRnb( DEPPS     , 0.01       , "V"                             , "Ps_delta for smoothing charge (CODEP=3)")         //CODEP = 3
`MPRcc( DEPQF     , 0.01       , "V"                , 1e-8 ,   8 , "smoothing param for Vdseff (CODEP=3)")            //CODEP = 3
`MPRcc( DEPQFRES  , 0.05       , "V"                , 1e-8 ,   8 , "smoothing param for Vdsres (CODEP=3)")            //CODEP = 3
`MPRcc( DEPFDPD   , 0.2        , "V"                , 1e-8 ,   4 , "smoothing param for FD/PD transition (CODEP=3)")  //CODEP = 3
`MPRco( DEPVGPSL  , ((CODEP < 3) ? 0.0:0.2) , "V"  ,     0 , inf , "Smoothing coefficient for gm (CODEP=2,3)")        //CODEP = 2, 3
`MPRcc( DEPPB0    ,0.5         , "V"                ,0     , 0.5 , "Flooring for Eeff for Mu_res depmos3") // HiSIM2 3.0.0 compatible

// Model parameters for drift resistance 
`MPRoo( RDRMUE    , 1.0E3      , "cm^2/V/s"          ,   0 , inf , "Mobility in drift region")
`MPRnb( RDRMUEBS1  , 0.0        , "-"                             , "Vbs coefficient to Rdrmue")
`MPRnb( RDRMUEBS2  , 0.0        , "-"                             , "2nd order Vbs coefficient to Rdrmue")
`MPRoo( RDRVMAX   , 3.0E7      , "cm/s"              ,   0 , inf , "Saturation velocity in drift region")
`MPRoo( RDRMUES   , RDRMUE     , "cm^2/V/s"          ,   0 , inf , "Mobility in drift region (source)") 
`MPRoo( RDRVMAXS  , RDRVMAX    , "cm/s"              ,   0 , inf , "Saturation velocity in drift region (source)") 
`MPRnb( RDRMUETMP , 0.0        , ""                              , "Temperature dependence of resistance")
`MPRnb( RDRMUETMPS, RDRMUETMP  , ""                              , "Temperature dependence of resistance (source)")
`MPRnb( RDRVTMP   , 0.0        , ""                              , "Temperature dependence of resistance")
`MPRnb( RDRVTMPS  , RDRVTMP    , ""                              , "Temperature dependence of resistance (source)")
`MPRoo( RDRDJUNC  , 1.0E-6     , "m"                 ,   0 , inf , "Junction depth at channel/drift region")
`MPRnb( RDRCX     , 0.0        , ""                              , "Exude of current flow from Xov")
`MPRnb( RDRCAR    , 1.0E-8     , ""                              , "High field injection in drift region")
`MPRoo( RDRDL1    , 0.0        , "m"  , -(LDRIFT1+LDRIFT2) , inf , "Effective Ldrift of current in drift region")
`MPRco( RDRDL2    , 0.0        , "m"  ,    0 , (LDRIFT1+LDRIFT2) , "Pinch-off length in drift region")
`MPRnb( RDRVMAXW  , 0.0        , ""                              , "Wgate dependence of the saturation velocity in drift region")
`MPRnb( RDRVMAXWP , 1.0        , ""                              , "Wgate dependence of the saturation velocity in drift region")
`MPRnb( RDRVMAXL  , 0.0        , ""                              , "Lgate dependence of the saturation velocity in drift region")
`MPRnb( RDRVMAXLP , 1.0        , ""                              , "Lgate dependence of the saturation velocity in drift region")
`MPRnb( RDRMUEL   , 0.0        , ""                              , "Lgate dependence of the mobility in drift region ")
`MPRnb( RDRMUELP  , 1.0        , ""                              , "Lgate dependence of the mobility in drift region ")
`MPRco( RDRQOVER  , 1E5        , "-"                 ,   0 , inf , "Inclusion of the overlap charge into Rdrift")
`MPRco( QOVADD    , 0.0        , "-"                 ,   0 , inf , "Parameter for additional Qover Charge  ")
`MPRco( QOVJUNC   , 0.0        , "-"                 ,  -1 ,  50 , "Parameter for Qover capacitance with p-n junction ")
`MPRco( SHEMAX    , 500        , "K"                 ,   0 , inf , "Maximum rise temperature for SHE  ")
`MPRnb( VGSMIN   , -100 * TYPE , "V"                             , "Minimum/maximum expected Vgs(NMOS/PMOS)  ")
`MPRco( GDSLEAK   , 0.0        , "A/V"               ,   0 , 1E4 , "Channel leakage conductance  ")
`MPRco( RDRBB     , 1          , "-"                 , 0.1 , inf , "Degradation of the mobility in drift region ")
`MPRco( RDRBBS    , RDRBB      , "-"                 , 0.1 , inf , "degradation of the mobility in drift region (source)")
`MPRnb( RDRBBTMP  , 0          , "1/K"                           , "Temperature coefficient of RDRBB ")
`MPRnb( RDRBBTMPS , RDRBBTMP   , "1/K"                           , "temperature coefficient of RDRBBS (source) ") 
`MPRco( NDRILIM ,  1         , "-"                 ,   0 , inf , "ceiling for Ndrift  ")
`MPRco( NDRIDLT ,  1         , "-"                 ,   0 , inf , "guardband for smoothing of Ndrift ")
`MPRco( NDRIPW ,   1         , "-"                 ,   0 , inf , "power exponent for smoothing of Ndrift")
`MPRcc( GMIN      , 0          , ""                  ,   0 , 1E4 , "Minimum conductance")
`MPRoo( RMIN      , 1E-4       , ""                  ,   0 , inf , "Minimum resistance for RS / RD")
// Model parameters for break-down model
`MPRnb( HBDA      , 0.0        , ""                              , "Coeff. for Hard Breakdown Voltage ")
`MPRnb( HBDB      , 0.0        , ""                              , "Coeff. for Hard Breakdown Voltage ")
`MPRnb( HBDC      , 100        , ""                              , "Coeff. for Hard Breakdown Voltage ")
`MPRnb( HBDCTMP   , 0.0        , ""                              , "Temperature Coeff. for HBDC")
`MPRnb( HBDF      , 1.0        , ""                              , "Coeff. for Hard Breakdown Voltage ")

// Model parameters for newest punchthrough model
`MPIcc( COPT      , 0          , "-"                  ,   0 ,   1 , "flag punchthrough ")   
`MPIsw( COPSPT    , 0          , "-"                             , "flag Ps0 method for new punchthrough ")
`MPRco( XJPT      , 3E-8       , "m"                 ,   0 ,   1 , "Junction depth; new Punchthrough ")
`MPRoo( NJUNC     , 1e20       , "cm-3"              ,   0 , inf , "Junction doping conc; new Punchthrough  ")
`MPRco( MUPT      , 0.0        , "m^2/Vs"            ,   0 , inf , "mobility for new Punchthrough ")
`MPRnb( VFBPT     , 0.0        , "V"                             , "dVfb for new punchthrough ")
`MPRnb( PSLIMPT   , 0.0        , "V"                             , "Ps0 limit for new punchthrough ")
`ifdef _AGING_
/////////////// Parameter for Aging model ///////////////
`BPRco( DEGTIME0   , 0.0        , "s"                ,   0 , inf , "aging simulation time ")
`BPRco( DEGTIME    , 0          , "s"                ,   0 , inf , "degradation time ")
`BPRnb( TRAPN      , 1.0        , "-"                            , "trap charge contribution to Eeff  ")
`BPRnb( TRAPP      , 1.0        , "-"                            , "BTI trap charge contribution to Eeff")
/////////////// Parameter for HC model //////////////////
`BPRnb( TRAPGC0    , 1.0E30    , "cm^-3eV^-1"                    , "coefficient of deep trap ")
`BPRoo( TRAPES0    , 0.01      , "eV"                            ,   0 , inf  , "coefficient of deep trap ")
`BPRnb( TRAPGC1    , 1.0E15    , "cm^-3eV^-1"                    , "coefficient of deep trap ")
`BPRoo( TRAPES1    , 0.2       , "eV"               ,   0 , inf  , "coefficient of deep trap ")
`BPRnb( TRAPGC2    , 5.0e13    , "cm^-3eV^-1"                    , "coefficient of shallow trap ")
`BPRoo( TRAPES2    , 0.03      , "eV"               ,   0 , inf  , "coefficient of shallow trap ")
`BPRoo( TRAPLX     , 1         , "V"                ,   0 , inf  , "Vds dependence of deep trap position")
`BPRnb( TRAPGCLIM  , 1E30      , "cm^-3eV^-1"                    , "coefficient of deep trap ")
`BPRnb( TRAPESLIM  , 5         , "eV"                            , "coefficient of deep trap ")
`BPRnb( TRAPTAUCAP  , 1e-6     , "s"                             , "Time constant of trap capture")
`BPRnb( TRAPGC1MAX  , 5E19     , "cm^-3eV^-1"                    , "GC1 aging coefficient ")
`BPRnb( TRAPES1MAX  , 1        , "eV"                            , "ES1 aging coefficient ")
`BPRnb( TRAPGCTIME1 , 30       , "s"                             , "starting stress duration of GC1 aging ")
`BPRnb( TRAPGCTIME2 , 1E8      , "s"                             , "saturating stress duration of GC1 aging ")
`BPRnb( TRAPESTIME1 , 100      , "s"                             , "starting stress duration of ES1 aging ")
`BPRnb( TRAPESTIME2 , 1E8      , "s"                             , "saturating stress duration of ES1 aging ")
`MPRnb( TRAPDTIME1 , 1000      , "s"                             , "starting stress duration of D1 aging")
`MPRnb( TRAPDTIME2 , 2E10      , "s"                             , "saturating stress duration of D1 aging")
`MPRnb( TRAPD1MAX  , 30.0      , "V"                             , "D1 aging coefficient")
`BPRoo( TRAPDVDDP  , 0.0       , "V"                , -inf , inf , "Ntrap in Rdrift")
`BPRnb( TRAPDLX    , 1.0       , "V"                             , "Vds dependence of trap position")
`BPRnb( TRAPTEMP1  , 0.0       , ""                              , "temperature dependence of trap")
`BPRnb( TRAPTEMP2  , 0.0       , ""                              , "temperature dependence of trap")
/////////////// Parameter for BTI model ///////////////
`BPRnb( TRAPDVTH   , 0.0        , "V"                            , "Vfb shift dut to interface trap density")
`MPRnb( TRAPA      , 0.0        , "V"                            , "coefficient of existing interface trap density")
`MPRnb( TRAPB      , 4E-9       , "m/V"                          , "coefficient of existing interface trap density")
`BPRnb( TRAPBTI    , 1e7        , "s"                            , "coefficient for duration time dependence")
`endif

`ifdef COEXTDIO 
       /* for Junction Diode Model */
`else 
`MPRnb( JS0       , 0.5E-6     , "A/m^2"                         , "Saturation current density  ")
`MPRnb( JS0SW     , 0.0        , "A/m"                           , "Side wall saturation current density  ")
`MPRnb( JS0SWG    , 0.0        , "A/m"                           , "Gate side wall saturation current density  ")
`MPRoo( NJ        , 1.0        , "-"                 ,   0 , inf , "Emission coefficient  ")
`MPRoo( NJSW      , 1.0        , "-"                 ,   0 , inf , "Sidewall emission coefficient ")
`MPRoo( NJSWG     , 1.0        , "-"                 ,   0 , inf , "Gate sidewall emission coefficient ")
`MPRnb( XTI       , 2.0        , "-"                             , "Junction current temperature exponent coefficient  ")
`MPRnb( CJ        , 5.0E-04    , "F/m^2"                         , "Bottom junction capacitance per unit area at zero bias  ")
`MPRnb( CJSW      , 5.0E-10    , "F/m"                           , "Source/drain sidewall junction capacitance grading coefficient per unit length at zero bias  ")
`MPRnb( CJSWG     , 5.0E-10    , "F/m"                           , "Source/drain gate sidewall junction capacitance per unit length at zero bias  ")
`MPRoo( MJ        , 0.5E0      , "-"                , -inf ,   1 , "Bottom junction capacitance grading coefficient ")
`MPRoo( MJSW      , 0.33E0     , "-"                , -inf ,   1 , "Source/drain sidewall junction capacitance grading coefficient ")
`MPRoo( MJSWG     , 0.33E0     , "-"                , -inf ,   1 , "Source/drain gate sidewall junction capacitance grading coefficient ")
`MPRoo( PB        , 1.0E0      , "V"                 ,   0 , inf , "Bottom junction build-in potential   ")
`MPRoo( PBSW      , 1.0E0      , "V"                 ,   0 , inf , "Source/drain sidewall junction build-in potential  ")
`MPRoo( PBSWG     , 1.0E0      , "V"                 ,   0 , inf , "Source/drain gate sidewall junction build-in potential  ")
`MPRnb( XTI2      , 0.0E0      , "-"                             , "Temperature coefficient  ")
`MPRnb( CISB      , 0.0E0      , "-"                             , "Reverse bias saturation current  ")
`MPRnb( CVB       , 0.0E0      , "-"                             , "Bias dependence coefficient of cisb  ")
`MPRnb( CTEMP     , 0.0E0      , "-"                             , "Temperature coefficient  ")
`MPRnb( CISBK     , 0.0E0      , "A"                             , "Reverse bias saturation current  ")
`MPRnb( CVBK      , CVB        , "-"                             , "Inactivated by CVB ")
`MPRnb( DIVX      , 0.0        , "1/V"                           , "Reverse current coefficient")
`MPRnb( VDIFFJ    , 0.6E-3     , "V"                             , "Threshold voltage for S/D junction diode  ")
`MPRnb( TCJBD     , 0.0        , "1/K"                           , "Temperature dependence of cjbd ")
`MPRnb( TCJBS     , 0.0        , "1/K"                           , "Temperature dependence of cjbs ")
`MPRnb( TCJBDSW   , 0.0        , "1/K"                           , "Temperature dependence of cjbdsw ")
`MPRnb( TCJBSSW   , 0.0        , "1/K"                           , "Temperature dependence of cjbssw ")
`MPRnb( TCJBDSWG  , 0.0        , "1/K"                           , "Temperature dependence of cjbdswg ")
`MPRnb( TCJBSSWG  , 0.0        , "1/K"                           , "Temperature dependence of cjbsswg ")
`MPRnb( TPBBD     , 0.0        , "V/K"                           , "Temperature dependence of pzbd ")
`MPRnb( TPBBS     , 0.0        , "V/K"                           , "Temperature dependence of pzbs ")
`MPRnb( TPBBDSW   , 0.0        , "V/K"                           , "Temperature dependence of pzbdsw ")
`MPRnb( TPBBSSW   , 0.0        , "V/K"                           , "Temperature dependence of pzbssw ")
`MPRnb( TPBBDSWG  , 0.0        , "V/K"                           , "Temperature dependence of pzbdswg ")
`MPRnb( TPBBSSWG  , 0.0        , "V/K"                           , "Temperature dependence of pzbsswg ")
`MPRnb( JS0D      , JS0        , "A/m^2"                         , "Saturation current density for drain junction  ")
`MPRnb( JS0SWD    , JS0SW      , "A/m"                           , "Side wall saturation current density for drain junction  ")
`MPRnb( JS0SWGD   , JS0SWG     , "A/m"                           , "Gate side wall saturation current density for drain junction  ")
`MPRoo( NJD       , NJ         , "-"                 ,   0 , inf , "Emission coefficient for drain junction  ")
`MPRoo( NJSWD     , NJSW       , "-"                 ,   0 , inf , "Sidewall emission coefficient for drain junction  ")
`MPRoo( NJSWGD    , NJSWG      , "-"                 ,   0 , inf , "Gate sidewall emission coefficient for drain junction  ")
`MPRnb( XTID      , XTI        , "-"                             , "Junction current temperature exponent coefficient for drain junction  ")
`MPRnb( CJD       , CJ         , "F/m^2"                         , "Bottom junction capacitance per unit area at zero bias for drain junction  ")
`MPRnb( CJSWD     , CJSW       , "F/m"                           , "Sidewall junction capacitance grading coefficient per unit length at zero bias for drain junction  ")
`MPRnb( CJSWGD    , CJSWG      , "F/m"                           , "Gate sidewall junction capacitance per unit length at zero bias for drain junction  ")
`MPRoo( MJD       , MJ         , "-"                , -inf ,   1 , "Bottom junction capacitance grading coefficient for drain junction  ")
`MPRoo( MJSWD     , MJSW       , "-"                , -inf ,   1 , "Sidewall junction capacitance grading coefficient for drain junction  ")
`MPRoo( MJSWGD    , MJSWG      , "-"                , -inf ,   1 , "Gate sidewall junction capacitance grading coefficient for drain junction  ")
`MPRoo( PBD       , PB         , "V"                ,   0  , inf , "Bottom junction build-in potential  for drain junction   ")
`MPRoo( PBSWD     , PBSW       , "V"                ,   0  , inf , "Sidewall junction build-in potential for drain junction  ")
`MPRoo( PBSWGD    , PBSWG      , "V"                ,   0  , inf , "Gate sidewall junction build-in potential for drain junction  ")
`MPRnb( XTI2D     , XTI2       , "-"                             , "Temperature coefficient for drain junction  ")
`MPRnb( CISBD     , CISB       , "-"                             , "Reverse bias saturation current for drain junction  ")
`MPRnb( CVBD      , CVB        , "-"                             , "Bias dependence coefficient of cisb for drain junction   ")
`MPRnb( CTEMPD    , CTEMP      , "-"                             , "Temperature coefficient for drain junction  ")
`MPRnb( CISBKD    , CISBK      , "A"                             , "Reverse bias saturation current for drain junction  ")
`MPRnb( DIVXD     , DIVX       , "1/V"                           , "Reverse coefficient coefficient for drain junction  ")
`MPRnb( VDIFFJD   , VDIFFJ     , "V"                             , "Threshold voltage for junction diode for drain junction  ")

`MPRnb( JS0S      , JS0D       , "A/m^2"                         , "Saturation current density for source junction  ")
`MPRnb( JS0SWS    , JS0SWD     , "A/m"                           , "Side wall saturation current density for source junction  ")
`MPRnb( JS0SWGS   , JS0SWGD    , "A/m"                           , "Gate side wall saturation current density for source junction  ")
`MPRoo( NJS       , NJD        , "-"                 ,   0 , inf , "Emission coefficient for source junction  ")
`MPRoo( NJSWS     , NJSWD      , "-"                  ,   0 , inf , "Sidewall emission coefficient for source junction  ")
`MPRoo( NJSWGS    , NJSWGD     , "-"                  ,   0 , inf , "Gate sidewall emission coefficient for source junction  ")
`MPRnb( XTIS      , XTID       , "-"                             , "Junction current temperature exponent coefficient for source junction  ")
`MPRnb( CJS       , CJD        , "F/m^2"                         , "Bottom junction capacitance per unit area at zero bias for source junction  ")
`MPRnb( CJSWS     , CJSWD      , "F/m"                           , "Sidewall junction capacitance grading coefficient per unit length at zero bias for source junction  ")
`MPRnb( CJSWGS    , CJSWGD     , "F/m"                           , "Gate sidewall junction capacitance per unit length at zero bias for source junction  ")
`MPRoo( MJS       , MJD        , "-"                 , -inf ,   1 , "Bottom junction capacitance grading coefficient for source junction  ")
`MPRoo( MJSWS     , MJSWD      , "-"                 , -inf ,   1 , "Sidewall junction capacitance grading coefficient for source junction  ")
`MPRoo( MJSWGS    , MJSWGD     , "-"                 , -inf ,   1 , "Gate sidewall junction capacitance grading coefficient for source junction  ")
`MPRoo( PBS       , PBD        , "V"                 ,   0 , inf , "Bottom junction build-in potential  for source junction   ")
`MPRoo( PBSWS     , PBSWD      , "V"                 ,   0 , inf , "Sidewall junction build-in potential for source junction  ")
`MPRoo( PBSWGS    , PBSWGD     , "V"                 ,   0 , inf , "Gate sidewall junction build-in potential for source junction  ")
`MPRnb( XTI2S     , XTI2D      , "-"                             , "Temperature coefficient for source junction  ")
`MPRnb( CISBS     , CISBD      , "-"                             , "Reverse bias saturation current for source junction  ")
`MPRnb( CVBS      , CVBD       , "-"                             , "Bias dependence coefficient of cisb for source junction   ")
`MPRnb( CTEMPS    , CTEMPD     , "-"                             , "Temperature coefficient for source junction  ")
`MPRnb( CISBKS    , CISBKD     , "A"                             , "Reverse bias saturation current for source junction  ")
`MPRnb( DIVXS     , DIVXD      , "1/V"                           , "Reverse coefficient coefficient for source junction  ")
`MPRnb( VDIFFJS   , VDIFFJD    , "V"                             , "Threshold voltage for junction diode for source junction  ")


//imported from DIODE_CMC_parlist.include:
    //diode reverse recovery model
`MPRnb( CORECOVERY , 0.0       , "-"                             , "Flag for recovery equations; 0=original, 1=Hiroshima")    //`from(0.0    , 1.0 )
`MPRnb( NDIBOT    , 1e16       , "cm^-3"                         , "Doping concentration of drift region")                    //`from(1      , 1e23 )
`MPRnb( INJ1      , 1.0        , "-"                             , "For carrier density")                                     //`from(0      , 3 )
`MPRnb( INJ2      , 10.0       , "-"                             , "For carrier density in high-injection condition")         //`from(0      , 20 )
`MPRnb( NQS       , 5e-9       , "sec"                           , "Carrier delay time")                                      //`from(0      , 1e-3 )
`MPRnb( TAU       , 2e-7       , "sec"                           , "Carrier lifetime")                                        //`from(0      , 1e-3 )
`MPRnb( WI        , 5e-6       , "m"                             , "Length of drift region")                                  //`from(0      , 1.0 )
`MPRnb( DEPNQS    , 0.0        , "sec"                           , "Depletion delay time")                                    //`from(0      , 1e-3 )
`MPRnb( TAUT      , 0.0        , "-"                             , "Temp. co of carrier lifetime")                            //`from(0.0    , 1.0e2 )
`MPRnb( INJT      , 0.0        , "-"                             , "Temp. co of carrier density in high-injection condition") //`from(0      , 20 )
    //End of diode reverse recovery model


`endif /* End of COEXTDIO */

//*** Binning Parameters ***
`MPRnb( LMIN      , 0.0        , "m"                             , "Minimum length for the model ")
`MPRnb( LMAX      , 1.0        , "m"                             , "Maximum length for the model ")
`MPRnb( WMIN      , 0.0        , "m"                             , "Minimum width for the model ")
`MPRnb( WMAX      , 1.0        , "m"                             , "Maximum width for the model ")
`MPRnb( LBINN     , 1.0        , "-"                             , "L modulation coefficient for binning ")
`MPRnb( WBINN     , 1.0        , "-"                             , "W modulation coefficient for binning ")
`MPRnb( LVMAX     , 0.0        , "-"                             , "Length dependence of vmax ")
`MPRnb( LBGTMP1   , 0.0        , "-"                             , "Length dependence of bgtmp1 ")
`MPRnb( LBGTMP2   , 0.0        , "-"                             , "Length dependence of bgtmp2 ")
`MPRnb( LEG0      , 0.0        , "-"                             , "Length dependence of eg0 ")
`MPRnb( LVFBOVER  , 0.0        , "-"                             , "Length dependence of vfbover ")
`MPRnb( LNOVER    , 0.0        , "-"                             , "Length dependence of nover ")
`MPRnb( LNOVERS   , 0.0        , "-"                             , "Length dependence of nover on source side ")
`MPRnb( LWL2      , 0.0        , "-"                             , "Length dependence of wl2 ")
`MPRnb( LVFBC     , 0.0        , "-"                             , "Length dependence of vfbc ")
`MPRnb( LNSUBC    , 0.0        , "-"                             , "Length dependence of nsubc ")
`MPRnb( LNSUBP    , 0.0        , "-"                             , "Length dependence of nsubp ")
`MPRnb( LSCP1     , 0.0        , "-"                             , "Length dependence of scp1 ")
`MPRnb( LSCP2     , 0.0        , "-"                             , "Length dependence of scp2 ")
`MPRnb( LSCP3     , 0.0        , "-"                             , "Length dependence of scp3 ")
`MPRnb( LSC1      , 0.0        , "-"                             , "Length dependence of sc1 ")
`MPRnb( LSC2      , 0.0        , "-"                             , "Length dependence of sc2 ")
`MPRnb( LSC3      , 0.0        , "-"                             , "Length dependence of sc3 ")
`MPRnb( LPGD1     , 0.0        , "-"                             , "Length dependence of pgd1 ")
`MPRnb( LNDEP     , 0.0        , "-"                             , "Length dependence of ndep ")
`MPRnb( LNINV     , 0.0        , "-"                             , "Length dependence of ninv ")
`MPRnb( LMUECB0   , 0.0        , "-"                             , "Length dependence of muecb0 ")
`MPRnb( LMUECB1   , 0.0        , "-"                             , "Length dependence of muecb1 ")
`MPRnb( LMUEPH1   , 0.0        , "-"                             , "Length dependence of mueph1 ")
`MPRnb( LVTMP     , 0.0        , "-"                             , "Length dependence of vtmp ")
`MPRnb( LWVTH0    , 0.0        , "-"                             , "Length dependence of wvth0 ")
`MPRnb( LMUESR1   , 0.0        , "-"                             , "Length dependence of muesr1 ")
`MPRnb( LMUETMP   , 0.0        , "-"                             , "Length dependence of muetmp ")

`MPRnb( LSUB1     , 0.0        , "-"                             , "Length dependence of sub1 ")
`MPRnb( LSUB2     , 0.0        , "-"                             , "Length dependence of sub2 ")
`MPRnb( LSVDS     , 0.0        , "-"                             , "Length dependence of svds ")
`MPRnb( LSVBS     , 0.0        , "-"                             , "Length dependence of svbs ")
`MPRnb( LSVGS     , 0.0        , "-"                             , "Length dependence of svgs ")
`MPRnb( LSUB1SNP  , LSUB1      , "-"                             , "Length dependence of sub1 ")
`MPRnb( LSUB2SNP  , LSUB2      , "-"                             , "Length dependence of sub2 ")
`MPRnb( LSVDSSNP  , LSVDS      , "-"                             , "Length dependence of svds ")
//`MPRnb( LSVGSSNP  , LSVGS      , "-"                             , "Length dependence of svgssnp ")
//`MPRnb( LSVBSSNP  , LSVBS      , "-"                             , "Length dependence of svbssnp ")

`MPRnb( LFN1      , 0.0        , "-"                             , "Length dependence of fn1 ")
`MPRnb( LFN2      , 0.0        , "-"                             , "Length dependence of fn2 ")
`MPRnb( LFN3      , 0.0        , "-"                             , "Length dependence of fn3 ")
`MPRnb( LFVBS     , 0.0        , "-"                             , "Length dependence of fvbs ")
`MPRnb( LNSTI     , 0.0        , "-"                             , "Length dependence of nsti ")
`MPRnb( LWSTI     , 0.0        , "-"                             , "Length dependence of wsti ")
`MPRnb( LSCSTI1   , 0.0        , "-"                             , "Length dependence of scsti1 ")
`MPRnb( LSCSTI2   , 0.0        , "-"                             , "Length dependence of scsti2 ")
`MPRnb( LVTHSTI   , 0.0        , "-"                             , "Length dependence of vthsti ")
`MPRnb( LMUESTI1  , 0.0        , "-"                             , "Length dependence of muesti1 ")
`MPRnb( LMUESTI2  , 0.0        , "-"                             , "Length dependence of muesti2 ")
`MPRnb( LMUESTI3  , 0.0        , "-"                             , "Length dependence of muesti3 ")
`MPRnb( LNSUBPSTI1, 0.0        , "-"                             , "Length dependence of nsubpsti1 ")
`MPRnb( LNSUBPSTI2, 0.0        , "-"                             , "Length dependence of nsubpsti2 ")
`MPRnb( LNSUBPSTI3, 0.0        , "-"                             , "Length dependence of nsubpsti3 ")
`MPRnb( LCGSO     , 0.0        , "-"                             , "Length dependence of cgso ")
`MPRnb( LCGDO     , 0.0        , "-"                             , "Length dependence of cgdo ")
`MPRnb( LCLM1     , 0.0        , "-"                             , "Length dependence of clm1 ")
`MPRnb( LCLM2     , 0.0        , "-"                             , "Length dependence of clm2 ")
`MPRnb( LCLM3     , 0.0        , "-"                             , "Length dependence of clm3 ")
`MPRnb( LWFC      , 0.0        , "-"                             , "Length dependence of wfc ")
`MPRnb( LGIDL1    , 0.0        , "-"                             , "Length dependence of gidl1 ")
`MPRnb( LGIDL2    , 0.0        , "-"                             , "Length dependence of gidl2 ")
`MPRnb( LGLEAK1   , 0.0        , "-"                             , "Length dependence of gleak1 ")
`MPRnb( LGLEAK2   , 0.0        , "-"                             , "Length dependence of gleak2 ")
`MPRnb( LGLEAK3   , 0.0        , "-"                             , "Length dependence of gleak3 ")
`MPRnb( LGLEAK6   , 0.0        , "-"                             , "Length dependence of gleak6 ")
`MPRnb( LGLKSD1   , 0.0        , "-"                             , "Length dependence of glksd1 ")
`MPRnb( LGLKSD2   , 0.0        , "-"                             , "Length dependence of glksd2 ")
`MPRnb( LGLKB1    , 0.0        , "-"                             , "Length dependence of glkb1 ")
`MPRnb( LGLKB2    , 0.0        , "-"                             , "Length dependence of glkb2 ")
`MPRnb( LNFTRP    , 0.0        , "-"                             , "Length dependence of nftrp ")
`MPRnb( LNFALP    , 0.0        , "-"                             , "Length dependence of nfalp ")
`MPRnb( LIBPC1    , 0.0        , "-"                             , "Length dependence of ibpc1 ")
`MPRnb( LIBPC2    , 0.0        , "-"                             , "Length dependence of ibpc2 ")
`MPRnb( LCGBO     , 0.0        , "-"                             , "Length dependence of cgbo ")
`MPRnb( LCVDSOVER , 0.0        , "-"                             , "Length dependence of cvdsover ")
`MPRnb( LFALPH    , 0.0        , "-"                             , "Length dependence of falph ")
`MPRnb( LNPEXT    , 0.0        , "-"                             , "Length dependence of npext ")
`MPRnb( LPOWRAT   , 0.0        , "-"                             , "Length dependence of powrat ")
`MPRnb( LRD       , 0.0        , "-"                             , "Length dependence of rd ")        //HV1
`MPRnb( LRD22     , 0.0        , "-"                             , "Length dependence of rd22 ")      //HV1
`MPRnb( LRD23     , 0.0        , "-"                             , "Length dependence of rd23 ")      //HV1
`MPRnb( LRD24     , 0.0        , "-"                             , "Length dependence of rd24 ")      //HV1
`MPRnb( LRDICT1   , 0.0        , "-"                             , "Length dependence of rdict1 ")    //HV1
`MPRnb( LRDOV13   , 0.0        , "-"                             , "Length dependence of rdov13 ")    //HV1
`MPRnb( LRDSLP1   , 0.0        , "-"                             , "Length dependence of rdslp1 ")    //HV1
`MPRnb( LRDVB     , 0.0        , "-"                             , "Length dependence of rdvb ")      //HV1
`MPRnb( LRDVD     , 0.0        , "-"                             , "Length dependence of rdvd ")      //HV1
`MPRnb( LRDVG11   , 0.0        , "-"                             , "Length dependence of rdvg11 ")    //HV1
`MPRnb( LRS       , 0.0        , "-"                             , "Length dependence of rs ")        //HV1  
`MPRnb( LRTH0     , 0.0        , "-"                             , "Length dependence of rth0 ")
`MPRnb( LVOVER    , 0.0        , "-"                             , "Length dependence of vover ")
`MPRnb( WVMAX     , 0.0        , "-"                             , "Width dependence of vmax ")
`MPRnb( WBGTMP1   , 0.0        , "-"                             , "Width dependence of bgtmp1 ")
`MPRnb( WBGTMP2   , 0.0        , "-"                             , "Width dependence of bgtmp2 ")
`MPRnb( WEG0      , 0.0        , "-"                             , "Width dependence of eg0 ")
`MPRnb( WVFBOVER  , 0.0        , "-"                             , "Width dependence of vfbover ")
`MPRnb( WNOVER    , 0.0        , "-"                             , "Width dependence of nover ")
`MPRnb( WNOVERS   , 0.0        , "-"                             , "Width dependence of novers on source side ")
`MPRnb( WWL2      , 0.0        , "-"                             , "Width dependence of wl2 ")
`MPRnb( WVFBC     , 0.0        , "-"                             , "Width dependence of vfbc ")
`MPRnb( WNSUBC    , 0.0        , "-"                             , "Width dependence of nsubc ")
`MPRnb( WNSUBP    , 0.0        , "-"                             , "Width dependence of nsubp ")
`MPRnb( WSCP1     , 0.0        , "-"                             , "Width dependence of scp1 ")
`MPRnb( WSCP2     , 0.0        , "-"                             , "Width dependence of scp2 ")
`MPRnb( WSCP3     , 0.0        , "-"                             , "Width dependence of scp3 ")
`MPRnb( WSC1      , 0.0        , "-"                             , "Width dependence of sc1 ")
`MPRnb( WSC2      , 0.0        , "-"                             , "Width dependence of sc2 ")
`MPRnb( WSC3      , 0.0        , "-"                             , "Width dependence of sc3 ")
`MPRnb( WPGD1     , 0.0        , "-"                             , "Width dependence of pgd1 ")
`MPRnb( WNDEP     , 0.0        , "-"                             , "Width dependence of ndep ")
`MPRnb( WNINV     , 0.0        , "-"                             , "Width dependence of ninv ")
`MPRnb( WMUECB0   , 0.0        , "-"                             , "Width dependence of muecb0 ")
`MPRnb( WMUECB1   , 0.0        , "-"                             , "Width dependence of muecb1 ")
`MPRnb( WMUEPH1   , 0.0        , "-"                             , "Width dependence of mueph1 ")
`MPRnb( WVTMP     , 0.0        , "-"                             , "Width dependence of vtmp ")
`MPRnb( WWVTH0    , 0.0        , "-"                             , "Width dependence of wvth0 ")
`MPRnb( WMUESR1   , 0.0        , "-"                             , "Width dependence of muesr1 ")
`MPRnb( WMUETMP   , 0.0        , "-"                             , "Width dependence of muetmp ")
`MPRnb( WSUB1     , 0.0        , "-"                             , "Width dependence of sub1 ")
`MPRnb( WSUB2     , 0.0        , "-"                             , "Width dependence of sub2 ")
`MPRnb( WSVDS     , 0.0        , "-"                             , "Width dependence of svds ")
`MPRnb( WSVBS     , 0.0        , "-"                             , "Width dependence of svbs ")
`MPRnb( WSVGS     , 0.0        , "-"                             , "Width dependence of svgs ")
`MPRnb( WSUB1SNP  , WSUB1      , "-"                             , "Width dependence of sub1 ")
`MPRnb( WSUB2SNP  , WSUB2      , "-"                             , "Width dependence of sub2 ")
`MPRnb( WSVDSSNP  , WSVDS      , "-"                             , "Width dependence of svds ")
//`MPRnb( WSVGSSNP  , WSVGS      , "-"                             , "Width dependence of svgssnp ")
//`MPRnb( WSVBSSNP  , WSVBS      , "-"                             , "Width dependence of svbssnp ")
`MPRnb( WFN1      , 0.0        , "-"                             , "Width dependence of fn1 ")
`MPRnb( WFN2      , 0.0        , "-"                             , "Width dependence of fn2 ")
`MPRnb( WFN3      , 0.0        , "-"                             , "Width dependence of fn3 ")
`MPRnb( WFVBS     , 0.0        , "-"                             , "Width dependence of fvbs ")
`MPRnb( WNSTI     , 0.0        , "-"                             , "Width dependence of nsti ")
`MPRnb( WWSTI     , 0.0        , "-"                             , "Width dependence of wsti ")
`MPRnb( WSCSTI1   , 0.0        , "-"                             , "Width dependence of scsti1 ")
`MPRnb( WSCSTI2   , 0.0        , "-"                             , "Width dependence of scsti2 ")
`MPRnb( WVTHSTI   , 0.0        , "-"                             , "Width dependence of vthsti ")
`MPRnb( WMUESTI1  , 0.0        , "-"                             , "Width dependence of muesti1 ")
`MPRnb( WMUESTI2  , 0.0        , "-"                             , "Width dependence of muesti2 ")
`MPRnb( WMUESTI3  , 0.0        , "-"                             , "Width dependence of muesti3 ")
`MPRnb( WNSUBPSTI1, 0.0        , "-"                             , "Width dependence of nsubpsti1 ")
`MPRnb( WNSUBPSTI2, 0.0        , "-"                             , "Width dependence of nsubpsti2 ")
`MPRnb( WNSUBPSTI3, 0.0        , "-"                             , "Width dependence of nsubpsti3 ")
`MPRnb( WCGSO     , 0.0        , "-"                             , "Width dependence of cgso ")
`MPRnb( WCGDO     , 0.0        , "-"                             , "Width dependence of cgdo ")
`MPRnb( WCLM1     , 0.0        , "-"                             , "Width dependence of clm1 ")
`MPRnb( WCLM2     , 0.0        , "-"                             , "Width dependence of clm2 ")
`MPRnb( WCLM3     , 0.0        , "-"                             , "Width dependence of clm3 ")
`MPRnb( WWFC      , 0.0        , "-"                             , "Width dependence of wfc ")
`MPRnb( WGIDL1    , 0.0        , "-"                             , "Width dependence of gidl1 ")
`MPRnb( WGIDL2    , 0.0        , "-"                             , "Width dependence of gidl2 ")
`MPRnb( WGLEAK1   , 0.0        , "-"                             , "Width dependence of gleak1 ")
`MPRnb( WGLEAK2   , 0.0        , "-"                             , "Width dependence of gleak2 ")
`MPRnb( WGLEAK3   , 0.0        , "-"                             , "Width dependence of gleak3 ")
`MPRnb( WGLEAK6   , 0.0        , "-"                             , "Width dependence of gleak6 ")
`MPRnb( WGLKSD1   , 0.0        , "-"                             , "Width dependence of glksd1 ")
`MPRnb( WGLKSD2   , 0.0        , "-"                             , "Width dependence of glksd2 ")
`MPRnb( WGLKB1    , 0.0        , "-"                             , "Width dependence of glkb1 ")
`MPRnb( WGLKB2    , 0.0        , "-"                             , "Width dependence of glkb2 ")
`MPRnb( WNFTRP    , 0.0        , "-"                             , "Width dependence of nftrp ")
`MPRnb( WNFALP    , 0.0        , "-"                             , "Width dependence of nfalp ")
`MPRnb( WIBPC1    , 0.0        , "-"                             , "Width dependence of ibpc1 ")
`MPRnb( WIBPC2    , 0.0        , "-"                             , "Width dependence of ibpc2 ")
`MPRnb( WCGBO     , 0.0        , "-"                             , "Width dependence of cgbo ")
`MPRnb( WCVDSOVER , 0.0        , "-"                             , "Width dependence of cvdsover ")
`MPRnb( WFALPH    , 0.0        , "-"                             , "Width dependence of falph ")
`MPRnb( WNPEXT    , 0.0        , "-"                             , "Width dependence of npext ")
`MPRnb( WPOWRAT   , 0.0        , "-"                             , "Width dependence of powrat ")
`MPRnb( WRD       , 0.0        , "-"                             , "Width dependence of rd ")         //HV1
`MPRnb( WRD22     , 0.0        , "-"                             , "Width dependence of rd22 ")         //HV1
`MPRnb( WRD23     , 0.0        , "-"                             , "Width dependence of rd23 ")         //HV1
`MPRnb( WRD24     , 0.0        , "-"                             , "Width dependence of rd24 ")         //HV1
`MPRnb( WRDICT1   , 0.0        , "-"                             , "Width dependence of rdict1 ")     //HV1
`MPRnb( WRDOV13   , 0.0        , "-"                             , "Width dependence of rdov13 ")     //HV1
`MPRnb( WRDSLP1   , 0.0        , "-"                             , "Width dependence of rdslp1 ")     //HV1
`MPRnb( WRDVB     , 0.0        , "-"                             , "Width dependence of rdvb ")         //HV1
`MPRnb( WRDVD     , 0.0        , "-"                             , "Width dependence of rdvd ")         //HV1
`MPRnb( WRDVG11   , 0.0        , "-"                             , "Width dependence of rdvg11 ")     //HV1
`MPRnb( WRS       , 0.0        , "-"                             , "Width dependence of rs ")         //HV1  
`MPRnb( WRTH0     , 0.0        , "-"                             , "Width dependence of rth0 ")
`MPRnb( WVOVER    , 0.0        , "-"                             , "Width dependence of vover ")
`MPRnb( PVMAX     , 0.0        , "-"                             , "Cross-term dependence of vmax ")
`MPRnb( PBGTMP1   , 0.0        , "-"                             , "Cross-term dependence of bgtmp1 ")
`MPRnb( PBGTMP2   , 0.0        , "-"                             , "Cross-term dependence of bgtmp2 ")
`MPRnb( PEG0      , 0.0        , "-"                             , "Cross-term dependence of eg0 ")
`MPRnb( PVFBOVER  , 0.0        , "-"                             , "Cross-term dependence of vfbover ")
`MPRnb( PNOVER    , 0.0        , "-"                             , "Cross-term dependence of nover ")
`MPRnb( PNOVERS   , 0.0        , "-"                             , "Cross-term dependence of nover on source side ")
`MPRnb( PWL2      , 0.0        , "-"                             , "Cross-term dependence of wl2 ")
`MPRnb( PVFBC     , 0.0        , "-"                             , "Cross-term dependence of vfbc ")
`MPRnb( PNSUBC    , 0.0        , "-"                             , "Cross-term dependence of nsubc ")
`MPRnb( PNSUBP    , 0.0        , "-"                             , "Cross-term dependence of nsubp ")
`MPRnb( PSCP1     , 0.0        , "-"                             , "Cross-term dependence of scp1 ")
`MPRnb( PSCP2     , 0.0        , "-"                             , "Cross-term dependence of scp2 ")
`MPRnb( PSCP3     , 0.0        , "-"                             , "Cross-term dependence of scp3 ")
`MPRnb( PSC1      , 0.0        , "-"                             , "Cross-term dependence of sc1 ")
`MPRnb( PSC2      , 0.0        , "-"                             , "Cross-term dependence of sc2 ")
`MPRnb( PSC3      , 0.0        , "-"                             , "Cross-term dependence of sc3 ")
`MPRnb( PPGD1     , 0.0        , "-"                             , "Cross-term dependence of pgd1 ")
`MPRnb( PNDEP     , 0.0        , "-"                             , "Cross-term dependence of ndep ")
`MPRnb( PNINV     , 0.0        , "-"                             , "Cross-term dependence of ninv ")
`MPRnb( PMUECB0   , 0.0        , "-"                             , "Cross-term dependence of muecb0 ")
`MPRnb( PMUECB1   , 0.0        , "-"                             , "Cross-term dependence of muecb1 ")
`MPRnb( PMUEPH1   , 0.0        , "-"                             , "Cross-term dependence of mueph1 ")
`MPRnb( PVTMP     , 0.0        , "-"                             , "Cross-term dependence of vtmp ")
`MPRnb( PWVTH0    , 0.0        , "-"                             , "Cross-term dependence of wvth0 ")
`MPRnb( PMUESR1   , 0.0        , "-"                             , "Cross-term dependence of muesr1 ")
`MPRnb( PMUETMP   , 0.0        , "-"                             , "Cross-term dependence of muetmp ")
`MPRnb( PSUB1     , 0.0        , "-"                             , "Cross-term dependence of sub1 ")
`MPRnb( PSUB2     , 0.0        , "-"                             , "Cross-term dependence of sub2 ")
`MPRnb( PSVDS     , 0.0        , "-"                             , "Cross-term dependence of svds ")
`MPRnb( PSVBS     , 0.0        , "-"                             , "Cross-term dependence of svbs ")
`MPRnb( PSVGS     , 0.0        , "-"                             , "Cross-term dependence of svgs ")
`MPRnb( PSUB1SNP  , PSUB1      , "-"                             , "Cross-term dependence of sub1 ")
`MPRnb( PSUB2SNP  , PSUB2      , "-"                             , "Cross-term dependence of sub2 ")
`MPRnb( PSVDSSNP  , PSVDS      , "-"                             , "Cross-term dependence of svds ")
//`MPRnb( PSVGSSNP  , PSVGS      , "-"                             , "Cross-term dependence of svgssnp ")
//`MPRnb( PSVBSSNP  , PSVBS      , "-"                             , "Cross-term dependence of svbssnp ")
`MPRnb( PFN1      , 0.0        , "-"                             , "Cross-term dependence of fn1 ")
`MPRnb( PFN2      , 0.0        , "-"                             , "Cross-term dependence of fn2 ")
`MPRnb( PFN3      , 0.0        , "-"                             , "Cross-term dependence of fn3 ")
`MPRnb( PFVBS     , 0.0        , "-"                             , "Cross-term dependence of fvbs ")
`MPRnb( PNSTI     , 0.0        , "-"                             , "Cross-term dependence of nsti ")
`MPRnb( PWSTI     , 0.0        , "-"                             , "Cross-term dependence of wsti ")
`MPRnb( PSCSTI1   , 0.0        , "-"                             , "Cross-term dependence of scsti1 ")
`MPRnb( PSCSTI2   , 0.0        , "-"                             , "Cross-term dependence of scsti2 ")
`MPRnb( PVTHSTI   , 0.0        , "-"                             , "Cross-term dependence of vthsti ")
`MPRnb( PMUESTI1  , 0.0        , "-"                             , "Cross-term dependence of muesti1 ")
`MPRnb( PMUESTI2  , 0.0        , "-"                             , "Cross-term dependence of muesti2 ")
`MPRnb( PMUESTI3  , 0.0        , "-"                             , "Cross-term dependence of muesti3 ")
`MPRnb( PNSUBPSTI1, 0.0        , "-"                             , "Cross-term dependence of nsubpsti1 ")
`MPRnb( PNSUBPSTI2, 0.0        , "-"                             , "Cross-term dependence of nsubpsti2 ")
`MPRnb( PNSUBPSTI3, 0.0        , "-"                             , "Cross-term dependence of nsubpsti3 ")
`MPRnb( PCGSO     , 0.0        , "-"                             , "Cross-term dependence of cgso ")
`MPRnb( PCGDO     , 0.0        , "-"                             , "Cross-term dependence of cgdo ")
`MPRnb( PCLM1     , 0.0        , "-"                             , "Cross-term dependence of clm1 ")
`MPRnb( PCLM2     , 0.0        , "-"                             , "Cross-term dependence of clm2 ")
`MPRnb( PCLM3     , 0.0        , "-"                             , "Cross-term dependence of clm3 ")
`MPRnb( PWFC      , 0.0        , "-"                             , "Cross-term dependence of wfc ")
`MPRnb( PGIDL1    , 0.0        , "-"                             , "Cross-term dependence of gidl1 ")
`MPRnb( PGIDL2    , 0.0        , "-"                             , "Cross-term dependence of gidl2 ")
`MPRnb( PGLEAK1   , 0.0        , "-"                             , "Cross-term dependence of gleak1 ")
`MPRnb( PGLEAK2   , 0.0        , "-"                             , "Cross-term dependence of gleak2 ")
`MPRnb( PGLEAK3   , 0.0        , "-"                             , "Cross-term dependence of gleak3 ")
`MPRnb( PGLEAK6   , 0.0        , "-"                             , "Cross-term dependence of gleak6 ")
`MPRnb( PGLKSD1   , 0.0        , "-"                             , "Cross-term dependence of glksd1 ")
`MPRnb( PGLKSD2   , 0.0        , "-"                             , "Cross-term dependence of glksd2 ")
`MPRnb( PGLKB1    , 0.0        , "-"                             , "Cross-term dependence of glkb1 ")
`MPRnb( PGLKB2    , 0.0        , "-"                             , "Cross-term dependence of glkb2 ")
`MPRnb( PNFTRP    , 0.0        , "-"                             , "Cross-term dependence of nftrp ")
`MPRnb( PNFALP    , 0.0        , "-"                             , "Cross-term dependence of nfalp ")
`MPRnb( PIBPC1    , 0.0        , "-"                             , "Cross-term dependence of ibpc1 ")
`MPRnb( PIBPC2    , 0.0        , "-"                             , "Cross-term dependence of ibpc2 ")
`MPRnb( PCGBO     , 0.0        , "-"                             , "Cross-term dependence of cgbo ")
`MPRnb( PCVDSOVER , 0.0        , "-"                             , "Cross-term dependence of cvdsover ")
`MPRnb( PFALPH    , 0.0        , "-"                             , "Cross-term dependence of falph ")
`MPRnb( PNPEXT    , 0.0        , "-"                             , "Cross-term dependence of npext ")
`MPRnb( PPOWRAT   , 0.0        , "-"                             , "Cross-term dependence of powrat ")
`MPRnb( PRD       , 0.0        , "-"                             , "Cross-term dependence of rd ")      //HV1
`MPRnb( PRD22     , 0.0        , "-"                             , "Cross-term dependence of rd22 ")      //HV1
`MPRnb( PRD23     , 0.0        , "-"                             , "Cross-term dependence of rd23 ")      //HV1
`MPRnb( PRD24     , 0.0        , "-"                             , "Cross-term dependence of rd24 ")      //HV1
`MPRnb( PRDICT1   , 0.0        , "-"                             , "Cross-term dependence of rdict1 ")    //HV1
`MPRnb( PRDOV13   , 0.0        , "-"                             , "Cross-term dependence of rdov13 ")    //HV1
`MPRnb( PRDSLP1   , 0.0        , "-"                             , "Cross-term dependence of rdslp1 ")    //HV1
`MPRnb( PRDVB     , 0.0        , "-"                             , "Cross-term dependence of rdvb ")      //HV1
`MPRnb( PRDVD     , 0.0        , "-"                             , "Cross-term dependence of rdvd ")      //HV1
`MPRnb( PRDVG11   , 0.0        , "-"                             , "Cross-term dependence of rdvg11 ")    //HV1
`MPRnb( PRS       , 0.0        , "-"                             , "Cross-term dependence of rs ")      //HV1   
`MPRnb( PRTH0     , 0.0        , "-"                             , "Cross-term dependence of rth0 ")
`MPRnb( PVOVER    , 0.0        , "-"                             , "Cross-term dependence of vover ")

`ifdef COEXTDIO 
       /* for Junction Diode Model */
`else 
`MPRnb( LJS0      , 0.0        , "-"                             , "Length dependence of js0 ")
`MPRnb( LJS0SW    , 0.0        , "-"                             , "Length dependence of js0sw ")
`MPRnb( LNJ       , 0.0        , "-"                             , "Length dependence of nj ")
`MPRnb( LCISBK    , 0.0        , "-"                             , "Length dependence of cisbk ")
`MPRnb( LVDIFFJ   , 0.0        , "-"                             , "Length dependence of vdiffj ")
`MPRnb( LJS0D     , LJS0       , "-"                             , "Length dependence of js0d ")
`MPRnb( LJS0SWD   , LJS0SW     , "-"                             , "Length dependence of js0swd ")
`MPRnb( LNJD      , LNJ        , "-"                             , "Length dependence of njd ")
`MPRnb( LCISBKD   , LCISBK     , "-"                             , "Length dependence of cisbkd ")
`MPRnb( LVDIFFJD  , LVDIFFJ    , "-"                             , "Length dependence of vdiffjd ")
`MPRnb( LJS0S     , LJS0D      , "-"                             , "Length dependence of js0s ")
`MPRnb( LJS0SWS   , LJS0SWD    , "-"                             , "Length dependence of js0sws ")
`MPRnb( LNJS      , LNJD       , "-"                             , "Length dependence of njs ")
`MPRnb( LCISBKS   , LCISBKD    , "-"                             , "Length dependence of cisbks ")
`MPRnb( LVDIFFJS  , LVDIFFJD   , "-"                             , "Length dependence of vdiffjs ")
`MPRnb( WJS0      , 0.0        , "-"                             , "Width dependence of js0 ")
`MPRnb( WJS0SW    , 0.0        , "-"                             , "Width dependence of js0sw ")
`MPRnb( WNJ       , 0.0        , "-"                             , "Width dependence of nj ")
`MPRnb( WCISBK    , 0.0        , "-"                             , "Width dependence of cisbk ")
`MPRnb( WVDIFFJ   , 0.0        , "-"                             , "Width dependence of vdiffj ")
`MPRnb( WJS0D     , WJS0       , "-"                             , "Wength dependence of js0d ")
`MPRnb( WJS0SWD   , WJS0SW     , "-"                             , "Wength dependence of js0swd ")
`MPRnb( WNJD      , WNJ        , "-"                             , "Wength dependence of njd ")
`MPRnb( WCISBKD   , WCISBK     , "-"                             , "Wength dependence of cisbkd ")
`MPRnb( WVDIFFJD  , WVDIFFJ    , "-"                             , "Wength dependence of vdiffjd ")
`MPRnb( WJS0S     , WJS0D      , "-"                             , "Wength dependence of js0s ")
`MPRnb( WJS0SWS   , WJS0SWD    , "-"                             , "Wength dependence of js0sws ")
`MPRnb( WNJS      , WNJD       , "-"                             , "Wength dependence of njs ")
`MPRnb( WCISBKS   , WCISBKD    , "-"                             , "Wength dependence of cisbks ")
`MPRnb( WVDIFFJS  , WVDIFFJD   , "-"                             , "Wength dependence of vdiffjs ")
`MPRnb( PJS0      , 0.0        , "-"                             , "Cross-term dependence of js0 ")
`MPRnb( PJS0SW    , 0.0        , "-"                             , "Cross-term dependence of js0sw ")
`MPRnb( PNJ       , 0.0        , "-"                             , "Cross-term dependence of nj ")
`MPRnb( PCISBK    , 0.0        , "-"                             , "Cross-term dependence of cisbk ")
`MPRnb( PVDIFFJ   , 0.0        , "-"                             , "Cross-term dependence of vdiffj ")
`MPRnb( PJS0D     , PJS0       , "-"                             , "Cross-term dependence of js0d ")
`MPRnb( PJS0SWD   , PJS0SW     , "-"                             , "Cross-term dependence of js0swd ")
`MPRnb( PNJD      , PNJ        , "-"                             , "Cross-term dependence of njd ")
`MPRnb( PCISBKD   , PCISBK     , "-"                             , "Cross-term dependence of cisbkd ")
`MPRnb( PVDIFFJD  , PVDIFFJ    , "-"                             , "Cross-term dependence of vdiffjd ")
`MPRnb( PJS0S     , PJS0D      , "-"                             , "Cross-term dependence of js0s ")
`MPRnb( PJS0SWS   , PJS0SWD    , "-"                             , "Cross-term dependence of js0sws ")
`MPRnb( PNJS      , PNJD       , "-"                             , "Cross-term dependence of njs ")
`MPRnb( PCISBKS   , PCISBKD    , "-"                             , "Cross-term dependence of cisbks ")
`MPRnb( PVDIFFJS  , PVDIFFJD   , "-"                             , "Cross-term dependence of vdiffjs )")
`endif /* End of COEXTDIO */


// Output Physical Values in operating point analysis //
`OPP( idse      , "A"   , "Drain-Source current")
`OPP( IdsAcc    , "A"   , "Accumlation current of Depletion mode MOSFET")
`OPP( IdsRes    , "A"   , "Resistor current of Depletion mode MOSFET")
`OPP( IresLeak  , "A"   , "Leak current of Depletion mode MOSFET")
`OPP( Rdd       , "Ohm" , " Drift Resistance on Drain side")
`OPP( Rsd       , "Ohm" , " Drift Resistance on Source side")
`OPP( Isuba     , "A"   , "Substrate current")
`OPP( igidle    , "A"   , "Gate-Induced Drain Leakage current")
`OPP( igisle    , "A"   , "Gate-Induced Source Leakage current")
`OPP( igde      , "A"   , "Gate-Drain current")
`OPP( igse      , "A"   , "Gate-Source current")
`OPP( igbe      , "A"   , "Gate-Substrate current")
`OPP( gme       , "S"   , "Transconductance")
`OPP( gdse      , "S"   , "Channel conductance")
`OPP( gmbse     , "S"   , "Body effect(Back gate) transconductance")
`OPP( gmte      , "S"   , "Temperature transconductance")
`OPP( Von       , "V"   , "Threshold voltage")
`OPP( Vdsat     , "V"   , "Saturation voltage")
`OPP( Qg        , "C"   , "Gate charge")
`OPP( Qd        , "C"   , "Drain charge")
`OPP( Qb        , "C"   , "Bulk charge")
`OPP( cggbd     , "F"   , " g-g MOSFET capacitance")
`OPP( cgdbd     , "F"   , " g-d MOSFET capacitance")
`OPP( cgsbd     , "F"   , " g-s MOSFET capacitance")
`OPP( cgbbd     , "F"   , " g-b MOSFET capacitance")
`OPP( cbgbd     , "F"   , " b-g MOSFET capacitance")
`OPP( cbsbd     , "F"   , " b-s MOSFET capacitance")
`OPP( cbdbd     , "F"   , " b-d MOSFET capacitance")
`OPP( cbbbd     , "F"   , " b-b MOSFET capacitance")
`OPP( cdgbd     , "F"   , " d-g MOSFET capacitance")
`OPP( cddbd     , "F"   , " d-d MOSFET capacitance")
`OPP( cdsbd     , "F"   , " d-s MOSFET capacitance")
`OPP( cdbbd     , "F"   , " d-b MOSFET capacitance")
`OPP( csgbd     , "F"   , " s-g MOSFET capacitance")
`OPP( csdbd     , "F"   , " s-d MOSFET capacitance")
`OPP( cssbd     , "F"   , " s-s MOSFET capacitance")
`OPP( csbbd     , "F"   , " s-b MOSFET capacitance")
`OPP( cgdobd    , "F"   , " g-d MOSFET overlap capacitance")
`OPP( cgsobd    , "F"   , " g-s MOSFET overlap capacitance")
`OPP( cgbobd    , "F"   , " g-b MOSFET overlap capacitance")
`OPP( TK        , "K"   , "Actual device temperature")
`OPP( DTSH      , "K"   , "Temperature increase due to self-heating")
`OPP( RS_Ps0LD  , "V"    , "source side overlap region surface potential")
`OPP( RS_QbuLD  , "C/m^2", "source side overlap region arial charge")
`OPP( IdsPT     , "A"    , "Punchthrough current ")
`OPP( IdsPT0    , "A"    , "Punchthrough current (surface) ")
`OPP( IdsPT1    , "A"    , "Punchthrough current (deep) ")
`OPP( OP_mode   , "-"    , "Drain/Source flipping mode")
`OPP( OP_Vdsi   , "V"    , "Internal drain-source voltage")
`OPP( OP_Vbscl  , "V"    , "Clamped bulk-source voltage")

`ifdef _AGING_
`OPP( DVTBTI    , "C"   , "threshold voltage shift of trap")
`OPP( NTRAP0    , "C"   , "trap charge on source side")
`OPP( NTRAPL    , "C"   , "trap charge on drain side")
`endif

`ifdef COEXTDIO 
       /* for Junction Diode Model */
`else 
`OPP( ibdb      , "A"   , " b-d Diode current")
`OPP( ibsb      , "A"   , " b-s Diode current")
`OPP( ibdbi     , "A"   , " b-dp Diode current")
`OPP( ibsbi     , "A"   , " b-sp Diode current")
`OPP( Gbd       , "S"   , " b-d Diode conductance")
`OPP( Gbs       , "S"   , " b-s Diode conductance")
`OPP( capbdb    , "F"   , " b-d Diode capacitance")
`OPP( capbsb    , "F"   , " b-s Diode capacitance")
`OPP( capbdbi   , "F"   , " b-dp Diode capacitance")
`OPP( capbsbi   , "F"   , " b-sp Diode capacitance")
`OPP( Qbd       , "C"   , " b-d Diode charge")
`OPP( Qbs       , "C"   , " b-s Diode charge")
`OPP( Qbdi      , "C"   , " b-dp Diode charge")
`OPP( Qbsi      , "C"   , " b-sp Diode charge")
`endif /* End of COEXTDIO */


// End of Output Physical Values //

//--------------------------------------------------
// Local Variables
//
// Constants ----------------------- //
// initial guess *//
   real Vbs_max ;
   real Vbs_bnd, Gjmin ; 
// Internal flags --------------------//
   integer flg_rsrd ; //HV1
   integer flg_rd, flg_rs; // node collaps
   integer flg_pprv ;
   integer flg_noqi ;
   integer flg_conv ;
   integer flg_qme ;
   integer flg_nqs ;
   integer flg_qy ;
// Important Variables in HiSIM -------//
// confined bias //
   real Vbscl, VbscldVbs, Vgp, Vgs_fb ;
// Ps0 : surface potential at the source side //
   real Ps0, Ps0_ini, Ps0_iniA, Ps0_iniB ;
// Psl : surface potential at the drain side //
   real Psl, Psl_lim, dPlim ;
// Pds :=Psl - Ps0 //
   real Pds, Pds_ini, Pds_max ;
// iteration numbers of Ps0 and Psl equations. //
   integer lp_s0, lp_sl ;
// Xi0 :=beta * ( Ps0 - Vbs ) - 1. //
   real Xi0, Xi0p12, Xi0p32 ;
// Xil :=beta * ( Psl - Vbs ) - 1. //
   real Xil, Xilp12, Xilp32 ;
// modified bias and potential for sym.//
   real Vbsz, Vdsz, Vgsz, Vzadd, Ps0z, Pzadd ;
// IBPC //
   real dVbsIBPC, dG3, dG4, dIdd, betaWL ;
// Chi :=beta * ( Ps{0/l} - Vbs ) //
   real Chi ;
// Rho :=beta * ( Psl - Vds ) //
   real Rho ;
// threshold voltage //
   real Vth, Vth0 ;
// variation of threshold voltage //
   real dVth, dVth0, dVthSC, Pb20b, dVthW ;

// Alpha and related parameters //
   real Alpha, Achi, VgVt, Pslsat, VdsatS, Delta ;
// Q_B and capacitances //
   real Qbu ;
// Q_I and capacitances //
   real Qi, Qiu ;
// Q_D and capacitances //
// channel current //
   real Ids, Ids0 , Idsorg ;
// STI //
   real dVthSCSTI, Vgssti, costi0, costi1, costi3, costi4, costi5, costi6 ;
   real costi7, Psasti, Psbsti, Psab, Psti, sq1sti, sq2sti, Qn0sti, Idssti ;
// constants //
   real beta, beta_inv, beta2, Pb2, Pb20, Pb2c, Vfb, c_eox, Leff, Weff ;
   real WeffLD_nf, Ldrift0, q_Nsub ;
// PART-1 //
// Accumulation zone //
   real Psa ;
// CLM//
   real Psdl, Lred, Lch, Wd, Aclm ;
// Pocket Implant //
   real Vthp, dVthLP, bs12, Qbmm, dqb, Vdx, Vdx2, Pbsum, sqrt_Pbsum ;
// Poly-Depletion Effect //
   real dPpg ;
// Quantum Effect //
   real Toxe, dTox, Cox, Cox_inv, Tox0, Cox0, Cox0_inv, Vthq ;
// Igate , Igidl , Igisl //
   real Psdlz, Egp12, Egp32, E1, Etun, Vdsp, Egidl, Egisl, Igate, Igs ;
   real Igd, Igb, Igidl, Igisl, Vdb, Vsb ;
// connecting function //
   real FD2, FMDVDS, cnst0, cnst1, cnstCoxi, fac1, fac1p2, fs01, fs01_dPs0 ;
   real fs02, fs02_dPs0, fsl1, fsl1_dPsl, fsl2, fsl2_dPsl, cfs1, fb, fb_dChi ;
   real fi, fi_dChi, exp_Chi, exp_Rho, exp_bVbs, exp_bVbsVds, exp_bPs0 ;
   real Fs0, Fs0_dPs0, Fsl, Fsl_dPsl, dPs0, dPsl, Qn0, Qb0, Qbnm, DtPds ;
   real Qinm, Qidn, Qdnm, Qddn, Quot, Qdrat, Idd, Idd1, Fdd, Eeff, Rns ;
   real Mu, Muun, Ey, Em, Vmaxe, Eta, Eta1, Eta1p12, Eta1p32, Eta1p52, Zeta12 ;
   real Zeta32, Zeta52, F00, F10, F30, F11, Vgs_min, Ps0_min, Acn, Acd ;
   real Ac1, Ac2, Ac3, Ac4, Ac31, Ac41 ;
// PART-2 (Isub) //
   real Isub, IsubLD, Psislsat, Psisubsat, Ifn, Eg12, Eg32 ;
   real IsubIBPC; // 20180413 additive contribution to Isub
   real wk_ii; // 20180413 stow impact ionization coeff for later use at IsubIBPC
// PART-3 (overlap) //
// const double cov_dlt=1.0e-1 ; //
// const double covvgmax=5.0 ; //
   real cov_slp, cov_mag, Qgos, Qgod, Qgbo, Cgdoe, Cgsoe, Cgbo_loc, Qgso ;
   real Qgdo ;
// fringe capacitance //
   real Qfd, Cfd, Qfs, Cfs ;
// Cqy //
   real Ec, Pslk, Qy ;
// PART-5 (NQS) //
   real tau, taub , tauov ;
// PART-6 (noise) //
// 1/f //
   real NFalpe, NFtrpe, Cite, Nflic ;
// thermal //
   real Eyd, Mu_Ave, Nthrml, Mud_hoso ;
// induced gate noise ( Part 0/3 ) //
   real kusai00, kusaidd, kusaiL, kusai00L ;
   integer flg_ign ;
   real sqrtkusaiL, kusai_ig, gds0_ign, gds0_h2, GAMMA, crl_f ;
   real Nign0, MuModA, MuModB, correct_w1 ;
// usage of previously calculated values //
// temporary vars. & derivatives//
   real TX, TY, T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12 ;
   integer flg_zone ;
// Vdseff //
   real Vdseff, Vdsorg ;
// D/S Overlap Charges: Qovd/Qovs //
   real Qovdext, Qovsext, Qovd, Qovs, QbuLD, QbdLD, QbsLD, QodAD ;
   real QbdLDext, QbsLDext ;
   real Qovd_ADD, QbdLD_ADD ;
   real Qovs_ADD, QbsLD_ADD ;
// Vgsz for SCE and PGD //
   real Vbsz2 ;
// Multiplication factor * number of gate fingers //
   real Mfactor ;
//
// HiSIM-HV
//
// bias-dependent Rd , Rs //
   real Rdrift, Rsdrift, Ra ;
   real Vdse_eff, VdseModeNML, VdseModeRVS, Vbsegmt, Vdsegmt, Vgsegmt ;
   real Vbserev, Vdserev, Vgserev, Ra_alpha, Ra_beta ;
// modified external biases for symmetry //
     real Vdserevz, Vgserevz, Vbserevz ;
// Substrate Effect //
   real Vsubsrev, Wdep , ddriftld , ddriftldc;
// temperature-dependent variables for SHE model //
   real TTEMP, TTEMP0, Tdiff0, Tdiff0_2, Tdiff, Tdiff_2, Eg, Nin ;
// Qover 5/1 ckt-bias use //
   real Vgbgmt, Vxbgmt, Vxbgmtcl ;
// for Clamping Vxbgmt //
   real QsuLD, QiuLD ;
// IBPC //
   real IdsIBPC ;
// Qover //
   integer flg_ovzone ;
   real VgpLD, Vgb_fb_LD, Ps0LD, cnst1over ;
   integer flg_never_reach_vfbover ;
   real Lover_func, Noverd, Nover_func, cnst0over_func ;
   //real WTRE_lim,
   real Wdep_func;
   real Coxb0, Cox0_func ;
// Qover DD Model //
// real Ps0LD0 ;
   real Ps0LD_Vxb, Chib, phi_b, phi_b_dPss, dphi_sb, c_sb, fb_dPss ;
   integer lp_s0_max ;
// Break Down Model
   real Ibreake; 
   real Ibreakhe , Ibreak, Ibreaks ; // revived 20161012
   real HBdv , HBDCeff ;  //removed 20160414; revived 20161012 
   real iiFac; // 20160217
   real wIbjt, Ibjte, Ibjt, Ibjts; // , Ibjte, IdsBJT, IdsBJTs; // 20160218, 20160414
   integer lp_ld ;
   real Ta, Tb, Tc, Td, Tv, Tu, Tp, Tq, VgpLD_shift, gammaChi, psi, Chi_A ;
   real Chi_B, Chi_1 ;
// UC_*** Vars. for Device and Model Parameters
   integer UC_CORSRD  ;  //HV1 
   integer UC_CORDRIFT ; //HV1
   real UC_XPDV     , UC_XLDLD    , UC_SCP22    , UC_RDRCX    , UC_VMAX     ;
   real UC_BGTMP1   , UC_BGTMP2   , UC_EG0      , UC_VFBOVER  , UC_NOVER    ;
   real UC_NOVERS   , UC_WL2      , UC_VFBC     , UC_NSUBC    , UC_NSUBP    ;
   real UC_SCP1     , UC_SCP2     , UC_SCP3     , UC_SC1      , UC_SC2      ;
   real UC_SC3      , UC_PGD1     , UC_NDEP     , UC_NINV     , UC_MUECB0   ;
   real UC_MUECB1   , UC_MUEPH1   , UC_VTMP     , UC_WVTH0    , UC_MUESR1   ;
   real UC_MUETMP   , UC_SUB1     , UC_SUB2     , UC_SVDS     , UC_SVBS     ;
   real UC_SVGS     ;
   real UC_SUB1SNP  , UC_SUB2SNP  , UC_SVDSSNP  ; //, UC_SVGSSNP  , UC_SVBSSNP  ;
   real UC_NJUNC ;
   real UC_FN1      , UC_FN2      , UC_FN3      , UC_FVBS     ;
   real UC_NSTI     , UC_WSTI     , UC_SCSTI1   , UC_SCSTI2   , UC_VTHSTI   ;
   real UC_MUESTI1  , UC_MUESTI2  , UC_MUESTI3  , UC_NSUBPSTI1 , UC_NSUBPSTI2 ;
   real UC_NSUBPSTI3 , UC_CGSO     , UC_CGDO     , UC_CLM1     , UC_CLM2     ;
   real UC_CLM3     , UC_WFC      , UC_GIDL1    , UC_GIDL2    , UC_GLEAK1   ;
   real UC_GLEAK2   , UC_GLEAK3   , UC_GLEAK6   , UC_GLKSD1   , UC_GLKSD2   ;
   real UC_GLKB1    , UC_GLKB2    , UC_NFTRP    , UC_NFALP    , UC_IBPC1    ;
   real UC_IBPC2    , UC_CGBO     , UC_CVDSOVER , UC_FALPH    , UC_NPEXT    ;
   real UC_POWRAT   ;
   real UC_RD       , UC_RD22     , UC_RD23     , UC_RD24     ;                //HV1
   real UC_RDICT1   , UC_RDOV13   , UC_RDSLP1   , UC_RDVB     , UC_RDVD     ;  //HV1
   real UC_RDVG11   , UC_RS       ;                                            //HV1 
   real UC_RTH0     , UC_VOVER    ;
   real UC_TOXB     ; //not for unit change but for range reset
// MKS_*** Vars. for Device and Model Parameters
   real MKS_NSUBSUB , MKS_LL      , MKS_WL      , MKS_SVGSL   , MKS_SVGSW   ;
   real MKS_SVBSL   , MKS_SLGL    , MKS_SUB1L   , MKS_SLG     , MKS_SUB2L   ;
   real MKS_SUBLD2  ;
   real MKS_RDTEMP1 , MKS_RDTEMP2 , MKS_RDVDTEMP1 , MKS_RDVDTEMP2 ;            //HV1
   real MKS_GLKSD3  , MKS_GLEAK4  , MKS_GLEAK5  , MKS_GLEAK7  , MKS_CIT     ;
   real MKS_OVSLP   , MKS_DLY3    , MKS_CTH0    , MKS_RDRMUE  , MKS_RDRVMAX , MKS_RDRMUES, MKS_RDRVMAXS;
   real MKS_NSUBCDFM;
   real MKS_DLYOV    ;
// Vars. for hsmhvtemp.c 
   real mueph  , dL     , dLLD   , LG     , dW     , dWLD   , dWCV   , WG     ;
   real WLG    , Lgate  , Wgate  , Nsubpp , Nsubps , Nsub   , Nsubb  , Lod_half ;
   real Lod_half_ref , log_Tratio , Edri   , Vdri   , Mu0    , Cx     , Car    , Xov    ;
   real Carr   , Vddpz  , Wdepl  , Wjunc0 , Wrdrdjunc , Wjunc  , Wjuncld ;
//eval_rdrifts.inc
   real Ldrifte_s, Novers, Mu0_s, Vmaxe_s, Edri_s, Vdri_s;
   real Mu_s, Carr_s, Xov_s, GD_s, Xmax_s;
//
   integer flg_pgd ;
   real egtnom , cecox  , msc    , clmmod , EF_MUEPH1 , EF_NSUBP;
   real EF_NSUBC , NSUBCDFM_GIVEN , muesr  , ndep_o_esi , ninv_o_esi;
   real ninvd0 , lgatesm , dVthsm , weff_ld , weff_cv , weff_nf;
   real ninvd0cRES , ninvd0hRES ; 
   real weffcv_nf , qnsub_esi , qnsub_esi2 , ptovr0 , costi00 , nsti_p2;
   real vmax0  , cnstpgd , grg    , Rd0;
   real Rdtemp0 , Rdvdtemp0;  //HV1
   real Rd23e  ;              //HV1
   real Rs0    , Xmax   , kdep   , kjunc  ;
   real Rdrvmaxle , Rdrmuele , RdrmueVbs , grbpb  , grbps  , grbpd  , DDLTe  ;
   real vg2const , xvbs   , xgate  , xsub1  , xsub2  , UC_SUBLD1;
   real vg2const_, xvbs_  , xgate_  , xsub1_ , xsub2_ ;
   real cfrng  , cqyb0  , ptl0   , pt40   , gdl0   , rthtemp0;
   real Tratio , sqrt_eg , betatnom , mphn0  , ptovr  , VMAXeff;
   real Ninvde , powratio , wdpl   , wdplp  , cnst0over , cnst0overs;
   real NinvdecRES , NinvdehRES ; 
   real Rde    , Rse    , Rdvde  , Rsvde  ; //HV1
   real costi0_p2;
   real drainConductance , sourceConductance , RD_Ps0LD , RD_QbuLD ;
   real IdsIBPCe , qge     , qde     , qse     , qdp   ;
   real qsp    , qgexte  , qdexte  , qsexte  , Xd     , isube  ;
   real IsubLDe , noiflick , noithrml , noiigate , noicross , Rdde  ;
   real Rsde  ;
   real Carr1 , Carr2 ;
// Local Vars. for hsmhveval.c 
   integer m0     , mm     , i     ;
   real x2     , xmax2  , xp     , xmp    , dnm    , vbd    , Vbsi    ;
   real Vdsi    , vgd    , Vgsi    , deltemp , Vdsei   , Vgsei   , Vbsei   , Isubs  ;
   real IsubLDs , IdsIBPCs , GD     , Gth    , Qs     , Qdext  , Qgext  , Qbext  ;
   real Veffpower , P      , Qi_nqs , Qb_nqs , Iqi_nqs , Iqb_nqs , Qd_nqs , Qs_nqs ;
   real Qg_nqs , Qbulk  , cgdb   , cggb   , cgsb   , cbdb   , cbgb   , cbsb   ;
   real cddb   , cdgb   , cdsb   , KTNOM  , flg_qmetemp , RTH    , CTH    , CGSO_GIVEN ;
   real CGDO_GIVEN;
   real arg    , ggds   , ggdss  , Npexte , Lbin   , Wbin   , LWbin  , UC_GDLD ;
   real mMod   , gminMod ,TMF0   , TMF1   , TMF2   , TMF3   , TMF4   , Ldrifte ;
   real Rdrvmaxwe , RRDRMUE , RRDRVMAX , Rd_Xldld  , Vds    , Vgs    , Vbs    , Vdse   ;
   real RRDRMUES, RRDRVMAXS; // 20160518 source resistance
   real Vgse   , Vbse   , Vsubs  , Vddp   , Cqi    , Cqb    , Itemp  , gbbs   ;
   real Vsps ; // 20160516 source resistance effects
   real gbds   , gbgs   , whi_noise ;
   real gbdsd  , gbdss  , qdov   , qgov   , qsov   ;
   real cgdobe , cgsobe ;
   real Ibd_nqs , Qbd_qs , Qbd_nqs ;
`ifdef COEXTDIO
             /* for External Diode Model */
`else
   real UC_JS0D     , UC_JS0SWD   , UC_NJD      , UC_CISBKD   , UC_VDIFFJD  ;
   real UC_JS0S     , UC_JS0SWS   , UC_NJS      , UC_CISBKS   , UC_VDIFFJS  ;
   real js     , jssw   , js2    , jssw2  , czbd   , czbdsw , czbdswg , czbs   ;
   real jsswg, jsswg2 ;
   real czbssw , czbsswg , sarg  ;
   real pzbd   , pzbdsw , pzbdswg , pzbs , pzbssw , pzbsswg ;
   real isbd   , vbdt   , exptempd , jd_nvtm_invd , jd_expcd;
   real isbs   , vbst   , exptemps , jd_nvtm_invs , jd_expcs;
   real vsbs   , vdbd   , vbs_jct , vbd_jct , Ibd    , Ibs    , Capbs , Capbd  ;
   real vbpsp  , vbpdp  , vbsi_jct, vbdi_jct, Ibdi   , Ibsi   , Capbsi, Capbdi ;
   real isbd_btm, isbd2_btm, isbd_sws, isbd2_sws, isbd_swg, isbd2_swg ;
   real isbs_btm, isbs2_btm, isbs_sws, isbs2_sws, isbs_swg, isbs2_swg ;
   real Ibd_btm, Ibs_btm, Ibd_sws, Ibs_sws, Ibd_swg, Ibs_swg ;
   real Qbd_btm, Qbs_btm, Qbd_sws, Qbs_sws, Qbd_swg, Qbs_swg ;
   real Capbd_btm, Capbs_btm, Capbd_sws, Capbs_sws, Capbd_swg, Capbs_swg ;


//imported from DIODE_CMC_varlist2.include:
    //diode reverse recovery model
    // for unit conversion
    real NDI_i;
    real MUEN_i, MUEP_i;
    real NJL;
    // bias-dependent variables for the recovery model
    real Pn0, Nin_dio;
    real Dn, Dp, Da, La, tau_HL;
    real Vjunc_A, W_depA;
    real Q_n0, Q_nexA_nqs, Q_nexK_nqs, Qrr;
    //, Q_depA;
    real q_pexA, q_pexK, Q_qs_A, Q_qs_K, Q_nqs_A, Q_nqs_K;
    real Inqs0_A, Inqs0_K, q_pexA_nqs, q_pexK_nqs;
    real W_qs_A, W_nqs_A, IWnqs0_A, W_depA_nqs;
    real V_HA, V_HK;
    real nj_K;
    real exp_A, exp_K, p_nA, p_nK;
    real exp_A2, exp_K2;
    // fixed parameters
    real JUNCDLT; //Smoothness of depletion width around zero bias (minor) [V]
    //End of diode reverse recovery model


`endif
// Others
   integer flg_brk1, flg_brk2 ;
   integer end_of_part_1, start_of_loopl, start_of_mobility ;
   integer mode , FATAL_flag ;
   real ci, sid, sigrat, sigrat_s, sigrat_d ;

   real UC_RDRBB , UC_SUBTMP , xsubtmp ;
   real UC_RDRBB_s; // 20160518 source resistance


//* for depletion mode MOS *
   real Vbipn, Pb2n, kjuncpn ;
   integer UC_CODEP ;
   real UC_NDEPM, UC_DEPTHN ;
   real UC_DEPMUE0, UC_DEPMUEBACK0 ;
   real UC_DEPMUE1, UC_DEPMUEBACK1 ;
   real UC_DEPMUE2 , UC_DEPMUEPH1 ;
   real UC_DEPLEAK, UC_DEPVMAX ;
   real UC_DEPVDSEF1, UC_DEPVDSEF2 ;
   real UC_DEPWLP ;
   real depmphn0 ;
   real Qiu_noi, Qi_noi ;

   real Pdsz , Ws , C_W_LD , Ps0DEP ;
   real Mu_acc , Mu_res ;
   real Ids_acc , Ids_res , Ires_leak , Vds_res , W_res , Qn_res ;
   real Vdssat_res;
//* depletion mos end */

/* new pt */
   integer BYPASS_ENABLE ;
   real dphi_vds , phim , WK_JNPT_A ;//, Qn0npt ;
//`define dbgpt
`ifdef dbgpt
   real Vbspt , Ps0pt , Ecpt , Vgppt , TYpt , wkpt , Wdeppt ;
`endif

`ifdef _AGING_
// ** Aging model ***************//
`define pretime  1e-12
   integer fno;
   real pretime0 , ivbs , ivds , ivgs ;
   real TMF5, TMF6, TMF7, TMF8 ;
   real MKS_TRAPGC1 , MKS_TRAPGC2 , MKS_TRAPES1 , MKS_GC1MAX ;
   real MKS_TRAPGCLIM ;
   real MKS_TRAPGC0, delta, NtK30, y1, y2, y3, y4, y5, y6 ;
   real keT, NtK10, NtK20 , NtK40 ;
   real efn_ec ;
   real gc1 , e1 , Isub_trap , Ids_trap , abstime ;

   real nt0s , ntls ;
   real Nt0s_temp ;
   real dVth_bti , ts_bti , tcycle , Eox ;
   real ontime , Vgsonave , Vgpon ;
   real ftr0_qs, ftr0_qs_dPs0 ;
   real ftrl_qs, ftrl_qs_dPsl ;
   real exp_time , Nt_max0 , Nt_maxl ;
   real Ps0_prv , Vtraplx_prv ;
   real Qt0; 

   real DVddp , Vdseeff , AVddp ;
`endif
// eval_loverLim (2.4.0)
   real Vxb_lim ;

// limited exp function (lexp)
`define CEXP_THRES 80.0
`define CMAX_EXP 5.540622384e+34
`define CMIN_EXP 1.804851387e-35
analog function real lexp;
       input x;
       real x;
       begin
         if(x > `CEXP_THRES) begin
                 lexp = `CMAX_EXP * ( 1.0 + (x) -`CEXP_THRES);
         end else if(x < -`CEXP_THRES) begin
                 lexp = `CMIN_EXP;
         end else begin
                 lexp = exp(x);
         end
       end
endfunction

  //================ Start of executable code.=================//

   analog begin
     
`ifdef _4_PORTS_CONNECTED_
     if ( COSUBNODE == 1 ) begin
        $write(" *** Fatal(HiSIM_HV:%M): COSUBNODE = 1 but terminal 'sub' is not connected.\n" );
        $finish(0);
     end
`else
`ifdef _5_PORTS_CONNECTED_
     if ($port_connected(sub) == 0) begin // 4 nodes
       if ( COSUBNODE == 1 )  begin
          $write(" *** Fatal(HiSIM_HV:%M): 'sub' is not connected but COSUBNODE = 1.\n" );
          $finish(0);
        end
     end
`else
//   _6_PORTS_CONNECTED_
     if ($port_connected(temp) == 1) begin
        if ( COSUBNODE == 0 ) begin
          $write(" *** Fatal(HiSIM_HV:%M): 6 nodes are connected but COSUBNODE = 0.\n" );
          $finish(0);
        end
     end
     else begin
        if ($port_connected(sub) == 0) begin // 4 nodes
          if ( COSUBNODE == 1 )  begin
            $write(" *** Fatal(HiSIM_HV:%M): 'sub' is not connected but COSUBNODE = 1.\n" );
            $finish(0);
          end
        end
     end
`endif 
     if (COSUBNODE == 0) begin
        V(sub,temp) <+ 0.0;
     end
`endif
     
     // Initial Settings
     NSUBCDFM_GIVEN = ($param_given(NSUBCDFM));
     CGDO_GIVEN = ($param_given(CGDO));
     CGSO_GIVEN = ($param_given(CGSO));

     Cgdoe      = 0.0;
     Cgsoe      = 0.0;
     Xd         = 0.0;
     Rdd        = 0.0;
     Rsd        = 0.0;
     flg_ign    = 0;
     flg_noqi   = 0;
     flg_rsrd   = 0;
     flg_zone   = 0;
     RD_Ps0LD   = 0.0;
     RD_QbuLD   = 0.0;
     Vbs_max    = 0.8e0;
     Vbs_bnd    = 0.4e0;
     flg_pprv   = 0;
     flg_conv   = 0;
     flg_qme    = 0;
     flg_nqs    = 0;
     FATAL_flag = 0;
     Vbscl      = 0.0;
     VbscldVbs = 0.0;
     Vgp        = 0.0;
     Vgs_fb     = 0.0;
     Ps0        = 0.0;
     Ps0_ini    = 0.0;
     Ps0_iniA   = 0.0;
     Ps0_iniB   = 0.0;
     Psl        = 0.0;
     Psl_lim    = 0.0;
     dPlim      = 0.0;
     Pds        = 0.0;
     Pds_ini    = 0.0;
     Pds_max    = 0.0;
     lp_s0      = 0;
     lp_sl      = 0;
     Xi0        = 0.0;
     Xi0p12     = 0.0;
     Xi0p32     = 0.0;
     Xil        = 0.0;
     Xilp12     = 0.0;
     Xilp32     = 0.0;
     Vbsz       = 0.0;
     Vdsz       = 0.0;
     Vgsz       = 0.0;
     Vzadd      = 0.0;
     Ps0z       = 0.0;
     Pzadd      = 0.0;
     dVbsIBPC   = 0.0;
     dG3        = 0.0;
     dG4        = 0.0;
     dIdd       = 0.0;
     betaWL     = 0.0;
     Chi        = 0.0;
     Chib       = 0.0;
     Rho        = 0.0;
     Vth        = 0.0;
     Vth0       = 0.0;
     dVth       = 0.0;
     dVth0      = 0.0;
     dVthSC     = 0.0;
     Pb20b      = 0.0;
     dVthW      = 0.0;
     Alpha      = 0.0;
     Achi       = 0.0;
     VgVt       = 0.0;
     Pslsat     = 0.0;
     Vdsat      = 0.0;
     VdsatS     = 0.0;
     Delta      = 0.0;
     Qb         = 0.0;
     Qbu        = 0.0;
     Qi         = 0.0;
     Qiu        = 0.0;
     Qd         = 0.0;
     Ids        = 0.0;
     Ids0       = 0.0;
     dVthSCSTI  = 0.0;
     Vgssti     = 0.0;
     costi0     = 0.0;
     costi1     = 0.0;
     costi3     = 0.0;
     costi4     = 0.0;
     costi5     = 0.0;
     costi6     = 0.0;
     costi7     = 0.0;
     Psasti     = 0.0;
     Psbsti     = 0.0;
     Psab       = 0.0;
     Psti       = 0.0;
     sq1sti     = 0.0;
     sq2sti     = 0.0;
     Qn0sti     = 0.0;
     Idssti     = 0.0;
     beta       = 0.0;
     beta_inv   = 0.0;
     beta2      = 0.0;
     Pb2        = 0.0;
     Pb20       = 0.0;
     Pb2c       = 0.0;
     Vfb        = 0.0;
     c_eox      = 0.0;
     Leff       = 0.0;
     Weff       = 0.0;
     WeffLD_nf  = 0.0;
     Ldrift0    = 0.0;
     q_Nsub     = 0.0;
     Psa        = 0.0;
     Psdl       = 0.0;
     Lred       = 0.0;
     Lch        = 0.0;
     Wd         = 0.0;
     Aclm       = 0.0;
     Vthp       = 0.0;
     dVthLP     = 0.0;
     bs12       = 0.0;
     Qbmm       = 0.0;
     dqb        = 0.0;
     Vdx        = 0.0;
     Vdx2       = 0.0;
     Pbsum      = 0.0;
     sqrt_Pbsum = 0.0;
     dPpg       = 0.0;
     dTox       = 0.0;
     Cox        = 0.0;
     Cox_inv    = 0.0;
     Tox0       = 0.0;
     Cox0       = 0.0;
     Coxb0      = 0.0;
     Cox0_inv   = 0.0;
     Vthq       = 0.0;
     Psdlz      = 0.0;
     Egp12      = 0.0;
     Egp32      = 0.0;
     E1         = 0.0;
     Etun       = 0.0;
     Vdsp       = 0.0;
     Egidl      = 0.0;
     Egisl      = 0.0;
     Igate      = 0.0;
     Igs        = 0.0;
     Igd        = 0.0;
     Igb        = 0.0;
     Igidl      = 0.0;
     Igisl      = 0.0;
     Vdb        = 0.0;
     Vsb        = 0.0;
     FD2        = 0.0;
     FMDVDS     = 0.0;
     cnst0      = 0.0;
     cnst1      = 0.0;
     cnstCoxi   = 0.0;
     fac1       = 0.0;
     fac1p2     = 0.0;
     fs01       = 0.0;
     fs01_dPs0  = 0.0;
     fs02       = 0.0;
     fs02_dPs0  = 0.0;
     fsl1       = 0.0;
     fsl1_dPsl  = 0.0;
     fsl2       = 0.0;
     fsl2_dPsl  = 0.0;
     cfs1       = 0.0;
     fb         = 0.0;
     fb_dChi    = 0.0;
     fi         = 0.0;
     fi_dChi    = 0.0;
     exp_Chi    = 0.0;
     exp_Rho    = 0.0;
     exp_bVbs   = 0.0;
     exp_bVbsVds = 0.0;
     exp_bPs0   = 0.0;
     Fs0        = 0.0;
     Fs0_dPs0   = 0.0;
     Fsl        = 0.0;
     Fsl_dPsl   = 0.0;
     dPs0       = 0.0;
     dPsl       = 0.0;
     Qn0        = 0.0;
     Qb0        = 0.0;
     Qbnm       = 0.0;
     DtPds      = 0.0;
     Qinm       = 0.0;
     Qidn       = 0.0;
     Qdnm       = 0.0;
     Qddn       = 0.0;
     Quot       = 0.0;
     Qdrat      = 0.5;
     Idd        = 0.0;
     Idd1       = 0.0;
     Fdd        = 0.0;
     Eeff       = 0.0;
     Rns        = 0.0;
     Mu         = 0.0;
     Muun       = 0.0;
     Ey         = 0.0;
     Em         = 0.0;
     Eta        = 0.0;
     Eta1       = 0.0;
     Eta1p12    = 0.0;
     Eta1p32    = 0.0;
     Eta1p52    = 0.0;
     Zeta12     = 0.0;
     Zeta32     = 0.0;
     Zeta52     = 0.0;
     F00        = 0.0;
     F10        = 0.0;
     F30        = 0.0;
     F11        = 0.0;
     Vgs_min    = 0.0;
     Ps0_min    = 0.0;
     Acn        = 0.0;
     Acd        = 0.0;
     Ac1        = 0.0;
     Ac2        = 0.0;
     Ac3        = 0.0;
     Ac4        = 0.0;
     Ac31       = 0.0;
     Ac41       = 0.0;
     Isub       = 0.0;
     IsubLD     = 0.0;
     Psislsat   = 0.0;
     Psisubsat  = 0.0;
     Ifn        = 0.0;
     Eg12       = 0.0;
     Eg32       = 0.0;
     cov_slp    = 0.0;
     cov_mag    = 0.0;
     Qgos       = 0.0;
     Qgod       = 0.0;
     Qgbo       = 0.0;
     Cgbo_loc   = 0.0;
     Qgso       = 0.0;
     Qgdo       = 0.0;
     Qfd        = 0.0;
     Cfd        = 0.0;
     Qfs        = 0.0;
     Cfs        = 0.0;
     Ec         = 0.0;
     Pslk       = 0.0;
     Qy         = 0.0;
     tau        = 0.0;
     taub       = 0.0;
     Nflic      = 0.0;
     Eyd        = 0.0;
     Mu_Ave     = 0.0;
     Nthrml     = 0.0;
     Mud_hoso   = 0.0;
     kusai00    = 0.0;
     kusaidd    = 0.0;
     kusaiL     = 0.0;
     kusai00L   = 0.0;
     sqrtkusaiL = 0.0;
     kusai_ig   = 0.0;
     gds0_ign   = 0.0;
     gds0_h2    = 0.0;
     GAMMA      = 0.0;
     crl_f      = 0.0;
     Nign0      = 0.0;
     MuModA     = 0.0;
     MuModB     = 0.0;
     correct_w1 = 0.0;
     TX         = 0.0;
     TY         = 0.0;
     T0         = 0.0;
     T1         = 0.0;
     T2         = 0.0;
     T3         = 0.0;
     T4         = 0.0;
     T5         = 0.0;
     T6         = 0.0;
     T7         = 0.0;
     T8         = 0.0;
     T9         = 0.0;
     T10        = 0.0;
     T11        = 0.0;
     T12        = 0.0;
     Vdseff     = 0.0;
     Vdsorg     = 0.0;
     Qovdext    = 0.0;
     Qovsext    = 0.0;
     Qovd       = 0.0;
     Qovs       = 0.0;
     QbuLD      = 0.0;
     QbdLD      = 0.0;
     QbsLD      = 0.0;
     QodAD      = 0.0;
     QbdLDext   = 0.0;
     QbsLDext   = 0.0;
     Vbsz2      = 0.0;
     Rdrift     = 0.0; //HV1 
     Rsdrift    = 0.0; //HV1
     Ra         = 0.0; //HV1
     Vdse_eff   = 0.0; //HV1
     VdseModeNML = 0.0;//HV1
     VdseModeRVS = 0.0;//HV1
     Vbsegmt    = 0.0; //HV1
     Vdsegmt    = 0.0;
     Vgsegmt    = 0.0; //HV1
     Vbserev    = 0.0; //HV1
     Vdserev    = 0.0; //HV1
     Vgserev    = 0.0; //HV1
     Vdserevz   = 0.0;
     Vgserevz   = 0.0; //HV1
     Vbserevz   = 0.0; //HV1
     Vsubsrev   = 0.0;
     TTEMP      = 0.0;
     TTEMP0     = 0.0;
     Tdiff0     = 0.0;
     Tdiff0_2   = 0.0;
     Tdiff      = 0.0;
     Tdiff_2    = 0.0;
     Eg         = 0.0;
     Nin        = 0.0;
     Vgbgmt     = 0.0;
     Vxbgmt     = 0.0;
     Vxbgmtcl   = 0.0;
     QsuLD      = 0.0;
     QiuLD      = 0.0;
     IdsIBPC    = 0.0;
     flg_ovzone = 0;
     VgpLD      = 0.0;
     Vgb_fb_LD  = 0.0;
     Ps0LD      = 0.0;
     cnst1over  = 0.0;
     ddriftld   = `DDRIFT ;
     ddriftldc  = `DDRIFT ;
     Nover_func = 0.0;
     cnst0over_func = 0.0;
     Ta         = 9.3868e-3;
     Tb         = -0.1047839;
     Chi_1      = 0.0;
     mueph      = 0.0;
     dL         = 0.0;
     dLLD       = 0.0;
     LG         = 0.0;
     dW         = 0.0;
     dWLD       = 0.0;
     dWCV       = 0.0;
     WG         = 0.0;
     WLG        = 0.0;
     Lgate      = 0.0;
     Wgate      = 0.0;
     Nsubpp     = 0.0;
     Nsubps     = 0.0;
     Nsub       = 0.0;
     Nsubb      = 0.0;
     Lod_half   = 0.0;
     Lod_half_ref = 0.0;
     log_Tratio = 0.0;
     Edri       = 0.0;
     Vdri       = 0.0;
     Mu0        = 0.0;
     Cx         = 0.0;
     Car        = 0.0;
     Xov        = 0.0;
     Carr       = 0.0;
     GD         = 0.0;
     Vddpz      = 0.0;
     arg        = 0.0;
     vbd        = 0.0;
     Vbsi        = 0.0;
     Vdsi        = 0.0;
     vgd        = 0.0;
     Vgsi        = 0.0;
     deltemp    = 0.0;
     Vdsei       = 0.0;
     Vgsei       = 0.0;
     Vbsei       = 0.0;
     Isubs      = 0.0;
     IsubLDs    = 0.0;
     IdsIBPCs   = 0.0;
     Gth        = 0.0;
     Qg         = 0.0;
     Qs         = 0.0;
     Veffpower  = 0.0;
     P          = 0.0;
     Qi_nqs     = 0.0;
     Qb_nqs     = 0.0;
     Iqi_nqs    = 0.0;
     Iqb_nqs    = 0.0;
     Qd_nqs     = 0.0;
     Qs_nqs     = 0.0;
     Qg_nqs     = 0.0;
     Qbulk      = 0.0;
     cgdb       = 0.0;
     cggb       = 0.0;
     cgsb       = 0.0;
     cbdb       = 0.0;
     cbgb       = 0.0;
     cbsb       = 0.0;
     cddb       = 0.0;
     cdgb       = 0.0;
     cdsb       = 0.0;
     Von        = 0.0;
     Ninvde     = 0.0;
     NinvdecRES = 0.0; // CODEP=3 
     NinvdehRES = 0.0; // CODEP=3 
     RRDRMUE    = 0.0;
     RRDRMUES   = 0.0;
     RRDRVMAX   = 0.0;
     Rde        = 0.0; //HV1
     Rdvde      = 0.0; //HV1
     Rse        = 0.0; //HV1
     Rsvde      = 0.0; //HV1
     RRDRVMAXS  = 0.0;
     Tratio     = 0.0;
     VMAXeff    = 0.0;
     betatnom   = 0.0;
     cnst0over  = 0.0;
     cnst0overs = 0.0;
     costi0_p2  = 0.0;
     mphn0      = 0.0;
     powratio   = 0.0;
     ptovr      = 0.0;
     sqrt_eg    = 0.0;
     wdpl       = 0.0;
     wdplp      = 0.0;
     UC_RDRBB   = RDRBB ;
     UC_RDRBB_s   = RDRBBS ;
     Ids_acc    = 0.0 ;
     Ids_res    = 0.0 ;
     Ires_leak  = 0.0 ;
     Pb2n       = 0.0 ;
     Vbipn      = 0.0 ;
     HBDCeff    = HBDC ;
     UC_SUBTMP  = SUBTMP ;
     depmphn0   = 0.0 ;
     Qiu_noi    = 0.0 ;
     lp_s0_max  = `lp_s0_max ;

`ifdef COEXTDIO 
       /* for Junction Diode Model */
`else 
     js         = 0.0;
     jssw       = 0.0;
     js2        = 0.0;
     jssw2      = 0.0;
     Ibs        = 0.0;
     Gbs        = 0.0;
     Ibd        = 0.0;
     Gbd        = 0.0;
     Ibsi       = 0.0;
     Ibdi       = 0.0;
     Qbs        = 0.0;
     Qbd        = 0.0;
     Qbsi       = 0.0;
     Qbdi       = 0.0;
     czbd       = 0.0;
     czbdsw     = 0.0;
     czbdswg    = 0.0;
     czbs       = 0.0;
     czbssw     = 0.0;
     czbsswg    = 0.0;
     pzbd       = 0.0;
     pzbdsw     = 0.0;
     pzbdswg    = 0.0;
     pzbs       = 0.0;
     pzbssw     = 0.0;
     pzbsswg    = 0.0;
     sarg       = 0.0;
     vsbs       = 0.0;
     vdbd       = 0.0;
     vbs_jct    = 0.0;
     vbd_jct    = 0.0;
     vbpsp      = 0.0;
     vbpdp      = 0.0;
     vbsi_jct   = 0.0;
     vbdi_jct   = 0.0;
     Capbd      = 0.0;
     Capbs      = 0.0;
     exptempd   = 0.0;
     exptemps   = 0.0;
     isbd       = 0.0;
     isbs       = 0.0;
     jd_expcd   = 0.0;
     jd_expcs   = 0.0;
     vbdt       = 0.0;
     vbst       = 0.0;
     jd_nvtm_invd  = 0.0;
     jd_nvtm_invs  = 0.0;
`endif /* End of COEXTDIO */

     end_of_part_1 = 0;
     flg_brk1   = 0;
     start_of_loopl = 0;
     flg_brk2   = 0;
     start_of_mobility = 0;

     //*  initialize for hidden state
     RS_Ps0LD    = 0.0 ;
     RS_QbuLD    = 0.0 ;
     Qbd_qs      = 0.0 ;
     Ibd_nqs     = 0.0 ;
     isbd_btm    = 0.0 ;
     isbd2_btm   = 0.0 ;
     isbd_sws    = 0.0 ;
     isbd2_sws   = 0.0 ;
     isbd_swg    = 0.0 ;
     isbd2_swg   = 0.0 ;
     isbs_btm    = 0.0 ;
     isbs2_btm   = 0.0 ;
     isbs_sws    = 0.0 ;
     isbs2_sws   = 0.0 ;
     isbs_swg    = 0.0 ;
     isbs2_swg   = 0.0 ;
     Qovd_ADD    = 0.0 ;
     Qovs_ADD    = 0.0 ;
     QbdLD_ADD   = 0.0 ;
     QbsLD_ADD   = 0.0 ;
     Wjuncld     = 0.0 ;

     IdsPT       = 0.0 ;
     IdsPT0      = 0.0 ;
     IdsPT1      = 0.0 ;

     BYPASS_ENABLE = 1 ; // flag for VgVt_Small (If 0, Chi can get 0 in zone D1. Clipping Ps0 & Psl can result in discontinuous C-V curves around Vds=0V.)
     Cox0_func   = 0.0 ;
     IWnqs0_A    = 0.0 ;
     Inqs0_A     = 0.0 ;
     Inqs0_K     = 0.0 ;
     IsubIBPC    = 0.0 ;
     Lover_func  = 0.0 ;
     Q_nqs_A     = 0.0 ;
     Q_nqs_K     = 0.0 ;
     W_nqs_A     = 0.0 ;
     W_res       = 0.0 ;
     Wdep_func   = 0.0 ;
     wIbjt       = 0.0 ;
     wk_ii       = 0.0 ;
     tauov       = 0.0 ;

     OP_mode     = 0.0 ;
     OP_Vdsi     = 0.0 ;
     OP_Vbscl    = 0.0 ;
     begin : initializeModel


       UC_CORSRD  = ((CORDRIFT) ? 0 : CORSRD); //HV1
                  
       UC_XPDV    = XPDV;
       UC_XLDLD   = XLDLD;
       UC_SCP22   = SCP22;
       UC_RDRCX   = RDRCX;


       `GMIN          // 
       Gjmin = gminMod ;
       `MFACTOR_DEC   // 
       Mfactor = `MFACTOR_USE ;
  

       if( Gjmin > 0.0 ) begin
         Gjmin = 0.0 ;
       end

       //-----------------------------------------------------------*
       //* Range check of model parameters
       //*-----------------//
       if(COERRREP) begin
         if( TOX <= 0 ) begin
           $write("warning(HiSIM_HV(%m)): TOX = %e\n ", TOX);
           $write("warning(HiSIM_HV(%m)): The model parameter TOX must be positive.\n");
         end
       end
       `RANGERESET(UC_SCP22,      0.0,      0.0, "SCP22")
       `MINRESET(UC_XLDLD  ,      0.0,           "XLDLD")
       `RANGERESET(UC_RDRCX,      0.0,      1.0, "RDRCX" )

       if(COERRREP) begin
         if( SHEMAX  < 300   || SHEMAX  > 900 ) $write("warning(HiSIM_HV(%m)): (SHEMAX = %g) range [300 , 900].\n",SHEMAX);
         if( XLD     < 0.0   || XLD     > 50.0e-9 ) $write("warning(HiSIM_HV(%m)): (XLD = %g) range [0.0 , 50.0e-9].\n",XLD);
         if( XWD     < -100.0e-9 || XWD     > 300.0e-9 ) $write("warning(HiSIM_HV(%m)): (XWD = %g) range [-100.0e-9 , 300.0e-9].\n",XWD);
         if( XWDC    < -500.0e-9 || XWDC    > 500.0e-9 ) $write("warning(HiSIM_HV(%m)): (XWDC = %g) range [-500.0e-9 , 500.0e-9].\n",XWDC);
         if( RSH     < 0.0   || RSH     > 500.0 ) $write("warning(HiSIM_HV(%m)): (RSH = %g) range [0.0 , 500.0].\n",RSH);
         if( RSHS     < 0.0   || RSHS     > 500.0 ) $write("warning(HiSIM_HV(%m)): (RSHS = %g) range [0.0 , 500.0].\n",RSHS);
         if( RSHG    < 0.0   || RSHG    > 100.0 ) $write("warning(HiSIM_HV(%m)): (RSHG = %g) range [0.0 , 100.0].\n",RSHG);
         if(XQY != 0.0) `RANGECHECK(XQY,        10.0e-9,  50.0e-9, "XQY") 
         `MINCHECK(XQY1,       0.0,           "XQY1") 
         `MINCHECK(XQY2,       0.0,           "XQY2") 
         if( VBI     < 1.0   || VBI     > 1.2  )  $write("warning(HiSIM_HV(%m)): (VBI = %g) range [1.0 , 1.2].\n",VBI);
         if( PARL2   < 0.0   || PARL2   > 50.0e-9 ) $write("warning(HiSIM_HV(%m)): (PARL2 = %g) range [0.0 , 50.0e-9].\n",PARL2);
         if( LP      < 0.0   || LP      > 300.0e-9 ) $write("warning(HiSIM_HV(%m)): (LP = %g) range [0.0 , 300.0e-9].\n",LP);
         if( PGD2    < 0.0   || PGD2    > 1.5  ) $write("warning(HiSIM_HV(%m)): (PGD2 = %g) range [0.0 , 1.5].\n",PGD2);
         if( PGD4    < 0.0   || PGD4    > 3.0 ) $write("warning(HiSIM_HV(%m)): (PGD4 = %g) range [0.0 , 3.0].\n",PGD4);
         if( MUEPH0  < 0.25  || MUEPH0  > 0.35 ) $write("warning(HiSIM_HV(%m)): (MUEPH0 = %g) range [0.25 , 0.35].\n",MUEPH0);
         if( MUESR0  < 1.8   || MUESR0  > 2.2  ) $write("warning(HiSIM_HV(%m)): (MUESR0 = %g) range [1.8 , 2.2].\n",MUESR0);
         if( LPEXT   < 1.0e-50 || LPEXT   > 10.0e-6 ) $write("warning(HiSIM_HV(%m)): (LPEXT = %g) range [1.0e-50 , 10.0e-6].\n",LPEXT);
         `MINCHECK(SC4,        0.0,           "SC4") 
         if( SCP21   < 0.0   || SCP21   > 5.0 ) $write("warning(HiSIM_HV(%m)): (SCP21 = %g) range [0.0 , 5.0].\n",SCP21);
         if( BS1     < 0.0   || BS1     > 50.0e-3 ) $write("warning(HiSIM_HV(%m)): (BS1 = %g) range [0.0 , 50.0e-3].\n",BS1);
         if( BS2     < 0.5   || BS2     > 1.0  ) $write("warning(HiSIM_HV(%m)): (BS2 = %g) range [0.5 , 1.0].\n",BS2);
         `MINCHECK(PTL,        0.0,           "PTL") 
         if( PTP     < 3.0   || PTP     > 4.0 ) $write("warning(HiSIM_HV(%m)): (PTP = %g) range [3.0 , 4.0].\n",PTP);
         `MINCHECK(PT2,        0.0,           "PT2") 
         `MINCHECK(PT4,        0.0,           "PT4") 
         `MINCHECK(PT4P,       0.0,           "PT4P") 
         if( GDL     < 0.0   || GDL     > 220e-9 ) $write("warning(HiSIM_HV(%m)): (GDL = %g) range [0.0 , 220e-9].\n",GDL);
         `MINCHECK(NINVD,      0.0,           "NINVD") 
         `MINCHECK(NINVDW,     0.0,           "NINVDW") 
         `MINCHECK(NINVDWP,    0.0,           "NINVDWP") 
         if( CLM5    < 0.0   || CLM5    > 2.0 ) $write("warning(HiSIM_HV(%m)): (CLM5 = %g) range [0.0 , 2.0].\n",CLM5);
         if( CLM6    < 0.0   || CLM6    > 20.0 ) $write("warning(HiSIM_HV(%m)): (CLM6 = %g) range [0.0 , 20.0].\n",CLM6);
         if( SUB2L   < 0.0   || SUB2L   > 1.0 ) $write("warning(HiSIM_HV(%m)): (SUB2L = %g) range [0.0 , 1.0].\n",SUB2L);
         if( VOVERP  < 0.0   || VOVERP  > 2.0 ) $write("warning(HiSIM_HV(%m)): (VOVERP = %g) range [0.0 , 2.0].\n",VOVERP);
         if( QME1    < 0.0   || QME1    > 1e-9 ) $write("warning(HiSIM_HV(%m)): (QME1 = %g) range [0.0 , 1e-9].\n",QME1);
         if( QME2    < 1.0   || QME2    > 3.0 ) $write("warning(HiSIM_HV(%m)): (QME2 = %g) range [1.0 , 3.0].\n",QME2);
         if( QME3    < 0.0   || QME3    > 500e-12 ) $write("warning(HiSIM_HV(%m)): (QME3 = %g) range [0.0 , 500e-12].\n",QME3);
         if( GLPART1 < 0.0   || GLPART1 > 1.0 ) $write("warning(HiSIM_HV(%m)): (GLPART1 = %g) range [0.0 , 1.0].\n",GLPART1);
         if( TNOM    < 22.0  || TNOM    > 32.0 ) $write("warning(HiSIM_HV(%m)): (TNOM = %g) range [22.0 , 32.0].\n",TNOM);
         if( DDLTMAX < 1.0   || DDLTMAX > 10.0 ) $write("warning(HiSIM_HV(%m)): (DDLTMAX = %g) range [1.0 , 10.0].\n",DDLTMAX);
         if( DDLTICT < -3.0  || DDLTICT > 20.0 ) $write("warning(HiSIM_HV(%m)): (DDLTICT = %g) range [-3.0 , 20.0].\n",DDLTICT);
         if( DDLTSLP < 0.0   || DDLTSLP > 20.0 ) $write("warning(HiSIM_HV(%m)): (DDLTSLP = %g) range [0.0 , 20.0].\n",DDLTSLP);
         if( MPHDFM  < -3.0  || MPHDFM  > 3.0 ) $write("warning(HiSIM_HV(%m)): (MPHDFM = %g) range [-3.0 , 3.0].\n",MPHDFM);
         if( RD20    < 0.0   || RD20    > 30.0 ) $write("warning(HiSIM_HV(%m)): (RD20 = %g) range [0.0 , 30.0].\n",RD20);     //HV1
         if( RD21    < 0.0   || RD21    > 1.0  ) $write("warning(HiSIM_HV(%m)): (RD21 = %g) range [0.0 , 1.0].\n",RD21);      //HV1
         if( RD22D   < 0.0   || RD22D   > 2.0 ) $write("warning(HiSIM_HV(%m)): (RD22D = %g) range [0.0 , 2.0].\n",RD22D);     //HV1
         `MINCHECK(  RD25,       0.0,           "RD25") 
         if( RDTEMP1 < -1e-1 || RDTEMP1 > 2e0 ) $write("warning(HiSIM_HV(%m)): (RDTEMP1 = %g) range [-0.1 , 2].\n",RDTEMP1);
         if( RDTEMP2 < -1e-3 || RDTEMP2 > 1e-3 ) $write("warning(HiSIM_HV(%m)): (RDTEMP2 = %g) range [-1e-3 , 1e-3].\n",RDTEMP2);
         if( RDVDTEMP1 < -1e-1 || RDVDTEMP1 > 1e0 ) $write("warning(HiSIM_HV(%m)): (RDVDTEMP1 = %g) range [-0.1 , 1].\n",RDVDTEMP1);
         if( RDVDTEMP2 < -1e-3 || RDVDTEMP2 > 1e-3 ) $write("warning(HiSIM_HV(%m)): (RDVDTEMP2 = %g) range [-1e-3 , 1e-3].\n",RDVDTEMP2);
         `MINCHECK(  RDVG12,     0.0,           "RDVG12") 
         if( RTHTEMP1 < -1.0  || RTHTEMP1 > 1.0 ) $write("warning(HiSIM_HV(%m)): (RTHTEMP1 = %g) range [-1.0 , 1.0].\n",RTHTEMP1);
         if( RTHTEMP2 < -1.0  || RTHTEMP2 > 1.0 ) $write("warning(HiSIM_HV(%m)): (RTHTEMP2 = %g) range [-1.0 , 1.0].\n",RTHTEMP2);
         if( RTH0W   < -100  || RTH0W   > 100 ) $write("warning(HiSIM_HV(%m)): (RTH0W  = %g) range [-100 , 100].\n",RTH0W);
         if( RTH0WP  < -10   || RTH0WP  > 10  ) $write("warning(HiSIM_HV(%m)): (RTH0WP = %g) range [ -10 ,  10].\n",RTH0WP);
         if( RTH0L   < -100  || RTH0L   > 100 ) $write("warning(HiSIM_HV(%m)): (RTH0L  = %g) range [-100 , 100].\n",RTH0L);
         if( RTH0LP  < -10   || RTH0LP  > 10  ) $write("warning(HiSIM_HV(%m)): (RTH0LP = %g) range [ -10 ,  10].\n",RTH0LP);
         if( RTH0NF  < -5.0  || RTH0NF  > 5.0 ) $write("warning(HiSIM_HV(%m)): (RTH0NF = %g) range [-5.0 , 5.0].\n",RTH0NF);
         if( PRATTEMP1 < -1.0  || PRATTEMP1 > 1.0 ) $write("warning(HiSIM_HV(%m)): (PRATTEMP1 = %g) range [-1.0 , 1.0].\n",PRATTEMP1);
         if( PRATTEMP2 < -1.0  || PRATTEMP2 > 1.0 ) $write("warning(HiSIM_HV(%m)): (PRATTEMP2 = %g) range [-1.0 , 1.0].\n",PRATTEMP2);
         `MINCHECK(  LDRIFT1,    0.0,           "LDRIFT1") 
         `MINCHECK(  LDRIFT1S,   0.0,           "LDRIFT1S") 
         `MINCHECK(  LDRIFT2,    0.0,           "LDRIFT2") 
         `MINCHECK(  LDRIFT2S,   0.0,           "LDRIFT2S") 
         if( RDS     < -100  || RDS     > 100 ) $write("warning(HiSIM_HV(%m)): (RDS = %g) range [-100 , 100].\n",RDS);           //HV1
         if( RDSP    < -10   || RDSP    > 10 ) $write("warning(HiSIM_HV(%m)): (RDSP = %g) range [-10 , 10].\n",RDSP);            //HV1
         if( RDVDL   < -100  || RDVDL   > 100 ) $write("warning(HiSIM_HV(%m)): (RDVDL = %g) range [-100 , 100].\n",RDVDL);       //HV1
         if( RDVDLP  < -10   || RDVDLP  > 10 ) $write("warning(HiSIM_HV(%m)): (RDVDLP = %g) range [-10 , 10].\n",RDVDLP);        //HV1
         if( RDVDS   < -100  || RDVDS   > 100 ) $write("warning(HiSIM_HV(%m)): (RDVDS = %g) range [-100 , 100].\n",RDVDS);       //HV1
         if( RDVDSP  < -10   || RDVDSP  > 10 ) $write("warning(HiSIM_HV(%m)): (RDVDSP = %g) range [-10 , 10].\n",RDVDSP);        //HV1
         if( RD23L   < -100  || RD23L   > 100 ) $write("warning(HiSIM_HV(%m)): (RD23L = %g) range [-100 , 100].\n",RD23L);       //HV1
         if( RD23LP  < -10   || RD23LP  > 10 ) $write("warning(HiSIM_HV(%m)): (RD23LP = %g) range [-10 , 10].\n",RD23LP);        //HV1
         if( RD23S   < -100  || RD23S   > 100 ) $write("warning(HiSIM_HV(%m)): (RD23S = %g) range [-100 , 100].\n",RD23S);       //HV1
         if( RD23SP  < -10   || RD23SP  > 10 ) $write("warning(HiSIM_HV(%m)): (RD23SP = %g) range [-10 , 10].\n",RD23SP);        //HV1
         if( RDOV11  < 0.0   || RDOV11  > 10 ) $write("warning(HiSIM_HV(%m)): (RDOV11 = %g) range [0.0 , 10].\n",RDOV11);        //HV1
         if( RDOV12  < 0.0   || RDOV12  > 2.0 ) $write("warning(HiSIM_HV(%m)): (RDOV12 = %g) range [0.0 , 2.0].\n",RDOV12);      //HV1
         if( RDSLP2  < -10.0 || RDSLP2  > 10.0 ) $write("warning(HiSIM_HV(%m)): (RDSLP2 = %g) range [-10.0 , 10.0].\n",RDSLP2);  //HV1
         if( RDICT2  < -10.0 || RDICT2  > 10.0 ) $write("warning(HiSIM_HV(%m)): (RDICT2 = %g) range [-10.0 , 10.0].\n",RDICT2);  //HV1 
         if( RDRVMAX < 1e6   || RDRVMAX > 100e6 ) $write("warning(HiSIM_HV(%m)): (RDRVMAX = %g) range [1e6 , 100e6].\n",RDRVMAX);
         if( RDRMUE  < 1e2   || RDRMUE  > 3e3  ) $write("warning(HiSIM_HV(%m)): (RDRMUE = %g) range [1e2 , 3e3].\n",RDRMUE);
         if( RDRQOVER < 0.0  || RDRQOVER > 1e7 ) $write("warning(HiSIM_HV(%m)): (RDRQOVER = %g) range [0.0 , 1e7].\n",RDRQOVER);

         if( RDRCAR  < 0.0   || RDRCAR  > 50e-9 ) $write("warning(HiSIM_HV(%m)): (RDRCAR = %g) range [0.0 , 50e-9].\n",RDRCAR);
         if( RDRMUETMP < 0.0   || RDRMUETMP > 2.0 ) $write("warning(HiSIM_HV(%m)): (RDRMUETMP = %g) range [0.0 , 2.0].\n",RDRMUETMP);
         if( RDRVTMP < -2.0  || RDRVTMP > 1.0 ) $write("warning(HiSIM_HV(%m)): (RDRVTMP = %g) range [-2.0 , 1.0].\n",RDRVTMP);
         `MINCHECK(  UC_XPDV,       0.0,           "XPDV"      ) 
         `MINCHECK(  XPVDTH,     0.0,           "XPVDTH"    ) 
         if( XPVDTHG < -1.0  || XPVDTHG > 1.0  ) $write("warning(HiSIM_HV(%m)): (XPVDTHG = %g) range [-1.0 , 1.0].\n",XPVDTHG);
         `MINCHECK(  IBPC1L,     0.0,           "IBPC1L"    ) 
 
`ifdef COEXTDIO 
       /* for Junction Diode Model */
`else 
         if( CVB     < -0.1  || CVB     > 0.2 ) $write("warning(HiSIM_HV(%m)): (CVB = %g) range [-0.1 , 0.2].\n",CVB);
         if( CVBD    < -0.1  || CVBD    > 0.2 ) $write("warning(HiSIM_HV(%m)): (CVBD = %g) range [-0.1 , 0.2].\n",CVBD);
         if( CVBS    < -0.1  || CVBS    > 0.2 ) $write("warning(HiSIM_HV(%m)): (CVBS = %g) range [-0.1 , 0.2].\n",CVBS);
`endif

       end
       
       UC_NDEPM = NDEPM ;
       UC_DEPTHN = TNDEP ;
       UC_CODEP = CODEP ;
       UC_DEPMUEBACK0 = DEPMUEBACK0 ;
       UC_DEPMUEBACK1 = DEPMUEBACK1 ;
       UC_DEPMUE0 = DEPMUE0 ;
       UC_DEPMUE1 = DEPMUE1 ;
       UC_DEPMUE2 = DEPMUE2 ;
       UC_DEPLEAK = DEPLEAK ;
       UC_DEPVMAX = DEPVMAX ;
       UC_DEPWLP  = DEPWLP  ; // protection against hidden state
       UC_DEPMUEPH1 = DEPMUEPH1 ;
       UC_DEPVDSEF1 = DEPVDSEF1 ;
       UC_DEPVDSEF2 = DEPVDSEF2 ;

       //
       //if( UC_CODEP != 0 )  begin
       if( UC_CODEP < 3 && UC_CODEP > 0 )  begin //{
         `RANGERESET(UC_NDEPM,      5e15,      1e18,   "NDEPM" )
         `RANGERESET(UC_DEPTHN,     1e-8,      1.0e-6, "TNDEP" )
         `RANGERESET(UC_DEPMUE0,       1,      1.0e5,  "DEPMUE0" )
         `RANGERESET(UC_DEPMUEBACK0,   1,      1.0e5,  "DEPMUEBACK0" )
         `RANGERESET(UC_DEPMUEPH1,     1,      1.0e5,  "DEPMUEPH1" )
         `RANGERESET(UC_DEPVDSEF2,   0.1,          4,  "DEPVDSEF2" )
         `RANGERESET(UC_DEPLEAK,     0.0,          5,  "DEPLEAK" )
       end //}
       else if ( UC_CODEP == 3 ) begin //{
         `RANGERESET(UC_NDEPM,      5e15,      1e18,   "NDEPM" )
         `RANGERESET(UC_DEPTHN,     1e-8,      1e-6,   "TNDEP" )
         `RANGERESET(UC_DEPMUE0,       1,      1e10,   "DEPMUE0" )
         //`RANGERESET(UC_DEPMUEPH1,   100,      2.0e4,  "DEPMUEPH1" ) // original
         //`RANGERESET(UC_DEPMUEPH1,   100,      2.0e6,  "DEPMUEPH1" )
         `RANGERESET(UC_DEPMUEPH1,   100,      2.0e9,  "DEPMUEPH1" )
         `RANGERESET(UC_DEPLEAK,     0.0,          5,  "DEPLEAK" ) 
       end //}

       if( UC_CODEP != 0 && COERRREP ) begin

         if( COISTI == 1 ) begin
           $write("warning(HiSIM_HV(%m)): STI leak model is not supported yet in Depletion mode model, skipped\n");
         end
         if( COTHRML == 1 ) begin
           $write("warning(HiSIM_HV(%m)): Thermal noise model is not supported yet in Depletion mode model, skipped\n");
         end
         if( COIGN == 1 ) begin
           $write("warning(HiSIM_HV(%m)): Induced gate noise model is not supported yet in Depletion mode model, skipped\n");
         end

       end

       if( UC_XPDV * UC_XLDLD > 1 && COERRREP ) begin
         $write("warning(HiSIM_HV(%m)): The model parameter XPDV (= %e) must be Smaller than 1/XLDLD (= %e).\n", 
                 UC_XPDV, 1/UC_XLDLD ); 
         $write("warning(HiSIM_HV(%m)): The model parameter XPDV (= %e) has been changed to %e.\n", 
                 UC_XPDV, 1/UC_XLDLD );                 
       end

       if( COPPRV == 1 ) begin
         $write("warning(HiSIM_HV(%m)): COPPRV is not supported yet in VA code, reset to 0.\n");
       end                    

       UC_TOXB = TOXB ;
       `RANGERESET(UC_TOXB,      TOX,       5e-7, "TOXB")

       //-----------------------------------------------------------*
       //* Change units into MKS.
       //*-----------------//
       MKS_LL        = LL        / pow( `C_m2cm , LLN ) ;
       MKS_WL        = WL        / pow( `C_m2cm , WLN ) ;
       MKS_SVGSL     = SVGSL     / pow( `C_m2cm , SVGSLP ) ;
       MKS_SVGSW     = SVGSW     / pow( `C_m2cm , SVGSWP ) ;
       MKS_SVBSL     = SVBSL     / pow( `C_m2cm , SVBSLP ) ;
       MKS_SLGL      = SLGL      / pow( `C_m2cm , SLGLP ) ;
       MKS_SUB1L     = SUB1L     / pow( `C_m2cm , SUB1LP ) ;
       MKS_SLG       = SLG       / `C_m2cm ;
       MKS_SUB2L     = SUB2L     / `C_m2cm ;
       MKS_SUBLD2    = SUBLD2    * `C_m2cm ;
       MKS_RDTEMP1   = RDTEMP1   / `C_m2cm ;     //HV1
       MKS_RDTEMP2   = RDTEMP2   / `C_m2cm ;     //HV1
       MKS_RDVDTEMP1 = RDVDTEMP1 / `C_m2cm ;     //HV1
       MKS_RDVDTEMP2 = RDVDTEMP2 / `C_m2cm ;     //HV1  
       MKS_NSUBSUB   = NSUBSUB   / `C_cm2m_p3 ;
       MKS_GLKSD3    = GLKSD3    * `C_m2cm ;
       MKS_GLEAK4    = GLEAK4    * `C_m2cm ;
       MKS_GLEAK5    = GLEAK5    * `C_m2cm ;
       MKS_GLEAK7    = GLEAK7    / `C_m2cm_p2 ;
       MKS_CIT       = CIT       * `C_m2cm_p2 ;
       MKS_OVSLP     = OVSLP     / `C_m2cm ;
       MKS_DLY3      = DLY3      / `C_m2cm_p2 ;
       MKS_DLYOV     = DLYOV     / `C_m2cm_p2 ;
       MKS_CTH0      = CTH0      * `C_m2cm ;
       MKS_RDRMUE    = RDRMUE    / `C_m2cm_p2 ;
       MKS_RDRVMAX   = RDRVMAX   / `C_m2cm ;
       MKS_RDRMUES   = RDRMUES   / `C_m2cm_p2 ;
       MKS_RDRVMAXS  = RDRVMAXS  / `C_m2cm ;
       UC_NDEPM      = UC_NDEPM  / `C_cm2m_p3 ;
       UC_NJUNC      = NJUNC     / `C_cm2m_p3 ; // new punchthough

       //-----------------------------------------------------------*
       //* Change unit into Kelvin.
       //*-----------------//
       KTNOM =  TNOM + 273.15 ; // [C] -> [K] //
//     subNode  = `subNode_define ; Vsubs    = 0.0 ; 
     end // initializeModel
              
     begin : initializeInstance
                
       drainConductance  = 0.0 ;
       sourceConductance = 0.0 ;
       //-----------------------------------------------------------*
       //* Range check of instance parameters
       //*-----------------//
       if(COERRREP) begin
         if( L       < LMIN  || L       > LMAX ) $write("warning(HiSIM_HV(%m)): (L = %g) range [LMIN , LMAX].\n",L);
         if( W/NF    < WMIN  || W/NF    > WMAX ) $write("warning(HiSIM_HV(%m)): (W/NF = %g) range [WMIN , WMAX].\n",W/NF);
       end            
   
       // Calculate & check the Lgate and Wgate 
       Lgate = L + XL ;
       Wgate = W / NF  + XW ;
       `RANGECHECKFoo( Lgate     , 0.0, `inf, "Lgate") 
       `RANGECHECKFoo( Wgate     , 0.0, `inf, "Wgate") 
       `RANGECHECKFoo( Lgate-XGL , 0.0, `inf, "Lgate-XGL" )
       `RANGECHECKFco( Lgate+LLD , 0.0, `inf, "Lgate+LLD" )
       `RANGECHECKFco( Wgate+WLD , 0.0, `inf, "Wgate+WLD") 
       `RANGECHECKFco( Lgate+GDLD, 0.0, `inf, "Lgate+GDLD") 

       // check LDRIFT1+LDRIFT2
       `RANGECHECKFoo( LDRIFT1+LDRIFT2  , 0.0, `inf, "LDRIFT1+LDRIFT2" )

       // Binning calculation //
       LG = Lgate * `C_m2um ; // Regard LG as unitless. Say, Lgate=1e-6 (m); LG = 1e-6 (m) / 1e-6 (m) = 1.
       WG = Wgate * `C_m2um ; // Regard WG as unitless.
       Lbin = pow(LG, LBINN) ;
       Wbin = pow(WG, WBINN) ;
       LWbin = Lbin * Wbin ;
             
       `BINNING(UC_VMAX, VMAX, LVMAX, WVMAX, PVMAX) 
       `BINNING(UC_BGTMP1, BGTMP1, LBGTMP1, WBGTMP1, PBGTMP1) 
       `BINNING(UC_BGTMP2, BGTMP2, LBGTMP2, WBGTMP2, PBGTMP2) 
       `BINNING(UC_EG0, EG0, LEG0, WEG0, PEG0) 
       `BINNING(UC_VFBOVER, VFBOVER, LVFBOVER, WVFBOVER, PVFBOVER) 
       `BINNING(UC_NOVER, NOVER, LNOVER, WNOVER, PNOVER) 
       `BINNING(UC_NOVERS, NOVERS, LNOVERS, WNOVERS, PNOVERS) 
       `BINNING(UC_WL2, WL2, LWL2, WWL2, PWL2) 
       `BINNING(UC_VFBC, VFBC, LVFBC, WVFBC, PVFBC) 
       `BINNING(UC_NSUBC, NSUBC, LNSUBC, WNSUBC, PNSUBC) 
       `BINNING(UC_NSUBP, NSUBP, LNSUBP, WNSUBP, PNSUBP) 
       `BINNING(UC_SCP1, SCP1, LSCP1, WSCP1, PSCP1) 
       `BINNING(UC_SCP2, SCP2, LSCP2, WSCP2, PSCP2) 
       `BINNING(UC_SCP3, SCP3, LSCP3, WSCP3, PSCP3) 
       `BINNING(UC_SC1, SC1, LSC1, WSC1, PSC1) 
       `BINNING(UC_SC2, SC2, LSC2, WSC2, PSC2) 
       `BINNING(UC_SC3, SC3, LSC3, WSC3, PSC3) 
       `BINNING(UC_PGD1, PGD1, LPGD1, WPGD1, PPGD1) 
       `BINNING(UC_NDEP, NDEP, LNDEP, WNDEP, PNDEP) 
       `BINNING(UC_NINV, NINV, LNINV, WNINV, PNINV) 
       `BINNING(UC_MUECB0, MUECB0, LMUECB0, WMUECB0, PMUECB0) 
       `BINNING(UC_MUECB1, MUECB1, LMUECB1, WMUECB1, PMUECB1) 
       `BINNING(UC_MUEPH1, MUEPH1, LMUEPH1, WMUEPH1, PMUEPH1) 
       `BINNING(UC_VTMP, VTMP, LVTMP, WVTMP, PVTMP) 
       `BINNING(UC_WVTH0, WVTH0, LWVTH0, WWVTH0, PWVTH0) 
       `BINNING(UC_MUESR1, MUESR1, LMUESR1, WMUESR1, PMUESR1) 
       `BINNING(UC_MUETMP, MUETMP, LMUETMP, WMUETMP, PMUETMP) 
       `BINNING(UC_SUB1, SUB1, LSUB1, WSUB1, PSUB1) 
       `BINNING(UC_SUB2, SUB2, LSUB2, WSUB2, PSUB2) 
       `BINNING(UC_SVDS, SVDS, LSVDS, WSVDS, PSVDS) 
       `BINNING(UC_SVBS, SVBS, LSVBS, WSVBS, PSVBS) 
       `BINNING(UC_SVGS, SVGS, LSVGS, WSVGS, PSVGS)

       `BINNING(UC_SUB1SNP, SUB1SNP, LSUB1SNP, WSUB1SNP, PSUB1SNP) 
       `BINNING(UC_SUB2SNP, SUB2SNP, LSUB2SNP, WSUB2SNP, PSUB2SNP) 
       `BINNING(UC_SVDSSNP, SVDSSNP, LSVDSSNP, WSVDSSNP, PSVDSSNP)
//       `BINNING(UC_SVGSSNP, SVGSSNP, LSVGSSNP, WSVGSSNP, PSVGSSNP)
//       `BINNING(UC_SVBSSNP, SVBSSNP, LSVBSSNP, WSVBSSNP, PSVBSSNP)
        
       `BINNING(UC_FN1, FN1, LFN1, WFN1, PFN1) 
       `BINNING(UC_FN2, FN2, LFN2, WFN2, PFN2) 
       `BINNING(UC_FN3, FN3, LFN3, WFN3, PFN3) 
       `BINNING(UC_FVBS, FVBS, LFVBS, WFVBS, PFVBS) 
       `BINNING(UC_NSTI, NSTI, LNSTI, WNSTI, PNSTI) 
       `BINNING(UC_WSTI, WSTI, LWSTI, WWSTI, PWSTI) 
       `BINNING(UC_SCSTI1, SCSTI1, LSCSTI1, WSCSTI1, PSCSTI1) 
       `BINNING(UC_SCSTI2, SCSTI2, LSCSTI2, WSCSTI2, PSCSTI2) 
       `BINNING(UC_VTHSTI, VTHSTI, LVTHSTI, WVTHSTI, PVTHSTI) 
       `BINNING(UC_MUESTI1, MUESTI1, LMUESTI1, WMUESTI1, PMUESTI1) 
       `BINNING(UC_MUESTI2, MUESTI2, LMUESTI2, WMUESTI2, PMUESTI2) 
       `BINNING(UC_MUESTI3, MUESTI3, LMUESTI3, WMUESTI3, PMUESTI3) 
       `BINNING(UC_NSUBPSTI1, NSUBPSTI1, LNSUBPSTI1, WNSUBPSTI1, PNSUBPSTI1) 
       `BINNING(UC_NSUBPSTI2, NSUBPSTI2, LNSUBPSTI2, WNSUBPSTI2, PNSUBPSTI2) 
       `BINNING(UC_NSUBPSTI3, NSUBPSTI3, LNSUBPSTI3, WNSUBPSTI3, PNSUBPSTI3) 
       `BINNING(UC_CGSO, CGSO, LCGSO, WCGSO, PCGSO) 
       `BINNING(UC_CGDO, CGDO, LCGDO, WCGDO, PCGDO) 
       `BINNING(UC_CLM1, CLM1, LCLM1, WCLM1, PCLM1) 
       `BINNING(UC_CLM2, CLM2, LCLM2, WCLM2, PCLM2) 
       `BINNING(UC_CLM3, CLM3, LCLM3, WCLM3, PCLM3) 
       `BINNING(UC_WFC, WFC, LWFC, WWFC, PWFC) 
       `BINNING(UC_GIDL1, GIDL1, LGIDL1, WGIDL1, PGIDL1) 
       `BINNING(UC_GIDL2, GIDL2, LGIDL2, WGIDL2, PGIDL2) 
       `BINNING(UC_GLEAK1, GLEAK1, LGLEAK1, WGLEAK1, PGLEAK1) 
       `BINNING(UC_GLEAK2, GLEAK2, LGLEAK2, WGLEAK2, PGLEAK2) 
       `BINNING(UC_GLEAK3, GLEAK3, LGLEAK3, WGLEAK3, PGLEAK3) 
       `BINNING(UC_GLEAK6, GLEAK6, LGLEAK6, WGLEAK6, PGLEAK6) 
       `BINNING(UC_GLKSD1, GLKSD1, LGLKSD1, WGLKSD1, PGLKSD1) 
       `BINNING(UC_GLKSD2, GLKSD2, LGLKSD2, WGLKSD2, PGLKSD2) 
       `BINNING(UC_GLKB1, GLKB1, LGLKB1, WGLKB1, PGLKB1) 
       `BINNING(UC_GLKB2, GLKB2, LGLKB2, WGLKB2, PGLKB2) 
       `BINNING(UC_NFTRP, NFTRP, LNFTRP, WNFTRP, PNFTRP) 
       `BINNING(UC_NFALP, NFALP, LNFALP, WNFALP, PNFALP) 
       `BINNING(UC_IBPC1, IBPC1, LIBPC1, WIBPC1, PIBPC1) 
       `BINNING(UC_IBPC2, IBPC2, LIBPC2, WIBPC2, PIBPC2) 
       `BINNING(UC_CGBO, CGBO, LCGBO, WCGBO, PCGBO) 
       `BINNING(UC_CVDSOVER, CVDSOVER, LCVDSOVER, WCVDSOVER, PCVDSOVER) 
       `BINNING(UC_FALPH, FALPH, LFALPH, WFALPH, PFALPH) 
       `BINNING(UC_NPEXT, NPEXT, LNPEXT, WNPEXT, PNPEXT) 
       `BINNING(UC_POWRAT, POWRAT, LPOWRAT, WPOWRAT, PPOWRAT) 
       `BINNING(UC_RD, RD, LRD, WRD, PRD) 
       `BINNING(UC_RD22, RD22, LRD22, WRD22, PRD22) 
       `BINNING(UC_RD23, RD23, LRD23, WRD23, PRD23) 
       `BINNING(UC_RD24, RD24, LRD24, WRD24, PRD24) 
       `BINNING(UC_RDICT1, RDICT1, LRDICT1, WRDICT1, PRDICT1) 
       `BINNING(UC_RDOV13, RDOV13, LRDOV13, WRDOV13, PRDOV13) 
       `BINNING(UC_RDSLP1, RDSLP1, LRDSLP1, WRDSLP1, PRDSLP1) 
       `BINNING(UC_RDVB, RDVB, LRDVB, WRDVB, PRDVB) 
       `BINNING(UC_RDVD, RDVD, LRDVD, WRDVD, PRDVD) 
       `BINNING(UC_RDVG11, RDVG11, LRDVG11, WRDVG11, PRDVG11) 
       `BINNING(UC_RS, RS, LRS, WRS, PRS) 
       `BINNING(UC_RTH0, RTH0, LRTH0, WRTH0, PRTH0) 
       `BINNING(UC_VOVER, VOVER, LVOVER, WVOVER, PVOVER) 

`ifdef COEXTDIO 
       /* for Junction Diode Model */
`else 
       `BINNING(UC_JS0D, JS0D, LJS0D, WJS0D, PJS0D) 
       `BINNING(UC_JS0SWD, JS0SWD,  LJS0SWD, WJS0SWD, PJS0SWD) 
       `BINNING(UC_NJD, NJD, LNJD, WNJD, PNJD) 
       `BINNING(UC_CISBKD, CISBKD, LCISBKD, WCISBKD, PCISBKD) 
       `BINNING(UC_VDIFFJD, VDIFFJD, LVDIFFJD, WVDIFFJD, PVDIFFJD) 
       `BINNING(UC_JS0S, JS0S, LJS0S, WJS0S, PJS0S) 
       `BINNING(UC_JS0SWS, JS0SWS,  LJS0SWS, WJS0SWS, PJS0SWS) 
       `BINNING(UC_NJS, NJS, LNJS, WNJS, PNJS) 
       `BINNING(UC_CISBKS, CISBKS, LCISBKS, WCISBKS, PCISBKS) 
       `BINNING(UC_VDIFFJS, VDIFFJS, LVDIFFJS, WVDIFFJS, PVDIFFJS) 
`endif /* End of COEXTDIO */

       // Check the binned parameters 
       if(NSUBC     != UC_NSUBC)     `RANGECHECKFoo( UC_NSUBC    ,  0.0, `inf, "NSUBC@binned") 
       if(MUESTI1   != UC_MUESTI1)   `RANGECHECKFco( UC_MUESTI1  ,  0.0, `inf, "MUESTI1@binned") 
       if(NSUBPSTI1 != UC_NSUBPSTI1) `RANGECHECKFco( UC_NSUBPSTI1,  0.0, `inf, "NSUBPSTI1@binned") 
       if(FATAL_flag) $finish(0);

//L-scaling (instance dependent)
       // LG scaling just as in hisim2
       // LG = Lgate * `C_m2um ;

       if( UC_CODEP != 0 ) begin
          // LG dependence NDEPM  20170913
          T3 = pow( LG, NDEPMLP ) ;
          UC_NDEPM = UC_NDEPM*(1.0+NDEPML/T3) ;
          if( UC_NDEPM < 1e21 ) UC_NDEPM = 1e21 ; // 1e15 (CGS)
  
          // LG dependence DEPVMAX
          T3 = pow( LG,  DEPVMAXLP ) ;
          UC_DEPVMAX = UC_DEPVMAX * (1.0 + DEPVMAXL / T3) ;
  
          //LG dependence DEPLEAK  
          T3 = pow( LG,  DEPLEAKLP ) ;
          UC_DEPLEAK = DEPLEAK * (1.0 + DEPLEAKL / T3) ;
          if( UC_DEPLEAK < 0.0 ) UC_DEPLEAK = 0.0 ;
  
          // LG dependence DEPMUE0 & DEPMUE1
          T3 = pow( LG,  DEPMUE0LP ) ;
          UC_DEPMUE0 = DEPMUE0 * (1.0 + DEPMUE0L / T3) ;
          if( UC_DEPMUE0 < 1.0 ) UC_DEPMUE0 = 1.0 ;
          T3 = pow( LG,  DEPMUE1LP ) ;
          UC_DEPMUE1 = DEPMUE1 * (1.0 + DEPMUE1L / T3) ;
          if( UC_DEPMUE1 < 0.0 ) UC_DEPMUE1 = 0.0 ;
  
          // LG dependence DEPMUEBACK0 & DEPMUEBACK1  (CODEP=1 only)
          T3 = pow( LG,  DEPMUEBACK0LP ) ;
          UC_DEPMUEBACK0 = DEPMUEBACK0 * (1.0 + DEPMUEBACK0L / T3) ;
          if( UC_DEPMUEBACK0 < 0.0 ) UC_DEPMUEBACK0 = 0.0 ;
          T3 = pow( LG,  DEPMUEBACK1LP ) ;
          UC_DEPMUEBACK1 = DEPMUEBACK1 * (1.0 + DEPMUEBACK1L / T3) ;
          if( UC_DEPMUEBACK1 < 0.0 ) UC_DEPMUEBACK1 = 0.0 ;
  
          // LG dependence DEPVDSEF1 & DEPVDSEF2  (CODEP=1 only)
          T3 = pow( LG,  DEPVDSEF1LP ) ;
          UC_DEPVDSEF1 = UC_DEPVDSEF1 * (1.0 + DEPVDSEF1L / T3) ;
          T3 = pow( LG,  DEPVDSEF2LP ) ;
          UC_DEPVDSEF2 = UC_DEPVDSEF2 * (1.0 + DEPVDSEF2L / T3) ;
          if( UC_DEPVDSEF2 < 0.1 ) UC_DEPVDSEF2 = 0.1 ;
  
       end else begin // if ( UC_CODEP != 0 )
          UC_NDEPM=0.0;
          UC_DEPVMAX=0.0;
          UC_DEPLEAK=0.0;
          UC_DEPMUE0=0.0;
          UC_DEPMUE1=0.0;
          UC_DEPMUEBACK0=0.0;
          UC_DEPMUEBACK1=0.0;
          UC_DEPVDSEF1=0.0;
          UC_DEPVDSEF2=0.0;
       end


       //-----------------------------------------------------------*
       //* Range check of model parameters
       //*-----------------//

       if(COERRREP) begin
         if( UC_VMAX < 1.0e6 || UC_VMAX > 20.0e6 ) $write("warning(HiSIM_HV(%m)): (VMAX = %g) range [1.0e6 , 20.0e6].\n",UC_VMAX);
         if( UC_BGTMP1 < 50.0e-6 || UC_BGTMP1 > 1.0e-3 ) $write("warning(HiSIM_HV(%m)): (BGTMP1 = %g) range [50.0e-6 , 1.0e-3].\n",UC_BGTMP1);
         if( UC_BGTMP2 < -1.0e-6 || UC_BGTMP2 > 1.0e-6 ) $write("warning(HiSIM_HV(%m)): (BGTMP2 = %g) range [-1.0e-6 , 1.0e-6].\n",UC_BGTMP2);
         if( UC_EG0  < 1.0   || UC_EG0  > 1.3 ) $write("warning(HiSIM_HV(%m)): (EG0 = %g) range [1.0 , 1.3].\n",UC_EG0);
         if( UC_VFBOVER < -1.2  || UC_VFBOVER > 1.0  ) $write("warning(HiSIM_HV(%m)): (VFBOVER = %g) range [-1.2 , 1.0].\n",UC_VFBOVER);
         if( UC_CODEP == 0 ) begin
           if( UC_VFBC < -1.2  || UC_VFBC > 0.0 ) $write("warning(HiSIM_HV(%m)): (VFBC = %g) range [-1.2 , -0.0].\n",UC_VFBC);
         end else begin
           if( UC_VFBC < -1.2  || UC_VFBC > 0.8 ) $write("warning(HiSIM_HV(%m)): (VFBC = %g) range [-1.2 , 0.8].\n",UC_VFBC);
         end
         if( UC_NSUBC < 1.0e16 || UC_NSUBC > 1.0e19 ) $write("warning(HiSIM_HV(%m)): (NSUBC = %g) range [1.0e16 , 1.0e19].\n",UC_NSUBC);
         if( UC_NSUBP < 1.0e16 || UC_NSUBP > 1.0e19 ) $write("warning(HiSIM_HV(%m)): (NSUBP = %g) range [1.0e16 , 1.0e19].\n",UC_NSUBP);
         if( UC_SCP1 < 0.0   || UC_SCP1 > 10.0 ) $write("warning(HiSIM_HV(%m)): (SCP1 = %g) range [0.0 , 10.0].\n",UC_SCP1);
         if( UC_SCP2 < 0.0   || UC_SCP2 > 1.0 ) $write("warning(HiSIM_HV(%m)): (SCP2 = %g) range [0.0 , 1.0].\n",UC_SCP2);
         if( UC_SCP3 < 0.0   || UC_SCP3 > 200e-9 ) $write("warning(HiSIM_HV(%m)): (SCP3 = %g) range [0.0 , 200e-9].\n",UC_SCP3);
         if( UC_SC1  < 0.0   || UC_SC1  > 10.0 ) $write("warning(HiSIM_HV(%m)): (SC1 = %g) range [0.0 , 10.0].\n",UC_SC1);
         if( UC_SC2  < 0.0   || UC_SC2  > 1.0 ) $write("warning(HiSIM_HV(%m)): (SC2 = %g) range [0.0 , 1.0].\n",UC_SC2);
         if( UC_SC3  < 0.0   || UC_SC3  > 20e-6 ) $write("warning(HiSIM_HV(%m)): (SC3 = %g) range [0.0 , 20e-6].\n",UC_SC3);
         if( UC_PGD1 < 0.0   || UC_PGD1 > 30.0e-3 ) $write("warning(HiSIM_HV(%m)): (PGD1 = %g) range [0.0 , 30.0e-3].\n",UC_PGD1);
         if( UC_NDEP < 0.0   || UC_NDEP > 1.0 ) $write("warning(HiSIM_HV(%m)): (NDEP = %g) range [0.0 , 1.0].\n",UC_NDEP);
         if( UC_NINV < 0.0   || UC_NINV > 1.0 ) $write("warning(HiSIM_HV(%m)): (NINV = %g) range [0.0 , 1.0].\n",UC_NINV);
         if( UC_MUECB0 < 100.0 || UC_MUECB0 > 100.0e3 ) $write("warning(HiSIM_HV(%m)): (MUECB0 = %g) range [100.0 , 100.0e3].\n",UC_MUECB0);
         if( UC_MUECB1 < 5.0   || UC_MUECB1 > 10.0e3 ) $write("warning(HiSIM_HV(%m)): (MUECB1 = %g) range [5.0 , 10.0e3].\n",UC_MUECB1);
         if( UC_MUEPH1 < 2.0e3 || UC_MUEPH1 > 30.0e3 ) $write("warning(HiSIM_HV(%m)): (MUEPH1 = %g) range [2.0e3 , 30.0e3].\n",UC_MUEPH1);
         if( UC_VTMP < -2.0  || UC_VTMP > 1.0 ) $write("warning(HiSIM_HV(%m)): (VTMP = %g) range [-2.0 , 1.0].\n",UC_VTMP);
         if( UC_MUESR1 < 1.0e14 || UC_MUESR1 > 1.0e16 ) $write("warning(HiSIM_HV(%m)): (MUESR1 = %g) range [1.0e14 , 1.0e16].\n",UC_MUESR1);
         if( UC_MUETMP < 0.5   || UC_MUETMP > 2.5 ) $write("warning(HiSIM_HV(%m)): (MUETMP = %g) range [0.5 , 2.5].\n",UC_MUETMP);
         if( UC_CLM1 < 0.01  || UC_CLM1 > 1.0 ) $write("warning(HiSIM_HV(%m)): (CLM1 = %g) range [0.01 , 1.0].\n",UC_CLM1);
         if( UC_CLM2 < 1.0   || UC_CLM2 > 4.0 ) $write("warning(HiSIM_HV(%m)): (CLM2 = %g) range [1.0 , 4.0].\n",UC_CLM2);
         if( UC_CLM3 < 0.5   || UC_CLM3 > 5.0 ) $write("warning(HiSIM_HV(%m)): (CLM3 = %g) range [0.5 , 5.0].\n",UC_CLM3);
         if( UC_WFC  < -5.0e-15 || UC_WFC  > 1.0e-6 ) $write("warning(HiSIM_HV(%m)): (WFC = %g) range [-5.0e-15 , 1.0e-6].\n",UC_WFC);
         if( UC_CGSO < 0.0   || UC_CGSO > 100e-9*`C_VAC*KAPPA/TOX ) $write("warning(HiSIM_HV(%m)): (CGSO = %g) range [0.0 , 100e-9*`C_VAC*KAPPA/TOX].\n",UC_CGSO);
         if( UC_CGDO < 0.0   || UC_CGDO > 100e-9*`C_VAC*KAPPA/TOX ) $write("warning(HiSIM_HV(%m)): (CGDO = %g) range [0.0 , 100e-9*`C_VAC*KAPPA/TOX].\n",UC_CGDO);
         if( UC_IBPC1 < 0.0   || UC_IBPC1 > 1.0e12 ) $write("warning(HiSIM_HV(%m)): (IBPC1 = %g) range [0.0 , 1.0e12].\n",UC_IBPC1);
         if( UC_IBPC2 < 0.0   || UC_IBPC2 > 1.0e12 ) $write("warning(HiSIM_HV(%m)): (IBPC2 = %g) range [0.0 , 1.0e12].\n",UC_IBPC2);
         if( UC_CVDSOVER < 0.0   || UC_CVDSOVER > 1.0 ) $write("warning(HiSIM_HV(%m)): (CVDSOVER = %g) range [0.0 , 1.0].\n",UC_CVDSOVER);
         if( UC_CVDSOVER != 0.0 && COTRENCH != 0) $write("warning(HiSIM_HV(%m)): non-zero CVDSOVER(%g) does not work for COTRENCH(%g) != 0", UC_CVDSOVER,COTRENCH); // 2.4.0
         if( UC_NSTI < 1.0e16 || UC_NSTI > 1.0e19 ) $write("warning(HiSIM_HV(%m)): (NSTI = %g) range [1.0e16 , 1.0e19].\n",UC_NSTI);
         `MINCHECK(  UC_CGBO,       0.0,           "CGBO") 
         if( UC_NPEXT < 1.0e16 || UC_NPEXT > 1.0e18 ) $write("warning(HiSIM_HV(%m)): (NPEXT = %g) range [1.0e16 , 1.0e18].\n",UC_NPEXT);
            if( UC_RD   < 0.0   || UC_RD   > 100.0e-3 ) $write("warning(HiSIM_HV(%m)): (RD = %g) range [0.0 , 100.0e-3].\n",UC_RD);
            if( UC_RD22 < -5.0  || UC_RD22 > 0.0 ) $write("warning(HiSIM_HV(%m)): (RD22 = %g) range [-5.0 , 0.0].\n",UC_RD22);
            if( UC_RD23 < 0.0   || UC_RD23 > 2.0 ) $write("warning(HiSIM_HV(%m)): (RD23 = %g) range [0.0 , 2.0].\n",UC_RD23);
            if( UC_RD24 < 0.0   || UC_RD24 > 0.1 ) $write("warning(HiSIM_HV(%m)): (RD24 = %g) range [0.0 , 0.1].\n",UC_RD24);
            if( UC_RDICT1 < -10.0 || UC_RDICT1 > 10.0 ) $write("warning(HiSIM_HV(%m)): (RDICT1 = %g) range [-10.0 , 10.0].\n",UC_RDICT1);
            if( UC_RDOV13 < 0.0   || UC_RDOV13 > 1.0 ) $write("warning(HiSIM_HV(%m)): (RDOV13 = %g) range [0.0 , 1.0].\n",UC_RDOV13);
            if( UC_RDSLP1 < -10.0 || UC_RDSLP1 > 10.0 ) $write("warning(HiSIM_HV(%m)): (RDSLP1 = %g) range [-10.0 , 10.0].\n",UC_RDSLP1);
            if( UC_RDVB < 0.0   || UC_RDVB > 2.0 ) $write("warning(HiSIM_HV(%m)): (RDVB = %g) range [0.0 , 2.0].\n",UC_RDVB);
            if( UC_RDVD < 0.0   || UC_RDVD > 2.0  ) $write("warning(HiSIM_HV(%m)): (RDVD = %g) range [0.0 , 2.0].\n",UC_RDVD);
            `MINCHECK(  UC_RDVG11,     0.0,           "RDVG11") 
            if( UC_RS   < 0.0   || UC_RS   > 10.0e-3 ) $write("warning(HiSIM_HV(%m)): (RS = %g) range [0.0 , 10.0e-3].\n",UC_RS);
         if( UC_RTH0 < 0.0   || UC_RTH0 > 10.0 ) $write("warning(HiSIM_HV(%m)): (RTH0 = %g) range [0.0 , 10.0].\n",UC_RTH0);
         if( UC_VOVER < 0.0   || UC_VOVER > 4.0  ) $write("warning(HiSIM_HV(%m)): (VOVER = %g) range [0.0 , 4.0].\n",UC_VOVER);
       end
       if( UC_XPDV * UC_XLDLD > 1 ) begin
         UC_XPDV = 1/UC_XLDLD ; 
       end 
       `RANGECHECK(UC_POWRAT, 0.0, 1.0, "POWRAT")


       //if( CORDRIFT && UC_NOVER == 0.0 ) begin
       //  $write("warning(HiSIM_HV(%m)): CORDRIFT has been inactivated when NOVER = 0.0.\n");
       //  UC_CORDRIFT = 0 ;
       //end
        
       if( CORDRIFT == 1 && ( (CORD > 0 && UC_NOVER == 0.0) || (CORS > 0 && UC_NOVERS == 0.0) ) ) begin
         $write("warning(HiSIM_HV(%m)): new resistance model has been inactivated. Falls back on to legacy resistance model.\n");
         UC_CORDRIFT = 0;
       end else begin
         UC_CORDRIFT = CORDRIFT ;
       end
       if( UC_CORDRIFT == 1  ) begin
         flg_rd = (CORD > 0) ? 1:0;
         flg_rs = (CORS > 0) ? 1:0;
       end else begin // UC_CORDRIFT == 0
         if( CORSRD == 0 || CORSRD == 2 ) begin
            flg_rd = 0;
            flg_rs = 0;   
         end else begin // CORSRD == 1 || CORSRD == 3
            T1 = RSH*NRD*NF + (UC_RD + UC_RDVD)*(LDRIFT1 * UC_RDSLP1 * `C_m2um + UC_RDICT1 )*
                 ( LDRIFT2 * RDSLP2 * `C_m2um + RDICT2 );
            flg_rd = (T1 > 0.0) ? 1:0 ;
            T1 = RSHS*NRS*NF + UC_RS *(LDRIFT1S * UC_RDSLP1 * `C_m2um + UC_RDICT1 )*
                 ( LDRIFT2S * RDSLP2 * `C_m2um + RDICT2 );
            flg_rs = (T1 > 0.0) ? 1:0 ;
         end
       end


                
       //-----------------------------------------------------------*
       //* Change unit into MKS for instance parameters.
       //*-----------------//
       MKS_NSUBCDFM  = NSUBCDFM / `C_cm2m_p3 ;
       MKS_SUBLD2    = SUBLD2   * `C_m2cm ;
       UC_NSUBC      = UC_NSUBC  / `C_cm2m_p3 ;
       UC_NSUBP      = UC_NSUBP  / `C_cm2m_p3 ;
       UC_NSTI       = UC_NSTI   / `C_cm2m_p3 ;
       UC_NOVER      = UC_NOVER  / `C_cm2m_p3 ;
       UC_NOVERS     = UC_NOVERS / `C_cm2m_p3 ;
       UC_NSUBPSTI1  = UC_NSUBPSTI1 / `C_m2cm ;
       UC_MUESTI1    = UC_MUESTI1 / `C_m2cm ;
       UC_VMAX       = UC_VMAX   / `C_m2cm ;
       UC_WFC        = UC_WFC    * `C_m2cm_p2 ;
       UC_GLKSD1     = UC_GLKSD1 / `C_m2cm ;
       UC_GLKSD2     = UC_GLKSD2 * `C_m2cm ;
       UC_GLEAK2     = UC_GLEAK2 * `C_m2cm ;
       UC_GLKB2      = UC_GLKB2  * `C_m2cm ;
       UC_FN2        = UC_FN2    * `C_m2cm ;
       UC_GIDL1      = UC_GIDL1  / `C_m2cm_p1o2 ;
       UC_GIDL2      = UC_GIDL2  * `C_m2cm ;
       UC_NFALP      = UC_NFALP  / `C_m2cm ;
       UC_NFTRP      = UC_NFTRP  * `C_m2cm_p2 ;
       UC_NPEXT      = UC_NPEXT  / `C_cm2m_p3 ;
       UC_RD22       = UC_RD22   / `C_m2cm ;
       UC_RD23       = UC_RD23   / `C_m2cm ;
       UC_RD24       = UC_RD24   / `C_m2cm ;
       UC_RDVD       = UC_RDVD   / `C_m2cm ;
       UC_RTH0       = UC_RTH0   / `C_m2cm ;
       UC_VFBOVER    = -UC_VFBOVER ; // for Backward Compatibility //
       UC_DEPVMAX    = UC_DEPVMAX   / `C_m2cm ;
                     
                 
       flg_nqs = CONQS ;
       flg_qy = ( XQY != 0.0 || XQY1 != 0.0 ) ? 1 : 0 ;

         
       if(INFO >= 4) begin
         $write("//******* DEVICE PARAMETERS ***********//\n");
         $write("  L          = %12.5e  W          = %12.5e\n",L       ,W );
         $write("  NRD        = %12.5e  NRS        = %12.5e  NGCON      = %12.5e\n",NRD     ,NRS    ,NGCON);
         $write("  XGW        = %12.5e  XGL        = %12.5e  NSUBCDFM   = %12.5e\n",XGW     ,XGL    ,NSUBCDFM); 
         $write("  SA         = %12.5e  SB         = %12.5e  SD         = %12.5e\n",SA      ,SB     ,SD);
         $write("  LOVER      = %12.5e  LOVERLD    = %12.5e\n",                     LOVER   ,LOVERLD);
         $write("  LDRIFT1    = %12.5e  LDRIFT2    = %12.5e  LDRIFT1S   = %12.5e\n",LDRIFT1 ,LDRIFT2,LDRIFT1S);
         $write("  LDRIFT2S   = %12.5e  SUBLD1     = %12.5e  SUBLD2     = %12.5e\n",LDRIFT2S,SUBLD1 ,SUBLD2);
         $write("  NF         = %12.5e  M          = %12.5e  DTEMP      = %12.5e\n",NF      ,mMod   ,DTEMP);

`ifdef COEXTDIO 
       /* for Junction Diode Model */
`else 
         $write("  AD         = %12.5e  PD         = %12.5e\n",AD      ,PD);
         $write("  AS         = %12.5e  PS         = %12.5e\n",AS      ,PS);
`endif /* End of COEXTDIO */

         $write("//******* FLAG PARAMETERS *************//\n");
         $write("  CORSRD     = %d  CORG       = %d  COIPRV     = %d\n",UC_CORSRD ,CORG   ,COIPRV);
         $write("  CORG       = %d  COIPRV     = %d\n",CORG   ,COIPRV);
         $write("  COPPRV     = %d  COADOV     = %d  COISUB     = %d\n",COPPRV ,COADOV ,COISUB);
         $write("  COIIGS     = %d  COGIDL     = %d  COOVLP     = %d\n",COIIGS ,COGIDL ,COOVLP);
         $write("  COOVLPS    = %d  COFLICK    = %d  COISTI     = %d\n",COOVLPS,COFLICK,COISTI);
         $write("  CONQS      = %d  COTHRML    = %d  COIGN      = %d\n",CONQS  ,COTHRML,COIGN);
         $write("  CODFM      = %d  CORBNET    = %d  COSELFHEAT = %d\n",CODFM  ,CORBNET,COSELFHEAT);
         $write("  COSYM      = %d  COSUBNODE  = %d  CORDRIFT   = %d\n",COSYM  ,COSUBNODE, UC_CORDRIFT);
         $write("//******* TECHNOLOGY PARAMETERS *******//\n");
         $write("  VERSION    = %-12.2f  TYPE       = %d   \n",VERSION,TYPE );
         $write("  VMAX       = %12.5e  VMAXT1     = %12.5e  VMAXT2     = %12.5e\n",VMAX   ,VMAXT1 ,VMAXT2);
         $write("  BGTMP1     = %12.5e  BGTMP2     = %12.5e  TNOM       = %12.5e\n",BGTMP1 ,BGTMP2 ,TNOM);
         $write("  EG0        = %12.5e  TOX        = %12.5e  XLD        = %12.5e\n",EG0    ,TOX    ,XLD);
         $write("  RDOV11     = %12.5e  RDOV12     = %12.5e  RDOV13     = %12.5e\n",RDOV11 ,RDOV12 ,RDOV13);//HV1 
         $write("  RDSLP1     = %12.5e  RDICT1     = %12.5e  RDSLP2     = %12.5e\n",RDSLP1 ,RDICT1 ,RDSLP2);//HV1
         $write("  RDICT2     = %12.5e\n",RDICT2); //HV1
         $write("  DDLTMAX    = %12.5e\n",DDLTMAX);
         $write("  DDLTSLP    = %12.5e  DDLTICT    = %12.5e  VFBOVER    = %12.5e\n",DDLTSLP,DDLTICT,VFBOVER); 
         $write("  NOVER      = %12.5e  NOVERS     = %12.5e  XWD        = %12.5e\n",NOVER  ,NOVERS ,XWD);
         $write("  XWDC       = %12.5e  XWDLD      = %12.5e                     \n",XWDC   ,XWDLD  );
         $write("  XL         = %12.5e  XW         = %12.5e  SAREF      = %12.5e\n",XL     ,XW     ,SAREF);
         $write("  SBREF      = %12.5e  LL         = %12.5e  LLD        = %12.5e\n",SBREF  ,LL     ,LLD);
         $write("  LLN        = %12.5e  WL         = %12.5e  WL1        = %12.5e\n",LLN    ,WL     ,WL1);
         $write("  WL1P       = %12.5e  WL2        = %12.5e  WL2P       = %12.5e\n",WL1P   ,WL2    ,WL2P);
         $write("  WLD        = %12.5e  WLN        = %12.5e  RSH        = %12.5e\n",WLD    ,WLN    ,RSH);
         $write("  RSHG       = %12.5e  XQY        = %12.5e  XQY1       = %12.5e\n",RSHG   ,XQY    ,XQY1);
         $write("  XQY2       = %12.5e\n", XQY2);
         $write("  RS         = %12.5e\n",RD     ,RS); //HV1
         $write("  VFBC       = %12.5e  VBI        = %12.5e  NSUBC      = %12.5e\n",VFBC   ,VBI    ,NSUBC);
         $write("  PARL2      = %12.5e  LP         = %12.5e  NSUBP      = %12.5e\n",PARL2  ,LP     ,NSUBP);
         $write("  NSUBP0     = %12.5e  NSUBWP     = %12.5e  SCP1       = %12.5e\n",NSUBP0 ,NSUBWP ,SCP1);
         $write("  SCP2       = %12.5e  SCP3       = %12.5e  SC1        = %12.5e\n",SCP2   ,SCP3   ,SC1);
         $write("  SC2        = %12.5e  SC3        = %12.5e  SC4        = %12.5e\n",SC2    ,SC3    ,SC4);
         $write("  PGD1       = %12.5e  PGD2       = %12.5e                     \n",PGD1   ,PGD2   );
         $write("  PGD4       = %12.5e  NDEP       = %12.5e  NDEPL      = %12.5e\n",PGD4   ,NDEP   ,NDEPL);
         $write("  NDEPLP     = %12.5e  NINV       = %12.5e  MUECB0     = %12.5e\n",NDEPLP ,NINV   ,MUECB0);
         $write("  MUECB1     = %12.5e  MUEPH0     = %12.5e  MUEPH1     = %12.5e\n",MUECB1 ,MUEPH0 ,MUEPH1);
         $write("  MUEPHW     = %12.5e  MUEPWP     = %12.5e  MUEPHL     = %12.5e\n",MUEPHW ,MUEPWP ,MUEPHL); 
         $write("  MUEPLP     = %12.5e  MUEPHS     = %12.5e  MUEPSP     = %12.5e\n",MUEPLP ,MUEPHS ,MUEPSP);
         $write("  VTMP       = %12.5e  WVTH0      = %12.5e  MUESR0     = %12.5e\n",VTMP   ,WVTH0  ,MUESR0);
         $write("  MUESR1     = %12.5e  MUESRL     = %12.5e  MUESRW     = %12.5e\n",MUESR1 ,MUESRL ,MUESRW);
         $write("  MUESWP     = %12.5e  MUESLP     = %12.5e  MUETMP     = %12.5e\n",MUESWP ,MUESLP ,MUETMP);
         $write("  BB         = %12.5e  SUB1       = %12.5e  SUB2       = %12.5e\n",BB     ,SUB1   ,SUB2);
         $write("  SVGS       = %12.5e  SVBS       = %12.5e  SVBSL      = %12.5e\n",SVGS   ,SVBS   ,SVBSL);
         $write("  SVDS       = %12.5e  SLG        = %12.5e  SUB1L      = %12.5e\n",SVDS   ,SLG    ,SUB1L);
         $write("  SUB2L      = %12.5e  FN1        = %12.5e  FN2        = %12.5e\n",SUB2L  ,FN1    ,FN2);
         $write("  FN3        = %12.5e  FVBS       = %12.5e  SVGSL      = %12.5e\n",FN3    ,FVBS   ,SVGSL);
         $write("  SVGSLP     = %12.5e  SVGSWP     = %12.5e  SVGSW      = %12.5e\n",SVGSLP ,SVGSWP ,SVGSW);
         $write("  SVBSLP     = %12.5e  SLGL       = %12.5e  SLGLP      = %12.5e\n",SVBSLP ,SLGL   ,SLGLP);
         $write("  SUB1LP     = %12.5e  NSTI       = %12.5e  WSTI       = %12.5e\n",SUB1LP ,NSTI   ,WSTI);
         $write("  WSTIL      = %12.5e  WSTILP     = %12.5e  WSTIW      = %12.5e\n",WSTIL  ,WSTILP ,WSTIW);
         $write("  WSTIWP     = %12.5e  SCSTI1     = %12.5e  SCSTI2     = %12.5e\n",WSTIWP ,SCSTI1 ,SCSTI2);
         $write("  VTHSTI     = %12.5e  VDSTI      = %12.5e  MUESTI1    = %12.5e\n",VTHSTI ,VDSTI  ,MUESTI1);
         $write("  MUESTI2    = %12.5e  MUESTI3    = %12.5e  NSUBPSTI1  = %12.5e\n",MUESTI2,MUESTI3,NSUBPSTI1);
         $write("  NSUBPSTI2  = %12.5e  NSUBPSTI3  = %12.5e  LPEXT      = %12.5e\n",NSUBPSTI2,NSUBPSTI3,LPEXT);
         $write("  NPEXT      = %12.5e  SCP21      = %12.5e  SCP22      = %12.5e\n",NPEXT  ,SCP21  ,UC_SCP22);
         $write("  BS1        = %12.5e  BS2        = %12.5e  TPOLY      = %12.5e\n",BS1    ,BS2    ,TPOLY);
         $write("  CGBO       = %12.5e  CLM1       = %12.5e  CLM2       = %12.5e\n",CGBO   ,CLM1   ,CLM2);
         $write("  CLM3       = %12.5e  CLM5       = %12.5e  CLM6       = %12.5e\n",CLM3   ,CLM5   ,CLM6);
         $write("  VOVER      = %12.5e  VOVERP     = %12.5e  WFC        = %12.5e\n",VOVER  ,VOVERP ,WFC);
         $write("  NSUBCW     = %12.5e  NSUBCWP    = %12.5e  QME1       = %12.5e\n",NSUBCW ,NSUBCWP,QME1);
         $write("  QME2       = %12.5e  QME3       = %12.5e  VOVERS     = %12.5e\n",QME2   ,QME3   ,VOVERS);
         $write("  VOVERSP    = %12.5e  GIDL1      = %12.5e  GIDL2      = %12.5e\n",VOVERSP,GIDL1  ,GIDL2);
         $write("  GIDL3      = %12.5e  GIDL4      = %12.5e  GIDL5      = %12.5e\n",GIDL3  ,GIDL4  ,GIDL5);
         $write("  GLEAK1     = %12.5e  GLEAK2     = %12.5e  GLEAK3     = %12.5e\n",GLEAK1 ,GLEAK2 ,GLEAK3);
         $write("  GLEAK4     = %12.5e  GLEAK5     = %12.5e  GLEAK6     = %12.5e\n",GLEAK4 ,GLEAK5 ,GLEAK6);
         $write("  GLEAK7     = %12.5e  GLPART1    = %12.5e\n"                     ,GLEAK7 ,GLPART1);
         $write("  GLKSD1     = %12.5e  GLKSD2     = %12.5e  GLKSD3     = %12.5e\n",GLKSD1 ,GLKSD2 ,GLKSD3);
         $write("  GLKB1      = %12.5e  GLKB2      = %12.5e  GLKB3      = %12.5e\n",GLKB1  ,GLKB2  ,GLKB3 );
         $write("  EGIG       = %12.5e  IGTEMP2    = %12.5e  IGTEMP3    = %12.5e\n",EGIG   ,IGTEMP2,IGTEMP3);
         $write("  VZADD0     = %12.5e  PZADD0     = %12.5e  NFTRP      = %12.5e\n",VZADD0 ,PZADD0 ,NFTRP);
         $write("  NFALP      = %12.5e  CIT        = %12.5e  FALPH      = %12.5e\n",NFALP  ,CIT    ,FALPH);
         $write("  KAPPA      = %12.5e  CGDO       = %12.5e  CGSO       = %12.5e\n",KAPPA  ,CGDO   ,CGSO);
         $write("  NINVD      = %12.5e                                          \n",NINVD);
         $write("  NINVDW     = %12.5e  NINVDWP    = %12.5e  NINVDT1    = %12.5e\n",NINVDW ,NINVDWP,NINVDT1);
         $write("  NINVDT2    = %12.5e  RDVB       = %12.5e  VBSMIN     = %12.5e\n",NINVDT2,RDVB   ,VBSMIN); //HV1
         $write("  NINVDT2    = %12.5e  VBSMIN     = %12.5e\n",NINVDT2,VBSMIN);
         $write("  DLY1       = %12.5e  DLY2       = %12.5e  DLY3       = %12.5e\n",DLY1   ,DLY2   ,DLY3);
         $write("  OVSLP      = %12.5e  OVMAG      = %12.5e                     \n",OVSLP  ,OVMAG);
         $write("  RBPB       = %12.5e  RBPD       = %12.5e  RBPS       = %12.5e\n",RBPB   ,RBPD   ,RBPS);
         $write("  IBPC1      = %12.5e  IBPC2      = %12.5e  MPHDFM     = %12.5e\n",IBPC1  ,IBPC2  ,MPHDFM);
         $write("  RDVG11     = %12.5e  RDVG12     = %12.5e  RTH0       = %12.5e\n",RDVG11 ,RDVG12 ,RTH0); //HV1
         $write("  RTH0R      = deleted CTH0       = %12.5e  POWRAT     = %12.5e\n",CTH0   ,POWRAT);
         $write("  DLYOV      = %12.5e  QDFTVD     = deleted XLDLD      = %12.5e\n",DLYOV  ,UC_XLDLD);
         $write("  RDVD       = %12.5e  RD20       = %12.5e                     \n",RDVD   ,RD20);             //HV1
         $write("  RD21       = %12.5e  RD22       = %12.5e  RD22D      = %12.5e\n",RD21   ,RD22   ,RD22D);    //HV1
         $write("  RD23       = %12.5e  RD24       = %12.5e  RD25       = %12.5e\n",RD23   ,RD24   ,RD25);     //HV1
         $write("  RDVDL      = %12.5e  RDVDLP     = %12.5e  RDVDS      = %12.5e\n",RDVDL  ,RDVDLP ,RDVDS);    //HV1
         $write("  RDVDSP     = %12.5e  RD23L      = %12.5e  RD23LP     = %12.5e\n",RDVDSP ,RD23L  ,RD23LP);   //HV1
         $write("  RDS        = %12.5e  RDSP       = %12.5e  RDTEMP1    = %12.5e\n",RDS    ,RDSP   ,RDTEMP1);  //HV1
         $write("  RDTEMP2    = %12.5e  RDVDTEMP1  = %12.5e  RDVDTEMP2  = %12.5e\n",RDTEMP2,RDVDTEMP1,RDVDTEMP2); //HV1
         $write("  RTH0W      = %12.5e  RTH0WP     = %12.5e  RTH0NF     = %12.5e\n",RTH0W  ,RTH0WP ,RTH0NF);
         $write("  RTHTEMP1   = %12.5e  RTHTEMP2   = %12.5e  PRATTEMP1  = %12.5e\n",RTHTEMP1,RTHTEMP2,PRATTEMP1);
         $write("  PRATTEMP2  = %12.5e  RDVSUB     = %12.5e  RDVDSUB    = %12.5e\n",PRATTEMP2,RDVSUB,RDVDSUB); //HV1
         $write("  PRATTEMP2  = %12.5e\n",PRATTEMP2);
         $write("  DDRIFT     = %12.5e  VBISUB     = %12.5e  NSUBSUB    = %12.5e\n",DDRIFT ,VBISUB,NSUBSUB);
         $write("  gminMod    = %12.5e  mMod       = %12.5e  GBMIN      = %12.5e\n",gminMod ,mMod  ,GBMIN);

`ifdef COEXTDIO 
       /* for Junction Diode Model */
`else 
         $write("  JS0        = %12.5e  JS0SW      = %12.5e  DIVX       = %12.5e\n",JS0    ,JS0SW  ,DIVX);
         $write("  NJ         = %12.5e  NJSW       = %12.5e  XTI        = %12.5e\n",NJ     ,NJSW   ,XTI);
         $write("  CJ         = %12.5e  CJSW       = %12.5e  CJSWG      = %12.5e\n",CJ     ,CJSW   ,CJSWG);
         $write("  MJ         = %12.5e  MJSW       = %12.5e  MJSWG      = %12.5e\n",MJ     ,MJSW   ,MJSWG);
         $write("  PB         = %12.5e  PBSW       = %12.5e  PBSWG      = %12.5e\n",PB     ,PBSW   ,PBSWG);
         $write("  XTI2       = %12.5e  CISB       = %12.5e  CVB        = %12.5e\n",XTI2   ,CISB   ,CVB);
         $write("  CTEMP      = %12.5e  CISBK      = %12.5e  VDIFFJ     = %12.5e\n",CTEMP  ,CISBK  ,VDIFFJ);
         $write("  TCJBD      = %12.5e  TCJBDSW    = %12.5e  TCJBDSWG   = %12.5e\n",TCJBD  ,TCJBDSW ,TCJBDSWG);
         $write("  TCJBS      = %12.5e  TCJBSSW    = %12.5e  TCJBSSWG   = %12.5e\n",TCJBS  ,TCJBSSW ,TCJBSSWG);
         $write("  TPBBD      = %12.5e  TPBBDSW    = %12.5e  TPBBDSWG   = %12.5e\n",TPBBD  ,TPBBDSW ,TPBBDSWG);
         $write("  TPBBS      = %12.5e  TPBBSSW    = %12.5e  TPBBSSWG   = %12.5e\n",TPBBS  ,TPBBSSW ,TPBBSSWG);
`endif /* End of COEXTDIO */

       end // end of printing
                
       begin   // hsmhvtemp.c
                  
         // Quantum Mechanical Effect //
         if(( QME1 == 0.0 && QME3 == 0.0 ) || QME2 == 0.0 ) begin
           flg_qmetemp = 0 ;
         end else begin
           flg_qmetemp = 1 ;
         end
                 
         begin     // HSMHVinstances
           WLG = WG * LG ;
           UC_GDLD = GDLD * `C_m2um ;
                
           // Band gap //
           egtnom = UC_EG0 - KTNOM    
                  * ( 90.25e-6 + KTNOM * 1.0e-7 ) ;
                  
           // C_EOX //
           cecox = `C_VAC * KAPPA ;
                 
           // Vth reduction for Small Vds //
           msc = UC_SCP22  ;
               
           // Poly-Si Gate Depletion //
           if( UC_PGD1 == 0.0 ) begin
             flg_pgd = 0 ;
             cnstpgd = 0.0 ;
           end else begin
             flg_pgd = 1 ;
             // constant for Poly depletion //
             cnstpgd = pow( 1e0 + 1e0 / LG , PGD4 ) * UC_PGD1 ;
           end
                   
           // CLM5 & CLM6 //
           clmmod = 1e0 + pow( LG , CLM5 ) * CLM6 ;
                  
           // Half length of diffusion //
           T1 = 1.0 / (SAREF + 0.5 * L)
              + 1.0 / (SBREF + 0.5 * L);
           Lod_half_ref = 2.0 / T1 ;
                        
           if(SA > 0.0 && SB > 0.0 &&
             (NF == 1.0 || (NF > 1.0 && SD > 0.0))) begin
             T1 = 0.0;
             for (i = 0; i < NF; i= i + 1) begin
               T1 = T1 + 1.0 / (SA + 0.5 * L
                  + i * (SD + L))
                  + 1.0 / (SB + 0.5 * L
                  + i * (SD + L));
             end
             Lod_half = 2.0 * NF / T1;
           end else begin
             Lod_half = 0.0;
           end
           Npexte = UC_NPEXT ;
           EF_MUEPH1 = UC_MUEPH1 ;
           EF_NSUBP  = UC_NSUBP ;
           EF_NSUBC  = UC_NSUBC ;
                     
           // DFM //
           if( CODFM == 1 && NSUBCDFM_GIVEN ) begin
             if( NSUBCDFM <   1.0e16 || NSUBCDFM >   1.0e19) $write("warning(HiSIM_HV(%m)): The parameter NSUBCDFM(= %e) must be in the range [ 1.0e16 , 1.0e19 ].\n",NSUBCDFM);
             EF_MUEPH1 = EF_MUEPH1 * ( MPHDFM
                       *( ln(MKS_NSUBCDFM) - ln(EF_NSUBC) ) + 1.0 ) ;
             EF_NSUBP = EF_NSUBP + MKS_NSUBCDFM - EF_NSUBC ;
             Npexte = Npexte + MKS_NSUBCDFM - EF_NSUBC ;
             EF_NSUBC = MKS_NSUBCDFM ;
           end
                    
           // Phonon Scattering(temperature-independent part) //
           mueph = EF_MUEPH1 
                 * (1.0e0 + (MUEPHW / pow( WG, MUEPWP)))
                 * (1.0e0 + (MUEPHL / pow( LG, MUEPLP)))
                 * (1.0e0 + (MUEPHS / pow( WLG, MUEPSP))) ;  

           if(Lod_half > 0.0) begin
             T1 = 1.0e0 / (1.0e0 + UC_MUESTI2) ;
             T2 = pow (UC_MUESTI1 / Lod_half, UC_MUESTI3) ;
             T3 = pow (UC_MUESTI1 / Lod_half_ref, UC_MUESTI3) ;
             mueph = mueph * (1.0e0 + T1 * T2) / (1.0e0 + T1 * T3); 
           end
                 
           // Surface Roughness Scattering //
           muesr = MUESR0 
                 * (1.0e0 +(MUESRL / pow(LG, MUESLP)))
                 * (1.0e0 +(MUESRW / pow(WG, MUESWP))) ;

           // Check the Mobility value after L , W and S dependence 
           `RANGECHECK(mueph, 2e3 , 30e3 , "MUEPH1@L,W,S dependence")
           `RANGECHECK(muesr, 1.8 , 2.2  , "MUESR0@L,W dependence ")
           mueph = ( mueph < `Small ) ? `Small : mueph ;
           muesr = ( muesr < `Small ) ? `Small : muesr ;
                 
           // Coefficients of Qbm for Eeff //
           T1 = pow( LG, NDEPLP ) ;
           ndep_o_esi = ( UC_NDEP * T1 ) / ( T1 + NDEPL )
                      / `C_ESI ;
           ninv_o_esi = UC_NINV / `C_ESI ;
           ninvd0 = NINVD * ( 1.0 + (NINVDL / pow( LG, NINVDLP)))*( 1.0 + (NINVDW / pow( WG, NINVDWP)));
           T1 = ( 1.0 + (DEPNINVDL / pow( LG, DEPNINVDLP))) * ( 1.0 + (DEPNINVDW / pow( WG, DEPNINVDWP))) ;
           ninvd0cRES = DEPNINVDC * T1 ;
           ninvd0hRES = DEPNINVDH * T1 ;
           // Metallurgical channel geometry //
           dL = XLD  +(MKS_LL / pow(Lgate + LLD, LLN)) ;
           dLLD = UC_XLDLD  +(MKS_LL / pow(Lgate + LLD, LLN)) ;
           dW = XWD  +(MKS_WL / pow(Wgate + WLD, WLN)) ;  
           dWLD = XWDLD  +(MKS_WL / pow(Wgate + WLD, WLN)) ;  
           dWCV = XWDC  +(MKS_WL / pow(Wgate + WLD, WLN)) ;  
           Leff = Lgate - ( dL + dLLD ) ;
           if( Leff <= 0.0 ) begin   
             $write("Fatal error(HiSIM_HV(%m)): An effective channel length is negative or 0\n");
             FATAL_flag = 1;
           end
                     
           // Wg dependence for short channel devices //
           lgatesm = Lgate + WL1 / pow( WLG , WL1P ) ;
           dVthsm = UC_WL2 / pow( WLG , WL2P ) ;
                  
           // Lg dependence of wsti //
           T1 = 1.0e0 + WSTIL / pow( lgatesm * `C_m2um  , WSTILP ) ;
           T2 = 1.0e0 + WSTIW / pow( WG , WSTIWP ) ;
           UC_WSTI = UC_WSTI * T1 * T2 ;
           Weff   = Wgate - 2.0e0 * dW ;
           weff_ld = Wgate - 2.0e0 * dWLD ;
           weff_cv = Wgate - 2.0e0 * dWCV ;
           if( Weff <= 0.0 ) begin   
             $write("Fatal error(HiSIM_HV(%m)): An effective channel width is negative or 0\n");
             FATAL_flag = 1;
           end
           if( weff_ld <= 0.0 ) begin   
             $write("Fatal error(HiSIM_HV(%m)): An effective channel width for XWDLD is negative or 0\n");
             FATAL_flag = 1;
           end
           if( weff_cv <= 0.0 ) begin   
             $write("Fatal error(HiSIM_HV(%m)): An effective channel width for XWDC is negative or 0\n");
             FATAL_flag = 1;
           end
           weff_nf = Weff * NF ;
           weffcv_nf = weff_cv * NF ;
                     
           // Surface impurity profile //
           // Note: Sign Changed --> //
           Nsubpp = EF_NSUBP   * ( 1.0e0 + (NSUBP0 / pow(WG, NSUBWP)) ) ;
           EF_NSUBC = EF_NSUBC * ( 1.0e0 + (NSUBCW / pow(WG, NSUBCWP)) ) ;
           T1 = EF_NSUBC * `C_cm2m_p3  ;
           T2 = Nsubpp * `C_cm2m_p3 ;

           // Clamping the minimum value for each parameters
           `MINRESET( T1 , 1e15, "NSUBC@W dependence")
           EF_NSUBC = T1 / `C_cm2m_p3 ;
           `MINRESET( T2 , 1e15, "NSUBP@W dependence")
           Nsubpp = T2 / `C_cm2m_p3 ;

           // <-- Note: Sign Changed //
           if(Lod_half > 0.0) begin
             T1 = 1.0e0 / (1.0e0 + UC_NSUBPSTI2) ;
             T2 = pow (UC_NSUBPSTI1 / Lod_half, UC_NSUBPSTI3) ;
             T3 = pow (UC_NSUBPSTI1 / Lod_half_ref, UC_NSUBPSTI3) ;
             Nsubps = Nsubpp * (1.0e0 + T1 * T2) / (1.0e0 + T1 * T3) ;
           end else begin
             Nsubps = Nsubpp ;
           end
           if( Lgate > LP ) begin
             Nsub = (EF_NSUBC * (Lgate - LP) 
                  +  Nsubps  * LP) / Lgate ;
           end else begin
             Nsub = Nsubps
                  + (Nsubps - EF_NSUBC) * (LP - Lgate) 
                  / LP ;
           end
           T3 = 0.5e0 * Lgate - LP ;
           `Fn_SL( T3 , T3 , 1E-9 , 1E-10 , T0 )
           T1 = 1.0e0 / ( 1.0e0 / T3 + 1.0e0 / LPEXT ) ;
           T2 = `Fn_Max(0.0e0, T1) ;
           Nsub = Nsub + T2 * (Npexte - EF_NSUBC) / Lgate ;
           q_Nsub   = `C_QE * Nsub ;
           qnsub_esi = q_Nsub * `C_ESI ;
           qnsub_esi2 = 2.0 * qnsub_esi ;
                      
           // Pocket Overlap(temperature-independent part) //
           if( Lgate <= 2.0e0 * LP ) begin
             Nsubb = 2.0e0 * Nsubps 
                   - (Nsubps - EF_NSUBC) * Lgate 
                   / LP - EF_NSUBC ;
             ptovr0 = ln (Nsubb / EF_NSUBC) ;
             // ptovr0 will be divided by beta later. //
           end else begin
             ptovr0 = 0.0e0 ;
           end
                  
           // costi0 and costi1 for STI transistor model(temperature-independent part) //
           costi00 = sqrt (2.0 * `C_QE * UC_NSTI * `C_ESI ) ;
           nsti_p2 = 1.0 / ( UC_NSTI * UC_NSTI ) ;
                   
           // Velocity Temperature Dependence(Temperature-dependent part will be multiplied later.) //
           vmax0 =(1.0e0 +(UC_VOVER / pow(LG, VOVERP)))
                 *(1.0e0 +(VOVERS / pow(WLG, VOVERSP))) ;
                 
           // 2 phi_B(temperature-independent) //
           // @300K, with pocket //
           Pb20 = 2.0e0 / `C_b300 * ln (Nsub / `C_Nin0) ;
           // @300K, w/o pocket //
           Pb2c = 2.0e0 / `C_b300 * ln (EF_NSUBC / `C_Nin0) ;
                   
           // Gate resistance //
           if( CORG == 1 ) begin
             T1 = XGW + Weff / (3.0e0 * NGCON);
             T2 = Lgate - XGL;
             grg = RSHG * T1 / (NGCON * T2 * NF);
             if (grg > 1.0e-3) grg = Mfactor / grg;
               else begin
               grg = Mfactor * 1.0e3;
               $write("warning(HiSIM_HV(%m)): The gate conductance reset to 1.0e3 mho.\n");
             end
           end else begin
             grg = 1/RMIN;
           end
               
           // Process source/drain series resistance //
           if( RSH > 0.0 ) begin
             Rd0 = RSH * NRD ;
             Rs0 = RSH * NRS ;
           end else begin
             Rd0 = 0.0 ;
             Rs0 = 0.0 ;
           end
           if( RSHS > 0.0 ) begin
             Rs0 = RSHS * NRS ;
           end else Rs0 = 0.0 ;

           if( UC_CORDRIFT == 0 ) begin // CORDIRFT = 0
             if( UC_RD > 0.0 || UC_RS > 0.0 ) begin
               Rdtemp0 = 1.0 + RDS / pow( WLG , RDSP ) ;
               if( UC_RDVD != 0.0 ) begin
                 T7 = ( 1.0 + RDVDS / pow( WLG , RDVDSP ) );
                 T6 = ( - RDVDL * pow( LG , RDVDLP ) ) ;
                 if(T6 > `large_arg) T6 = `large_arg ;  
                 T6 = exp( T6 ) ;
                 Rdvdtemp0 = T6 * T7 ;
               end else begin
                 Rdvdtemp0 = 0.0 ;
               end
             end else begin
               Rdtemp0 = 0.0 ;
               Rdvdtemp0 = 0.0 ;
             end
             if( UC_RD23 != 0.0 ) begin
               T2 = ( 1.0 + RD23S / pow( WLG , RD23SP ) );
               T1 = ( - RD23L * pow( LG , RD23LP ) ) ;
               if(T1 > `large_arg)  T1 = `large_arg ; 
               T1 = exp( T1 ) ;
               T3 = UC_RD23 * T2 * T1 ;
               Rd23e = 0.5 * ( T3 + sqrt ( T3 * T3 + 4.0 * `dlt_rd23 * `dlt_rd23 ) ) ;
             end else begin
               Rd23e = 0.0 ;
             end
             Xmax = 0.0 ; Xmax_s = 0.0 ;
             Rdrvmaxwe = 0.0 ; Rdrvmaxle = 0.0 ;
             Rdrmuele = 0.0 ; RdrmueVbs = 0.0;
           end // CORDIRFT = 0
           else begin // CORDIRFT = 1
             Xmax  = sqrt ( RDRDJUNC * RDRDJUNC + UC_XLDLD * UC_XLDLD ) ;
             Xmax_s = sqrt( RDRDJUNC * RDRDJUNC + XLD * XLD ) ;   // 20160615 compatibility
             Rdrvmaxwe = 1.0e0 + (RDRVMAXW / pow( WG, RDRVMAXWP)) ;
             Rdrvmaxle = 1.0e0 + (RDRVMAXL / pow( LG, RDRVMAXLP)) ;
             Rdrmuele  = 1.0e0 + (RDRMUEL  / pow( LG, RDRMUELP )) ;
	     RdrmueVbs = 1.0; // see eval_rdrift later
             Rdtemp0 = 0.0 ; Rdvdtemp0 = 0.0 ; Rd23e = 0.0 ;
           end // CORDIRFT = 1
           if( UC_NOVER > 0.0 ) begin
             kdep  = 2.0 * `C_ESI / ( `C_QE * UC_NOVER ) ;
             kjunc = 2.0 * `C_ESI / `C_QE * EF_NSUBC / ( UC_NOVER + EF_NSUBC ) / UC_NOVER ;
           end else begin
             kdep = 0.0 ; kjunc = 0.0 ;
           end
                    
           // Body resistance //
           if( CORBNET == 1 ) begin
             if (RBPB < 1.0e-3) grbpb = Mfactor * 1.0e3 ;
             else grbpb = Mfactor * ( GBMIN + 1.0 / RBPB ) ;
             if (RBPS < 1.0e-3) grbps = Mfactor * 1.0e3 ;
             else grbps = Mfactor * ( GBMIN + 1.0 / RBPS ) ;
             if (RBPD < 1.0e-3) grbpd = Mfactor * 1.0e3 ;
             else grbpd = Mfactor * ( GBMIN + 1.0 / RBPD ) ;
           end else begin
             grbpb = Mfactor * 1.0e3 ; grbps = grbpb ; grbpd = grbpb ;
           end
                    
           // Vdseff //
           if( CODDLT == 0 ) begin
             T1 = DDLTSLP * LG + DDLTICT ;
             if ( T1 < 0.0 ) begin T1 = 0.0 ; end
             DDLTe = T1 * DDLTMAX / ( T1 + DDLTMAX ) + 1.0 ;
           end else begin
             T1 = DDLTSLP * LG ;
             if ( T1 < 0.0 ) begin T1 = 0.0 ; end
             DDLTe = T1 * DDLTMAX / ( T1 + DDLTMAX ) + DDLTICT + `Small ;
           end
           `MINRESET( DDLTe , 0.1, "DDLT@L dependence")
                
           // Isub //
           if( COISUB ) begin
             T2 = pow( Weff , SVGSWP ) ;
             vg2const = UC_SVGS
                      * ( 1.0e0 + MKS_SVGSL / pow( Lgate , SVGSLP ) )
                      * ( T2 / ( T2 + MKS_SVGSW ) ) ; 
             xvbs  = UC_SVBS * ( 1.0 + MKS_SVBSL/ pow( Lgate , SVBSLP ) ) ;
             xgate = MKS_SLG * ( 1.0 + MKS_SLGL / pow( Lgate , SLGLP ) ) ;
             xsub1 = UC_SUB1 * ( 1.0 + MKS_SUB1L / pow( Lgate , SUB1LP ) ) ;
             xsub2 = UC_SUB2 * ( 1.0 + MKS_SUB2L / Lgate ) ;

             xsub1_ = xsub1;
             xsub2_ = xsub2;
             vg2const_ = vg2const;
             xvbs_  = xvbs;
             xgate_ = xgate;
             if ( COSNP ) begin // SVGS
                //vg2const_ = UC_SVGSSNP * ( 1.0e0 + MKS_SVGSL / pow( Lgate , SVGSLP ) )* ( T2 / ( T2 + MKS_SVGSW ) ) ;
                xsub1_ = UC_SUB1SNP * ( 1.0 + MKS_SUB1L / pow( Lgate , SUB1LP ) ) ;
                xsub2_ = UC_SUB2SNP * ( 1.0 + MKS_SUB2L / Lgate ) ;
                //xvbs_  = UC_SVBSSNP * ( 1.0 + MKS_SVBSL/ pow( Lgate , SVBSLP ) ) ; // 20180413
             end
             
             UC_SUBLD1 = SUBLD1 * ( 1.0 + SUBLD1L / pow( LG , SUBLD1LP ) ) ;
           end else begin
             vg2const = 0.0 ;  xvbs = 0.0 ; xgate = 0.0 ; 
             xsub1 = 0.0 ; xsub2 = 0.0 ; UC_SUBLD1 = 0.0 ;
            
             vg2const_ = 0.0; xvbs_ = 0.0; xgate_ = 0.0;
             xsub1_ = 0.0 ; xsub2_ = 0.0 ;

           end
                     
           // IBPC //
           UC_IBPC1  = ( UC_IBPC1 != 0.0 ) ? UC_IBPC1 * ( 1.0 + IBPC1L  / pow( LG , IBPC1LP ) ) : 0.0 ;
                     
           // Fringing capacitance //
           cfrng = `C_EOX / ( `C_Pi / 2.0e0 ) * weffcv_nf
                 * ln( 1.0e0 + TPOLY / TOX ) ;
                 
           // Additional term of lateral-field-induced capacitance //
           cqyb0 = ( XQY1 != 0.0 ) ? `C_m2um * weffcv_nf
                   * XQY1 / pow( LG , XQY2 ) : 0.0 ;
                 
           // Parasitic component of the channel current //
           ptl0 = PTL * pow( LG        , - PTLP ) ;
           pt40 = PT4 * pow( LG        , - PT4P ) ;
           gdl0 = GDL * pow( LG + UC_GDLD , - GDLP ) ;
                
           // Self heating //
           RTH = UC_RTH0 / ( Mfactor * weff_nf )
               * ( 1.0 + RTH0L / pow( LG , RTH0LP ) )
               * ( 1.0 + RTH0W / pow( WG , RTH0WP ) );
           CTH = MKS_CTH0 * ( Mfactor * weff_nf ) ;
           RTH = RTH * ( ( 1.0 / pow( NF , RTH0NF ) ) ) ;
               
           rthtemp0 = 1.0 / pow( NF , RTH0NF ) / ( Mfactor * weff_nf )
                    * ( 1.0 + RTH0L / pow( LG , RTH0LP ) )
                    * ( 1.0 + RTH0W / pow( WG , RTH0WP ) );
                    
           //-----------------------------------------------------------*
           //* Temperature dependent constants. 
           //*-----------------//
           if( COSELFHEAT == 0 || UC_RTH0 == 0.0 ) begin

             cnst0over = 0.0; cnst0overs= 0.0;
                      
`include "HSMHV_temp_eval.inc"

             if ( UC_CORDRIFT > 0) begin  
`include "HSMHV_temp_eval_rdris.inc"
`include "HSMHV_temp_eval_rdri.inc"
             end

`ifdef COEXTDIO 
       /* for Junction Diode Model */
`else 
`include "HSMHV_temp_eval_dio.inc"
`endif /* End of COEXTDIO */

                      
           end // end of if( tempNode < 0 || UC_RTH0 == 0.0 ) //
                    
         end
       end
     end
     if(FATAL_flag) $finish(0);
//   Initial Settings for each steps. 
              
//   Check 5 Terminals
//
//   get biases from CKT //
     Vdsi  = TYPE * V(BRdpsp) ;
     Vgsi  = TYPE * V(BRgpsp) ;
     Vbsi  = TYPE * V(BRbpsp) ;
     Vdsei = TYPE * V(BRds )  ;
     Vgsei = TYPE * V(BRgps ) ;
     Vbsei = TYPE * V(BRbps ) ;
     Vddp = TYPE * V(BRddp ) ;
     Vsps = TYPE * V(BRsps)  ;


//20160613 possible conflict still remains: should work it out --> Probably, OK. 
`ifdef COEXTDIO 
       /* for Junction Diode Model */
`else 
     vsbs = TYPE * V(BRsbs ) ;
     vdbd = TYPE * V(BRdbd ) ;
     vbpsp = TYPE * V(BRbpsp ) ;
     vbpdp = TYPE * V(BRbpdp ) ;
     vbs_jct = vsbs;
     vbd_jct = vdbd;
     vbsi_jct = vbpsp;
     vbdi_jct = vbpdp;
`endif /* End of COEXTDIO */

`ifdef _4_PORTS_CONNECTED_
     Vsubs = 0 ;
`else // 5 or 6 Terminals
     Vsubs = TYPE * V(sub,s) ;
`endif
//   NQS nodes
     if(flg_nqs) begin
       Qi_nqs = V(qi) ;
       Qb_nqs = V(qb) ;
     end else begin
       Qi_nqs = 0.0 ;
       Qb_nqs = 0.0 ;
     end
//
     vgd  = Vgsi -Vdsi ;
     vbd  = Vbsi -Vdsi ;
     
     if(Vdsi >= 0)  begin  // normal mode //
       mode = 1;
       Vds  = Vdsi ;
       Vgs  = Vgsi ;
       Vbs  = Vbsi ;
       Vdse = Vdsei ;
       Vgse = Vgsei ;
       Vbse = Vbsei ;
     end  else  begin     // reverse mode //
       mode = -1;
       Vds  = -Vdsi ;
       Vgs  =  vgd ;
       Vbs  =  vbd ;
       Vdse = -Vdsei ;
       Vgse = Vgsei - Vdsei ;
       Vbse = Vbsei - Vdsei ;
     end
          
     if( INFO >= 5 )  begin  // mode, bias conditions ... //
       $write( "--- variables given to HSMHVevaluate() ----\n" );
       $write( "TYPE   = %d\n" , TYPE );
       $write( "mode   = %d\n" , mode );
       $write( "Vbsei Vbsi    = %20.10e %20.10e\n" , Vbsei, Vbsi );
       $write( "Vdsei Vdsi    = %20.10e %20.10e\n" , Vdsei, Vdsi );
       $write( "Vgsei Vgsi    = %20.10e %20.10e\n" , Vgsei, Vgsi );
       $write( "Vsubs       = %20.10e \n" , Vsubs );

`ifdef COEXTDIO 
       /* for Junction Diode Model */
`else 
       $write( "vbsj vbdj   = %20.10e %20.10e\n" , vbs_jct, vbd_jct );
`endif /* End of COEXTDIO */
            
     end // if( INFO >= 5 )
          
     if( INFO >= 6 )  begin  // input flags //
       $write( "corsrd  = %d\n" , UC_CORSRD );
       $write( "coadov  = %d\n" , COADOV );
       $write( "coisub  = %d\n" , COISUB );
       $write( "coiigs  = %d\n" , COIIGS );
       $write( "cogidl  = %d\n" , COGIDL );
       $write( "coovlp  = %d\n" , COOVLP );
       $write( "coovlps = %d\n" , COOVLPS );
       $write( "coflick = %d\n" , COFLICK );
       $write( "coisti  = %d\n" , COISTI );
       $write( "conqs   = %d\n" , CONQS  );
       $write( "cothrml = %d\n" , COTHRML );
       $write( "coign   = %d\n" , COIGN );
       $write( "coselfheat = %d\n" , COSELFHEAT );
       $write( "cosym   = %d\n" , COSYM );
       $write( "cosubnode = %d\n" , COSUBNODE );
       $write( "cordrift = %d\n" , UC_CORDRIFT );
     end // if( INFO >= 6 )
          
//start_of_evaluation;
          
     //-----------------------------------------------------------*
     //* Temperature dependent constants.
     //*-----------------//
     if( COSELFHEAT > 0 && UC_RTH0 != 0.0 ) begin

       deltemp = V(temp) ;
       if( COSELFHEAT == 2 ) begin  
         // Clamping the maximum temperature rise (SHEMAX) //
         `Fn_SU( deltemp , deltemp , SHEMAX , SHEMAXDLT*10 ,  T0 ) 
       end 

       if( INFO >= 5 ) $write( "deltemp     = %20.10e \n" , deltemp );
            
`include "HSMHV_temp_eval.inc"

     end else begin
       TTEMP       = $temperature + DTEMP;
     end // else: !if( COSELFHEAT > 0 && UC_RTH0 != 0.0 )
`ifdef _AGING_
     keT = `C_KB / `C_QE * TTEMP ; // aging; trap
`endif
     // Metallurgical channel geometry //
     WeffLD_nf = weff_ld * NF ;
     Ldrift0 = LDRIFT1 +  LDRIFT2 ;
             
     // Flat band voltage //
`ifdef _AGING_
     Vfb = UC_VFBC + V(dVfb) + TRAPDVTH ;
`else
     Vfb = UC_VFBC ;
`endif
         
     // Velocity Temperature Dependence //
     Vmaxe = VMAXeff ;
          
     // c_eox: Permittivity in ox  //
     c_eox = cecox ;
           
     // Tox and Cox without QME //
     Tox0 = TOX ;
     Cox0 = c_eox / Tox0 ;
     Cox0_inv = 1.0 / Cox0 ;
     Coxb0 = c_eox / UC_TOXB ;
              
     // for calculation of Ps0_min //
     Vgs_min = TYPE * VGSMIN ;
             
     //---------------------------------------------------*
     //* Determine clamping limits for too large Vbs(internal).
     //*-----------------//
     `Fn_SU( T1 , Pb2  - VZADD0 , 0.8 , 0.1 , T0 ) 
     Vbs_max = T1 ;
     if( Pb20 - VZADD0 < Vbs_max ) begin
       Vbs_max = Pb20 - VZADD0 ;
     end
     if( Pb2c - VZADD0 < Vbs_max ) begin
       Vbs_max = Pb2c - VZADD0 ;
     end
     if( UC_CODEP > 0 && UC_CODEP <= 3 ) begin
       if( Pb2n - VZADD0 < Vbs_max ) begin
         Vbs_max = Pb2n - VZADD0 ;
       end
       if( Vbipn - VZADD0 < Vbs_max ) begin
         Vbs_max = Vbipn - VZADD0 ;
       end
     end

     if( Vbs_bnd > Vbs_max * 0.5 ) begin
       Vbs_bnd = 0.5 * Vbs_max ;
     end
        if(Rse > 0.0 || Rde > 0.0) begin         //HV1
          if ( UC_CORSRD == 1 ) flg_rsrd  = 1 ;  //HV1
          if ( UC_CORSRD == 2 ) flg_rsrd  = 2 ;  //HV1
          if ( UC_CORSRD == 3 ) flg_rsrd  = 3 ;  //HV1
        end                                      //HV1

     //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     //* PART-1: Basic device characteristics.
     //*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++//
     flg_pprv = 0 ;

                
     if( ( UC_CORDRIFT == 1 && COSUBNODE == 1 ) && 
         ( UC_NOVER * ( `NSUBSUB + UC_NOVER ) ) > 0 ) begin
       //----------------------------------------------------------*
       //* Considering these special cases:
       //* ( mode == HiSIM_NORMAL_MODE  && Vdse < 0.0 )
       //* ( mode == HiSIM_REVERSE_MODE && Vdse < 0.0 )
       //*----------------------------------------------------------//
       Vdsegmt = Vdsei ; // geometrical outer biases //
       if( Vdsegmt >= 0.0 ) begin // vdse normal mode //
         Vdserev = Vdsegmt ;
         Vsubsrev = Vsubs ;
       end else begin // vdse reverse mode //
         Vdserev = - Vdsegmt ;
         Vsubsrev = Vsubs - Vdsegmt ;
       end
       //-----------------------------------------------------------*
       //* Vxserevz: Modified bias introduced to realize symmetry at Vds=0.
       //*-----------------//
       `Fn_SymAdd( Vzadd , Vdserev / 2 , VZADD0 , T2 )
       if( Vzadd < `ps_conv ) begin
         Vzadd = `ps_conv ;
       end
       Vdserevz = Vdserev + 2.0 * Vzadd ;

       // external substrate node exists && LDMOS case: //
       // Substrate Effect //
       T0 = `VBISUB - `RDVDSUB * Vdserevz - `RDVSUB * Vsubsrev ;
       `Fn_SZ( T1, T0, 10.0, T2 ) 
       T1 = T1 + `epsm10 ;
       T0 = `NSUBSUB / ( UC_NOVER * ( `NSUBSUB + UC_NOVER ) ) ;
       T4 = 2 * `C_ESI / `C_QE * T0 ;
       Wdep = sqrt ( T4 * T1 ) + `Small ;
       `Fn_SU( Wdep, Wdep, `DDRIFT, `C_sub_delta* `DDRIFT, T0 ) 
     end else begin
       Wdep = 0.0 ;
     end
//HV1{
        if( flg_rsrd == 1 || flg_rsrd == 3 ) begin
              
          //----------------------------------------------------------*
          //* Considering these special cases:
          //* ( mode == HiSIM_NORMAL_MODE  && Vdse < 0.0 )
          //* ( mode == HiSIM_REVERSE_MODE && Vdse < 0.0 )
          //*----------------------------------------------------------//
          Vdsegmt = Vdsei ; // geometrical outer biases //
          Vgsegmt = Vgsei ;
          Vbsegmt = Vbsei ;
          if( Vdsegmt >= 0.0 ) begin // vdse normal mode //
            VdseModeNML = 1 ;
            VdseModeRVS = 0 ;
            Vdserev = Vdsegmt ;
            Vgserev = Vgsegmt ;
            Vbserev = Vbsegmt ;
            Vsubsrev = Vsubs ;
          end else begin // vdse reverse mode //
            VdseModeNML = 0 ;
            VdseModeRVS = 1 ;
            Vdserev = - Vdsegmt ;
            Vgserev = Vgsegmt - Vdsegmt ;
            Vbserev = Vbsegmt - Vdsegmt ;
            Vsubsrev = Vsubs - Vdsegmt ;
          end
          if( Rdvde > 0.0 || Rsvde > 0.0 || UC_RDVG11 > 0.0 || UC_RDVB > 0.0 || COSUBNODE == 1 ) begin
            //-----------------------------------------------------------*
            //* Vxserevz: Modified bias introduced to realize symmetry at Vds=0.
            //*-----------------//
            `Fn_SymAdd( Vzadd , Vdserev / 2 , VZADD0 , T2 ) 
            if( Vzadd < `ps_conv ) begin
              Vzadd = `ps_conv ;
            end
            Vdserevz = Vdserev + 2.0 * Vzadd ;
            Vgserevz = Vgserev + Vzadd ;
            Vbserevz = Vbserev + Vzadd ;
                     
            // bias-dependent Rdrift for HVMOS/LDMOS //
                     
            if( COSYM == 1 || VdseModeNML == 1 ) begin // HVMOS or normal mode LDMOS: //
              // ... Vdse dependence             //
              T1 = VdseModeNML * Rde + VdseModeRVS * Rse ;
              T0 = VdseModeNML * Rdvde + VdseModeRVS * Rsvde ;
              T4 = T1 + T0 * Vdserevz ;
                     
              // ... Vgse dependence             //
              `Fn_SZ( T10 , RDVG12 , `rdsz_dlt , T0 ) 
              T1  = T4 * ( 1.0 + UC_RDVG11 * ( 1.0 - Vgserevz   / T10 ) ) ;
              `Fn_SL2( T2 , T1 , T4 , `rdsl2_dlt , T0 , T5 ) 
              T3  = T4 * ( 1.0 + UC_RDVG11 ) ;
              `Fn_SU2( Rdrift , T2 , T3 , `rdsu2_dlt , T0, T5 ) 
                     
              // ... Vbse dependence             //
              T1 = 1.0 - UC_RDVB * Vbserevz ;
              `Fn_SZ( T3 , T1 , `rdsz_dlt , T4 ) 
              T3 = T3 + `Small ;
              T0 = Rdrift ;
              Rdrift = Rdrift       * T3 ;
                           
            end else begin // reverse mode LDMOS: //
              Rdrift = Rse ;
            end
                   
            // Rsdrift //
            T4 = ( VdseModeNML * Rse + VdseModeRVS * Rde ) ;
                  
            if( COSYM == 1 || VdseModeRVS == 1 ) begin // HVMOS or reverse mode LDMOS: //
              // ... Vdse dependence             //
              T0 = VdseModeNML * Rsvde + VdseModeRVS * Rdvde ;
              T4 = T4 + T0 * ( 2.0 * VZADD0 ) ;
                 
              // ... Vgse dependence             //
              T10 = RDVG12 + `Small ;
              T1  = T4 * ( 1.0 + UC_RDVG11 * ( 1.0 - Vgserevz   / T10 ) ) ;
              `Fn_SL2( T2 , T1 , T4 , `rdsl2_dlt , T0 , T5 ) 
              T3  = T4 * ( 1.0 + UC_RDVG11 ) ;
              `Fn_SU2( Rsdrift , T2 , T3 , `rdsu2_dlt , T0, T5 ) 
                     
              // ... Vbse dependence             //
              T1 = 1.0 - UC_RDVB * Vbserevz ;
              `Fn_SZ( T3 , T1 , `rdsz_dlt , T4 ) 
              T3 = T3 + `Small ;
              T0 = Rsdrift ;
              Rsdrift = Rsdrift       * T3 ;
            end else begin // LDMOS normal mode: //
              Rsdrift = Rse ;
            end
                    
            if( COSUBNODE == 1 && COSYM == 0 &&
              ( UC_NOVER * ( `NSUBSUB + UC_NOVER ) ) > 0 ) begin
              // external substrate node exists && LDMOS case: //
              // Substrate Effect //
              T0 = `VBISUB - `RDVDSUB * Vdserevz - `RDVSUB * Vsubsrev ;
              `Fn_SZ( T1, T0, 10.0, T2 ) 
              T1 = T1 + `epsm10 ;
              T0 = `NSUBSUB / ( UC_NOVER * ( `NSUBSUB + UC_NOVER ) ) ;
              T4 = 2 * `C_ESI / `C_QE * T0 ;
              Wdep = sqrt ( T4 * T1 ) + `Small ;
              `Fn_SU( Wdep, Wdep, `DDRIFT, `C_sub_delta* `DDRIFT, T0 ) 
              T6 = Ldrift0 / (`DDRIFT - Wdep) ;
                 
              T0 = Rdrift  * T6 ;
              T1 = Rsdrift * T6 ;
              Rdrift  = T0 * VdseModeNML + Rdrift  * VdseModeRVS ;
              Rsdrift = T1 * VdseModeRVS + Rsdrift * VdseModeNML ;
            end else begin
              Wdep = 0.0 ;
            end
            
            Rdd = Rdrift ;
            Rsd = Rsdrift ;
               
          end else begin // bias-independent Rs/Rd //
            Rdd = VdseModeNML * Rde + VdseModeRVS * Rse ;
            Rsd = VdseModeNML * Rse + VdseModeRVS * Rde ;
          end
             
          // Weff dependence of the resistances //
          Rdd = Rdd /  WeffLD_nf ;
          Rsd = Rsd /  WeffLD_nf ;
             
          // Sheet resistances are added. //
          Rdd = Rdd + VdseModeNML * Rd0 + VdseModeRVS * Rs0 ;
          Rsd = Rsd + VdseModeNML * Rs0 + VdseModeRVS * Rd0 ;
             
          // Re-stamps for hsmhvnoi.c //
          // Please see hsmhvnoi.c //
          T0 = VdseModeNML * Rdd + VdseModeRVS * Rsd ; // mode-dependent --> geometrical //
          if ( T0 > `Res_min && COTHRML != 0 ) drainConductance = Mfactor / T0 ;
          else drainConductance = 0.0 ;

          T0 = VdseModeNML * Rsd + VdseModeRVS * Rdd ; // mode-dependent --> geometrical //
          if ( T0 > `Res_min && COTHRML != 0 ) sourceConductance = Mfactor / T0 ;
          else sourceConductance = 0.0 ;
             
        end // end of case flg_rsrd=1 or flg_rsrd=3 //}HV1
        
     // Clamping for Vbs > Vbs_bnd //
     if( Vbs > Vbs_bnd ) begin
       T1 = Vbs - Vbs_bnd ;
       T2 = Vbs_max - Vbs_bnd ;
       `Fn_SUPoly4m( TY , T1 , T2 , VbscldVbs , T0 ) 
       Vbscl    = Vbs_bnd    + TY ;

       T3 = 1 / T2 ;
       T4 = T1 * T3 ;
       T5 = T4 * T4;
           
       T7 = 1 + T4 + T5 * ( 1 + T4 + T5 ) ;
       VbscldVbs = ( 1 + 2 * T4 + 3 * T5 + 4 * T4 * T5)/(T7 * T7) ;
     end  else begin
       Vbscl     = Vbs ;
       VbscldVbs = 1.0 ;
     end

     //-----------------------------------------------------------*
     //* Vxsz: Modified bias introduced to realize symmetry at Vds=0.
     //*-----------------//
     T1 = VbscldVbs * Vds / 2 ;
     `Fn_SymAdd(  Vzadd , T1 , VZADD0 , T2 ) 
     if( Vzadd < `ps_conv ) begin
         Vzadd = `ps_conv ;
     end
     Vbsz = Vbscl + Vzadd ;
     Vdsz = Vds + 2.0 * Vzadd ;
     Vgsz = Vgs + Vzadd ;
          
     //---------------------------------------------------*
     //* Factor of modification for symmetry.
     //*-----------------//
     T1 = qnsub_esi * Cox0_inv * Cox0_inv ;
     T2 = Vgs - Vfb ;
     T3 = 1 + 2.0 / T1 * ( T2 - 1.0 / betatnom - Vbscl ) ;
     `Fn_SZ( T4 , T3 , 1e-3 , T5 ) 
     T4 = T4 + `Small ;
     TX = sqrt( T4 ) ;
     Pslsat = T2 + T1 * ( 1.0 - TX ) ;
     VdsatS = Pslsat - Pb2c ;
     `Fn_SL( VdsatS , VdsatS , 0.1 , 5e-2 , T6 ) 
     T1 = Vds / VdsatS ;
     `Fn_SUPoly4( TX , T1 , 1.0 , T0 ) 
     FMDVDS = TX * TX ;
        
     //-----------------------------------------------------------*
     //* Quantum effect
     //*-----------------//
     if( flg_qmetemp == 0 ) begin
       flg_qme = 0 ;
     end else begin
       flg_qme = 1 ;
     end
     T1 = qnsub_esi2 ;
     T2 = sqrt( T1 * Pb20 ) ;
     Vthq = Pb20 + Vfb + T2 * Cox0_inv ;
     if( flg_qme == 0 ) begin
       Toxe = Tox0 ;
       Cox = Cox0 ;
       Cox_inv  = Cox0_inv ;
       T0 = cnst0 * cnst0 * Cox_inv ;
       cnstCoxi = T0 * Cox_inv ;
     end else begin
       T5 = Vgs  - Vbs - Vthq + QME2 ;
       `Fn_SZ( T2 , T5 , `qme_dlt, T3) 
       T2 = T2 + `Small ;
       T3 = 1.0 /  T2 ;
       T7 = -1.0 / ( T2 * T2 ) ;
       T4 = 2.0 * abs(Vthq) ;
       T6 = T5 - Vgs + Vfb ;
       if(T6 > T4) begin T4 = T6; end
       `Fn_SU( T2 , T3 , 1.0 / T4  , `qme_dlt, T6 ) 
       dTox = QME1 * T2 + QME3 ;
       T7   = QME1 ;
       if( dTox * 1.0e12 < Tox0 ) begin
         dTox = 0.0 ;
         flg_qme = 0 ;
       end
       Toxe = Tox0 + dTox ;
       Cox = c_eox / Toxe ;
       T1 = - c_eox / ( Toxe * Toxe ) ;
       Cox_inv  = Toxe / c_eox ;
       T1 = 1.0 / c_eox ;
       T0 = cnst0 * cnst0 * Cox_inv ;
       cnstCoxi = T0 * Cox_inv ;
     end
        
     //---------------------------------------------------*
     //* Vbsz2 : Vbs for dVth
     //*-----------------//
     Vbsz2 = Vbsz ;
           
     //---------------------------------------------------*
     //* Vthp : Vth with pocket.
     //*-----------------//
     T1 = qnsub_esi2 ;
     Qb0 = sqrt (T1 * (Pb20 - Vbsz2)) ;
     T2 = 0.5 * T1 / Qb0 ;
     Vthp = Pb20 + Vfb + Qb0 * Cox_inv + ptovr;
     Pb20b = Pb20 ;
     T0 = 0.95 ;
     T4 = (UC_CODEP > 1) ? 0.0 : 1.0 ;
     T1 = T0 * Pb20b - T4 * Vbsz2 - 1.0e-3 ;
     T2 = sqrt (T1 * T1 + 4.0 * T0 * Pb20b * 1.0e-3) ;
     T3 = T0 * Pb20b - 0.5 * (T1 + T2) ;
     T5 = (UC_CODEP == 1) ? DEPETA * Vdsz : 0.0 ;
     Pbsum = Pb20b - T3 + T5 ; 

     sqrt_Pbsum = sqrt( Pbsum ) ;

             
     //-------------------------------------------*
     //* dVthLP : Short-channel effect induced by pocket.
     //* - Vth0 : Vth without pocket.
     //*-----------------//
     if( LP != 0.0 ) begin
       T1 = qnsub_esi2 ;
       T2 = BS2 - Vbsz2 ;
       T3 = T2 + `Small ;
       T4 = sqrt (T3 * T3 + 4.0 * `vth_dlt) ;
       T5 = 0.5 * (T3 + T4) ;
       T6 = 0.5 * (1.0 + T3 / T4) ;
       T7 = 1.0 / T5 ;
       bs12 = BS1 * T7 ;
       T8 = - bs12 * T7 ;
       `Fn_SU( T10 , Vbsz2 + bs12, 0.93 * Pb20, `vth_dlt, T0) 
       Qbmm = sqrt (T1 * (Pb20 - T10 )) ;
       T9 = T0 / Qbmm ;
       dqb = (Qb0 - Qbmm) * Cox_inv ;
       T1 = 2.0 * `C_QE * EF_NSUBC * `C_ESI ;
       T2 = sqrt( T1 * ( Pb2c - Vbsz2 ) ) ;
       Vth0 = Pb2c + Vfb + T2 * Cox_inv ;
       T3 = 0.5 * T1 / T2 * Cox_inv ;
       T1 = `C_ESI * Cox_inv ;
       T2 = wdplp ;
       T4 = 1.0e0 / ( LP * LP ) ;
       T5 = 2.0e0 * ( VBI - Pb20b ) * T1 * T2 * T4 ;
       dVth0 = T5 * sqrt_Pbsum ;
       T6 = 0.5 * T5 / sqrt_Pbsum ;
       T7 = 2.0e0 * ( VBI - Pb20b ) * `C_ESI * T2 * T4 * sqrt_Pbsum ;
       T8 = - 2.0e0 * T1 * T2 * T4 * sqrt_Pbsum ;
       T1 = Vthp - Vth0 ;
       T2 = UC_SCP1 + UC_SCP3 * Pbsum / LP ;
       T3 = T2 + UC_SCP2 * Vdsz ;
       Vdx = SCP21 + Vdsz ;
       Vdx2 = Vdx * Vdx ;
       dVthLP = T1 * dVth0 * T3 + dqb - msc / Vdx2 ;
     end else begin
       dVthLP = 0.0e0 ;
     end
            
     //---------------------------------------------------*
     //* dVthSC : Short-channel effect induced by Vds.
     //*-----------------//

     T1 = `C_ESI * Cox_inv ;
     T2 = wdpl ;
     T3 = Lgate - PARL2 ;
     T4 = 1.0e0 / ( T3 * T3 ) ;
     T5 = 2.0e0 * ( VBI - Pb20b ) * T1 * T2 * T4 ;
     dVth0 = T5 * sqrt_Pbsum ;
     T6 = T5 / 2.0 / sqrt_Pbsum ;
     T7 = 2.0e0 * ( VBI - Pb20b ) * `C_ESI * T2 * T4 * sqrt_Pbsum ;
     T8 = - 2.0e0 * T1 * T2 * T4 * sqrt_Pbsum ;
     T1 = UC_SC3 / Lgate ;
     T4 = UC_SC1 + T1 * Pbsum ;
     T5 = T4 + UC_SC2 * Vdsz * ( 1.0 +  SC4 * Pbsum );
     dVthSC = dVth0 * T5 ;
            
     //---------------------------------------------------*
     //* dVthW : narrow-channel effect.
     //*-----------------//
     T1 = 1.0 / Cox ;
     T2 = T1 * T1 ;
     T3 = 1.0 / ( Cox +  UC_WFC / Weff ) ;
     T4 = T3 * T3 ;
     T5 = T1 - T3 ;
     T6 = Qb0 * ( T2 - T4 ) ;
     dVthW = Qb0 * T5 + UC_WVTH0 / WG ;
           
     //---------------------------------------------------*
     //* dVth : Total variation.
     //* - Positive dVth means the decrease in Vth.
     //*-----------------//
     dVth = dVthSC + dVthLP + dVthW + dVthsm ;
          
     //---------------------------------------------------*
     //* Vth : Threshold voltage for OP.
     //*-----------------//
     T2 = sqrt( qnsub_esi2 * (Pb2-Vbsz) ) ;
     Vth = Pb2 + Vfb + T2 * Cox0_inv - dVth ;
         
     //-----------------------------------------------------------*
     //* Constants in the equation of Ps0 .
     //*-----------------//
     fac1 = cnst0 * Cox_inv ;
     fac1p2 = fac1 * fac1 ;
            
     //---------------------------------------------------*
     //* Poly-Depletion Effect
     //*-----------------//
     dPpg = 0.0 ;
     if( flg_pgd == 1 ) begin
       T7 = Vgsz ;
       T0 = cnstpgd ;
       T3 = T7 - PGD2 ;
       `Fn_ExpLim( dPpg , T3 , T6 ) 
       `Fn_SZ( dPpg , dPpg - 1.0 , `pol_dlt , T6 ) 
       dPpg = dPpg * T0 ;
       `Fn_SU( dPpg , dPpg , `pol_b , `pol_dlt , T9 ) 
            
     end
          
     //---------------------------------------------------*
     //* Vgp : Effective gate bias with SCE & RSCE & flatband.
     //*-----------------//
     Vgp = Vgs - Vfb + dVth - dPpg ;

     //---------------------------------------------------*
     //* Vgs_fb : Actual flatband voltage taking account Vbscl.
     //* - note: if Vgs == Vgs_fb then Vgp == Ps0 == Vbscl .
     //*------------------//
     Vgs_fb = Vfb - dVth + dPpg + Vbscl ;
            
`ifdef _AGING_
     //-----------------------------------------------------------*
     //* initilization for Trap simulation.
     //*-----------------//
     Ids_trap  = 0.0 ;
     Isub_trap = 0.0 ;
     keT       = `C_KB / `C_QE * TTEMP ;       
     Qt0       = 0.0 ;
     DVddp     = 0.0 ;
     AVddp     = Vdse - Vds ;
     pretime0  = (DEGTIME0 > 0 ) ? DEGTIME0 * 0.01 : `pretime ;
     //*-----------------//
     Nt0s_temp = 0.0 ;
     Nt_max0 = 0.0 ;
     Nt_maxl = 0.0 ;
     MKS_TRAPGC1 = TRAPGC1 / `C_cm2m_p3 ;
     MKS_TRAPES1 = TRAPES1 ;
     MKS_TRAPGC0 = TRAPGC0 / `C_cm2m_p3 ;
     MKS_TRAPGCLIM = TRAPGCLIM / `C_cm2m_p3 ;
     MKS_TRAPGC2 = TRAPGC2 / `C_cm2m_p3 ;
     MKS_GC1MAX  = TRAPGC1MAX / `C_cm2m_p3 ;
     if( TRAPGC1MAX == 0 ) begin
       nt0s      = 0.0 ;
       ntls      = 0.0 ;
     end else begin
       `Fn_calcNKT(NtK20,MKS_TRAPGC2,TRAPES2)
       `Fn_calcNKT(NtK30,MKS_TRAPGC0,TRAPES0)
       `Fn_calcNKT(NtK40,MKS_TRAPGCLIM,TRAPESLIM)

       //---------------------------------------------------*
       //* Trap estimation.
       //*-----------------//
       if ( analysis("tran") == 0 ) begin
         Ps0_prv     = V(ps0) ; 
         Vtraplx_prv = V(vtraplx) ; 
       end
       if ( CODEG == 1 ) begin // update NtK10 
         if( CODEGSTEP == 0 ) begin
            MKS_TRAPGC1 = V(vgc1) * 1e24 ; 
            MKS_TRAPES1 = V(ve1) ; 
         end else begin
            MKS_TRAPGC1 = gc1 ; 
            MKS_TRAPES1 = e1  ; 
         end
       end
       `Fn_SL(MKS_TRAPES1, MKS_TRAPES1, 1e-6 , 1e-3 , T0)
       `Fn_calcNKT(NtK10,MKS_TRAPGC1,MKS_TRAPES1) 
       `Fn_calc_TRAP( Nt_maxl , NtK10 , NtK20, NtK30, NtK40, Ps0_prv , 0 ) 
       Nt_max0 = Nt_maxl ; 
       exp_time = (CODEG == 1 && TRAPTAUCAP > 0.0) ? lexp( - 0.5 * DEGTIME / TRAPTAUCAP ) : 0.0 ; 
       nt0s  = ( Nt_max0 * (1.0 + Nt0s_temp) ) * ( 1 - exp_time ) ; 
       ntls  = nt0s ; 
     end // end of Trap estimation

`ifdef _AGING_DEBUG_
     $write( "debugz %m Vgs %g Vgp %g NtK10 %g NtK20 %g Nt_max0 %g nt0s %g MKS_TRAPGC1 %g MKS_TRAPES1 \n",
     Vgs, Vgp, NtK10 , NtK20, Nt_max0, nt0s , MKS_TRAPGC1 , MKS_TRAPES1 ) ;
     $write( "debugz1 abstime %g %m nt0s %g Isub_trap %g \n", $abstime, nt0s, Isub_trap );
`endif

`endif // _AGING_

     if( UC_CODEP != 0 ) begin

     //---------------------------------------------------*
     //* depletion MOS mode  
     //*------------------//

       case (CODEP)
       1: begin // 2.2.0 old Model
`include "HSMHV_depmos.inc"
          end
       2: begin // 2.3.0 new Model
`include "HSMHV_depmos2.inc"
          end
       3: begin // 2.4.0 new Model
`include "HSMHV_depmos3.inc"
          end
       endcase // case (CODEP)

     end else begin

     //-----------------------------------------------------------*
     //  normal mode 
     //* Accumulation zone. (zone-A)
     //* - evaluate basic characteristics and exit from this part.
     //*-----------------//
       if( Vgs < Vgs_fb ) begin
         flg_zone = -1 ;
                
       //---------------------------------------------------*
       //* Evaluation of Ps0.
       //* - Psa : Analytical solution of
       //*             Cox( Vgp - Psa ) = cnst0 * Qacc
       //*         where Qacc is the 3-degree series of(fdep)^begin1/2end.
       //*         The unknown  is transformed to Chi=beta(Ps0-Vbs).
       //* - Ps0_min : |Ps0_min| when Vbs=0.
       //*-----------------//
       // Ps0_min: approx. solution of Poisson equation at Vgs_min //
       //          ( easy to improve, if necessary  )              //
       //    Ps0_min = Eg - Pb2 ;
       //    Ps0_min_dT = Eg_dT - Pb2_dT ;
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
         if( Ac4 < Ac3*1.0e-8 ) begin
           Ac1 = 0.5 * Ac4 / Ac31 ;
         end else begin
           Ac2 = sqrt( Ac4 + Ac3 ) ;
           Ac1 = -Ac31 + Ac2 ;
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
          
       //---------------------------------------------------*
       //* Characteristics.
       //*-----------------//
         Psl = Ps0 ;
           
         Pds = 0.0 ;
         T2 = ( Vgp - Ps0 ) ;
         Qbu = Cox * T2 ;
         Qiu = 0.0e0 ;
         Qdrat = 0.0e0 ;
         Lred = 0.0e0 ;
         Ids = 0.0e0 ;
         VgVt = 0.0 ;
         flg_noqi = 1 ;
         end_of_part_1 = 1 ;
       end
                                if(end_of_part_1 == 0) begin
                   
     //---------------------------------------------------*
     //* Ps0_iniA: solution of subthreshold equation assuming zone-D1/D2.
     //*-----------------//
       TX = 1.0e0 + 4.0e0 * ( beta * ( Vgp - Vbscl ) - 1.0e0 ) / ( fac1p2 * beta2 ) ;
       TX = `Fn_Max( TX , `epsm10 ) ;
       Ps0_iniA = Vgp + fac1p2 * beta * 0.5 * ( 1.0e0 - sqrt( TX ) ) ;
              
       if( flg_pprv == 0 ) begin
                
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
         //* Weak inversion zone.
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
           Ps0_iniB = ln ( T2 ) / T3 ;
           `Fn_SU( Ps0_ini , Ps0_iniA, Ps0_iniB, `c_ps0ini_2, T1) 
         end
       end
       TX = Vbscl + `ps_conv / 2 ;
       if ( Ps0_ini < TX ) Ps0_ini = TX ;
        
     //---------------------------------------------------*
     //* Assign initial guess.
     //*-----------------//
       Ps0 = Ps0_ini ;
       Psl_lim = Ps0_iniA ;

`ifdef _AGING_
       ftr0_qs = 0.0 ; ftr0_qs_dPs0 = 0.0 ;
       ftrl_qs = 0.0 ; ftrl_qs_dPsl = 0.0 ;
`endif             

     //---------------------------------------------------*
     //* Calculation of Ps0. (beginning of Newton loop)
     //* - Fs0 : Fs0 = 0 is the equation to be solved.
     //* - dPs0 : correction value.
     //*-----------------//
       exp_bVbs = exp( beta * Vbscl ) ;
       cfs1 = cnst1 * exp_bVbs ;
       flg_conv = 0 ;
       for ( lp_s0 = 1 ; lp_s0 <= lp_s0_max + 1 ; lp_s0 = lp_s0 + 1 ) begin
`ifdef _AGING_
         if( nt0s > 0.0 ) begin
           T1 = ( Ps0 - Vbs ) * 0.5 ;
           T2 = beta / Nsub * nt0s ;
           ftr0_qs      = T2 * T1 ;
           ftr0_qs_dPs0 = T2 * 0.5 ;
           if( ftr0_qs < 0.0 ) begin
               ftr0_qs = 0.0 ; ftr0_qs_dPs0 = 0.0 ;
           end
         end
`endif
         Chi = beta * ( Ps0 - Vbscl ) ;
         if( Chi < `znbd5 ) begin
         //-------------------------------------------*
         //* zone-D1/D2.  (Ps0)
         //* - Qb0 is approximated to 5-degree polynomial.
         //*-----------------//
           fi = Chi * Chi * Chi * ( `cn_im53 + Chi * ( `cn_im54 + Chi * `cn_im55 ) ) ;
           fi_dChi = Chi * Chi * ( 3 * `cn_im53 + Chi * ( 4 * `cn_im54 + Chi * 5 * `cn_im55 ) ) ;
           fs01 = cfs1 * fi * fi ;
           fs01_dPs0 = cfs1 * beta * 2 * fi * fi_dChi ;
           fb = Chi * ( `cn_nc51 + Chi * ( `cn_nc52 + Chi * ( `cn_nc53 + Chi * ( `cn_nc54 + Chi * `cn_nc55 ) ) ) ) ;
           fb_dChi = `cn_nc51
                 + Chi * ( 2 * `cn_nc52
                 + Chi * ( 3 * `cn_nc53
                 + Chi * ( 4 * `cn_nc54 + Chi * 5 * `cn_nc55 ) ) ) ;
`ifdef _AGING_
           fs02 = sqrt( fb * fb + fs01 + ftr0_qs );
           fs02_dPs0 = ( beta * fb_dChi * 2 * fb + fs01_dPs0 + ftr0_qs_dPs0 ) / ( fs02 + fs02 ) ;
`else
           fs02 = sqrt( fb * fb + fs01 ) ;
           fs02_dPs0 = ( beta * fb_dChi * 2 * fb + fs01_dPs0 ) / ( fs02 + fs02 ) ;
`endif
         end else begin
         //-------------------------------------------*
         //* zone-D3.  (Ps0)
         //*-----------------//
           if( Chi < `large_arg ) begin // avoid exp_Chi to become extremely large //
             exp_Chi = exp( Chi ) ;
             fs01 = cfs1 * ( exp_Chi - 1.0e0 ) ;
             fs01_dPs0 = cfs1 * beta * ( exp_Chi ) ;
           end else begin
             exp_bPs0 = exp( beta*Ps0 ) ;
             fs01     = cnst1 * ( exp_bPs0 - exp_bVbs ) ;
             fs01_dPs0 = cnst1 * beta * exp_bPs0 ;
           end
`ifdef _AGING_
           fs02 = sqrt( Chi - 1.0 + fs01 + ftr0_qs ) ;
           fs02_dPs0 = ( beta + fs01_dPs0  + ftr0_qs_dPs0 ) / ( fs02 + fs02 ) ;
`else
           fs02 = sqrt( Chi - 1.0 + fs01 ) ;
           fs02_dPs0 = ( beta + fs01_dPs0 ) / ( fs02 + fs02 ) ;
`endif
         end // end of if( Chi ... ) else block //
         Fs0 = Vgp - Ps0 - fac1 * fs02 ;
         Fs0_dPs0 = - 1.0e0 - fac1 * fs02_dPs0 ;
         if( flg_conv == 1 ) flg_brk1 = 1 ; // break
                                  if(flg_brk1 == 0) begin
         dPs0 = - Fs0 / Fs0_dPs0 ;
            
       //-------------------------------------------*
       //* Update Ps0 .
       //* - clamped to Vbscl if Ps0 < Vbscl .
       //*-----------------//
         dPlim = 0.5*`dP_max*(1.0 + `Fn_Max(1.0e0,abs(Ps0))) ;
         if ( abs( dPs0 ) > dPlim ) dPs0 = dPlim * `Fn_Sgn( dPs0 ) ;
         Ps0 = Ps0 + dPs0 ;
           
       //-------------------------------------------*
       //* Check convergence.
       //* NOTE: This condition may be too rigid.
       //*-----------------//
         if( abs( dPs0 ) <= `ps_conv && abs( Fs0 ) <= `gs_conv ) begin
           flg_conv = 1 ;
         end
                
                                  end if(flg_brk1)  lp_s0 = lp_s0_max + 1 ; flg_brk1 = 0; // end of flg_brk1
       end // end of Ps0 Newton loop //
            
     // Reduce loop count to exclude the sweep for derivative calculation //
       lp_s0 = lp_s0 - 1 ;
           
     //-------------------------------------------*
     //* Procedure for diverged case.
     //*-----------------//
       if( flg_conv == 0 ) begin
         $write( "*** warning(HiSIM_HV(%m)): Went Over Iteration Maximum(Ps0)\n" ) ;
         $write( " Vbse   = %7.3f Vdse = %7.3f Vgse = %7.3f\n" ,Vbse , Vdse , Vgse ) ;
       end
           
       if( Chi < `znbd5 ) begin
       //-------------------------------------------*
       //* zone-D1/D2. (Ps0)
       //* Xi0 := fdep0^2 = fb * fb  [D1,D2]
       //*-----------------//
         Xi0 = fb * fb + `epsm10 ;
         Xi0p12 = fb + `epsm10 ;
         Xi0p32 = fb * fb * fb + `epsm10 ;
       end else begin
       //-------------------------------------------*
       //* zone-D3. (Ps0)
       //*-----------------//
         flg_zone = 3 ;
         flg_noqi = 0 ;
                
       //-----------------------------------*
       //* Xi0 := fdep0^2 = Chi - 1 = beta * ( Ps0 - Vbscl ) - 1 [D3]
       //*-----------------//
         Xi0 = Chi - 1.0e0 ;
         Xi0p12 = sqrt( Xi0 ) ;
         Xi0p32 = Xi0 * Xi0p12 ;
       end // end of if( Chi  ... ) block //
        
     //-----------------------------------------------------------*
     //* Qb0 : Qb at source side.
     //* Qn0 : Qi at source side.
     //*-----------------//
`ifdef _AGING_
       Qt0 = (ftr0_qs <= 0) ? 0.0 : cnst0 * sqrt( ftr0_qs ) ;
`endif
       Qb0 = cnst0 * Xi0p12 ;
       T1 = 1.0 / ( fs02 + Xi0p12 ) ;
       Qn0 = cnst0 * fs01 * T1 + `Small ;

        
     //-----------------------------------------------------------*
     //* zone-D1 and D2
     //*-----------------//
       if( Chi < `znbd5 ) begin
         if( Chi < `znbd3 ) begin
         //-------------------------------------------*
         //* zone-D1. (Ps0)
         //*-----------------//
           flg_zone = 1 ;
           flg_noqi = 1 ; //* !! to be revisited !! *//
           Qiu = Qn0 ;
           Qbu = Qb0 ;
           Qdrat = 0.5 ;
           Lred = 0.0e0 ;
              
         end else begin
         //-------------------------------------------*
         //* zone-D2 (Ps0)
         //*-----------------//
           flg_zone = 2 ;
           flg_noqi = 0 ;
         //-----------------------------------------------------------*
         //* FD2 : connecting function for zone-D2.
         //* - Qiu, Qbu, Qdrat and Lred should be interpolated later.
         //*-----------------//
           T1 = 1.0 / ( `znbd5 - `znbd3 ) ;
           TX = T1 * ( Chi - `znbd3 ) ;
           FD2 = TX * TX * TX * ( 10.0 + TX * ( -15.0 + TX * 6.0 ) ) ;
            
         end // end of zone-D2 //
       end
        
     //---------------------------------------------------*
     //* VgVt : Vgp - Vth_qi. ( Vth_qi is Vth for Qi evaluation. )
     //*-----------------//
       VgVt = Qn0 * Cox_inv ;
          
     //-----------------------------------------------------------*
     //* make Qi=Qd=Ids=0 if VgVt <= VgVt_Small
     //*-----------------//
       if( VgVt <= `VgVt_Small && BYPASS_ENABLE ) begin
         flg_zone = 4 ;
         flg_noqi = 1 ;
         Psl = Ps0 ;
         Pds = 0.0 ;
         Qbu = Qb0 ;
         Qiu = 0.0 ;
         Qdrat = 0.5 ;
         Lred = 0.0 ;
         Ids = 0.0e0 ;
         end_of_part_1 = 1 ;
       end
                                if(end_of_part_1 == 0) begin
                   
     //-----------------------------------------------------------*
     //* Start point of Psl(= Ps0 + Pds) calculation.(label)
     //*-----------------//
// start_of_Psl:
                   
     // Vdseff(begin) //
       Vdsorg = Vds ;
       T2 = qnsub_esi / ( Cox * Cox ) ;
       T0 = Vgp - beta_inv - Vbsz ;
       T1 = 1.0e0 + 2.0e0 / T2 * T0 ;
       `Fn_SZ( T9, T1, 1e-3, TX )
       T9 = T9 + `Small ;
       T3 = sqrt( T9 ) ;
       T10 = Vgp + T2 * ( 1.0e0 - T3 ) ;
       `Fn_SZ( T10 , T10 , 0.01 , T0 )
       T10 = T10 + `epsm10 ;
       T1 = Vds / T10 ;
       T2 = `Fn_Pow( T1 , DDLTe - 1.0 ) ;
       T3 = 1.0 + T2 * T1 ;
       T4 = `Fn_Pow( T3 , 1.0 / DDLTe - 1.0 ) ;
       T6 = T4 * T3 ;
       Vdseff = Vds / T6 ;  
       Vds = Vdseff ;
     // Vdseff(end) //
      
       exp_bVbsVds = exp( beta * ( Vbscl - Vds ) ) ;
                 
     //---------------------------------------------------*
     //* Skip Psl calculation when Vds is very Small.
     //*-----------------//
       if( Vds <  0.0 ) begin
         Pds = 0.0 ;
         Psl = Ps0 ;
//       flg_conv = 1 ;
         start_of_loopl = 1 ;
       end
                                if(start_of_loopl == 0) begin
                    
     //-----------------------------------------------------------*
     //* Initial guess for Pds( = Psl - Ps0 ).
     //*-----------------//
       if( flg_pprv == 0 ) begin
       //---------------------------------------------------*
       //* Analytical initial guess.
       //*-----------------//
         Pds_max = `Fn_Max( Psl_lim - Ps0 , 0.0e0 ) ;
         `Fn_SU( Pds_ini , Vds, (1.0e0 + `c_pslini_1) * Pds_max, `c_pslini_2, T1 ) 
         Pds_ini = `Fn_Min( Pds_ini , Pds_max ) ;
       end
       if ( Pds_ini < 0.0 ) Pds_ini = 0.0 ;
       else if ( Pds_ini > Vds ) Pds_ini = Vds ;
             
     //---------------------------------------------------*
     //* Assign initial guess.
     //*-----------------//
       Pds = Pds_ini ;
       Psl = Ps0 + Pds ;
         
     //---------------------------------------------------*
     //* Calculation of Psl by solving Poisson eqn.
     //* (beginning of Newton loop)
     //* - Fsl : Fsl = 0 is the equation to be solved.
     //* - dPsl : correction value.
     //*-----------------//
       flg_conv = 0 ;
              
     //---------------------------------------------------*
     //* start of Psl calculation. (label)
     //*-----------------//
                                end if(start_of_loopl)  start_of_loopl = 0; // end of start_of_loopl
// start_of_loopl:
       for ( lp_sl = 1 ; lp_sl <= `lp_sl_max + 1 ; lp_sl = lp_sl + 1 ) begin
`ifdef _AGING_
         if( ntls > 0.0 ) begin
           T1 = ( Psl - Vbs ) * 0.5 ;
           T2 = beta / Nsub * ntls ;
           ftrl_qs      = T2 * T1 ;
           ftrl_qs_dPsl = T2 * 0.5 ;
           if( ftrl_qs < 0.0 ) begin
                 ftrl_qs = 0.0 ; ftrl_qs_dPsl = 0.0 ;
           end
         end
`endif

         Chi = beta * ( Psl - Vbscl ) ;
         if( Chi  < `znbd5 ) begin
         //-------------------------------------------*
         //* zone-D2.  (Psl)
         //* - Qb0 is approximated to 5-degree polynomial.
         //*-----------------//
           fi = Chi * Chi * Chi * ( `cn_im53 + Chi * ( `cn_im54 + Chi * `cn_im55 ) ) ;
           fi_dChi = Chi * Chi * ( 3 * `cn_im53 + Chi * ( 4 * `cn_im54 + Chi * 5 * `cn_im55 ) ) ;
           cfs1 = cnst1 * exp_bVbsVds ;
           fsl1 = cfs1 * fi * fi ;
           fsl1_dPsl = cfs1 * beta * 2 * fi * fi_dChi ;
           fb = Chi * ( `cn_nc51 + Chi * ( `cn_nc52 + Chi * ( `cn_nc53 + Chi * ( `cn_nc54 + Chi * `cn_nc55 ) ) ) ) ;
           fb_dChi = `cn_nc51
                 + Chi * ( 2 * `cn_nc52
                 + Chi * ( 3 * `cn_nc53
                 + Chi * ( 4 * `cn_nc54 + Chi * 5 * `cn_nc55 ) ) ) ;
`ifdef _AGING_
           fsl2 = sqrt( fb * fb + fsl1 + ftrl_qs ) ;
           fsl2_dPsl = ( beta * fb_dChi * 2 * fb + fsl1_dPsl + ftrl_qs_dPsl ) / ( fsl2 + fsl2 ) ;
`else
           fsl2 = sqrt( fb * fb + fsl1 ) ;
           fsl2_dPsl = ( beta * fb_dChi * 2 * fb + fsl1_dPsl ) / ( fsl2 + fsl2 ) ;
`endif
         end else begin
         //-------------------------------------------*
         //* zone-D3.  (Psl)
         //*-----------------//
           Rho = beta * ( Psl - Vds ) ;
           exp_Rho = exp( Rho ) ;
           fsl1 = cnst1 * ( exp_Rho - exp_bVbsVds ) ;
           fsl1_dPsl = cnst1 * beta * ( exp_Rho ) ;
           Xil = Chi - 1.0e0 ;
`ifdef _AGING_
           fsl2 = sqrt( Xil + fsl1  + ftrl_qs ) ;
           fsl2_dPsl = ( beta + fsl1_dPsl + ftrl_qs_dPsl ) / ( fsl2 + fsl2 ) ;
`else
           fsl2 = sqrt( Xil + fsl1 ) ;
           fsl2_dPsl = ( beta + fsl1_dPsl ) / ( fsl2 + fsl2 ) ;
`endif
         end
         Fsl = Vgp - Psl - fac1 * fsl2 ;
         Fsl_dPsl = - 1.0e0 - fac1 * fsl2_dPsl ;
         if( flg_conv == 1 ) flg_brk2 = 1 ; // break
                                  if(flg_brk2 == 0) begin
         dPsl = - Fsl / Fsl_dPsl ;
            
       //-------------------------------------------*
       //* Update Psl .
       //* - clamped to Vbscl if Psl < Vbscl .
       //*-----------------//
         dPlim = 0.5*`dP_max*(1.0 + `Fn_Max(1.0e0,abs(Psl))) ;
         if ( abs( dPsl ) > dPlim ) dPsl = dPlim * `Fn_Sgn( dPsl ) ;
         Psl = Psl + dPsl ;
         
       //-------------------------------------------*
       //* Check convergence.
       //* NOTE: This condition may be too rigid.
       //*-----------------//
         if( abs( dPsl ) <= `ps_conv && abs( Fsl ) <= `gs_conv ) begin
           flg_conv = 1 ;
         end
                                  end if(flg_brk2)  lp_sl = `lp_sl_max + 1 ; flg_brk2 = 0; // end of flg_brk2
       end // end of Psl Newton loop //
              
     // Reduce loop count to exclude derivative calculation sweep //
       lp_sl = lp_sl - 1 ;
           
     //-------------------------------------------*
     //* Procedure for diverged case.
     //*-----------------//
       if( flg_conv == 0 ) begin
         $write( "*** warning(HiSIM_HV(%m)): Went Over Iteration Maximum(Psl)\n" ) ;
         $write( " Vbse   = %7.3f Vdse = %7.3f Vgse = %7.3f\n" ,Vbse , Vdse , Vgse ) ;
       end
           
       if( Chi < `znbd5 ) begin
       //-------------------------------------------*
       //* zone-D1/D2. (Psl)
       //*-----------------//
         Xil = fb * fb + `epsm10 ;
         Xilp12 = fb + `epsm10 ;
         Xilp32 = fb * fb * fb + `epsm10 ;
       end else begin
       //-------------------------------------------*
       //* zone-D3. (Psl)
       //*-----------------//
         Xil = Chi - 1.0e0 ;
         Xilp12 = sqrt( Xil ) ;
         Xilp32 = Xil * Xilp12 ;
       end

     //---------------------------------------------------*
     //* Assign Pds.
     //*-----------------//
       Pds = Psl - Ps0 ;  // bugfix_Vds0_20160201 
         
     // Vdseff //
       Vds = Vdsorg;

     //-----------------------------------------------------------*
     //* Evaluate Idd.
     //* - Eta : substantial variable of QB'/Pds and Idd/Pds.
     //* - note: Eta   = 4 * GAMMA_{hisim_0}
     //*-----------------//
       T1 = beta / Xi0 ;
       Eta = T1 * Pds ;
        
     // ( Eta + 1 )^n //
       Eta1 = Eta + 1.0e0 ;
       Eta1p12 = sqrt( Eta1 ) ;
       Eta1p32 = Eta1p12 * Eta1 ;
       Eta1p52 = Eta1p32 * Eta1 ;
             
     // 1 / ( ( Eta + 1 )^n + 1 ) //
       Zeta12 = 1.0e0 / ( Eta1p12 + 1.0e0 ) ;
       Zeta32 = 1.0e0 / ( Eta1p32 + 1.0e0 ) ;
       Zeta52 = 1.0e0 / ( Eta1p52 + 1.0e0 ) ;
            
     //---------------------------------------------------*
     //* F00 := PS00/Pds(n=1/2)
     //*-----------------//
       F00 = Zeta12 / Xi0p12 ;
        
     //---------------------------------------------------*
     //* F10 := PS10/Pds(n=3/2)
     //*-----------------//
       T1 = 3.0e0 + Eta * ( 3.0e0 + Eta ) ;
       F10 = `C_2o3 * Xi0p12 * Zeta32 * T1 ;
        
     //---------------------------------------------------*
     //* F30 := PS30/Pds(n=5/2)
     //*-----------------//
       T1 = 5e0 + Eta * ( 10e0 + Eta * ( 10e0 + Eta * ( 5e0 + Eta ) ) ) ;
       F30 = 4e0 / ( 15e0 * beta ) * Xi0p32 * Zeta52 * T1 ;
        
     //---------------------------------------------------*
     //* F11 := PS11/Pds.
     //*-----------------//
       F11 = Ps0 * F10 + `C_2o3 * beta_inv * Xilp32 - F30 ;
        
     //---------------------------------------------------*
     //* Fdd := Idd/Pds.
     //*-----------------//
       T1 = Vgp + beta_inv - 0.5e0 * ( 2.0e0 * Ps0 + Pds ) ;
       T2 = - F10 + F00 ;
       T3 = beta * Cox ;
       T4 = beta * cnst0 ;
       Fdd = T3 * T1 + T4 * T2 ;
         
     //---------------------------------------------------*
     //*  Idd:
     //*-----------------//
       Idd = Pds * Fdd ;
     //-----------------------------------------------------------*
     //* Skip CLM and integrated charges if zone==D1
     //*-----------------//
       if( flg_zone == 1 ) begin
         start_of_mobility = 1 ;
       end
                                if(start_of_mobility == 0) begin
                       
     //-----------------------------------------------------------*
     //* Channel Length Modulation. Lred: \Delta L
     //*-----------------//
       if( UC_CLM2 < `epsm10 && UC_CLM3 < `epsm10 ) begin
         Lred = 0.0e0 ;
         Psdl = Psl ;
`ifdef REPLACE_CLIPPING_WITH_SMOOTHING //revised for continuity (Psdl)
       `Fn_SU_CP(Psdl, Psdl, (Ps0 + Vds - `epsm10), `epsm10, 2, T0)
`else
         if( Psdl > Ps0 + Vds - `epsm10 ) begin
           Psdl = Ps0 + Vds - `epsm10 ;
         end
`endif
       end else begin
         T1 = wdpl ;
         T8 = sqrt (Psl - Vbscl) ;
         Wd = T1 * T8 ;
         T9 = 0.5 * T1 / T8 ;
         T0 = 1.0 / Wd ;
         T1 = Qn0 * T0 ;
         T2 = UC_CLM3 * T1 ;
         T3 = UC_CLM3 * T0 ;
         T5 = UC_CLM2 * q_Nsub + T2 ;
         T1 = 1.0 / T5 ;
         T4 = `C_ESI * T1 ;
         T1 = (1.0e0 - UC_CLM1) ;
         Psdl = UC_CLM1 * (Vds + Ps0) + T1 * Psl ;
`ifdef REPLACE_CLIPPING_WITH_SMOOTHING //revised for continuity (Psdl and Lred)
       `Fn_SU_CP(Psdl, Psdl, (Ps0 + Vds - `epsm10), `epsm10, 2, T0)
`else
         if( Psdl > Ps0 + Vds - `epsm10 ) begin
           Psdl = Ps0 + Vds - `epsm10 ;
         end
`endif
         T6 = Psdl - Psl ;
         T3 = beta * Qn0 ;
         T1 = 1.0 / T3 ;
         T5 = Idd * T1 ;
         T2 = T5 * beta ;
         T10 = q_Nsub / `C_ESI ;
         T1 = 1.0e5 ;
         T2 = 1.0 / Leff ;
         T11 = (2.0 * T5 + 2.0 * T10 * T6 * T4 + T1 * T4) * T2 ;
         T3 = T2 * T4 ;
         T7 = T11 * T4 ;
         T11 = 4.0 * (2.0 * T10 * T6 + T1) ;
         T1 = 8.0 * T10 * T4 * T4 ;
         T2 = 2.0 * T11 * T4 ;
         T8 = T11 * T4 * T4 ;
         T9 = sqrt (T7 * T7 + T8);
         Lred = 0.5 * (- T7 + T9) ;
       //---------------------------------------------------*
       //* Modify Lred for symmetry.
       //*-----------------//
         T1 = Lred ;
         Lred = FMDVDS * T1 ;
       end
          
     // CLM5 & CLM6 //
       Lred = Lred * clmmod ;
          
     //---------------------------------------------------*
     //* Qbu : -Qb in unit area.
     //*-----------------//
       T1 = Vgp + beta_inv ;
       T2 = T1 * F10 - F11 ;
        
       Qbnm = cnst0 * ( cnst0 * ( 1.5e0 - ( Xi0 + 1.0e0 ) - 0.5e0 * beta * Pds )
          + Cox * T2 ) ;
       T1 = beta ;
       Qbu = T1 * Qbnm / Fdd ;
        
     //---------------------------------------------------*
     //* preparation for Qi and Qd.
     //* - DtPds: Delta * Pds ;
     //* - Achi: (1+Delta) * Pds ;
     //*-----------------//
       T1 = 2.0e0 * fac1 ;
       DtPds = T1 * ( F10 - Xi0p12 ) ;
       T2 = 2.0 * ( F10 - Xi0p12 ) ;
       Achi = Pds + DtPds ;
          
     //-----------------------------------------------------------*
     //* Alpha : parameter to evaluate charges.
     //* - Achi: (1+Delta) * Pds ;
     //* - clamped to 0 if Alpha < 0.
     //*-----------------//
       T1 = 1.0 / VgVt ;
       T2 = Achi * T1 ;
       T3 = 1.0e0 - T2 ;
       TX = 1.0 - T3 ;
       `Fn_CP( TY , TX , 1.0 , 4 , T4 ) 
       Alpha = 1.0 - TY ;
        
     //-----------------------------------------------------------*
     //* Qiu : -Qi in unit area.
     //*-----------------//
       Qinm = 1.0e0 + Alpha * ( 1.0e0 + Alpha ) ;
`ifdef REPLACE_CLIPPING_WITH_SMOOTHING //revised for continuity (Qiu and Qdrat)
       `Fn_SL_CP(Qidn, (1.0e0 + Alpha), `epsm10, `epsm10, 2, T0)
`else
       T1 = 1.0e0 + Alpha + Alpha ;
       Qidn = `Fn_Max( 1.0e0 + Alpha , `epsm10 ) ;
`endif
       T1 = `C_2o3 * VgVt * Qinm / Qidn ;
       Qiu = T1 * Cox ;
        
     //-----------------------------------------------------------*
     //* Qdrat : Qd/Qi
     //*-----------------//
       Qdnm = 0.5e0 + Alpha ;
       Qddn = Qidn * Qinm ;
       Quot = 0.4e0 * Qdnm  / Qddn ;
       Qdrat = 0.6e0 - Quot ;
       if( Qdrat > 0.5e0 ) begin
         Qdrat = 0.5e0 ;
       end
           
     //-----------------------------------------------------------*
     //* Interpolate charges and CLM for zone-D2.
     //*-----------------//
       if( flg_zone == 2 ) begin
         T1 = Qbu ;
         Qbu = FD2 * Qbu + ( 1.0 - FD2 ) * Qb0 ;
         if( Qbu < 0.0 ) begin
           Qbu = 0.0 ;
         end
         T1 = Qiu ;
         Qiu = FD2 * Qiu + ( 1.0 - FD2 ) * Qn0 ;
         if( Qiu < 0.0 ) begin
           Qiu = 0.0 ;
         end
         T1 = Qdrat ;
         Qdrat = FD2 * Qdrat + ( 1.0 - FD2 ) * 0.5e0 ;
             
       // note: Lred=0 in zone-D1 //
         T1 = Lred ;
         Lred = FD2 * Lred ;
            
       end // end of flg_zone==2 if-block //
          
                                end if(start_of_mobility)  start_of_mobility = 0; // end of start_of_mobility
// start_of_mobility:
       Lch = Leff - Lred ;
       if( Lch < 1.0e-9 ) begin
         $write("*** warning(HiSIM_HV(%m)): actual channel length is too Small. (Lch=%e[m])\n" , Lch ) ;
         $write("                    CLM5 and/or CLM6 might be too large.\n" ) ;
         Lch = 1.0e-9 ;   
       end
         

     //-----------------------------------------------------------*
     //* Muun : universal mobility.  (CGS unit)
     //*-----------------//

       T1 = ndep_o_esi / `C_m2cm  ;
       T2 = ninv_o_esi / `C_m2cm ;
       T0 = Ninvde ;
       T4 = 1.0 + ( Psl - Ps0 ) * T0 ;
`ifdef _AGING_
       T3 = TRAPN / `C_ESI / `C_m2cm ;
       T11 = TRAPP * TRAPDVTH / Tox0 ;
       T5 = T1 * Qbu + T2 * Qiu + T3 * Qt0 + T11 ;
`else
       T5 = T1 * Qbu + T2 * Qiu ;
`endif
       T3 = T5 / T4 ;
       Eeff = T3 * ( 1.0 + MUEEFB * Vbsz ) ;
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
       T2 = beta * (Qn0 + `Small) * Lch ;
       T1 = 1.0e0 / T2 ;
       T3 = T1 * T1 ;
       T4 =  - beta * T3 ;
       T5 = T4 * Lch ;
       T6 = T4 * (Qn0 + `Small) ;
       TY = Idd * T1 ;
       T2 = 0.2 * Vmaxe /  Muun ;
       T3 = - T2 / Muun ;
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
        
// end_of_mobility:
        
     //-----------------------------------------------------------*
     //* Ids: channel current.
     //*-----------------//
       betaWL = weff_nf * beta_inv / Lch ;
       T1 = - betaWL / Lch ;
       Ids0 = betaWL * Idd * Mu ;
     //-----------------------------------------------------------*
     //* Adding parasitic components to the channel current.
     //*-----------------//
       if( PTL != 0 ) begin
         T1 =  0.5 * ( Vds - Pds ) ;
         `Fn_SymAdd( T6 , T1 , 0.01 , T2 ) 
         T2 = T2 * 0.5 ;
         T1 = 1.1 - ( Ps0 + T6 );
         `Fn_SZ( T2 , T1 , 0.05 , T0 ) 
         T2 = T2 + `Small ;
         T0 = beta * ptl0 ;
         T3 = Cox * T0 ;
         T0 = pow( T2 , PTP ) ;
         T9 = T3 * T0 ;
         T4 = 1.0 + Vdsz * PT2 ;
         T0 = pt40 ;
         T5 = Ps0 + T6 - Vbsz ;
         T4 = T4 + Vdsz * T0 * T5 ;
         T6 = T9     * T4 ;
         T9 = T6 ;
       end else begin
         T9 = 0.0 ;
       end
       if( GDL != 0 ) begin
         T1 = beta * gdl0 ;
         T2 = Cox * T1 ;
         T8 = T2 * Vdsz ;
       end else begin
         T8 = 0.0 ;
       end
       if(( T9 + T8 ) > 0.0 ) begin
         Idd1 = Pds * ( T9 + T8 ) ;
//       Ids0 = Ids0 + betaWL * Idd1 * Mu ;
         IdsPT0 = betaWL * Idd1 * Mu ;
         Ids0   = Ids0 + IdsPT0 ;
       end else begin
         IdsPT0 = 0.0 ;
       end
//HV1{
          if( flg_rsrd == 2 || flg_rsrd == 3 ) begin
            if( RD20 > 0.0 ) begin
              T4 = Rd23e ;
              T1 = UC_RD24 * ( Vgse - RD25 ) ;
              `Fn_SL( T2 , T1 , T4 , `delta_rd , T0 ) 
              T3 = T4 * ( RD20 + 1.0 ) ;
              `Fn_SU( T7 , T2 , T3 , `delta_rd , T0 ) 
            end else begin
              T7 = Rd23e;
            end
             
            if(Vdse >= 0.0) begin
              Vdse_eff = Vdse ;
            end else begin
              Vdse_eff = 0.0 ;
            end
                   
          // smoothing of Ra for Vdse_eff close to zero //
          // ... smoothing parameter is Ra_N            //
            if(Vdse_eff < `Ra_N * `Small2) begin
              Ra_alpha = pow( `Ra_N+1.0 , RD21-1.0 )
                     * (`Ra_N+1.0-0.5*RD21*`Ra_N)
                     * pow( `Small2,RD21 );
              Ra_beta = 0.5*RD21
                    * pow( `Ra_N+1.0 , RD21-1.0 ) / `Ra_N
                    * pow( `Small2, RD21-2.0 );
              T1 = Ra_alpha + Ra_beta*Vdse_eff*Vdse_eff;
            end else begin
              T1 = pow( Vdse_eff + `Small2 , RD21 ) ;
            end
            T9 = pow( Vdse_eff + `Small2 , RD22D ) ;
            Ra = ( T7 * T1 + Vbse * UC_RD22 * T9 ) / weff_nf ;
            T0 = Ra * Ids0 ;
            T1 = Vds + `Small2 ;
            T2 = 1.0 / T1 ;
            T3 = 1.0 + T0 * T2 ;
            T4 = 1.0 / T3 ;
            Ids = Ids0 * T4 ;
          end else begin
            Ids = Ids0 ;
            Ra = 0.0 ;
          end //}HV1

     //-----------------------------------------------------------*
     //* STI
     //*-----------------//
       if( COISTI != 0 ) begin
       //---------------------------------------------------*
       //* dVthSCSTI : Short-channel effect induced by Vds(STI).
       //*-----------------//
         T1 = `C_ESI * Cox_inv ;
         T2 = wdpl ;
         T3 =  lgatesm - PARL2 ;
         T4 = 1.0 / (T3 * T3) ;
         T5 = 2.0 * (VBI - Pb20b) * T1 * T2 * T4 ;
         dVth0 = T5 * sqrt_Pbsum ;
         T6 = T5 * 0.5 / sqrt_Pbsum ;
         T7 = 2.0 * (VBI - Pb20b) * `C_ESI * T2 * T4 * sqrt_Pbsum ;
         T8 = - 2.0 * T1 * T2 * T4 * sqrt_Pbsum ;
         T4 = UC_SCSTI1 ;
         T6 = UC_SCSTI2 ;
         T1  = T4 + T6 * Vdsz ;
         dVthSCSTI = dVth0 * T1 ;
         T1 = UC_VTHSTI - VDSTI * Vds ;
         Vgssti = Vgsz - Vfb + T1 + dVthSCSTI ;
              
         costi3 = costi0_p2 * Cox_inv * Cox_inv ;
         costi4 = costi3 * beta * 0.5 ;
         costi5 = costi4 * beta * 2.0 ;
         T11 = beta * 0.25 ;
         T10 = beta_inv - costi3 * T11 + Vfb - UC_VTHSTI - dVthSCSTI + `Small ;
         T1 = Vgsz - T10 - `psisti_dlt ;
         T0 = `Fn_Sgn(T10) ;
         T2 = sqrt (T1 * T1 + T0 * 4.0 * T10 * `psisti_dlt) ;
         T3 = T10 + 0.5 * (T1 + T2) - Vfb + UC_VTHSTI + dVthSCSTI - Vbsz ;
         T4 = beta * T3 - 1.0 ;
         T5 = 4.0 / costi5 ;
         T1 = 1.0 + T4 * T5 ;
         T6 = beta * T5 ;
         T7 = T4 * T5 ;
         `Fn_SZ( T1 , T1, 1.0e-2, T2) 
         T1 = T1 + `Small ;
         costi6 = sqrt(T1) ;
         T0 = costi4 * (1.0 - costi6) ;
         Psasti = Vgssti + T0 ;
         T0 = 1.0 / (beta + 2.0 / (Vgssti + `Small)) ;
         Psbsti = ln (1.0 / costi1 / costi3 * (Vgssti * Vgssti)) * T0 ;
         Psab = Psbsti - Psasti - `sti2_dlt ;
         T0 = sqrt (Psab * Psab + 4.0 * `sti2_dlt * Psbsti) ;
         Psti = Psbsti - 0.5 * (Psab + T0) ;
         T0 = costi1 * exp (beta * Psti) ;
         T1 = beta * (Psti - Vbsz) - 1.0 + T0 ;
         `Fn_SZ( T1 , T1, 1.0e-2, T0) 
         T1 = T1 + `Small ;

         sq1sti = sqrt (T1);
         T1 = beta * (Psti - Vbsz) - 1.0;
         `Fn_SZ( T1 , T1, 1.0e-2, T0) 
         T1 = T1 + `Small ;

         sq2sti = sqrt (T1);
         T2 = 0.5 / sq2sti ;
         Qn0sti = costi0 * (sq1sti - sq2sti) ;
              
       // T1: Vdsatsti //
         T1 = Psasti - Psti ;
         `Fn_SZ( T1 , T1 , 1.0e-1 , T2 ) 
         T1 = T1 + `Small ;

         TX = Vds / T1 ;
         T2 = 1.0 / ( T1 * T1 ) ;
         `Fn_CP( TY , TX , 1.0 , 4 , T2 ) 
         costi7 = 2.0 * UC_WSTI * NF * beta_inv ;
         T1 = Lch ;
         Idssti = costi7 * Mu * Qn0sti * TY / T1 ;
        Ids = Ids + Idssti ;
           
       end
         
     //----------------------------------------------------------*
     //* induced gate noise. ( Part 1/3 )
     //*----------------------//
       if( COIGN != 0 && COTHRML != 0 && UC_CODEP == 0) begin
         kusai00 = VgVt * VgVt ;
         kusaidd = 2.0e0 * beta_inv * Cox_inv * Idd ;
         kusaiL = kusai00 - kusaidd ;
         `Fn_SZ( kusai00 , kusai00 , 1.0e-3 , T0 ) 
         `Fn_SZ( kusaiL , kusaiL , 1.0e-3 , T0 ) 

         kusai00L = kusai00 - kusaiL ;
         if ( Qn0 < `epsm10 || kusai00L < `epsm10 ) flg_ign = 0 ;
         else flg_ign = 1 ;
       end
              
     //-----------------------------------------------------------*
     //* End of PART-1. (label)
     //*-----------------//
                                end if(end_of_part_1)  end_of_part_1 = 0; // end of end_of_part_1
                                end if(end_of_part_1)  end_of_part_1 = 0; // end of end_of_part_1
// end_of_part_1:
              
     //-----------------------------------------------------------*
     //* Implicit quantities related to Alpha.
     //*-----------------//
       if( flg_noqi == 0 && VgVt > `VgVt_Small ) begin
         Delta = fac1 * beta / ( 2 * Xi0p12 ) ;
         Pslsat = VgVt / ( 1.0 + Delta ) + Ps0 ;
       end else begin
         Pslsat = 0.0 ;
       end
       Vdsat = Pslsat - Pb2 ;
       if( Vdsat < 0.0 ) begin
         Vdsat = 0.0 ;
       end

     //-----------------------------------------------------------*
     //* Adding punchthrough current.
     //*-----------------//
       Idsorg = Ids ;
       IdsPT1 = 0.0 ;
       if(COPT > 0 && MUPT > 0.0) begin
`include "HSMHV_eval_newPT.inc"
       end //
       Ids    = Idsorg + IdsPT1 ;
       IdsPT  = IdsPT0 + IdsPT1 ;
           
     // Qiu for noise calc.
       Qiu_noi = Qiu ;

     end // else: !if( UC_CODEP != 0 )

     //----------------------------------------------------------*
     //* Evaluate integrated charges in unit [C].
     //*----------------------//

       T1 = - weffcv_nf * Leff ;
       Qb = T1 * Qbu ;
       Qi = T1 * Qiu ;
       Qd = Qi * Qdrat ;

     // Qiu for noise calc.
       Qi_noi = T1*Qiu_noi ;
   
     //-----------------------------------------------------------*
     //* Modified potential for symmetry.
     //*-----------------//
       T1 =  ( Vds - Pds ) / 2 ;
       `Fn_SymAdd( Pzadd , T1 , PZADD0 , T2 ) 
`ifdef REPLACE_CLIPPING_WITH_SMOOTHING //revised for continuity (Isub and Igate)
     `Fn_SL_CP(Pzadd, Pzadd, `epsm10, `epsm10, 2, T0)
`else
       if( Pzadd  < `epsm10 ) begin
           Pzadd = `epsm10 ;
       end
`endif
       Ps0z = Ps0 + Pzadd ;
          
     //-----------------------------------------------------------*
     //* Channel leakage current.
     //*-----------------//
     Ids = Ids + Weff/Leff * GDSLEAK * Vds ;
         
     //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     //* PART-2: Substrate / gate / leak currents
     //*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++//
         
     //-----------------------------------------------------------*
     //* Isub : substrate current induced by impact ionization.
     //*-----------------//
     if( COISUB == 0 ) begin //{
       // Accumulation zone or nonconductive case, in which Ids==0. //
       Isub = 0.0e0 ;  wk_ii=0.0; // wk_ii stows impact ionization coeff.       
     end else begin //{
       //-------------------------------------------*
       //* Conductive case.
       //*-----------------//
       if( UC_SUB1 > 0.0e0 && UC_VMAX > 0.0e0 ) begin //{
         T1 = vg2const * Vgp ;
         T3 = qnsub_esi / (Cox0 * Cox0) ;
         T4 = 2.0 / qnsub_esi * (Cox0 * Cox0) ;
         T5 = T1 - beta_inv - xvbs * Vbsz ;
         T6 = 1.0 + T4 * T5 ;
         T7 = 2.0 * ( 1.0 + T4 ) ;
         `Fn_SL_CP( T6 , T6 , 1e-6 , T7 , 4 , T0 )
         T6 = sqrt( T6 ) ;
         Psislsat = T1 + T3 * ( 1.0 - T6 ) ;
         T2 = Lgate / (xgate + Lgate) ;
         Psisubsat = UC_SVDS * Vdsz + Ps0z - T2 * Psislsat ;
`ifdef _AGING__
     $write(" UC_SVDS %g Vdsz %g Ps0z %g Psislsat %g T2 %g \n", UC_SVDS , Vdsz , Ps0z , Psislsat , T2 );
`endif
         `Fn_SZ( Psisubsat , Psisubsat, 1.0e-3, T9 ) 
         Psisubsat = Psisubsat + `Small ;
         xsubtmp = 1.0 + UC_SUBTMP * (TTEMP-KTNOM) ; 
         xsubtmp = ( xsubtmp <= 1.0e-3 ) ? 1.0e-3  : xsubtmp ;
         T5 = xsub1 / xsubtmp ;
         T6 = xsub2 * xsubtmp ;
         T2 = exp( - T6 / Psisubsat ) ;
         Isub = T5 * Psisubsat * Ids * T2 ;
         wk_ii = T5 * Psisubsat * T2; // 20180413
       end else begin //{
         Isub = 0.0e0 ;
       end // end of if( UC_SUB1 ... ) else block. //}

`ifdef _AGING__
       $write(" Isub %g T5 %g Psisubsat %g Ids %g T2 %g \n",Isub , T5 , Psisubsat , Ids , T2 );
`endif

       //---------------------------------------------------*
       //* Impact-Ionization Current in the Drift Region
       //*-----------------//
       T8 = UC_SUBLD1 ;
       if( T8 != 0.0 ) begin
         T0 = Vddp ;
         `Fn_SZ( T0, T0, 1e-6, T1 ) 
         T1 = sqrt( VgVt + `Small  ) ;
         T3 = 1.0 / ( 2.0 * T1 ) ;
         T4 = T0 - XPVDTH * ( 1.0 + XPVDTHG * Vgs ) ;
         `Fn_SZ( T4, T4, 1.0e-2, T9 ) 
         T4 = T4 + `Small ;
         T10 = UC_XPDV * UC_XLDLD * exp( - 1.0e0 / T4 ) ;
         T11 = T10 * ( 1.0 + 1.0 / T4 ) ;
         T3 = T4 * T10 ;
         T0 = T0 - T3 ;
         `Fn_SZ( T0, T0, 1.0e-2 , T9 ) 
         T0 = T0 + `Small;

         T4 = 1.0 / ( T0 * T1 ) ;
         T7 = Ldrift0 * MKS_SUBLD2 ;
         T2 = exp( - T7 * T4 );
         T6 = T7 * T2 * T4 * T4 ;
         IsubLD = T8 * Ids * T0 * T2 ;
 
       end
     end

`ifdef _AGING_
       // ---------------------------------------------------//
       // * stress simulation
       `include "HSMHV_eval_aging.inc"
       //*-----------------//

       // restore Ps0 , Vtraplx 
       Ps0_prv = Ps0 ;                      // in conjunction with aging
       Vtraplx_prv = Vdseff + Ps0 - Psl ;   // in conjunction with aging

`endif

     //---------------------------------------------------*
     //* Breakdown currents (Ibreake,Ibreakhe,Ibreakse)
     //*-----------------//
     // Hard Breakdown Current specified by user
     if( COHBD == 0 ) begin
       Ibreakhe = 0.0 ;
     end else begin
       // calc. Hard Breakdown Voltage
       if( COHBD * (Vgse - HBDB) < 0 ) begin
         HBdv = HBDCeff ;
       end else begin
         HBdv = HBDA * pow(Vgse - HBDB,2) + HBDCeff ;
       end
       Ibreakhe = HBDF * lexp( beta * ( Vdse - HBdv ) ) ;
     end

     // Total Breakdown Current
     if( Ibreakhe > 0 ) begin
       `Fn_SU_CP( T2 , Ibreakhe , 1.0e5 , 5e4, 1 , T0 )
       Ibreake = Mfactor * weff_nf * T2 ;
     end else begin
       Ibreake = 0.0 ;
     end

     //---------------------------------------------------*
     //* Impact-Ionization Induced Bulk Potential Change(IBPC)
     //*-----------------//
     if( (Isub + IsubLD) > 0e0 && UC_IBPC1 != 0e0 && UC_CODEP == 0 ) begin //{
              
       // delta Vbs //
       T0 = 1e0 + UC_IBPC2 * dVth ;
       T1 = Isub + IsubLD ;
       dVbsIBPC = UC_IBPC1 * T0 * T1 ;
                
       // dG3 & dG4 //
       T10 = 1e0 / Xi0 ;
       T1 = beta * dVbsIBPC * T10 ;
       T11 = T10 * T10;
       T10 = 1e0 / Xil ;
       T2 = beta * dVbsIBPC * T10 ;
       T11 = T10 * T10;
       dG3 = cnst0 * ( Xilp32 * T2 - Xi0p32 * T1 ) ;
       dG4 = cnst0 * 0.5 * ( - Xilp12 * T2 + Xi0p12 * T1 ) ;
           
       // Add IBPC current into Ids //
       dIdd = dG3 + dG4 ;
       IdsIBPC = betaWL * dIdd * Mu ;
       IsubIBPC = wk_ii * IdsIBPC; // 20180413
        
     end // End if(IBPC) //}

     //-----------------------------------------------------------*
     //* Igate : Gate current induced by tunneling.
     //*-----------------//
     if( COIIGS != 0 ) begin
       // Igate //
       if( flg_noqi == 0 ) begin
         Psdlz = Ps0z + Vdsz - `epsm10 ;
         T1 = Vgsz - Vfb + MKS_GLEAK4 * (dVth - dPpg) * Leff - Psdlz * UC_GLEAK3 ;
         T1 = T1 * T1 ;
         T3 = 1.0 / Tox0 ;
         T2 = T1 * T3 ;
         T3 = 1.0 / MKS_GLEAK5 ;
         T7 = 1.0 + Ey * T3 ;
         Etun = T2 * T7 ;
         `Fn_SZ( Etun , Etun , `igate_dlt , T5 ) 
         `Fn_SZ( T3 , Vgsz , 1.0e-3 , T4 ) 
         T3 = T3 - VZADD0 ;
         TX = T3 / `cclmmdf ;
         T2 = 1.0 +  TX * TX ;
         T1 = 1.0 - 1.0 / T2 ;
         Etun = Etun * T1 ;
         T0 = Leff * weff_nf ;
         T7 = MKS_GLEAK7 / (MKS_GLEAK7 + T0) ;
         T6 = UC_GLEAK6 ;
         T9 = T6 / (T6 + Vdsz) ;
         T4 = 1 / (Etun + `Small ) ;
         T1 = - UC_GLEAK2 * Egp32 * T4 ;
         T3 =  UC_GLEAK2 * T4 * T4;
         if( T1 < - `EXP_THR ) begin
           Igate = 0.0 ;
         end else begin
           T2 = exp ( T1 ) ;
           T3 = UC_GLEAK1 / Egp12 * `C_QE * T0 ;
           T5 = 1 / cnst0 ;
           T6 =  sqrt ((Qiu_noi + Cox0 * `VgVt_Small )* T5 ) ;
           T4 =  T2 * T3 * T6 ;
           T5 = T4 * Etun ;
           T10 = T5 * Etun ;
           Igate = T7 * T9 * T10 ;
         end
       end
             
       // Igs //
       T0 = - UC_GLKSD2 * Vgs + MKS_GLKSD3 ;
       T2 = exp (Tox0 * T0);
       T0 = Vgs / Tox0 / Tox0 ;
       T3 = Vgs * T0 ;
       T4 = UC_GLKSD1 / 1.0e6 * weff_nf ;
       Igs = T4 * T2 * T3 ;
       if( Vgs >= 0.0e0 ) begin
         Igs = Igs * -1.0 ;
       end
           
       // Igd //
       T1 = Vgs - Vds ;
       T0 = - UC_GLKSD2 * T1 + MKS_GLKSD3 ;
       T2 = exp (Tox0 * T0);
       T0 = T1 / Tox0 / Tox0 ;
       T3 = T1 * T0 ;
       T4 = UC_GLKSD1 / 1.0e6 * weff_nf ;
       Igd = T4 * T2 * T3 ;
       if( T1 >= 0.0e0 ) begin
         Igd = Igd * -1.0 ;
       end
           
       // Igb //
       Etun = ( - ( Vgs - Vbs ) + Vfb + GLKB3 ) / Tox0 ;
       `Fn_SZ( Etun , Etun, `igate_dlt, T5) 
       Etun = Etun + `Small ;
       T1 = - UC_GLKB2 / Etun ;
       if( T1 < - `EXP_THR ) begin
         Igb = 0.0 ;
       end else begin
         T2 = exp ( T1 );
         T3 =  UC_GLKB2 / ( Etun * Etun ) * T2 ;
         T3 = UC_GLKB1 * weff_nf * Leff ;
         Igb = T3 * Etun * Etun * T2 ;
       end
           
       // Ifn: Fowler-Nordheim tunneling current //
       Eg12 = sqrt_eg ;
       Eg32 = Eg * Eg12 ;
       T2 = - ( UC_FVBS * Vbsz - Vgsz + dVthSC + dVthLP - UC_FN3 ) / Tox0 ;
       T0 = T2 * T2 ;
       T1 = UC_FN2 * Eg32 ;
       T3 = - T1 / T2  ;
       if( T3 < - `EXP_THR ) begin
         T5 = 0.0 ;
       end else begin
         T5 = exp( T3 ) ;
       end
       T4 = `C_QE * UC_FN1 * weff_nf * Lgate / Eg12 ;
       if( 2e0 * T2 + T1 < 0e0 ) begin
         Ifn = 0.25e0 * T4 * T1 * T1 * `c_exp_2 ; // minimum value //
       end else begin
         Ifn = T4 * T0 * T5 ;
       end
       Igb = Igb - Ifn ;
     end // if( coiigs == 0 ) //
         
     //-----------------------------------------------------------*
     //* Vdsp : Vds modification for GIDL/GISL
     //*-----------------//
     if( COGIDL != 0 ) begin
       T1 = Vds * (1.0 - `gidla * Vds) - `gidlvds_dlt ;
       T2 = sqrt (T1 * T1 + 4.0 * `gidlvds_dlt * Vds) ;
       Vdsp = Vds - 0.5 * (T1 + T2) ;
     end
        
     //-----------------------------------------------------------*
     //* Igidl : GIDL
     //*-----------------//
     if( COGIDL == 0 ) begin
       Igidl = 0.0e0 ;
     end else begin
       T1 = GIDL3 * (Vdsp + GIDL4) - Vgs + (dVthSC + dVthLP) * GIDL5 ;
       T2 = 1.0 / Tox0 ;
       E1 = T1 * T2 ;
       `Fn_SZ( Egidl , E1, `eef_dlt, T5) 
       T3 = 1 / (Egidl + `Small) ;
       T0 = - UC_GIDL2 * Egp32 * T3 ;
       if( T0 < - `EXP_THR ) begin
         Igidl = 0.0 ;
       end else begin
         T1 = exp ( T0 ) ;
         T2 = UC_GIDL1 / Egp12 * `C_QE * weff_nf ;
         Igidl = T2 * Egidl * Egidl * T1 ;
       end
          
       // bug-fix //
       Vdb = Vds - Vbs ;
       if( Vdb > 0.0 ) begin
         T2 = Vdb * Vdb ;
         T4 = T2 * Vdb ;
         T0 = T4 + `C_gidl_delta ;
         T5 = T4 / T0 ;
         T7 = ( 3.0 * T2 * T0 - T4 * 3.0 * T2 ) / ( T0 * T0 ) ; 
         Igidl = Igidl * T5 ;
       end else begin
         Igidl = 0.0 ;
       end
     end
           
     //-----------------------------------------------------------*
     //* Igisl : GISL
     //*-----------------//
     if( COGIDL == 0) begin
       Igisl = 0.0e0 ;
     end else begin
       T1 = GIDL3 * ( - Vdsp + GIDL4 )
          - ( Vgs - Vdsp ) + ( dVthSC + dVthLP ) * GIDL5 ;
       T2 = 1.0 / Tox0 ;
       E1 = T1 * T2 ;
       `Fn_SZ( Egisl , E1, `eef_dlt, T5) 
       T3 =  1  / (Egisl + `Small) ;

       T0 = - UC_GIDL2 * Egp32 * T3 ;
       if( T0 < - `EXP_THR ) begin
         Igisl = 0.0 ;
       end else begin
         T1 = exp ( T0 ) ;
         T3 = 1 / Egp12 ;
         T2 = UC_GIDL1 * T3 * `C_QE * weff_nf ;
         Igisl = T2 * Egisl * Egisl * T1 ;
       end
          
       // bug-fix //
       Vsb = - Vbs ;
       if( Vsb > 0.0 ) begin
         T2 = Vsb * Vsb ;
         T4 = T2 * Vsb ;
         T0 = T4 + `C_gidl_delta ;
         T5 = T4 / T0 ;
         T7 = ( 3.0 * T2 * T0 - T4 * 3.0 * T2 ) / ( T0 * T0 ) ; 
         Igisl = Igisl * T5 ;
       end else begin
         Igisl = 0.0 ;
       end
     end
           
     //-----------------------------------------------------------*
     //* End of PART-2. (label)
     //*-----------------//
// end_of_part_2:
           
     //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     //* PART-3: Overlap charge
     //*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++//
      //-----------------------------------------------------------*
      //* Overlap charges
      //*-----------------//
      begin:  evalOverlap
	integer flg_COOVLPS, i_over, flg_calcqover, flg_COOVLP; 
`include "HSMHV_eval_Overlap.inc"
      end

     //---------------------------------------------------*
     //* Lateral-field-induced capacitance (depends on the Vds mode).
     //*-----------------//
     Aclm = UC_CLM1 ;
     if( flg_noqi != 0 ) begin
       //-------------------------------------------*
       //* Calculation of Psdl for cases of flg_noqi==1.
       //*-----------------//
       Psdl = Aclm * (Vds + Ps0) + (1.0e0 - Aclm) * Psl ;
`ifdef REPLACE_CLIPPING_WITH_SMOOTHING //revised for continuity (Psdl for Nthrml)
       `Fn_SU_CP(Psdl, Psdl, (Ps0 + Vds - `epsm10), `epsm10, 2, T0)
`else
       if( Psdl > Ps0 + Vds - `epsm10 ) begin
         Psdl = Ps0 + Vds - `epsm10 ;
       end
`endif
       if(flg_qy) begin
         Ec = 0.0e0 ;
       end
     end else begin
       // Ec is removed from Lred calc. part //
       if(flg_qy) begin
         if( Idd < `C_IDD_MIN || Vdseff < `C_VDS_MIN ) begin
           Ec = 0.0e0 ;
         end else begin
           Ec = Idd / Qn0 * beta_inv / Leff ;
         end
      end
     end

     if( flg_qy == 0 ) begin
       Qy = 0.0e0 ;
     end else begin
       T2 = `C_ESI * weffcv_nf * wdpl * 1.3 ;
       if( XQY != 0.0 ) begin
         Pslk = Ec * Leff + Ps0 ;
         T1 = Aclm * ( Vdsz + Ps0 ) + ( 1.0e0 - Aclm ) * Pslk ;  // 20160525 Vds->Vdsz
         Qy = - ( ( Ps0 + Vdsz  - T1 ) / XQY ) * T2 ; // 20160525 Vds->Vdsz
       end
       if( XQY1 != 0.0 ) begin
         Qy = Qy + cqyb0 * Vbs ;
       end
     end
        
     //---------------------------------------------------*
     //* Fringing capacitance.
     //*-----------------//
     Cfd = cfrng ;
     Cfs = cfrng ;
     Qfd = Cfd * ( Vgsei - Vdsei ) ;
     Qfs = Cfs * Vgsei ;
         
     //-----------------------------------------------------------*
     //* End of PART-3. (label)
     //*-----------------//
         
// end_of_part_3:
         
     //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     //* PART-4: Substrate-source/drain junction diode.
     //*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++//
         

`ifdef COEXTDIO 
       /* for Junction Diode Model */
`else 
`include "HSMHV_eval_dio.inc"
`endif /* End of COEXTDIO */

     // snapback //
//     wIbjt = 0.0;  iiFac = 0.0;  wk_iiFac = 0.0;  // probing 20180410
if ( COSNP == 1 ) begin //{
// This part comes from PART-2.
       //-------------------------------------------*
       //* Conductive case.
       //*-----------------//
       if( UC_SUB1SNP > 0.0e0 && UC_VMAX > 0.0e0 ) begin //{
         T1 = vg2const_ * Vgp ;
         T3 = qnsub_esi / (Cox0 * Cox0) ;
         T4 = 2.0 / qnsub_esi * (Cox0 * Cox0) ;
         T5 = T1 - beta_inv - xvbs_ * Vbsz ;
         T6 = 1.0 + T4 * T5 ;
         T7 = 2.0 * ( 1.0 + T4 ) ;
         `Fn_SL_CP( T6 , T6 , 0, T7 , 4 , T0 )
         T6 = sqrt( T6 ) ;
         Psislsat = T1 + T3 * ( 1.0 - T6 ) ;
         T2 = Lgate / (xgate_ + Lgate) ;
         Psisubsat = UC_SVDSSNP * Vdsz + Ps0z - T2 * Psislsat ;
         `Fn_SZ( Psisubsat , Psisubsat, 1.0e-3, T9 ) 
         Psisubsat = Psisubsat + `Small ;
         xsubtmp = 1.0 + UC_SUBTMP * (TTEMP-KTNOM) ; 
         xsubtmp = ( xsubtmp <= 1.0e-3 ) ? 1.0e-3  : xsubtmp ;
         T5 = xsub1_ / xsubtmp ;
         T6 = xsub2_ * xsubtmp ;
         T2 = exp( - T6 / Psisubsat ) ;
         iiFac = T5 * Psisubsat * T2 ;

// Ibs (CODIbdO = 0)or Ibsi (CODIO > 0 ) From 20160610 on
          if ( CODIO > 0) begin //new model
              wIbjt = ( 1.0 + iiFac) * Ibsi; // Ibs:  weff_nf, M factor incl.
          end else begin
              wIbjt = ( 1.0 + iiFac) * Ibs; // Ibs:  weff_nf, M factor incl.
          end
         end // } if( UC_SUB1SNP > 0.0e0 && UC_VMAX > 0.0e0 )
        end //}  if ( COSNP == 1 )
//        wk_iiFac = iiFac; wk_Ibjt = wIbjt; wk_Ibsi = Ibsi; wk_Ibs = Ibs; // 20180410 probing 
     //-----------------------------------------------------------*
     //* End of PART-4. (label)
     //*-----------------//
         
// end_of_part_4:
         
     //-----------------------------------------------------------*
     //* PART-5: NQS. (label)
     //*-----------------//
                 
     if( flg_nqs ) begin 
       if(flg_noqi == 0) begin
         T12 = DLY1 ;
         T10 = DLY2 ;
         T3 = Lch ;
         T1 = T12 * T10 * T3 * T3 ;
         T2 = Mu * VgVt * T12 + T10 * T3 * T3 + `Small ;
         tau = T1 / T2 ;
       end else begin
         tau = DLY1 ;
       end
       T2 = MKS_DLY3 ;
       taub = T2 * Cox;
     end
          
     //-----------------------------------------------------------*
     //* End of PART-5. (label)
     //*-----------------//
// end_of_part_5:
          
     //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     //* PART-6: Noise Calculation.
     //*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++//
          
     //-----------------------------------------------------------*
     //* 1/f noise.
     //*-----------------//
     if( COFLICK != 0 && !flg_noqi ) begin
       NFalpe = UC_NFALP ;
       NFtrpe = UC_NFTRP ;
       Cite = MKS_CIT ;
       T1 = Qn0 / `C_QE ;
       `Fn_SZ( T5 , (Ps0 - Vbscl) , 1e-3 , T0 )
       T2 = ( Cox + Qn0 / ( T5 ) + Cite ) * beta_inv / `C_QE ;
       T3 = -2.0E0 * Qi_noi / `C_QE / Lch / weffcv_nf - T1 ;
       if( T3 != T1 ) begin
         T4 = 1.0E0 / ( T1 + T2 ) / ( T3 + T2 ) + 2.0E0 * NFalpe * Ey * Mu / ( T3 - T1 )
            * ln(( T3 + T2 ) /( T1 + T2 ) ) + NFalpe * Ey * Mu * NFalpe * Ey * Mu ;
       end  else begin
         T4 = 1.0 / ( T1 + T2 ) / ( T3 + T2 ) + 2.0 * NFalpe * Ey * Mu / ( T1 + T2 )
            + NFalpe * Ey * Mu * NFalpe * Ey * Mu;
       end
       Nflic = Ids * Ids * NFtrpe / ( Lch * beta * weff_nf ) * T4 ;
     end else begin
       Nflic = 0.0 ;
     end
           
     //-----------------------------------------------------------*
     //* thermal noise.
     //*-----------------//
     if( COTHRML != 0 && !flg_noqi && UC_CODEP == 0) begin
       Eyd = ( Psdl - Ps0 + `epsm10 ) / Lch ;
       T12 = Muun * Eyd / 1.0e7 ;
       // note: bb = 2 (electron) ;1 (hole) //
       if( 1.0e0 - `epsm10 <= BB && BB <= 1.0e0 + `epsm10 ) begin
         T7  = 1.0e0 ;
       end else if( 2.0e0 - `epsm10 <= BB && BB <= 2.0e0 + `epsm10 ) begin
         T7  = T12 ;
       end else begin
         T7  = `Fn_Pow( Eyd, BB - 1.0e0 ) ;
       end
       T8 = T12 * T7 ;
       T9 = 1.0e0 + T8 ;
       T10 = `Fn_Pow( T9, ( - 1.0e0 / BB - 1.0e0 ) ) ;
       T11 = T9 * T10 ;
       Mud_hoso = Muun * T11 ;
       Mu_Ave = ( Mu + Mud_hoso ) / 2.0 ;
       // Sid_h = GAMMA * 4.0 * C_KB * temp * gds0_h2; //
       T0 = Alpha * Alpha ;
       Nthrml  = weff_nf * Cox * VgVt * Mu
               * ( ( 1e0 + 3e0 * Alpha + 6e0 * T0 ) * Mud_hoso * Mud_hoso
               + ( 3e0 + 4e0 * Alpha + 3e0 * T0 ) * Mud_hoso * Mu
               + ( 6e0 + 3e0 * Alpha + T0 ) * Mu * Mu )
               / ( 15e0 * Lch * ( 1e0 + Alpha ) * Mu_Ave * Mu_Ave ) ;
     end else begin
       Nthrml = 0e0 ;
     end
            
     //----------------------------------------------------------*
     //* induced gate noise. ( Part 2/3 )
     //*----------------------//
     if( COIGN != 0 && COTHRML != 0 && flg_ign == 1 && !flg_noqi && UC_CODEP == 0) begin
       sqrtkusaiL = sqrt( kusaiL ) ;
       T2 = VgVt + sqrtkusaiL ;
       T3 = kusai00 * kusai00 ;
       T4 = kusaiL * kusaiL ;
       T5 = 42.0e0 * kusai00 * kusaiL ;
       T5 = T5 + 4.0e0 * ( T3 + T4 ) ;
       T5 = T5 + 20.0e0 * sqrtkusaiL * VgVt * ( kusai00 + kusaiL ) ;
       T10 = T2 * T2 ;
       T10 = T10 * T10 ;
       kusai_ig = T5 / ( T10 * T2 ) ; // Induced Gate Noise parameter //
       gds0_ign = weff_nf / Lch * Mu * Cox ;
       gds0_h2 = gds0_ign * VgVt ;
       GAMMA = Nthrml / gds0_h2 ;
       T7 = kusai00 + 4.0e0 * VgVt * sqrtkusaiL + kusaiL ;
       // cross-correlation coefficient(= Sigid/sqrt(Sig*Sid) ) //
       crl_f = `c_sqrt_15 * kusai00L * T7
             / ( 6.0e0 * T2 * sqrt( GAMMA * T2 * VgVt * T5 ) ) ;
     end
           
     //-----------------------------------------------------------*
     //* End of PART-6. (label)
     //*-----------------//
// end_of_part_6:
           
     //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     //* PART-7: Evaluation of outputs.
     //*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++//
           
     //-----------------------------------------------------------*
     //* Assign outputs.
     //*-----------------//
        
     //---------------------------------------------------*
     //* Channel current and conductances.
     //*-----------------//
     idse  = Mfactor * Ids ;
     IdsAcc = Mfactor * Ids_acc ;
     IdsRes = Mfactor * Ids_res ;
     IresLeak = Mfactor * Ires_leak ;
     IdsIBPCe  = Mfactor * IdsIBPC ;

     Ibjte = wIbjt; // 20160414  Mfactor incl. ibs (diode)

     // -------------------------------------*
     //* Intrinsic charges / capacitances.
     //*-----------------//
     qgexte = 0.0 ;
     qdexte = 0.0 ;
     qsexte = 0.0 ;
     qgov = 0.0 ;
     qdov = 0.0 ;
     qsov = 0.0 ;
     qdp = 0.0 ;
     qsp = 0.0 ;
     if(flg_nqs || COADOV==2) begin // for flat handling of NQS: the NQS charges are added in hsmhvld // (COADOV=2 for debugging; intrinsic charge zero;)
       qge = 0.0 ;
       qde = 0.0 ;
       qse = 0.0 ;
             
       Xd       = Qdrat;
       Qbulk    = Mfactor * Qb ;
       Qi       = Mfactor * Qi ;
                
     end else  begin // QS //
       qge = Mfactor * - (Qb + Qi) ;
       qde = Mfactor * Qd ;
       qse = Mfactor * ( Qi - Qd ) ;
     end

     //---------------------------------------------------*
     //* Add S/D overlap charges/capacitances to intrinsic ones.
     //* - NOTE: This function depends on coadov, a control option.
     //*-----------------//
     if( CONQSOV ) begin
       MKS_DLYOV = MKS_DLYOV * Psl ; //Psl depends on the Vds mode!
       `Fn_SZ(MKS_DLYOV, MKS_DLYOV, 1e-12 , T0)
       tauov = MKS_DLYOV * Cox0 ;
       Qbd_nqs = V(qbd) ;
       Ibd_nqs = (Qbd_nqs - Qbd_qs) / tauov ;
         Qovd  = Qovd - (Qbd_qs - Qbd_nqs) ;
         QbdLD = Qbd_nqs ;
     end else begin
       Qbd_nqs = Qbd_qs ;
     end
     if( COADOV > 0 ) begin
       // note: independent of Vds mode:
       qgov = Mfactor * ( Qgbo - Qovd - Qovs ) ;
       qdov = Mfactor * ( + QbdLD ) ;
       qsov = Mfactor * ( + QbsLD ) ;

       // note: dependent on Vds mode:
       qge = qge + Mfactor * ( + Qgod + Qgos + Qy - Qovd_ADD - Qovs_ADD  ) ;
       qde = qde + Mfactor * ( - Qgod        - Qy + QbdLD_ADD ) ;
       qse = qse + Mfactor * ( - Qgos             + QbsLD_ADD ) ;

       // note: independent of Vds mode:
       qgexte = Mfactor * ( - Qovdext - Qovsext ) ;
       qdexte = Mfactor * QbdLDext ;
       qsexte = Mfactor * QbsLDext ;
           
       // for fringing capacitances (independent of Vds mode) //
       qdp = qdp + Mfactor * ( - Qfd - Qgdo ) ;
       qsp = qsp + Mfactor * ( - Qfs - Qgso ) ;
             
     end

     //---------------------------------------------------*
     //* Substrate/gate/leak currents.
     //*-----------------//
     isube  = Mfactor * (Isub+IsubIBPC) ; // 20180413
     IsubLDe = Mfactor * IsubLD ;
     igbe   = Mfactor * -Igb ;
     if(mode == `HiSIM_NORMAL_MODE) begin
       igde = Mfactor * ( GLPART1 * Igate - Igd ) ;
     end else begin
       igde = Mfactor * ( (1.0e0 - GLPART1 ) * Igate - Igs ) ;
     end
     if(mode == `HiSIM_NORMAL_MODE) begin
       igse = Mfactor * ( (1.0e0  - GLPART1) * Igate - Igs ) ;
     end else begin
       igse = Mfactor * ( GLPART1 * Igate - Igd ) ;
     end
           
     // note: igd and igs are already subjected to mode handling,
     igidle    = Mfactor * Igidl ;
     igisle    = Mfactor * Igisl ;
              
     //---------------------------------------------------*
     //* Von, Vdsat.
     //*-----------------//
     Von = Vth ;
     Vdsat = Vdsat ;
           
     //-----------------------------------------------------------*
     //* Noise.
     //*-----------------//
// Noise coeff. calculation
`define C_QE2    2.0 * `C_QE
     whi_noise = 4.0 * `C_KB* TTEMP * `Kwhite ; 
     noiflick = Mfactor * Nflic ;
     noithrml = Mfactor * Nthrml ;
              
     cgdbd = ddx(qge,V(dp)) ; cgdbd = TYPE * cgdbd ;
     cgsbd = ddx(qge,V(sp)) ; cgsbd = TYPE * cgsbd ;
     cgsb  = (mode > 0) ? cgsbd : cgdbd ;

     noiigate = 0.0e0 ;
     noicross = 0.0e0 ;
     //----------------------------------------------------------*
     //* induced gate noise. ( Part 3/3 )
     //*----------------------//
     if( COIGN != 0 && COTHRML != 0 && flg_ign == 1 && !flg_noqi && UC_CODEP == 0 ) begin
       T0 = `Cox_Small * Cox * weffcv_nf * Leff ;
       T1 = cgsb / Mfactor ;
         Nign0 = `c_16o135 * `C_QE * beta_inv * T1 * T1 / gds0_ign ;
         if( kusai00L > `epsm10 && Vds > `epsm10 ) begin
           MuModA = Muun / Mu ;
           MuModB = ( Muun / Mud_hoso - MuModA ) / Vds ;
           correct_w1 = MuModA + `C_2o3 * MuModB
                      * ( kusai00 + VgVt * sqrtkusaiL + kusaiL )
                      / ( VgVt + sqrtkusaiL ) ;
         end else begin
           correct_w1 = Muun / Mud_hoso ;
         end
         noiigate = Mfactor * Nign0 * kusai_ig * correct_w1 ;
         noicross = crl_f ;
         noiigate = ( noiigate < 0.0 ) ? 0.0e0 : noiigate ;
       noiigate = ( - T1 > T0 ) ? noiigate : 0.0 ;
       noicross = ( - T1 > T0 ) ? noicross : 0.0 ;
     end
     // Thermal noise and induced gate noise:
     sid    = whi_noise * noithrml;
     ci     = noicross;    
     sigrat = (sid > 0.0 && noiigate > 0.0) ? sqrt(noiigate/sid) : 0.0 ;
     sigrat_s  = (mode > 0) ? sigrat*(1-Qdrat) : sigrat*   Qdrat ;
     sigrat_d  = (mode > 0) ? sigrat*   Qdrat  : sigrat*(1-Qdrat) ;
              
     //-----------------------------------------------------------*
     //* End of PART-7. (label)
     //*-----------------//
          

     //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     //* PART of New Drift Resistance Model.
     //*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++//
     Rsde=0.0; 
     Rdde=0.0;
        if( UC_CORDRIFT == 1 ) begin          
`include "HSMHV_eval_rdrifts.inc"
`include "HSMHV_eval_rdrift.inc"
        end else begin // UC_CORDRIFT == 0
          // Clamping to Res_min // 
          // 20161025 Might need use flooring function to avoid zero and not to hinder VA`s autogeneration of derivative codes.
          if(Rdd < `Res_min) begin
            Rdd = `Res_min ;
          end
          if(Rsd < `Res_min) begin
            Rsd = `Res_min ;
          end
          if( VdseModeNML > 0.0 ) begin
            Rdde = Rdd / Mfactor ;
            Rsde = Rsd / Mfactor ;
          end else begin
            Rdde = Rsd / Mfactor ;
            Rsde = Rdd / Mfactor ;
          end
        end // if( UC_CORDRIFT == 1 ) else block
     //-----------------------------------------------------------*
     //* End of New Drift Resistance Model.
     //*-----------------//
          
          
// *-------------------------------------------------------------
// * Actually load the device currents                 hsmhvld.c
// * 
          
     // note: Rdde and Rsde are already subjected to mode handling,
     Rdd       = Rdde ;
     Rsd       = Rsde ;
     // note: igd and igs are already subjected to mode handling,
     Igd       =   igde ;
     Igs       =   igse ;
     Igb       =   igbe ;
     if( mode > 0 ) begin // forward mode //
       Ids       = idse ;
       Qd        = qde ;
       Qg        = qge ;
       Qs        = qse ;
       Qb        = - (qge + qde + qse) ;
       Isub      = isube ;
       Isubs     = 0.0 ;
       IsubLD    = IsubLDe ;
       IsubLDs   = 0.0 ;
       IdsIBPC   = IdsIBPCe ;
       IdsIBPCs  = 0.0 ;
       Ibjt  = Ibjte; // 20160414
       Ibjts = 0.0;   // 20160414
       Ibreak    = Ibreake ;
       Ibreaks   = 0.0 ;
       Igidl     = igidle ;
       Igisl     = igisle ;
                    
       //---------------------------------------------------*
       if(flg_nqs) begin
         Qdrat   = Xd         ;
       end
                   
     end else begin // reverse mode //
       Ids       = - idse ;
       Qd        =   qse ;
       Qg        =   qge ;
       Qs        =   qde ;
       Qb        = - (qge + qde + qse) ;
       Isub      = 0.0 ;
       Isubs     =   isube ;
       IsubLD    = 0.0 ;
       IsubLDs   =   IsubLDe ;
       IdsIBPC   = 0.0 ;
       IdsIBPCs  =   IdsIBPCe ;
       Ibjt     = 0.0;    // 20160414
       Ibjts    = Ibjte;  // 20160414
       Ibreak    = 0.0 ;
       Ibreaks   = Ibreake ;
       Igidl     =   igisle ;
       Igisl     =   igidle ;
                    
       //---------------------------------------------------*
       if(flg_nqs) begin
         Qdrat   =   1.0 - Xd         ;
       end
     end // end of reverse mode //
     // note: independent of Vds mode:
     Qg = Qg + qgov ;
     Qd = Qd + qdov ;
     Qs = Qs + qsov ;
     Qb = - (Qg + Qd + Qs) ;
     // note: independent of Vdse mode:
     Qfd       = qdp ;
     Qfs       = qsp ;
     Qdext     = qdexte ;
     Qgext     = qgexte ;
     Qbext     = - (qgexte + qdexte + qsexte) ;


     if(COSELFHEAT > 0) begin
       if(RTH > `C_RTH_MIN) begin
         Gth = 1.0/RTH ;
       end else begin
         Gth = 1.0/`C_RTH_MIN ;
       end
       //     P = Ids * (Vdsi + param * ( Vdse - Vdsi)) //
       //       = Ids * Veffpower                       //
       if( Vdsi *(Vdsei - Vdsi) >= 0.0) begin
         if( UC_POWRAT == 1.0 ) begin
           Veffpower = Vdsei ;
         end else begin
           Veffpower = Vdsi + powratio * (Vdsei - Vdsi) ;
         end
       end else begin
         Veffpower   = Vdsi ;
       end
       P  = Ids  * Veffpower ;
                   
       if( COSELFHEAT == 1 ) begin  
         // Clamping the maximum temperature rise (SHEMAX) //
         T1 = SHEMAX * Gth ;
         `Fn_SU( T2 , P , T1 , SHEMAXDLT * Gth , T0 ) 
         P = T2 ;
       end

     end else begin
       Gth = 0.0 ;
       P   = 0.0 ;
     end
                 
     // in case of nqs: construct static contributions to the nqs equations(Iqi_nqs, Iqb_nqs)       //
     //   and nqs charge contributions to inner drain, gate and source node(Qd_nqs, Qg_nqs, Qs_nqs) //
     if(flg_nqs) begin
       // .. tau, taub must be > 0 //
       if(tau < `tau_min) begin
          tau = `tau_min ;
       end
       if(taub < `tau_min) begin
          taub = `tau_min ;
       end
       Iqi_nqs          = (Qi_nqs - Qi) / tau ;
       Iqb_nqs          = (Qb_nqs - Qbulk) / taub ;
       Qd_nqs           = Qi_nqs * Qdrat ;
       Qg_nqs           = - Qi_nqs - Qb_nqs ;
       Qs_nqs           =   Qi_nqs * (1.0 - Qdrat) ;
     end else begin
       Iqi_nqs = 0.0 ; Iqb_nqs = 0.0 ; Qd_nqs = 0.0 ; Qg_nqs = 0.0 ; Qs_nqs = 0.0 ;
     end
             
     //---------------------------------------------------*
     //* 
     //--print Op points--------*/
     idse    =  TYPE * mode * Ids ;
     Isuba   =  TYPE * (Isub + Isubs) ;

     ggds    =  ddx(idse,V(dp)) ; gdse = ggds ;
     ggdss   =  ddx(idse,V(sp)) ;
     gme     =  ddx(idse,V(gp)) ; 
     gmbse   =  ddx(idse,V(bp)) ; 
     gmte    =  ddx(idse,V(temp));
     gmte    =  ddx(idse,V(temp));

     gbgs    =  ddx(Isuba,V(gp)); 
     gbdsd   =  ddx(Isuba,V(dp));
     gbdss   =  ddx(Isuba,V(sp)); 
     gbds    =  gbdsd + gbdss ;
     gbbs    =  ddx(Isuba,V(bp)); 

     cggbd   =  ddx(Qg,V(gp)) ; cggbd = TYPE * cggbd ;
     cgdbd   =  ddx(Qg,V(dp)) ; cgdbd = TYPE * cgdbd ;
     cgsbd   =  ddx(Qg,V(sp)) ; cgsbd = TYPE * cgsbd ;
     cgbbd   =  ddx(Qg,V(bp)) ; cgbbd = TYPE * cgbbd ;

     cdgbd   =  ddx(Qd,V(gp)) ; cdgbd = TYPE * cdgbd ;
     cddbd   =  ddx(Qd,V(dp)) ; cddbd = TYPE * cddbd ;
     cdsbd   =  ddx(Qd,V(sp)) ; cdsbd = TYPE * cdsbd ;
     cdbbd   =  ddx(Qd,V(bp)) ; cdbbd = TYPE * cdbbd ;

     cbgbd   =  ddx(Qb,V(gp)) ; cbgbd = TYPE * cbgbd ;
     cbdbd   =  ddx(Qb,V(dp)) ; cbdbd = TYPE * cbdbd ;
     cbsbd   =  ddx(Qb,V(sp)) ; cbsbd = TYPE * cbsbd ;
     cbbbd   =  ddx(Qb,V(bp)) ; cbbbd = TYPE * cbbbd ;

     csgbd   =  ddx(Qs,V(gp)) ; csgbd = TYPE * csgbd ;
     csdbd   =  ddx(Qs,V(dp)) ; csdbd = TYPE * csdbd ;
     cssbd   =  ddx(Qs,V(sp)) ; cssbd = TYPE * cssbd ;
     csbbd   =  ddx(Qs,V(bp)) ; csbbd = TYPE * csbbd ;

     cgdobe  =  ddx(qgov,V(d))  ; cgdobe = TYPE * cgdobe ;
     cgsobe  =  ddx(qgov,V(s))  ; cgsobe = TYPE * cgsobe ;
     cgdobd  =  ddx(qgov,V(dp)) ; cgdobd = TYPE * cgdobd + cgdobe ;
     cgsobd  =  ddx(qgov,V(sp)) ; cgsobd = TYPE * cgsobd + cgsobe ;
     cgbobd  =  ddx(qgov,V(bp)) ; cgbobd = TYPE * cgbobd ;
             
     if(mode == `HiSIM_NORMAL_MODE) begin

       cggb  = cggbd ;
       cgdb  = cgdbd ;
       cgsb  = cgsbd ;

       cdgb  = cdgbd ;
       cddb  = cddbd ; 
       cdsb  = cdsbd ;
             
       cbgb  = cbgbd ;
       cbdb  = cbdbd ; 
       cbsb  = cbsbd ;

     end else begin
       ggds  = ggdss ;
       cggb  = cggbd ;
       cgdb  = cgsbd ; 
       cgsb  = cgdbd ;

       cdgb  = csgbd ;
       cddb  = cssbd ; 
       cdsb  = csdbd ;
             
       cbgb  = cbgbd ;
       cbdb  = cbsbd ; 
       cbsb  = cbdbd ;
             
     end

     /* Temperatures */
     TK = TTEMP ;
     DTSH = deltemp ;

`ifdef COEXTDIO 
       /* for Junction Diode Model */
`else 
     ibdb    =  TYPE * Ibd  ;
     ibsb    =  TYPE * Ibs  ;
     ibdbi   =  TYPE * Ibdi ;
     ibsbi   =  TYPE * Ibsi ;
     Gbd     =  ddx(ibdb,V(db));  
     Gbs     =  ddx(ibsb,V(sb));  
     capbdb  =  Capbd  ; 
     capbsb  =  Capbs  ; 
     capbdbi =  Capbdi ; 
     capbsbi =  Capbsi ; 
`endif /* End of COEXTDIO */

`ifdef _AGING_
     DVTBTI = dVth_bti ;
     NTRAP0 = nt0s ;
     NTRAPL = ntls ;
`endif

     OP_mode  = mode  ;
     OP_Vdsi  = Vdsi  ;
     OP_Vbscl = Vbscl ;

// print all outputs ------------VV //
     if( INFO >= 8 ) begin
       $write( "von,vdsat = %18.10e %18.10e \n" , Von, Vdsat ) ;
       $write( "Ids       = %18.10e \n" , idse ) ;
       $write( "Rdd,Rdd   = %18.10e %18.10e \n" , Rdd , Rsd ) ;
       $write( "gds       = %18.10e \n" , gdse ) ;
       $write( "gm        = %18.10e \n" , gme  ) ;
       $write( "gmbs      = %18.10e \n" , gmbse) ;
       $write( "gmt       = %18.10e \n" , gmte ) ;
       $write( "Qg        = %18.10e \n" , Qg ) ;
       $write( "Qd        = %18.10e \n" , Qd ) ;
       $write( "Qs        = %18.10e \n" , Qs  ) ;
       $write( "cggb      = %18.10e \n" , cggb  ) ;
       $write( "cgsb      = %18.10e \n" , cgsb  ) ;
       $write( "cgdb      = %18.10e \n" , cgdb  ) ;
       $write( "cbgb      = %18.10e \n" , cbgb  ) ;
       $write( "cbsb      = %18.10e \n" , cbsb  ) ;
       $write( "cbdb      = %18.10e \n" , cbdb  ) ;
       $write( "cdgb      = %18.10e \n" , cdgb  ) ;
       $write( "cdsb      = %18.10e \n" , cdsb  ) ;
       $write( "cddb      = %18.10e \n" , cddb  ) ;
       $write( "Isub      = %18.10e \n" , Isuba ) ;
       $write( "gbgs      = %18.10e \n" , gbgs ) ;
       $write( "gbds      = %18.10e \n" , gbds ) ;
       $write( "gbbs      = %18.10e \n" , gbbs ) ;
       $write( "tau,taub  = %18.10e %18.10e\n" , tau, taub ) ;
       $write( "Qi   ,Qi_nqs,Iqi_nqs = %18.10e %18.10e %18.10e\n" , Qi   , Qi_nqs, Iqi_nqs ) ;
       $write( "Qbulk,Qb_nqs,Iqb_nqs = %18.10e %18.10e %18.10e\n" , Qbulk, Qb_nqs, Iqb_nqs ) ;

`ifdef _AGING_
       $write( "DVTBTI    = %18.10e \n" , DVTBTI) ;
       $write( "NTRAP0    = %18.10e \n" , NTRAP0) ;
       $write( "NTRAPL    = %18.10e \n" , NTRAPL) ;
`endif

`ifdef COEXTDIO 
       /* for Junction Diode Model */
`else 
       $write( "Ibd,Ibs     = %18.10e %18.10e\n" , ibdb , ibsb ) ;
       $write( "Ibdi,Ibsi   = %18.10e %18.10e\n" , ibdbi, ibsbi ) ;
       $write( "Gbd,Gbs     = %18.10e %18.10e\n" , Gbd  , Gbs ) ;
       $write( "Capbd,Capbs = %18.10e %18.10e\n" , capbdb , capbsb ) ;
       $write( "Capbdi,Capbsi = %18.10e %18.10e\n" , capbdbi, capbsbi ) ;
       $write( "Qbd,Qbs     = %18.10e %18.10e\n" , Qbd , Qbs ) ;
       $write( "Qbdi,Qbsi   = %18.10e %18.10e\n" , Qbdi , Qbsi ) ;
`endif /* End of COEXTDIO */

     end
// print all outputs ------------AA //

//`include "dbgprn5.cnt"

// Drain , Substrate ,  GIDL and GISL Currents
     I(BRdpsp) <+  TYPE * (Ids  + IdsIBPC - IdsIBPCs ) ;
     I(BRds  ) <+  TYPE * ( Ibreak - Ibreaks ); // Ibreak, -Ibreaks revived 20161016.
     I(BRdpbp) <+  TYPE * (Igidl + Isub  + Ibjt  ); // Ibreak added 20160204; Ibreak removed; Ibjt entered 20160414; Ibreak revived 20161012; removed 20161016
     I(BRspbp) <+  TYPE * (Igisl + Isubs + Ibjts );  // Ibreaks added 20160204; Ibreaks removed; Ibjts entered    20160414; Ibreaks revived 20161012; removed 20161016
     I(BRdbp ) <+  TYPE * IsubLD ;
     I(BRsbp ) <+  TYPE * IsubLDs ;

`ifdef COEXTDIO 
       /* for Junction Diode Model */
`else 
// Junction Diode Currents 
     I(BRsbs ) <+  TYPE * Ibs ;
     I(BRdbd ) <+  TYPE * Ibd ;
     I(BRsbs ) <+  TYPE * ddt(Qbs) ;
     I(BRdbd ) <+  TYPE * ddt(Qbd) ;
     if ( CODIO > 0 ) begin //new model:
        I(BRbpsp) <+  TYPE * Ibsi ;
        I(BRbpdp) <+  TYPE * Ibdi ;
        I(BRbpsp) <+  TYPE * ddt(Qbsi) ;
        I(BRbpdp) <+  TYPE * ddt(Qbdi) ;
     end
//     wk_loadIbs=Ibs; wk_loadIbsi=Ibsi; wk_idsibpce = IdsIBPCe;  // probing only

`endif /* End of COEXTDIO */

// Gate Leakage Currents
     if(COIIGS == 1) begin
       I(BRgpsp) <+  TYPE * Igs;
       I(BRgpdp) <+  TYPE * Igd;
       I(BRgpbp) <+  TYPE * Igb;
     end

// Source & Drain Resistance Currents
     //flg_rd = (CORD > 0) ? 1:0; // moved upfront
     //flg_rs = (CORS > 0) ? 1:0; // moved upfront
     if(flg_rd) I(BRddp ) <+ V(BRddp ) / Rdd; else V(BRddp ) <+ 0.0 ;
     if(flg_rs) I(BRsps ) <+ V(BRsps ) / Rsd; else V(BRsps ) <+ 0.0 ;

//
// Inner Capacitance Currents
     I(BRgpsp) <+  TYPE * ddt(Qg + Qg_nqs);
     I(BRdpsp) <+  TYPE * ddt(Qd + Qd_nqs);
     I(BRbpsp) <+  TYPE * ddt(Qb - (Qg_nqs + Qd_nqs + Qs_nqs));
     I(BRgps)  <+  TYPE * ddt(Qgext);
     I(BRds)   <+  TYPE * ddt(Qdext);
     I(BRbps)  <+  TYPE * ddt(Qbext);
 
// Outer Fringe Capacitance Currents
     I(BRgpd ) <+ -TYPE * ddt(Qfd);
     I(BRgps ) <+ -TYPE * ddt(Qfs);
//
// Noise Currents
     if( flg_rsrd != 1 && flg_rsrd != 3 && UC_CORDRIFT != 1 ) begin
       drainConductance = 0.0 ; sourceConductance = 0.0 ;
     end
// 1/F Noise
     I(BRdpsp) <+ flicker_noise(noiflick * `Kflic , UC_FALPH  , "iflick");
// Thermal Noise & IG Noise
     I(n)      <+ 1*V(n) ;
     I(n)      <+ white_noise(sid) ;
     I(BRdpsp) <+ white_noise((1.0-ci*ci)*sid) ;
     I(BRdpsp) <+ ci*V(n) ;
     I(BRgpsp) <+ ddt(V(n)*sigrat_s) ;
     I(BRgpdp) <+ ddt(V(n)*sigrat_d) ;

     if (flg_rs) I(BRsps ) <+ white_noise(whi_noise * sourceConductance , "isource");
     if (flg_rd) I(BRddp ) <+ white_noise(whi_noise * drainConductance  , "idrain");
// Shot Noise
     I(BRgpdp) <+ white_noise(`C_QE2 * abs(Igd) , "iigd");
     I(BRgpsp) <+ white_noise(`C_QE2 * abs(Igs) , "iigs");
     I(BRgpbp) <+ white_noise(`C_QE2 * abs(Igb) , "iigb");
//
// Gate Resistance Currents
     if(CORG ==1 && RSHG > 0.0) I(BRggp) <+ grg * V(BRggp);
     else V(BRggp) <+ 0.0 ;
//
// Substrate Resistance Network
     if(CORBNET) begin
       I(BRsbbp) <+  grbps * V(BRsbbp) ;
       I(BRdbbp) <+  grbpd * V(BRdbbp) ;
       I(BRbbp ) <+  grbpb * V(BRbbp ) ;
     end else begin
       V(BRbbp ) <+  0.0 ; V(BRsbbp) <+  0.0 ; V(BRdbbp) <+  0.0 ; 
     end
//     wk_vbrbbp = V(BRbbp); wk_ibrbbp = grbpb * V(BRbbp);  // probing 20180410
//
// Thermal Node Currents with thermal dissipation
     if(COSELFHEAT > 0) begin
       Itemp = P ;
       I(temp) <+ V(temp) * Gth ;
       I(temp) <+ - Itemp;
     end else begin
       CTH = 0.0;
       I(temp) <+ V(temp)*1e4;
     end
     I(temp) <+ ddt(CTH*V(temp));
//
// NQS Node Currents 
     if(CONQS) begin
       I(qi) <+ Iqi_nqs;
       I(qb) <+ Iqb_nqs;
       Cqi = 1.0; Cqb = 1.0;
       I(qi) <+ ddt(Cqi*V(qi));
       I(qb) <+ ddt(Cqb*V(qb));
     end else begin
       V(qi) <+ 0.0;
       V(qb) <+ 0.0;
     end
// NQS Node Currents for Qover
     if(CONQSOV) begin
       I(qbd) <+ Ibd_nqs;
       I(qbd) <+ ddt(V(qbd));
     end else begin
       V(qbd) <+ 0;
     end


`ifdef _AGING_
     // **************************************** //
     // integration Isub & Ontime
     // **************************************** //
     T0 =  TYPE * Isuba ;
     T0 = ( T0 < 0 || `pretime > $abstime ) ? 0.0 : T0 ;
     T1 =  TYPE * idse ;
     T1 = ( T1 < 0 || `pretime > $abstime ) ? 0.0 : T1 ;
     V(idtag1) <+ idt( T0 , 0 );
     V(idtag2) <+ idt( T1 , 0 );
     T2 = ( Vgs > Vth  && TRAPA > 0 && `pretime < $abstime ) ? 1.0 : 0.0 ;
     T3 = ( Vgs > Vth  && TRAPA > 0 && `pretime < $abstime ) ? Vgs : 0.0 ;
     if ( TRAPA > 0 && CODEGSTEP == 1 ) begin      // for BTI model
       V(idtag3) <+ idt( T2 , 0 ); // On time 
       V(idtag4) <+ idt( T3 , 0 ); // Vgp  
     end else begin
       V(idtag3) <+ 0 ;
       V(idtag4) <+ 0 ;
     end
     if ( CODEG == 1 && TRAPA > 0 ) begin          // for BTI model
       V(dVfb)   <+ dVth_bti ;
     end else begin
       V(dVfb)   <+ 0 ;
     end
     if( CODEG == 1 && TRAPGC1MAX > 0 ) begin      // for HC model
       V(vgc1)    <+ gc1/1e24;
       V(ve1)     <+ e1;
     end else begin
       V(vgc1)    <+ 0 ;
       V(ve1)     <+ 0 ;
     end
     if( TRAPGC1MAX > 0 && CODEGSTEP == 0 ) begin  // for HC model
       V(ps0)     <+ Ps0_prv ;
       V(vtraplx) <+ Vtraplx_prv ;
     end else begin
       V(ps0)     <+ 0 ;
       V(vtraplx) <+ 0 ;
     end
     // ************************************************ //
     // output the degradation file for aging simulation
     // ************************************************ //
     if( CODEG == 1 && CODEGSTEP == 0 ) begin
       fno=$fopen("degradation.out") ;
       ivgs = V(g,s) ;
       ivds = V(d,s) ;
       ivbs = V(b,s) ;
       $fdisplay(fno,".param %M_DEGTIME=%g %M_gc1deg=%.5e %M_es1deg=%.5g %M_dVthbti=%.5g %M_Dvddpdeg=%.5g %M_TRAPGC1=%g %M_TRAPGC2=%g %M_TRAPGC1MAX=%g %M_TRAPGCTIME1=%g %M_TRAPGCTIME2=%g %M_TRAPES1=%g %M_TRAPES2=%g %M_TRAPES1MAX=%g %M_TRAPLX=%g \n*     Isub_trap=%g Ids=%g VGDEG:%g VDDEG:%g VBDEG:%g",
       DEGTIME,gc1/`C_m2cm_p3,e1,dVth_bti,DVddp,
       TRAPGC1,TRAPGC2,TRAPGC1MAX,TRAPGCTIME1,TRAPGCTIME2,TRAPES1,TRAPES2,TRAPES1MAX,TRAPLX,
       Isub_trap,Ids,ivgs,ivds,ivbs) ;
     end
     if( CODEG == 1 && CODEGSTEP == 1 ) begin
       @(final_step("tran")) begin
         fno=$fopen("degtran.out") ;
         $fdisplay(fno,".param %M_DEGTIME=%g %M_gc1deg=%.5e %M_es1deg=%.5g %M_dVthbti=%.5g %M_Dvddpdeg=%.5g \n*     Isub_trap=%g Ids=%g tcycle=%g Vgsonave=%g time:%g ",
         DEGTIME,gc1/`C_m2cm_p3,e1,dVth_bti,DVddp, Isub_trap,Ids, tcycle,Vgsonave, $abstime) ;
       end
     end

`endif

//`include "dbgprn5.cnt"

   end 
//================ End of executable code.=================//
