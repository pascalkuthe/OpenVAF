// Copyright (c) 2000-2007, NXP Semiconductor
// Copyright (c) 2007-2014, Delft University of Technology
// Copyright (c) 2015-2017, Auburn University
// All rights reserved, see IP_NOTICE_DISCLAIMER_LICENSE for further information.

//Instance parameters
`IPRnb( dta            ,0.0            ,"degC"                                   ,"Difference between the local and global ambient temperatures" )
`IPRoo( mult           ,1.0            ,""            ,0.0         ,inf          ,"Multiplication factor" )

// Mextram parameters
`MPRco( version        ,505.00         ,""            ,505.00      ,505.01       ,"Model version" )
`MPIty( type           ,1              ,""                                       ,"Flag for NPN (1) or PNP (-1) transistor type" )
`MPRco( tref           ,25.0           ,"degC"        ,-273.0      ,inf          ,"Reference temperature" )
`MPIcc( exmod          ,1              ,""            ,0           ,2            ,"Flag for extended modeling of the reverse current gain" )
`MPIcc( exphi          ,1              ,""            ,0           ,1            ,"Flag for distributed high-frequency effects in transient" )
`MPIcc( exavl          ,0              ,""            ,0           ,1            ,"Flag for extended modeling of avalanche currents" )

`ifdef SUBSTRATE
    `MPIcc( exsub          ,1              ,""            ,0           ,1           ,"Flag for extended modeling of substrate currents" )
`endif

// main current parameters
`MPRoo( is             ,22.0a          ,"A"           ,0.0         ,inf          ,"Saturation current of main current" )
`MPRco( nff            ,1.0            ,""            ,1.0         ,inf          ,"Non-ideality factor of forward main current" )
`MPRco( nfr            ,1.0            ,""            ,1.0         ,inf          ,"Non-ideality factor of reverse main current" )
`MPRco( ik             ,0.1            ,"A"           ,1.0p        ,inf          ,"CE high injection knee current" )
`MPRco( ver            ,2.5            ,"V"           ,0.01        ,inf          ,"Reverse Early voltage" )
`MPRco( vef            ,44.0           ,"V"           ,0.01        ,inf          ,"Forward Early voltage" )
// forward base current parameters
`MPRco( ibi            ,0.1a           ,"A"           ,0.0         ,inf          ,"Saturation current of ideal base current" )
`MPRco( nbi            ,1.0            ,""            ,0.1         ,inf          ,"Non-ideality factor of ideal base current" )
`MPRco( ibis           ,0.0            ,"A"           ,0.0         ,inf          ,"Saturation current of ideal side wall base current" )
`MPRco( nbis           ,1.0            ,""            ,0.1         ,inf          ,"Non-ideality factor of ideal side wall base current" )
`MPRco( ibf            ,2.7f           ,"A"           ,0.0         ,inf          ,"Saturation current of non-ideal forward base current" )
`MPRco( mlf            ,2.0            ,""            ,0.1         ,inf          ,"Non-ideality factor of non-ideal forward base current" )
`MPRco( ibfs           ,0.0            ,"A"           ,0.0         ,inf          ,"Saturation current of non-ideal side wall forward base current" )
`MPRco( mlfs           ,2.0            ,""            ,0.1         ,inf          ,"Non-ideality factor of non-ideal side wall forward base current" )
// reverse base current parameters
`MPRco( ibx            ,3.14a          ,"A"           ,0.0         ,inf          ,"Saturation current of extrinsic reverse base current" )
`MPRco( ikbx           ,14.29m         ,"A"           ,1.0p        ,inf          ,"Extrinsic CB high injection knee current" )
`MPRco( ibr            ,1.0f           ,"A"           ,0.0         ,inf          ,"Saturation current of non-ideal reverse base current" )
`MPRco( mlr            ,2.0            ,""            ,0.1         ,inf          ,"Non-ideality factor of non-ideal reverse base current" )
`MPRcc( xext           ,0.63           ,""            ,0.0         ,1.0          ,"Part of currents and charges that belong to extrinsic region" )
// EB tunneling model parameters
`MPRco( izeb           ,0.0            ,"A"           ,0.0         ,inf          ,"Pre-factor of EB Zener tunneling current" )
`MPRco( nzeb           ,22.0           ,""            ,0.0         ,inf          ,"Coefficient of EB Zener tunneling current" )
// CB tunneling model parameters
`MPRco( izcb           ,0.0            ,"A"           ,0.0         ,inf          ,"Pre-factor of CB Zener tunneling current" )
`MPRco( nzcb           ,22.0           ,""            ,0.0         ,inf          ,"Coefficient of CB Zener tunneling current" )
// 505 avalanche model parameters
`MPIcc( swavl          ,1              ,""            ,0           ,2            ,"Switch of avalanche factor Gem model" )
`MPRco( aavl           ,400.0          ,""            ,0.0         ,inf          ,"aavl of swavl=1 Gem model" )
`MPRoo( cavl           ,-370.0m        ,""            ,-inf        ,0.0          ,"cavl of swavl=1 Gem model" )
`MPRoo( itoavl         ,500.0m         ,"A"           ,0.0         ,inf          ,"Current dependence parameter of swavl=1 Gem model" )
`MPRoo( bavl           ,25.0           ,""            ,0.0         ,inf          ,"bavl of swavl=1 Gem model" )
`MPRoo( vdcavl         ,100.0m         ,"V"           ,-inf        ,inf          ,"CB diffusion voltage dedicated for swavl=1 Gem model" )
// 504 avalanche model parameters
`MPRco( wavl           ,1.1u           ,"m"           ,1.0n        ,inf          ,"Epilayer thickness used in weak-avalanche model" )
`MPRco( vavl           ,3.0            ,"V"           ,0.01        ,inf          ,"Voltage determining curvature of avalanche current" )
`MPRco( sfh            ,0.3            ,""            ,0.0         ,inf          ,"Current spreading factor of avalanche model when exavl=1" )
// resistance parameters
`MPRco( re             ,5.0            ,"Ohm"         ,1.0m        ,inf          ,"Emitter resistance" )
`MPRco( rbc            ,23.0           ,"Ohm"         ,1.0m        ,inf          ,"Constant part of base resistance" )
`MPRco( rbv            ,18.0           ,"Ohm"         ,1.0m        ,inf          ,"Zero-bias value of variable part of the base resistance" )
`MPRco( rcc            ,12.0           ,"Ohm"         ,1.0m        ,inf          ,"Constant part of collector resistance" )
`MPRco( rcblx          ,0.0            ,"Ohm"         ,0.0         ,inf          ,"Resistance Collector Buried Layer extrinsic" )
`MPRco( rcbli          ,0.0            ,"Ohm"         ,0.0         ,inf          ,"Resistance Collector Buried Layer Intrinsic" )
// epilayer dc parameters
`MPRco( rcv            ,150.0          ,"Ohm"         ,1.0m        ,inf          ,"Resistance of un-modulated epilayer" )
`MPRco( scrcv          ,1250.0         ,"Ohm"         ,1.0m        ,inf          ,"Space charge resistance of epilayer" )
`MPRco( ihc            ,4.0m           ,"A"           ,1.0p        ,inf          ,"Critical current for velocity saturation in epilayer" )
`MPRco( axi            ,0.3            ,""            ,0.02        ,inf          ,"Smoothness parameter for onset of quasi-saturation" )
`MPRco( vdc            ,0.68           ,"V"           ,0.05        ,inf          ,"CB diffusion voltage" )
// EB junction capacitance parameters
`MPRco( cje            ,73.0f          ,"F"           ,0.0         ,inf          ,"Zero-bias EB depletion capacitance" )
`MPRco( vde            ,0.95           ,"V"           ,0.05        ,inf          ,"EB diffusion voltage" )
`MPRco( pe             ,0.4            ,""            ,0.01        ,0.99         ,"EB grading coefficient" )
`MPRcc( xcje           ,0.4            ,""            ,0.0         ,1.0          ,"Sidewall fraction of EB depletion capacitance" )
`MPRco( cbeo           ,0.0            ,"F"           ,0.0         ,inf          ,"EB overlap capacitance" )
// CB junction capacitance parameters
`MPRco( cjc            ,78.0f          ,"F"           ,0.0         ,inf          ,"Zero-bias CB depletion capacitance" )
`MPRco( vdcctc         ,0.68           ,"V"           ,0.05        ,inf          ,"CB diffusion voltage of depletion capacitance" )
`MPRco( pc             ,0.5            ,""            ,0.01        ,0.99         ,"CB grading coefficient" )
`MPIcc( swvchc         ,0              ,""            ,0           ,1            ,"Switch of Vch for CB depletion capacitance" )
`MPIcc( swvjunc        ,0              ,""            ,0           ,2            ,"Switch of Vjunc for collector junction capacitance" )
`MPRco( xp             ,0.35           ,""            ,0.0         ,0.99         ,"Constant part of Cjc" )
`MPRco( mc             ,0.5            ,""            ,0.0         ,1.0          ,"Coefficient for current modulation of CB depletion capacitance" )
`MPRcc( xcjc           ,32.0m          ,""            ,0.0         ,1.0          ,"Fraction of CB depletion capacitance under the emitter" )
`MPRco( cbco           ,0.0            ,"F"           ,0.0         ,inf          ,"CB overlap capacitance" )
// transit time parameters
`MPRco( mtau           ,1.0            ,""            ,0.1         ,inf          ,"Non-ideality factor of emitter stored charge" )
`MPRco( taue           ,2.0p           ,"s"           ,0.0         ,inf          ,"Minimum transit time of stored emitter charge" )
`MPRoo( taub           ,4.2p           ,"s"           ,0.0         ,inf          ,"Transit time of stored base charge" )
`MPRco( tepi           ,41.0p          ,"s"           ,0.0         ,inf          ,"Transit time of stored epilayer charge" )
`MPRco( taur           ,520.0p         ,"s"           ,0.0         ,inf          ,"Transit time of reverse extrinsic stored base charge" )
// heterojunction parameters
`MPRnb( deg            ,0.0            ,"eV"                                       ,"Bandgap difference over the base" )
// neutral base recombination parameter
`MPRco( xrec           ,0.0            ,""            ,0.0         ,inf          ,"Pre-factor of the recombination part of Ib1" )
// charge partition parameters
`MPRcc( xqb            ,`one_third     ,""            ,0.0         ,1.0          ,"Emitter-fraction of base diffusion charge" )
`MPRcc( ke             ,0.0            ,""            ,0.0         ,1.0          ,"Fraction of QE in excess phase shift" )
// general temperature scaling parameters
`MPRnb( aqbo           ,0.3            ,""                                       ,"Temperature coefficient of zero-bias base charge" )
`MPRnb( ae             ,0.0            ,""                                       ,"Temperature coefficient of resistivity of the emitter" )
`MPRnb( ab             ,1.0            ,""                                       ,"Temperature coefficient of resistivity of the base" )
`MPRnb( aepi           ,2.5            ,""                                       ,"Temperature coefficient of resistivity of the epilayer" )
`MPRnb( aex            ,0.62           ,""                                       ,"Temperature coefficient of resistivity of the extrinsic base" )
`MPRnb( ac             ,2.0            ,""                                       ,"Temperature coefficient of resistivity of the collector contact" )
`MPRnb( acx            ,1.3            ,""                                       ,"Temperature coefficient of extrinsic reverse base current" )
`MPRco( acbl           ,2.0            ,""            ,0.0         ,inf          ,"Temperature coefficient of resistivity of the collector buried layer" )
`MPRco( vgb            ,1.17           ,"V"           ,0.1         ,inf          ,"Band-gap voltage of base" )
`MPRco( vgc            ,1.18           ,"V"           ,0.1         ,inf          ,"Band-gap voltage of collector" )
`MPRco( vge            ,1.12           ,"V"           ,0.1         ,inf          ,"Band-gap voltage of emitter" )
`MPRco( vgcx           ,1.125          ,"V"           ,0.1         ,inf          ,"Band-gap voltage of extrinsic collector" )
`MPRco( vgj            ,1.15           ,"V"           ,0.1         ,inf          ,"Band-gap voltage recombination EB junction" )
`MPRco( vgzeb          ,1.15           ,"V"           ,0.1         ,inf          ,"Band-gap voltage at Tref for EB tunneling" )
`MPRoo( avgeb          ,4.73e-4        ,"V/K"         ,-inf        ,inf          ,"Temperature coefficient of band-gap voltage for EB tunneling" )
`MPRco( tvgeb          ,636.0          ,"K"           ,0.0         ,inf          ,"Temperature coefficient of band-gap voltage for EB tunneling" )
`MPRco( vgzcb          ,1.15           ,"V"           ,0.1         ,inf          ,"Band-gap voltage at Tref for CB tunneling" )
`MPRoo( avgcb          ,4.73e-4        ,"V/K"         ,-inf        ,inf          ,"Temperature coefficient of band-gap voltage for CB tunneling" )
`MPRco( tvgcb          ,636.0          ,"K"           ,0.0         ,inf          ,"Temperature coefficient of band-gap voltage for CB tunneling" )
`MPRnb( dvgte          ,0.05           ,"V"                                      ,"Band-gap voltage difference of emitter stored charge" )
`MPRnb( dais           ,0.0            ,""                                       ,"Fine tuning of temperature dependence of CE saturation current" )
`MPRoo( tnff           ,0.0            ,"/K"          ,-inf        ,inf          ,"Temperature coefficient of nff" )
`MPRoo( tnfr           ,0.0            ,"/K"          ,-inf        ,inf          ,"Temperature coefficient of nfr" )
`MPRnb( tbavl          ,500.0u         ,""                                       ,"Temperature scaling parameter of bavl when swavl=1" )
// 1/f noise parameters
`MPRco( af             ,2.0            ,""            ,0.01        ,inf          ,"Exponent of Flicker-noise of ideal base current" )
`MPRco( afn            ,2.0            ,""            ,0.01        ,inf          ,"Exponent of Flicker-noise of non-ideal base current" )
`MPRco( kf             ,20.0p          ,""            ,0.0         ,inf          ,"Flicker-noise coefficient of ideal base current" )
`MPRco( kfn            ,20.0p          ,""            ,0.0         ,inf          ,"Flicker-noise coefficient of non-ideal base current" )
// avalanche noise switch
`MPIcc( kavl           ,0              ,""            ,0           ,1            ,"Switch for white noise contribution due to avalanche" )
// correlated noise parameters
`MPIcc( kc             ,0              ,""            ,0           ,2            ,"Switch for RF correlation noise model selection" )
`MPRcc( ftaun          ,0.0            ,""            ,0.0         ,1.0          ,"Fraction of noise transit time to total transit time" )

`ifdef SUBSTRATE
    `MPRco( iss            ,48.0a          ,"A"           ,0.0         ,inf          ,"Saturation current of parasitic BCS transistor main current" )
    `MPRco( icss           ,0.0            ,"A"           ,0.0         ,inf          ,"CS ideal saturation current" )
    `MPRco( iks            ,545.5u         ,"A"           ,1.0p        ,inf          ,"Knee current for BCS transistor main current" )
    `MPRco( cjs            ,315.0f         ,"F"           ,0.0         ,inf          ,"Zero-bias CS depletion capacitance" )
    `MPRoo( vds            ,0.62           ,"V"           ,0.05        ,inf          ,"CS diffusion voltage" )
    `MPRoo( ps             ,0.34           ,""            ,0.01        ,0.99         ,"CS grading coefficient" )
    `MPRco( vgs            ,1.20           ,"V"           ,0.1         ,inf          ,"Band-gap voltage of the substrate" )
    `MPRnb( as             ,1.58           ,""                                       ,"Substrate temperature coefficient" )
    `MPRnb( asub           ,2.0            ,""                                       ,"Temperature coefficient for mobility of minorities in the substrate" )
`endif

// self heating parameters
`ifdef SELFHEATING
    `MPRoo( rth            ,300.0          ,"K/W"         ,0.0         ,inf         ,"Thermal resistance" )
    `MPRco( cth            ,3.0n           ,"J/K"         ,0.0         ,inf         ,"Thermal capacitance" )
    `MPRnb( ath            ,0.0            ,""                                      ,"Temperature coefficient of thermal resistance" )
`endif

// reliability modeling parameters
`MPRco( isibrel        ,0.0            ,"A"           ,0.0         ,inf         ,"Saturation current of base current for reliability simulation" )
`MPRco( nfibrel        ,2.0            ,""            ,0.1         ,inf         ,"Non-ideality factor of base current for reliability simulation" )

`MPRcc( gmin           ,1e-13          ,""            ,0.0         ,1e-10       ,"Minimum conductance" )
