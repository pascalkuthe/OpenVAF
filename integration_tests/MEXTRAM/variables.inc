// Copyright (c) 2000-2007, NXP Semiconductor
// Copyright (c) 2007-2014, Delft University of Technology
// Copyright (c) 2015-2017, Auburn University
// All rights reserved, see IP_NOTICE_DISCLAIMER_LICENSE for further information.

// Declaration of variables

// Model constants

real An, Bn;

// Temperature scaling variables

real Tk, Trk, tN, Tamb;
real Vt, Vtr, VtINV, VtrINV, VdtINV;
real Vdt;
real dT;

real UdeT, vde_t, UdcT, vdc_t, vdc_ctc_t, UdcT_ctc, vdc_zener_t, UdcT_zener;
real cje_t, cjc_t, xp_t;
real cjc_scale, cjc_scale_inv;

real re_t, rbv_t, rbc_t, rcv_t;
real rcc_xx_t, rcc_ex_t, rcc_in_t;

real is_t, ik_t, ikbx_t, ibf_t, ibr_t, vef_t, ver_t, ibi_t, ibx_t, ibis_t, ibfs_t, isibrel_t;
real nff_t, nfr_t, nff_t_tmp, nfr_t_tmp, eps_nf;

// Zener tunneling parameters and variables:
real Izteb, izeb_t, E0BE, dE0BE,nzeb_t, pow2_2m_pe, pow2_pe_m2, inv_vde, inv_vde_t, inv_vdc_zener, inv_vdc_zener_t;
real e_zeb, e_dzeb, dzeb, vgzeb_t, vgzeb_tr, inv_vgzeb_tr, cje_t_div_cje;
real vgzebok;
real Vcbr, Vdc_zener, Pc_zener, E0CB, e_zcb, pow2_2m_pc, dE0CB, e_dzcb, dzcb, Iztcb, nzcb_t, izcb_t, vgzcb_t, inv_vgzcb_tr, vgzcb_tr, vgzcbok, pow2_pc_m2;
real cjc_t_div_cjc_zener;

real taue_t, taub_t, tepi_t, taur_t;
real Bnt, deg_t, Tk300;

`ifdef SELFHEATING
    real rth_tamb;
`endif

`ifdef SUBSTRATE
    real UdsT, vds_t, cjs_t, iss_t, icss_t, iks_t;
`endif

// mult - scaling variables

real inv_mult;
real is_tm, ik_tm, ikbx_tm, ibf_tm, ibr_tm, ihc_m, ibi_tm, ibx_tm, ibis_tm, ibfs_tm, isibrel_tm;
// Zener tunneling parameters
real izeb_tm, izcb_tm;

real cje_tm, cjc_tm;

real re_tm, rbc_tm, rbv_tm, rcv_tm, scrcv_m;
real rcc_xx_tm, rcc_ex_tm, rcc_in_tm;
real gcc_xx_tm, gcc_ex_tm, gcc_in_tm;

real kf_m, kfn_m;

`ifdef SELFHEATING
    real rth_tamb_m, cth_m;
`endif

`ifdef SUBSTRATE
    real iss_tm, icss_tm, iks_tm, cjs_tm;
`endif


// Epilayer model variables

real K0, Kw, pW, Ec, Ic1c2;
real Vqs_th, Vqs, Iqs;
real alpha, vyi, yi, xi_w, xi_w1;
real gp0, gp02, p0star, Vb2c2star, eVb2c2star, eVb2c2star_nfr;
real B1, B2, Vxi0, Vch, Icap, pav;

// Effective emitter and collector junction bias variables

real Vfe, Vje, Vte;
real Vjunc, bjc, Vfc, Vjc, fI, Vcv, Vtc;

// Transfer current variables

real If0, f1, f2, n0, nB;
real q0I, q1I, qBI, Ir, If, In;

// Base and substrate current(s) variables

real Xext1;
real Ib1, Ib1_s, Ib2, Ib3, Ib2_s, Ibrel;
real Iex;
real g1, g2, pWex, nBex;
real Xg1, XnBex, XIMex, XIMsub, Vex, VBex, Fex, XIex;

`ifdef SUBSTRATE
    real Isub, XIsub, Isf;
`endif

// Distributed base effects variables

real q0Q, q1Q, qBQ, Rb2, Ib1b2;
real dVteVb2e1, dVteVje, dVjeVb2e1;
real dQteVb2e1, dQbeVb2e1, dQeVb2e1;
real dn0Vb2e1;

// swavl=2 (504) avalanche current variables

real dEdx0, xd, Weff, Wd, Eav, E0, Em, SHw, Efi, Ew;
real lambda, Gem, Gmax, Iavl;
real Icap_ihc;

`ifdef SELFHEATING
    real Tki, power;
`endif

// Charges and capacitances variables

real Qte, Vje_s, Qte_s;
real Qtc;
real Qb0, Qbe, Qbc, Qb1b2;
real Qbe_qs, Qbc_qs;
real Vjcex, Vtexv, Qtex, XVjcex, XVtexv, XQtex;

`ifdef SUBSTRATE
    real Vfs, Vjs, Qts;
`endif

real Qe0, Qe;
real Qe_qs;
real Qepi0, Qepi, Xg2, XpWex, XQex;
real Qex;
real cbeo_m, cbco_m;

// Biases and exponential terms variables

real Vb2c1, Vb2c2, Vb2e1, Vb1e1, Vb1b2, Vb1c4, Vc1c2;
real Vc3c4, Vc4c1;
`ifdef  SUBSTRATE
    real Vsc1, Vsc3, Vsc4, eVsc1, eVsc3, eVsc4;
`endif
real Vee1, Vbb1, Vbc3, Vcc3, Vbe, Vbc;
real eVb2c2, eVb2e1, eVb1b2, eVb1c4, eVbc3;
real eVb1c4vdc, eVb2c2vdc, eVbc3vdc, eVb2c1vdc;

// Help variables

// lntN introduced to speed up T-scaling:
// Acknowledgements due to Geoffrey Coram
real lntN;

// Variables for local use; may be re-used globally:
real x, y, xx, yy;

real dxa, sqr_arg;
real eps2, x2;
real alpha1, vdif, Ic1c2_Iqs, gp0_help;
real EmEav_Em, Vb2e1Vfe, termE, termC;
real Vex_bias;
real eps_vdc, a_vde, a_vdcctc;

real expl, tmpExp, tmpV;


`ifdef  SUBSTRATE
    real a_vds;
`endif

// Noise variables
real common;
real power_rec, power_rbc, power_rcc_xx, power_rcc_ex, power_rcc_in, power_rbv;
real powerCCS;
real powerFBCS;
real powerFBC1f;
real powerNFBC1f;
real powerEBSCS;
real powerRBCS, powerRBC1f, powerZTCB;
real powerExCS, powerExCSMOD, powerExC1f, powerExC1fMOD;
real powerIIS;

`ifdef SUBSTRATE
    real powerSubsCS_B1S, powerSubsCS_BS;
`endif

// noise correlation help variables
real In_N, Gem_N, Taub_N, taun, Qbe_qs_eff;

// New avalanche model when swavl=1
real expIn, vl, bavl_t, bavl_t_tmp, eps_bavl_t, expMm1;

// Minimum conductances
real GMIN;
