// Copyright (c) 2000-2007, NXP Semiconductor
// Copyright (c) 2007-2014, Delft University of Technology
// Copyright (c) 2015-2017, Auburn University
// All rights reserved, see IP_NOTICE_DISCLAIMER_LICENSE for further information.

// Evaluate model equations

// Currents and charges
// Nodal biases
Vb2c1 = type * V(b2, c1);
Vb2c2 = type * V(b2, c2);
Vb2e1 = type * V(b2, e1);
Vb1e1 = type * V(b1, e1);
Vb1b2 = type * V(b1, b2);
`ifdef SUBSTRATE
    Vsc1  = type * V(s,  c1);
`endif
Vc1c2 = type * V(c1, c2);
Vee1  = type * V(e,  e1);
Vbb1  = type * V(b,  b1);
Vbe   = type * V(b,  e);
Vbc   = type * V(b,  c);

// RvdT, 03-12-2007, voltage differences
//associated with distributed parasitic collector.
//Evaluated taking values of resistances into account:
//in case of vanishing resistance corresponding node
//is not addressed:

if (rcblx > 0.0) begin
    if (rcbli > 0.0) begin
        Vc4c1 = type * V(c4, c1);
        Vc3c4 = type * V(c3, c4);
    end else begin
        Vc4c1 = 0.0;
        Vc3c4 = type * V(c3, c1);
    end
end else begin
    if (rcbli > 0.0) begin
        Vc4c1 = type * V(c4, c1);
        Vc3c4 = 0.0;
    end else begin
        Vc4c1 = 0.0;
        Vc3c4 = 0.0;
    end
end

Vb1c4 = Vb1b2 + Vb2c2 - Vc1c2 - Vc4c1;
Vcc3  = - Vbc + Vbb1 + Vb1c4 - Vc3c4;
Vbc3  = Vbc + Vcc3;

`ifdef  SUBSTRATE
    Vsc4 = Vsc1 - Vc4c1;
    Vsc3 = Vsc4 - Vc3c4;
`endif


// Exponential bias terms

`expLin(eVb2c2,Vb2c2 * VtINV)
`expLin(eVb2e1,Vb2e1 * VtINV / nff_t)
`expLin(eVb1c4,Vb1c4 * VtINV)
`expLin(eVb1b2,Vb1b2 * VtINV)
`expLin(eVbc3,Vbc3  * VtINV)
`ifdef SUBSTRATE
    `expLin(eVsc1,Vsc1  * VtINV)
    // RvdT MXT504.10, new: eVsc3, eVsc4
    `expLin(eVsc3,Vsc3  * VtINV)
    `expLin(eVsc4,Vsc4  * VtINV)
`endif

`expLin(eVbc3vdc,(Vbc3  - vdc_t) * VtINV)
`expLin(eVb1c4vdc,(Vb1c4 - vdc_t) * VtINV)
`expLin(eVb2c2vdc,(Vb2c2 - vdc_t) * VtINV)
`expLin(eVb2c1vdc,(Vb2c1 - vdc_t) * VtINV)

// Governing equations

// Epilayer model

K0 = sqrt(1.0 + 4.0 * eVb2c2vdc);
Kw = sqrt(1.0 + 4.0 * eVb2c1vdc);
pW = 2.0 *  eVb2c1vdc / (1.0 + Kw);
if (pW < `TEN_M40) pW = 0.0;
Ec = Vt * (K0 - Kw - ln((K0 + 1.0) / (Kw + 1.0)) );
Ic1c2 =  (Ec + Vc1c2) / rcv_tm;

if (Ic1c2 > 0.0) begin
    `linLog(tmpV,Vb2c1,100.0)
    Vqs_th = vdc_t +
             2.0 * Vt * ln(0.5 * Ic1c2 * rcv_tm * VtINV + 1.0) - tmpV;
    eps_vdc = 0.2 * vdc_t;
    `max_hyp0(Vqs, Vqs_th, eps_vdc)
    Iqs = Vqs * (Vqs + ihc_m * scrcv_m) / (scrcv_m * (Vqs + ihc_m * rcv_tm));

    Ic1c2_Iqs = Ic1c2 / Iqs;
    `max_logexp(alpha1, Ic1c2_Iqs, 1.0, axi)
    alpha = alpha1 / (1.0 + axi * ln(1.0 + exp(-1.0 / axi)));
    vyi = Vqs / (ihc_m * scrcv_m);
    yi = (1.0 + sqrt(1.0 + 4.0 * alpha * vyi * (1.0 + vyi))) /
         (2.0 * alpha * (1.0 + vyi));

    //xi_w = 1.0 - yi / (1.0 + pW * yi);
    /* Niu 5/23/2015, fixes numerical discontinuity at forward/reverse transition,
    see "Epi layer model improvement of smoothness at I=0" */
    xi_w = (1.0 - yi + pW * yi) / (1.0 + pW * yi);
    gp0 = 0.5 * Ic1c2 * rcv_tm * xi_w * VtINV;

    gp0_help = 2.0 * gp0 + pW * (pW + gp0 + 1.0);
    gp02 = 0.5 * (gp0 - 1.0);
    sqr_arg = gp02 * gp02 + gp0_help;
    if (gp0 >= 1.0) begin
        p0star =  gp02 + sqrt(sqr_arg);
    end else begin
        p0star = gp0_help / (sqrt(sqr_arg) - gp02);
    end
    if (p0star < `TEN_M40) begin
        p0star = 0.0;
    end

    eVb2c2star = p0star * (p0star + 1.0) * exp(vdc_t * VtINV);
    B1 = 0.5 * scrcv_m * (Ic1c2 - ihc_m);
    B2 = scrcv_m * rcv_tm * ihc_m * Ic1c2;
    Vxi0 = B1 + sqrt(B1 * B1 + B2);

    if (swvchc == 0) begin
        Vch = vdc_ctc_t * 0.1;
    end else begin
        Vch = vdc_ctc_t * (0.1 + 2.0 * Ic1c2 / (Ic1c2 + Iqs));
    end
    Icap = ihc_m * Ic1c2 / (ihc_m + Ic1c2);
    Icap_ihc = ihc_m / (ihc_m + Ic1c2);
end else begin
    Iqs = 0.0;
    p0star = 2.0 * eVb2c2vdc / (1.0 + K0);
    eVb2c2star = eVb2c2;

    if ((abs(Vc1c2) < 1.0e-5 * Vt) ||
        (abs(Ec) < `TEN_M40 * Vt * (K0 + Kw))) begin

        pav = 0.5 * (p0star + pW);
        xi_w = pav / (pav + 1.0);
    end else begin
        xi_w = Ec / (Ec + Vb2c2 - Vb2c1);
    end

    Vxi0 = Vc1c2;
    Vch = 0.1 * vdc_ctc_t;
    Icap = Ic1c2;
    Icap_ihc = 1.0 - Icap / ihc_m;
end

// Effective EB junction capacitance bias

Vfe = vde_t * (1.0 - pow(`AJE , -1.0 / pe));
a_vde = 0.1 * vde_t;
`min_logexp(Vje, Vb2e1, Vfe, a_vde)

// RvdT, November 2008, E0BE to be re-used in EB- Zener tunnel model:
E0BE = pow(1.0 - Vje * inv_vde_t, 1.0 - pe);
Vte = vde_t / (1.0 - pe) * (1.0 - E0BE) +
      `AJE * (Vb2e1 - Vje);

// Effective CB junction capacitance bias switch
if (swvjunc == 1) begin
    // ignore epi layer voltage drop
    Vjunc = Vb2c1; 
end else if (swvjunc == 2) begin
    // 504, using resistance at xi=0
    Vjunc = Vb2c1 + Vxi0; 
end else begin
    // default
    Vjunc = Vb2c2; 
end

bjc = (`AJC - xp_t) / (1.0 - xp_t);
Vfc = vdc_ctc_t * (1.0 - pow(bjc, -1.0 / pc));
`min_logexp(Vjc, Vjunc, Vfc, Vch)
fI = pow(Icap_ihc, mc);
Vcv = vdc_ctc_t / (1.0 - pc) * (1.0 - fI * pow(1.0 - Vjc / vdc_ctc_t, 1.0 - pc)) +
      fI * bjc * (Vjunc - Vjc);
Vtc = (1.0 - xp_t) * Vcv + xp_t * Vb2c1;

// Transfer current

If0 = 4.0 * is_tm / ik_tm;
// nff effect included in eVb2e1 definition, 
// necessary to keep Qbe/If ratio at transit time, 
// so that the effective transit time is not affected 
// by addition of nff
f1 =  If0 * eVb2e1;
n0 =  f1 / (1.0 + sqrt(1.0 + f1));

// nfr effect on diffusion charge included here
eVb2c2star_nfr = pow(eVb2c2star, 1.0 / nfr_t);
f2 =  If0 * eVb2c2star_nfr;
nB =  f2 / (1.0 + sqrt(1.0 + f2));

if (deg == 0.0) begin
    q0I = 1.0 + Vte / ver_t + Vtc / vef_t;
end else begin
    termE = (Vte / ver_t + 1.0) * deg_t * VtINV;
    termC = -Vtc / vef_t * deg_t * VtINV;
    q0I = (exp(termE) - exp(termC)) /
          (exp(deg_t * VtINV) - 1.0);
end
`max_hyp0(q1I, q0I, 0.1)
qBI = q1I * (1.0 + 0.5 * (n0 + nB));

Ir = is_tm * eVb2c2star_nfr;
If = is_tm * eVb2e1;
In = (If - Ir) / qBI;

// Base and substrate current(s)

`expLin(tmpExp,Vb2e1 * VtINV / nbi)
if (xrec == 0.0) begin
    Ib1 = ibi_tm * (tmpExp - 1.0);
end else begin
    Ib1 = ibi_tm * ((1.0 - xrec) * (tmpExp - 1.0) +
          xrec * (tmpExp + eVb2c2star - 2.0) * (1.0 + Vtc / vef_t));
end

`expLin(tmpExp,Vb1e1 * VtINV / nbis)
Ib1_s = ibis_tm * (tmpExp - 1.0);
`expLin(tmpExp,Vb2e1 * VtINV / mlf)
Ib2 = ibf_tm * (tmpExp - 1.0) + GMIN * Vb2e1;
`expLin(tmpExp,Vb1e1 * VtINV / mlfs)
Ib2_s = ibfs_tm * (tmpExp - 1.0);
`expLin(tmpExp,Vb1c4 * VtINV / mlr)
Ib3 = ibr_tm * (tmpExp - 1.0) + GMIN  * Vb1c4;
`expLin(tmpExp,Vb1e1 * VtINV / nfibrel)
Ibrel = isibrel_tm * (tmpExp - 1.0);

// begin  RvdT, November 2008, MXT504.8_alpha

// Base-emitter tunneling current
// max E-field E0BE calculated in BE depletion charge model:

if ((izeb > 0.0) && (nzeb > 0.0) && (Vb2e1 < 0.0)) begin
    `expLin(e_zeb, nzeb_t * (1.0 - (pow2_2m_pe / (2.0 * E0BE))))
    // Force all derivatives at Vb2e1=0 to zero by using in dzeb a
    // modified dE0BE expression for E0BE:
    x = Vb2e1 * inv_vde_t;
    dE0BE = pow(-x, -2.0 - pe) *
            (pe * (1.0 - pe * pe - 3.0 * x * (pe - 1.0)) -
            6.0 * x * x * (pe - 1.0 + x)) *
            `one_sixth;
    `expLin(e_dzeb, Vb2e1 * pow2_2m_pe * nzeb_t / (vgzeb_t * dE0BE ))
    dzeb = - Vb2e1 - vgzeb_t * dE0BE * (1.0 - e_dzeb) / (pow2_2m_pe * nzeb_t);
    Izteb = 2.0 * izeb_tm * dzeb * E0BE * e_zeb * inv_vde_t * pow2_pe_m2;
end else begin
    dzeb  = 0.0;
    Izteb = 0.0;
end

// end  RvdT, November 2008, MXT504.8_alpha

// 505. Collector-base tunneling current
// max E-field E0CB calculated from CB capacitance using dedicated Vdc_zener and Pc_zener:

Vcbr = -1.0 * Vb2c1;

if ((izcb > 0.0) && (nzcb > 0.0) && (Vcbr > 0.0)) begin
    E0CB = pow(1.0 + Vcbr * inv_vdc_zener_t, 1.0 - Pc_zener);
    `expLin(e_zcb, nzcb_t * (1.0 - (pow2_2m_pc / (2.0 * E0CB))))
    xx = -Vcbr * inv_vdc_zener_t;
    dE0CB = pow(-xx, -2.0 - Pc_zener) *
            (Pc_zener * (1.0 - Pc_zener * Pc_zener - 3.0 * xx * (Pc_zener - 1.0)) -
            6.0 * xx * xx * (Pc_zener - 1.0 + xx)) *
            `one_sixth;
    `expLin(e_dzcb, -Vcbr * pow2_2m_pc * nzcb_t / (vgzcb_t * dE0CB ))
    dzcb = Vcbr - vgzcb_t * dE0CB * (1.0 - e_dzcb) / (pow2_2m_pc * nzcb_t);
    Iztcb = 2.0 * izcb_tm * dzcb * E0CB * e_zcb * inv_vdc_zener_t * pow2_pc_m2;
end else begin
    dzcb  = 0.0;
    Iztcb = 0.0;
end

// Iex, Isub (XIex, XIsub)
g1 = If0 * eVb1c4;
g2 = 4.0 * eVb1c4vdc;

// nBex until and including MXT 504.9:
//     nBex = g1 / (1.0 + sqrt(1.0 + g1));
// nBex since MXT 504.10.1: Ackn. Jos Peters, Geoffrey Coram
nBex = (g1 - If0) / (1.0 + sqrt(1.0 + g1));
pWex = g2 / (1.0 + sqrt(1.0 + g2));
/* Iex until and including MXT 504.9:
    Iex = (1.0 / BRI_T) * (0.5 * ik_tm * nBex - is_tm);
*/

// Iex since MXT 505.0 - hole injection from p+ extrinsic base into n-epi
Iex = 2.0 * ibx_tm * (eVb1c4 - 1.0) / (1.0 + sqrt(1.0 + 4.0 * ibx_tm / ikbx_tm * eVb1c4));

`ifdef SUBSTRATE
    // RvdT MXT504.10, new term: eVsc4
    if (exsub == 1) begin
        Isub = 2.0 * iss_tm * (eVb1c4 - eVsc4) /
               (1.0 + sqrt(1.0 + 4.0 * (iss_tm / iks_tm) * eVb1c4));
    end else begin
        Isub = 2.0 * iss_tm * (eVb1c4 - 1.0) /
               (1.0 + sqrt(1.0 + 4.0 * (iss_tm / iks_tm) * eVb1c4));
    end

    Isf =  icss_tm * (eVsc1 - 1.0);

`endif

XIex =0.0;

`ifdef SUBSTRATE
    XIsub = 0.0;
`endif

/* beginof RvdT, Q4 2012, Mextram 504.11: added exmod=2 option:     */
if ((exmod == 1) || (exmod == 2)) begin
    Iex = Iex *  Xext1;

    `ifdef SUBSTRATE
        Isub = Isub * Xext1;
    `endif

    Xg1 = If0 * eVbc3;
    // XnBex until and including MXT 504.9:
    //       XnBex = Xg1 / (1.0 + sqrt(1.0 + Xg1));
    // XnBex in MXT 504.10.1: Ackn. Jos Peters, Geoffrey Coram:
    XnBex = (Xg1 - If0) / (1.0 + sqrt(1.0 + Xg1));
    /* XIMex until and including MXT 504.9:
         XIMex = xext * (0.5 * ik_tm * XnBex - is_tm) / BRI_T;
    */
    // XIMex 505.0.0:
    XIMex = xext * 2.0 * ibx_tm * (eVbc3 - 1.0) / (1.0 + sqrt(1.0 + 4.0 * ibx_tm / ikbx_tm * eVbc3));

    `ifdef SUBSTRATE
        // RvdT MXT504.10, new term: eVsc3
        if (exsub == 1) begin
            XIMsub = xext * 2.0 * iss_tm * (eVbc3 - eVsc3) /
                     (1.0 + sqrt(1.0 + 4.0 * iss_t / iks_t * eVbc3));
        end else begin
            XIMsub = xext * 2.0 * iss_tm * (eVbc3 - 1.0) /
                     (1.0 + sqrt(1.0 + 4.0 * iss_t / iks_t * eVbc3));
        end
    `else
        XIMsub = 0.0;
    `endif

    if ((exmod == 1) && (xext > 0.0)) begin
        `ifdef SUBSTRATE
            Vex_bias = xext * (ibx_tm + iss_tm) * rcc_xx_tm;
        `else
            Vex_bias = xext * ibx_tm * rcc_xx_tm;
        `endif
        Vex  = Vt * (2.0 - ln( Vex_bias * VtINV ));
        vdif = Vbc3 - Vex;
        `max_hyp0(VBex, vdif, 0.11)
        Fex  = VBex /(Vex_bias + (XIMex + XIMsub) * rcc_xx_tm + VBex);
    end else begin
        Vex  = 0.0;
        vdif = 0.0;
        VBex = 0.0;
        Fex  = 1.0;
    end

    /* endof: RvdT, Q4, 2012, Mextram 504.11: added exmod=2 option:     */

    XIex = Fex * XIMex;

    `ifdef SUBSTRATE
        XIsub = Fex * XIMsub;
    `endif
end else begin
    Fex = 0.0;
    XnBex = 0.0;
end
// Variable base resistance
q0Q = 1.0 + Vte / ver_t + Vtc / vef_t;
`max_hyp0(q1Q, q0Q, 0.1)
qBQ = q1Q * (1.0 + 0.5 * (n0 + nB));

Rb2 = 3.0 * rbv_tm / qBQ;
Ib1b2 =  (2.0 * Vt * (eVb1b2 - 1.0) + Vb1b2) / Rb2;

// Avalanche factor and avalanche current
Gem  = 0.0;
Iavl = 0.0;

if (In > 0.0) begin
    if (swavl == 1) begin
        if (Vb2c1 < vdcavl) begin
            `expLin(expIn, -In / itoavl)
            vl = (vdcavl - Vb2c1) * expIn;
            `expLin(expMm1, -bavl_t * pow(vl, cavl))
            Gem = aavl / bavl_t * vl * expMm1;
        end
    end else if (swavl == 2) begin
        if (Vb2c1 < vdc_t) begin
            dEdx0 = 2.0 * vavl / (wavl * wavl);
            sqr_arg = (vdc_t - Vb2c1) / Icap_ihc;
            xd = sqrt(2.0 * sqr_arg / dEdx0);
            if (exavl == 0) begin
                Weff = wavl;
            end else begin
                xi_w1 = 1.0 - 0.5 * xi_w;
                Weff = wavl * xi_w1 * xi_w1;
            end
            Wd = xd * Weff / sqrt(xd * xd + Weff * Weff);
            Eav = (vdc_t - Vb2c1) / Wd;
            E0 = Eav + 0.5 * Wd * dEdx0 * Icap_ihc;

            if (exavl == 0) begin
                Em = E0;
            end else begin
                SHw = 1.0 + 2.0 * sfh * (1.0 + 2.0 * xi_w);
                Efi = (1.0 + sfh) / (1.0 + 2.0 * sfh);
                Ew = Eav - 0.5 * Wd * dEdx0 * (Efi - In / (ihc_m * SHw));
                sqr_arg = (Ew - E0) * (Ew - E0) + 0.1 * Eav * Eav * Icap / ihc_m;
                Em = 0.5 * (Ew + E0 + sqrt(sqr_arg));
            end    

            EmEav_Em = (Em - Eav) / Em;
            if (abs(EmEav_Em) > `TEN_M07) begin
                lambda = 0.5 * Wd / EmEav_Em;
                Gem = An / Bnt * Em * lambda *
                        (exp(-Bnt / Em) - exp(-Bnt / Em * (1.0 + Weff / lambda)) );
            end else begin
                Gem = An * Weff * exp(-Bnt / Em);
            end
        end
    end
    if (Gem > 0.0) begin
        Gmax = Vt / (In * (rbc_tm + Rb2)) + qBI / is_tm * ibi_tm +
                re_tm / (rbc_tm + Rb2);
        Iavl = In * Gem * Gmax / (Gem + Gmax);
    end
end
if (eVb2c2star > 0.0) begin
    Vb2c2star = Vt * ln(eVb2c2star);
end else begin
    Vb2c2star = Vb2c2;
end

`ifdef SELFHEATING
    // Power dissipation
    // RvdT 03-12-2007, modified power equation due to distribution collector resistance
    power =  In * (Vb2e1 - Vb2c2star) +
             Ic1c2 * (Vb2c2star - Vb2c1) -
             Iavl  * Vb2c2star +
             Vee1  * Vee1  / re_tm +
             Vcc3  * Vcc3  * gcc_xx_tm +
             Vc3c4 * Vc3c4 * gcc_ex_tm +
             Vc4c1 * Vc4c1 * gcc_in_tm +
             Vbb1  * Vbb1  / rbc_tm +
             Ib1b2 * Vb1b2 +
             // 504.8: Nov. 2008, RvdT, TU_Delft: Zener current contribution added:
             // Izteb > 0 for Vb2e1 < 0, hence the minus sign:
             (Ib1 + Ib2 - Izteb) * Vb2e1 - Iztcb * Vb2c2 +
             (Ib1_s + Ib2_s + Ibrel) * Vb1e1 +
    `ifdef SUBSTRATE
        (Iex + Ib3) * Vb1c4 + XIex  * Vbc3 +
        Isub * (Vb1c4 - Vsc4) +
        XIsub * (Vbc3 - Vsc3) +
        Isf * Vsc1;
    `else
        (Iex + Ib3) * Vb1c4 + XIex * Vbc3;
    `endif
`endif

// Charges
Qte = (1.0 - xcje) * cje_tm * Vte;
`min_logexp(Vje_s, Vb1e1, Vfe, a_vde)
Qte_s = xcje * cje_tm * (vde_t / (1.0 - pe) *
        (1.0 - pow(1.0 - Vje_s * inv_vde_t, 1.0 - pe)) +
        `AJE * (Vb1e1 - Vje_s));

Qtc = xcjc * cjc_tm * Vtc;
Qb0 = taub_t * ik_tm;
Qbe_qs = 0.5 * Qb0 * n0 * q1Q;
Qbc_qs = 0.5 * Qb0 * nB * q1Q;

a_vdcctc = 0.1 * vdc_ctc_t;
`min_logexp(Vjcex, Vb1c4, Vfc, a_vdcctc)
Vtexv = vdc_ctc_t / (1.0 - pc) * (1.0 - pow(1.0 - Vjcex / vdc_ctc_t, 1.0 - pc)) +
        bjc * (Vb1c4 - Vjcex);
Qtex = cjc_tm * ((1.0 - xp_t) * Vtexv + xp_t * Vb1c4) *
       (1.0 - xcjc) * (1.0 - xext);

`min_logexp(XVjcex, Vbc3, Vfc, a_vdcctc)
XVtexv = vdc_ctc_t / (1.0 - pc) * (1.0 - pow(1.0 - XVjcex / vdc_ctc_t, 1.0 - pc)) +
         bjc * (Vbc3 - XVjcex);
XQtex = cjc_tm * ((1.0 - xp_t) * XVtexv + xp_t * Vbc3) *
        (1.0 - xcjc) * xext;

`ifdef SUBSTRATE
    a_vds = 0.1 * vds_t;
    Vfs = vds_t * (1.0 - pow(`AJS , -1.0 / ps));
    `min_logexp(Vjs, Vsc1, Vfs, a_vds)
    Qts = cjs_tm * (vds_t / (1.0 - ps) *
          (1.0 - pow(1.0 - Vjs / vds_t, 1.0 - ps)) +  `AJS * (Vsc1 - Vjs));
`endif

Qe0 = taue_t * ik_tm * pow(is_tm / ik_tm, 1.0 / mtau);
`expLin(tmpExp,Vb2e1 / (mtau * Vt))

// Niu Q2, 2016, for fixing reverse VBE noise when ke=1, kc=1,
// Previous Qe_qs causes unphysically large noise correlation time constant tau_n
Qe_qs = Qe0 * tmpExp;

Qepi0 = 4.0 * tepi_t * Vt / rcv_tm;
Qepi = 0.5 * Qepi0 * xi_w * (p0star + pW + 2.0);

Qex = taur_t * 0.5 * (Qb0 * nBex + Qepi0 * pWex) / (taub_t + tepi_t);
XQex = 0.0;

if (exmod == 1) begin
    Qex = Qex * (1.0 - xext);
    Xg2 = 4.0 * eVbc3vdc;
    XpWex = Xg2 / (1.0 + sqrt(1.0 + Xg2));
    XQex = 0.5 * Fex * xext * taur_t *
           (Qb0 * XnBex + Qepi0 * XpWex) / (taub_t + tepi_t);
end

Qb1b2 = 0.0;
if (exphi == 1) begin
    dVteVje = pow(1.0 - Vje * inv_vde_t, -pe) - `AJE;
    Vb2e1Vfe = (Vb2e1 - Vfe) / a_vde;
    if (Vb2e1Vfe < 0.0) begin
        dVjeVb2e1 = 1.0 / (1.0 + exp(Vb2e1Vfe));
    end else begin
        dVjeVb2e1 = exp(- Vb2e1Vfe) / (1.0 + exp(- Vb2e1Vfe));
    end

    dVteVb2e1 = dVteVje * dVjeVb2e1 + `AJE;
    dQteVb2e1 = (1.0 - xcje) * cje_tm * dVteVb2e1;

    // nff needs to be in diffusion capacitance too
    // Note that eVb2e1 includes nff_t
    dn0Vb2e1 = If0 * eVb2e1 * VtINV / nff_t * (0.5 / sqrt(1.0 + f1));
    dQbeVb2e1 = 0.5 * Qb0 * q1Q * dn0Vb2e1;

    // Niu, Q2 2016. Modified to fix reverse VBE noise problem.
    dQeVb2e1 = Qe_qs / (mtau * Vt);

    Qb1b2 = 0.2 * Vb1b2 * (dQteVb2e1 + dQbeVb2e1 + dQeVb2e1);

    Qe = (1.0 - ke) * Qe_qs;
    Qbe_qs_eff = Qbe_qs + ke * Qe_qs;
    Qbc = xqb * Qbe_qs_eff + Qbc_qs;
    Qbe = (1.0 - xqb) * Qbe_qs_eff;
end else begin
    Qbe = Qbe_qs;
    Qbc = Qbc_qs;
    Qe = Qe_qs;
end


// Add branch current contributions

// Static currents
I(c1, c2) <+ type * Ic1c2;
I(c2, e1) <+ type * In;
I(b1, e1) <+ type * (Ib1_s + Ib2_s + Ibrel);
// begin  RvdT, 28-10-2008, MXT504.8_alpha
// contribution tunnel current added
I(b2, e1) <+ type * (Ib1 + Ib2 - Izteb);

// CB tunneling current
I(b2, c2) <+ type * (-Iztcb);

`ifdef SUBSTRATE
    I(b1, s)  <+ type * Isub;
    I(b,  s)  <+ type * XIsub;
    I(s,  c1) <+ type * Isf;
`endif
I(b1, b2) <+ type * Ib1b2;
I(b2, c2) <+ type * (-1.0 * Iavl);
I(e,  e1) <+ type * Vee1 / re_tm;
I(b,  b1) <+ type * Vbb1 / rbc_tm;

`ifdef SELFHEATING
    // Electrical equivalent for the thermal network
    I(dt) <+ V(dt) / rth_tamb_m;
    I(dt) <+ ddt(cth_m * V(dt));
    I(dt) <+ -1.0 *  power;
`endif

// Dynamic currents
I(b2, e1) <+ ddt(type * (Qte + Qbe + Qe));
I(b1, e1) <+ ddt(type * (Qte_s));
I(b2, c2) <+ ddt(type * (Qtc + Qbc + Qepi));
`ifdef SUBSTRATE
    I(s,  c1) <+ ddt(type * Qts);
`endif
I(b1, b2) <+ ddt(type * Qb1b2);
I(b,   e) <+ ddt(type * cbeo_m * Vbe);
I(b,   c) <+ ddt(type * cbco_m * Vbc);


//RvdT, Delft Univ. Tech. 03-12-2007.
//Distribution of parasitic collector resistance.
//This construct supports the case
//rcbli = 0.0 and or rcblx = 0.0 .
//It is up to the compiler to adjust the circuit topology
//and perform a node-collapse in such cases.
if (rcblx > 0.0) begin
    I(b,  c3) <+ type * XIex;
    I(c,  c3) <+ type * Vcc3 * gcc_xx_tm;
    I(b,  c3) <+ ddt(type * (XQtex + XQex));
    if (rcbli > 0.0) begin
        I(c4, c1) <+ type * Vc4c1 * gcc_in_tm;
        I(b1, c4) <+ type * (Ib3 + Iex);
        I(c3, c4) <+ type * Vc3c4 * gcc_ex_tm;
        I(b1, c4) <+ ddt(type * (Qtex + Qex));
    end else begin
        V(c4, c1) <+ 0.0;
        I(b1, c1) <+ type * (Ib3 + Iex);
        I(b1, c1) <+ ddt(type * (Qtex + Qex));
        I(c3, c1) <+ type * Vc3c4 * gcc_ex_tm;
    end
end else begin
    V(c3, c4) <+ 0.0;
    if (rcbli > 0.0) begin
        I(b,  c4) <+ type * XIex;
        I(c,  c4) <+ type * Vcc3 * gcc_xx_tm;
        I(c4, c1) <+ type * Vc4c1 * gcc_in_tm;
        I(b1, c4) <+ type * (Ib3 + Iex);
        I(b1, c4) <+ ddt(type * (Qtex + Qex));
        I(b,  c4) <+ ddt(type * (XQtex + XQex));
    end else begin
        I(b,  c1) <+ type * XIex;
        I(c,  c1) <+ type * Vcc3 * gcc_xx_tm;
        V(c4, c1) <+ 0.0;
        I(b1, c1) <+ type * (Ib3 + Iex);
        I(b1, c1) <+ ddt(type * (Qtex + Qex));
        I(b,  c1) <+ ddt(type * (XQtex + XQex));
        I(c3, c1) <+ type * Vc3c4 * gcc_ex_tm;
    end
end
