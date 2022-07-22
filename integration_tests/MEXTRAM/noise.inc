// Copyright (c) 2000-2007, NXP Semiconductor
// Copyright (c) 2007-2014, Delft University of Technology
// Copyright (c) 2015-2017, Auburn University
// All rights reserved, see IP_NOTICE_DISCLAIMER_LICENSE for further information.

// Noise sources
// Thermal noise
common = 4.0 * `KB * Tk;
power_rec = common / re_tm;  // Emitter resistance
power_rbc = common / rbc_tm; // Constant Base resistance
power_rcc_xx = common * gcc_xx_tm; // Collector resistance
power_rcc_ex = common * gcc_ex_tm; // Collector resistance
power_rcc_in = common * gcc_in_tm; // Collector resistance
power_rbv = common / Rb2 * (4.0 * eVb1b2 + 5.0) * `one_third; // Variable base resistance

// Main current shot noise
In_N = (If + Ir) / qBI;
powerCCS = 2.0 * `QQ * abs(In_N);

// Weak-avalanche current shot noise
if (kavl > 0) begin
    Gem_N = abs(Iavl / In_N);
end else begin
    Gem_N = 0.0;
end

powerIIS = 2.0 * `QQ * Iavl * (Gem_N + 1.0);

// Transit time for noise
if (In_N > 0.0) begin
    Taub_N = (Qbe + Qbc) / In_N;
end else begin
    Taub_N = taub_t * q1Q * qBI;
end

// RF correlation noise model switch
if (kc == 1) begin
    // use charge partition for noise transit time
    taun = xqb * Taub_N;
end else if (kc == 2) begin
    // use fraction of transit time for noise transit time
    taun = ftaun * Taub_N;
end else begin
    // kc == 0, no correlation noise
    taun = 0.0;
end

// Forward base current shot noise
powerFBCS = 2.0 * `QQ * (abs(Ib1) + abs(Ib2) + abs(Izteb));

// Ideal forward base current 1/f noise
powerFBC1f = kf_m * pow(abs(Ib1) + abs(Ib1_s), af);

// Non-ideal forward base current 1/f noise
powerNFBC1f = kfn_m * pow(abs(Ib2) + abs(Ib2_s) + abs(Ibrel), afn);

// Emitter-base sidewall current shot
powerEBSCS = 2.0 * `QQ * (abs(Ib1_s) + abs(Ib2_s) + abs(Ibrel));

// Reverse base current shot noise and 1/f noise
powerRBCS = 2.0 * `QQ * abs(Ib3);
powerRBC1f = kf_m * pow(abs(Ib3), af);
powerZTCB = 2.0 * `QQ * abs(Iztcb);

// Extrinsic current shot noise and 1/f noise
powerExCS = 2.0 * `QQ * abs(Iex);
powerExC1f = kf_m * (1.0 - (exmod * xext)) *
                    pow((abs(Iex) / (1.0 - (exmod * xext))), af);
powerExCSMOD = 2.0 * `QQ * abs(XIex) * exmod;
if (xext == 0.0) begin
    powerExC1fMOD = 0.0;
end else begin
    powerExC1fMOD = kf_m * exmod * xext * pow((abs(XIex) / xext), af);
end

`ifdef SUBSTRATE
    // Substrate current shot noise (between nodes B1 and S, resp. B and S)
    powerSubsCS_B1S = 2.0 * `QQ * abs(Isub);
    powerSubsCS_BS  = 2.0 * `QQ * abs(XIsub);
`endif

// Reference un-correlated current shot noise sources
I(noi) <+ white_noise(powerCCS, "in");
I(noi) <+ V(noi);

// Implementing correlated noise sources
I(b2, e1) <+ taun * ddt(V(noi));
I(c2, b2) <+ Gem_N * V(noi);
I(c2, e1) <+ V(noi);

// Implementing un-correlated noise sources
I(c2, b2) <+ white_noise(powerIIS, "iavl");
I(b2, e1) <+ white_noise(powerFBCS, "ib2e1");

// Add noise sources  
I(e, e1)    <+ white_noise(power_rec, "re");
I(b, b1)    <+ white_noise(power_rbc, "rbc");
I(b1, b2)   <+ white_noise(power_rbv, "rbv");
I(b2, e1)   <+ flicker_noise(powerFBC1f, 1, "ib2e1_f");
I(b1, e1)   <+ flicker_noise(powerNFBC1f, 1, "ib1e1_f");
I(b1, e1)   <+ white_noise(powerEBSCS, "ib1e1");
I(b1, c4)   <+ white_noise(powerRBCS, "ib3");
I(b1, c4)   <+ flicker_noise(powerRBC1f, 1, "ib3_f");
I(c2, b2)   <+ white_noise(powerZTCB, "iztcb");
I(b1, c4)   <+ white_noise(powerExCS, "iex");
I(b1, c4)   <+ flicker_noise(powerExC1f, 1, "iex_f");
I(b, c3)    <+ white_noise(powerExCSMOD, "xiex");
I(b, c3)    <+ flicker_noise(powerExC1fMOD, 1, "xiex_f");

`ifdef SUBSTRATE
    I(b1, s)   <+ white_noise(powerSubsCS_B1S, "isub");
    I(b, s)    <+ white_noise(powerSubsCS_BS, "xisub");
`endif

if (rcblx > 0.0) begin
    if (rcbli > 0.0) begin /* all branches exist */
        I(c,  c3)  <+ white_noise(power_rcc_xx, "rcc");
        I(c3, c4)  <+ white_noise(power_rcc_ex, "rcblx");
        I(c4, c1)  <+ white_noise(power_rcc_in, "rcbli");
    end else begin  /* only Rcblx exists */
        I(c,  c3)  <+ white_noise(power_rcc_xx, "rcc");
        I(c3, c1)  <+ white_noise(power_rcc_ex, "rcblx");
    end
end else begin
    if (rcbli > 0.0) begin   /* only Rcbli exists */
        I(c,  c4)  <+ white_noise(power_rcc_xx, "rcc");
        I(c4, c1)  <+ white_noise(power_rcc_in, "rcbli");
    end else begin /* neither Rcblx nor Rcbli exists */
        I(c,  c1)  <+ white_noise(power_rcc_xx, "rcc");
    end
end

