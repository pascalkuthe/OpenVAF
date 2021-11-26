use crate::Vfs;

const CONSTANTS_SRC: &str = r#"
// Copyright(c) 2009-2014 Accellera Systems Initiative Inc.
// 1370 Trancas Street #163, Napa, CA 94558, USA.
//
// The material in constants.vams is an essential part of the Accellera Systems
// Initiative ("Accellera") Verilog-AMS Language Standard. Verbatim copies of
// the material in this Annex may be used and distributed without restriction.
// All other uses require permission from Accellera IP Committee
// (ipr-chair@lists.accellera.org).
// All other rights reserved
//
// Version 2.4.0
//

// Mathematical and physical constants
`ifdef CONSTANTS_VAMS
`else
`define CONSTANTS_VAMS 1

// M_ is a mathematical constant
`define    M_E                2.7182818284590452354
`define    M_LOG2E            1.4426950408889634074
`define    M_LOG10E           0.43429448190325182765
`define    M_LN2              0.69314718055994530942
`define    M_LN10             2.30258509299404568402
`define    M_PI               3.14159265358979323846
`define    M_TWO_PI           6.28318530717958647693
`define    M_PI_2             1.57079632679489661923
`define    M_PI_4             0.78539816339744830962
`define    M_1_PI             0.31830988618379067154
`define    M_2_PI             0.63661977236758134308
`define    M_2_SQRTPI         1.12837916709551257390
`define    M_SQRT2            1.41421356237309504880
`define    M_SQRT1_2          0.70710678118654752440

// The following constants have been taken from http://physics.nist.gov
// P_ is a physical constant
// charge of electron in Coulombs
`define    P_Q_SPICE          1.60219e-19
`define    P_Q_OLD            1.6021918e-19
`define    P_Q_NIST1998       1.602176462e-19
`define    P_Q_NIST2010       1.602176565e-19
// speed of light in vacuum in meters/second
`define    P_C                2.99792458e8
// Boltzmann's constant in Joules/Kelvin
`define    P_K_SPICE          1.38062e-23
`define    P_K_OLD            1.3806226e-23
`define    P_K_NIST1998       1.3806503e-23
`define    P_K_NIST2010       1.3806488e-23
// Planck's constant in Joules*second
`define    P_H_SPICE          6.62620e-34
`define    P_H_OLD            6.6260755e-34
`define    P_H_NIST1998       6.62606876e-34
`define    P_H_NIST2010       6.62606957e-34
// permittivity of vacuum in Farads/meter
`define    P_EPS0_SPICE       8.854214871e-12
`define    P_EPS0_OLD         8.85418792394420013968e-12
`define    P_EPS0_NIST1998    8.854187817e-12
`define    P_EPS0_NIST2010    8.854187817e-12
// permeability of vacuum in Henrys/meter
`define    P_U0               (4.0e-7 * `M_PI)
// zero Celsius in Kelvin
`define    P_CELSIUS0         273.15

`ifdef PHYSICAL_CONSTANTS_SPICE
// from UC Berkeley SPICE 3F5
`define    P_Q                `P_Q_SPICE
`define    P_K                `P_K_SPICE
`define    P_H                `P_H_SPICE
`define    P_EPS0             `P_EPS0_SPICE

`else
`ifdef PHYSICAL_CONSTANTS_OLD
// from Verilog-A LRM 1.0 and Verilog-AMS LRM 2.0
`define    P_Q                `P_Q_OLD
`define    P_K                `P_K_OLD
`define    P_H                `P_H_OLD
`define    P_EPS0             `P_EPS0_OLD

`else
`ifdef PHYSICAL_CONSTANTS_NIST2010
`define    P_Q                `P_Q_NIST2010
`define    P_K                `P_K_NIST2010
`define    P_H                `P_H_NIST2010
`define    P_EPS0             `P_EPS0_NIST2010

`else
// use NIST1998 values as in LRM 2.2 - 2.3 for backwards-compatibility
`define    P_Q                `P_Q_NIST1998
`define    P_K                `P_K_NIST1998
`define    P_H                `P_H_NIST1998
`define    P_EPS0             `P_EPS0_NIST1998
`endif
`endif
`endif

`endif

"#;

const DISCIPLINCES_SRC: &str = r#"
// Copyright(c) 2009-2014 Accellera Systems Initiative Inc.
// 1370 Trancas Street #163, Napa, CA 94558, USA.
//
// The material in disciplines.vams is an essential part of the Accellera Systems
// Initiative ("Accellera") Verilog-AMS Language Standard. Verbatim copies of
// the material in this Annex may be used and distributed without restriction.
// All other uses require permission from Accellera IP Committee
// (ipr-chair@lists.accellera.org).
// All other rights reserved.
//
// Version 2.4.0

`ifdef DISCIPLINES_VAMS
`else
`define DISCIPLINES_VAMS 1

//
// Natures and Disciplines
//

discipline \logic ;
  domain discrete;
enddiscipline

discipline ddiscrete;
  domain discrete;
enddiscipline

/*
 *   Default absolute tolerances may be overridden by setting the
 *   appropriate _ABSTOL prior to including this file
 */

// Electrical

// Current in amperes
nature Current;
  units        = "A";
  access       = I;
  idt_nature   = Charge;
`ifdef CURRENT_ABSTOL
  abstol       = `CURRENT_ABSTOL;
`else
  abstol       = 1e-12;
`endif
endnature

// Charge in coulombs
nature Charge;
  units      = "coul";
  access     = Q;
  ddt_nature = Current;
`ifdef CHARGE_ABSTOL
  abstol     = `CHARGE_ABSTOL;
`else
  abstol     = 1e-14;
`endif
endnature

// Potential in volts
nature Voltage;
  units      = "V";
  access     = V;
  idt_nature = Flux;
`ifdef VOLTAGE_ABSTOL
  abstol     = `VOLTAGE_ABSTOL;
`else
  abstol     = 1e-6;
`endif
endnature

// Flux in Webers
nature Flux;
  units      = "Wb";
  access     = Phi;
  ddt_nature = Voltage;
`ifdef FLUX_ABSTOL
  abstol     = `FLUX_ABSTOL;
`else
  abstol     = 1e-9;
`endif
endnature

// Conservative discipline
discipline electrical;
  potential    Voltage;
  flow         Current;
enddiscipline

// Signal flow disciplines
discipline voltage;
  potential    Voltage;
enddiscipline

discipline current;
  flow    Current;
enddiscipline

// Magnetic

// Magnetomotive force in Ampere-Turns.
nature Magneto_Motive_Force;
  units      = "A*turn";
  access     = MMF;
`ifdef MAGNETO_MOTIVE_FORCE_ABSTOL
  abstol     = `MAGNETO_MOTIVE_FORCE_ABSTOL;
`else
  abstol     = 1e-12;
`endif
endnature

// Conservative discipline
discipline magnetic;
  potential    Magneto_Motive_Force;
  flow         Flux;
enddiscipline

// Thermal

// Temperature in Kelvin
nature Temperature;
  units      = "K";
  access     = Temp;
`ifdef TEMPERATURE_ABSTOL
  abstol     = `TEMPERATURE_ABSTOL;
`else
  abstol     = 1e-4;
`endif
endnature

// Power in Watts
nature Power;
  units      = "W";
  access     = Pwr;
`ifdef POWER_ABSTOL
  abstol     = `POWER_ABSTOL;
`else
  abstol     = 1e-9;
`endif
endnature

// Conservative discipline
discipline thermal;
  potential    Temperature;
  flow         Power;
enddiscipline

// Kinematic

// Position in meters
nature Position;
  units      = "m";
  access     = Pos;
  ddt_nature = Velocity;
`ifdef POSITION_ABSTOL
  abstol     = `POSITION_ABSTOL;
`else
  abstol     = 1e-6;
`endif
endnature

// Velocity in meters per second
nature Velocity;
  units      = "m/s";
  access     = Vel;
  ddt_nature = Acceleration;
  idt_nature = Position;
`ifdef VELOCITY_ABSTOL
  abstol     = `VELOCITY_ABSTOL;
`else
  abstol     = 1e-6;
`endif
endnature

// Acceleration in meters per second squared
nature Acceleration;
  units      = "m/s^2";
  access     = Acc;
  ddt_nature = Impulse;
  idt_nature = Velocity;
`ifdef ACCELERATION_ABSTOL
  abstol     = `ACCELERATION_ABSTOL;
`else
  abstol     = 1e-6;
`endif
endnature

// Impulse in meters per second cubed
nature Impulse;
  units      = "m/s^3";
  access     = Imp;
  idt_nature = Acceleration;
`ifdef IMPULSE_ABSTOL
  abstol     = `IMPULSE_ABSTOL;
`else
  abstol     = 1e-6;
`endif
endnature

// Force in Newtons
nature Force;
  units      = "N";
  access     = F;
`ifdef FORCE_ABSTOL
  abstol     = `FORCE_ABSTOL;
`else
  abstol     = 1e-6;
`endif
endnature

// Conservative disciplines

discipline kinematic;
  potential    Position;
  flow         Force;
enddiscipline

discipline kinematic_v;
  potential    Velocity;
  flow         Force;
enddiscipline

// Rotational

// Angle in radians
nature Angle;
  units      = "rads";
  access     = Theta;
  ddt_nature = Angular_Velocity;
`ifdef ANGLE_ABSTOL
  abstol     = `ANGLE_ABSTOL;
`else
  abstol     = 1e-6;
`endif
endnature

// Angular Velocity in radians per second
nature Angular_Velocity;
  units      = "rads/s";
  access     = Omega;
  ddt_nature = Angular_Acceleration;
  idt_nature = Angle;
`ifdef ANGULAR_VELOCITY_ABSTOL
  abstol     = `ANGULAR_VELOCITY_ABSTOL;
`else
  abstol     = 1e-6;
`endif
endnature

// Angular acceleration in radians per second squared
nature Angular_Acceleration;
  units      = "rads/s^2";
  access     = Alpha;
  idt_nature = Angular_Velocity;
`ifdef ANGULAR_ACCELERATION_ABSTOL
  abstol     = `ANGULAR_ACCELERATION_ABSTOL;
`else
  abstol     = 1e-6;
`endif
endnature

// Torque in Newtons
nature Angular_Force;
  units      = "N*m";
  access     = Tau;
`ifdef ANGULAR_FORCE_ABSTOL
  abstol     = `ANGULAR_FORCE_ABSTOL;
`else
  abstol     = 1e-6;
`endif
endnature

// Conservative disciplines
discipline rotational;
  potential    Angle;
  flow         Angular_Force;
enddiscipline

discipline rotational_omega;
  potential    Angular_Velocity;
  flow         Angular_Force;
enddiscipline

`endif
"#;

pub const CONSTANTS_PATHS: [&str; 3] = ["constants.vams", "constants.va", "constants.h"];
pub const DISCIPLINES_PATHS: [&str; 4] =
    ["disciplines.vams", "disciplines.va", "disciplines.h", "discipline.h"];

impl Vfs {
    pub fn insert_std_lib(&mut self) {
        for name in CONSTANTS_PATHS {
            self.add_virt_file(&format!("/std/{}", name), CONSTANTS_SRC);
        }
        for name in DISCIPLINES_PATHS {
            self.add_virt_file(&format!("/std/{}", name), DISCIPLINCES_SRC);
        }
    }
}
