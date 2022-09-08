use super::*;
use core::f64;
use core::fmt::Display;
use core::str::FromStr;

// Verify that `text` can be parsed as a `T` into a value that displays as `want`.
fn parse_ok<T: FromStr + Display>(text: &str, want: &str)
where
    <T as FromStr>::Err: Display,
{
    match text.parse::<T>() {
        Err(s) => panic!("\"{}\".parse() error: {}", text, s),
        Ok(x) => assert_eq!(x.to_string(), want),
    }
}

// Verify that `text` fails to parse as `T` with the error `msg`.
fn parse_err<T: FromStr + Display>(text: &str, msg: &str)
where
    <T as FromStr>::Err: Display,
{
    match text.parse::<T>() {
        Err(s) => assert_eq!(s.to_string(), msg),
        Ok(x) => panic!("Wanted Err({}), but got {}", msg, x),
    }
}

#[test]
fn format_ieee64() {
    assert_eq!(Ieee64::with_float(0.0).to_string(), "0.0");
    assert_eq!(Ieee64::with_float(-0.0).to_string(), "-0.0");
    assert_eq!(Ieee64::with_float(1.0).to_string(), "0x1.0000000000000p0");
    assert_eq!(Ieee64::with_float(1.5).to_string(), "0x1.8000000000000p0");
    assert_eq!(Ieee64::with_float(0.5).to_string(), "0x1.0000000000000p-1");
    assert_eq!(Ieee64::with_float(f64::EPSILON).to_string(), "0x1.0000000000000p-52");
    assert_eq!(Ieee64::with_float(f64::MIN).to_string(), "-0x1.fffffffffffffp1023");
    assert_eq!(Ieee64::with_float(f64::MAX).to_string(), "0x1.fffffffffffffp1023");
    // Smallest positive normal number.
    assert_eq!(Ieee64::with_float(f64::MIN_POSITIVE).to_string(), "0x1.0000000000000p-1022");
    // Subnormals.
    assert_eq!(Ieee64::with_float(f64::MIN_POSITIVE / 2.0).to_string(), "0x0.8000000000000p-1022");
    assert_eq!(
        Ieee64::with_float(f64::MIN_POSITIVE * f64::EPSILON).to_string(),
        "0x0.0000000000001p-1022"
    );
    assert_eq!(Ieee64::with_float(f64::INFINITY).to_string(), "+Inf");
    assert_eq!(Ieee64::with_float(f64::NEG_INFINITY).to_string(), "-Inf");
    assert_eq!(Ieee64::with_float(f64::NAN).to_string(), "+NaN");
    assert_eq!(Ieee64::with_float(-f64::NAN).to_string(), "-NaN");
    // Construct some qNaNs with payloads.
    assert_eq!(Ieee64(0x7ff8000000000001).to_string(), "+NaN:0x1");
    assert_eq!(Ieee64(0x7ffc000000000001).to_string(), "+NaN:0x4000000000001");
    // Signaling NaNs.
    assert_eq!(Ieee64(0x7ff0000000000001).to_string(), "+sNaN:0x1");
    assert_eq!(Ieee64(0x7ff4000000000001).to_string(), "+sNaN:0x4000000000001");
}

#[test]
fn parse_ieee64() {
    parse_ok::<Ieee64>("0.0", "0.0");
    parse_ok::<Ieee64>("-0.0", "-0.0");
    parse_ok::<Ieee64>("0x0", "0.0");
    parse_ok::<Ieee64>("0x0.0", "0.0");
    parse_ok::<Ieee64>("0x.0", "0.0");
    parse_ok::<Ieee64>("0x0.", "0.0");
    parse_ok::<Ieee64>("0x1", "0x1.0000000000000p0");
    parse_ok::<Ieee64>("-0x1", "-0x1.0000000000000p0");
    parse_ok::<Ieee64>("0x10", "0x1.0000000000000p4");
    parse_ok::<Ieee64>("0x10.0", "0x1.0000000000000p4");
    parse_err::<Ieee64>("0.", "Float must be hexadecimal");
    parse_err::<Ieee64>(".0", "Float must be hexadecimal");
    parse_err::<Ieee64>("0", "Float must be hexadecimal");
    parse_err::<Ieee64>("-0", "Float must be hexadecimal");
    parse_err::<Ieee64>(".", "Float must be hexadecimal");
    parse_err::<Ieee64>("", "Float must be hexadecimal");
    parse_err::<Ieee64>("-", "Float must be hexadecimal");
    parse_err::<Ieee64>("0x", "No digits");
    parse_err::<Ieee64>("0x..", "Multiple radix points");

    // Check significant bits.
    parse_ok::<Ieee64>("0x0.fffffffffffff8", "0x1.fffffffffffffp-1");
    parse_ok::<Ieee64>("0x1.fffffffffffff", "0x1.fffffffffffffp0");
    parse_ok::<Ieee64>("0x3.ffffffffffffe", "0x1.fffffffffffffp1");
    parse_ok::<Ieee64>("0x7.ffffffffffffc", "0x1.fffffffffffffp2");
    parse_ok::<Ieee64>("0xf.ffffffffffff8", "0x1.fffffffffffffp3");
    parse_err::<Ieee64>("0x3.fffffffffffff", "Too many significant bits");
    parse_err::<Ieee64>("0x001.fffffe00000000", "Too many digits");

    // Exponents.
    parse_ok::<Ieee64>("0x1p3", "0x1.0000000000000p3");
    parse_ok::<Ieee64>("0x1p-3", "0x1.0000000000000p-3");
    parse_ok::<Ieee64>("0x1.0p3", "0x1.0000000000000p3");
    parse_ok::<Ieee64>("0x2.0p3", "0x1.0000000000000p4");
    parse_ok::<Ieee64>("0x1.0p1023", "0x1.0000000000000p1023");
    parse_ok::<Ieee64>("0x1.0p-1022", "0x1.0000000000000p-1022");
    parse_ok::<Ieee64>("0x0.1p-1018", "0x1.0000000000000p-1022");
    parse_err::<Ieee64>("0x2.0p1023", "Magnitude too large");

    // Subnormals.
    parse_ok::<Ieee64>("0x1.0p-1023", "0x0.8000000000000p-1022");
    parse_ok::<Ieee64>("0x1.0p-1074", "0x0.0000000000001p-1022");
    parse_ok::<Ieee64>("0x0.0000000000001p-1022", "0x0.0000000000001p-1022");
    parse_err::<Ieee64>("0x0.10000000000008p-1022", "Subnormal underflow");
    parse_err::<Ieee64>("0x1.8p-1074", "Subnormal underflow");
    parse_err::<Ieee64>("0x1.0p-1075", "Magnitude too small");

    // NaNs and Infs.
    parse_ok::<Ieee64>("Inf", "+Inf");
    parse_ok::<Ieee64>("-Inf", "-Inf");
    parse_ok::<Ieee64>("NaN", "+NaN");
    parse_ok::<Ieee64>("-NaN", "-NaN");
    parse_ok::<Ieee64>("NaN:0x0", "+NaN");
    parse_err::<Ieee64>("NaN:", "Float must be hexadecimal");
    parse_err::<Ieee64>("NaN:0", "Float must be hexadecimal");
    parse_err::<Ieee64>("NaN:0x", "Invalid NaN payload");
    parse_ok::<Ieee64>("NaN:0x000001", "+NaN:0x1");
    parse_ok::<Ieee64>("NaN:0x4000000000001", "+NaN:0x4000000000001");
    parse_err::<Ieee64>("NaN:0x8000000000001", "Invalid NaN payload");
    parse_ok::<Ieee64>("sNaN:0x1", "+sNaN:0x1");
    parse_err::<Ieee64>("sNaN:0x0", "Invalid sNaN payload");
    parse_ok::<Ieee64>("sNaN:0x4000000000001", "+sNaN:0x4000000000001");
    parse_err::<Ieee64>("sNaN:0x8000000000001", "Invalid sNaN payload");
}
