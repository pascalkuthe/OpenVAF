use std::cmp::Ordering;
use std::fmt::{self, Display, Formatter};
use std::str::FromStr;

#[cfg(test)]
mod tests;

/// An IEEE binary64 immediate floating point value, represented as a u64
/// containing the bit pattern.
///
/// All bit patterns are allowed.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct Ieee64(u64);

/// Format a floating point number in a way that is reasonably human-readable, and that can be
/// converted back to binary without any rounding issues. The hexadecimal formatting of normal and
/// subnormal numbers is compatible with C99 and the `printf "%a"` format specifier. The NaN and Inf
/// formats are not supported by C99.
///
/// The encoding parameters are:
///
/// w - exponent field width in bits
/// t - trailing significand field width in bits
///
fn format_float(bits: u64, w: u8, t: u8, f: &mut Formatter) -> fmt::Result {
    debug_assert!(w > 0 && w <= 16, "Invalid exponent range");
    debug_assert!(1 + w + t <= 64, "Too large IEEE format for u64");
    debug_assert!((t + w + 1).is_power_of_two(), "Unexpected IEEE format size");

    let max_e_bits = (1u64 << w) - 1;
    let t_bits = bits & ((1u64 << t) - 1); // Trailing significand.
    let e_bits = (bits >> t) & max_e_bits; // Biased exponent.
    let sign_bit = (bits >> (w + t)) & 1;

    let bias: i32 = (1 << (w - 1)) - 1;
    let e = e_bits as i32 - bias; // Unbiased exponent.
    let emin = 1 - bias; // Minimum exponent.

    // How many hexadecimal digits are needed for the trailing significand?
    let digits = (t + 3) / 4;
    // Trailing significand left-aligned in `digits` hexadecimal digits.
    let left_t_bits = t_bits << (4 * digits - t);

    // All formats share the leading sign.
    if sign_bit != 0 {
        write!(f, "-")?;
    }

    if e_bits == 0 {
        if t_bits == 0 {
            // Zero.
            write!(f, "0.0")
        } else {
            // Subnormal.
            write!(f, "0x0.{0:01$x}p{2}", left_t_bits, usize::from(digits), emin)
        }
    } else if e_bits == max_e_bits {
        // Always print a `+` or `-` sign for these special values.
        // This makes them easier to parse as they can't be confused as identifiers.
        if sign_bit == 0 {
            write!(f, "+")?;
        }
        if t_bits == 0 {
            // Infinity.
            write!(f, "Inf")
        } else {
            // NaN.
            let payload = t_bits & ((1 << (t - 1)) - 1);
            if t_bits & (1 << (t - 1)) != 0 {
                // Quiet NaN.
                if payload != 0 {
                    write!(f, "NaN:0x{:x}", payload)
                } else {
                    write!(f, "NaN")
                }
            } else {
                // Signaling NaN.
                write!(f, "sNaN:0x{:x}", payload)
            }
        }
    } else {
        // Normal number.
        write!(f, "0x1.{0:01$x}p{2}", left_t_bits, usize::from(digits), e)
    }
}

/// Parse a float using the same format as `format_float` above.
///
/// The encoding parameters are:
///
/// w - exponent field width in bits
/// t - trailing significand field width in bits
///
fn parse_float(s: &str, w: u8, t: u8) -> Result<u64, &'static str> {
    debug_assert!(w > 0 && w <= 16, "Invalid exponent range");
    debug_assert!(1 + w + t <= 64, "Too large IEEE format for u64");
    debug_assert!((t + w + 1).is_power_of_two(), "Unexpected IEEE format size");

    let (sign_bit, s2) = if let Some(rem) = s.strip_prefix('-') {
        (1u64 << (t + w), rem)
    } else if let Some(rem) = s.strip_prefix('+') {
        (0, rem)
    } else {
        (0, s)
    };

    if !s2.starts_with("0x") {
        let max_e_bits = ((1u64 << w) - 1) << t;
        let quiet_bit = 1u64 << (t - 1);

        // The only decimal encoding allowed is 0.
        if s2 == "0.0" {
            return Ok(sign_bit);
        }

        if s2 == "Inf" {
            // +/- infinity: e = max, t = 0.
            return Ok(sign_bit | max_e_bits);
        }
        if s2 == "NaN" {
            // Canonical quiet NaN: e = max, t = quiet.
            return Ok(sign_bit | max_e_bits | quiet_bit);
        }
        if let Some(rem) = s2.strip_prefix("NaN:0x") {
            // Quiet NaN with payload.
            return match u64::from_str_radix(rem, 16) {
                Ok(payload) if payload < quiet_bit => {
                    Ok(sign_bit | max_e_bits | quiet_bit | payload)
                }
                _ => Err("Invalid NaN payload"),
            };
        }
        if let Some(rem) = s2.strip_prefix("sNaN:0x") {
            // Signaling NaN with payload.
            return match u64::from_str_radix(rem, 16) {
                Ok(payload) if 0 < payload && payload < quiet_bit => {
                    Ok(sign_bit | max_e_bits | payload)
                }
                _ => Err("Invalid sNaN payload"),
            };
        }

        return Err("Float must be hexadecimal");
    }
    let s3 = &s2[2..];

    let mut digits = 0u8;
    let mut digits_before_period: Option<u8> = None;
    let mut significand = 0u64;
    let mut exponent = 0i32;

    for (idx, ch) in s3.char_indices() {
        match ch {
            '.' => {
                // This is the radix point. There can only be one.
                if digits_before_period.is_some() {
                    return Err("Multiple radix points");
                } else {
                    digits_before_period = Some(digits);
                }
            }
            'p' => {
                // The following exponent is a decimal number.
                let exp_str = &s3[1 + idx..];
                match exp_str.parse::<i16>() {
                    Ok(e) => {
                        exponent = i32::from(e);
                        break;
                    }
                    Err(_) => return Err("Bad exponent"),
                }
            }
            _ => match ch.to_digit(16) {
                Some(digit) => {
                    digits += 1;
                    if digits > 16 {
                        return Err("Too many digits");
                    }
                    significand = (significand << 4) | u64::from(digit);
                }
                None => return Err("Invalid character"),
            },
        }
    }

    if digits == 0 {
        return Err("No digits");
    }

    if significand == 0 {
        // This is +/- 0.0.
        return Ok(sign_bit);
    }

    // Number of bits appearing after the radix point.
    match digits_before_period {
        None => {} // No radix point present.
        Some(d) => exponent -= 4 * i32::from(digits - d),
    };

    // Normalize the significand and exponent.
    let significant_bits = (64 - significand.leading_zeros()) as u8;
    if significant_bits > t + 1 {
        let adjust = significant_bits - (t + 1);
        if significand & ((1u64 << adjust) - 1) != 0 {
            return Err("Too many significant bits");
        }
        // Adjust significand down.
        significand >>= adjust;
        exponent += i32::from(adjust);
    } else {
        let adjust = t + 1 - significant_bits;
        significand <<= adjust;
        exponent -= i32::from(adjust);
    }
    debug_assert_eq!(significand >> t, 1);

    // Trailing significand excludes the high bit.
    let t_bits = significand & ((1 << t) - 1);

    let max_exp = (1i32 << w) - 2;
    let bias: i32 = (1 << (w - 1)) - 1;
    exponent += bias + i32::from(t);

    if exponent > max_exp {
        Err("Magnitude too large")
    } else if exponent > 0 {
        // This is a normal number.
        let e_bits = (exponent as u64) << t;
        Ok(sign_bit | e_bits | t_bits)
    } else if 1 - exponent <= i32::from(t) {
        // This is a subnormal number: e = 0, t = significand bits.
        // Renormalize significand for exponent = 1.
        let adjust = 1 - exponent;
        if significand & ((1u64 << adjust) - 1) != 0 {
            Err("Subnormal underflow")
        } else {
            significand >>= adjust;
            Ok(sign_bit | significand)
        }
    } else {
        Err("Magnitude too small")
    }
}

impl PartialOrd for Ieee64 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        f64::from_bits(self.0).partial_cmp(&f64::from_bits(other.0))
    }
}

impl Display for Ieee64 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let bits: u64 = self.0;
        format_float(bits, 11, 52, f)
    }
}

impl FromStr for Ieee64 {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, &'static str> {
        match parse_float(s, 11, 52) {
            Ok(b) => Ok(Self(b)),
            Err(s) => Err(s),
        }
    }
}

impl From<f64> for Ieee64 {
    fn from(x: f64) -> Self {
        Self::with_float(x)
    }
}

impl From<u64> for Ieee64 {
    fn from(x: u64) -> Self {
        Self::with_float(f64::from_bits(x))
    }
}

impl From<Ieee64> for f64 {
    fn from(x: Ieee64) -> Self {
        f64::from_bits(x.0)
    }
}

impl Ieee64 {
    /// Create a new `Ieee64` containing the bits of `x`.
    pub fn with_bits(x: u64) -> Self {
        Self(x)
    }

    /// Create a new `Ieee64` representing the number `x`.
    pub fn with_float(x: f64) -> Self {
        Self(x.to_bits())
    }

    /// Get the bitwise representation.
    pub fn bits(self) -> u64 {
        self.0
    }

    /// Check if the value is a NaN. For [Ieee64], this means checking that the 11 exponent bits are
    /// all set.
    pub fn is_nan(self) -> bool {
        f64::from_bits(self.0).is_nan()
    }

    pub fn is_finite(self) -> bool {
        f64::from_bits(self.0).is_finite()
    }

    /// Get the bitwise representation.
    pub fn is_zero(self) -> bool {
        // IEEE number is zero if mantissa is zero
        (self.0 & 0x000FFFFFFFFFFFFF) == 0
    }
}
