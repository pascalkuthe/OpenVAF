/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

/// This function assumes that source is a valid string matched by the lexer and only strips "" and resolves any escaped characters
pub(super) fn parse_string(source: &str) -> String {
    (&source[1..source.len() - 1])
        .replace(r"\n", "\n")
        .replace(r"\\", "\\")
        .replace(r"\t", "\t")
        .replace(r#"\""#, "\"")
}
pub enum RealLiteralType {
    DotScaleChar,
    DotExp,
    ScaleChar,
    Exp,
    Dot,
}
pub fn parse_real_value(slice: &str, literal_type: RealLiteralType) -> f64 {
    let source = slice.replace("_", "");
    match literal_type {
        RealLiteralType::Dot => source.parse::<f64>().unwrap(),
        RealLiteralType::DotExp | RealLiteralType::Exp => {
            let mut base_and_exp = source.split(|c: char| c.eq_ignore_ascii_case(&'e'));
            let base = base_and_exp.next().unwrap().parse::<f64>().unwrap();
            let exp = base_and_exp.next().unwrap().parse::<i32>().unwrap();
            base * (10_f64).powi(exp)
        }
        RealLiteralType::DotScaleChar | RealLiteralType::ScaleChar => {
            let scale_factor = match source.chars().last().unwrap() {
                'T' => 12,
                'G' => 9,
                'M' => 6,
                'K' | 'k' => 3,
                'm' => -3,
                'u' => -6,
                'p' => -9,
                'f' => -12,
                'a' => -15,
                _ => unreachable_unchecked!("Lexer should not allow this"),
            };
            let base = source[0..source.len() - 1].parse::<f64>().unwrap();
            base * (10_f64).powi(scale_factor)
        }
    }
}
pub fn parse_unsigned_int_value(slice: &str) -> u32 {
    slice.replace("_", "").parse::<u32>().unwrap()
}
