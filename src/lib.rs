//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the rust_adms project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/rust_adms/blob/master/LICENSE.
//  *  No part of rust_adms, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************
//! A frontend for Verilog AMS that returns an Ast as its endresult
//!
//! TODO showoff API
//!
#[macro_use]
extern crate pest_derive;
pub mod ast;
pub mod error;
pub mod parsing;
#[cfg(test)]
mod test;