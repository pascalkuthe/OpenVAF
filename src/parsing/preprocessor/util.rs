//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the rust_adms project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/jamescoding/rust_adms/blob/master/LICENSE.
//  *  No part of rust_adms, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use pest::iterators::Pair;

use crate::parsing::preprocessor::Rule;

//This might get merged straight into mod but at the moment I will leave this here in case

//Shorthands and sensible defaults for often used types

pub type PreprocessorResult<T = ()> = crate::parsing::util::Result<Rule, T>;
pub(crate) type ParseTreeNode<'lifetime> = Pair<'lifetime, Rule>;

