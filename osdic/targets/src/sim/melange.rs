/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::sim::{LimFunctionInfo, Simulator, SimulatorResult};
use openvaf_data_structures::index_vec::index_box;
use osdi_types::Type;

pub fn simulator() -> SimulatorResult {
    let res = Simulator {
        name: "melange".to_owned(),
        is_builtin: true,
        min_version: "0.1.0".to_string(),
        lim_functions: index_box![
            LimFunctionInfo {
                name: "pnjlim".to_owned(),
                args: vec![
                    ("vt".to_owned(), Type::REAL),
                    ("vte".to_owned(), Type::REAL)
                ]
                .into_boxed_slice()
            },
            LimFunctionInfo {
                name: "limvds".to_owned(),
                args: Vec::new().into_boxed_slice(),
            },
            LimFunctionInfo {
                name: "limvds".to_owned(),
                args: Vec::new().into_boxed_slice(),
            },
            LimFunctionInfo {
                name: "fetlim".to_owned(),
                args: vec![("vto".to_owned(), Type::REAL)].into_boxed_slice()
            },
        ],
        sim_paras_real: Vec::new().into_boxed_slice(),
        sim_paras_str: Vec::new().into_boxed_slice(),
    };

    Ok(res)
}
