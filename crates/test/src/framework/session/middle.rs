/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::framework::TestSession;
use session::lints::SyntaxCtx;
use std::path::Path;

struct TestLowering;

impl HirLowering for TestLowering {
    fn handle_attribute(_: &mut HirFold<Self>, _: &Attribute, _: AttributeCtx, _: SyntaxCtx) {
        // attribute can be ignored
    }

    fn handle_statement_attribute<'a, 'h>(
        _: &mut LocalCtx<'a, 'h, Self>,
        _: &Attribute,
        _: StatementId,
        _: SyntaxCtx,
    ) {
        // attribute can be ignored
    }
}

#[derive(Clone, Debug, Display, From, TryInto, CfgFunctions)]
#[cfg_inputs(DefaultInputs)]
pub enum TestFunctions {
    Noise(NoiseCall),
    TimeDerivative(TimeDerivative),
    StopTask(StopTask),
    Print(Print),
    NodeCollapse(NodeCollapse),
    VoltageLim(VoltageLim),
    CurrentLim(CurrentLim),
    AcStimulus(AcStimulus),
    Analysis(Analysis),
    Discontinuity(Discontinuity),
}

impl TestSession<'_> {
    pub fn compile_to_mir(&self, file: impl AsRef<Path>) -> Result<Mir> {
        let ast = self.run_parser(file)?;
        let warnings = Linter::early_user_diagnostics::<ExpansionPrinter>()?;
        if !warnings.0.is_empty() {
            self.println(warnings.to_string());
        }
        let hir = lower_ast_userfacing(ast, |_| AllowedOperations::empty())?;
        Ok(lower_hir_userfacing(hir, &mut TestLowering)?)
    }
}

impl From<DefaultFunctions> for TestFunctions {
    fn from(fun: DefaultFunctions) -> Self {
        match fun {
            DefaultFunctions::Noise(x) => x.into(),
            DefaultFunctions::TimeDerivative(x) => x.into(),
            DefaultFunctions::StopTask(x) => x.into(),
            DefaultFunctions::Print(x) => x.into(),
            DefaultFunctions::NodeCollapse(x) => x.into(),
            DefaultFunctions::VoltageLim(x) => x.into(),
            DefaultFunctions::CurrentLim(x) => x.into(),
            DefaultFunctions::AcStimulus(x) => x.into(),
            DefaultFunctions::Analysis(x) => x.into(),
            DefaultFunctions::Discontinuity(x) => x.into(),
        }
    }
}
