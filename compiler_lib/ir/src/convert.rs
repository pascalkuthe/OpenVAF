/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::Spanned;

impl<A, B> Convert<Spanned<B>> for Spanned<A>
where
    A: Convert<B>,
{
    fn convert(self) -> Spanned<B> {
        Spanned {
            span: self.span,
            contents: self.contents.convert(),
        }
    }
}

pub trait Convert<X> {
    fn convert(self) -> X;
}

impl<A, B> Convert<Option<B>> for Option<A>
where
    A: Convert<B>,
{
    fn convert(self) -> Option<B> {
        self.map(|x| x.convert())
    }
}
