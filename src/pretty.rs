use pretty::RcDoc;

use crate::{cst::*, lexer::Token};

pub trait Pretty {
    fn pretty(&self) -> RcDoc<()>;
}

impl Pretty for Token {
    fn pretty(&self) -> RcDoc<()> {
        todo!()
    }
}

impl<T> Pretty for Positioned<T>
where
    T: Pretty,
{
    fn pretty(&self) -> RcDoc<()> {
        todo!()
    }
}

impl<T> Pretty for Wrapped<T>
where
    T: Pretty,
{
    fn pretty(&self) -> RcDoc<()> {
        todo!()
    }
}

impl Pretty for Defun {
    fn pretty(&self) -> RcDoc<()> {
        RcDoc::text("(")
            .append("defun")
            .append("f ")
            .append("() ")
            .append(RcDoc::hardline().append("x").nest(2).group())
            .append(")")
    }
}
