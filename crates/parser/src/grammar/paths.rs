use super::*;

pub(crate) const PATH_SEGMENT_TS: TokenSet = TokenSet::new(&[IDENT, ROOT_KW]);

pub(crate) fn path(p: &mut Parser) -> CompletedMarker {
    assert!(p.at_ts(PATH_SEGMENT_TS));
    let path = p.start();
    p.expect_ts(PATH_SEGMENT_TS);
    let mut qual = path.complete(p, PATH);
    while p.at(T![.]) {
        let path = qual.precede(p);
        p.bump(T![.]);
        p.expect_ts(PATH_SEGMENT_TS);
        let path = path.complete(p, PATH);
        qual = path;
    }
    qual
}
