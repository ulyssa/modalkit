macro_rules! mv {
    ($mt: expr) => {
        EditTarget::Motion($mt.clone(), Count::Contextual)
    };
    ($mt: expr, $c: expr) => {
        EditTarget::Motion($mt.clone(), Count::Exact($c))
    };
}

macro_rules! range {
    ($rt: expr) => {
        EditTarget::Range($rt, Count::Contextual)
    };
    ($rt: expr, $c: expr) => {
        EditTarget::Range($rt, Count::Exact($c))
    };
}

macro_rules! edit {
    ($ebuf: expr, $act: expr, $target: expr, $ctx: expr) => {
        $ebuf.edit(&$act, &$target, $ctx).unwrap()
    };
}

macro_rules! ctx {
    ($curid: expr, $vwctx: expr, $vctx: expr) => {
        &($curid, &$vwctx, &$vctx)
    };
}

macro_rules! type_char {
    ($ebuf: expr, $c: expr, $curid: expr, $vwctx: expr, $vctx: expr) => {
        $ebuf
            .type_char(Char::Single($c).into(), MoveDir1D::Previous, ctx!($curid, $vwctx, $vctx))
            .unwrap()
    };
}
