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
        EditTarget::Range($rt, true, Count::Contextual)
    };
    ($rt: expr, $c: expr) => {
        EditTarget::Range($rt, true, Count::Exact($c))
    };
}

macro_rules! edit {
    ($ebuf: expr, $act: expr, $target: expr, $ctx: expr) => {
        $ebuf.edit(&$act, &$target, $ctx).unwrap()
    };
}

macro_rules! paste {
    ($ebuf: expr, $dir: expr, $c: expr, $ctx: expr) => {
        $ebuf.paste($dir, $c, $ctx).unwrap()
    };
}

macro_rules! ctx {
    ($curid: expr, $vwctx: expr, $vctx: expr) => {
        &($curid, &$vwctx, &$vctx)
    };
}

macro_rules! cell {
    ($shape: expr, $str: expr) => {
        RegisterCell::new($shape, EditRope::from($str))
    };
}

macro_rules! set_reg {
    ($ebuf: expr, $reg: expr, $shape: expr, $txt: expr) => {
        $ebuf.set_register(&Some($reg), cell!($shape, $txt), false, false);
    };
}

macro_rules! set_named_reg {
    ($ebuf: expr, $reg: expr, $shape: expr, $txt: expr) => {
        set_reg!($ebuf, Register::Named($reg), $shape, $txt);
    };
}

macro_rules! type_char {
    ($ebuf: expr, $c: expr, $curid: expr, $vwctx: expr, $vctx: expr) => {
        $ebuf
            .type_char(
                Char::Single($c).into(),
                MoveDir1D::Previous,
                1.into(),
                ctx!($curid, $vwctx, $vctx),
            )
            .unwrap()
    };
}
