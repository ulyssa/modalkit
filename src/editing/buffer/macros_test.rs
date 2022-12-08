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
    ($ebuf: expr, $act: expr, $target: expr, $ctx: expr, $store: expr) => {
        $ebuf.edit(&$act, &$target, $ctx, &mut $store).unwrap()
    };
}

macro_rules! paste {
    ($ebuf: expr, $dir: expr, $c: expr, $ctx: expr, $store: expr) => {
        $ebuf.paste($dir, &$c, $ctx, &mut $store).unwrap()
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
    ($store: expr, $reg: expr, $shape: expr, $txt: expr) => {
        $store.registers.put(&$reg, cell!($shape, $txt), RegisterPutFlags::NONE);
    };
}

macro_rules! set_named_reg {
    ($store: expr, $reg: expr, $shape: expr, $txt: expr) => {
        set_reg!($store, Register::Named($reg), $shape, $txt);
    };
}

macro_rules! type_char {
    ($ebuf: expr, $c: expr, $curid: expr, $vwctx: expr, $vctx: expr, $store: expr) => {
        $ebuf
            .type_char(
                Char::Single($c).into(),
                MoveDir1D::Previous,
                &1.into(),
                ctx!($curid, $vwctx, $vctx),
                &mut $store,
            )
            .unwrap()
    };
}

macro_rules! mark {
    ($c: expr) => {
        Mark::BufferNamed($c)
    };
}

macro_rules! edit_char_mark {
    ($ebuf: expr, $act: expr, $c: expr, $curid: expr, $vwctx: expr, $vctx: expr, $store: expr) => {
        edit!(
            $ebuf,
            $act,
            EditTarget::CharJump(Specifier::Exact(mark!($c))),
            ctx!($curid, $vwctx, $vctx),
            $store
        )
    };
}

macro_rules! edit_line_mark {
    ($ebuf: expr, $act: expr, $c: expr, $curid: expr, $vwctx: expr, $vctx: expr, $store: expr) => {
        edit!(
            $ebuf,
            $act,
            EditTarget::LineJump(Specifier::Exact(mark!($c))),
            ctx!($curid, $vwctx, $vctx),
            $store
        )
    };
}
