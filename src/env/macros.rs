macro_rules! unmapped {
    () => {
        isv!()
    };
}

macro_rules! insert_text {
    ($it: expr) => {
        act!(Action::InsertText($it))
    };
    ($it: expr, $ns: expr) => {
        act!(Action::InsertText($it), $ns)
    };
}

macro_rules! paste {
    ($dir: expr) => {
        insert_text!(InsertTextAction::Paste($dir, Count::Contextual))
    };
    ($dir: expr, $c: literal) => {
        insert_text!(InsertTextAction::Paste($dir, Count::Exact($c)))
    };
    ($dir: expr, $c: expr) => {
        insert_text!(InsertTextAction::Paste($dir, $c))
    };
    ($dir: expr, $c: expr, $nm: expr) => {
        insert_text!(InsertTextAction::Paste($dir, $c), $nm)
    };
}

macro_rules! chartype {
    () => {
        insert_text!(InsertTextAction::Type(Specifier::Contextual, MoveDir1D::Previous, 1.into()))
    };
    ($c: expr) => {
        insert_text!(InsertTextAction::Type(Specifier::Exact($c), MoveDir1D::Previous, 1.into()))
    };
    ($c: expr, $dir: expr) => {
        insert_text!(InsertTextAction::Type(Specifier::Exact($c), $dir, 1.into()))
    };
}

macro_rules! goto {
    ($mode: expr) => {
        isv!(vec![], vec![], $mode)
    };
}

macro_rules! cmdbar {
    ($type: expr) => {
        act!(Action::CommandBar($type))
    };
    ($type: expr, $ns: expr) => {
        act!(Action::CommandBar($type), $ns)
    };
}

macro_rules! command {
    ($type: expr) => {
        act!(Action::Command($type))
    };
    ($type: expr, $ns: expr) => {
        act!(Action::Command($type), $ns)
    };
}

macro_rules! selection {
    ($ea: expr) => {
        act!(Action::Selection($ea))
    };
    ($ea: expr, $ns: expr) => {
        act!(Action::Selection($ea), $ns)
    };
}

macro_rules! history {
    ($act: expr) => {
        act!(Action::History($act), Default::default())
    };
}

macro_rules! jump {
    ($l: expr, $d: expr) => {
        act!(Action::Jump($l, $d, Count::Contextual))
    };
}

macro_rules! scroll {
    ($style: expr) => {
        act!(Action::Scroll($style))
    };
}

macro_rules! scroll2d {
    ($d: expr, $t: expr) => {
        scroll!(ScrollStyle::Direction2D($d, $t, Count::Contextual))
    };
    ($d: expr, $t: expr, $c: literal) => {
        scroll!(ScrollStyle::Direction2D($d, $t, Count::Exact($c)))
    };
    ($d: expr, $t: expr, $c: expr) => {
        scroll!(ScrollStyle::Direction2D($d, $t, $c))
    };
}

macro_rules! edit_target {
    ($ea: expr, $et: expr) => {
        act!(Action::Edit(Specifier::Exact($ea), $et))
    };
    ($ea: expr, $et: expr, $mode: expr) => {
        act!(Action::Edit(Specifier::Exact($ea), $et), $mode)
    };
}

macro_rules! edit_buffer {
    ($ea: expr, $term: expr) => {
        edit_target!(
            $ea,
            EditTarget::Boundary(RangeType::Buffer, true, $term, Count::Contextual),
            Default::default()
        )
    };
    ($ea: expr, $term: expr, $mode: expr) => {
        edit_target!(
            $ea,
            EditTarget::Boundary(RangeType::Buffer, true, $term, Count::Contextual),
            $mode
        )
    };
}

macro_rules! edit {
    ($ea: expr, $mt: expr) => {
        edit_target!($ea, EditTarget::Motion($mt, Count::Contextual))
    };
    ($ea: expr, $mt: expr, $c: literal) => {
        edit_target!($ea, EditTarget::Motion($mt, Count::Exact($c)))
    };
    ($ea: expr, $mt: expr, $c: expr) => {
        edit_target!($ea, EditTarget::Motion($mt, $c))
    };
    ($ea: expr, $mt: expr, $c: expr, $mode: expr) => {
        edit_target!($ea, EditTarget::Motion($mt, $c), $mode)
    };
}
