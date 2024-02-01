//! # Common keybinding macros
//!
//! This private module contains convenience macros for helping to define actions to take for
//! different keybindings. Consumers are expected to define the following macros themselves:
//!
//! - isv!()
//! - act!()
//! - blackhole!()
//!
macro_rules! unmapped {
    () => {
        isv!()
    };
}

macro_rules! editor {
    ($ea: expr) => {
        act!(Action::Editor($ea))
    };
    ($ea: expr, $ns: expr) => {
        act!(Action::Editor($ea), $ns)
    };
}

macro_rules! complete {
    ($ct: expr, $cs: expr, $cd: expr) => {
        editor!(EditorAction::Complete($ct, $cs, $cd))
    };
}

macro_rules! insert_text {
    ($it: expr) => {
        editor!(EditorAction::InsertText($it))
    };
    ($it: expr, $ns: expr) => {
        editor!(EditorAction::InsertText($it), $ns)
    };
}

macro_rules! paste {
    ($style: expr) => {
        insert_text!(InsertTextAction::Paste($style, Count::Contextual))
    };
    ($style: expr, $c: literal) => {
        insert_text!(InsertTextAction::Paste($style, Count::Exact($c)))
    };
    ($style: expr, $c: expr) => {
        insert_text!(InsertTextAction::Paste($style, $c))
    };
    ($style: expr, $c: expr, $nm: expr) => {
        insert_text!(InsertTextAction::Paste($style, $c), $nm)
    };
}

macro_rules! paste_dir {
    ($dir: expr) => {
        paste!(PasteStyle::Side($dir))
    };
    ($dir: expr, $c: expr) => {
        paste!(PasteStyle::Side($dir), $c)
    };
    ($dir: expr, $c: expr, $nm: expr) => {
        paste!(PasteStyle::Side($dir), $c, $nm)
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

macro_rules! prompt {
    ($act: expr) => {
        act!(Action::Prompt($act))
    };
    ($act: expr, $ns: expr) => {
        act!(Action::Prompt($act), $ns)
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
        editor!(EditorAction::Selection($ea))
    };
    ($ea: expr, $ns: expr) => {
        editor!(EditorAction::Selection($ea), $ns)
    };
}

macro_rules! history {
    ($act: expr) => {
        editor!(EditorAction::History($act), Default::default())
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

macro_rules! scrollcp {
    ($p: expr, $axis: expr) => {
        scroll!(ScrollStyle::CursorPos($p, $axis))
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
        act!(Action::from(EditorAction::Edit(Specifier::Exact($ea), $et)))
    };
    ($ea: expr, $et: expr, $mode: expr) => {
        act!(Action::from(EditorAction::Edit(Specifier::Exact($ea), $et)), $mode)
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

macro_rules! edit_selection {
    ($ea: expr) => {
        edit_target!($ea, EditTarget::Selection, Default::default())
    };
    ($ea: expr, $mode: expr) => {
        edit_target!($ea, EditTarget::Selection, $mode)
    };
}

macro_rules! erase_target {
    ($et: expr) => {
        blackhole!(EditorAction::Edit(EditAction::Delete.into(), $et))
    };
}

macro_rules! erase {
    ($mt: expr) => {
        erase_target!(EditTarget::Motion($mt, Count::Contextual))
    };
    ($mt: expr, $c: literal) => {
        erase_target!(EditTarget::Motion($mt, Count::Exact($c)))
    };
}

macro_rules! erase_range {
    ($rt: expr) => {
        erase_target!(EditTarget::Range($rt, true, Count::Contextual))
    };
}
