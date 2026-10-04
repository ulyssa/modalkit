use editor_types_parser::{ActionToken, Flag};

use crate::prelude::*;
use crate::*;

mod action;
mod edit_target;
mod motion;
mod range;

trait ToTokens {
    fn to_tokens(&self) -> Vec<ActionToken<'_>>;
}

/// Helper for building an [ActionToken::Group] that's just a `word` followed by a value.
///
/// This is needed for building things like:
///
/// - `(exact 1)`
/// - `(line scope)`
/// - `(merge select-cursor -d next)`
#[inline(always)]
fn prefixed<'a>(word: &'a str, value: ActionToken<'a>) -> ActionToken<'a> {
    let mut tokens = vec![ActionToken::Word(word)];

    match value {
        ActionToken::Group(group) => tokens.extend(group),
        token => tokens.push(token),
    }

    ActionToken::Group(tokens)
}

/// Convert a [Specifier] into the appropriate `ctx` or `(exact ...)` tokens.
fn specifier<'a, 'b, T>(input: &'a Specifier<T>) -> ActionToken<'b>
where
    ActionToken<'b>: From<&'a T>,
{
    match input {
        Specifier::Contextual => ActionToken::Word("ctx"),
        Specifier::Exact(value) => prefixed("exact", ActionToken::from(value)),
    }
}

impl From<&Axis> for ActionToken<'_> {
    fn from(input: &Axis) -> Self {
        match input {
            Axis::Horizontal => ActionToken::Word("horizontal"),
            Axis::Vertical => ActionToken::Word("vertical"),
        }
    }
}

impl<'a> From<&'a Char> for ActionToken<'a> {
    fn from(input: &'a Char) -> Self {
        match input {
            Char::CopyLine(dir) => {
                ActionToken::Group(vec![
                    ActionToken::Word("copy-line"),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                ])
            },
            Char::CtrlSeq(input) => {
                ActionToken::Group(vec![
                    ActionToken::Word("ctrl-seq"),
                    ActionToken::Flag(Flag::Input),
                    ActionToken::Str(Cow::Borrowed(input.as_str())),
                ])
            },
            Char::Single(c) => ActionToken::Char(*c),
            Char::Digraph(c1, c2) => {
                ActionToken::Group(vec![
                    ActionToken::Word("digraph"),
                    ActionToken::Char(*c1),
                    ActionToken::Char(*c2),
                ])
            },
        }
    }
}

impl From<&CommandType> for ActionToken<'_> {
    fn from(input: &CommandType) -> Self {
        match input {
            CommandType::Application => ActionToken::Word("application"),
            CommandType::Command => ActionToken::Word("command"),
            CommandType::Content => ActionToken::Word("content"),
            CommandType::Search => ActionToken::Word("search"),
            CommandType::Shell => ActionToken::Word("shell"),
        }
    }
}

impl From<&CompletionDisplay> for ActionToken<'_> {
    fn from(input: &CompletionDisplay) -> Self {
        match input {
            CompletionDisplay::None => ActionToken::Word("none"),
            CompletionDisplay::Bar => ActionToken::Word("bar"),
            CompletionDisplay::List => ActionToken::Word("list"),
        }
    }
}

impl From<&CompletionScope> for ActionToken<'_> {
    fn from(input: &CompletionScope) -> Self {
        match input {
            CompletionScope::Buffer => ActionToken::Word("buffer"),
            CompletionScope::Global => ActionToken::Word("global"),
        }
    }
}

impl From<&CompletionStyle> for ActionToken<'_> {
    fn from(input: &CompletionStyle) -> Self {
        match input {
            CompletionStyle::None => ActionToken::Word("none"),
            CompletionStyle::Prefix => ActionToken::Word("prefix"),
            CompletionStyle::Single => ActionToken::Word("single"),
            CompletionStyle::List(dir, toggle) => {
                ActionToken::Group(vec![
                    ActionToken::Word("list"),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                    ActionToken::Flag(Flag::Long("toggle".into())),
                    ActionToken::Bool(*toggle),
                ])
            },
        }
    }
}

impl From<&CompletionType> for ActionToken<'_> {
    fn from(input: &CompletionType) -> Self {
        match input {
            CompletionType::Auto => ActionToken::Word("auto"),
            CompletionType::File => ActionToken::Word("file"),
            CompletionType::Line(scope) => prefixed("line", ActionToken::from(scope)),
            CompletionType::Word(scope) => prefixed("word", ActionToken::from(scope)),
        }
    }
}

impl From<&CursorCloseTarget> for ActionToken<'_> {
    fn from(input: &CursorCloseTarget) -> Self {
        match input {
            CursorCloseTarget::Leader => ActionToken::Word("leader"),
            CursorCloseTarget::Followers => ActionToken::Word("followers"),
        }
    }
}

impl From<&CursorGroupCombineStyle> for ActionToken<'_> {
    fn from(input: &CursorGroupCombineStyle) -> Self {
        match input {
            CursorGroupCombineStyle::Append => ActionToken::Word("append"),
            CursorGroupCombineStyle::Replace => ActionToken::Word("replace"),
            CursorGroupCombineStyle::Merge(style) => prefixed("merge", ActionToken::from(style)),
        }
    }
}

impl From<&CursorMergeStyle> for ActionToken<'_> {
    fn from(input: &CursorMergeStyle) -> Self {
        match input {
            CursorMergeStyle::Union => ActionToken::Word("union"),
            CursorMergeStyle::Intersect => ActionToken::Word("intersect"),
            CursorMergeStyle::SelectShort => ActionToken::Word("select-short"),
            CursorMergeStyle::SelectLong => ActionToken::Word("select-long"),
            CursorMergeStyle::SelectCursor(dir) => {
                ActionToken::Group(vec![
                    ActionToken::Word("select-cursor"),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                ])
            },
        }
    }
}

impl From<&Case> for ActionToken<'_> {
    fn from(input: &Case) -> Self {
        match input {
            Case::Upper => ActionToken::Word("upper"),
            Case::Lower => ActionToken::Word("lower"),
            Case::Title => ActionToken::Word("title"),
            Case::Toggle => ActionToken::Word("toggle"),
        }
    }
}

impl From<&Count> for ActionToken<'_> {
    fn from(input: &Count) -> Self {
        match input {
            Count::Contextual => ActionToken::Word("ctx"),
            Count::MinusOne => ActionToken::Word("ctx-sub-one"),
            Count::Exact(n) => ActionToken::Number(*n),
        }
    }
}

impl From<&EditAction> for ActionToken<'_> {
    fn from(input: &EditAction) -> Self {
        match input {
            EditAction::Motion => ActionToken::Word("motion"),
            EditAction::Delete => ActionToken::Word("delete"),
            EditAction::Yank => ActionToken::Word("yank"),
            EditAction::Format => ActionToken::Word("format"),
            EditAction::Replace(virt) => {
                ActionToken::Group(vec![
                    ActionToken::Word("replace"),
                    ActionToken::Flag(Flag::Long("virtual".into())),
                    ActionToken::Bool(*virt),
                ])
            },
            EditAction::ChangeNumber(style, multiple) => {
                ActionToken::Group(vec![
                    ActionToken::Word("change-number"),
                    ActionToken::Flag(Flag::Style),
                    ActionToken::from(style),
                    ActionToken::Flag(Flag::Long("multiply".into())),
                    ActionToken::Bool(*multiple),
                ])
            },
            EditAction::Join(style) => {
                ActionToken::Group(vec![
                    ActionToken::Word("join"),
                    ActionToken::Flag(Flag::Style),
                    ActionToken::from(style),
                ])
            },
            EditAction::Indent(change) => {
                ActionToken::Group(vec![
                    ActionToken::Word("indent"),
                    ActionToken::Flag(Flag::Style),
                    ActionToken::from(change),
                ])
            },
            EditAction::ChangeCase(case) => {
                ActionToken::Group(vec![
                    ActionToken::Word("change-case"),
                    ActionToken::Flag(Flag::Style),
                    ActionToken::from(case),
                ])
            },
        }
    }
}

impl From<&FocusChange> for ActionToken<'_> {
    fn from(input: &FocusChange) -> Self {
        match input {
            FocusChange::Current => ActionToken::Word("current"),
            FocusChange::PreviouslyFocused => ActionToken::Word("previously-focused"),
            FocusChange::Offset(count, clamp_last) => {
                ActionToken::Group(vec![
                    ActionToken::Word("offset"),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                    ActionToken::Flag(Flag::Short('l')),
                    ActionToken::Bool(*clamp_last),
                ])
            },
            FocusChange::Position(pos) => {
                ActionToken::Group(vec![
                    ActionToken::Word("position"),
                    ActionToken::Flag(Flag::Position),
                    ActionToken::from(pos),
                ])
            },
            FocusChange::Direction1D(dir, count, wrap) => {
                ActionToken::Group(vec![
                    ActionToken::Word("dir1d"),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                    ActionToken::Flag(Flag::Wrap),
                    ActionToken::Bool(*wrap),
                ])
            },
            FocusChange::Direction2D(dir, count) => {
                ActionToken::Group(vec![
                    ActionToken::Word("dir2d"),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ])
            },
        }
    }
}

impl From<&IndentChange> for ActionToken<'_> {
    fn from(input: &IndentChange) -> Self {
        match input {
            IndentChange::Auto => ActionToken::Word("auto"),
            IndentChange::Increase(count) => {
                ActionToken::Group(vec![
                    ActionToken::Word("increase"),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ])
            },
            IndentChange::Decrease(count) => {
                ActionToken::Group(vec![
                    ActionToken::Word("decrease"),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ])
            },
        }
    }
}

impl From<&NumberChange> for ActionToken<'_> {
    fn from(input: &NumberChange) -> Self {
        match input {
            NumberChange::Increase(count) => {
                ActionToken::Group(vec![
                    ActionToken::Word("increase"),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ])
            },
            NumberChange::Decrease(count) => {
                ActionToken::Group(vec![
                    ActionToken::Word("decrease"),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ])
            },
        }
    }
}

impl From<&JoinStyle> for ActionToken<'_> {
    fn from(input: &JoinStyle) -> Self {
        match input {
            JoinStyle::NoChange => ActionToken::Word("no-change"),
            JoinStyle::OneSpace => ActionToken::Word("one-space"),
            JoinStyle::NewSpace => ActionToken::Word("new-space"),
        }
    }
}

impl From<&KeywordTarget> for ActionToken<'_> {
    fn from(input: &KeywordTarget) -> Self {
        match input {
            KeywordTarget::Selection => ActionToken::Word("selection"),
            KeywordTarget::Word(style) => prefixed("word", ActionToken::from(style)),
        }
    }
}

impl From<&Mark> for ActionToken<'_> {
    fn from(input: &Mark) -> Self {
        match input {
            Mark::BufferLastExited => ActionToken::Word("buffer-last-exited"),
            Mark::LastChanged => ActionToken::Word("last-changed"),
            Mark::LastInserted => ActionToken::Word("last-inserted"),
            Mark::LastJump => ActionToken::Word("last-jump"),
            Mark::VisualBegin => ActionToken::Word("visual-begin"),
            Mark::VisualEnd => ActionToken::Word("visual-end"),
            Mark::LastYankedBegin => ActionToken::Word("last-yanked-begin"),
            Mark::LastYankedEnd => ActionToken::Word("last-yanked-end"),
            Mark::BufferNamed(c) => prefixed("buffer-named", ActionToken::Char(*c)),
            Mark::GlobalLastExited(n) => prefixed("global-last-exited", ActionToken::Number(*n)),
            Mark::GlobalNamed(c) => prefixed("global-named", ActionToken::Char(*c)),
        }
    }
}

impl From<&MoveDir1D> for ActionToken<'_> {
    fn from(input: &MoveDir1D) -> Self {
        match input {
            MoveDir1D::Next => ActionToken::Word("next"),
            MoveDir1D::Previous => ActionToken::Word("prev"),
        }
    }
}

impl From<&MoveDir2D> for ActionToken<'_> {
    fn from(input: &MoveDir2D) -> Self {
        match input {
            MoveDir2D::Up => ActionToken::Word("up"),
            MoveDir2D::Down => ActionToken::Word("down"),
            MoveDir2D::Left => ActionToken::Word("left"),
            MoveDir2D::Right => ActionToken::Word("right"),
        }
    }
}

impl From<&MoveDirMod> for ActionToken<'_> {
    fn from(input: &MoveDirMod) -> Self {
        match input {
            MoveDirMod::Same => ActionToken::Word("same"),
            MoveDirMod::Flip => ActionToken::Word("flip"),
            MoveDirMod::Exact(dir) => prefixed("exact", ActionToken::from(dir)),
        }
    }
}

impl From<&MovePosition> for ActionToken<'_> {
    fn from(input: &MovePosition) -> Self {
        match input {
            MovePosition::Beginning => ActionToken::Word("b"),
            MovePosition::Middle => ActionToken::Word("m"),
            MovePosition::End => ActionToken::Word("e"),
        }
    }
}

impl From<&MoveTerminus> for ActionToken<'_> {
    fn from(input: &MoveTerminus) -> Self {
        match input {
            MoveTerminus::Beginning => ActionToken::Word("b"),
            MoveTerminus::End => ActionToken::Word("e"),
        }
    }
}

impl<'a, W: ApplicationWindowId> From<&'a OpenTarget<W>> for ActionToken<'a> {
    fn from(input: &'a OpenTarget<W>) -> Self {
        match input {
            OpenTarget::Alternate => ActionToken::Word("alternate"),
            OpenTarget::Application(..) => {
                // XXX: Need to provide a way to handle application targets:
                ActionToken::Word("current")
            },
            OpenTarget::Current => ActionToken::Word("current"),
            OpenTarget::Selection => ActionToken::Word("selection"),
            OpenTarget::Unnamed => ActionToken::Word("unnamed"),
            OpenTarget::Cursor(style) => {
                ActionToken::Group(vec![
                    ActionToken::Word("cursor"),
                    ActionToken::Flag(Flag::Style),
                    ActionToken::from(style),
                ])
            },
            OpenTarget::List(count) => {
                ActionToken::Group(vec![
                    ActionToken::Word("list"),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ])
            },
            OpenTarget::Name(name) => {
                ActionToken::Group(vec![
                    ActionToken::Word("name"),
                    ActionToken::Flag(Flag::Input),
                    ActionToken::Str(Cow::Borrowed(name.as_str())),
                ])
            },
            OpenTarget::Offset(dir, count) => {
                ActionToken::Group(vec![
                    ActionToken::Word("offset"),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ])
            },
        }
    }
}

impl From<&PasteStyle> for ActionToken<'_> {
    fn from(input: &PasteStyle) -> Self {
        match input {
            PasteStyle::Cursor => ActionToken::Word("cursor"),
            PasteStyle::Replace => ActionToken::Word("replace"),
            PasteStyle::Side(dir) => {
                ActionToken::Group(vec![
                    ActionToken::Word("side"),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                ])
            },
        }
    }
}

impl From<&PositionList> for ActionToken<'_> {
    fn from(input: &PositionList) -> Self {
        match input {
            PositionList::JumpList => ActionToken::Word("jump-list"),
            PositionList::ChangeList => ActionToken::Word("change-list"),
        }
    }
}

impl From<&Radix> for ActionToken<'_> {
    fn from(input: &Radix) -> Self {
        match input {
            Radix::Binary => ActionToken::Word("bin"),
            Radix::Octal => ActionToken::Word("oct"),
            Radix::Decimal => ActionToken::Word("dec"),
            Radix::Hexadecimal => ActionToken::Word("hex"),
        }
    }
}

impl From<&RecallFilter> for ActionToken<'_> {
    fn from(input: &RecallFilter) -> Self {
        match input {
            RecallFilter::All => ActionToken::Word("all"),
            RecallFilter::PrefixMatch => ActionToken::Word("prefix-match"),
        }
    }
}

impl From<&SizeChange> for ActionToken<'_> {
    fn from(input: &SizeChange) -> Self {
        match input {
            SizeChange::Decrease(count) => prefixed("decrease", ActionToken::from(count)),
            SizeChange::Increase(count) => prefixed("increase", ActionToken::from(count)),
            SizeChange::Exact(count) => prefixed("exact", ActionToken::from(count)),
            SizeChange::Equal => ActionToken::Word("equal"),
        }
    }
}

impl From<&ScrollSize> for ActionToken<'_> {
    fn from(input: &ScrollSize) -> Self {
        match input {
            ScrollSize::Cell => ActionToken::Word("cell"),
            ScrollSize::HalfPage => ActionToken::Word("half-page"),
            ScrollSize::Page => ActionToken::Word("page"),
        }
    }
}

impl From<&ScrollStyle> for ActionToken<'_> {
    fn from(input: &ScrollStyle) -> Self {
        match input {
            ScrollStyle::Direction2D(dir, size, count) => {
                ActionToken::Group(vec![
                    ActionToken::Word("dir2d"),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                    ActionToken::Flag(Flag::Short('z')),
                    ActionToken::from(size),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ])
            },
            ScrollStyle::CursorPos(pos, axis) => {
                ActionToken::Group(vec![
                    ActionToken::Word("cursor-pos"),
                    ActionToken::Flag(Flag::Position),
                    ActionToken::from(pos),
                    ActionToken::Flag(Flag::Short('x')),
                    ActionToken::from(axis),
                ])
            },
            ScrollStyle::LinePos(pos, count) => {
                ActionToken::Group(vec![
                    ActionToken::Word("line-pos"),
                    ActionToken::Flag(Flag::Position),
                    ActionToken::from(pos),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ])
            },
        }
    }
}

impl From<&SearchType> for ActionToken<'_> {
    fn from(input: &SearchType) -> Self {
        match input {
            SearchType::Regex => ActionToken::Word("regex"),
            SearchType::Char(multiline) => {
                ActionToken::Group(vec![
                    ActionToken::Word("char"),
                    ActionToken::Flag(Flag::Long("multiline".into())),
                    ActionToken::Bool(*multiline),
                ])
            },
            SearchType::Word(style, boundary) => {
                ActionToken::Group(vec![
                    ActionToken::Word("word"),
                    ActionToken::Flag(Flag::Style),
                    ActionToken::from(style),
                    ActionToken::Flag(Flag::Short('b')),
                    ActionToken::Bool(*boundary),
                ])
            },
        }
    }
}

impl From<&MatchAction> for ActionToken<'_> {
    fn from(input: &MatchAction) -> Self {
        match input {
            MatchAction::Keep => ActionToken::Word("keep"),
            MatchAction::Drop => ActionToken::Word("drop"),
        }
    }
}

impl From<&RepeatType> for ActionToken<'_> {
    fn from(input: &RepeatType) -> Self {
        match input {
            RepeatType::EditSequence => ActionToken::Word("edit-sequence"),
            RepeatType::LastAction => ActionToken::Word("last-action"),
            RepeatType::LastSelection => ActionToken::Word("last-selection"),
        }
    }
}

impl From<&SelectionBoundary> for ActionToken<'_> {
    fn from(input: &SelectionBoundary) -> Self {
        match input {
            SelectionBoundary::Line => ActionToken::Word("line"),
            SelectionBoundary::NonWhitespace => ActionToken::Word("non-ws"),
        }
    }
}

impl From<&SelectionCursorChange> for ActionToken<'_> {
    fn from(input: &SelectionCursorChange) -> Self {
        match input {
            SelectionCursorChange::Beginning => ActionToken::Word("beginning"),
            SelectionCursorChange::End => ActionToken::Word("end"),
            SelectionCursorChange::SwapAnchor => ActionToken::Word("swap-anchor"),
            SelectionCursorChange::SwapSide => ActionToken::Word("swap-side"),
        }
    }
}

impl From<&SelectionResizeStyle> for ActionToken<'_> {
    fn from(input: &SelectionResizeStyle) -> Self {
        match input {
            SelectionResizeStyle::Extend => ActionToken::Word("extend"),
            SelectionResizeStyle::Object => ActionToken::Word("object"),
            SelectionResizeStyle::Restart => ActionToken::Word("restart"),
        }
    }
}

impl From<&SelectionSplitStyle> for ActionToken<'_> {
    fn from(input: &SelectionSplitStyle) -> Self {
        match input {
            SelectionSplitStyle::Anchor => ActionToken::Word("anchor"),
            SelectionSplitStyle::Lines => ActionToken::Word("lines"),
            SelectionSplitStyle::Regex(act) => prefixed("regex", ActionToken::from(act)),
        }
    }
}

impl From<&TargetShape> for ActionToken<'_> {
    fn from(input: &TargetShape) -> Self {
        match input {
            TargetShape::CharWise => ActionToken::Word("char"),
            TargetShape::LineWise => ActionToken::Word("line"),
            TargetShape::BlockWise => ActionToken::Word("block"),
        }
    }
}

impl From<&TargetShapeFilter> for ActionToken<'_> {
    fn from(input: &TargetShapeFilter) -> Self {
        if input.contains(TargetShapeFilter::ALL) {
            return ActionToken::Word("all");
        }

        let mut flags = vec![];

        if input.contains(TargetShapeFilter::CHAR) {
            flags.push(ActionToken::Word("char"));
        }

        if input.contains(TargetShapeFilter::LINE) {
            flags.push(ActionToken::Word("line"));
        }

        if input.contains(TargetShapeFilter::BLOCK) {
            flags.push(ActionToken::Word("block"));
        }

        if flags.is_empty() {
            ActionToken::Word("none")
        } else {
            ActionToken::Group(flags)
        }
    }
}

impl From<&TabTarget> for ActionToken<'_> {
    fn from(input: &TabTarget) -> Self {
        match input {
            TabTarget::All => ActionToken::Word("all"),
            TabTarget::AllBut(fc) => prefixed("all-but", ActionToken::from(fc)),
            TabTarget::Single(fc) => prefixed("single", ActionToken::from(fc)),
        }
    }
}

impl From<&CloseFlags> for ActionToken<'_> {
    fn from(input: &CloseFlags) -> Self {
        let mut flags = vec![];

        if input.contains(CloseFlags::FORCE) {
            flags.push(ActionToken::Word("force"));
        }

        if input.contains(CloseFlags::QUIT) {
            flags.push(ActionToken::Word("quit"));
        }

        if input.contains(CloseFlags::WRITE) {
            flags.push(ActionToken::Word("write"));
        }

        if flags.is_empty() {
            ActionToken::Word("none")
        } else {
            ActionToken::Group(flags)
        }
    }
}

impl From<&WriteFlags> for ActionToken<'_> {
    fn from(input: &WriteFlags) -> Self {
        let mut flags = vec![];

        if input.contains(WriteFlags::FORCE) {
            flags.push(ActionToken::Word("force"));
        }

        if flags.is_empty() {
            ActionToken::Word("none")
        } else {
            ActionToken::Group(flags)
        }
    }
}

impl From<&WindowTarget> for ActionToken<'_> {
    fn from(input: &WindowTarget) -> Self {
        match input {
            WindowTarget::All => ActionToken::Word("all"),
            WindowTarget::AllBut(fc) => prefixed("all-but", ActionToken::from(fc)),
            WindowTarget::Single(fc) => prefixed("single", ActionToken::from(fc)),
        }
    }
}

impl From<&WordStyle> for ActionToken<'_> {
    fn from(input: &WordStyle) -> Self {
        match input {
            WordStyle::AlphaNum => ActionToken::Word("alphanum"),
            WordStyle::Big => ActionToken::Word("big"),
            WordStyle::CharSet(..) => {
                // Can't actually represent a Rust function in the DSL, and we don't generate
                // this with `proptest`, so just use `alphanum` if we somehow get here:
                ActionToken::Word("alphanum")
            },
            WordStyle::FileName => ActionToken::Word("filename"),
            WordStyle::FilePath => ActionToken::Word("filepath"),
            WordStyle::Little => ActionToken::Word("little"),
            WordStyle::NonAlphaNum => ActionToken::Word("non-alphanum"),
            WordStyle::Number(radix) => prefixed("radix", ActionToken::from(radix)),
            WordStyle::Whitespace(wrap) => {
                ActionToken::Group(vec![
                    ActionToken::Word("whitespace"),
                    ActionToken::Flag(Flag::Wrap),
                    ActionToken::Bool(*wrap),
                ])
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use proptest::prelude::*;

    use super::*;

    /// Assert that converting the [Action] to a string can be parsed back into the same action.
    fn assert_round_trip(act: &Action) -> Result<(), TestCaseError> {
        let s = act.to_string();
        let parsed = Action::from_str(&s);

        match parsed {
            Ok(parsed) => {
                prop_assert_eq!(
                    act,
                    &parsed,
                    "failed to parse `{}` back into the original action",
                    s
                );
                Ok(())
            },
            Err(e) => {
                let msg = format!("failed to parse `{s}`: {e}");
                Err(TestCaseError::fail(msg))
            },
        }
    }

    /// Strategy for generating a `cmdbar focus`, which we skip in the derived implementation
    /// in order to avoid recursion.
    fn cmdbar_focus() -> impl Strategy<Value = Action> {
        (any::<String>(), any::<CommandType>(), any::<Action>()).prop_map(
            |(prompt, cmdtype, act)| {
                Action::CommandBar(CommandBarAction::Focus(prompt, cmdtype, Box::new(act)))
            },
        )
    }

    proptest! {
        #[test]
        fn test_round_trip(act: Action) {
            assert_round_trip(&act)?;
        }

        #[test]
        fn test_round_trip_cmdbar_focus(act in cmdbar_focus()) {
            assert_round_trip(&act)?;
        }
    }
}
