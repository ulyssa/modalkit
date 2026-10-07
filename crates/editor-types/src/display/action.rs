use std::fmt;

use super::*;

impl<I> fmt::Display for Action<I>
where
    I: ApplicationInfo,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, token) in self.to_tokens().into_iter().enumerate() {
            if i == 0 {
                write!(f, "{token}")?;
            } else {
                write!(f, " {token}")?;
            }
        }

        Ok(())
    }
}

impl<I> ToTokens for Action<I>
where
    I: ApplicationInfo,
{
    fn to_tokens(&self) -> Vec<ActionToken<'_>> {
        match self {
            Action::NoOp => vec![ActionToken::Word("no-op")],
            Action::RedrawScreen => vec![ActionToken::Word("redraw-screen")],
            Action::Suspend => vec![ActionToken::Word("suspend")],
            Action::Repeat(rt) => {
                vec![
                    ActionToken::Word("repeat"),
                    ActionToken::Flag(Flag::Style),
                    ActionToken::from(rt),
                ]
            },
            Action::Jump(target, dir, count) => {
                vec![
                    ActionToken::Word("jump"),
                    ActionToken::Flag(Flag::Target),
                    ActionToken::from(target),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ]
            },
            Action::KeywordLookup(t) => {
                vec![
                    ActionToken::Word("keyword-lookup"),
                    ActionToken::Flag(Flag::Target),
                    ActionToken::from(t),
                ]
            },
            Action::Scroll(style) => {
                vec![
                    ActionToken::Word("scroll"),
                    ActionToken::Flag(Flag::Style),
                    ActionToken::from(style),
                ]
            },
            Action::Search(dir, count) => {
                vec![
                    ActionToken::Word("search"),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ]
            },
            Action::ShowInfoMessage(_) => {
                // XXX
                vec![]
            },
            Action::Application(..) => {
                // XXX
                vec![]
            },

            Action::Editor(act) => act.to_tokens(),
            Action::Command(act) => act.to_tokens(),
            Action::CommandBar(act) => act.to_tokens(),
            Action::Macro(act) => act.to_tokens(),
            Action::Prompt(act) => act.to_tokens(),
            Action::Tab(act) => act.to_tokens(),
            Action::Window(act) => act.to_tokens(),
        }
    }
}

impl ToTokens for EditorAction {
    fn to_tokens(&self) -> Vec<ActionToken<'_>> {
        match self {
            EditorAction::Complete(style, comptype, display) => {
                vec![
                    ActionToken::Word("complete"),
                    ActionToken::Flag(Flag::Style),
                    ActionToken::from(style),
                    ActionToken::Flag(Flag::Short('T')),
                    ActionToken::from(comptype),
                    ActionToken::Flag(Flag::Short('D')),
                    ActionToken::from(display),
                ]
            },
            EditorAction::Edit(act, target) => {
                vec![
                    ActionToken::Word("edit"),
                    ActionToken::Flag(Flag::Short('o')),
                    specifier(act),
                    ActionToken::Flag(Flag::Target),
                    ActionToken::from(target),
                ]
            },
            EditorAction::Mark(mark) => {
                vec![
                    ActionToken::Word("mark"),
                    ActionToken::Flag(Flag::Mark),
                    specifier(mark),
                ]
            },

            EditorAction::Cursor(act) => act.to_tokens(),
            EditorAction::History(act) => act.to_tokens(),
            EditorAction::InsertText(act) => act.to_tokens(),
            EditorAction::Selection(act) => act.to_tokens(),
        }
    }
}

impl ToTokens for CursorAction {
    fn to_tokens(&self) -> Vec<ActionToken<'_>> {
        match self {
            CursorAction::Close(target) => {
                vec![
                    ActionToken::Word("cursor"),
                    ActionToken::Word("close"),
                    ActionToken::Flag(Flag::Target),
                    ActionToken::from(target),
                ]
            },
            CursorAction::Restore(reg, style) => {
                vec![
                    ActionToken::Word("cursor"),
                    ActionToken::Word("restore"),
                    ActionToken::Flag(Flag::Register),
                    specifier(reg),
                    ActionToken::Flag(Flag::Style),
                    ActionToken::from(style),
                ]
            },
            CursorAction::Rotate(dir, count) => {
                vec![
                    ActionToken::Word("cursor"),
                    ActionToken::Word("rotate"),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ]
            },
            CursorAction::Save(reg, style) => {
                vec![
                    ActionToken::Word("cursor"),
                    ActionToken::Word("save"),
                    ActionToken::Flag(Flag::Register),
                    specifier(reg),
                    ActionToken::Flag(Flag::Style),
                    ActionToken::from(style),
                ]
            },
            CursorAction::Split(count) => {
                vec![
                    ActionToken::Word("cursor"),
                    ActionToken::Word("split"),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ]
            },
        }
    }
}

impl ToTokens for HistoryAction {
    fn to_tokens(&self) -> Vec<ActionToken<'_>> {
        match self {
            HistoryAction::Checkpoint => {
                vec![
                    ActionToken::Word("history"),
                    ActionToken::Word("checkpoint"),
                ]
            },
            HistoryAction::Redo(count) => {
                vec![
                    ActionToken::Word("history"),
                    ActionToken::Word("redo"),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ]
            },
            HistoryAction::Undo(count) => {
                vec![
                    ActionToken::Word("history"),
                    ActionToken::Word("undo"),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ]
            },
        }
    }
}

impl ToTokens for InsertTextAction {
    fn to_tokens(&self) -> Vec<ActionToken<'_>> {
        match self {
            InsertTextAction::OpenLine(shape, dir, count) => {
                vec![
                    ActionToken::Word("insert"),
                    ActionToken::Word("open-line"),
                    ActionToken::Flag(Flag::Short('S')),
                    ActionToken::from(shape),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ]
            },
            InsertTextAction::Paste(style, reg, count) => {
                vec![
                    ActionToken::Word("insert"),
                    ActionToken::Word("paste"),
                    ActionToken::Flag(Flag::Style),
                    ActionToken::from(style),
                    ActionToken::Flag(Flag::Register),
                    specifier(reg),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ]
            },
            InsertTextAction::Transcribe(input, dir, count) => {
                vec![
                    ActionToken::Word("insert"),
                    ActionToken::Word("transcribe"),
                    ActionToken::Flag(Flag::Input),
                    ActionToken::Str(Cow::Borrowed(input.as_str())),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ]
            },
            InsertTextAction::Type(c, dir, count) => {
                vec![
                    ActionToken::Word("insert"),
                    ActionToken::Word("type"),
                    ActionToken::Flag(Flag::Input),
                    specifier(c),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ]
            },
        }
    }
}

impl ToTokens for SelectionAction {
    fn to_tokens(&self) -> Vec<ActionToken<'_>> {
        match self {
            SelectionAction::CursorSet(change) => {
                vec![
                    ActionToken::Word("selection"),
                    ActionToken::Word("cursor-set"),
                    ActionToken::Flag(Flag::Focus),
                    ActionToken::from(change),
                ]
            },
            SelectionAction::Duplicate(dir, count) => {
                vec![
                    ActionToken::Word("selection"),
                    ActionToken::Word("duplicate"),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ]
            },
            SelectionAction::Expand(boundary, filter) => {
                vec![
                    ActionToken::Word("selection"),
                    ActionToken::Word("expand"),
                    ActionToken::Flag(Flag::Short('b')),
                    ActionToken::from(boundary),
                    ActionToken::Flag(Flag::Target),
                    ActionToken::from(filter),
                ]
            },
            SelectionAction::Filter(act) => {
                vec![
                    ActionToken::Word("selection"),
                    ActionToken::Word("filter"),
                    ActionToken::Flag(Flag::Short('F')),
                    ActionToken::from(act),
                ]
            },
            SelectionAction::Join => {
                vec![ActionToken::Word("selection"), ActionToken::Word("join")]
            },
            SelectionAction::Resize(style, target) => {
                vec![
                    ActionToken::Word("selection"),
                    ActionToken::Word("resize"),
                    ActionToken::Flag(Flag::Style),
                    ActionToken::from(style),
                    ActionToken::Flag(Flag::Target),
                    ActionToken::from(target),
                ]
            },
            SelectionAction::Split(style, filter) => {
                vec![
                    ActionToken::Word("selection"),
                    ActionToken::Word("split"),
                    ActionToken::Flag(Flag::Style),
                    ActionToken::from(style),
                    ActionToken::Flag(Flag::Short('F')),
                    ActionToken::from(filter),
                ]
            },
            SelectionAction::Trim(boundary, filter) => {
                vec![
                    ActionToken::Word("selection"),
                    ActionToken::Word("trim"),
                    ActionToken::Flag(Flag::Short('b')),
                    ActionToken::from(boundary),
                    ActionToken::Flag(Flag::Target),
                    ActionToken::from(filter),
                ]
            },
        }
    }
}

impl ToTokens for CommandAction {
    fn to_tokens(&self) -> Vec<ActionToken<'_>> {
        match self {
            CommandAction::Execute(count) => {
                vec![
                    ActionToken::Word("command"),
                    ActionToken::Word("execute"),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ]
            },
            CommandAction::Run(input) => {
                vec![
                    ActionToken::Word("command"),
                    ActionToken::Word("run"),
                    ActionToken::Flag(Flag::Input),
                    ActionToken::Str(Cow::Borrowed(input.as_str())),
                ]
            },
        }
    }
}

impl<I> ToTokens for CommandBarAction<I>
where
    I: ApplicationInfo,
{
    fn to_tokens(&self) -> Vec<ActionToken<'_>> {
        match self {
            CommandBarAction::Focus(prompt, cmdtype, act) => {
                vec![
                    ActionToken::Word("cmdbar"),
                    ActionToken::Word("focus"),
                    ActionToken::Flag(Flag::Short('P')),
                    ActionToken::Str(Cow::Borrowed(prompt.as_str())),
                    ActionToken::Flag(Flag::Style),
                    ActionToken::from(cmdtype),
                    ActionToken::Flag(Flag::Short('a')),
                    ActionToken::Group(act.to_tokens()),
                ]
            },
            CommandBarAction::Unfocus => {
                vec![ActionToken::Word("cmdbar"), ActionToken::Word("unfocus")]
            },
        }
    }
}

impl ToTokens for MacroAction {
    fn to_tokens(&self) -> Vec<ActionToken<'_>> {
        match self {
            MacroAction::Execute(reg, count) => {
                vec![
                    ActionToken::Word("macro"),
                    ActionToken::Word("execute"),
                    ActionToken::Flag(Flag::Register),
                    specifier(reg),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ]
            },
            MacroAction::Run(input, count) => {
                vec![
                    ActionToken::Word("macro"),
                    ActionToken::Word("run"),
                    ActionToken::Flag(Flag::Input),
                    ActionToken::Str(Cow::Borrowed(input.as_str())),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ]
            },
            MacroAction::Repeat(count) => {
                vec![
                    ActionToken::Word("macro"),
                    ActionToken::Word("repeat"),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ]
            },
            MacroAction::ToggleRecording(reg, style) => {
                vec![
                    ActionToken::Word("macro"),
                    ActionToken::Word("toggle-recording"),
                    ActionToken::Flag(Flag::Register),
                    specifier(reg),
                    ActionToken::Flag(Flag::Style),
                    specifier(style),
                ]
            },
        }
    }
}

impl ToTokens for PromptAction {
    fn to_tokens(&self) -> Vec<ActionToken<'_>> {
        match self {
            PromptAction::Abort(empty) => {
                vec![
                    ActionToken::Word("prompt"),
                    ActionToken::Word("abort"),
                    ActionToken::Flag(Flag::Long("empty".into())),
                    ActionToken::Bool(*empty),
                ]
            },
            PromptAction::Recall(filter, dir, count) => {
                vec![
                    ActionToken::Word("prompt"),
                    ActionToken::Word("recall"),
                    ActionToken::Flag(Flag::Short('F')),
                    ActionToken::from(filter),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ]
            },
            PromptAction::Submit => {
                vec![ActionToken::Word("prompt"), ActionToken::Word("submit")]
            },
        }
    }
}

impl<I> ToTokens for TabAction<I>
where
    I: ApplicationInfo,
{
    fn to_tokens(&self) -> Vec<ActionToken<'_>> {
        match self {
            TabAction::Close(target, flags) => {
                vec![
                    ActionToken::Word("tab"),
                    ActionToken::Word("close"),
                    ActionToken::Flag(Flag::Target),
                    ActionToken::from(target),
                    ActionToken::Flag(Flag::Short('F')),
                    ActionToken::from(flags),
                ]
            },
            TabAction::Extract(fc, dir) => {
                vec![
                    ActionToken::Word("tab"),
                    ActionToken::Word("extract"),
                    ActionToken::Flag(Flag::Focus),
                    ActionToken::from(fc),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                ]
            },
            TabAction::Focus(fc) => {
                vec![
                    ActionToken::Word("tab"),
                    ActionToken::Word("focus"),
                    ActionToken::Flag(Flag::Focus),
                    ActionToken::from(fc),
                ]
            },
            TabAction::Move(fc) => {
                vec![
                    ActionToken::Word("tab"),
                    ActionToken::Word("move"),
                    ActionToken::Flag(Flag::Focus),
                    ActionToken::from(fc),
                ]
            },
            TabAction::Open(target, fc) => {
                vec![
                    ActionToken::Word("tab"),
                    ActionToken::Word("open"),
                    ActionToken::Flag(Flag::Target),
                    ActionToken::from(target),
                    ActionToken::Flag(Flag::Focus),
                    ActionToken::from(fc),
                ]
            },
        }
    }
}

impl<I> ToTokens for WindowAction<I>
where
    I: ApplicationInfo,
{
    fn to_tokens(&self) -> Vec<ActionToken<'_>> {
        match self {
            WindowAction::ClearSizes => {
                vec![
                    ActionToken::Word("window"),
                    ActionToken::Word("clear-sizes"),
                ]
            },
            WindowAction::ZoomToggle => {
                vec![
                    ActionToken::Word("window"),
                    ActionToken::Word("zoom-toggle"),
                ]
            },
            WindowAction::Close(target, flags) => {
                vec![
                    ActionToken::Word("window"),
                    ActionToken::Word("close"),
                    ActionToken::Flag(Flag::Target),
                    ActionToken::from(target),
                    ActionToken::Flag(Flag::Short('F')),
                    ActionToken::from(flags),
                ]
            },
            WindowAction::Exchange(fc) => {
                vec![
                    ActionToken::Word("window"),
                    ActionToken::Word("exchange"),
                    ActionToken::Flag(Flag::Focus),
                    ActionToken::from(fc),
                ]
            },
            WindowAction::Focus(fc) => {
                vec![
                    ActionToken::Word("window"),
                    ActionToken::Word("focus"),
                    ActionToken::Flag(Flag::Focus),
                    ActionToken::from(fc),
                ]
            },
            WindowAction::MoveSide(dir) => {
                vec![
                    ActionToken::Word("window"),
                    ActionToken::Word("move-side"),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                ]
            },
            WindowAction::Open(target, axis, dir, count) => {
                vec![
                    ActionToken::Word("window"),
                    ActionToken::Word("open"),
                    ActionToken::Flag(Flag::Target),
                    ActionToken::from(target),
                    ActionToken::Flag(Flag::Short('x')),
                    ActionToken::from(axis),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ]
            },
            WindowAction::Resize(fc, axis, size) => {
                vec![
                    ActionToken::Word("window"),
                    ActionToken::Word("resize"),
                    ActionToken::Flag(Flag::Focus),
                    ActionToken::from(fc),
                    ActionToken::Flag(Flag::Short('x')),
                    ActionToken::from(axis),
                    ActionToken::Flag(Flag::Short('z')),
                    ActionToken::from(size),
                ]
            },
            WindowAction::Rotate(dir) => {
                vec![
                    ActionToken::Word("window"),
                    ActionToken::Word("rotate"),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                ]
            },
            WindowAction::Split(target, axis, dir, count) => {
                vec![
                    ActionToken::Word("window"),
                    ActionToken::Word("split"),
                    ActionToken::Flag(Flag::Target),
                    ActionToken::from(target),
                    ActionToken::Flag(Flag::Short('x')),
                    ActionToken::from(axis),
                    ActionToken::Flag(Flag::Dir),
                    ActionToken::from(dir),
                    ActionToken::Flag(Flag::Count),
                    ActionToken::from(count),
                ]
            },
            WindowAction::Switch(target) => {
                vec![
                    ActionToken::Word("window"),
                    ActionToken::Word("switch"),
                    ActionToken::Flag(Flag::Target),
                    ActionToken::from(target),
                ]
            },
            WindowAction::Write(target, name, flags) => {
                let mut tokens = vec![
                    ActionToken::Word("window"),
                    ActionToken::Word("write"),
                    ActionToken::Flag(Flag::Target),
                    ActionToken::from(target),
                ];

                if let Some(name) = name {
                    tokens.push(ActionToken::Flag(Flag::Input));
                    tokens.push(ActionToken::Str(Cow::Borrowed(name.as_str())));
                }

                tokens.push(ActionToken::Flag(Flag::Short('F')));
                tokens.push(ActionToken::from(flags));

                tokens
            },
        }
    }
}
