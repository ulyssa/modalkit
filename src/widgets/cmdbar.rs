//! # Command bar
//!
//! ## Overview
//!
//! These components allow creating a bar for entering searches and commands.

//! Typically, this widget is used indirectly by consumers through [Screen], which places this at
//! the bottom of the terminal window.
//!
//! [Screen]: super::screen::Screen
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};

use tui::{buffer::Buffer, layout::Rect, text::Span, widgets::StatefulWidget};

use crate::editing::{
    action::{
        Action,
        CommandAction,
        CommandBarAction,
        EditResult,
        EditorAction,
        PromptAction,
        Promptable,
    },
    application::ApplicationInfo,
    base::{CommandType, Count, EditTarget, MoveDir1D, MoveDirMod, SearchType},
    completion::CompletionList,
    context::{EditContext, Resolve},
    history::ScrollbackState,
    rope::EditRope,
    store::Store,
};

use super::{
    textbox::{TextBox, TextBoxState},
    PromptActions,
    WindowOps,
};

/// Persistent state for rendering [CommandBar].
pub struct CommandBarState<I: ApplicationInfo> {
    scrollback: ScrollbackState,
    searchdir: MoveDir1D,
    cmdtype: CommandType,
    tbox_cmd: TextBoxState<I>,
    tbox_search: TextBoxState<I>,
}

impl<I> CommandBarState<I>
where
    I: ApplicationInfo,
{
    /// Create state for a [CommandBar] widget.
    pub fn new(store: &mut Store<I>) -> Self {
        let buffer_cmd = store.load_buffer(I::content_of_command(CommandType::Command));
        let buffer_search = store.load_buffer(I::content_of_command(CommandType::Search));

        CommandBarState {
            scrollback: ScrollbackState::Pending,
            searchdir: MoveDir1D::Next,
            cmdtype: CommandType::Command,
            tbox_cmd: TextBoxState::new(buffer_cmd),
            tbox_search: TextBoxState::new(buffer_search),
        }
    }

    /// Get completion candidates from the command bar to show the user.
    pub fn get_completions(&self) -> Option<CompletionList> {
        self.deref().get_completions()
    }

    /// Set the type of command that the bar is being used for.
    pub fn set_type(&mut self, ct: CommandType, dir: MoveDir1D) {
        self.cmdtype = ct;
        self.searchdir = dir;
    }

    /// Reset the contents of the bar, and return the contents as an [EditRope].
    pub fn reset(&mut self) -> EditRope {
        self.scrollback = ScrollbackState::Pending;

        self.deref_mut().reset()
    }

    /// Reset the contents of the bar, and return the contents as a [String].
    pub fn reset_text(&mut self) -> String {
        self.reset().to_string()
    }
}

impl<I> Deref for CommandBarState<I>
where
    I: ApplicationInfo,
{
    type Target = TextBoxState<I>;

    fn deref(&self) -> &Self::Target {
        match self.cmdtype {
            CommandType::Command => &self.tbox_cmd,
            CommandType::Search => &self.tbox_search,
        }
    }
}

impl<I> DerefMut for CommandBarState<I>
where
    I: ApplicationInfo,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self.cmdtype {
            CommandType::Command => &mut self.tbox_cmd,
            CommandType::Search => &mut self.tbox_search,
        }
    }
}

impl<I> PromptActions<EditContext, Store<I>, I> for CommandBarState<I>
where
    I: ApplicationInfo,
{
    fn submit(
        &mut self,
        ctx: &EditContext,
        store: &mut Store<I>,
    ) -> EditResult<Vec<(Action<I>, EditContext)>, I> {
        let unfocus = CommandBarAction::Unfocus.into();

        let action = match self.cmdtype {
            CommandType::Command => {
                let rope = self.reset();
                let text = rope.to_string();

                store.set_last_cmd(rope);

                CommandAction::Execute(text).into()
            },
            CommandType::Search => {
                let text = self.reset().trim();

                store.set_last_search(text);

                let dir = MoveDirMod::Same;
                let count = Count::Contextual;
                let target = EditTarget::Search(SearchType::Regex, dir, count);

                EditorAction::Edit(Default::default(), target).into()
            },
        };

        Ok(vec![(unfocus, ctx.clone()), (action, ctx.clone())])
    }

    fn abort(
        &mut self,
        _empty: bool,
        ctx: &EditContext,
        store: &mut Store<I>,
    ) -> EditResult<Vec<(Action<I>, EditContext)>, I> {
        // We always unfocus currently, regardless of whether _empty=true.
        let act = Action::CommandBar(CommandBarAction::Unfocus);

        let text = self.reset().trim();

        match self.cmdtype {
            CommandType::Search => {
                store.set_aborted_search(text);
            },
            CommandType::Command => {
                store.set_aborted_cmd(text);
            },
        }

        Ok(vec![(act, ctx.clone())])
    }

    fn recall(
        &mut self,
        dir: &MoveDir1D,
        count: &Count,
        prefixed: bool,
        ctx: &EditContext,
        store: &mut Store<I>,
    ) -> EditResult<Vec<(Action<I>, EditContext)>, I> {
        let count = ctx.resolve(count);
        let rope = self.deref().get();

        let text = match self.cmdtype {
            CommandType::Search => {
                store.searches.recall(&rope, &mut self.scrollback, *dir, prefixed, count)
            },
            CommandType::Command => {
                store.commands.recall(&rope, &mut self.scrollback, *dir, prefixed, count)
            },
        };

        if let Some(text) = text {
            self.set_text(text);
        }

        Ok(vec![])
    }
}

impl<I> Promptable<EditContext, Store<I>, I> for CommandBarState<I>
where
    I: ApplicationInfo,
{
    fn prompt(
        &mut self,
        act: &PromptAction,
        ctx: &EditContext,
        store: &mut Store<I>,
    ) -> EditResult<Vec<(Action<I>, EditContext)>, I> {
        match act {
            PromptAction::Abort(empty) => self.abort(*empty, ctx, store),
            PromptAction::Recall(dir, count, prefixed) => {
                self.recall(dir, count, *prefixed, ctx, store)
            },
            PromptAction::Submit => self.submit(ctx, store),
        }
    }
}

/// Widget for rendering a command bar.
pub struct CommandBar<'a, I: ApplicationInfo> {
    focused: bool,
    message: Option<Span<'a>>,

    _pc: PhantomData<I>,
}

impl<'a, I> CommandBar<'a, I>
where
    I: ApplicationInfo,
{
    /// Create a new widget.
    pub fn new() -> Self {
        CommandBar { focused: false, message: None, _pc: PhantomData }
    }

    /// Indicate whether the widget is currently focused.
    pub fn focus(mut self, focused: bool) -> Self {
        self.focused = focused;
        self
    }

    /// Set a status string that will be displayed instead of the contents when the widget is not
    /// currently focused.
    pub fn status(mut self, msg: Option<Span<'a>>) -> Self {
        self.message = msg;
        self
    }
}

impl<'a, I> StatefulWidget for CommandBar<'a, I>
where
    I: ApplicationInfo,
{
    type State = CommandBarState<I>;

    fn render(self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        if self.focused {
            let prompt = match (state.cmdtype, state.searchdir) {
                (CommandType::Command, _) => ":",
                (CommandType::Search, MoveDir1D::Next) => "/",
                (CommandType::Search, MoveDir1D::Previous) => "?",
            };

            let tbox = TextBox::new().prompt(prompt).oneline();
            let tbox_state = match state.cmdtype {
                CommandType::Command => &mut state.tbox_cmd,
                CommandType::Search => &mut state.tbox_search,
            };

            tbox.render(area, buf, tbox_state);
        } else if let Some(span) = self.message {
            buf.set_span(area.left(), area.top(), &span, area.width);
        }
    }
}

impl<'a, I> Default for CommandBar<'a, I>
where
    I: ApplicationInfo,
{
    fn default() -> Self {
        CommandBar::new()
    }
}
