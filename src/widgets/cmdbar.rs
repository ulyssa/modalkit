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

use crate::input::InputContext;

use crate::editing::{
    history::ScrollbackState,
    rope::EditRope,
    store::{SharedStore, Store},
};

use crate::editing::action::{
    Action,
    CommandAction,
    CommandBarAction,
    EditAction,
    EditResult,
    PromptAction,
    Promptable,
};

use crate::editing::base::{
    Application,
    CommandType,
    Count,
    EditContext,
    EditTarget,
    MoveDir1D,
    MoveDirMod,
    SearchType,
};

use super::{
    textbox::{TextBox, TextBoxState},
    PromptActions,
};

/// Persistent state for rendering [CommandBar].
pub struct CommandBarState<C: EditContext + InputContext, P: Application> {
    scrollback: ScrollbackState,
    cmdtype: CommandType,
    tbox: TextBoxState<C, P>,
    store: SharedStore<C, P>,
}

impl<C, P> CommandBarState<C, P>
where
    C: EditContext + InputContext,
    P: Application,
{
    /// Create state for a [CommandBar] widget.
    pub fn new(store: SharedStore<C, P>) -> Self {
        let buffer = Store::new_buffer(&store);

        CommandBarState {
            scrollback: ScrollbackState::Pending,
            cmdtype: CommandType::Command,
            tbox: TextBoxState::new(buffer),
            store,
        }
    }

    /// Set the type of command that the bar is being used for.
    pub fn set_type(&mut self, ct: CommandType) {
        self.cmdtype = ct;
    }

    /// Reset the contents of the bar, and return the contents as an [EditRope].
    pub fn reset(&mut self) -> EditRope {
        self.scrollback = ScrollbackState::Pending;

        self.tbox.reset()
    }

    /// Reset the contents of the bar, and return the contents as a [String].
    pub fn reset_text(&mut self) -> String {
        self.reset().to_string()
    }
}

impl<C, P> Deref for CommandBarState<C, P>
where
    C: EditContext + InputContext,
    P: Application,
{
    type Target = TextBoxState<C, P>;

    fn deref(&self) -> &Self::Target {
        &self.tbox
    }
}

impl<C, P> DerefMut for CommandBarState<C, P>
where
    C: EditContext + InputContext,
    P: Application,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.tbox
    }
}

impl<C, P> PromptActions<Action<P>, C> for CommandBarState<C, P>
where
    C: Default + EditContext + InputContext,
    P: Application,
{
    fn submit(&mut self, ctx: &C) -> EditResult<Vec<(Action<P>, C)>> {
        let unfocus = CommandBarAction::Unfocus.into();

        let action = match self.cmdtype {
            CommandType::Command => {
                let rope = self.reset();
                let text = rope.to_string();

                Store::set_last_cmd(rope, &self.store);

                CommandAction::Execute(text).into()
            },
            CommandType::Search(_, _) => {
                let text = self.reset().trim();

                Store::set_last_search(text, &self.store);

                let target =
                    EditTarget::Search(SearchType::Regex, MoveDirMod::Same, Count::Contextual);

                Action::Edit(EditAction::Motion.into(), target)
            },
        };

        Ok(vec![(unfocus, ctx.clone()), (action, ctx.clone())])
    }

    fn abort(&mut self, _empty: bool, ctx: &C) -> EditResult<Vec<(Action<P>, C)>> {
        // We always unfocus currently, regardless of whether _empty=true.
        let act = Action::CommandBar(CommandBarAction::Unfocus).into();

        let text = self.reset().trim();

        match self.cmdtype {
            CommandType::Search(_, _) => {
                Store::set_aborted_search(text, &self.store);
            },
            CommandType::Command => {
                Store::set_aborted_cmd(text, &self.store);
            },
        }

        Ok(vec![(act, ctx.clone())])
    }

    fn recall(
        &mut self,
        dir: &MoveDir1D,
        count: &Count,
        ctx: &C,
    ) -> EditResult<Vec<(Action<P>, C)>> {
        let count = ctx.resolve(count);
        let rope = self.tbox.get();

        let text = match self.cmdtype {
            CommandType::Search(_, _) => {
                let mut locked = self.store.write().unwrap();
                locked.searches.recall(&rope, &mut self.scrollback, *dir, count)
            },
            CommandType::Command => {
                let mut locked = self.store.write().unwrap();
                locked.commands.recall(&rope, &mut self.scrollback, *dir, count)
            },
        };

        if let Some(text) = text {
            self.set_text(text);
        }

        Ok(vec![])
    }
}

impl<C, P> Promptable<Action<P>, C> for CommandBarState<C, P>
where
    C: Default + EditContext + InputContext,
    P: Application,
{
    fn prompt(&mut self, act: PromptAction, ctx: &C) -> EditResult<Vec<(Action<P>, C)>> {
        match act {
            PromptAction::Abort(empty) => self.abort(empty, ctx),
            PromptAction::Recall(dir, count) => self.recall(&dir, &count, ctx),
            PromptAction::Submit => self.submit(ctx),
        }
    }
}

/// Widget for rendering a command bar.
pub struct CommandBar<'a, C: EditContext + InputContext, P: Application> {
    focused: bool,
    message: Option<Span<'a>>,

    _pc: PhantomData<(C, P)>,
}

impl<'a, C, P> CommandBar<'a, C, P>
where
    C: EditContext + InputContext,
    P: Application,
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

impl<'a, C, P> StatefulWidget for CommandBar<'a, C, P>
where
    C: EditContext + InputContext,
    P: Application,
{
    type State = CommandBarState<C, P>;

    fn render(self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        if self.focused {
            let prompt = match state.cmdtype {
                CommandType::Command => ":",
                CommandType::Search(MoveDir1D::Next, _) => "/",
                CommandType::Search(MoveDir1D::Previous, _) => "?",
            };

            let tbox = TextBox::new().prompt(prompt);

            tbox.render(area, buf, &mut state.tbox);
        } else if let Some(span) = self.message {
            buf.set_span(area.left(), area.top(), &span, area.width);
        }
    }
}

impl<'a, C, P> Default for CommandBar<'a, C, P>
where
    C: EditContext + InputContext,
    P: Application,
{
    fn default() -> Self {
        CommandBar::new()
    }
}
