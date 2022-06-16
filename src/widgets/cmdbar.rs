use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};

use tui::{buffer::Buffer, layout::Rect, text::Span, widgets::StatefulWidget};

use crate::input::InputContext;

use crate::editing::store::{SharedStore, Store};

use crate::editing::base::{
    Action,
    Application,
    CommandType,
    Count,
    EditContext,
    EditTarget,
    MoveDir1D,
    MoveDirMod,
    SearchType,
    Specifier,
};

use super::{
    textbox::{TextBox, TextBoxState},
    Submitable,
};

pub struct CommandBarState<C: EditContext + InputContext, P: Application> {
    pub cmdtype: CommandType,
    pub tbox: TextBoxState<C, P>,
}

impl<C, P> CommandBarState<C, P>
where
    C: EditContext + InputContext,
    P: Application,
{
    pub fn new(store: SharedStore<C, P>) -> Self {
        let buffer = Store::new_buffer(&store);

        CommandBarState {
            cmdtype: CommandType::Command,
            tbox: TextBoxState::new(buffer),
        }
    }

    pub fn set_type(&mut self, ct: CommandType) {
        self.cmdtype = ct;
    }

    pub fn reset(&mut self) {
        let _ = self.tbox.reset_text();
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

impl<C, P> Submitable<Action<P>, C> for CommandBarState<C, P>
where
    C: Default + EditContext + InputContext,
    P: Application,
{
    fn submit(&mut self, _: &mut C) -> Option<Action<P>> {
        let text = self.tbox.reset_text();

        match self.cmdtype {
            CommandType::Command => Some(Action::CommandRun(text)),
            CommandType::Search(_) => {
                // XXX: need to use dir and text!

                let target =
                    EditTarget::Search(SearchType::Regex, MoveDirMod::Same, Count::Contextual);
                let action = Action::Edit(Specifier::Contextual, target);

                Some(action)
            },
        }
    }
}

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
    pub fn new() -> Self {
        CommandBar { focused: false, message: None, _pc: PhantomData }
    }

    pub fn focus(mut self, focused: bool) -> Self {
        self.focused = focused;
        self
    }

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
                CommandType::Search(MoveDir1D::Next) => "/",
                CommandType::Search(MoveDir1D::Previous) => "?",
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
