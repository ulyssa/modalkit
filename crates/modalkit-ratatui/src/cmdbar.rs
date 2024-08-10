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

use ratatui::{buffer::Buffer, layout::Rect, text::Span, widgets::StatefulWidget};

use modalkit::actions::{Action, CommandBarAction, PromptAction, Promptable};
use modalkit::editing::{
    application::ApplicationInfo,
    completion::CompletionList,
    context::{EditContext, Resolve},
    history::ScrollbackState,
    rope::EditRope,
    store::Store,
};
use modalkit::errors::EditResult;
use modalkit::prelude::*;

use super::{
    textbox::{TextBox, TextBoxState},
    PromptActions,
    WindowOps,
};

/// Persistent state for rendering [CommandBar].
pub struct CommandBarState<I: ApplicationInfo> {
    scrollback: ScrollbackState,
    prompt: String,
    action: Option<(Action<I>, EditContext)>,
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
            prompt: String::new(),
            action: None,
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
    pub fn set_type(&mut self, prompt: &str, ct: CommandType, act: &Action<I>, ctx: &EditContext) {
        self.prompt = prompt.into();
        self.action = Some((act.clone(), ctx.clone()));
        self.cmdtype = ct;
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
        let rope = self.reset().trim_end_matches(|c| c == '\n');
        store.registers.set_last_command(self.cmdtype, rope);

        let mut acts = vec![(CommandBarAction::Unfocus.into(), ctx.clone())];
        acts.extend(self.action.take());

        Ok(acts)
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
        store.registers.set_aborted_command(self.cmdtype, text);

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

        let hist = store.registers.get_command_history(self.cmdtype);
        let text = hist.recall(&rope, &mut self.scrollback, *dir, prefixed, count);

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
            let tbox = TextBox::new().prompt(&state.prompt).oneline();
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

#[cfg(test)]
mod tests {
    use super::*;
    use modalkit::editing::application::EmptyInfo;
    use modalkit::editing::context::EditContextBuilder;

    #[test]
    fn test_set_type_submit() {
        let mut store = Store::<EmptyInfo>::default();
        let mut cmdbar = CommandBarState::new(&mut store);

        // Verify that set_type() action and context are returned when the bar is submitted.
        let act = Action::Suspend;
        let ctx = EditContextBuilder::default().search_regex_dir(MoveDir1D::Previous).build();
        cmdbar.set_type(":", CommandType::Command, &act, &ctx);

        let res = cmdbar.submit(&EditContext::default(), &mut store).unwrap();
        assert_eq!(res.len(), 2);
        assert_eq!(res[0].0, Action::from(CommandBarAction::Unfocus));
        assert_eq!(res[0].1, EditContext::default());
        assert_eq!(res[1].0, act);
        assert_eq!(res[1].1, ctx);

        // Verify that the most recent set_type() call wins.
        let act1 = Action::Suspend;
        let ctx1 = EditContextBuilder::default().search_regex_dir(MoveDir1D::Previous).build();
        cmdbar.set_type(":", CommandType::Command, &act1, &ctx1);
        let act2 = Action::KeywordLookup;
        let ctx2 = EditContextBuilder::default().search_regex_dir(MoveDir1D::Next).build();
        cmdbar.set_type(":", CommandType::Command, &act2, &ctx2);

        let res = cmdbar.submit(&EditContext::default(), &mut store).unwrap();
        assert_eq!(res.len(), 2);
        assert_eq!(res[0].0, Action::from(CommandBarAction::Unfocus));
        assert_eq!(res[0].1, EditContext::default());
        assert_eq!(res[1].0, act2);
        assert_eq!(res[1].1, ctx2);
    }
}
