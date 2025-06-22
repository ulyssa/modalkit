//! Traits to support consuming [editor_types].
//!
//! The traits contained here can be implemented to indicate that an object is capable of
//! processing the associated action from the [editor_types] crate, and can help drive
//! implementing new variants added over time.
pub use editor_types::{
    Action,
    CommandAction,
    CommandBarAction,
    CursorAction,
    EditAction,
    EditorAction,
    HistoryAction,
    InsertTextAction,
    MacroAction,
    PromptAction,
    SelectionAction,
    TabAction,
    WindowAction,
};

use crate::{
    commands::{Command, CommandMachine},
    editing::application::*,
    editing::context::{EditContext, Resolve},
    editing::store::RegisterStore,
    errors::{EditError, EditResult, UIResult},
    prelude::*,
};

/// An object capable of performing editing operations.
pub trait EditorActions<C, S, I>
where
    I: ApplicationInfo,
{
    /// Perform an editing operation over the targeted text.
    fn edit(
        &mut self,
        action: &EditAction,
        target: &EditTarget,
        ctx: &C,
        store: &mut S,
    ) -> EditResult<EditInfo, I>;

    /// Create or update a cursor mark based on the leader's cursor position.
    fn mark(&mut self, name: Mark, ctx: &C, store: &mut S) -> EditResult<EditInfo, I>;

    /// Complete the text before the cursor group leader.
    fn complete(
        &mut self,
        style: &CompletionStyle,
        comptype: &CompletionType,
        display: &CompletionDisplay,
        ctx: &C,
        store: &mut S,
    ) -> EditResult<EditInfo, I>;

    /// Insert text relative to the current cursor position.
    fn insert_text(
        &mut self,
        act: &InsertTextAction,
        ctx: &C,
        store: &mut S,
    ) -> EditResult<EditInfo, I>;

    /// Modify the current selection.
    fn selection_command(
        &mut self,
        act: &SelectionAction,
        ctx: &C,
        store: &mut S,
    ) -> EditResult<EditInfo, I>;

    /// Perform an action over a cursor group.
    fn cursor_command(
        &mut self,
        act: &CursorAction,
        ctx: &C,
        store: &mut S,
    ) -> EditResult<EditInfo, I>;

    /// Move to a different point in the buffer's editing history.
    fn history_command(
        &mut self,
        act: &HistoryAction,
        ctx: &C,
        store: &mut S,
    ) -> EditResult<EditInfo, I>;
}

/// Trait for objects which can process [EditorActions](EditorAction).
pub trait Editable<C, S, I>
where
    I: ApplicationInfo,
{
    /// Execute an editor action.
    fn editor_command(
        &mut self,
        act: &EditorAction,
        ctx: &C,
        store: &mut S,
    ) -> EditResult<EditInfo, I>;
}

/// Trait for objects which can process [CommandActions](CommandAction).
pub trait Commandable<C, I>
where
    C: Command,
    I: ApplicationInfo,
{
    /// Execute a command action.
    fn command(
        &mut self,
        action: &CommandAction,
        ctx: &C::Context,
        rstore: &mut RegisterStore,
    ) -> UIResult<Vec<(C::Action, C::Context)>, I>;
}

impl<C, I> Commandable<C, I> for CommandMachine<C>
where
    C: Command<Action = Action<I>, Context = EditContext>,
    I: ApplicationInfo,
{
    fn command(
        &mut self,
        action: &CommandAction,
        ctx: &C::Context,
        rstore: &mut RegisterStore,
    ) -> UIResult<Vec<(Action<I>, C::Context)>, I> {
        match action {
            CommandAction::Execute(count) => {
                let count = ctx.resolve(count);
                let cmd = rstore.get_last_cmd().to_string();
                let msg = format!(":{cmd}");
                let msg = Action::ShowInfoMessage(msg.into());
                let mut acts = vec![(msg, ctx.clone())];

                for _ in 0..count {
                    let mut res = self.input_cmd(cmd.as_str(), ctx.clone())?;

                    acts.append(&mut res);
                }

                Ok(acts)
            },
            CommandAction::Run(cmd) => {
                rstore.set_last_cmd(cmd.as_str());
                let acts = self.input_cmd(cmd, ctx.clone())?;

                Ok(acts)
            },

            a => {
                let msg = format!("unknown command action: {a:?}");
                let err = EditError::Unimplemented(msg);
                Err(err.into())
            },
        }
    }
}

/// Trait for objects that can process [PromptAction] values.
pub trait Promptable<C, S, I>
where
    I: ApplicationInfo,
{
    /// Execute a prompt action.
    fn prompt(
        &mut self,
        act: &PromptAction,
        ctx: &C,
        store: &mut S,
    ) -> EditResult<Vec<(Action<I>, C)>, I>;
}

/// Trait counting tabs withing an object.
pub trait TabCount {
    /// Number of currently open tabs.
    fn tabs(&self) -> usize;
}

/// Trait for objects that contain tabbed content.
pub trait TabContainer<C, S, I>: TabCount
where
    I: ApplicationInfo,
{
    /// Execute a tab action.
    fn tab_command(&mut self, act: &TabAction<I>, ctx: &C, store: &mut S) -> UIResult<EditInfo, I>;
}

/// Trait for counting windows within an object.
pub trait WindowCount {
    /// Number of currently open windows.
    fn windows(&self) -> usize;
}

/// Trait for objects that contain windows.
pub trait WindowContainer<C, S, I>: WindowCount
where
    I: ApplicationInfo,
{
    /// Execute a window action.
    fn window_command(
        &mut self,
        action: &WindowAction<I>,
        ctx: &C,
        store: &mut S,
    ) -> UIResult<EditInfo, I>;
}

/// Trait for objects that can move through a [PositionList].
pub trait Jumpable<C, I>
where
    I: ApplicationInfo,
{
    /// Move through a [PositionList] in [MoveDir1D] direction `count` times.
    ///
    /// The result indicates any leftover `count`.
    fn jump(
        &mut self,
        list: PositionList,
        dir: MoveDir1D,
        count: usize,
        ctx: &C,
    ) -> UIResult<usize, I>;
}

/// Trait for objects that can be scrolled.
pub trait Scrollable<C, S, I>
where
    I: ApplicationInfo,
{
    /// Scroll the viewable content in this object.
    fn scroll(&mut self, style: &ScrollStyle, ctx: &C, store: &mut S) -> EditResult<EditInfo, I>;
}

/// Trait for objects that can be searched.
pub trait Searchable<C, S, I>
where
    I: ApplicationInfo,
{
    /// Search for the [*n*<sup>th</sup>](Count) result in [MoveDirMod] direction.
    fn search(
        &mut self,
        dir: MoveDirMod,
        count: Count,
        ctx: &C,
        store: &mut S,
    ) -> UIResult<EditInfo, I>;
}
