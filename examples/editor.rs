use std::collections::hash_map::{Entry, HashMap};
use std::collections::VecDeque;
use std::io::{stdout, Stdout};
use std::ops::{Deref, DerefMut};
use std::sync::{Arc, RwLock};
use std::time::Duration;

use modalkit::crossterm::{
    self,
    event::{poll, read, Event},
    execute,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen},
};

use modalkit::tui::{
    backend::CrosstermBackend,
    buffer::Buffer,
    layout::Rect,
    style::{Color, Style},
    text::Span,
    widgets::Paragraph,
    Terminal,
};

use modalkit::{
    editing::{
        action::{
            Action,
            Commandable,
            EditError,
            EditInfo,
            EditResult,
            Editable,
            EditorAction,
            Jumpable,
            PromptAction,
            Promptable,
            Scrollable,
            Searchable,
            TabContainer,
            TabCount,
            UIError,
            UIResult,
            WindowContainer,
        },
        application::{
            ApplicationContentId,
            ApplicationInfo,
            ApplicationStore,
            ApplicationWindowId,
        },
        base::{
            CloseFlags,
            Count,
            MoveDir1D,
            MoveDirMod,
            PositionList,
            RepeatType,
            ScrollStyle,
            WordStyle,
        },
        buffer::EditBuffer,
        context::{EditContext, Resolve},
        key::KeyManager,
        store::Store,
    },
    env::{
        mixed::{MixedBindings, MixedChoice, MixedContext},
        vim::command::VimCommandMachine,
    },
    input::{bindings::BindingMachine, key::TerminalKey},
    widgets::{
        cmdbar::CommandBarState,
        screen::{Screen, ScreenState},
        textbox::TextBoxState,
        TermOffset,
        TerminalCursor,
        TerminalExtOps,
        Window,
        WindowOps,
    },
};

type Context = MixedContext<EditorInfo>;

struct EditorStore {
    filenames: HashMap<String, usize>,
    fileindex: usize,
}

impl Default for EditorStore {
    fn default() -> Self {
        EditorStore { filenames: HashMap::default(), fileindex: 0 }
    }
}

impl ApplicationStore for EditorStore {}

struct EditorBox(TextBoxState<EditorInfo>);

impl From<TextBoxState<EditorInfo>> for EditorBox {
    fn from(tbox: TextBoxState<EditorInfo>) -> Self {
        EditorBox(tbox)
    }
}

impl Deref for EditorBox {
    type Target = TextBoxState<EditorInfo>;

    fn deref(&self) -> &Self::Target {
        return &self.0;
    }
}

impl DerefMut for EditorBox {
    fn deref_mut(&mut self) -> &mut Self::Target {
        return &mut self.0;
    }
}

impl TerminalCursor for EditorBox {
    fn get_term_cursor(&self) -> Option<TermOffset> {
        self.0.get_term_cursor()
    }
}

impl WindowOps<EditorInfo> for EditorBox {
    fn dup(&self, store: &mut Store<EditorInfo>) -> Self {
        return EditorBox(self.0.dup(store));
    }

    fn close(&mut self, flags: CloseFlags, store: &mut Store<EditorInfo>) -> bool {
        return self.0.close(flags, store);
    }

    fn draw(&mut self, area: Rect, buf: &mut Buffer, focused: bool, store: &mut Store<EditorInfo>) {
        self.0.draw(area, buf, focused, store)
    }

    fn get_cursor_word(&self, style: &WordStyle) -> Option<String> {
        self.0.get_cursor_word(style)
    }

    fn get_selected_word(&self) -> Option<String> {
        self.0.get_selected_word()
    }
}

fn load_file(name: String, store: &mut Store<EditorInfo>) -> UIResult<EditorBox, EditorInfo> {
    let index = store
        .application
        .filenames
        .get(&name)
        .cloned()
        .unwrap_or(store.application.fileindex);

    let window = match store.buffers.entry(EditorContentId::File(index)) {
        Entry::Vacant(v) => {
            let s = std::fs::read_to_string(name.as_str())?;
            let buffer = EditBuffer::from_str(v.key().clone(), s.as_str());
            let buffer = Arc::new(RwLock::new(buffer));

            v.insert(buffer.clone());

            TextBoxState::new(buffer).into()
        },
        Entry::Occupied(o) => TextBoxState::new(o.get().clone()).into(),
    };

    if index == store.application.fileindex {
        store.application.fileindex += 1;
    }

    return Ok(window);
}

impl Window<EditorInfo> for EditorBox {
    fn id(&self) -> EditorContentId {
        return self.0.buffer().read().unwrap().id();
    }

    fn open(id: EditorContentId, store: &mut Store<EditorInfo>) -> UIResult<Self, EditorInfo> {
        match id {
            id @ (EditorContentId::Scratch | EditorContentId::File(_)) => {
                let buffer = store.load_buffer(id);

                Ok(TextBoxState::new(buffer).into())
            },
            EditorContentId::Command => {
                let msg = "Cannot open command bar in a window";

                Err(UIError::Failure(msg.into()))
            },
        }
    }

    fn find(name: String, store: &mut Store<EditorInfo>) -> UIResult<Self, EditorInfo> {
        return load_file(name, store);
    }

    fn posn(index: usize, store: &mut Store<EditorInfo>) -> UIResult<Self, EditorInfo> {
        let id = EditorContentId::File(index);

        match store.buffers.entry(id) {
            Entry::Occupied(o) => {
                let ebuf = o.get().clone();
                let tbox = TextBoxState::new(ebuf);

                Ok(tbox.into())
            },
            Entry::Vacant(_) => {
                let msg = "";
                let err = UIError::Failure(msg.into());

                Err(err)
            },
        }
    }
}

impl<C> Editable<C, Store<EditorInfo>, EditorInfo> for EditorBox
where
    C: EditContext,
{
    fn editor_command(
        &mut self,
        act: &EditorAction,
        ctx: &C,
        store: &mut Store<EditorInfo>,
    ) -> EditResult<EditInfo, EditorInfo> {
        self.0.editor_command(act, ctx, store)
    }
}

impl<C: EditContext> Jumpable<C, EditorInfo> for EditorBox {
    fn jump(
        &mut self,
        list: PositionList,
        dir: MoveDir1D,
        count: usize,
        ctx: &C,
    ) -> UIResult<usize, EditorInfo> {
        self.0.jump(list, dir, count, ctx)
    }
}

impl<C: EditContext> Promptable<C, Store<EditorInfo>, EditorInfo> for EditorBox {
    fn prompt(
        &mut self,
        act: &PromptAction,
        ctx: &C,
        store: &mut Store<EditorInfo>,
    ) -> EditResult<Vec<(Action<EditorInfo>, C)>, EditorInfo> {
        self.0.prompt(act, ctx, store)
    }
}

impl<C: EditContext> Scrollable<C, Store<EditorInfo>, EditorInfo> for EditorBox {
    fn scroll(
        &mut self,
        style: &ScrollStyle,
        ctx: &C,
        store: &mut Store<EditorInfo>,
    ) -> EditResult<EditInfo, EditorInfo> {
        self.0.scroll(style, ctx, store)
    }
}

impl<C: EditContext> Searchable<C, Store<EditorInfo>, EditorInfo> for EditorBox {
    fn search(
        &mut self,
        dir: MoveDirMod,
        count: Count,
        ctx: &C,
        store: &mut Store<EditorInfo>,
    ) -> UIResult<EditInfo, EditorInfo> {
        self.0.search(dir, count, ctx, store)
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
enum EditorContentId {
    Command,
    File(usize),
    Scratch,
}

impl ApplicationWindowId for EditorContentId {}
impl ApplicationContentId for EditorContentId {}

#[derive(Clone, Debug, Eq, PartialEq)]
enum EditorInfo {}

impl ApplicationInfo for EditorInfo {
    type Error = String;
    type Action = ();
    type Store = EditorStore;
    type WindowId = EditorContentId;
    type ContentId = EditorContentId;
}

struct Editor {
    terminal: Terminal<CrosstermBackend<Stdout>>,
    bindings: KeyManager<TerminalKey, Action<EditorInfo>, RepeatType, Context>,
    actstack: VecDeque<(Action<EditorInfo>, Context)>,
    cmds: VimCommandMachine<Context, EditorInfo>,
    screen: ScreenState<EditorBox, EditorInfo>,
    store: Store<EditorInfo>,
}

impl Editor {
    pub fn new(env: MixedChoice) -> Result<Self, std::io::Error> {
        crossterm::terminal::enable_raw_mode()?;

        let mut stdout = stdout();

        crossterm::execute!(stdout, EnterAlternateScreen)?;

        let backend = CrosstermBackend::new(stdout);
        let terminal = Terminal::new(backend)?;

        let mut store = Store::default();
        let bindings = MixedBindings::<TerminalKey, EditorInfo>::from(env);
        let bindings = KeyManager::new(bindings);
        let cmds = VimCommandMachine::default();

        let buf = store.load_buffer(EditorContentId::Scratch);
        let win = EditorBox(TextBoxState::new(buf));
        let cmd = CommandBarState::new(EditorContentId::Command, &mut store);
        let screen = ScreenState::new(win, cmd);

        let actstack = VecDeque::new();

        Ok(Editor { terminal, bindings, actstack, cmds, screen, store })
    }

    pub fn run(&mut self) -> Result<(), std::io::Error> {
        self.terminal.clear()?;

        while self.screen.tabs() != 0 {
            let key = self.step()?;

            self.bindings.input_key(key);

            let mut keyskip = false;

            while let Some((action, ctx)) = self.action_pop(keyskip) {
                match self.action_run(action, ctx) {
                    Ok(None) => {
                        // Continue processing.
                        continue;
                    },
                    Ok(Some(info)) => {
                        self.screen.push_info(info);

                        // Continue processing; we'll redraw later.
                        continue;
                    },
                    Err(e) => {
                        self.screen.push_error(e);

                        // Skip processing any more keypress Actions until the next key.
                        keyskip = true;
                        continue;
                    },
                }
            }
        }

        crossterm::terminal::disable_raw_mode()?;
        execute!(self.terminal.backend_mut(), LeaveAlternateScreen)?;
        self.terminal.show_cursor()?;

        return Ok(());
    }

    fn step(&mut self) -> Result<TerminalKey, std::io::Error> {
        loop {
            self.redraw(false)?;

            if !poll(Duration::from_millis(500))? {
                continue;
            }

            match read()? {
                Event::Key(ke) => {
                    return Ok(TerminalKey::from(ke));
                },
                Event::Mouse(_) => {
                    // Do nothing for now.
                },
                Event::FocusGained | Event::FocusLost => {
                    // Do nothing for now.
                },
                Event::Resize(_, _) => {
                    // We'll redraw for the new size next time step() is called.
                },
                Event::Paste(_) => {
                    // Do nothing for now.
                },
            }
        }
    }

    fn action_prepend(&mut self, acts: Vec<(Action<EditorInfo>, Context)>) {
        let mut acts = VecDeque::from(acts);
        acts.append(&mut self.actstack);
        self.actstack = acts;
    }

    fn action_pop(&mut self, keyskip: bool) -> Option<(Action<EditorInfo>, Context)> {
        if let res @ Some(_) = self.actstack.pop_front() {
            return res;
        }

        if keyskip {
            return None;
        } else {
            return self.bindings.pop();
        }
    }

    fn action_run(
        &mut self,
        action: Action<EditorInfo>,
        ctx: Context,
    ) -> UIResult<EditInfo, EditorInfo> {
        let info = match action {
            // Do nothing.
            Action::Application(()) => None,
            Action::NoOp => None,

            // Simple delegations.
            Action::CommandBar(act) => self.screen.command_bar(&act, &ctx)?,
            Action::Editor(act) => self.screen.editor_command(&act, &ctx, &mut self.store)?,
            Action::Macro(act) => self.bindings.macro_command(&act, &ctx, &mut self.store)?,
            Action::Scroll(style) => self.screen.scroll(&style, &ctx, &mut self.store)?,
            Action::Search(dir, count) => self.screen.search(dir, count, &ctx, &mut self.store)?,
            Action::Suspend => self.terminal.program_suspend()?,
            Action::Tab(cmd) => self.screen.tab_command(&cmd, &ctx, &mut self.store)?,
            Action::Window(cmd) => self.screen.window_command(&cmd, &ctx, &mut self.store)?,

            Action::Jump(l, dir, count) => {
                let _ = self.screen.jump(l, dir, ctx.resolve(&count), &ctx)?;

                None
            },

            // UI actions.
            Action::RedrawScreen => {
                self.screen.clear_message();
                self.redraw(true)?;

                None
            },

            // Actions that create more Actions.
            Action::Prompt(act) => {
                let acts = self.screen.prompt(&act, &ctx, &mut self.store)?;
                self.action_prepend(acts);

                None
            },
            Action::Command(act) => {
                let acts = self.cmds.command(&act, &ctx)?;
                self.action_prepend(acts);

                None
            },
            Action::Repeat(rt) => {
                self.bindings.repeat(rt, Some(ctx));

                None
            },

            Action::KeywordLookup => {
                // XXX: implement
                None
            },

            // Handle non-exhaustive pattern.
            _ => {
                let msg = format!("Unknown action: {:?}", action);
                let err = EditError::Unimplemented(msg);

                return Err(err.into());
            },
        };

        return Ok(info);
    }

    fn redraw(&mut self, full: bool) -> Result<(), std::io::Error> {
        let modestr = self.bindings.showmode();
        let cursor = self.bindings.get_cursor_indicator();
        let sstate = &mut self.screen;
        let store = &mut self.store;
        let term = &mut self.terminal;

        if full {
            term.clear()?;
        }

        term.draw(|f| {
            let area = f.size();

            let screen = Screen::new(store).showmode(modestr);
            f.render_stateful_widget(screen, area, sstate);

            if let Some((cx, cy)) = sstate.get_term_cursor() {
                if let Some(c) = cursor {
                    let style = Style::default().fg(Color::Green);
                    let span = Span::styled(c.to_string(), style);
                    let para = Paragraph::new(span);
                    let inner = Rect::new(cx, cy, 1, 1);
                    f.render_widget(para, inner)
                }
                f.set_cursor(cx, cy);
            }
        })?;

        Ok(())
    }
}

fn main() -> Result<(), std::io::Error> {
    let mut args = std::env::args();
    let _ = args.next();

    let env = match args.next() {
        Some(arg) => {
            match arg.as_str().trim() {
                "e" | "emacs" => MixedChoice::Emacs,
                "v" | "vim" => MixedChoice::Vim,
                m => panic!("Unknown environment: {:?}", m),
            }
        },
        None => MixedChoice::Vim,
    };

    let mut ed = Editor::new(env)?;

    return ed.run();
}
