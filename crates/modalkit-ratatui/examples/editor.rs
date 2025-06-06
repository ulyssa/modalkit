use std::collections::hash_map::{Entry, HashMap};
use std::collections::VecDeque;
use std::fs::{DirEntry, File, FileType};
use std::io::{stdout, Stdout};
use std::path::Path;
use std::sync::{Arc, RwLock};
use std::time::Duration;

use modalkit::crossterm::{
    self,
    event::{poll, read, Event, KeyboardEnhancementFlags, PushKeyboardEnhancementFlags},
    execute,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen},
};

use ratatui::{
    backend::CrosstermBackend,
    buffer::Buffer,
    layout::Rect,
    style::{Modifier as StyleModifier, Style},
    text::{Line, Span, Text},
    Terminal,
};

use modalkit::{
    actions::{
        Action,
        Commandable,
        Editable,
        EditorAction,
        Jumpable,
        PromptAction,
        Promptable,
        Scrollable,
        Searchable,
        TabContainer,
        TabCount,
        WindowAction,
        WindowContainer,
    },
    editing::{
        application::{
            ApplicationContentId,
            ApplicationInfo,
            ApplicationStore,
            ApplicationWindowId,
        },
        buffer::EditBuffer,
        completion::{Completer, CompletionList},
        context::{EditContext, Resolve},
        cursor::Cursor,
        key::KeyManager,
        rope::EditRope,
        store::Store,
    },
    env::mixed::{MixedBindings, MixedChoice},
    env::vim::command::{complete_cmdbar, VimCommandMachine},
    errors::{EditError, EditResult, UIError, UIResult},
    key::TerminalKey,
    keybindings::{dialog::Pager, BindingMachine},
    prelude::*,
};

use modalkit_ratatui::{
    cmdbar::CommandBarState,
    list::{ListCursor, ListItem, ListState},
    render_cursor,
    screen::{Screen, ScreenState},
    textbox::TextBoxState,
    TermOffset,
    TerminalCursor,
    TerminalExtOps,
    Window,
    WindowOps,
};

#[derive(Clone)]
struct DirectoryItem {
    ftype: FileType,
    entry: String,
}

impl ToString for DirectoryItem {
    fn to_string(&self) -> String {
        return self.entry.clone();
    }
}

impl Promptable<EditContext, Store<EditorInfo>, EditorInfo> for DirectoryItem {
    fn prompt(
        &mut self,
        act: &PromptAction,
        ctx: &EditContext,
        _: &mut Store<EditorInfo>,
    ) -> EditResult<Vec<(Action<EditorInfo>, EditContext)>, EditorInfo> {
        if let PromptAction::Submit = act {
            let target = OpenTarget::Name(self.entry.clone());
            let act = WindowAction::Switch(target);

            Ok(vec![(act.into(), ctx.clone())])
        } else {
            let msg = format!("Cannot perform {:?} inside a list", act);
            let err = EditError::Unimplemented(msg);

            Err(err)
        }
    }
}

impl DirectoryItem {
    fn new(entry: DirEntry) -> Option<Self> {
        let ftype = entry.file_type().ok()?;
        let entry = entry.path().to_str()?.to_owned();

        Some(Self { ftype, entry })
    }
}

impl ListItem<EditorInfo> for DirectoryItem {
    fn show(
        &self,
        selected: bool,
        _: &ViewportContext<ListCursor>,
        _: &mut Store<EditorInfo>,
    ) -> Text {
        let mut style = Style::default();

        if selected {
            style = style.add_modifier(StyleModifier::REVERSED);
        }

        let suffix = if self.ftype.is_dir() {
            Span::from("/")
        } else {
            Span::from("")
        };

        let entry = Span::styled(self.entry.as_str(), style);
        let line = Line::from(vec![entry, suffix]);

        return Text::from(line);
    }
}

#[derive(Default)]
struct EditorStore {
    cmds: VimCommandMachine<EditorInfo>,
    filenames: HashMap<String, usize>,
    fileindex: usize,
}

impl ApplicationStore for EditorStore {}

enum EditorWindow {
    Text(TextBoxState<EditorInfo>),
    Listing(ListState<DirectoryItem, EditorInfo>),
}

impl From<ListState<DirectoryItem, EditorInfo>> for EditorWindow {
    fn from(tbox: ListState<DirectoryItem, EditorInfo>) -> Self {
        EditorWindow::Listing(tbox)
    }
}

impl From<TextBoxState<EditorInfo>> for EditorWindow {
    fn from(tbox: TextBoxState<EditorInfo>) -> Self {
        EditorWindow::Text(tbox)
    }
}

impl TerminalCursor for EditorWindow {
    fn get_term_cursor(&self) -> Option<TermOffset> {
        match self {
            EditorWindow::Text(tbox) => tbox.get_term_cursor(),
            EditorWindow::Listing(ls) => ls.get_term_cursor(),
        }
    }
}

impl WindowOps<EditorInfo> for EditorWindow {
    fn dup(&self, store: &mut Store<EditorInfo>) -> Self {
        match self {
            EditorWindow::Text(tbox) => tbox.dup(store).into(),
            EditorWindow::Listing(ls) => ls.dup(store).into(),
        }
    }

    fn close(&mut self, flags: CloseFlags, store: &mut Store<EditorInfo>) -> bool {
        match self {
            EditorWindow::Text(tbox) => tbox.close(flags, store),
            EditorWindow::Listing(ls) => ls.close(flags, store),
        }
    }

    fn write(
        &mut self,
        path: Option<&str>,
        flags: WriteFlags,
        store: &mut Store<EditorInfo>,
    ) -> UIResult<EditInfo, EditorInfo> {
        match self {
            EditorWindow::Text(tbox) => {
                let buffer = tbox.buffer();
                let buffer = buffer.read().unwrap();

                let id = match buffer.id() {
                    EditorContentId::File(id) => id,
                    _ => {
                        let msg = "Only file buffers can be written";
                        let err = UIError::Failure(msg.into());

                        return Err(err);
                    },
                };

                if tbox.is_readonly() && !flags.contains(WriteFlags::FORCE) {
                    return Err(EditError::ReadOnly.into());
                }

                let path = if let Some(p) = path {
                    p
                } else {
                    let val = store.application.filenames.iter().find(|(_, index)| id == **index);

                    if let Some((path, _)) = val {
                        path.as_str()
                    } else {
                        let msg = "Could not determine filename to write to";
                        let err = UIError::Failure(msg.into());

                        return Err(err);
                    }
                };

                buffer.get().write_to(File::create(path)?)?;

                Ok(None)
            },
            EditorWindow::Listing(_) => {
                let msg = "Cannot write directory listing";
                let err = UIError::Failure(msg.into());

                Err(err)
            },
        }
    }

    fn draw(&mut self, area: Rect, buf: &mut Buffer, focused: bool, store: &mut Store<EditorInfo>) {
        match self {
            EditorWindow::Text(tbox) => tbox.draw(area, buf, focused, store),
            EditorWindow::Listing(ls) => ls.draw(area, buf, focused, store),
        }
    }

    fn get_completions(&self) -> Option<CompletionList> {
        match self {
            EditorWindow::Text(tbox) => tbox.get_completions(),
            EditorWindow::Listing(ls) => ls.get_completions(),
        }
    }

    fn get_cursor_word(&self, style: &WordStyle) -> Option<String> {
        match self {
            EditorWindow::Text(tbox) => tbox.get_cursor_word(style),
            EditorWindow::Listing(ls) => ls.get_cursor_word(style),
        }
    }

    fn get_selected_word(&self) -> Option<String> {
        match self {
            EditorWindow::Text(tbox) => tbox.get_selected_word(),
            EditorWindow::Listing(ls) => ls.get_selected_word(),
        }
    }
}

fn load_file(name: String, store: &mut Store<EditorInfo>) -> UIResult<EditorWindow, EditorInfo> {
    let path = Path::new(name.as_str());

    if path.is_dir() {
        let ls = path
            .read_dir()?
            .filter_map(|entry| {
                if let Ok(entry) = entry {
                    return DirectoryItem::new(entry);
                } else {
                    return None;
                }
            })
            .collect();
        let id = EditorContentId::Directory(name);

        return Ok(ListState::new(id, ls).into());
    }

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
        store.application.filenames.insert(name, index);
    }

    return Ok(window);
}

impl Window<EditorInfo> for EditorWindow {
    fn id(&self) -> EditorContentId {
        match self {
            EditorWindow::Text(tbox) => {
                return tbox.buffer().read().unwrap().id();
            },
            EditorWindow::Listing(ls) => {
                return ls.id();
            },
        }
    }

    fn get_win_title(&self, _: &mut Store<EditorInfo>) -> Line {
        match self.id() {
            EditorContentId::Command(CommandType::Command) => Line::from("[Command Line]"),
            EditorContentId::Command(CommandType::Search) => Line::from("[Search Bar]"),
            EditorContentId::Scratch => Line::from("[Scratch]"),
            EditorContentId::File(index) => Line::from(format!("Buffer {index}")),
            EditorContentId::Directory(name) => Line::from(name),
        }
    }

    fn open(id: EditorContentId, store: &mut Store<EditorInfo>) -> UIResult<Self, EditorInfo> {
        match id {
            id @ (EditorContentId::Scratch | EditorContentId::File(_)) => {
                let buffer = store.load_buffer(id);

                Ok(TextBoxState::new(buffer).into())
            },
            EditorContentId::Directory(name) => load_file(name, store),
            EditorContentId::Command(_) => {
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

    fn unnamed(store: &mut Store<EditorInfo>) -> UIResult<Self, EditorInfo> {
        Self::open(EditorContentId::Scratch, store)
    }
}

impl Editable<EditContext, Store<EditorInfo>, EditorInfo> for EditorWindow {
    fn editor_command(
        &mut self,
        act: &EditorAction,
        ctx: &EditContext,
        store: &mut Store<EditorInfo>,
    ) -> EditResult<EditInfo, EditorInfo> {
        match self {
            EditorWindow::Text(tbox) => tbox.editor_command(act, ctx, store),
            EditorWindow::Listing(ls) => ls.editor_command(act, ctx, store),
        }
    }
}

impl Jumpable<EditContext, EditorInfo> for EditorWindow {
    fn jump(
        &mut self,
        list: PositionList,
        dir: MoveDir1D,
        count: usize,
        ctx: &EditContext,
    ) -> UIResult<usize, EditorInfo> {
        match self {
            EditorWindow::Text(tbox) => tbox.jump(list, dir, count, ctx),
            EditorWindow::Listing(ls) => ls.jump(list, dir, count, ctx),
        }
    }
}

impl Promptable<EditContext, Store<EditorInfo>, EditorInfo> for EditorWindow {
    fn prompt(
        &mut self,
        act: &PromptAction,
        ctx: &EditContext,
        store: &mut Store<EditorInfo>,
    ) -> EditResult<Vec<(Action<EditorInfo>, EditContext)>, EditorInfo> {
        match self {
            EditorWindow::Text(tbox) => tbox.prompt(act, ctx, store),
            EditorWindow::Listing(ls) => ls.prompt(act, ctx, store),
        }
    }
}

impl Scrollable<EditContext, Store<EditorInfo>, EditorInfo> for EditorWindow {
    fn scroll(
        &mut self,
        style: &ScrollStyle,
        ctx: &EditContext,
        store: &mut Store<EditorInfo>,
    ) -> EditResult<EditInfo, EditorInfo> {
        match self {
            EditorWindow::Text(tbox) => tbox.scroll(style, ctx, store),
            EditorWindow::Listing(ls) => ls.scroll(style, ctx, store),
        }
    }
}

impl Searchable<EditContext, Store<EditorInfo>, EditorInfo> for EditorWindow {
    fn search(
        &mut self,
        dir: MoveDirMod,
        count: Count,
        ctx: &EditContext,
        store: &mut Store<EditorInfo>,
    ) -> UIResult<EditInfo, EditorInfo> {
        match self {
            EditorWindow::Text(tbox) => tbox.search(dir, count, ctx, store),
            EditorWindow::Listing(ls) => ls.search(dir, count, ctx, store),
        }
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
enum EditorContentId {
    Command(CommandType),
    Directory(String),
    File(usize),
    Scratch,
}

impl ApplicationWindowId for EditorContentId {}
impl ApplicationContentId for EditorContentId {}

#[derive(Default)]
struct EditorCompleter;

impl Completer<EditorInfo> for EditorCompleter {
    fn complete(
        &mut self,
        text: &EditRope,
        cursor: &mut Cursor,
        content: &EditorContentId,
        store: &mut EditorStore,
    ) -> Vec<String> {
        match content {
            EditorContentId::Command(CommandType::Command) => {
                complete_cmdbar(text, cursor, &store.cmds)
            },
            EditorContentId::Command(CommandType::Search) => vec![],
            EditorContentId::Directory(_) => vec![],
            EditorContentId::File(_) => vec![],
            EditorContentId::Scratch => vec![],
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum EditorInfo {}

impl ApplicationInfo for EditorInfo {
    type Error = String;
    type Action = ();
    type Store = EditorStore;
    type WindowId = EditorContentId;
    type ContentId = EditorContentId;

    fn content_of_command(ct: CommandType) -> EditorContentId {
        EditorContentId::Command(ct)
    }
}

struct Editor {
    terminal: Terminal<CrosstermBackend<Stdout>>,
    bindings: KeyManager<TerminalKey, Action<EditorInfo>, RepeatType>,
    actstack: VecDeque<(Action<EditorInfo>, EditContext)>,
    screen: ScreenState<EditorWindow, EditorInfo>,
    store: Store<EditorInfo>,
}

impl Editor {
    pub fn new(env: MixedChoice) -> Result<Self, std::io::Error> {
        crossterm::terminal::enable_raw_mode()?;

        let mut stdout = stdout();

        crossterm::execute!(stdout, EnterAlternateScreen)?;

        if crossterm::terminal::supports_keyboard_enhancement()? {
            // Enable the Kitty keyboard enhancement protocol for improved keypresses.
            crossterm::execute!(
                stdout,
                PushKeyboardEnhancementFlags(KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES)
            )?;
        }

        let backend = CrosstermBackend::new(stdout);
        let terminal = Terminal::new(backend)?;

        let mut store = Store::default();
        let bindings = MixedBindings::<TerminalKey, EditorInfo>::from(env);
        let bindings = KeyManager::new(bindings);

        let buf = store.load_buffer(EditorContentId::Scratch);
        let win = EditorWindow::Text(TextBoxState::new(buf));
        let cmd = CommandBarState::new(&mut store);
        let screen = ScreenState::new(win, cmd);

        let actstack = VecDeque::new();

        Ok(Editor { terminal, bindings, actstack, screen, store })
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
                    Ok(Some(InfoMessage::Pager(text))) => {
                        let pager = Box::new(Pager::new(text, vec![]));
                        self.bindings.run_dialog(pager);

                        // Continue processing; we'll redraw later.
                        continue;
                    },
                    Ok(Some(InfoMessage::Message(info))) => {
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

    fn action_prepend(&mut self, acts: Vec<(Action<EditorInfo>, EditContext)>) {
        let mut acts = VecDeque::from(acts);
        acts.append(&mut self.actstack);
        self.actstack = acts;
    }

    fn action_pop(&mut self, keyskip: bool) -> Option<(Action<EditorInfo>, EditContext)> {
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
        ctx: EditContext,
    ) -> UIResult<EditInfo, EditorInfo> {
        let info = match action {
            // Do nothing.
            Action::Application(()) => None,
            Action::NoOp => None,

            // Show an informational message.
            Action::ShowInfoMessage(msg) => Some(msg),

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
                let astore = &mut self.store.application;
                let rstore = &mut self.store.registers;
                let acts = astore.cmds.command(&act, &ctx, rstore)?;
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
        let bindings = &mut self.bindings;
        let sstate = &mut self.screen;
        let store = &mut self.store;
        let term = &mut self.terminal;

        if full {
            term.clear()?;
        }

        term.draw(|f| {
            let area = f.area();

            let modestr = bindings.show_mode();
            let cursor = bindings.get_cursor_indicator();
            let dialogstr = bindings.show_dialog(area.width as usize, area.height as usize);

            let screen = Screen::new(store).show_dialog(dialogstr).show_mode(modestr).borders(true);
            f.render_stateful_widget(screen, area, sstate);

            render_cursor(f, sstate, cursor);
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
