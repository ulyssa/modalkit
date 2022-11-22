use std::collections::VecDeque;
use std::io::{stdout, Stdout};
use std::time::Duration;

use modalkit::crossterm::{
    self,
    event::{poll, read, Event},
    execute,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen},
};

use modalkit::tui::{
    backend::CrosstermBackend,
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
            Editable,
            Jumpable,
            Promptable,
            Scrollable,
            Searchable,
            TabContainer,
            UIResult,
        },
        application::ApplicationInfo,
        base::RepeatType,
        context::Resolve,
        key::KeyManager,
        store::Store,
    },
    env::{
        mixed::{MixedBindings, MixedChoice, MixedContext},
        vim::command::VimCommandMachine,
    },
    input::{bindings::BindingMachine, key::TerminalKey},
    widgets::{
        screen::{Screen, ScreenState},
        textbox::TextBoxState,
        TerminalCursor,
        TerminalExtOps,
        WindowContainer,
    },
};

type Context = MixedContext<EditorInfo>;
type EditorAction = Action<EditorInfo>;
type EditorBox = TextBoxState<EditorInfo>;
type EditorStore = Store<EditorInfo>;

#[derive(Clone, Debug, Eq, PartialEq)]
enum EditorInfo {}

impl ApplicationInfo for EditorInfo {
    type Action = ();
    type Store = ();
}

struct Editor {
    terminal: Terminal<CrosstermBackend<Stdout>>,
    bindings: KeyManager<TerminalKey, EditorAction, RepeatType, Context>,
    actstack: VecDeque<(EditorAction, Context)>,
    cmds: VimCommandMachine<Context, EditorInfo>,
    screen: ScreenState<EditorBox, EditorInfo>,
    store: EditorStore,
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

        let buf = store.new_buffer();
        let win = TextBoxState::new(buf);
        let screen = ScreenState::new(win, &mut store);

        let actstack = VecDeque::new();

        Ok(Editor { terminal, bindings, actstack, cmds, screen, store })
    }

    pub fn run(&mut self) -> Result<(), std::io::Error> {
        self.terminal.clear()?;

        while TabContainer::<Context, EditorStore>::tabs(&self.screen) != 0 {
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
                        let s = info.to_string();
                        let style = Style::default();

                        self.screen.push_message(s, style);

                        // Continue processing; we'll redraw later.
                        continue;
                    },
                    Err(e) => {
                        let s = e.to_string();
                        let style = Style::default().fg(Color::Red);

                        self.screen.push_message(s, style);

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

    fn action_prepend(&mut self, acts: Vec<(EditorAction, Context)>) {
        let mut acts = VecDeque::from(acts);
        acts.append(&mut self.actstack);
        self.actstack = acts;
    }

    fn action_pop(&mut self, keyskip: bool) -> Option<(EditorAction, Context)> {
        if let res @ Some(_) = self.actstack.pop_front() {
            return res;
        }

        if keyskip {
            return None;
        } else {
            return self.bindings.pop();
        }
    }

    fn action_run(&mut self, action: EditorAction, ctx: Context) -> UIResult {
        let info = match action {
            // Do nothing.
            Action::Application(()) => None,
            Action::NoOp => None,

            // Simple delegations.
            Action::CommandBar(act) => self.screen.command_bar(act, &ctx)?,
            Action::Cursor(act) => self.screen.cursor_command(&act, &ctx, &mut self.store)?,
            Action::Edit(action, mov) => {
                self.screen.edit(&ctx.resolve(&action), &mov, &ctx, &mut self.store)?
            },
            Action::History(act) => self.screen.history_command(act, &ctx, &mut self.store)?,
            Action::InsertText(act) => self.screen.insert_text(act, &ctx, &mut self.store)?,
            Action::Macro(act) => self.bindings.macro_command(act, &ctx, &mut self.store)?,
            Action::Mark(mark) => self.screen.mark(ctx.resolve(&mark), &ctx, &mut self.store)?,
            Action::Scroll(style) => self.screen.scroll(&style, &ctx, &mut self.store)?,
            Action::Search(dir, count) => self.screen.search(dir, count, &ctx, &mut self.store)?,
            Action::Selection(act) => self.screen.selection_command(act, &ctx, &mut self.store)?,
            Action::Suspend => self.terminal.program_suspend()?,
            Action::Tab(cmd) => self.screen.tab_command(cmd, &ctx, &mut self.store)?,
            Action::Window(cmd) => self.screen.window_command(cmd, &ctx)?,

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
                let acts = self.screen.prompt(act, &ctx, &mut self.store)?;
                self.action_prepend(acts);

                None
            },
            Action::Command(act) => {
                let acts = self.cmds.command(act, &ctx)?;
                self.action_prepend(acts);

                None
            },
            Action::Repeat(rt) => {
                self.bindings.repeat(rt, Some(ctx));

                None
            },

            Action::Complete(_, _) => {
                // XXX: implement
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
        let term = &mut self.terminal;

        if full {
            term.clear()?;
        }

        term.draw(|f| {
            let area = f.size();

            let screen = Screen::new().showmode(modestr);
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
