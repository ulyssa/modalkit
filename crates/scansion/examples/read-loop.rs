use modalkit::{
    env::{
        emacs::keybindings::{EmacsBindings, EmacsMachine},
        mixed::MixedChoice,
        vim::keybindings::{VimBindings, VimMachine},
    },
    key::TerminalKey,
    keybindings::InputBindings,
};
use scansion::{ReadLine, ReadLineInfo};

fn main() -> Result<(), std::io::Error> {
    let mode = select_mode();
    let mut rl = create_readline(mode)?;

    println!("Reading input in {mode:?} mode; 'q' or 'quit' quits the loop.");

    loop {
        match rl.readline(Some("> ".to_string())) {
            Ok(s) => match s.trim() {
                "q" | "quit" => {
                    return Ok(());
                },
                _ => {
                    println!("User typed: {:?}", s);
                },
            },
            Err(e) => {
                // Print out editor error messages.
                println!("{}", e);
            },
        }
    }
}

fn select_mode() -> MixedChoice {
    let mut args = std::env::args();
    let _ = args.next();

    match args.next() {
        Some(arg) => match arg.as_str().trim() {
            "e" | "emacs" => MixedChoice::Emacs,
            "v" | "vim" => MixedChoice::Vim,
            m => panic!("Unknown environment: {:?}", m),
        },
        None => MixedChoice::Vim,
    }
}

fn create_readline(mode: MixedChoice) -> Result<ReadLine, std::io::Error> {
    match mode {
        MixedChoice::Emacs => {
            let mut emacs = EmacsMachine::empty();
            EmacsBindings::default().submit_on_enter().setup(&mut emacs);
            ReadLine::new(emacs)
        },
        MixedChoice::Vim | _ => {
            let mut vi = VimMachine::<TerminalKey, ReadLineInfo>::empty();
            VimBindings::default().submit_on_enter().setup(&mut vi);
            ReadLine::new(vi)
        },
    }
}