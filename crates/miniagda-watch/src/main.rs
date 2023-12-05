use clap::Parser;
use futures::StreamExt;
use inotify::{Inotify, WatchMask};
use miniagda_core::diagnostics::error::Error;
use miniagda_core::diagnostics::Result;
use miniagda_core::{elaboration::elab, parsing::parse, syntax::surface_to_core};
use std::io;
use std::io::Write;
use std::path::Path;
use termion::cursor;
use termion::{clear, color};

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
  path: std::path::PathBuf,
}

fn run<P: AsRef<Path>>(path: P) -> Result<()> {
  let prog = parse(path)?;
  let prog = surface_to_core(prog)?;
  elab(prog)
}

fn check<P: AsRef<Path>>(path: P) {
  print!("{}{}", clear::All, cursor::Goto(2, 1));
  match run(path) {
    Ok(()) => print!("{}✓{} All Done", color::Fg(color::Green), color::Fg(color::Reset)),
// hässlich
    Err(e) => {
      let e = match e {
        Error::SurfaceToCore(e) => format!("{e}"),
        Error::Parse(e) => format!("{e}"),
        Error::Lex(e) => format!("{e}"),
        Error::Elab(e) => format!("{e}"),
      };
      print!("{}⨯{} {}", color::Fg(color::Red), color::Fg(color::Reset), e);
    }
  };
  io::stdout().flush().unwrap();
}

#[tokio::main]
async fn main() -> std::result::Result<(), Box<dyn std::error::Error>> {
  let args = Args::parse();
  let inotify = Inotify::init().expect("error initializing inotify");
  inotify.watches().add(&args.path, WatchMask::MODIFY).expect("failed to add file watch");
  let mut buffer = [0; 1024];
  let mut stream = inotify.into_event_stream(&mut buffer)?;
  check(&args.path);
  while let Some(event_or_error) = stream.next().await {
    let _ = event_or_error?;
    check(&args.path);
  }
  Ok(())
}
