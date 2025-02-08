use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct Cli {
    /// Sets a script file
    #[arg(short, long, value_name = "FILE")]
    pub file: Option<PathBuf>,
}
