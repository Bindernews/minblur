use std::{fs, path::PathBuf, str::FromStr, time::Instant};

use clap::Parser;
use minblur_lib::compiler::*;

#[derive(Parser, Debug)]
#[clap(author, version, about)]
struct Args {
    /// Input filename
    #[clap(short = 'f', long)]
    file: String,
    /// Output filename
    #[clap(short = 'o', long, default_value = "")]
    output: String,
    /// Define additional constants (multi-use)
    #[clap(short = 'D', long)]
    define: Vec<String>,
    /// Hide timing information and other metadata
    #[clap(long)]
    no_meta: bool,
}

fn main() -> anyhow::Result<()> {
    let mut args = Args::parse();
    let file_path = PathBuf::from_str(&args.file).expect("'file' is not a path name");
    if args.output.is_empty() {
        args.output = file_path
            .with_extension("minasm")
            .into_os_string()
            .into_string()
            .unwrap();
    }
    let t_start = Instant::now();
    let mut comp = CompilerEnv::new(OsSystemApi::new());
    comp.register_builtin_macros().unwrap();
    for define_string in &args.define {
        add_define(&mut comp, define_string)?;
    }
    let content = fs::read_to_string(&args.file)?;
    let result = comp
        .compile(&args.file, &content)
        .map(|st| GenerateCode::new(&st).to_string())
        .map_err(|e| e.to_string());
    let t_end = Instant::now();
    if !args.no_meta {
        println!("Done in {}s", (t_end - t_start).as_secs_f64());
    }
    match result {
        Ok(out) => {
            if args.output == "-" {
                println!("{}", out);
            } else {
                fs::write(&args.output, out)?;
            }
            Ok(())
        }
        Err(e) => {
            eprintln!("{}", e);
            Err(anyhow::Error::msg(""))
        }
    }
}

pub fn add_define(comp: &mut CompilerEnv, define_string: &str) -> anyhow::Result<()> {
    let pos = define_string.find('=').ok_or_else(|| {
        let msg = format!(
            "define string '{}' must be in format <key>=<value>",
            define_string
        );
        anyhow::Error::msg(msg)
    })?;
    let (key, value_str) = define_string.split_at(pos);
    // Remove the equal sign
    let value = comp
        .parse_value(value_str[1..].trim())
        .map_err(|e| anyhow::Error::msg(format!("{}", e)))?;
    comp.add_define(EnvMode::Global, key.trim(), value);
    Ok(())
}

#[cfg(test)]
mod test {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
