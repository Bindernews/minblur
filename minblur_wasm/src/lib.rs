use std::rc::Rc;

use minblur_lib::{compiler::*, parser::Source};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    pub fn resolve_path(base: &str, relative: &str) -> Option<String>;
    pub fn fs_read_to_string(path: &str) -> Option<String>;
}

#[derive(Debug)]
struct WasmSystemApi {}
impl SystemApi for WasmSystemApi {
    fn resolve_path(&self, base: &str, relative: &str) -> Result<String, std::io::Error> {
        resolve_path(base, relative)
            .ok_or_else(|| std::io::Error::from(std::io::ErrorKind::InvalidData))
    }

    fn fs_read_to_string(&mut self, path: &str) -> Result<String, std::io::Error> {
        fs_read_to_string(path).ok_or_else(|| std::io::Error::from(std::io::ErrorKind::NotFound))
    }
}

#[wasm_bindgen]
pub struct CompilerApi {
    env: CompilerEnv,
    system_source: Source,
}
#[wasm_bindgen]
impl CompilerApi {
    pub fn create() -> Result<CompilerApi, String> {
        let mut env = CompilerEnv::new(WasmSystemApi {});
        env.register_builtin_macros()
            .map_err(|e| format!("{}", e))?;
        Ok(Self {
            env,
            system_source: Source::new(Rc::new("<system>".into()), 0, 0),
        })
    }

    pub fn add_define_parse(&mut self, key: &str, value_str: &str) -> Result<(), String> {
        let value = self
            .env
            .parse_value(value_str)
            .map_err(|e| e.with_source(&self.system_source).to_string())?;
        self.env.add_define(EnvMode::Pass, key, value);
        Ok(())
    }

    pub fn compile(&mut self, source_name: &str, code: &str) -> Result<String, String> {
        self.env
            .compile_and_generate(source_name, code)
            .map_err(|e| e.to_string())
    }

    pub fn generate_labels(&mut self, source_name: &str, code: &str) -> Result<String, String> {
        self.env
            .generate_labels(source_name, code)
            .map(|st| GenerateCode::new(&st).to_string())
            .map_err(|e| e.to_string())
    }
}
impl CompilerApi {}
