use lazy_static::lazy_static;
use serde::de::DeserializeOwned;
use std::{
    collections::HashMap,
    env,
    panic::set_hook,
    path::{Path, PathBuf},
    sync::Arc,
};
use swc_core::{
    ecma::{ast::Program, visit::FoldWith},
};

use swc_core::{
    base::{
        config::{IsModule, SourceMapsConfig},
        try_with_handler, Compiler,
    },
    common::{collections::AHashMap, comments::Comments, FileName, FilePathMapping, SourceMap},
    ecma::{
        ast::{EsVersion},
        parser::{Syntax, TsConfig},
    },
};
use swc_plugin_transform_vue3_jsx::{create_folder, parse_options};
use wasm_bindgen::{ prelude::wasm_bindgen};

lazy_static! {
    static ref COMPILER: Arc<Compiler> = {
        let cm = Arc::new(SourceMap::new(FilePathMapping::empty()));

        Arc::new(Compiler::new(cm))
    };
}

fn get_compiler() -> Arc<Compiler> {
    COMPILER.clone()
}

#[wasm_bindgen]
pub fn transform(input: String, file_name: String, options: String)  {
    let compiler = get_compiler();
    let filename = FileName::Real(Path::new(&file_name).to_path_buf());
    let comments = Some(compiler.comments() as &dyn Comments);
    let program = try_with_handler(
        compiler.cm.clone(),
        swc_core::base::HandlerOpts {
            skip_filename: false,
            ..Default::default()
        },
        |handler| {
            compiler.run(|| {
                let fm = compiler.cm.new_source_file(filename, input.to_string());

                compiler.parse_js(
                    fm,
                    handler,
                    EsVersion::latest(),
                    Syntax::Typescript(TsConfig {
                        tsx: true,
                        ..Default::default()
                    }),
                    IsModule::Bool(true),
                    comments,
                )
            })
        },
    )
    .unwrap();

    let folder = create_folder(Some(parse_options(&options, &file_name)), comments.clone());
    let names = AHashMap::default();

    program = program.fold_with(&mut folder);

    let result = compiler
        .print(
            &program,
            None,
            None,
            false,
            EsVersion::latest(),
            SourceMapsConfig::Bool(true),
            &names,
            None,
            false,
            Some(&comments),
            false,
            false,
        )
        .unwrap();

    // return result;
}

#[test]
fn a() {}
