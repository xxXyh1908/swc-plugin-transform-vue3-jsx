use std::path::Path;

use anyhow::Error;
use serde_wasm_bindgen::{from_value, to_value};
use swc_core::{
    base::{
        config::{IsModule, SourceMapsConfig},
        try_with_handler, TransformOutput,
    },
    common::{collections::AHashMap, comments::Comments, FileName, Mark},
    ecma::{
        ast::EsVersion,
        parser::{Syntax, TsConfig},
        visit::FoldWith,
    },
};
use swc_ecma_transforms_base::{hygiene::hygiene, resolver};
use swc_ecma_transforms_typescript::strip;
use wasm_bindgen::prelude::{wasm_bindgen, JsValue};

use vue3_jsx_folder::{create_folder, TransformVue3JsxOptions};

use crate::utils::{convert_err, get_compiler, set_panic_hook};

mod utils;

#[wasm_bindgen(typescript_custom_section)]
const INTERFACE_DEFINITIONS: &'static str = r#"
export interface Options {
    include?: string[]
    exclude?: string[]
    customElement?: string[]
    pragma?: string,
    hmr?: boolean
    mergeProps?: boolean
    enableObjectSlots?: boolean
    optimize?: boolean
    reactStyle?: boolean
    transformOn?: boolean
    transformSlot?: boolean
    transformVSlot?: boolean
    transformOnUpdateEvent?: boolean
    vOn?: boolean
    vModel?: boolean
}

export interface TransformOutput {
    code: string,
    map?: string
}

export function transform(input: string, file_name?: string, options?: Options): TransformOutput;
"#;

fn _transform(
    input: String,
    file_name: Option<String>,
    options: Option<TransformVue3JsxOptions>,
) -> Result<TransformOutput, Error> {
    set_panic_hook();
    let compiler = get_compiler();
    let result = try_with_handler(
        compiler.cm.clone(),
        swc_core::base::HandlerOpts { skip_filename: false, ..Default::default() },
        |handler| {
            compiler.run(|| {
                let filename = match &file_name {
                    Some(file_name) => FileName::Real(Path::new(&file_name).to_path_buf()),
                    None => FileName::Anon,
                };

                let fm = compiler.cm.new_source_file(filename, input.to_string());
                let comments = Some(compiler.comments() as &dyn Comments);

                let program = compiler.parse_js(
                    fm,
                    handler,
                    EsVersion::latest(),
                    Syntax::Typescript(TsConfig { tsx: true, ..Default::default() }),
                    IsModule::Bool(true),
                    comments,
                );

                let result = match program {
                    Ok(mut program) => {
                        let mut comments = compiler.comments().clone();
                        let comments = Some(&mut comments as &mut dyn Comments);

                        let unresolved_mark = Mark::new();
                        let top_level_mark = Mark::new();

                        // Optionally transforms decorators here before the resolver pass
                        // as it might produce runtime declarations.

                        // Conduct identifier scope analysis
                        program = program.fold_with(&mut resolver(unresolved_mark, top_level_mark, true));

                        if let Some(mut folder) = create_folder(options, file_name, comments, None) {
                            program = program.fold_with(&mut folder);
                        }

                        // Remove typescript types
                        program = program.fold_with(&mut strip(top_level_mark));

                        // Fix up any identifiers with the same name, but different contexts
                        program = program.fold_with(&mut hygiene());

                        let names = AHashMap::default();
                        let output = compiler.print(
                            &program,
                            None,
                            None,
                            false,
                            EsVersion::latest(),
                            SourceMapsConfig::Bool(true),
                            &names,
                            None,
                            false,
                            Some(compiler.comments()),
                            true,
                            false,
                        );

                        output
                    }
                    Err(err) => Result::Err(err),
                };

                result
            })
        },
    );

    result
}

#[wasm_bindgen(js_name = "transform", skip_typescript)]
pub fn transform(input: String, file_name: JsValue, options: JsValue) -> Result<JsValue, JsValue> {
    let result = _transform(input, file_name.as_string(), from_value(options).ok());

    match result {
        Ok(out) => to_value(&out).map_err(JsValue::from),
        Err(err) => Err(convert_err(err)),
    }
}
