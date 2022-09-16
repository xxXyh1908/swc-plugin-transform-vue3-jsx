use std::sync::Arc;

use swc_core::{
    common::{source_map::Pos, BytePos, FileName, Globals, SourceFile, SourceMap, GLOBALS, comments::NoopComments},
    ecma::{
        ast::{EsVersion, Program},
        codegen::{text_writer::JsWriter, Emitter},
        parser::{parse_file_as_script, EsConfig, PResult, Syntax},
        visit::{as_folder, FoldWith},
    },
};

#[test]
fn test_transform() {
    let create_source_file = |input: String| -> SourceFile {
        SourceFile {
            name: FileName::QuoteExpansion,
            name_was_remapped: false,
            unmapped_path: None,
            crate_of_origin: 0,
            src_hash: 0,
            start_pos: BytePos::from_usize(0),
            end_pos: BytePos::from_usize(input.len()),
            src: Arc::new(input),
            lines: vec![],
            multibyte_chars: vec![],
            non_narrow_chars: vec![],
            name_hash: 0,
        }
    };

    let parse = |source_file: SourceFile| -> PResult<Program> {
        let mut recovered_errors = vec![];
        let script = parse_file_as_script(
            &source_file,
            Syntax::Es(EsConfig {
                jsx: true,
                ..EsConfig::default()
            }),
            EsVersion::latest(),
            None,
            &mut recovered_errors,
        );

        println!("\n\n\n\n");

        match script {
            Ok(script) => Ok(Program::Script(script)),
            Err(err) => Err(err),
        }
    };

    fn tr(program: Program) -> Program {
        let global = Globals::new();
        let r = GLOBALS.set(&global, move || {
            program.fold_with(&mut as_folder(crate::JSXTransformVisitor::default()))
        });

        r
    }

    fn emit(program: Program) -> Option<String> {
        let mut u8_array = Vec::new();
        let sourcemap = Arc::new(SourceMap::default());
        let writer = JsWriter::new(sourcemap.clone(), "\n", &mut u8_array, None);
        let mut emitter = Emitter {
            cfg: Default::default(),
            cm: sourcemap.clone(),
            comments: None,
            wr: writer,
        };

        return match program {
            Program::Module(module) => match emitter.emit_module(&module) {
                Ok(_) => unsafe { Some(String::from_utf8_unchecked(u8_array)) },
                _ => {
                    println!("emit err");
                    None
                }
            },
            Program::Script(script) => match emitter.emit_script(&script) {
                Ok(_) => unsafe { Some(String::from_utf8_unchecked(u8_array)) },
                _ => None,
            },
        };
    }

    match parse(create_source_file(String::from(
        r#"

        const vDirective = {}
        const Public = {}
        const ComponentA = defineComponent({
            render() {
                return <teleport {...spread1} v-show={show} prop={prop} {...spread2} >
                    <>
                        <div v-html={html}>text</div>
                        <Private node={<span/>} v-private v-directive:arg_modifier={directiveValue} on={{a: () => {}}} onA={onA} on={on} >{slots}</Private>
                        <Public v-slots={{slotA: () => []}} >
                            <input v-model={inputTextValue} />
                            <input type="tel" v-model={[inputTextValue, "moduleValue", ["number"]]} />
                            <input type="checkbox" v-model={inputCheckboxValue} />
                            <input type={dynamicType} v-model={inputDynamicValue} />
                            <select v-model={selectValue} />
                        </Public>
                    </>
                </teleport>
            }
        })

    "#,
    ))) {
        Ok(program) => {
            let result = emit(tr(program));
            match result {
                Some(result) => {
                    println!("{}", result);
                }
                None => {
                    println!("None");
                }
            }
        }
        Err(_err) => {
            println!("err");
        }
    }
}
