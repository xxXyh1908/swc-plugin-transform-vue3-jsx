use swc_core::{
    common::comments::Comments,
    ecma::{
        ast::Program,
        visit::{Fold, FoldWith},
    },
    plugin::{metadata::TransformPluginMetadataContextKind, plugin_transform, proxies::TransformPluginProgramMetadata},
};

use vue3_jsx_folder::{create_folder, TransformVue3JsxOptions};

/// An example plugin function with macro support.
/// `plugin_transform` macro interop pointers into deserialized structs, as well
/// as returning ptr back to host.
///
/// It is possible to opt out from macro by writing transform fn manually
/// if plugin need to handle low-level ptr directly via
/// `__transform_plugin_process_impl(
///     ast_ptr: *const u8, ast_ptr_len: i32,
///     unresolved_mark: u32, should_enable_comments_proxy: i32) ->
///     i32 /*  0 for success, fail otherwise.
///             Note this is only for internal pointer interop result,
///             not actual transform result */`
///
/// This requires manual handling of serialization / deserialization from ptrs.
/// Refer swc_plugin_macro to see how does it work internally.
#[plugin_transform]
pub fn process_transform(mut program: Program, mut metadata: TransformPluginProgramMetadata) -> Program {
    if let Some(mut folder) = create_folder_from_metadata(&mut metadata) {
        program = program.fold_with(&mut folder);
    }

    program
}

fn create_folder_from_metadata<'a>(metadata: &'a mut TransformPluginProgramMetadata) -> Option<Box<dyn Fold + 'a>> {
    let cwd = metadata.get_context(&TransformPluginMetadataContextKind::Cwd);
    let options_str = metadata.get_transform_plugin_config().unwrap_or(String::from("{}"));
    let options = serde_json::from_str::<TransformVue3JsxOptions>(&options_str).expect("invalid options for vue3-jsx");
    let file_name = metadata.get_context(&TransformPluginMetadataContextKind::Filename);

    let comments = match &mut metadata.comments {
        Some(comments) => Some(comments as &'a mut dyn Comments),
        None => None,
    };

    create_folder(Some(options), file_name, comments, cwd)
}
