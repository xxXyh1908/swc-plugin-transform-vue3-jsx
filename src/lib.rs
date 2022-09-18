use swc_core::{
    ecma::{ast::Program, visit::FoldWith},
    plugin::{plugin_transform, proxies::TransformPluginProgramMetadata},
};

use crate::options::create_folders_from_metadata;
pub use crate::visitor::JSXTransformVisitor;
pub use crate::visitor::JSXTransformVisitorConfig;

mod constants;
mod flags;
mod options;
mod test;
mod utils;
mod visitor;
mod visitor_helpers;

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
pub fn process_transform(
    mut program: Program,
    metadata: TransformPluginProgramMetadata,
) -> Program {
    if let Some(mut folders) = create_folders_from_metadata(metadata) {
        for mut folder in folders.drain(..) {
            program = program.fold_with(folder.as_mut());
        }
    }

    program
}
