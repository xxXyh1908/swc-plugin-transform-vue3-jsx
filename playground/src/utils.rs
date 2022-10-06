use std::sync::Arc;

use anyhow::Error;
use lazy_static::lazy_static;
use swc::config::ErrorFormat;
use swc_core::{
    base::Compiler,
    common::{FilePathMapping, SourceMap},
};
use wasm_bindgen::JsValue;

lazy_static! {
    static ref COMPILER: Arc<Compiler> = {
        let cm = Arc::new(SourceMap::new(FilePathMapping::empty()));

        Arc::new(Compiler::new(cm))
    };
}

pub(crate) fn get_compiler() -> Arc<Compiler> {
    COMPILER.clone()
}

pub(crate) fn convert_err(err: Error) -> JsValue {
    ErrorFormat::Normal.format(&err).into()
}

#[inline]
pub(crate) fn set_panic_hook() {
    // When the `console_error_panic_hook` feature is enabled, we can call the
    // `set_panic_hook` function at least once during initialization, and then
    // we will get better error messages if our code ever panics.
    //
    // For more details see
    // https://github.com/rustwasm/console_error_panic_hook#readme
    if cfg!(feature = "console_error_panic_hook") {
        console_error_panic_hook::set_once();
    }
}

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;
