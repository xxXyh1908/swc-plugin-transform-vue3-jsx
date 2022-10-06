use serde::Deserialize;
use swc_core::{
    common::comments::Comments,
    ecma::visit::{as_folder, Fold},
};

use crate::{
    utils::{create_file_name_filter, create_filter},
    visitor::{JSXTransformVisitor, JSXTransformVisitorOptions},
    visitor_helpers::create_ident,
};

mod constants;
mod flags;
mod utils;
mod visitor;
mod visitor_helpers;

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct TransformVue3JsxOptions {
    #[serde(default = "get_config_default_include")]
    pub include: Vec<String>,
    #[serde(default)]
    pub exclude: Vec<String>,
    #[serde(default)]
    pub custom_element: Vec<String>,
    #[serde(default = "get_false")]
    pub hmr: bool,
    #[serde(default = "get_true")]
    pub merge_props: bool,
    #[serde(default = "get_true")]
    pub enable_object_slots: bool,
    #[serde(default = "get_false")]
    pub optimize: bool,
    #[serde(default = "get_false")]
    pub react_style: bool,
    #[serde(default = "get_false")]
    pub transform_on: bool,
    #[serde(default = "get_false")]
    pub transform_slot: bool,
    #[serde(default = "get_false")]
    pub transform_v_slot: bool,
    #[serde(default = "get_false")]
    pub transform_on_update_event: bool,
    #[serde(default = "get_true")]
    pub v_on: bool,
    #[serde(default = "get_true")]
    pub v_model: bool,
    pub pragma: Option<String>,
}

#[test]
fn test_options() {
    let options = serde_json::from_str::<TransformVue3JsxOptions>(
        r#"
        {
            "include": [ "test" ]
        }
    "#,
    )
    .expect("invalid config for vue3-jsx");

    create_file_name_filter(Some(options.include.clone()), Some(options.exclude.clone()), None);

    println!("{:?}", options);
}

#[inline]
fn get_true() -> bool {
    true
}

#[inline]
fn get_false() -> bool {
    false
}

#[inline]
fn get_config_default_include() -> Vec<String> {
    vec![String::from(r"/./")]
}

pub fn create_folder<'a>(
    options: Option<TransformVue3JsxOptions>,
    filename: Option<String>,
    comments: Option<&'a mut dyn Comments>,
    cwd: Option<String>,
) -> Option<Box<dyn Fold + 'a>> {
    let file_name = filename.clone().unwrap_or(String::from("module.tsx"));

    let options = match options {
        Some(options) => {
            if let Some(filename) = filename {
                let filter = create_file_name_filter(Some(options.include), Some(options.exclude), cwd);

                if !filter.do_filter(&filename) {
                    return None;
                }
            }

            let pragma = options.pragma;
            let pragma_ident = match &pragma {
                Some(pragma) => Some(create_ident(pragma)),
                _ => None,
            };
            JSXTransformVisitorOptions {
                pragma: pragma_ident.clone(),
                is_custom_element: create_filter(Some(options.custom_element)),
                hmr: options.hmr,
                merge_props: options.merge_props,
                enable_object_slots: options.enable_object_slots,
                optimize: options.optimize,
                react_style: options.react_style,
                transform_on: options.transform_on,
                transform_slot: options.transform_slot,
                transform_v_slot: options.transform_v_slot,
                transform_on_update_event: options.transform_on_update_event,
                v_on: options.v_on,
                v_model: options.v_model,
                file_name,
            }
        }
        None => {
            let mut options = JSXTransformVisitorOptions::default();
            options.file_name = file_name;
            options
        }
    };

    let jsx_visitor: JSXTransformVisitor<'a> = JSXTransformVisitor::new(options, comments);

    Some(Box::from(as_folder(jsx_visitor)))
}
