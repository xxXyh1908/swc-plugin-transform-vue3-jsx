use serde::Deserialize;
use swc_core::{
    ecma::visit::{as_folder, Fold},
    plugin::{
        metadata::TransformPluginMetadataContextKind, proxies::TransformPluginProgramMetadata,
    },
};

use crate::{
    utils::{create_file_name_filter, create_filter},
    visitor::{JSXTransformVisitor, JSXTransformVisitorConfig},
    visitor_helpers::create_ident,
};

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
struct TransformVue3JsxOptions {
    #[serde(default = "get_config_default_include")]
    include: Vec<String>,
    #[serde(default)]
    exclude: Vec<String>,
    #[serde(default)]
    custom_element: Vec<String>,
    #[serde(default = "get_false")]
    hmr: bool,
    #[serde(default = "get_true")]
    merge_props: bool,
    #[serde(default = "get_true")]
    enable_object_slots: bool,
    #[serde(default = "get_false")]
    optimize: bool,
    #[serde(default = "get_false")]
    react_style: bool,
    #[serde(default = "get_false")]
    transform_on: bool,
    #[serde(default = "get_false")]
    transform_v_slot: bool,
    #[serde(default = "get_false")]
    transform_on_update_event: bool,
    pragma: Option<String>,
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

    create_file_name_filter(
        Some(options.include.clone()),
        Some(options.exclude.clone()),
        None,
    );

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

pub(crate) fn create_folders_from_metadata(
    metadata: TransformPluginProgramMetadata,
) -> Option<Vec<Box<dyn Fold>>> {
    let cwd = metadata.get_context(&TransformPluginMetadataContextKind::Cwd);
    let config_str = metadata
        .get_transform_plugin_config()
        .unwrap_or(String::from("{}"));
    let config = serde_json::from_str::<TransformVue3JsxOptions>(&config_str)
        .expect("invalid config for vue3-jsx");
    let file_name = metadata.get_context(&TransformPluginMetadataContextKind::Filename);

    if let Some(file_name) = &file_name {
        let filter = create_file_name_filter(
            Some(config.include),
            Some(config.exclude),
            match cwd.as_ref() {
                None => None,
                Some(cwd) => Some(&cwd),
            },
        );

        if !filter.do_filter(&file_name) {
            return None;
        }
    }

    let file_name = file_name.unwrap_or(String::from("module.tsx"));

    let pragma = config.pragma;
    let pragma_ident = match &pragma {
        Some(pragma) => Some(create_ident(pragma)),
        _ => None,
    };

    let jsx_visitor = JSXTransformVisitor::new(
        JSXTransformVisitorConfig {
            pragma: pragma_ident.clone(),
            is_custom_element: create_filter(Some(config.custom_element)),
            hmr: config.hmr,
            merge_props: config.merge_props,
            enable_object_slots: config.enable_object_slots,
            optimize: config.optimize,
            react_style: config.react_style,
            transform_on: config.transform_on,
            transform_v_slot: config.transform_v_slot,
            transform_on_update_event: config.transform_on_update_event,
            file_name,
        },
        metadata.comments,
    );

    let folders: Vec<Box<dyn Fold>> = vec![Box::from(as_folder(jsx_visitor))];

    return Some(folders);
}
