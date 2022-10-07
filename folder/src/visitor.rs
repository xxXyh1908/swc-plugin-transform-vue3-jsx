use std::{
    collections::{hash_map::DefaultHasher, HashSet},
    hash::{Hash, Hasher},
};

use lazy_static::lazy_static;
use linked_hash_map::LinkedHashMap;
use regex::Regex;
use swc_core::{
    common::{comments::Comments, BytePos, Span, DUMMY_SP},
    ecma::{
        ast::{
            ArrayLit, ArrowExpr, AssignPat, BindingIdent, BlockStmt, CallExpr, Callee, ClassExpr, ClassMethod,
            ComputedPropName, Constructor, Decl, ExportSpecifier, Expr, ExprOrSpread, FnDecl, FnExpr, ForInStmt,
            ForOfStmt, ForStmt, Function, GetterProp, Ident, ImportDecl, ImportNamedSpecifier, ImportSpecifier,
            JSXAttrName, JSXAttrOrSpread, JSXAttrValue, JSXElement, JSXElementChild, JSXElementName, JSXExpr,
            JSXExprContainer, JSXFragment, JSXText, KeyValuePatProp, KeyValueProp, Lit, MethodProp, Module, ModuleDecl,
            ModuleExportName, ModuleItem, Number, ObjectLit, ObjectPat, ObjectPatProp, Pat, PrivateMethod, Program,
            Prop, PropName, PropOrSpread, Script, SeqExpr, SetterProp, SpreadElement, Stmt, Str, ThisExpr,
            TsExportAssignment, TsModuleDecl, UnaryOp, VarDecl, VarDeclKind, VarDeclarator,
        },
        atoms::js_word,
        utils::{private_ident, quote_ident, quote_str},
        visit::{VisitMut, VisitMutWith},
    },
    quote,
};

use crate::{
    constants::{is_builtin_component, is_known_tag},
    flags::{PatchFlags, SlotFlags},
    utils::{camelize, camelize_upper_first, lower_first, upper_first, StringFilter},
    visitor_helpers::{
        ast_ident_to_string, ast_str_to_string, clone_ident, clone_lit, create_ident, create_key_value_prop,
        create_null_expr, create_private_ident, create_true_expr, create_void_zero_expr,
        find_decl_ident_from_module_items, find_decl_ident_from_pattern, find_decl_ident_from_pattern0,
        find_decl_ident_from_stmts, is_constant_expr, is_define_component_expr, is_script_top,
        jsx_member_ref_to_member_expr, unwrap_expr, unwrap_expr_move, DrainValues, ForLike, FunctionScope, JsxTag,
        ObjectKey, PropsItem, PropsItemMapItem, VarDeclRecord,
    },
    visitor_helpers::{prepend_var_decls_into_module_items, AstSpanAccessor},
};

const HELPER_ID: &str = "swc-plugin-transform-vue3-jsx/helpers";

mod event_helpers {
    use std::collections::{HashMap, HashSet};

    use lazy_static::lazy_static;
    use swc_core::{
        common::DUMMY_SP,
        ecma::{
            ast::{
                ArrayLit, ArrowExpr, BinExpr, BinaryOp, BindingIdent, BlockStmt, BlockStmtOrExpr, ComputedPropName,
                Expr, ExprOrSpread, Ident, Lit, MemberExpr, MemberProp, Number, Pat, Stmt, Str,
            },
            utils::private_ident,
        },
        quote,
    };

    const KEY_MODIFIERS: [&str; 4] = ["ctrl", "shift", "alt", "meta"];

    fn gen_guard(expr: Expr) -> Stmt {
        quote!(r"if( $expr ) { return }" as Stmt, expr: Expr = expr)
    }

    fn modifier_stop() -> Stmt {
        quote!(r"$event.stopPropagation();" as Stmt, event: Ident = super::clone_ident(&EVENT_IDENT))
    }

    fn modifier_prevent() -> Stmt {
        quote!(r"$event.preventDefault();" as Stmt, event: Ident = super::clone_ident(&EVENT_IDENT))
    }

    fn modifier_self() -> Stmt {
        gen_guard(quote!(
            r"$event.target !== $event.currentTarget" as Expr,
            event: Ident = super::clone_ident(&EVENT_IDENT)
        ))
    }

    fn modifier_ctrl() -> Stmt {
        gen_guard(quote!(r"!$event.ctrlKey" as Expr, event: Ident = super::clone_ident(&EVENT_IDENT)))
    }

    fn modifier_shift() -> Stmt {
        gen_guard(quote!(r"!$event.shiftKey" as Expr, event: Ident = super::clone_ident(&EVENT_IDENT)))
    }

    fn modifier_alt() -> Stmt {
        gen_guard(quote!(r"!$event.altKey" as Expr, event: Ident = super::clone_ident(&EVENT_IDENT)))
    }

    fn modifier_meta() -> Stmt {
        gen_guard(quote!(r"!$event.metaKey" as Expr, event: Ident = super::clone_ident(&EVENT_IDENT)))
    }

    fn modifier_left() -> Stmt {
        gen_guard(quote!(
            r"'button' in $event && $event.button !== 0" as Expr,
            event: Ident = super::clone_ident(&EVENT_IDENT)
        ))
    }

    fn modifier_middle() -> Stmt {
        gen_guard(quote!(
            r"'button' in $event && $event.button !== 1" as Expr,
            event: Ident = super::clone_ident(&EVENT_IDENT)
        ))
    }

    fn modifier_right() -> Stmt {
        gen_guard(quote!(
            r"'button' in $event && $event.button !== 2" as Expr,
            event: Ident = super::clone_ident(&EVENT_IDENT)
        ))
    }

    lazy_static! {
        pub static ref EVENT_IDENT: Ident = private_ident!("$event");
        pub static ref CHECK_KEY_CODES_IDENT: Ident = private_ident!("_checkKeyCodes");
        static ref KEY_CODES: HashMap<&'static str, Vec<i32>> = HashMap::from([
            ("esc", vec![27]),
            ("tab", vec![9]),
            ("enter", vec![13]),
            ("space", vec![32]),
            ("up", vec![38]),
            ("left", vec![37]),
            ("right", vec![39]),
            ("down", vec![40]),
            ("delete", vec![8, 46]),
        ]);
        static ref KEY_NAMES: HashMap<&'static str, Vec<&'static str>> = HashMap::from([
              // #7880: IE11 and Edge use `Esc` for Escape key name.
            ("esc", vec!["Esc", "Escape"]),
            ("tab", vec!["Tab"]),
            ("enter", vec!["Enter"]),
            ("space", vec![" "]),
              // #7806: IE11 uses key names without `Arrow` prefix for arrow keys.
            ("up", vec!["Up", "ArrowUp"]),
            ("left", vec!["Left", "ArrowLeft"]),
            ("right", vec!["Right","ArrowRight"]),
            ("down", vec!["Down","ArrowDown"]),
            ("delete", vec!["Backspace","Delete" ]),
        ]);
        static ref MODIFIERS_STMTS: HashMap<&'static str, fn() -> Stmt> = HashMap::from([
            ("stop", modifier_stop as fn() -> Stmt),
            ("prevent", modifier_prevent as fn() -> Stmt),
            ("self", modifier_self as fn() -> Stmt),
            ("ctrl", modifier_ctrl as fn() -> Stmt),
            ("shift", modifier_shift as fn() -> Stmt),
            ("alt", modifier_alt as fn() -> Stmt),
            ("meta", modifier_meta as fn() -> Stmt),
            ("left", modifier_left as fn() -> Stmt),
            ("middle", modifier_middle as fn() -> Stmt),
            ("right", modifier_right as fn() -> Stmt)
        ]);
    }

    fn gen_filter_code(key: &str) -> Expr {
        if let Ok(key_val) = key.parse::<i32>() {
            return quote!(
                r#"$event . keyCode !== $key_val"# as Expr,
                event: Ident = super::clone_ident(&self::EVENT_IDENT),
                key_val: Expr = Expr::Lit(Lit::Num(Number::from(key_val as f64)))
            );
        }

        let key_code = self::KEY_CODES.get(&key);
        let key_name = self::KEY_NAMES.get(&key);

        let key_code_expr = if let Some(key_code) = key_code {
            if key_code.len() == 1 {
                Expr::Lit(Lit::Num(Number::from(*key_code.first().unwrap() as f64)))
            } else {
                Expr::Array(ArrayLit {
                    span: DUMMY_SP,
                    elems: key_code
                        .iter()
                        .map(|code| {
                            Some(ExprOrSpread {
                                spread: None,
                                expr: Box::new(Expr::Lit(Lit::Num(Number::from(*code as f64)))),
                            })
                        })
                        .collect(),
                })
            }
        } else {
            super::create_void_zero_expr()
        };

        let key_name_expr = if let Some(key_name) = key_name {
            if key_name.len() == 1 {
                Expr::Lit(Lit::Str(Str::from(key_name.first().unwrap().to_string())))
            } else {
                Expr::Array(ArrayLit {
                    span: DUMMY_SP,
                    elems: key_name
                        .iter()
                        .map(|name| {
                            Some(ExprOrSpread {
                                spread: None,
                                expr: Box::new(Expr::Lit(Lit::Str(Str::from(name.to_string())))),
                            })
                        })
                        .collect(),
                })
            }
        } else {
            super::create_void_zero_expr()
        };

        quote!(
            r#"$check_ident( $event . keyCode, $key, $key_code, $event . key, $key_name )"# as Expr,
            check_ident: Ident = super::clone_ident(&self::CHECK_KEY_CODES_IDENT),
            event: Ident = super::clone_ident(&self::EVENT_IDENT),
            key: Expr = Expr::Lit(Lit::Str(Str::from(key.to_string()))),
            key_code: Expr = key_code_expr,
            key_name: Expr = key_name_expr
        )
    }

    pub fn gen_handler(
        mut event: String,
        expression: Option<Expr>,
        mut modifiers: HashSet<String>,
    ) -> (String, Option<Expr>) {
        if event == "click" && modifiers.contains("right") {
            modifiers.remove("right");
            event = String::from("contextmenu")
        }

        if event == "click" && modifiers.contains("middle") {
            event = String::from("mouseup")
        }

        if modifiers.is_empty() {
            return (event, expression);
        }

        let mut code = Vec::new();
        let mut gen_modifier_code = Vec::new();
        let mut keys = Vec::new();

        for modifier in modifiers.iter() {
            if let Some(gen_key) = self::MODIFIERS_STMTS.get(&modifier.as_str()) {
                gen_modifier_code.push(gen_key());
                if self::KEY_CODES.contains_key(&modifier.as_str()) {
                    keys.push(modifier.to_string())
                }
            } else {
                match &modifier as &str {
                    "exact" => {
                        let expr = self::KEY_MODIFIERS
                            .iter()
                            .filter(|modifier| modifiers.contains(**modifier))
                            .map(|modifier| {
                                Expr::Member(MemberExpr {
                                    span: DUMMY_SP,
                                    obj: Box::new(Expr::Ident(super::clone_ident(&self::EVENT_IDENT))),
                                    prop: MemberProp::Computed(ComputedPropName {
                                        span: DUMMY_SP,
                                        expr: Box::new(Expr::Lit(Lit::Str(Str::from(modifier.to_string() + "Key")))),
                                    }),
                                })
                            })
                            .reduce(|acc, item| {
                                Expr::Bin(BinExpr {
                                    span: DUMMY_SP,
                                    op: BinaryOp::LogicalOr,
                                    left: Box::new(acc),
                                    right: Box::new(item),
                                })
                            });

                        if let Some(expr) = expr {
                            gen_modifier_code.push(quote!(r"if( $expr ) { return }" as Stmt, expr: Expr = expr))
                        }
                    }
                    "capture" => event = format!("{}Capture", &event),
                    "once" => event = format!("{}Once", &event),
                    _ => keys.push(modifier.to_string()),
                }
            }
        }

        if !keys.is_empty() {
            let expr = keys.iter().map(|key| gen_filter_code(key)).fold(
                quote!(r#"!("button" in $event)"# as Expr, event: Ident = super::clone_ident(&self::EVENT_IDENT),),
                |acc, item| {
                    Expr::Bin(BinExpr {
                        span: DUMMY_SP,
                        op: BinaryOp::LogicalAnd,
                        left: Box::new(acc),
                        right: Box::new(item),
                    })
                },
            );
            code.push(gen_guard(expr))
        }

        code.append(&mut gen_modifier_code);

        if code.is_empty() {
            return (event, expression);
        }

        if let Some(expression) = expression {
            code.push(quote!(
                r#"return ( $callee ) ?. ($event)"# as Stmt,
                callee: Expr = expression,
                event: Ident = super::clone_ident(&self::EVENT_IDENT),
            ));
        }

        return (
            event,
            Some(Expr::Arrow(ArrowExpr {
                span: DUMMY_SP,
                params: vec![Pat::Ident(BindingIdent::from(super::clone_ident(&self::EVENT_IDENT)))],
                body: BlockStmtOrExpr::BlockStmt(BlockStmt { span: DUMMY_SP, stmts: code }),
                is_async: false,
                is_generator: false,
                type_params: None,
                return_type: None,
            })),
        );
    }
}

lazy_static! {
    static ref STRIP_EMPTY_JSX_TEXT_REGEX: Regex = Regex::new(r"(\r?\n[ \t]*)*").unwrap();
    static ref IS_NEED_MERGE_KEY_REGEX: Regex = Regex::new(r"^(class|style|on[^a-z].*)$").unwrap();
    static ref IS_NOT_ELEMENT_TAG_REGEX: Regex = Regex::new(r"^[^a-z0-9_]").unwrap();
    static ref IS_VUE_DIRECTIVE_REGEX: Regex = Regex::new(r"^v(-[a-zA-Z]|[A-Z])").unwrap();
    static ref IS_ON_REGEX: Regex = Regex::new(r"^on[^a-z]").unwrap();
    static ref ON_UPDATE_EVENT_REGEX: Regex = Regex::new(r"^onUpdate([A-Z])(\S*)$").unwrap();
}

pub(crate) struct JSXTransformVisitorOptions {
    pub pragma: Option<Ident>,
    pub is_custom_element: Box<dyn StringFilter>,
    pub hmr: bool,
    pub optimize: bool,
    pub merge_props: bool,
    pub react_style: bool,
    pub transform_on: bool,
    pub transform_slot: bool,
    pub transform_v_slot: bool,
    pub transform_on_update_event: bool,
    pub enable_object_slots: bool,
    pub file_name: String,
    pub v_on: bool,
    pub v_model: bool,
}

pub(crate) struct JSXTransformVisitor<'a> {
    /* base */
    imports: LinkedHashMap<String, LinkedHashMap<String, Ident>>,
    vars: Vec<VarDeclRecord>,
    new_global_vars: LinkedHashMap<String, (VarDeclKind, Ident, Option<Box<Expr>>)>,
    new_local_vars_vec: Vec<LinkedHashMap<String, (VarDeclKind, Ident, Option<Box<Expr>>)>>,
    _i: u64,
    _j: u64,

    /* jsx */
    text_pragma: Option<Ident>,
    with_directives_pragma: Option<Ident>,
    case_array_id: Option<Ident>,
    config: JSXTransformVisitorOptions,

    comments: Option<&'a mut dyn Comments>,
}

struct NoopFilter;

struct DefineComponentInfo {
    local: Ident,
    exports: Vec<String>,
}

#[derive(Debug, PartialEq, Hash, Clone)]
enum VModelArgument {
    Str(String),
    Expr(Box<Expr>),
}

impl VModelArgument {
    fn into_expr(self) -> Expr {
        match self {
            VModelArgument::Str(str) => Expr::Lit(Lit::Str(Str::from(str))),
            VModelArgument::Expr(expr) => *expr,
        }
    }

    fn into_expr_box(self) -> Box<Expr> {
        match self {
            VModelArgument::Str(str) => Box::new(Expr::Lit(Lit::Str(Str::from(str)))),
            VModelArgument::Expr(expr) => expr,
        }
    }

    fn to_expr(&self) -> Expr {
        match self {
            VModelArgument::Str(str) => Expr::Lit(Lit::Str(Str::from(str.to_string()))),
            VModelArgument::Expr(expr) => {
                let expr_box = expr.clone();
                *expr_box
            }
        }
    }

    fn to_expr_box(&self) -> Box<Expr> {
        match self {
            VModelArgument::Str(str) => Box::new(Expr::Lit(Lit::Str(Str::from(str.to_string())))),
            VModelArgument::Expr(expr) => expr.clone(),
        }
    }

    fn to_prop_name(&self) -> PropName {
        match self {
            VModelArgument::Str(str) => PropName::Str(Str::from(str.to_string())),
            VModelArgument::Expr(expr) => {
                PropName::Computed(ComputedPropName { span: expr.as_ref().get_span(), expr: expr.clone() })
            }
        }
    }

    fn to_prop_name_with_prefix(&self, prefix: &str) -> PropName {
        match self {
            VModelArgument::Str(str) => PropName::Str(Str::from(String::from(prefix) + str)),
            VModelArgument::Expr(expr) => PropName::Computed(ComputedPropName {
                span: expr.as_ref().get_span(),
                expr: quote!(
                    r#"$prefix + ($expr)"# as Box<Expr>,
                    prefix: Expr = Expr::Lit(Lit::Str(Str::from(prefix.to_string()))),
                    expr: Expr = expr.as_ref().clone()
                ),
            }),
        }
    }

    fn to_prop_name_with_suffix(&self, suffix: &str) -> PropName {
        match self {
            VModelArgument::Str(str) => PropName::Str(Str::from(String::from(str) + suffix)),
            VModelArgument::Expr(expr) => PropName::Computed(ComputedPropName {
                span: expr.as_ref().get_span(),
                expr: quote!(
                    r#"($expr) + $suffix"# as Box<Expr>,
                    expr: Expr = expr.as_ref().clone(),
                    suffix: Expr = Expr::Lit(Lit::Str(Str::from(suffix.to_string())))
                ),
            }),
        }
    }

    fn to_prop_name_with_prefix_suffix(&self, prefix: &str, suffix: &str) -> PropName {
        match self {
            VModelArgument::Str(str) => PropName::Str(Str::from(String::from(prefix) + str + suffix)),
            VModelArgument::Expr(expr) => PropName::Computed(ComputedPropName {
                span: expr.as_ref().get_span(),
                expr: quote!(
                    r#"$prefix + ($expr) + $suffix"# as Box<Expr>,
                    prefix: Expr = Expr::Lit(Lit::Str(Str::from(prefix.to_string()))),
                    suffix: Expr = Expr::Lit(Lit::Str(Str::from(suffix.to_string()))),
                    expr: Expr = expr.as_ref().clone(),
                ),
            }),
        }
    }
}

impl StringFilter for NoopFilter {}

impl Default for JSXTransformVisitorOptions {
    fn default() -> Self {
        Self {
            pragma: None,
            is_custom_element: Box::new(NoopFilter {}),
            hmr: false,
            optimize: false,
            merge_props: true,
            react_style: false,
            transform_on: true,
            transform_slot: false,
            transform_v_slot: false,
            transform_on_update_event: false,
            enable_object_slots: true,
            file_name: "module.tsx".to_string(),
            v_on: true,
            v_model: true,
        }
    }
}

/* base */
impl JSXTransformVisitor<'_> {
    pub fn with_pure(&mut self, pos: BytePos) {
        if let Some(comments) = &mut self.comments {
            if self.vars.len() < 2 {
                comments.add_pure_comment(pos)
            }
        }
    }

    pub fn add_import(&mut self, _module_id: &str, name: &str) -> Ident {
        let import_map = self.imports.entry(_module_id.to_string()).or_insert_with(LinkedHashMap::new);

        let ident = import_map.entry(name.to_string()).or_insert_with(|| create_private_ident(name));

        clone_ident(ident)
    }

    pub fn end_helper(&mut self, program: &mut Program) {
        match program {
            Program::Module(module) => {
                let items = &mut module.body;

                for (i, (module_id, value)) in self.imports.drain().enumerate() {
                    let mut specifiers = Vec::with_capacity(value.len());
                    for (name, local) in value.into_iter() {
                        specifiers.push(ImportSpecifier::Named(ImportNamedSpecifier {
                            span: DUMMY_SP,
                            local,
                            imported: Some(ModuleExportName::Ident(create_ident(&name))),
                            is_type_only: false,
                        }));
                    }
                    items.insert(
                        i,
                        ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
                            span: DUMMY_SP,
                            specifiers,
                            src: module_id.into(),
                            type_only: false,
                            asserts: None,
                        })),
                    );
                }

                let values = self.new_global_vars.drain_values();

                prepend_var_decls_into_module_items(items, values.into_iter());
            }
            Program::Script(script) => {
                let items = &mut script.body;
                let mut decls = Vec::new();

                let mut insert_index = items.len();

                for (i, item) in items.iter().enumerate() {
                    if !is_script_top(item) {
                        insert_index = i;
                        break;
                    }
                }

                for (module_id, value) in self.imports.drain() {
                    let mut properties = Vec::with_capacity(value.len());
                    for (name, local) in value.into_iter() {
                        properties.push(ObjectPatProp::KeyValue(KeyValuePatProp {
                            key: PropName::Ident(create_ident(&name)),
                            value: Box::from(BindingIdent::from(local)),
                        }))
                    }
                    decls.push(VarDeclarator {
                        span: DUMMY_SP,
                        name: Pat::Object(ObjectPat {
                            span: DUMMY_SP,
                            props: properties,
                            optional: false,
                            type_ann: None,
                        }),
                        init: Some(Box::from(Expr::Call(CallExpr {
                            span: DUMMY_SP,
                            callee: Callee::Expr(Box::from(quote_ident!("require"))),
                            args: vec![ExprOrSpread::from(Expr::Lit(Lit::Str(Str::from(module_id))))],
                            type_args: None,
                        }))),
                        definite: false,
                    })
                }

                if !decls.is_empty() {
                    items.insert(
                        insert_index,
                        Stmt::Decl(Decl::Var(VarDecl {
                            span: DUMMY_SP,
                            kind: VarDeclKind::Const,
                            declare: false,
                            decls,
                        })),
                    );
                    insert_index += 1;
                }

                for (kind, ident, init) in self.new_global_vars.drain_values() {
                    items.insert(
                        insert_index,
                        Stmt::Decl(Decl::Var(VarDecl {
                            span: DUMMY_SP,
                            kind,
                            declare: false,
                            decls: vec![VarDeclarator {
                                span: DUMMY_SP,
                                name: Pat::Ident(BindingIdent::from(ident)),
                                init,
                                definite: false,
                            }],
                        })),
                    );
                    insert_index += 1;
                }
            }
        };
    }

    fn _add_variable<F: FnOnce() -> Option<Box<Expr>>>(
        &mut self,
        key: String,
        get_expr: F,
        kind: VarDeclKind,
        local: bool,
    ) -> Ident {
        let local_vars = if local {
            match self.new_local_vars_vec.last_mut() {
                Some(local_vars) => local_vars,
                _ => &mut self.new_global_vars,
            }
        } else {
            &mut self.new_global_vars
        };

        if let Some((_kind, ident, _)) = local_vars.get(&key) {
            return clone_ident(ident);
        }
        let ident = create_private_ident(&format!("private_var_{}", {
            self._i += 1;
            self._i
        }));
        local_vars.insert(key, (kind, clone_ident(&ident), get_expr()));

        return ident;
    }

    pub fn add_local_variable<F: FnOnce() -> Option<Box<Expr>>>(
        &mut self,
        key: String,
        get_expr: F,
        kind: VarDeclKind,
    ) -> Ident {
        self._add_variable(key, get_expr, kind, true)
    }

    pub fn add_global_variable<F: FnOnce() -> Option<Box<Expr>>>(
        &mut self,
        key: String,
        get_expr: F,
        kind: VarDeclKind,
    ) -> Ident {
        self._add_variable(key, get_expr, kind, false)
    }

    pub fn add_temp_variable<F: FnOnce() -> Option<Box<Expr>>>(&mut self, get_expr: F, kind: VarDeclKind) -> Ident {
        self._j += 1;
        self._add_variable(self._j.to_string(), get_expr, kind, true)
    }

    pub fn contains_var_ident(&self, ident: &Ident) -> bool {
        for vars in self.vars.iter().rev() {
            if vars.contains(ident) {
                return true;
            }
        }
        false
    }

    pub fn get_var_ident(&self, str: &str) -> Option<Ident> {
        for vars in self.vars.iter().rev() {
            let result = vars.get_ident(str);
            if let Some(ident) = result {
                return Some(clone_ident(ident));
            }
        }
        None
    }

    #[warn(non_snake_case)]
    fn _visit_mut_function_scope__children_with<T: FunctionScope + VisitMutWith<Self>>(
        &mut self,
        function_scope: &mut T,
    ) {
        let mut vars: VarDeclRecord = VarDeclRecord::new(true);
        if let Some(ident) = function_scope._get_ident() {
            if function_scope._is_decl_stmt() {
                self._push_var(ident)
            }
            vars.insert(clone_ident(ident));
        }

        let params = function_scope._get_params();
        let params_iter = match &params {
            None => function_scope._get_params_iter(),
            Some(params) => Some(params.iter()),
        };
        if let Some(params_iter) = params_iter {
            for param in params_iter {
                for ident in find_decl_ident_from_pattern(&param.pat).into_iter() {
                    vars.insert(ident);
                }
            }
        }

        self.new_local_vars_vec.push(LinkedHashMap::new());
        self.vars.push(vars);
        function_scope.visit_mut_children_with(self);
        self.vars.pop();

        if let Some(mut new_local_vars) = self.new_local_vars_vec.pop() {
            let values = new_local_vars.drain_values();
            if !values.is_empty() {
                function_scope._prepend_var_decls(values.into_iter());
            }
        }
    }

    #[warn(non_snake_case)]
    fn _visit_mut_for__children_with<T: ForLike + VisitMutWith<Self>>(&mut self, for_stmt: &mut T) {
        if let Some(decl) = for_stmt._get_init_decl() {
            if let VarDeclKind::Var = decl.kind {
                let var_record = self._get_fn_scope_vars_record();

                if let Some(var_record) = var_record {
                    let mut idents = Vec::new();
                    for decl in decl.decls.iter() {
                        find_decl_ident_from_pattern0(&decl.name, &mut idents);
                    }

                    for ident in idents {
                        var_record.insert(ident);
                    }
                }

                for_stmt.visit_mut_children_with(self);
            } else {
                let mut var_record = VarDeclRecord::new(false);
                let mut idents = Vec::new();

                for decl in decl.decls.iter() {
                    find_decl_ident_from_pattern0(&decl.name, &mut idents);
                }

                for ident in idents {
                    var_record.insert(ident);
                }

                self.vars.push(var_record);
                for_stmt.visit_mut_children_with(self);
                self.vars.pop();
            }
        } else {
            for_stmt.visit_mut_children_with(self);
        }
    }

    fn _push_var(&mut self, ident: &Ident) {
        if let Some(vars) = self.vars.last_mut() {
            vars.insert(clone_ident(ident));
        }
    }

    fn _push_vars(&mut self, idents: &Vec<&Ident>) {
        if let Some(vars) = self.vars.last_mut() {
            for ident in idents.iter() {
                vars.insert(clone_ident(ident));
            }
        }
    }

    fn _get_fn_scope_vars_record(&mut self) -> Option<&'_ mut VarDeclRecord> {
        let vars_records = &mut self.vars;
        for vars in vars_records.iter_mut().rev() {
            if let VarDeclRecord::Func(_) = vars {
                return Some(vars);
            }
        }

        None
    }
}

/* jsx */
impl JSXTransformVisitor<'_> {
    pub fn new<'a>(
        config: JSXTransformVisitorOptions,
        comments: Option<&'a mut dyn Comments>,
    ) -> JSXTransformVisitor<'a> {
        JSXTransformVisitor {
            imports: LinkedHashMap::new(),
            vars: Vec::new(),
            new_global_vars: LinkedHashMap::new(),
            new_local_vars_vec: Vec::new(),
            _i: 0,
            _j: 0,
            text_pragma: None,
            with_directives_pragma: None,
            case_array_id: None,
            config,
            comments,
        }
    }

    pub fn cast_array(&mut self, mut expr: Expr) -> Expr {
        expr = unwrap_expr_move(expr);
        match &expr {
            Expr::Array(_) => expr,
            _ => quote!(r#"$cast_array($expr)"# as Expr, cast_array: Ident = self._get_cast_array(), expr: Expr = expr),
        }
    }

    pub fn convert_to_array(&self, mut expr: Expr) -> ArrayLit {
        expr = unwrap_expr_move(expr);

        return if let Expr::Array(array) = expr {
            array
        } else {
            ArrayLit { span: expr.get_span(), elems: vec![Some(ExprOrSpread::from(expr))] }
        };
    }

    pub fn convert_to_object_slots(&mut self, mut expr: Expr) -> Expr {
        expr = unwrap_expr_move(expr);

        return if let Expr::Object(_) = &expr {
            expr
        } else {
            let mut is_vue_child = false;

            match &mut expr {
                Expr::Lit(lit) => {
                    if let Lit::Null(_) = lit {
                        return create_null_expr();
                    }
                }
                Expr::Ident(ident) => {
                    if ident.sym == js_word!("undefined") {
                        return create_null_expr();
                    }
                }
                Expr::Unary(unary) => {
                    if let UnaryOp::Void = &unary.op {
                        return match unary.arg.as_ref() {
                            Expr::Lit(_) | Expr::Ident(_) | Expr::This(_) => create_null_expr(),
                            _ => Expr::Object(ObjectLit {
                                span: DUMMY_SP,
                                props: vec![PropOrSpread::Spread(SpreadElement {
                                    dot3_token: DUMMY_SP,
                                    expr: unary.arg.clone(),
                                })],
                            }),
                        };
                    }
                }
                Expr::Array(array) => {
                    if array.elems.is_empty() {
                        return create_null_expr();
                    }
                    if array.elems.len() == 1 {
                        let elem = array.elems.pop();

                        if let Some(elem) = elem {
                            if let Some(elem) = elem {
                                if let None = elem.spread {
                                    expr = unwrap_expr_move(*elem.expr);
                                } else {
                                    array.elems.push(Some(elem));
                                }
                            } else {
                                array.elems.push(None);
                            }
                        }
                    }
                }
                _ => {}
            };

            match &mut expr {
                Expr::Object(_) => {
                    return expr;
                }
                Expr::Fn(_) | Expr::Arrow(_) => {
                    return Expr::Object(ObjectLit {
                        span: DUMMY_SP,
                        props: vec![PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                            key: PropName::Ident(quote_ident!("default")),
                            value: Box::new(expr),
                        })))],
                    });
                }
                Expr::Array(_) | Expr::Lit(_) | Expr::Tpl(_) | Expr::JSXElement(_) | Expr::JSXFragment(_) => {
                    is_vue_child = true
                }
                Expr::Call(call_expr) => {
                    let callee = &call_expr.callee;
                    match callee {
                        Callee::Expr(expr) => {
                            let expr_ref = expr.as_ref();
                            match expr_ref {
                                Expr::Ident(ident) => {
                                    if let Some(pragma) = &self.config.pragma {
                                        if pragma == ident {
                                            is_vue_child = true
                                        }
                                    }

                                    if !is_vue_child {
                                        if let Some(pragma) = &self.text_pragma {
                                            if pragma == ident {
                                                is_vue_child = true
                                            }
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                        _ => {}
                    }
                }
                _ => {}
            };

            if is_vue_child {
                let with_ctx = self.add_import("vue", "withCtx");
                quote!(
                    r#"{ default: $with_ctx(() => ($expr)) }"# as Expr,
                    expr: Expr = Expr::Array(self.convert_to_array(expr)),
                    with_ctx: Ident = with_ctx
                )
            } else {
                quote!(
                    r#"$to_object_slots($expr)"# as Expr,
                    to_object_slots: Ident = self.add_import(HELPER_ID, "toObjectSlots"),
                    expr: Expr = expr
                )
            }
        };
    }

    pub fn resolve_jsx_text(&mut self, jsx_text: JSXText) -> Option<Expr> {
        let test_str = STRIP_EMPTY_JSX_TEXT_REGEX.replace_all(&jsx_text.value.to_string(), "").to_string();

        if test_str.is_empty() {
            return None;
        }

        let expr = quote!(
            r#"$create_text_v_node($text)"# as Expr,
            create_text_v_node: Ident = self._get_create_text_v_node(),
            text: Expr = Expr::from(Str::from(test_str))
        );

        Some(expr)
    }

    pub fn resolve_jsx_children(&mut self, children: &mut Vec<JSXElementChild>) -> ArrayLit {
        let mut elements = Vec::new();

        for child in children.drain(..) {
            match child {
                JSXElementChild::JSXText(jsx_text) => {
                    let expr = self.resolve_jsx_text(jsx_text);
                    if let Some(expr) = expr {
                        elements.push(Some(ExprOrSpread::from(expr)));
                    }
                }
                JSXElementChild::JSXExprContainer(expr_container) => match expr_container.expr {
                    JSXExpr::Expr(expr) => elements.push(Some(ExprOrSpread::from(*expr))),
                    _ => {}
                },
                JSXElementChild::JSXSpreadChild(spread_child) => {
                    elements.push(Some(ExprOrSpread { spread: Some(spread_child.get_span()), expr: spread_child.expr }))
                }
                JSXElementChild::JSXElement(expr) => elements.push(Some(ExprOrSpread::from(Expr::JSXElement(expr)))),
                JSXElementChild::JSXFragment(expr) => elements.push(Some(ExprOrSpread::from(Expr::JSXFragment(expr)))),
            };
        }

        ArrayLit { span: DUMMY_SP, elems: elements }
    }

    pub fn resolve_jsx_tag_name(&mut self, tag_name: &JSXElementName) -> JsxTag {
        let mut tag_ident = None;
        let (str, span) = match tag_name {
            JSXElementName::Ident(ident) => {
                if ident.sym == js_word!("this") {
                    return JsxTag::Expr(Expr::This(ThisExpr { span: ident.get_span() }));
                }

                let name = ast_ident_to_string(ident);
                if is_known_tag(&name) {
                    return JsxTag::String(name);
                }

                tag_ident = Some(clone_ident(ident));

                (name, ident.get_span())
            }
            JSXElementName::JSXMemberExpr(expr) => {
                return JsxTag::Expr(Expr::Member(jsx_member_ref_to_member_expr(expr)));
            }
            JSXElementName::JSXNamespacedName(name) => {
                let ns = &name.ns;
                let name = &name.name;

                (format!("{}:{}", ns.sym, name.sym), name.get_span())
            }
        };

        if let Some(ident) = tag_ident {
            if IS_NOT_ELEMENT_TAG_REGEX.is_match(&str) {
                let ident = if self.contains_var_ident(&ident) { Some(ident) } else { self.get_var_ident(&str) };

                if let Some(ident) = ident {
                    let expr = Expr::Ident(ident);
                    if str == "Fragment" || str == "KeepAlive" || str == "Teleport" {
                        return JsxTag::Fragment(expr);
                    }
                    return JsxTag::Expr(expr);
                }
            }
        }

        let camelize_str = camelize_upper_first(&str);

        if is_builtin_component(&camelize_str) {
            let expr = Expr::Ident(self.add_import("vue", &camelize_str));
            if camelize_str == "Fragment" || camelize_str == "KeepAlive" || camelize_str == "Teleport" {
                return JsxTag::Fragment(expr);
            }
            return JsxTag::Expr(expr);
        }

        return if self.config.is_custom_element.do_filter(&str) {
            JsxTag::String(str)
        } else {
            let resolve_component = self.add_import("vue", "resolveComponent");
            let get_resolved_component_expr = || {
                Expr::Call(CallExpr {
                    span,
                    callee: Callee::Expr(Box::new(resolve_component.into())),
                    args: vec![ExprOrSpread {
                        spread: None,
                        expr: Box::new(Expr::Lit(Lit::Str(Str::from(str.clone())))),
                    }],
                    type_args: None,
                })
            };

            self.with_pure(span.lo);

            JsxTag::Expr(if self.config.optimize {
                Expr::Ident(self.add_local_variable(
                    format!("resolveComponent:{}", &str),
                    move || Some(Box::new(get_resolved_component_expr())),
                    VarDeclKind::Const,
                ))
            } else {
                get_resolved_component_expr()
            })
        };
    }

    pub fn transform_jsx_fragment(&mut self, jsx_fragment: &mut JSXFragment) -> Expr {
        let tag = JsxTag::Fragment(Expr::Ident(self.add_import("vue", "Fragment")));
        let children = self.resolve_jsx_children(&mut jsx_fragment.children);

        self._create_v_node(tag, None, None, vec![], children, None, jsx_fragment.get_span())
    }

    fn _transform_slot_element(&mut self, jsx_element: &mut Box<JSXElement>) -> Expr {
        let attrs = &jsx_element.opening.attrs;
        let children = Expr::Array(self.resolve_jsx_children(&mut jsx_element.children));
        let mut slot_name = None;
        let mut props_array = Vec::new();

        for attr in attrs.iter() {
            match attr {
                JSXAttrOrSpread::JSXAttr(attr) => {
                    let name = match &attr.name {
                        JSXAttrName::Ident(ident) => ast_ident_to_string(ident),
                        JSXAttrName::JSXNamespacedName(name) => {
                            let ns = &name.ns;
                            let name = &name.name;

                            format!("{}:{}", ns.sym, name.sym)
                        }
                        _ => {
                            continue;
                        }
                    };
                    let expression = match &attr.value {
                        Some(value) => match value {
                            JSXAttrValue::Lit(lit) => Some(Expr::Lit(clone_lit(lit))),
                            JSXAttrValue::JSXExprContainer(expr) => match &expr.expr {
                                JSXExpr::Expr(expr) => Some(unwrap_expr_move(expr.as_ref().clone())),
                                _ => None,
                            },
                            JSXAttrValue::JSXElement(element) => Some(Expr::JSXElement(Box::new(JSXElement {
                                span: element.span.clone(),
                                opening: element.opening.clone(),
                                children: element.children.clone(),
                                closing: element.closing.clone(),
                            }))),
                            JSXAttrValue::JSXFragment(fragment) => Some(Expr::JSXFragment(JSXFragment {
                                span: fragment.span.clone(),
                                opening: fragment.opening.clone(),
                                children: fragment.children.clone(),
                                closing: fragment.closing.clone(),
                            })),
                            _ => None,
                        },
                        _ => None,
                    };

                    if name == "name" {
                        slot_name = expression;
                        continue;
                    }

                    let expression = expression.unwrap_or_else(create_true_expr);

                    let camelize_name = camelize(&name);

                    if camelize_name != name {
                        let key = PropName::Str(Str::from(name));
                        let value = private_ident!("value");

                        props_array.push(PropOrSpread::Prop(Box::new(Prop::Getter(GetterProp {
                            span: DUMMY_SP,
                            key: key.clone(),
                            type_ann: None,
                            body: Some(BlockStmt {
                                span: DUMMY_SP,
                                stmts: vec![quote!(
                                    r#"return this[ $key ];"# as Stmt,
                                    key: Expr = Expr::Lit(Lit::Str(Str::from(camelize_name.clone())))
                                )],
                            }),
                        }))));

                        props_array.push(PropOrSpread::Prop(Box::new(Prop::Setter(SetterProp {
                            span: DUMMY_SP,
                            key: key.clone(),
                            param: Pat::Ident(BindingIdent::from(clone_ident(&value))),
                            body: Some(BlockStmt {
                                span: DUMMY_SP,
                                stmts: vec![quote!(
                                    r#"this[ $key ] = $value;"# as Stmt,
                                    value: Ident = value,
                                    key: Expr = Expr::Lit(Lit::Str(Str::from(camelize_name.clone())))
                                )],
                            }),
                        }))));
                    }

                    props_array.push(PropOrSpread::Prop(Box::new(create_key_value_prop(camelize_name, expression))));
                }
                JSXAttrOrSpread::SpreadElement(spread) => {
                    props_array.push({
                        PropOrSpread::Spread(SpreadElement { dot3_token: DUMMY_SP, expr: spread.expr.clone() })
                    });
                }
            };
        }

        let slot_name = slot_name.unwrap_or_else(move || Expr::Lit(Lit::Str(quote_str!("default"))));
        let props = Expr::Object(ObjectLit { span: DUMMY_SP, props: props_array });
        let get_current_instance = self.add_import("vue", "getCurrentInstance");
        let current_instance = self.add_local_variable(
            String::from("getCurrentInstance()"),
            move || quote!(r"$get()" as Option<Box<Expr>>, get = get_current_instance),
            VarDeclKind::Const,
        );

        quote!(
            r#"[($current_instance.slots[$name] || (() => $children))($props)]"# as Expr,
            current_instance: Ident = current_instance,
            name: Expr = slot_name,
            props: Expr = props,
            children: Expr = children
        )
    }

    pub fn transform_jsx_element(&mut self, jsx_element: &mut Box<JSXElement>) -> Expr {
        if self.config.transform_slot {
            if let JSXElementName::Ident(ident) = &jsx_element.opening.name {
                if "slot" == ast_ident_to_string(ident) {
                    return self._transform_slot_element(jsx_element);
                }
            }
        }

        let tag = self.resolve_jsx_tag_name(&jsx_element.opening.name);
        let attrs = &jsx_element.opening.attrs;
        let mut slots_array = Vec::new();
        let mut props_array = Vec::new();
        let mut directives_array = Vec::new();
        let mut constant_idents = HashSet::new();

        let (is_input_tag, is_textarea_tag, is_select_tag) = match &tag {
            JsxTag::String(str) => match str as &str {
                "input" => (true, false, false),
                "textarea" => (false, true, false),
                "select" => (false, false, true),
                _ => (false, false, false),
            },
            _ => (false, false, false),
        };

        let mut input_type: Option<String> = None;
        let mut get_input_type = || match &input_type {
            Some(input_type) => input_type.to_string(),
            _ => {
                let mut result = "text";
                for attr in attrs.iter() {
                    match attr {
                        JSXAttrOrSpread::JSXAttr(attr) => {
                            match &attr.name {
                                JSXAttrName::Ident(ident) => {
                                    let name = ast_ident_to_string(ident);
                                    if name == "type" {
                                        let expr = match &attr.value {
                                            Some(value) => match value {
                                                JSXAttrValue::Lit(lit) => Some(Expr::Lit(clone_lit(lit))),
                                                JSXAttrValue::JSXExprContainer(expr) => match &expr.expr {
                                                    JSXExpr::Expr(expr) => {
                                                        Some(unwrap_expr_move(expr.as_ref().clone()))
                                                    }
                                                    _ => None,
                                                },
                                                _ => None,
                                            },
                                            _ => None,
                                        };

                                        if let Some(expr) = expr {
                                            match &expr {
                                                Expr::Lit(lit) => {
                                                    result = match lit {
                                                        Lit::Str(str) => match &ast_str_to_string(str) as &str {
                                                            "radio" => "radio",
                                                            "checkbox" => "checkbox",
                                                            _ => "text",
                                                        },
                                                        _ => "text",
                                                    }
                                                }
                                                _ => result = "dynamic",
                                            }
                                        }
                                    }
                                }
                                _ => {}
                            };
                        }
                        JSXAttrOrSpread::SpreadElement(_) => result = "dynamic",
                    }
                }
                if result.is_empty() {
                    result = "text";
                }
                let result = result.to_string();
                input_type = Some(result.clone());
                result
            }
        };

        for attr in attrs.iter() {
            match attr {
                JSXAttrOrSpread::JSXAttr(attr) => {
                    let (mut name, name_span) = match &attr.name {
                        JSXAttrName::Ident(ident) => (ast_ident_to_string(ident), ident.get_span()),
                        JSXAttrName::JSXNamespacedName(name) => {
                            let ns = &name.ns;
                            let name = &name.name;

                            (format!("{}:{}", ns.sym, name.sym), name.get_span())
                        }
                        _ => {
                            continue;
                        }
                    };
                    let expression = match &attr.value {
                        Some(value) => match value {
                            JSXAttrValue::Lit(lit) => Some(Expr::Lit(clone_lit(lit))),
                            JSXAttrValue::JSXExprContainer(expr) => match &expr.expr {
                                JSXExpr::Expr(expr) => Some(unwrap_expr_move(expr.as_ref().clone())),
                                _ => None,
                            },
                            JSXAttrValue::JSXElement(element) => Some(Expr::JSXElement(Box::new(JSXElement {
                                span: element.span.clone(),
                                opening: element.opening.clone(),
                                children: element.children.clone(),
                                closing: element.closing.clone(),
                            }))),
                            JSXAttrValue::JSXFragment(fragment) => Some(Expr::JSXFragment(JSXFragment {
                                span: fragment.span.clone(),
                                opening: fragment.opening.clone(),
                                children: fragment.children.clone(),
                                closing: fragment.closing.clone(),
                            })),
                            _ => None,
                        },
                        _ => None,
                    };

                    if self.config.transform_on && name == "on" {
                        if let Some(mut expr) = expression {
                            if let Expr::Object(object) = &mut expr {
                                let mut should_extra_props = true;

                                for prop in object.props.iter() {
                                    match prop {
                                        PropOrSpread::Spread(_) => {
                                            should_extra_props = false;
                                            break;
                                        }
                                        PropOrSpread::Prop(prop) => match prop.as_ref() {
                                            Prop::KeyValue(key_value) => {
                                                if let ObjectKey::Expr(_) = ObjectKey::from(&key_value.key) {
                                                    should_extra_props = false;
                                                    break;
                                                }
                                            }
                                            Prop::Assign(_) | Prop::Getter(_) | Prop::Setter(_) => {
                                                should_extra_props = false;
                                                break;
                                            }
                                            Prop::Method(method) => {
                                                if let ObjectKey::Expr(_) = ObjectKey::from(&method.key) {
                                                    should_extra_props = false;
                                                    break;
                                                }
                                            }
                                            _ => {}
                                        },
                                    }
                                }

                                if should_extra_props {
                                    let mut on_map = LinkedHashMap::new();
                                    for prop in object.props.drain(..) {
                                        if let PropOrSpread::Prop(prop) = prop {
                                            match *prop {
                                                Prop::Shorthand(ident) => {
                                                    on_map.insert(
                                                        String::from("on") + &upper_first(&ast_ident_to_string(&ident)),
                                                        Expr::Ident(clone_ident(&ident)),
                                                    );
                                                }
                                                Prop::KeyValue(key_value) => {
                                                    if let ObjectKey::Str(key) = ObjectKey::from(&key_value.key) {
                                                        on_map.insert(
                                                            String::from("on") + &upper_first(&key),
                                                            *key_value.value,
                                                        );
                                                    }
                                                }
                                                Prop::Method(method) => {
                                                    if let ObjectKey::Str(key) = ObjectKey::from(&method.key) {
                                                        on_map.insert(
                                                            String::from("on") + &upper_first(&key),
                                                            Expr::Fn(FnExpr {
                                                                ident: Some(create_ident(&key)),
                                                                function: method.function,
                                                            }),
                                                        );
                                                    }
                                                }
                                                _ => {}
                                            }
                                        }
                                    }

                                    for (key, value) in on_map.into_iter() {
                                        props_array.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                                            key: PropName::Str(Str::from(key)),
                                            value: Box::new(value),
                                        }))));
                                    }

                                    continue;
                                }
                            }

                            props_array.push(PropOrSpread::Spread(SpreadElement {
                                dot3_token: DUMMY_SP,
                                expr: Box::new(Expr::Call(CallExpr {
                                    span: DUMMY_SP,
                                    callee: Callee::Expr(Box::from(Expr::Ident(
                                        self.add_import(HELPER_ID, "transformOn"),
                                    ))),
                                    args: vec![ExprOrSpread { spread: None, expr: Box::new(expr) }],
                                    type_args: None,
                                })),
                            }))
                        }

                        continue;
                    }

                    if self.config.react_style {
                        match &name as &str {
                            "tabIndex" => name = String::from("tabindex"),
                            "className" => name = String::from("class"),
                            "htmlFor" => name = String::from("for"),
                            "dangerouslySetInnerHTML" => name = String::from("v-html"),
                            "onDoubleClick" => name = String::from("onDblClick"),
                            "onChange" => {
                                if is_input_tag || is_textarea_tag {
                                    name = String::from("onInput")
                                }
                            }
                            _ => {}
                        }
                    }

                    if self.config.transform_on_update_event {
                        if let Some(caps) = ON_UPDATE_EVENT_REGEX.captures(&name) {
                            name = String::from("onUpdate:")
                                + &(match &caps.get(1) {
                                    None => String::from(""),
                                    Some(m) => m.as_str().to_lowercase(),
                                })
                                + (match &caps.get(2) {
                                    None => "",
                                    Some(m) => m.as_str(),
                                });
                        }
                    }

                    if IS_VUE_DIRECTIVE_REGEX.is_match(&name) {
                        if name.starts_with("v-") {
                            name = name[2..].to_string();
                        } else {
                            name = name[1..].to_string();
                        }

                        let mut arg = None;
                        let mut modifiers = Vec::new();
                        let split_n: Vec<&str> = name.splitn(3, ":").collect();
                        let split_0 = split_n.get(0).unwrap_or(&"").to_string();
                        let split_1 = split_n.get(1);

                        if let Some(split_1) = split_1 {
                            let split_1 = split_1.to_string();
                            let mut split: Vec<&str> = split_1.split("_").collect();

                            name = split_0.to_string();
                            arg = Some(split.remove(0).to_string());

                            for modifier in split.iter() {
                                if !modifier.is_empty() {
                                    modifiers.push(modifier.to_string());
                                }
                            }
                        } else {
                            let mut split: Vec<&str> = split_0.split("_").collect();

                            name = split.remove(0).to_string();

                            for modifier in split.iter() {
                                if !modifier.is_empty() {
                                    modifiers.push(modifier.to_string());
                                }
                            }
                        }

                        name = lower_first(&name);

                        let modifiers = HashSet::from_iter(modifiers.into_iter());

                        match &name as &str {
                            "slots" => {
                                if let Some(expr) = expression {
                                    slots_array.push(expr)
                                }
                            }
                            "text" => props_array.push(PropOrSpread::Prop(Box::from(create_key_value_prop(
                                String::from("textContent"),
                                expression.unwrap_or_else(create_void_zero_expr),
                            )))),
                            "html" => props_array.push(PropOrSpread::Prop(Box::from(create_key_value_prop(
                                String::from("innerHTML"),
                                expression.unwrap_or_else(create_void_zero_expr),
                            )))),
                            _ => {
                                if self.config.v_on && name == "on" {
                                    if let Some(arg) = arg {
                                        if !arg.is_empty() {
                                            let expression = if let Some(expression) = expression {
                                                match &expression {
                                                    Expr::Fn(_) | Expr::Arrow(_) => Some(expression),
                                                    _ => {
                                                        if self.is_vue_strip_expr(&expression) {
                                                            None
                                                        } else {
                                                            Some(quote!(
                                                                r#"$merge ( $expr )"# as Expr,
                                                                merge: Ident = self.add_import(HELPER_ID, "mergeFn")
                                                            ))
                                                        }
                                                    }
                                                }
                                            } else {
                                                None
                                            };

                                            let (event, expression) =
                                                event_helpers::gen_handler(arg, expression, modifiers);

                                            props_array.push(PropOrSpread::Prop(Box::new(create_key_value_prop(
                                                format!("on{}", upper_first(&event)),
                                                expression.unwrap_or_else(create_true_expr),
                                            ))));
                                        }
                                    }
                                } else if self.config.v_model && (name == "model" || name == "models") {
                                    if let Some(mut expr) = expression {
                                        expr = unwrap_expr_move(expr);

                                        let mut raw_models = Vec::new();
                                        let arg = arg.as_ref();

                                        fn add_model(
                                            models: &mut Vec<(Expr, VModelArgument, HashSet<String>)>,
                                            expr: Expr,
                                            mut modifiers: HashSet<String>,
                                            default_name: Option<&String>,
                                        ) {
                                            match &expr {
                                                Expr::Ident(_) | Expr::Member(_) => {
                                                    models.push((
                                                        expr,
                                                        VModelArgument::Str(match default_name {
                                                            Some(name) => String::from(name),
                                                            _ => String::from("modelValue"),
                                                        }),
                                                        modifiers,
                                                    ));

                                                    return;
                                                }
                                                _ => {}
                                            }

                                            match expr {
                                                Expr::Array(mut array) => {
                                                    if array.elems.is_empty() {
                                                        return;
                                                    }
                                                    let mut left = None;
                                                    if let Some(source) = array.elems.remove(0) {
                                                        if let None = source.spread {
                                                            let expr = source.expr;
                                                            match expr.as_ref() {
                                                                Expr::Ident(_) | Expr::Member(_) => left = Some(expr),
                                                                _ => {}
                                                            }
                                                        }
                                                    }

                                                    match left {
                                                        None => {
                                                            return;
                                                        }
                                                        Some(left) => {
                                                            let mut model_arg: Option<VModelArgument> = None;
                                                            let mut modifiers_array = None;

                                                            if !array.elems.is_empty() {
                                                                if let Some(param1) = array.elems.remove(0) {
                                                                    if let None = param1.spread {
                                                                        match unwrap_expr(param1.expr.as_ref()) {
                                                                            Expr::Lit(expr1) => {
                                                                                if let Lit::Str(str) = expr1 {
                                                                                    model_arg =
                                                                                        Some(VModelArgument::Str(
                                                                                            ast_str_to_string(str),
                                                                                        ))
                                                                                }
                                                                            }
                                                                            Expr::Array(expr1) => {
                                                                                modifiers_array = Some(ArrayLit {
                                                                                    span: expr1.span.clone(),
                                                                                    elems: expr1.elems.clone(),
                                                                                })
                                                                            }
                                                                            _ => {
                                                                                model_arg = Some(VModelArgument::Expr(
                                                                                    param1.expr.clone(),
                                                                                ))
                                                                            }
                                                                        }
                                                                    }
                                                                }

                                                                if !array.elems.is_empty() {
                                                                    if let None = modifiers_array {
                                                                        if let Some(param2) = array.elems.remove(0) {
                                                                            if let None = param2.spread {
                                                                                if let Expr::Array(expr2) =
                                                                                    unwrap_expr_move(*param2.expr)
                                                                                {
                                                                                    modifiers_array = Some(expr2)
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }

                                                            if let Some(modifiers_array) = modifiers_array {
                                                                for elem in modifiers_array.elems.iter() {
                                                                    if let Some(elem) = elem {
                                                                        if let None = elem.spread {
                                                                            if let Expr::Lit(elem) =
                                                                                unwrap_expr(elem.expr.as_ref())
                                                                            {
                                                                                if let Lit::Str(str) = elem {
                                                                                    modifiers
                                                                                        .insert(ast_str_to_string(str));
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }

                                                            let model_arg = match model_arg {
                                                                Some(name) => name,
                                                                _ => VModelArgument::Str(match default_name {
                                                                    Some(name) => String::from(name),
                                                                    _ => String::from("modelValue"),
                                                                }),
                                                            };

                                                            models.push((left.as_ref().clone(), model_arg, modifiers))
                                                        }
                                                    }
                                                }
                                                _ => {}
                                            }
                                        }

                                        match &name as &str {
                                            "model" => {
                                                add_model(&mut raw_models, expr, modifiers, arg);
                                            }
                                            "models" => {
                                                if let Expr::Array(mut array) = expr {
                                                    for expr in array.elems.drain(..) {
                                                        if let Some(expr) = expr {
                                                            if let None = expr.spread {
                                                                add_model(
                                                                    &mut raw_models,
                                                                    *expr.expr,
                                                                    modifiers.clone(),
                                                                    arg,
                                                                );
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                            _ => {}
                                        }

                                        for (left_expr, mut model_arg, modifiers) in raw_models.into_iter() {
                                            if let VModelArgument::Expr(model_expr) = &model_arg {
                                                match model_expr.as_ref() {
                                                    Expr::This(_) | Expr::Ident(_) | Expr::Lit(_) => {}
                                                    _ => {
                                                        let once_identifier =
                                                            self.add_import(HELPER_ID, "onceArrowFunctionWithNoArgs");
                                                        let model_expr_box = model_expr.clone();
                                                        let model_expr = *model_expr_box;
                                                        let span = model_expr.get_span();

                                                        let ident = self.add_temp_variable(
                                                            move || {
                                                                let mut result = quote!(
                                                                    r"$once(() => ( $expr ))" as Expr,
                                                                    once: Ident = once_identifier,
                                                                    expr: Expr = model_expr
                                                                );
                                                                result.set_span(span);
                                                                Some(Box::new(result))
                                                            },
                                                            VarDeclKind::Const,
                                                        );

                                                        self.with_pure(span.lo);

                                                        model_arg =
                                                            VModelArgument::Expr(Box::new(Expr::Call(CallExpr {
                                                                span: DUMMY_SP,
                                                                callee: Callee::Expr(Box::new(Expr::Ident(ident))),
                                                                args: vec![],
                                                                type_args: None,
                                                            })));
                                                    }
                                                }
                                            }

                                            props_array.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(
                                                KeyValueProp {
                                                    key: model_arg.to_prop_name_with_prefix("onUpdate:"),
                                                    value: quote!(
                                                        r#"( $event ) => void (($left) = $event) "# as Box<Expr>,
                                                        event: Ident = clone_ident(&event_helpers::EVENT_IDENT),
                                                        left: Expr = left_expr.clone()
                                                    ),
                                                },
                                            ))));

                                            let mut v_model_directive = None;

                                            if let VModelArgument::Str(_) = &model_arg {
                                                if is_select_tag {
                                                    v_model_directive = Some(self.add_import("vue", "vModelSelect"));
                                                } else if is_textarea_tag {
                                                    v_model_directive = Some(self.add_import("vue", "vModelText"));
                                                } else if is_input_tag {
                                                    let input_type = get_input_type();
                                                    match &input_type as &str {
                                                        "checkbox" => {
                                                            v_model_directive =
                                                                Some(self.add_import("vue", "vModelCheckbox"));
                                                        }
                                                        "radio" => {
                                                            v_model_directive =
                                                                Some(self.add_import("vue", "vModelRadio"));
                                                        }
                                                        "text" | "number" | "tel" | "search" | "email" | "url" => {
                                                            v_model_directive =
                                                                Some(self.add_import("vue", "vModelText"));
                                                        }
                                                        _ => {
                                                            v_model_directive =
                                                                Some(self.add_import("vue", "vModelDynamic"));
                                                        }
                                                    }
                                                }
                                            }

                                            let modifiers_expr = if modifiers.is_empty() {
                                                None
                                            } else {
                                                let modifier = self.generate_modifiers_object(&modifiers);
                                                if let Expr::Ident(ident) = &modifier {
                                                    constant_idents.insert(clone_ident(ident));
                                                }
                                                Some(modifier)
                                            };

                                            if let Some(v_model_directive) = v_model_directive {
                                                directives_array.push(Some(ExprOrSpread {
                                                    spread: None,
                                                    expr: quote!(
                                                        r#"[$directive, ($expression), undefined, $modifiers]"# as Box<Expr>,
                                                        directive: Ident = v_model_directive,
                                                        expression: Expr = left_expr,
                                                        modifiers: Expr =
                                                            modifiers_expr.unwrap_or_else(create_void_zero_expr)
                                                    ),
                                                }));
                                            } else {
                                                if let Some(modifier_expr) = modifiers_expr {
                                                    props_array.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(
                                                        KeyValueProp {
                                                            key: model_arg.to_prop_name_with_suffix("Modifiers"),
                                                            value: Box::new(modifier_expr),
                                                        },
                                                    ))));
                                                }
                                                props_array.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(
                                                    KeyValueProp {
                                                        key: model_arg.to_prop_name(),
                                                        value: Box::new(left_expr),
                                                    },
                                                ))));
                                            }
                                        }
                                    }
                                } else if self.config.transform_v_slot && name == "slot" {
                                    if let Some(arg) = arg {
                                        let expression = expression.unwrap_or_else(create_void_zero_expr);

                                        slots_array.push(Expr::Object(ObjectLit {
                                            span: DUMMY_SP,
                                            props: vec![PropOrSpread::Prop(Box::new(create_key_value_prop(
                                                arg,
                                                match unwrap_expr(&expression) {
                                                    Expr::Fn(_) | Expr::Arrow(_) => expression,
                                                    Expr::Array(_) | Expr::Lit(_) | Expr::Tpl(_) => {
                                                        let with_ctx = self.add_import("vue", "withCtx");
                                                        quote!(
                                                            r#"() => $with_ctx($children)"# as Expr,
                                                            children: Expr =
                                                                Expr::Array(self.convert_to_array(expression)),
                                                            with_ctx: Ident = with_ctx
                                                        )
                                                    }
                                                    _ => quote!(
                                                        r#"$to_slot($expr)"# as Expr,
                                                        to_slot: Ident = self.add_import(HELPER_ID, "toSlot"),
                                                        expr: Expr = expression
                                                    ),
                                                },
                                            )))],
                                        }))
                                    }
                                } else {
                                    let directive_expr = match &name as &str {
                                        "show" => Expr::Ident(self.add_import("vue", "vShow")),
                                        _ => {
                                            let ident = self.get_var_ident(&format!("v{}", upper_first(&name)));

                                            if let Some(ident) = ident {
                                                Expr::Ident(ident)
                                            } else {
                                                let resolve_directive = self.add_import("vue", "resolveDirective");

                                                let get_resolved_directive_expr = || {
                                                    Expr::Call(CallExpr {
                                                        span: name_span.clone(),
                                                        callee: Callee::Expr(Box::new(resolve_directive.into())),
                                                        args: vec![ExprOrSpread {
                                                            spread: None,
                                                            expr: Box::new(Expr::Lit(Lit::Str(Str::from(
                                                                name.to_string(),
                                                            )))),
                                                        }],
                                                        type_args: None,
                                                    })
                                                };

                                                self.with_pure(name_span.lo);

                                                if self.config.optimize {
                                                    Expr::Ident(self.add_local_variable(
                                                        format!("resolve-directive:{}", &name),
                                                        move || Some(Box::from(get_resolved_directive_expr())),
                                                        VarDeclKind::Const,
                                                    ))
                                                } else {
                                                    get_resolved_directive_expr()
                                                }
                                            }
                                        }
                                    };
                                    let argument_expr = if let Some(arg) = arg {
                                        Expr::Lit(Lit::Str(Str::from(arg)))
                                    } else {
                                        create_void_zero_expr()
                                    };
                                    let modifiers_expr = if modifiers.is_empty() {
                                        create_void_zero_expr()
                                    } else {
                                        let modifiers_expr = self.generate_modifiers_object(&modifiers);
                                        if let Expr::Ident(ident) = &modifiers_expr {
                                            constant_idents.insert(clone_ident(ident));
                                        }
                                        modifiers_expr
                                    };

                                    directives_array.push(Some(ExprOrSpread {
                                        spread: None,
                                        expr: quote!(
                                            r#"[$directive, ($expression), $argument, $modifiers]"# as Box<Expr>,
                                            directive: Expr = directive_expr,
                                            expression: Expr = expression.unwrap_or_else(create_true_expr),
                                            argument: Expr = argument_expr,
                                            modifiers: Expr = modifiers_expr
                                        ),
                                    }))
                                }
                            }
                        }
                    } else {
                        props_array.push(PropOrSpread::Prop(Box::new(create_key_value_prop(
                            name,
                            expression.unwrap_or_else(create_true_expr),
                        ))))
                    }
                }
                JSXAttrOrSpread::SpreadElement(spread) => props_array
                    .push(PropOrSpread::Spread(SpreadElement { dot3_token: DUMMY_SP, expr: spread.expr.clone() })),
            }
        }

        let slots = if slots_array.is_empty() {
            None
        } else {
            let mut slots_items = Vec::new();
            for item in slots_array.into_iter() {
                slots_items.push(self.convert_to_object_slots(item));
            }
            let slots = self.merge_objects(slots_items);
            match &slots {
                Expr::Object(object) => {
                    if object.props.is_empty() {
                        None
                    } else {
                        Some(slots)
                    }
                }
                _ => Some(slots),
            }
        };

        let props = if props_array.is_empty() {
            None
        } else {
            let expressions = vec![Expr::Object(ObjectLit { span: DUMMY_SP, props: props_array })];
            let props = if self.config.merge_props {
                self.merge_v_node_props(expressions)
            } else {
                self.merge_objects(expressions)
            };
            match &props {
                Expr::Object(object) => {
                    if object.props.is_empty() {
                        None
                    } else {
                        Some(props)
                    }
                }
                _ => Some(props),
            }
        };

        let children = self.resolve_jsx_children(&mut jsx_element.children);

        self._create_v_node(
            tag,
            props,
            slots,
            directives_array,
            children,
            Some(constant_idents),
            jsx_element.get_span(),
        )
    }

    pub fn handle_hmr(&mut self, module: &mut Module) {
        if !self.config.hmr {
            return;
        }

        let mut components = LinkedHashMap::new();
        let mut exports = LinkedHashMap::new();
        let mut extra_items = Vec::new();

        for item in module.body.iter_mut() {
            let mut exported = false;
            let mut var_decl_stmt = None;

            match item {
                ModuleItem::ModuleDecl(decl) => match decl {
                    ModuleDecl::ExportDecl(decl) => match &decl.decl {
                        Decl::Var(var) => {
                            exported = true;
                            var_decl_stmt = Some(var)
                        }
                        _ => {
                            continue;
                        }
                    },
                    ModuleDecl::ExportNamed(named) => {
                        if let None = named.src {
                            if !named.type_only {
                                for spec in named.specifiers.iter() {
                                    match spec {
                                        ExportSpecifier::Named(named) => {
                                            if !named.is_type_only {
                                                let local = match &named.orig {
                                                    ModuleExportName::Ident(ident) => ast_ident_to_string(ident),
                                                    ModuleExportName::Str(str) => ast_str_to_string(str),
                                                };

                                                let export_name = if let Some(name) = &named.exported {
                                                    match name {
                                                        ModuleExportName::Ident(ident) => ast_ident_to_string(ident),
                                                        ModuleExportName::Str(str) => ast_str_to_string(str),
                                                    }
                                                } else {
                                                    local.clone()
                                                };

                                                exports.insert(export_name, local);
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }
                        continue;
                    }
                    ModuleDecl::ExportDefaultExpr(export_default) => {
                        if let Expr::Ident(ident) = export_default.expr.as_ref() {
                            exports.insert(String::from("default"), ast_ident_to_string(ident));
                        } else if is_define_component_expr(export_default.expr.as_ref()) {
                            let ident = create_private_ident("__default__");

                            *item = quote!(
                                r#"const $local = $expr;"# as ModuleItem,
                                local: Ident = clone_ident(&ident),
                                expr: Expr = export_default.expr.as_ref().clone()
                            );

                            extra_items.push(quote!(
                                r#"export { $local as default };"# as ModuleItem,
                                local: Ident = clone_ident(&ident)
                            ));

                            components.insert(
                                String::from("default"),
                                DefineComponentInfo { local: ident, exports: vec![String::from("default")] },
                            );
                        }
                        continue;
                    }
                    ModuleDecl::TsExportAssignment(export_assignment) => {
                        if let Expr::Ident(ident) = export_assignment.expr.as_ref() {
                            exports.insert(String::from("default"), ast_ident_to_string(ident));
                        } else {
                            let ident = create_private_ident("__export_assignment__");

                            *item = quote!(
                                r#"const $local = $expr;"# as ModuleItem,
                                local: Ident = clone_ident(&ident),
                                expr: Expr = export_assignment.expr.as_ref().clone()
                            );

                            extra_items.push(ModuleItem::ModuleDecl(ModuleDecl::TsExportAssignment(
                                TsExportAssignment { span: DUMMY_SP, expr: Box::new(Expr::Ident(clone_ident(&ident))) },
                            )));

                            components.insert(
                                String::from("default"),
                                DefineComponentInfo { local: ident, exports: vec![String::from("default")] },
                            );
                        }
                        continue;
                    }
                    _ => {
                        continue;
                    }
                },
                ModuleItem::Stmt(stmt) => {
                    if let Stmt::Decl(decl) = stmt {
                        if let Decl::Var(var_decl) = decl {
                            var_decl_stmt = Some(var_decl);
                        } else {
                            continue;
                        }
                    } else {
                        continue;
                    }
                }
                _ => {
                    continue;
                }
            };

            if let Some(_var_decl_stmt) = var_decl_stmt {
                for decl in _var_decl_stmt.decls.iter() {
                    if let Pat::Ident(ident) = &decl.name {
                        if let Some(expr) = &decl.init {
                            if is_define_component_expr(expr.as_ref()) {
                                let mut exports = vec![];
                                if exported {
                                    exports.push(ast_ident_to_string(&ident.id));
                                }

                                components.insert(
                                    ast_ident_to_string(&ident.id),
                                    DefineComponentInfo { local: clone_ident(&ident.id), exports },
                                );
                            }
                        }
                    }
                }
            }
        }

        for (exported, local) in exports.iter() {
            let comp = components.get_mut(local);
            if let Some(comp) = comp {
                comp.exports.push(exported.to_string());
            }
        }

        module.body.append(&mut extra_items);

        if components.is_empty() {
            return;
        }

        let ssr_register_helper_identifier = self.add_import(HELPER_ID, "ssrRegisterHelper");
        let ssr_identifier = self.add_import(HELPER_ID, "ssr");
        let mut create_record_expressions = Vec::new();
        let mut reload_record_expressions = Vec::new();
        let mut callback_param_binds_pattern = Vec::new();
        let mut ssr_register_helper_expressions = Vec::new();
        let __VUE_HMR_RUNTIME__ = quote_ident!("__VUE_HMR_RUNTIME__");
        let __MODULE__ = private_ident!("__MODULE__");

        for (_, comp) in components {
            let DefineComponentInfo { local, mut exports } = comp;
            if let Some(export_name) = exports.pop() {
                let id_str = {
                    let mut hasher = DefaultHasher::new();
                    export_name.hash(&mut hasher);
                    self.config.file_name.hash(&mut hasher);
                    format!("{:X}", hasher.finish() & 0xFFFFFFFF)
                };

                create_record_expressions.push(quote!(
                    r#"$__VUE_HMR_RUNTIME__.createRecord($id, $local)"# as Box<Expr>,
                    __VUE_HMR_RUNTIME__: Ident = clone_ident(&__VUE_HMR_RUNTIME__),
                    id: Expr = Expr::Lit(Lit::Str(Str::from(id_str.to_string()))),
                    local: Ident = clone_ident(&local)
                ));

                reload_record_expressions.push(quote!(
                    r#"$__VUE_HMR_RUNTIME__.reloadRecord($id, $local)"# as  Box<Expr>,
                    __VUE_HMR_RUNTIME__: Ident = clone_ident(&__VUE_HMR_RUNTIME__),
                    id: Expr = Expr::Lit(Lit::Str(Str::from(id_str.to_string()))),
                    local: Ident = clone_ident(&local)
                ));

                callback_param_binds_pattern.push(ObjectPatProp::KeyValue(KeyValuePatProp {
                    key: PropName::Ident(create_ident(&export_name)),
                    value: Box::new(if export_name == "default" {
                        Pat::Assign(AssignPat {
                            span: DUMMY_SP,
                            left: Box::new(Pat::Ident(BindingIdent::from(clone_ident(&local)))),
                            right: Box::new(Expr::Ident(clone_ident(&__MODULE__))),
                            type_ann: None,
                        })
                    } else {
                        Pat::Ident(BindingIdent::from(clone_ident(&local)))
                    }),
                }));

                ssr_register_helper_expressions.push(quote!(
                    r#"$ssr_register_helper($local, $__MODULE__)"# as Box<Expr>,
                    __MODULE__: Ident = clone_ident(&__MODULE__),
                    ssr_register_helper: Ident = clone_ident(&ssr_register_helper_identifier),
                    local: Ident = clone_ident(&local),
                ));
            }
        }

        let hmr_stmt: Stmt = quote!(
            r#"
                if ($ssr_ident) {
                    const $__MODULE__ = $file_name;
                    $ssr_register_helper_expressions;
                } else if (typeof $__VUE_HMR_RUNTIME__ !== 'undefined') {
                    $create_record_expressions;
                    const accept_callback = ($__MODULE__) => {
                        if ($__MODULE__) {
                            const $object_pattern = $__MODULE__; 
                            $reload_record_expressions;
                        }
                    };

                    if (import.meta.hot) {
                        const hot = import.meta.hot;
                        hot?.accept?.(accept_callback);
                    } else if (import.meta.webpackHot) {
                        const hot = import.meta.webpackHot;
                        hot?.accept?.((err, { module }) => accept_callback(module.exports));
                    }
                }
            "# as Stmt,
            __MODULE__: Ident = clone_ident(&__MODULE__),
            __VUE_HMR_RUNTIME__: Ident = clone_ident(&__VUE_HMR_RUNTIME__),
            ssr_ident: Ident = clone_ident(&ssr_identifier),
            file_name: Expr = Expr::Lit(Lit::Str(Str::from(self.config.file_name.to_string()))),
            object_pattern: Pat = Pat::Object(ObjectPat {
                span: DUMMY_SP,
                props: callback_param_binds_pattern,
                optional: false,
                type_ann: None
            }),
            create_record_expressions: Expr = Expr::Seq(SeqExpr { span: DUMMY_SP, exprs: create_record_expressions }),
            reload_record_expressions: Expr = Expr::Seq(SeqExpr { span: DUMMY_SP, exprs: reload_record_expressions }),
            ssr_register_helper_expressions: Expr =
                Expr::Seq(SeqExpr { span: DUMMY_SP, exprs: ssr_register_helper_expressions })
        );

        module.body.push(ModuleItem::Stmt(hmr_stmt));
    }

    pub fn merge_objects(&mut self, mut objects: Vec<Expr>) -> Expr {
        if objects.len() == 1 {
            return objects.pop().unwrap();
        }
        let mut properties = Vec::new();

        for mut object in objects.into_iter() {
            object = unwrap_expr_move(object);

            match object {
                Expr::Object(mut object) => {
                    for prop in object.props.drain(..) {
                        properties.push(prop);
                    }
                }
                Expr::Lit(_) | Expr::Tpl(_) => {}
                _ => {
                    if match &mut object {
                        Expr::Unary(unary) => {
                            let op = &unary.op;
                            let arg = &*unary.arg;
                            match op {
                                UnaryOp::Void => match arg {
                                    Expr::Lit(_) | Expr::Ident(_) | Expr::This(_) => false,
                                    _ => true,
                                },
                                _ => true,
                            }
                        }
                        Expr::Ident(ident) => ident.sym != js_word!("undefined"),
                        _ => true,
                    } {
                        properties
                            .push(PropOrSpread::Spread(SpreadElement { dot3_token: DUMMY_SP, expr: Box::new(object) }));
                    }
                }
            }
        }

        if properties.len() == 1 {
            let prop = properties.get_mut(0).unwrap();
            match prop {
                PropOrSpread::Spread(spread) => {
                    return (*spread.expr).clone();
                }
                _ => {}
            }
        }

        Expr::Object(ObjectLit { span: DUMMY_SP, props: properties })
    }

    pub fn merge_array_props(&mut self, items: Vec<Expr>, strip: bool) -> Expr {
        let mut elements = Vec::new();

        for mut element in items.into_iter() {
            element = unwrap_expr_move(element);

            if strip && self.is_vue_strip_expr(&element) {
                continue;
            }

            match &mut element {
                Expr::Array(array) => {
                    for elem in array.elems.drain(..) {
                        if let Some(elem) = elem {
                            if strip {
                                let elem_expr = unwrap_expr(&elem.expr);

                                if !self.is_vue_strip_expr(elem_expr) {
                                    elements.push(Some(elem));
                                }
                            } else {
                                elements.push(Some(elem));
                            }
                        }
                    }
                }
                Expr::Object(_) | Expr::Lit(_) | Expr::Tpl(_) | Expr::Fn(_) | Expr::Arrow(_) | Expr::Class(_) => {
                    elements.push(Some(ExprOrSpread::from(element)))
                }
                _ => {
                    match &mut element {
                        Expr::Unary(_) => {
                            elements.push(Some(ExprOrSpread::from(element.clone())));
                            continue;
                        }
                        Expr::Call(call) => {
                            let callee = &call.callee;
                            match callee {
                                Callee::Import(_) => {
                                    elements.push(Some(ExprOrSpread::from(element.clone())));
                                    continue;
                                }
                                Callee::Expr(expr_box) => {
                                    let expr = &**expr_box;

                                    match expr {
                                        Expr::Ident(ident) => match &self.case_array_id {
                                            None => {}
                                            Some(case_array_id) => {
                                                if *case_array_id == *ident {
                                                    elements.push(Some(ExprOrSpread {
                                                        spread: Some(DUMMY_SP),
                                                        expr: Box::new(element.clone()),
                                                    }));
                                                    continue;
                                                }
                                            }
                                        },
                                        _ => {}
                                    }
                                }
                                _ => {}
                            }
                        }
                        _ => {}
                    }

                    elements
                        .push(Some(ExprOrSpread { spread: Some(DUMMY_SP), expr: Box::new(self.cast_array(element)) }));
                }
            }
        }

        if elements.len() == 1 {
            let elem = elements.get_mut(0).unwrap();
            match elem {
                Some(elem) => {
                    return (*elem.expr).clone();
                }
                _ => {}
            }
        }

        Expr::Array(ArrayLit { span: DUMMY_SP, elems: elements })
    }

    pub fn merge_v_node_props(&mut self, props_objects: Vec<Expr>) -> Expr {
        let object = self.merge_objects(props_objects);
        return match object {
            Expr::Object(mut object) => {
                let mut items = vec![PropsItem::Map(LinkedHashMap::new())];

                for prop in object.props.drain(..) {
                    match prop {
                        PropOrSpread::Spread(spread) => {
                            items.push(PropsItem::Spread(spread));
                        }
                        PropOrSpread::Prop(prop) => {
                            let prop_ref = prop.as_ref();
                            match prop_ref {
                                Prop::Shorthand(shorthand) => {
                                    self._set_map_item_expr(
                                        &mut items,
                                        ObjectKey::Str(shorthand.to_string()),
                                        Expr::from(clone_ident(shorthand)),
                                    );
                                }
                                Prop::KeyValue(key_value) => {
                                    let key = ObjectKey::from(&key_value.key);
                                    let value = &*key_value.value;

                                    self._set_map_item_expr(&mut items, key, value.clone());
                                }
                                Prop::Getter(getter) => {
                                    let key = ObjectKey::from(&getter.key);
                                    self._set_map_item_accessor(&mut items, key, Some(prop), None);
                                }
                                Prop::Setter(setter) => {
                                    let key = ObjectKey::from(&setter.key);
                                    self._set_map_item_accessor(&mut items, key, None, Some(prop));
                                }
                                Prop::Method(method) => {
                                    let key = ObjectKey::from(&method.key);
                                    let function = &method.function;
                                    self._set_map_item_expr(
                                        &mut items,
                                        key,
                                        Expr::Fn(FnExpr {
                                            ident: None,
                                            function: Function {
                                                params: function.params.clone(),
                                                decorators: function.decorators.clone(),
                                                span: function.span.clone(),
                                                body: function.body.clone(),
                                                is_generator: function.is_generator,
                                                is_async: function.is_async,
                                                type_params: function.type_params.clone(),
                                                return_type: function.return_type.clone(),
                                            },
                                        }),
                                    )
                                }
                                _ => {}
                            }
                        }
                    }
                }

                let mut args = Vec::new();

                let mut is_object = false;

                for item in items.into_iter() {
                    match item {
                        PropsItem::Map(map) => {
                            if !map.is_empty() {
                                let mut props = Vec::new();
                                for (key, value) in map.into_iter() {
                                    match value {
                                        PropsItemMapItem::Normal(expr) => {
                                            props.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                                                key: key.to_prop_name(),
                                                value: Box::new(expr),
                                            }))));
                                        }
                                        PropsItemMapItem::Accessor((getter, setter)) => {
                                            if let Some(getter) = getter {
                                                props.push(PropOrSpread::Prop(getter))
                                            }
                                            if let Some(setter) = setter {
                                                props.push(PropOrSpread::Prop(setter))
                                            }
                                        }
                                    }
                                }

                                is_object = true;

                                args.push(ExprOrSpread {
                                    spread: None,
                                    expr: Box::new(Expr::Object(ObjectLit { span: DUMMY_SP, props })),
                                })
                            }
                        }
                        PropsItem::Spread(expr) => args.push(ExprOrSpread { spread: None, expr: expr.expr }),
                    }
                }

                if args.len() == 1 && is_object {
                    return args.pop().unwrap().expr.as_ref().clone();
                }

                Expr::Call(CallExpr {
                    span: DUMMY_SP,
                    callee: Callee::Expr(Box::new(Expr::Ident(self.add_import("vue", "mergeProps")))),
                    args,
                    type_args: None,
                })
            }
            _ => object,
        };
    }

    pub fn is_need_merge_key(&self, key: &str) -> bool {
        IS_NEED_MERGE_KEY_REGEX.is_match(key)
    }

    pub fn is_vue_strip_expr(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Lit(lit) => match lit {
                Lit::Bool(_) | Lit::Null(_) => true,
                _ => false,
            },
            Expr::Unary(unary) => match &unary.op {
                UnaryOp::Void => match &*unary.arg {
                    Expr::Lit(_) | Expr::Ident(_) | Expr::This(_) => true,
                    _ => false,
                },
                _ => false,
            },
            Expr::Ident(ident) => ident.sym == js_word!("undefined"),
            _ => false,
        }
    }

    pub fn generate_modifiers_object(&mut self, modifiers: &HashSet<String>) -> Expr {
        let mut modifiers: Vec<String> = modifiers.clone().into_iter().collect();
        modifiers.sort();

        Expr::Ident(self.add_global_variable(
            String::from("modifiers:") + &modifiers.join(","),
            move || {
                Some(Box::new(Expr::Object({
                    let mut props = Vec::with_capacity(modifiers.len());
                    for modifier in modifiers.into_iter() {
                        props.push(PropOrSpread::Prop(Box::new(create_key_value_prop(modifier, create_true_expr()))))
                    }
                    ObjectLit { span: DUMMY_SP, props }
                })))
            },
            VarDeclKind::Const,
        ))
    }

    pub fn generate_array_props(&mut self, array_props: &HashSet<String>) -> Expr {
        let mut array_props: Vec<String> = array_props.clone().into_iter().collect();
        array_props.sort();

        Expr::Ident(self.add_global_variable(
            String::from("array_props:") + &array_props.join(","),
            move || {
                Some(Box::new(Expr::Array({
                    let mut elems = Vec::with_capacity(array_props.len());
                    for prop in array_props.into_iter() {
                        elems.push(Some(ExprOrSpread {
                            spread: None,
                            expr: Box::new(Expr::Lit(Lit::Str(Str::from(prop)))),
                        }));
                    }
                    ArrayLit { span: DUMMY_SP, elems }
                })))
            },
            VarDeclKind::Const,
        ))
    }

    fn _create_v_node(
        &mut self,
        tag: JsxTag,
        props: Option<Expr>,
        slots: Option<Expr>,
        directives: Vec<Option<ExprOrSpread>>,
        children: ArrayLit,
        constant_idents: Option<HashSet<Ident>>,
        span: Span,
    ) -> Expr {
        let is_element = match &tag {
            JsxTag::Fragment(_) | JsxTag::String(_) => true,
            JsxTag::Expr(_) => false,
        };

        let mut children_param: Expr;
        let mut props_param = props.unwrap_or_else(create_null_expr);
        let mut children_expr = if self.config.enable_object_slots {
            if children.elems.is_empty() {
                create_null_expr()
            } else {
                Expr::Array(children)
            }
        } else {
            Expr::Array(children)
        };

        if let Some(slots) = slots {
            children_expr = self.convert_to_object_slots(children_expr);
            children_param = self.merge_objects(vec![slots, children_expr]);
        } else if !is_element && self.config.enable_object_slots {
            children_param = self.convert_to_object_slots(children_expr);
        } else {
            children_param = children_expr;
        }

        if let Expr::Array(array) = &mut children_param {
            if array.elems.is_empty() {
                children_param = create_null_expr()
            } else if array.elems.len() == 1 {
                let mut result = create_null_expr();
                match array.elems.pop() {
                    Some(elem1) => match elem1 {
                        Some(elem1) => {
                            if let Some(_) = &elem1.spread {
                                result = Expr::Array(ArrayLit { span: array.get_span(), elems: vec![Some(elem1)] })
                            } else {
                                match unwrap_expr(elem1.expr.as_ref()) {
                                    Expr::Object(_) | Expr::Array(_) => {
                                        result = elem1.expr.as_ref().clone();
                                    }
                                    _ => {
                                        result =
                                            Expr::Array(ArrayLit { span: array.get_span(), elems: vec![Some(elem1)] });
                                    }
                                };
                            }
                        }
                        _ => {}
                    },
                    _ => {}
                };

                children_param = result;
            }
        }

        if let Expr::Object(object) = &mut children_param {
            if object.props.is_empty() {
                children_param = create_null_expr()
            } else if self.config.optimize {
                let mut optimize_slots = true;
                for prop in object.props.iter() {
                    if let PropOrSpread::Prop(prop) = prop {
                        match prop.as_ref() {
                            Prop::Shorthand(_) => {
                                continue;
                            }
                            Prop::KeyValue(key_value) => {
                                if let ObjectKey::Str(_) = ObjectKey::from(&key_value.key) {
                                    if let Expr::Arrow(_) = &key_value.value.as_ref() {
                                        continue;
                                    }
                                }
                            }
                            _ => {}
                        }
                    }

                    optimize_slots = false;
                    break;
                }

                if optimize_slots {
                    object.props.push(PropOrSpread::Prop(Box::new(create_key_value_prop(
                        String::from("_"),
                        Expr::Lit(Lit::Num(Number::from(SlotFlags::STABLE as f64))),
                    ))))
                }
            }
        }

        match &mut props_param {
            Expr::Object(object) => {
                if object.props.is_empty() {
                    props_param = create_null_expr()
                }
            }
            _ => {}
        }

        let mut optimize_params = Vec::new();

        if self.config.optimize && !self.is_vue_strip_expr(&props_param) {
            let mut patch_flags = 0;
            let mut dynamic_props = None;
            match &props_param {
                Expr::Object(object) => {
                    let mut dyn_props = HashSet::new();
                    let mut has_dyn_key = false;
                    let mut has_ref = false;

                    for prop in object.props.iter() {
                        match prop {
                            PropOrSpread::Spread(_) => {
                                has_dyn_key = true;
                                break;
                            }
                            PropOrSpread::Prop(prop) => {
                                let mut check_dyn_key = |key: &PropName| {
                                    if let ObjectKey::Str(str) = ObjectKey::from(key) {
                                        match &str as &str {
                                            "ref" => has_ref = true,
                                            _ => {
                                                dyn_props.insert(str);
                                            }
                                        }
                                        false
                                    } else {
                                        has_dyn_key = true;
                                        true
                                    }
                                };

                                match prop.as_ref() {
                                    Prop::Shorthand(ident) => {
                                        let key = ast_ident_to_string(ident);
                                        match &key as &str {
                                            "ref" => has_ref = true,
                                            _ => {
                                                dyn_props.insert(key);
                                            }
                                        }
                                    }
                                    Prop::KeyValue(key_value) => {
                                        if let ObjectKey::Str(str) = ObjectKey::from(&key_value.key) {
                                            match &str as &str {
                                                "ref" => has_ref = true,
                                                _ => {
                                                    if !is_constant_expr(key_value.value.as_ref(), &constant_idents) {
                                                        dyn_props.insert(str);
                                                    }
                                                }
                                            }
                                        } else {
                                            has_dyn_key = true;
                                            break;
                                        }
                                    }
                                    Prop::Getter(method) => {
                                        if check_dyn_key(&method.key) {
                                            break;
                                        }
                                    }
                                    Prop::Setter(method) => {
                                        if check_dyn_key(&method.key) {
                                            break;
                                        }
                                    }
                                    Prop::Method(method) => {
                                        if check_dyn_key(&method.key) {
                                            break;
                                        }
                                    }
                                    _ => {}
                                }
                            }
                            _ => {}
                        }
                    }

                    if has_dyn_key {
                        patch_flags = PatchFlags::FULL_PROPS;
                    } else {
                        let mut has_hydration_event_binding = false;
                        let mut has_class_binding = false;
                        let mut has_style_binding = false;

                        for key in dyn_props.iter() {
                            match key as &str {
                                "ref" => {
                                    has_ref = true;
                                }
                                "class" => {
                                    has_class_binding = true;
                                }
                                "style" => {
                                    has_style_binding = true;
                                }
                                _ => {
                                    if is_element 
                                        && IS_ON_REGEX.is_match(key)  
                                        && /* omit v-model handlers */ key != "onUpdate:modelValue"
                                        && key.to_lowercase() != "onclick" {
                                        has_hydration_event_binding = true
                                    }
                                }
                            }
                        }

                        dyn_props.remove("class");
                        dyn_props.remove("style");
                        dyn_props.remove("ref");
                        dyn_props.remove("key");

                        if has_class_binding {
                            patch_flags |= PatchFlags::CLASS
                        }
                        if has_style_binding {
                            patch_flags |= PatchFlags::STYLE
                        }
                        if !dyn_props.is_empty() {
                            patch_flags |= PatchFlags::PROPS
                        }
                        if has_hydration_event_binding {
                            patch_flags |= PatchFlags::HYDRATE_EVENTS
                        }
                        if (patch_flags == 0 || patch_flags == PatchFlags::HYDRATE_EVENTS)
                            && (has_ref || !directives.is_empty())
                        {
                            patch_flags |= PatchFlags::NEED_PATCH
                        }

                        dynamic_props = Some(dyn_props);
                    }
                }
                _ => {
                    patch_flags = PatchFlags::FULL_PROPS;
                }
            }

            optimize_params.push(ExprOrSpread {
                spread: None,
                expr: Box::new(Expr::Lit(Lit::Num(Number::from(patch_flags as f64)))),
            });

            if let Some(dynamic_props) = dynamic_props {
                optimize_params
                    .push(ExprOrSpread { spread: None, expr: Box::new(self.generate_array_props(&dynamic_props)) });
            }
        }

        let mut params = vec![
            // tag
            ExprOrSpread { spread: None, expr: Box::new(tag.into_expr()) },
            // props
            ExprOrSpread { spread: None, expr: Box::new(props_param) },
            // children
            ExprOrSpread { spread: None, expr: Box::new(children_param) },
        ];

        params.append(&mut optimize_params);

        let mut v_node = Expr::Call(CallExpr {
            span: span.clone(),
            callee: Callee::Expr(Box::new(Expr::Ident(self._get_create_v_node()))),
            args: params,
            type_args: None,
        });

        if !directives.is_empty() {
            v_node = Expr::Call(CallExpr {
                span: span.clone(),
                callee: Callee::Expr(Box::new(self._get_with_directives().into())),
                args: vec![
                    ExprOrSpread { spread: None, expr: Box::new(v_node) },
                    ExprOrSpread {
                        spread: None,
                        expr: Box::new(Expr::Array(ArrayLit { span: DUMMY_SP, elems: directives })),
                    },
                ],
                type_args: None,
            });
        }

        self.with_pure(span.lo);

        v_node
    }

    fn _get_create_text_v_node(&mut self) -> Ident {
        match &self.text_pragma {
            Some(ident) => clone_ident(ident),
            None => {
                let ident = self.add_import("vue", "createTextVNode");
                self.text_pragma = Some(clone_ident(&ident));
                ident
            }
        }
    }

    fn _get_with_directives(&mut self) -> Ident {
        match &self.with_directives_pragma {
            Some(ident) => clone_ident(ident),
            None => {
                let ident = self.add_import("vue", "withDirectives");
                self.with_directives_pragma = Some(clone_ident(&ident));
                ident
            }
        }
    }

    fn _get_create_v_node(&mut self) -> Ident {
        match &self.config.pragma {
            Some(ident) => clone_ident(ident),
            None => {
                let ident = self.add_import("vue", "createVNode");
                self.config.pragma = Some(clone_ident(&ident));
                ident
            }
        }
    }

    fn _get_cast_array(&mut self) -> Ident {
        match &self.case_array_id {
            Some(ident) => clone_ident(ident),
            None => {
                let ident = self.add_import(HELPER_ID, "castArray");
                self.case_array_id = Some(clone_ident(&ident));
                ident
            }
        }
    }

    fn _get_props_item_map<'a>(
        &self,
        items: &'a mut Vec<PropsItem>,
    ) -> &'a mut LinkedHashMap<ObjectKey, PropsItemMapItem> {
        match items.last() {
            Some(item) => match item {
                PropsItem::Map(_) => {}
                _ => {
                    items.push(PropsItem::Map(LinkedHashMap::new()));
                }
            },
            _ => {}
        }

        match items.last_mut() {
            Some(item) => match item {
                PropsItem::Map(item) => {
                    return item;
                }
                _ => {}
            },
            _ => {}
        }

        panic!()
    }

    fn _set_map_item_expr(&mut self, items: &mut Vec<PropsItem>, key: ObjectKey, value: Expr) {
        let map = self._get_props_item_map(items);
        let old = map.get_mut(&key);
        match old {
            None => {
                map.insert(key, PropsItemMapItem::Normal(value));
            }
            Some(old) => {
                *old = PropsItemMapItem::Normal(match key {
                    ObjectKey::Str(key) => match old {
                        PropsItemMapItem::Normal(old) => {
                            if self.is_need_merge_key(&key) {
                                self.merge_array_props(vec![old.clone(), value], true)
                            } else {
                                value
                            }
                        }
                        _ => value,
                    },
                    _ => value,
                });
            }
        };
    }

    fn _set_map_item_accessor(
        &mut self,
        items: &mut Vec<PropsItem>,
        key: ObjectKey,
        getter: Option<Box<Prop>>,
        setter: Option<Box<Prop>>,
    ) {
        let map = self._get_props_item_map(items);
        let old = map.get_mut(&key);
        match old {
            None => {
                map.insert(key, PropsItemMapItem::Accessor((getter, setter)));
            }
            Some(old) => match old {
                PropsItemMapItem::Normal(_) => *old = PropsItemMapItem::Accessor((getter, setter)),
                PropsItemMapItem::Accessor(_) => {
                    if let Some(item) = map.remove(&key) {
                        if let PropsItemMapItem::Accessor((old_getter, old_setter)) = item {
                            map.insert(
                                key,
                                PropsItemMapItem::Accessor((
                                    match &getter {
                                        None => old_getter,
                                        Some(_) => getter,
                                    },
                                    match &setter {
                                        None => old_setter,
                                        Some(_) => setter,
                                    },
                                )),
                            );
                        }
                    };
                }
            },
        };
    }
}

impl Default for JSXTransformVisitor<'_> {
    fn default() -> Self {
        Self {
            imports: LinkedHashMap::new(),
            vars: Vec::new(),
            new_global_vars: LinkedHashMap::new(),
            new_local_vars_vec: Vec::new(),
            _i: 0,
            _j: 0,
            text_pragma: None,
            with_directives_pragma: None,
            case_array_id: None,
            config: JSXTransformVisitorOptions::default(),
            comments: None,
        }
    }
}

impl VisitMut for JSXTransformVisitor<'_> {
    /* base */
    fn visit_mut_stmts(&mut self, stmts: &mut Vec<Stmt>) {
        let (idents, fn_scope_idents) = find_decl_ident_from_stmts(stmts);

        if let Some(vars_record) = self.vars.last_mut() {
            for ident in idents {
                vars_record.insert(ident);
            }
        }

        let fn_scope_vars_record = match self._get_fn_scope_vars_record() {
            Some(record) => Some(record),
            _ => self.vars.last_mut(),
        };
        if let Some(fn_scope_vars_record) = fn_scope_vars_record {
            for ident in fn_scope_idents {
                fn_scope_vars_record.insert(ident);
            }
        }

        stmts.visit_mut_children_with(self);
    }

    fn visit_mut_module_items(&mut self, items: &mut Vec<ModuleItem>) {
        let (idents, fn_scope_idents) = find_decl_ident_from_module_items(items);

        if let Some(vars_record) = self.vars.last_mut() {
            for ident in idents {
                vars_record.insert(ident);
            }
        }

        let fn_scope_vars_record = match self._get_fn_scope_vars_record() {
            Some(record) => Some(record),
            _ => self.vars.last_mut(),
        };
        if let Some(fn_scope_vars_record) = fn_scope_vars_record {
            for ident in fn_scope_idents {
                fn_scope_vars_record.insert(ident);
            }
        }

        items.visit_mut_children_with(self);
    }

    fn visit_mut_for_in_stmt(&mut self, for_stmt: &mut ForInStmt) {
        self._visit_mut_for__children_with(for_stmt);
    }
    fn visit_mut_for_of_stmt(&mut self, for_stmt: &mut ForOfStmt) {
        self._visit_mut_for__children_with(for_stmt);
    }
    fn visit_mut_for_stmt(&mut self, for_stmt: &mut ForStmt) {
        self._visit_mut_for__children_with(for_stmt);
    }

    fn visit_mut_block_stmt(&mut self, block: &mut BlockStmt) {
        self.vars.push(VarDeclRecord::new(false));
        block.visit_mut_children_with(self);
        self.vars.pop();
    }

    fn visit_mut_class_expr(&mut self, class: &mut ClassExpr) {
        if let Some(ident) = &class.ident {
            self._push_var(ident);
        }

        class.visit_mut_children_with(self);
    }

    fn visit_mut_ts_module_decl(&mut self, decl: &mut TsModuleDecl) {
        self._visit_mut_function_scope__children_with(decl);
    }

    fn visit_mut_script(&mut self, script: &mut Script) {
        self._visit_mut_function_scope__children_with(script);
    }

    fn visit_mut_module(&mut self, module: &mut Module) {
        self._visit_mut_function_scope__children_with(module);
        self.handle_hmr(module);
    }

    fn visit_mut_arrow_expr(&mut self, n: &mut ArrowExpr) {
        self._visit_mut_function_scope__children_with(n);
    }

    fn visit_mut_class_method(&mut self, n: &mut ClassMethod) {
        self._visit_mut_function_scope__children_with(n);
    }

    fn visit_mut_private_method(&mut self, n: &mut PrivateMethod) {
        self._visit_mut_function_scope__children_with(n);
    }

    fn visit_mut_constructor(&mut self, n: &mut Constructor) {
        self._visit_mut_function_scope__children_with(n);
    }

    fn visit_mut_fn_decl(&mut self, n: &mut FnDecl) {
        self._visit_mut_function_scope__children_with(n);
    }

    fn visit_mut_fn_expr(&mut self, n: &mut FnExpr) {
        self._visit_mut_function_scope__children_with(n);
    }

    fn visit_mut_getter_prop(&mut self, n: &mut GetterProp) {
        self._visit_mut_function_scope__children_with(n);
    }

    fn visit_mut_setter_prop(&mut self, n: &mut SetterProp) {
        self._visit_mut_function_scope__children_with(n);
    }

    fn visit_mut_method_prop(&mut self, n: &mut MethodProp) {
        self._visit_mut_function_scope__children_with(n);
    }

    fn visit_mut_program(&mut self, program: &mut Program) {
        program.visit_mut_children_with(self);
        self.end_helper(program);
    }

    fn visit_mut_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::JSXElement(jsx_element) => {
                *expr = self.transform_jsx_element(jsx_element);
            }
            Expr::JSXFragment(jsx_fragment) => {
                *expr = self.transform_jsx_fragment(jsx_fragment);
            }
            _ => {}
        }

        expr.visit_mut_children_with(self);
    }

    fn visit_mut_jsx_element_child(&mut self, child: &mut JSXElementChild) {
        match child {
            JSXElementChild::JSXElement(jsx_element) => {
                *child = JSXElementChild::JSXExprContainer(JSXExprContainer {
                    span: DUMMY_SP,
                    expr: JSXExpr::Expr(Box::new(self.transform_jsx_element(jsx_element))),
                });
            }
            JSXElementChild::JSXFragment(jsx_fragment) => {
                *child = JSXElementChild::JSXExprContainer(JSXExprContainer {
                    span: DUMMY_SP,
                    expr: JSXExpr::Expr(Box::new(self.transform_jsx_fragment(jsx_fragment))),
                });
            }
            _ => {}
        }

        child.visit_mut_children_with(self);
    }

    fn visit_mut_jsx_attr_value(&mut self, value: &mut JSXAttrValue) {
        match value {
            JSXAttrValue::JSXElement(jsx_element) => {
                *value = JSXAttrValue::JSXExprContainer(JSXExprContainer {
                    span: DUMMY_SP,
                    expr: JSXExpr::Expr(Box::new(self.transform_jsx_element(jsx_element))),
                });
            }
            JSXAttrValue::JSXFragment(jsx_fragment) => {
                *value = JSXAttrValue::JSXExprContainer(JSXExprContainer {
                    span: DUMMY_SP,
                    expr: JSXExpr::Expr(Box::new(self.transform_jsx_fragment(jsx_fragment))),
                });
            }
            _ => {}
        }

        value.visit_mut_children_with(self);
    }
}
