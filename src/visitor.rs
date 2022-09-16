use std::{
    collections::{hash_map::DefaultHasher, HashMap, HashSet},
    hash::{Hash, Hasher},
};

use lazy_static::lazy_static;
use linked_hash_map::LinkedHashMap;
use regex::Regex;
use swc_core::{
    common::{
        comments::{Comments, NoopComments},
        Span, DUMMY_SP,
    },
    ecma::{
        ast::{
            self, ArrayLit, ArrayPat, ArrowExpr, AssignPat, BigInt, BindingIdent, BlockStmt,
            BlockStmtOrExpr, Bool, CallExpr, Callee, ClassExpr, ClassMethod, ComputedPropName,
            Constructor, Decl, ExportSpecifier, Expr, ExprOrSpread, FnDecl, FnExpr, ForInStmt,
            ForOfStmt, ForStmt, Function, GetterProp, Ident, ImportDecl, ImportNamedSpecifier,
            ImportSpecifier, JSXAttrName, JSXAttrOrSpread, JSXAttrValue, JSXElement,
            JSXElementChild, JSXElementName, JSXExpr, JSXExprContainer, JSXFragment, JSXMemberExpr,
            JSXObject, JSXText, KeyValuePatProp, KeyValueProp, Lit, MemberExpr, MemberProp,
            MethodProp, Module, ModuleDecl, ModuleExportName, ModuleItem, Null, Number, ObjectLit,
            ObjectPat, ObjectPatProp, Param, ParamOrTsParamProp, Pat, PrivateMethod, Program, Prop,
            PropName, PropOrSpread, ReturnStmt, Script, SeqExpr, SetterProp, SpreadElement, Stmt,
            Str, ThisExpr, TsExportAssignment, TsModuleDecl, TsModuleName, TsNamespaceBody,
            TsParamPropParam, UnaryExpr, UnaryOp, VarDecl, VarDeclKind, VarDeclOrExpr,
            VarDeclOrPat, VarDeclarator,
        },
        atoms::js_word,
        utils::{private_ident, quote_ident},
        visit::{noop_visit_type, Visit, VisitMut, VisitMutWith, VisitWith},
    },
    quote,
};

use crate::{
    constants::{is_builtin_component, is_known_tag},
    flags::{PatchFlags, SlotFlags},
    hashmap_str_key_get_mut_default,
    utils::{camelize, camelize_upper_first, lower_first, upper_first, StringFilter},
};

const HELPER_ID: &str = "swc-plugin-transform-vue3-jsx/helpers";

lazy_static! {
    static ref STRIP_EMPTY_JSX_TEXT_REGEX: Regex = Regex::new(r"(\n[ \t]*)*").unwrap();
    static ref IS_NEED_MERGE_KEY_REGEX: Regex = Regex::new(r"^(class|style|on[^a-z].*)$").unwrap();
    static ref IS_NOT_ELEMENT_TAG_REGEX: Regex = Regex::new(r"^[^a-z0-9_]").unwrap();
    static ref IS_VUE_DIRECTIVE_REGEX: Regex = Regex::new(r"^v(-[a-zA-Z]|[A-Z])").unwrap();
    static ref ON_UPDATE_EVENT_REGEX: Regex = Regex::new(r"^onUpdate([A-Z])(\S*)$").unwrap();
}

pub enum JsxTag {
    Fragment(Expr),
    String(String),
    Expr(Expr),
}

enum PropsItemMapItem {
    Normal(Expr),
    Accessor((Option<Box<Prop>>, Option<Box<Prop>>)),
}

enum PropsItem {
    Map(LinkedHashMap<ObjectKey, PropsItemMapItem>),
    Spread(SpreadElement),
}

enum ObjectKey {
    Str(String),
    Expr(Expr),
}

enum VarDeclRecord {
    Func((HashSet<Ident>, HashMap<String, Ident>)),
    Block((HashSet<Ident>, HashMap<String, Ident>)),
}

impl VarDeclRecord {
    fn new(is_function_scope: bool) -> VarDeclRecord {
        if is_function_scope {
            VarDeclRecord::Func((HashSet::new(), HashMap::new()))
        } else {
            VarDeclRecord::Block((HashSet::new(), HashMap::new()))
        }
    }
    fn insert(&mut self, ident: Ident) -> bool {
        let (set, map) = match self {
            VarDeclRecord::Func(item) => item,
            VarDeclRecord::Block(item) => item,
        };

        map.insert(ast_ident_to_string(&ident), clone_ident(&ident));
        set.insert(ident)
    }
    fn contains(&self, ident: &Ident) -> bool {
        let (set, _) = match self {
            VarDeclRecord::Func(item) => item,
            VarDeclRecord::Block(item) => item,
        };

        set.contains(ident)
    }
    fn get_ident(&self, name: &str) -> Option<&Ident> {
        let (_, map) = match self {
            VarDeclRecord::Func(item) => item,
            VarDeclRecord::Block(item) => item,
        };

        map.get(name)
    }
}

impl Hash for ObjectKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            ObjectKey::Str(str) => str.hash(state),
            ObjectKey::Expr(expr) => expr.hash(state),
        }
    }
}

impl Eq for ObjectKey {}

impl PartialEq<Self> for ObjectKey {
    fn eq(&self, other: &Self) -> bool {
        match self {
            ObjectKey::Str(str) => match other {
                ObjectKey::Str(other_str) => *other_str == *str,
                _ => false,
            },
            ObjectKey::Expr(expr) => match other {
                ObjectKey::Expr(other_expr) => *other_expr == *expr,
                _ => false,
            },
        }
    }
}

impl ObjectKey {
    pub fn to_prop_name(&self) -> PropName {
        match self {
            ObjectKey::Str(str) => PropName::Str(Str::from(str.to_string())),
            ObjectKey::Expr(expr) => PropName::Computed(ComputedPropName {
                span: DUMMY_SP,
                expr: Box::new(expr.clone()),
            }),
        }
    }
}

impl From<&PropName> for ObjectKey {
    fn from(name: &PropName) -> Self {
        match name {
            PropName::Ident(ident) => ObjectKey::Str(ast_ident_to_string(ident)),
            PropName::Str(str) => ObjectKey::Str(ast_str_to_string(str)),
            PropName::Num(num) => {
                let f64_val = num.value;
                let f32_val = f64_val as f32;

                if f64_val == (f32_val as f64) {
                    ObjectKey::Str(num.value.to_string())
                } else {
                    ObjectKey::Expr(Expr::Lit(Lit::Num(Number::from(f64_val))))
                }
            }
            PropName::BigInt(bigint) => {
                let value = &bigint.value;
                match value.to_biguint() {
                    None => ObjectKey::Expr(Expr::Lit(Lit::BigInt(BigInt::from(value.clone())))),
                    Some(value) => ObjectKey::Str(value.to_string()),
                }
            }
            PropName::Computed(computed) => {
                let name = unwrap_expr(computed.expr.as_ref());

                match name {
                    Expr::Lit(lit) => match lit {
                        Lit::Str(str) => ObjectKey::Str(ast_str_to_string(str)),
                        Lit::Num(num) => {
                            let f64_val = num.value;
                            let f32_val: f32 = f64_val as f32;

                            if f64_val == (f32_val as f64) {
                                ObjectKey::Str(num.value.to_string())
                            } else {
                                ObjectKey::Expr(Expr::Lit(Lit::Num(Number::from(f64_val))))
                            }
                        }
                        Lit::BigInt(bigint) => {
                            let value = &bigint.value;
                            match value.to_biguint() {
                                None => ObjectKey::Expr(Expr::Lit(Lit::BigInt(BigInt::from(
                                    value.clone(),
                                )))),
                                Some(value) => ObjectKey::Str(value.to_string()),
                            }
                        }
                        _ => ObjectKey::Expr(name.clone()),
                    },
                    _ => ObjectKey::Expr(name.clone()),
                }
            }
        }
    }
}

impl JsxTag {
    fn into_expr(self) -> Expr {
        match self {
            JsxTag::Fragment(expr) => expr,
            JsxTag::String(str) => Expr::Lit(Lit::Str(Str::from(str))),
            JsxTag::Expr(expr) => expr,
        }
    }
}

pub(crate) trait FunctionScope {
    fn _get_params_iter(&self) -> Option<std::slice::Iter<'_, Param>> {
        None
    }
    fn _get_params_iter_mut(&mut self) -> Option<std::slice::IterMut<'_, Param>> {
        None
    }
    fn _get_params(&self) -> Option<Vec<Param>> {
        None
    }
    fn _get_ident(&self) -> Option<&Ident> {
        None
    }
    fn _is_decl_stmt(&self) -> bool {
        false
    }
    fn _prepend_var_decls<Iter: Iterator<Item = (VarDeclKind, Ident, Option<Box<Expr>>)>>(
        &mut self,
        _var_decls: Iter,
    ) {
    }
}

pub(crate) trait ForLike {
    fn _get_init_decl(&self) -> Option<&'_ VarDecl> {
        None
    }
}

trait DrainValues<V> {
    fn drain_values(&mut self) -> Vec<V>;
}

impl<K: Hash + Eq, V> DrainValues<V> for HashMap<K, V> {
    fn drain_values(&mut self) -> Vec<V> {
        let mut values = Vec::with_capacity(self.len());

        for (_key, value) in self.drain() {
            values.push(value)
        }

        values
    }
}

impl<K: Hash + Eq, V> DrainValues<V> for LinkedHashMap<K, V> {
    fn drain_values(&mut self) -> Vec<V> {
        let mut values = Vec::new();

        for (_key, value) in self.drain() {
            values.push(value)
        }

        values
    }
}

fn _prepend_var_decls_into_stmts<Iter: Iterator<Item = (VarDeclKind, Ident, Option<Box<Expr>>)>>(
    stmts: &mut Vec<Stmt>,
    var_decls: Iter,
) {
    let mut insert_index = stmts.len();

    for (i, stmt) in stmts.iter().enumerate() {
        if !is_script_top(stmt) {
            insert_index = i;
            break;
        }
    }

    for (kind, ident, init) in var_decls {
        stmts.insert(
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

fn _prepend_var_decls_into_module_items<
    Iter: Iterator<Item = (VarDeclKind, Ident, Option<Box<Expr>>)>,
>(
    items: &mut Vec<ModuleItem>,
    var_decls: Iter,
) {
    let mut insert_index = items.len();

    for (i, item) in items.iter().enumerate() {
        if !is_module_top(item) {
            insert_index = i;
            break;
        }
    }

    for (kind, ident, init) in var_decls {
        items.insert(
            insert_index,
            ModuleItem::Stmt(Stmt::Decl(Decl::Var(VarDecl {
                span: DUMMY_SP,
                kind,
                declare: false,
                decls: vec![VarDeclarator {
                    span: DUMMY_SP,
                    name: Pat::Ident(BindingIdent::from(ident)),
                    init,
                    definite: false,
                }],
            }))),
        );
        insert_index += 1;
    }
}

fn _prepend_var_decls_into_block_stmt<
    Iter: Iterator<Item = (VarDeclKind, Ident, Option<Box<Expr>>)>,
>(
    block: &mut BlockStmt,
    var_decls: Iter,
) {
    _prepend_var_decls_into_stmts(&mut block.stmts, var_decls);
}

fn _prepend_var_decls_into_option_block_stmt<
    Iter: Iterator<Item = (VarDeclKind, Ident, Option<Box<Expr>>)>,
>(
    block: &mut Option<BlockStmt>,
    var_decls: Iter,
) {
    if let Some(block) = block {
        _prepend_var_decls_into_block_stmt(block, var_decls);
    }
}

impl FunctionScope for MethodProp {
    fn _get_params_iter(&self) -> Option<std::slice::Iter<'_, Param>> {
        Some(self.function.params.iter())
    }
    fn _get_params_iter_mut(&mut self) -> Option<std::slice::IterMut<'_, Param>> {
        Some(self.function.params.iter_mut())
    }
    fn _prepend_var_decls<Iter: Iterator<Item = (VarDeclKind, Ident, Option<Box<Expr>>)>>(
        &mut self,
        var_decls: Iter,
    ) {
        _prepend_var_decls_into_option_block_stmt(&mut self.function.body, var_decls);
    }
}

impl FunctionScope for ClassMethod {
    fn _get_params_iter(&self) -> Option<std::slice::Iter<'_, Param>> {
        Some(self.function.params.iter())
    }
    fn _get_params_iter_mut(&mut self) -> Option<std::slice::IterMut<'_, Param>> {
        Some(self.function.params.iter_mut())
    }
    fn _prepend_var_decls<Iter: Iterator<Item = (VarDeclKind, Ident, Option<Box<Expr>>)>>(
        &mut self,
        var_decls: Iter,
    ) {
        _prepend_var_decls_into_option_block_stmt(&mut self.function.body, var_decls);
    }
}

impl FunctionScope for PrivateMethod {
    fn _get_params_iter(&self) -> Option<std::slice::Iter<'_, Param>> {
        Some(self.function.params.iter())
    }
    fn _get_params_iter_mut(&mut self) -> Option<std::slice::IterMut<'_, Param>> {
        Some(self.function.params.iter_mut())
    }
    fn _prepend_var_decls<Iter: Iterator<Item = (VarDeclKind, Ident, Option<Box<Expr>>)>>(
        &mut self,
        var_decls: Iter,
    ) {
        _prepend_var_decls_into_option_block_stmt(&mut self.function.body, var_decls);
    }
}

impl FunctionScope for Constructor {
    fn _get_params(&self) -> Option<Vec<Param>> {
        let mut params = Vec::new();
        for param in self.params.iter() {
            match param {
                ParamOrTsParamProp::TsParamProp(param) => match &param.param {
                    TsParamPropParam::Ident(ident) => params.push(Param {
                        span: param.span.clone(),
                        decorators: vec![],
                        pat: Pat::Ident(ident.clone()),
                    }),
                    TsParamPropParam::Assign(asset) => params.push(Param {
                        span: param.span.clone(),
                        decorators: vec![],
                        pat: Pat::Assign(AssignPat {
                            span: asset.span.clone(),
                            left: asset.left.clone(),
                            right: asset.right.clone(),
                            type_ann: asset.type_ann.clone(),
                        }),
                    }),
                },
                ParamOrTsParamProp::Param(param) => params.push(Param {
                    span: param.span.clone(),
                    decorators: param.decorators.clone(),
                    pat: param.pat.clone(),
                }),
            }
        }
        Some(params)
    }
    fn _prepend_var_decls<Iter: Iterator<Item = (VarDeclKind, Ident, Option<Box<Expr>>)>>(
        &mut self,
        var_decls: Iter,
    ) {
        _prepend_var_decls_into_option_block_stmt(&mut self.body, var_decls);
    }
}

impl FunctionScope for GetterProp {
    fn _prepend_var_decls<Iter: Iterator<Item = (VarDeclKind, Ident, Option<Box<Expr>>)>>(
        &mut self,
        var_decls: Iter,
    ) {
        _prepend_var_decls_into_option_block_stmt(&mut self.body, var_decls);
    }
}

impl FunctionScope for SetterProp {
    fn _get_params(&self) -> Option<Vec<Param>> {
        Some(vec![Param {
            span: DUMMY_SP,
            decorators: vec![],
            pat: self.param.clone(),
        }])
    }
    fn _prepend_var_decls<Iter: Iterator<Item = (VarDeclKind, Ident, Option<Box<Expr>>)>>(
        &mut self,
        var_decls: Iter,
    ) {
        _prepend_var_decls_into_option_block_stmt(&mut self.body, var_decls);
    }
}

impl FunctionScope for FnExpr {
    fn _get_params_iter(&self) -> Option<std::slice::Iter<'_, Param>> {
        Some(self.function.params.iter())
    }
    fn _get_params_iter_mut(&mut self) -> Option<std::slice::IterMut<'_, Param>> {
        Some(self.function.params.iter_mut())
    }
    fn _get_ident(&self) -> Option<&Ident> {
        match &self.ident {
            None => None,
            Some(ident) => Some(ident),
        }
    }
    fn _prepend_var_decls<Iter: Iterator<Item = (VarDeclKind, Ident, Option<Box<Expr>>)>>(
        &mut self,
        var_decls: Iter,
    ) {
        _prepend_var_decls_into_option_block_stmt(&mut self.function.body, var_decls);
    }
}

impl FunctionScope for FnDecl {
    fn _get_params_iter(&self) -> Option<std::slice::Iter<'_, Param>> {
        Some(self.function.params.iter())
    }
    fn _get_params_iter_mut(&mut self) -> Option<std::slice::IterMut<'_, Param>> {
        Some(self.function.params.iter_mut())
    }
    fn _get_ident(&self) -> Option<&Ident> {
        Some(&self.ident)
    }
    fn _is_decl_stmt(&self) -> bool {
        true
    }
    fn _prepend_var_decls<Iter: Iterator<Item = (VarDeclKind, Ident, Option<Box<Expr>>)>>(
        &mut self,
        var_decls: Iter,
    ) {
        _prepend_var_decls_into_option_block_stmt(&mut self.function.body, var_decls);
    }
}

impl FunctionScope for ArrowExpr {
    fn _get_params(&self) -> Option<Vec<Param>> {
        let mut result = Vec::with_capacity(self.params.len());

        for pat in self.params.iter() {
            result.push(Param {
                span: DUMMY_SP,
                decorators: vec![],
                pat: pat.clone(),
            })
        }
        Some(result)
    }
    fn _prepend_var_decls<Iter: Iterator<Item = (VarDeclKind, Ident, Option<Box<Expr>>)>>(
        &mut self,
        var_decls: Iter,
    ) {
        match &mut self.body {
            BlockStmtOrExpr::BlockStmt(block) => {
                _prepend_var_decls_into_block_stmt(block, var_decls);
            }
            BlockStmtOrExpr::Expr(expr) => {
                let mut block = BlockStmt {
                    span: DUMMY_SP,
                    stmts: vec![Stmt::Return(ReturnStmt {
                        span: DUMMY_SP,
                        arg: Some(expr.clone()),
                    })],
                };

                _prepend_var_decls_into_block_stmt(&mut block, var_decls);

                self.body = BlockStmtOrExpr::BlockStmt(block);
            }
            _ => {}
        };
    }
}

impl FunctionScope for TsModuleDecl {
    fn _get_ident(&self) -> Option<&Ident> {
        match &self.id {
            TsModuleName::Ident(ident) => Some(ident),
            _ => None,
        }
    }
    fn _is_decl_stmt(&self) -> bool {
        true
    }
    fn _prepend_var_decls<Iter: Iterator<Item = (VarDeclKind, Ident, Option<Box<Expr>>)>>(
        &mut self,
        var_decls: Iter,
    ) {
        if let Some(body) = &mut self.body {
            if let TsNamespaceBody::TsModuleBlock(block) = body {
                _prepend_var_decls_into_module_items(&mut block.body, var_decls);
            }
        }
    }
}

impl FunctionScope for Module {
    fn _prepend_var_decls<Iter: Iterator<Item = (VarDeclKind, Ident, Option<Box<Expr>>)>>(
        &mut self,
        var_decls: Iter,
    ) {
        _prepend_var_decls_into_module_items(&mut self.body, var_decls);
    }
}

impl FunctionScope for Script {
    fn _prepend_var_decls<Iter: Iterator<Item = (VarDeclKind, Ident, Option<Box<Expr>>)>>(
        &mut self,
        var_decls: Iter,
    ) {
        _prepend_var_decls_into_stmts(&mut self.body, var_decls);
    }
}

impl ForLike for ForStmt {
    fn _get_init_decl(&self) -> Option<&'_ VarDecl> {
        if let Some(init) = &self.init {
            if let VarDeclOrExpr::VarDecl(decl) = init {
                return Some(decl);
            }
        }
        None
    }
}

impl ForLike for ForInStmt {
    fn _get_init_decl(&self) -> Option<&'_ VarDecl> {
        if let VarDeclOrPat::VarDecl(decl) = &self.left {
            return Some(decl);
        }
        None
    }
}

impl ForLike for ForOfStmt {
    fn _get_init_decl(&self) -> Option<&'_ VarDecl> {
        if let VarDeclOrPat::VarDecl(decl) = &self.left {
            return Some(decl);
        }
        None
    }
}

pub struct JSXTransformVisitorConfig {
    pub pragma: Option<Ident>,
    pub is_custom_element: Box<dyn StringFilter>,
    pub hmr: bool,
    pub optimize: bool,
    pub merge_props: bool,
    pub react_style: bool,
    pub transform_on: bool,
    pub transform_on_update_event: bool,
    pub enable_object_slots: bool,
    pub file_name: String,
}

pub struct JSXTransformVisitor<C: Comments> {
    /* base */
    imports: HashMap<String, HashMap<String, Ident>>,
    vars: Vec<VarDeclRecord>,
    new_global_vars: LinkedHashMap<String, (VarDeclKind, Ident, Option<Box<Expr>>)>,
    new_local_vars_vec: Vec<LinkedHashMap<String, (VarDeclKind, Ident, Option<Box<Expr>>)>>,
    _i: u64,

    /* jsx */
    text_pragma: Option<Ident>,
    with_directives_pragma: Option<Ident>,
    case_array_id: Option<Ident>,
    config: JSXTransformVisitorConfig,

    comments: Option<C>,
}

struct NoopFilter;

struct DefineComponentInfo {
    local: Ident,
    exports: Vec<String>,
}

impl StringFilter for NoopFilter {}

impl Default for JSXTransformVisitorConfig {
    fn default() -> Self {
        Self {
            pragma: None,
            is_custom_element: Box::new(NoopFilter {}),
            hmr: true,
            optimize: true,
            merge_props: true,
            react_style: true,
            transform_on: true,
            transform_on_update_event: true,
            enable_object_slots: true,
            file_name: "module.tsx".to_string(),
        }
    }
}

/* base */
impl<C: Comments> JSXTransformVisitor<C> {
    pub fn add_import(&mut self, _module_id: &str, name: &str) -> Ident {
        let import_map: &mut HashMap<String, Ident> =
            hashmap_str_key_get_mut_default!(self.imports, _module_id, HashMap::new());
        let ident: &mut Ident =
            hashmap_str_key_get_mut_default!(import_map, name, create_private_ident(name));
        clone_ident(ident)
    }

    pub fn end_helper(&mut self, program: &mut Program) {
        match program {
            Program::Module(module) => {
                let items = &mut module.body;

                for (i, (module_id, mut value)) in self.imports.drain().enumerate() {
                    let mut specifiers = Vec::with_capacity(value.len());
                    for (name, local) in value.drain() {
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

                let mut values = self.new_global_vars.drain_values();

                _prepend_var_decls_into_module_items(items, values.drain(..));
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

                for (module_id, mut value) in self.imports.drain() {
                    let mut properties = Vec::with_capacity(value.len());
                    for (name, local) in value.drain() {
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
                            args: vec![ExprOrSpread::from(Expr::Lit(Lit::Str(Str::from(
                                module_id,
                            ))))],
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
                for ident in _find_decl_ident_from_pattern(&param.pat).drain(..) {
                    vars.insert(ident);
                }
            }
        }

        self.new_local_vars_vec.push(LinkedHashMap::new());
        self.vars.push(vars);
        function_scope.visit_mut_children_with(self);
        self.vars.pop();

        if let Some(mut new_local_vars) = self.new_local_vars_vec.pop() {
            let mut values = new_local_vars.drain_values();
            function_scope._prepend_var_decls(values.drain(..));
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
                        _find_decl_ident_from_pattern0(&decl.name, &mut idents);
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
                    _find_decl_ident_from_pattern0(&decl.name, &mut idents);
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
impl<C: Comments> JSXTransformVisitor<C> {
    pub fn new(config: JSXTransformVisitorConfig, comments: Option<C>) -> Self {
        Self {
            imports: HashMap::new(),
            vars: Vec::new(),
            new_global_vars: LinkedHashMap::new(),
            new_local_vars_vec: Vec::new(),
            _i: 0,
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
            _ => quote!(
                r#"$cast_array($expr)"# as Expr,
                cast_array: Ident = self._get_cast_array(),
                expr: Expr = expr
            ),
        }
    }

    pub fn convert_to_array(&self, mut expr: Expr) -> ArrayLit {
        expr = unwrap_expr_move(expr);

        return if let Expr::Array(array) = expr {
            array
        } else {
            ArrayLit {
                span: DUMMY_SP,
                elems: vec![Some(ExprOrSpread::from(expr))],
            }
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
                Expr::Array(_) | Expr::Lit(_) | Expr::Tpl(_) => is_vue_child = true,
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
                quote!(
                    r#"{ default: () => ($expr) }"# as Expr,
                    expr: Expr = Expr::Array(self.convert_to_array(expr),)
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
        let test_str = STRIP_EMPTY_JSX_TEXT_REGEX
            .replace_all(&jsx_text.value.to_string(), "")
            .to_string();

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
                    elements.push(Some(ExprOrSpread {
                        spread: Some(spread_child.span),
                        expr: spread_child.expr,
                    }))
                }
                JSXElementChild::JSXElement(expr) => {
                    elements.push(Some(ExprOrSpread::from(Expr::JSXElement(expr))))
                }
                JSXElementChild::JSXFragment(expr) => {
                    elements.push(Some(ExprOrSpread::from(Expr::JSXFragment(expr))))
                }
            };
        }

        ArrayLit {
            span: DUMMY_SP,
            elems: elements,
        }
    }

    pub fn resolve_jsx_tag_name(&mut self, tag_name: &JSXElementName) -> JsxTag {
        let mut tag_ident = None;
        let (str, span) = match tag_name {
            JSXElementName::Ident(ident) => {
                if ident.sym == js_word!("this") {
                    return JsxTag::Expr(Expr::This(ThisExpr {
                        span: ident.span.clone(),
                    }));
                }

                let name = ast_ident_to_string(ident);
                if is_known_tag(&name) {
                    return JsxTag::String(name);
                }

                tag_ident = Some(clone_ident(ident));

                (name, ident.span.clone())
            }
            JSXElementName::JSXMemberExpr(expr) => {
                return JsxTag::Expr(Expr::Member(jsx_member_ref_to_member_expr(expr)));
            }
            JSXElementName::JSXNamespacedName(name) => {
                let ns = &name.ns;
                let name = &name.name;

                (format!("{}:{}", ns.sym, name.sym), name.span.clone())
            }
        };

        if let Some(ident) = tag_ident {
            if IS_NOT_ELEMENT_TAG_REGEX.is_match(&str) {
                let ident = if self.contains_var_ident(&ident) {
                    Some(ident)
                } else {
                    self.get_var_ident(&str)
                };

                if let Some(ident) = ident {
                    let expr = Expr::Ident(ident);
                    if &str == "Fragment" {
                        return JsxTag::Fragment(expr);
                    }
                    return JsxTag::Expr(expr);
                }
            }
        }

        let camelize_str = camelize_upper_first(&str);

        if is_builtin_component(&camelize_str) {
            let expr = Expr::Ident(self.add_import("vue", &camelize_str));
            if &camelize_str == "Fragment" {
                return JsxTag::Fragment(expr);
            }
            return JsxTag::Expr(expr);
        }

        return if self.config.is_custom_element.do_filter(&str) {
            JsxTag::String(str)
        } else {
            let resolve_component = self.add_import("vue", "resolveComponent");
            if let Some(comments) = &mut self.comments {
                comments.add_pure_comment(span.lo);
            }
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

        self._create_v_node(
            tag,
            None,
            None,
            vec![],
            children,
            None,
            jsx_fragment.span.clone(),
        )
    }

    pub fn transform_jsx_element(&mut self, jsx_element: &mut Box<JSXElement>) -> Expr {
        let attrs = &jsx_element.opening.attrs;
        let tag = self.resolve_jsx_tag_name(&jsx_element.opening.name);
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
                                                JSXAttrValue::Lit(lit) => {
                                                    Some(Expr::Lit(clone_lit(lit)))
                                                }
                                                JSXAttrValue::JSXExprContainer(expr) => {
                                                    match &expr.expr {
                                                        JSXExpr::Expr(expr) => {
                                                            Some(expr.as_ref().clone())
                                                        }
                                                        _ => None,
                                                    }
                                                }
                                                _ => None,
                                            },
                                            _ => None,
                                        };

                                        if let Some(expr) = expr {
                                            match &expr {
                                                Expr::Lit(lit) => {
                                                    result = match lit {
                                                        Lit::Str(str) => {
                                                            match &ast_str_to_string(str) as &str {
                                                                "radio" => "radio",
                                                                "checkbox" => "checkbox",
                                                                _ => "text",
                                                            }
                                                        }
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
                    let (mut name, mut name_span) = match &attr.name {
                        JSXAttrName::Ident(ident) => {
                            (ast_ident_to_string(ident), ident.span.clone())
                        }
                        JSXAttrName::JSXNamespacedName(name) => {
                            let ns = &name.ns;
                            let name = &name.name;

                            (format!("{}:{}", ns.sym, name.sym), name.span.clone())
                        }
                        _ => {
                            continue;
                        }
                    };
                    let expression = match &attr.value {
                        Some(value) => match value {
                            JSXAttrValue::Lit(lit) => Some(Expr::Lit(clone_lit(lit))),
                            JSXAttrValue::JSXExprContainer(expr) => match &expr.expr {
                                JSXExpr::Expr(expr) => Some(expr.as_ref().clone()),
                                _ => None,
                            },
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
                                                if let ObjectKey::Expr(_) =
                                                    ObjectKey::from(&key_value.key)
                                                {
                                                    should_extra_props = false;
                                                    break;
                                                }
                                            }
                                            Prop::Assign(_) | Prop::Getter(_) | Prop::Setter(_) => {
                                                should_extra_props = false;
                                                break;
                                            }
                                            Prop::Method(method) => {
                                                if let ObjectKey::Expr(_) =
                                                    ObjectKey::from(&method.key)
                                                {
                                                    should_extra_props = false;
                                                    break;
                                                }
                                            }
                                            _ => {}
                                        },
                                    }
                                }

                                if should_extra_props {
                                    let mut on_map = HashMap::new();
                                    for prop in object.props.drain(..) {
                                        if let PropOrSpread::Prop(prop) = prop {
                                            match *prop {
                                                Prop::Shorthand(ident) => {
                                                    on_map.insert(
                                                        String::from("on")
                                                            + &upper_first(&ast_ident_to_string(
                                                                &ident,
                                                            )),
                                                        Expr::Ident(clone_ident(&ident)),
                                                    );
                                                }
                                                Prop::KeyValue(key_value) => {
                                                    if let ObjectKey::Str(key) =
                                                        ObjectKey::from(&key_value.key)
                                                    {
                                                        on_map.insert(
                                                            String::from("on") + &upper_first(&key),
                                                            *key_value.value,
                                                        );
                                                    }
                                                }
                                                Prop::Method(method) => {
                                                    if let ObjectKey::Str(key) =
                                                        ObjectKey::from(&method.key)
                                                    {
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

                                    for (key, value) in on_map.drain() {
                                        props_array.push(PropOrSpread::Prop(Box::new(
                                            Prop::KeyValue(KeyValueProp {
                                                key: PropName::Str(Str::from(key)),
                                                value: Box::new(value),
                                            }),
                                        )));
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
                                    args: vec![ExprOrSpread {
                                        spread: None,
                                        expr: Box::new(expr),
                                    }],
                                    type_args: None,
                                })),
                            }))
                        }

                        continue;
                    }

                    if self.config.react_style {
                        match &name as &str {
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
                        name = lower_first(&camelize(&name)[1..].to_string());
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

                        let modifiers = HashSet::from_iter(modifiers.drain(..));

                        match &name as &str {
                            "slots" => {
                                if let Some(expr) = expression {
                                    slots_array.push(expr);
                                }
                            }
                            "text" => props_array.push(PropOrSpread::Prop(Box::from(
                                create_key_value_prop(
                                    String::from("textContent"),
                                    expression.unwrap_or(create_void_zero_expr()),
                                ),
                            ))),
                            "html" => props_array.push(PropOrSpread::Prop(Box::from(
                                create_key_value_prop(
                                    String::from("innerHTML"),
                                    expression.unwrap_or(create_void_zero_expr()),
                                ),
                            ))),
                            "model" | "models" => {
                                if let Some(mut expr) = expression {
                                    expr = unwrap_expr_move(expr);

                                    let mut raw_models = Vec::new();
                                    let arg = arg.as_ref();

                                    fn add_model(
                                        models: &mut Vec<(Expr, String, HashSet<String>)>,
                                        expr: Expr,
                                        mut modifiers: HashSet<String>,
                                        default_name: Option<&String>,
                                    ) {
                                        match &expr {
                                            Expr::Ident(_) | Expr::Member(_) => {
                                                models.push((
                                                    expr,
                                                    match default_name {
                                                        Some(name) => String::from(name),
                                                        _ => String::from("modelValue"),
                                                    },
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
                                                            Expr::Ident(_) | Expr::Member(_) => {
                                                                left = Some(expr)
                                                            }
                                                            _ => {}
                                                        }
                                                    }
                                                }

                                                match left {
                                                    None => {
                                                        return;
                                                    }
                                                    Some(left) => {
                                                        let mut model_name: Option<String> = None;
                                                        let mut modifiers_array = None;

                                                        if !array.elems.is_empty() {
                                                            if let Some(param1) =
                                                                array.elems.remove(0)
                                                            {
                                                                if let None = param1.spread {
                                                                    match unwrap_expr_move(
                                                                        *param1.expr,
                                                                    ) {
                                                                        Expr::Lit(expr1) => {
                                                                            if let Lit::Str(str) =
                                                                                expr1
                                                                            {
                                                                                let name =
                                                                                ast_str_to_string(
                                                                                    &str,
                                                                                );
                                                                                if !name.is_empty()
                                                                                {
                                                                                    model_name =
                                                                                        Some(name)
                                                                                }
                                                                            }
                                                                        }
                                                                        Expr::Array(expr1) => {
                                                                            modifiers_array =
                                                                                Some(expr1)
                                                                        }
                                                                        _ => {}
                                                                    }
                                                                }
                                                            }

                                                            if !array.elems.is_empty()
                                                                && modifiers_array == None
                                                            {
                                                                if let Some(param2) =
                                                                    array.elems.remove(0)
                                                                {
                                                                    if let None = param2.spread {
                                                                        if let Expr::Array(expr2) =
                                                                            unwrap_expr_move(
                                                                                *param2.expr,
                                                                            )
                                                                        {
                                                                            modifiers_array =
                                                                                Some(expr2)
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }

                                                        if let Some(modifiers_array) =
                                                            modifiers_array
                                                        {
                                                            for elem in modifiers_array.elems.iter()
                                                            {
                                                                if let Some(elem) = elem {
                                                                    if let None = elem.spread {
                                                                        if let Expr::Lit(elem) =
                                                                            unwrap_expr(
                                                                                elem.expr.as_ref(),
                                                                            )
                                                                        {
                                                                            if let Lit::Str(str) =
                                                                                elem
                                                                            {
                                                                                modifiers.insert(ast_str_to_string(str));
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }

                                                        let name = match model_name {
                                                            Some(name) => name,
                                                            _ => match default_name {
                                                                Some(name) => String::from(name),
                                                                _ => String::from("modelValue"),
                                                            },
                                                        };

                                                        models.push((
                                                            left.as_ref().clone(),
                                                            name,
                                                            modifiers,
                                                        ))
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

                                    for (left_expr, model_name, modifiers) in raw_models.drain(..) {
                                        props_array.push(PropOrSpread::Prop(Box::new(
                                            create_key_value_prop(
                                                format!("onUpdate:{}", &model_name),
                                                quote!(
                                                    r#"( $event ) => void (($left) = $event) "#
                                                        as Expr,
                                                    event: Ident = private_ident!("$event"),
                                                    left: Expr = left_expr.clone()
                                                ),
                                            ),
                                        )));

                                        let mut v_model_directive = None;

                                        if model_name == "modelValue" {
                                            if is_select_tag {
                                                v_model_directive =
                                                    Some(self.add_import("vue", "vModelSelect"));
                                            } else if is_textarea_tag {
                                                v_model_directive =
                                                    Some(self.add_import("vue", "vModelText"));
                                            } else if is_input_tag {
                                                let input_type = get_input_type();
                                                match &input_type as &str {
                                                    "checkbox" => {
                                                        v_model_directive =
                                                            Some(self.add_import(
                                                                "vue",
                                                                "vModelCheckbox",
                                                            ));
                                                    }
                                                    "radio" => {
                                                        v_model_directive = Some(
                                                            self.add_import("vue", "vModelRadio"),
                                                        );
                                                    }
                                                    "text" | "number" | "tel" | "search"
                                                    | "email" | "url" => {
                                                        v_model_directive = Some(
                                                            self.add_import("vue", "vModelText"),
                                                        );
                                                    }
                                                    _ => {
                                                        v_model_directive = Some(
                                                            self.add_import("vue", "vModelDynamic"),
                                                        );
                                                    }
                                                }
                                            }
                                        }

                                        let modifiers_expr = if modifiers.is_empty() {
                                            None
                                        } else {
                                            let modifier =
                                                self.generate_modifiers_object(&modifiers);
                                            if let Expr::Ident(ident) = &modifier {
                                                constant_idents.insert(clone_ident(ident));
                                            }
                                            Some(modifier)
                                        };

                                        if let Some(v_model_directive) = v_model_directive {
                                            directives_array.push(Some(ExprOrSpread {
                                                spread: None,
                                                expr: Box::new(
                                                    quote!(
                                                        r#"[$directive, $expression, undefined, $modifiers]"#
                                                            as Expr,
                                                        directive: Ident = v_model_directive,
                                                        expression: Expr = left_expr,
                                                        modifiers: Expr = modifiers_expr
                                                            .unwrap_or(create_void_zero_expr())
                                                    )
                                                ),
                                            }));
                                        } else {
                                            if let Some(modifier_expr) = modifiers_expr {
                                                props_array.push(PropOrSpread::Prop(Box::new(
                                                    create_key_value_prop(
                                                        format!("{}Modifiers", &model_name),
                                                        modifier_expr,
                                                    ),
                                                )));
                                            }
                                            props_array.push(PropOrSpread::Prop(Box::new(
                                                create_key_value_prop_box(
                                                    model_name,
                                                    Box::new(left_expr),
                                                ),
                                            )));
                                        }
                                    }
                                }
                            }
                            _ => {
                                let directive_expr = match &name as &str {
                                    "show" => Expr::Ident(self.add_import("vue", "vShow")),
                                    _ => {
                                        let ident =
                                            self.get_var_ident(&format!("v{}", upper_first(&name)));

                                        if let Some(ident) = ident {
                                            Expr::Ident(ident)
                                        } else {
                                            let resolve_directive =
                                                self.add_import("vue", "resolveDirective");

                                            if let Some(comments) = &mut self.comments {
                                                comments.add_pure_comment(name_span.lo.clone());
                                            }

                                            let get_resolved_directive_expr = || {
                                                Expr::Call(CallExpr {
                                                    span: name_span.clone(),
                                                    callee: Callee::Expr(Box::new(
                                                        resolve_directive.into(),
                                                    )),
                                                    args: vec![ExprOrSpread {
                                                        spread: None,
                                                        expr: Box::new(Expr::Lit(Lit::Str(
                                                            Str::from(name.to_string()),
                                                        ))),
                                                    }],
                                                    type_args: None,
                                                })
                                            };

                                            if self.config.optimize {
                                                Expr::Ident(self.add_local_variable(
                                                    format!("resolve-directive:{}", &name),
                                                    {
                                                        move || {
                                                            Some(Box::from(
                                                                get_resolved_directive_expr(),
                                                            ))
                                                        }
                                                    },
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
                                    expr: Box::new(quote!(
                                        r#"[$directive, $expression, $argument, $modifiers]"#
                                            as Expr,
                                        directive: Expr = directive_expr,
                                        expression: Expr = expression.unwrap_or(create_true_expr()),
                                        argument: Expr = argument_expr,
                                        modifiers: Expr = modifiers_expr
                                    )),
                                }))
                            }
                        }
                    } else {
                        props_array.push(PropOrSpread::Prop(Box::new(create_key_value_prop(
                            name,
                            expression.unwrap_or(create_true_expr()),
                        ))))
                    }
                }
                JSXAttrOrSpread::SpreadElement(spread) => props_array.push({
                    PropOrSpread::Spread(SpreadElement {
                        dot3_token: DUMMY_SP,
                        expr: spread.expr.clone(),
                    })
                }),
            }
        }

        let slots = if slots_array.is_empty() {
            None
        } else {
            let mut slots_items = Vec::new();
            for item in slots_array.drain(..) {
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
            let expressions = vec![Expr::Object(ObjectLit {
                span: DUMMY_SP,
                props: props_array,
            })];
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
            jsx_element.span.clone(),
        )
    }

    pub fn handle_hmr(&mut self, module: &mut Module) {
        if !self.config.hmr {
            return;
        }

        let mut components = HashMap::new();
        let mut exports = HashMap::new();
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
                                                    ModuleExportName::Ident(ident) => {
                                                        ast_ident_to_string(ident)
                                                    }
                                                    ModuleExportName::Str(str) => {
                                                        ast_str_to_string(str)
                                                    }
                                                };

                                                let export_name =
                                                    if let Some(name) = &named.exported {
                                                        match name {
                                                            ModuleExportName::Ident(ident) => {
                                                                ast_ident_to_string(ident)
                                                            }
                                                            ModuleExportName::Str(str) => {
                                                                ast_str_to_string(str)
                                                            }
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
                                DefineComponentInfo {
                                    local: ident,
                                    exports: vec![String::from("default")],
                                },
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

                            extra_items.push(ModuleItem::ModuleDecl(
                                ModuleDecl::TsExportAssignment(TsExportAssignment {
                                    span: DUMMY_SP,
                                    expr: Box::new(Expr::Ident(clone_ident(&ident))),
                                }),
                            ));

                            components.insert(
                                String::from("default"),
                                DefineComponentInfo {
                                    local: ident,
                                    exports: vec![String::from("default")],
                                },
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

            if let Some(var_decl_stmt) = var_decl_stmt {
                for decl in var_decl_stmt.decls.iter() {
                    if let Pat::Ident(ident) = &decl.name {
                        if let Some(expr) = &decl.init {
                            if is_define_component_expr(expr.as_ref()) {
                                let mut exports = vec![];
                                if exported {
                                    exports.push(ast_ident_to_string(&ident.id));
                                }

                                components.insert(
                                    ast_ident_to_string(&ident.id),
                                    DefineComponentInfo {
                                        local: clone_ident(&ident.id),
                                        exports,
                                    },
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

                create_record_expressions.push(Box::new(quote!(
                    r#"$__VUE_HMR_RUNTIME__.createRecord($id, $local)"# as Expr,
                    __VUE_HMR_RUNTIME__: Ident = clone_ident(&__VUE_HMR_RUNTIME__),
                    id: Expr = Expr::Lit(Lit::Str(Str::from(id_str.to_string()))),
                    local: Ident = clone_ident(&local)
                )));

                reload_record_expressions.push(Box::new(quote!(
                    r#"$__VUE_HMR_RUNTIME__.reloadRecord($id, $local)"# as Expr,
                    __VUE_HMR_RUNTIME__: Ident = clone_ident(&__VUE_HMR_RUNTIME__),
                    id: Expr = Expr::Lit(Lit::Str(Str::from(id_str.to_string()))),
                    local: Ident = clone_ident(&local)
                )));

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

                ssr_register_helper_expressions.push(Box::new(quote!(
                    r#"$ssr_register_helper($local, $__MODULE__)"# as Expr,
                    __MODULE__: Ident = clone_ident(&__MODULE__),
                    ssr_register_helper: Ident = clone_ident(&ssr_register_helper_identifier),
                    local: Ident = clone_ident(&local),
                )));
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
                type_ann: None,
            }),
            create_record_expressions: Expr = Expr::Seq(SeqExpr {
                span: DUMMY_SP,
                exprs: create_record_expressions
            }),
            reload_record_expressions: Expr = Expr::Seq(SeqExpr {
                span: DUMMY_SP,
                exprs: reload_record_expressions
            }),
            ssr_register_helper_expressions: Expr = Expr::Seq(SeqExpr {
                span: DUMMY_SP,
                exprs: ssr_register_helper_expressions
            })
        );

        module.body.push(ModuleItem::Stmt(hmr_stmt));
    }

    pub fn merge_objects(&mut self, mut objects: Vec<Expr>) -> Expr {
        if objects.len() == 1 {
            return objects.pop().unwrap();
        }
        let mut properties = Vec::new();

        for mut object in objects.drain(..) {
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
                        properties.push(PropOrSpread::Spread(SpreadElement {
                            dot3_token: DUMMY_SP,
                            expr: Box::new(object),
                        }));
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

        Expr::Object(ObjectLit {
            span: DUMMY_SP,
            props: properties,
        })
    }

    pub fn merge_array_props(&mut self, mut items: Vec<Expr>, strip: bool) -> Expr {
        let mut elements = Vec::new();

        for mut element in items.drain(..) {
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
                Expr::Object(_)
                | Expr::Lit(_)
                | Expr::Tpl(_)
                | Expr::Fn(_)
                | Expr::Arrow(_)
                | Expr::Class(_) => elements.push(Some(ExprOrSpread::from(element))),
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

                    elements.push(Some(ExprOrSpread {
                        spread: Some(DUMMY_SP),
                        expr: Box::new(self.cast_array(element)),
                    }));
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

        Expr::Array(ArrayLit {
            span: DUMMY_SP,
            elems: elements,
        })
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

                for item in items.drain(..) {
                    match item {
                        PropsItem::Map(mut map) => {
                            if !map.is_empty() {
                                let mut props = Vec::new();
                                for (key, value) in map.drain() {
                                    match value {
                                        PropsItemMapItem::Normal(expr) => {
                                            props.push(PropOrSpread::Prop(Box::new(
                                                Prop::KeyValue(KeyValueProp {
                                                    key: key.to_prop_name(),
                                                    value: Box::new(expr),
                                                }),
                                            )));
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
                                    expr: Box::new(Expr::Object(ObjectLit {
                                        span: DUMMY_SP,
                                        props,
                                    })),
                                })
                            }
                        }
                        PropsItem::Spread(expr) => args.push(ExprOrSpread {
                            spread: None,
                            expr: expr.expr,
                        }),
                    }
                }

                if args.len() == 1 && is_object {
                    return args.pop().unwrap().expr.as_ref().clone();
                }

                Expr::Call(CallExpr {
                    span: DUMMY_SP,
                    callee: Callee::Expr(Box::new(Expr::Ident(
                        self.add_import("vue", "mergeProps"),
                    ))),
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
        let mut modifiers = Vec::from_iter(modifiers.clone().drain());
        modifiers.sort();

        Expr::Ident(self.add_global_variable(
            String::from("modifiers:") + &modifiers.join(","),
            move || {
                Some(Box::new(Expr::Object({
                    let mut props = Vec::with_capacity(modifiers.len());
                    for modifier in modifiers.drain(..) {
                        props.push(PropOrSpread::Prop(Box::new(create_key_value_prop(
                            modifier,
                            create_true_expr(),
                        ))))
                    }
                    ObjectLit {
                        span: DUMMY_SP,
                        props,
                    }
                })))
            },
            VarDeclKind::Const,
        ))
    }

    pub fn generate_array_props(&mut self, array_props: &HashSet<String>) -> Expr {
        let mut array_props = Vec::from_iter(array_props.clone().drain());
        array_props.sort();

        Expr::Ident(self.add_global_variable(
            String::from("array_props:") + &array_props.join(","),
            move || {
                Some(Box::new(Expr::Array({
                    let mut elems = Vec::with_capacity(array_props.len());
                    for prop in array_props.drain(..) {
                        elems.push(Some(ExprOrSpread {
                            spread: None,
                            expr: Box::new(Expr::Lit(Lit::Str(Str::from(prop)))),
                        }));
                    }
                    ArrayLit {
                        span: DUMMY_SP,
                        elems,
                    }
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
        let mut props_param = props.unwrap_or(create_null_expr());
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

        match &mut children_param {
            Expr::Array(array) => {
                if array.elems.is_empty() {
                    children_param = create_null_expr()
                }
            }
            Expr::Object(object) => {
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
                        object
                            .props
                            .push(PropOrSpread::Prop(Box::new(create_key_value_prop(
                                String::from("_"),
                                Expr::Lit(Lit::Num(Number::from(SlotFlags::STABLE as f64))),
                            ))))
                    }
                }
            }
            _ => {}
        }

        match &mut props_param {
            Expr::Object(object) => {
                if object.props.is_empty() {
                    children_param = create_null_expr()
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
                                        if let ObjectKey::Str(str) = ObjectKey::from(&key_value.key)
                                        {
                                            match &str as &str {
                                                "ref" => has_ref = true,
                                                _ => {
                                                    if !is_constant_expr(
                                                        key_value.value.as_ref(),
                                                        &constant_idents,
                                                    ) {
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
                                    if is_element && key.to_lowercase() != "onclick"
                                        /* omit v-model handlers */ && key != "onUpdate:modelValue"
                                    {
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
                optimize_params.push(ExprOrSpread {
                    spread: None,
                    expr: Box::new(self.generate_array_props(&dynamic_props)),
                });
            }
        }

        let mut params = vec![
            // tag
            ExprOrSpread {
                spread: None,
                expr: Box::new(tag.into_expr()),
            },
            // props
            ExprOrSpread {
                spread: None,
                expr: Box::new(props_param),
            },
            // children
            ExprOrSpread {
                spread: None,
                expr: Box::new(children_param),
            },
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
                    ExprOrSpread {
                        spread: None,
                        expr: Box::new(v_node),
                    },
                    ExprOrSpread {
                        spread: None,
                        expr: Box::new(Expr::Array({
                            ArrayLit {
                                span: DUMMY_SP,
                                elems: directives,
                            }
                        })),
                    },
                ],
                type_args: None,
            });
        }

        if let Some(comments) = &mut self.comments {
            comments.add_pure_comment(span.lo)
        }

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

impl Default for JSXTransformVisitor<NoopComments> {
    fn default() -> Self {
        Self {
            imports: HashMap::new(),
            vars: Vec::new(),
            new_global_vars: LinkedHashMap::new(),
            new_local_vars_vec: Vec::new(),
            _i: 0,
            text_pragma: None,
            with_directives_pragma: None,
            case_array_id: None,
            config: JSXTransformVisitorConfig::default(),
            comments: None,
        }
    }
}

impl<C: Comments> VisitMut for JSXTransformVisitor<C> {
    /* base */
    fn visit_mut_stmts(&mut self, stmts: &mut Vec<Stmt>) {
        let (idents, fn_scope_idents) = _find_decl_ident_from_stmts(stmts);

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
        let (idents, fn_scope_idents) = _find_decl_ident_from_module_items(items);

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
        expr.visit_mut_children_with(self);

        match expr {
            Expr::JSXElement(jsx_element) => {
                *expr = self.transform_jsx_element(jsx_element);
            }
            Expr::JSXFragment(jsx_fragment) => {
                *expr = self.transform_jsx_fragment(jsx_fragment);
            }
            _ => {}
        }
    }

    fn visit_mut_jsx_element_child(&mut self, child: &mut JSXElementChild) {
        child.visit_mut_children_with(self);

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
    }

    fn visit_mut_jsx_attr_value(&mut self, value: &mut JSXAttrValue) {
        value.visit_mut_children_with(self);

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
    }
}

pub(crate) fn create_key_value_prop(key: String, value: Expr) -> Prop {
    create_key_value_prop_box(key, Box::new(value))
}

pub(crate) fn create_key_value_prop_box(key: String, value: Box<Expr>) -> Prop {
    Prop::KeyValue(KeyValueProp {
        key: PropName::Str(Str::from(key)),
        value,
    })
}

pub(crate) fn create_void_zero_expr() -> Expr {
    Expr::Unary(UnaryExpr {
        span: DUMMY_SP,
        op: UnaryOp::Void,
        arg: Box::new(Expr::Lit(Lit::Num(Number::from(0)))),
    })
}

pub(crate) fn create_true_expr() -> Expr {
    Expr::Lit(Lit::Bool(Bool::from(true)))
}

pub(crate) fn create_null_expr() -> Expr {
    Expr::Lit(Lit::Null(Null { span: DUMMY_SP }))
}

pub(crate) fn create_ident(name: &str) -> Ident {
    Ident::new(name.into(), DUMMY_SP)
}

pub(crate) fn create_private_ident(name: &str) -> Ident {
    let ident_name = format!("_{}", name);
    private_ident!(ident_name)
}

pub(crate) fn clone_ident(ident: &Ident) -> Ident {
    Ident {
        span: ident.span.clone(),
        sym: ident.sym.clone(),
        optional: ident.optional,
    }
}

pub(crate) fn clone_lit(lit: &Lit) -> Lit {
    match lit {
        Lit::Str(str) => Lit::Str(Str::from(String::from(ast_str_to_string(str)))),
        Lit::Bool(_bool) => Lit::Bool(Bool::from(_bool.value)),
        Lit::Num(num) => Lit::Num(Number::from(num.value)),
        Lit::BigInt(bigint) => Lit::BigInt(BigInt::from(bigint.value.clone())),
        Lit::Regex(regexp) => Lit::Regex(ast::Regex {
            span: regexp.span.clone(),
            exp: regexp.exp.clone(),
            flags: regexp.flags.clone(),
        }),
        Lit::JSXText(jsx_text) => Lit::JSXText(JSXText {
            span: jsx_text.span.clone(),
            value: jsx_text.value.clone(),
            raw: jsx_text.raw.clone(),
        }),
        _ => Lit::Null(Null { span: DUMMY_SP }),
    }
}

pub(crate) fn ast_str_to_string(str: &Str) -> String {
    String::from(str.value.chars().as_str())
}

pub(crate) fn ast_ident_to_string(ident: &Ident) -> String {
    String::from(ident.sym.chars().as_str())
}

fn unwrap_expr(mut expr: &Expr) -> &Expr {
    loop {
        match expr {
            Expr::TsAs(ts_as) => expr = ts_as.expr.as_ref(),
            Expr::Paren(paren) => expr = paren.expr.as_ref(),
            Expr::TsTypeAssertion(assertion) => expr = assertion.expr.as_ref(),
            Expr::TsConstAssertion(assertion) => expr = assertion.expr.as_ref(),
            Expr::TsNonNull(non_null) => expr = non_null.expr.as_ref(),
            Expr::TsInstantiation(inst) => expr = inst.expr.as_ref(),
            _ => break expr,
        }
    }
}

fn unwrap_expr_move(mut expr: Expr) -> Expr {
    loop {
        match expr {
            Expr::TsAs(ts_as) => expr = *ts_as.expr,
            Expr::Paren(paren) => expr = *paren.expr,
            Expr::TsTypeAssertion(assertion) => expr = *assertion.expr,
            Expr::TsConstAssertion(assertion) => expr = *assertion.expr,
            Expr::TsNonNull(non_null) => expr = *non_null.expr,
            Expr::TsInstantiation(inst) => expr = *inst.expr,
            Expr::Tpl(tpl) => {
                if tpl.exprs.is_empty() {
                    if let Some(t) = tpl.quasis.first() {
                        let mut str = Str::from(t.raw.to_string());
                        str.span = t.span.clone();
                        expr = Expr::Lit(Lit::Str(str));
                        break expr;
                    }
                }
                expr = Expr::Tpl(tpl);
                break expr;
            }
            _ => break expr,
        }
    }
}

fn is_constant_expr(expr: &Expr, constant_idents: &Option<HashSet<Ident>>) -> bool {
    match unwrap_expr(expr) {
        Expr::Lit(_) => true,
        Expr::Unary(unary) => match unary.op {
            UnaryOp::Void => true,
            _ => false,
        },
        Expr::Ident(ident) => {
            if ident.sym == js_word!("undefined") {
                true
            } else if let Some(constant_idents) = constant_idents {
                constant_idents.contains(ident)
            } else {
                false
            }
        }
        _ => false,
    }
}

fn jsx_member_ref_to_member_expr(jsx_member: &JSXMemberExpr) -> MemberExpr {
    MemberExpr {
        span: DUMMY_SP,
        obj: Box::new(match &jsx_member.obj {
            JSXObject::JSXMemberExpr(expr) => Expr::Member(jsx_member_ref_to_member_expr(expr)),
            JSXObject::Ident(ident) => Expr::Ident(clone_ident(ident)),
        }),
        prop: MemberProp::Ident(jsx_member.prop.clone()),
    }
}

fn is_top_directive(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Expr(expr) => match expr.expr.as_ref() {
            Expr::Lit(lit) => match lit {
                Lit::Str(_) => true,
                _ => false,
            },
            _ => false,
        },
        _ => false,
    }
}

fn is_require_call(expr: &Expr) -> bool {
    match expr {
        Expr::Call(expr) => match &expr.callee {
            Callee::Expr(expr) => match expr.as_ref() {
                Expr::Ident(ident) => ident.sym == js_word!("require"),
                _ => false,
            },
            _ => false,
        },
        _ => false,
    }
}

fn is_module_top(module_item: &ModuleItem) -> bool {
    match module_item {
        ModuleItem::ModuleDecl(decl) => match decl {
            ModuleDecl::Import(_) | ModuleDecl::TsImportEquals(_) => true,
            ModuleDecl::ExportNamed(decl) => match decl.src {
                Some(_) => true,
                _ => false,
            },
            _ => false,
        },
        ModuleItem::Stmt(stmt) => is_top_directive(stmt),
    }
}

fn is_script_top(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Expr(expr) => is_top_directive(stmt) || is_require_call(expr.expr.as_ref()),
        Stmt::Decl(decl) => match decl {
            Decl::Var(var) => {
                if var.decls.len() == 1 {
                    match var.decls.get(0) {
                        Some(decl) => match &decl.init {
                            Some(expr) => is_require_call(expr.as_ref()),
                            _ => false,
                        },
                        _ => false,
                    }
                } else {
                    false
                }
            }
            _ => false,
        },
        _ => false,
    }
}

fn is_define_component_expr(expr: &Expr) -> bool {
    match unwrap_expr(expr) {
        Expr::Call(call_expr) => match &call_expr.callee {
            Callee::Expr(callee) => match callee.as_ref() {
                Expr::Ident(ident) => {
                    return ast_ident_to_string(ident) == "defineComponent";
                }
                _ => {}
            },
            _ => {}
        },
        _ => {}
    };

    return false;
}

struct HasJsxStruct {
    has_jsx: bool,
}

impl Visit for HasJsxStruct {
    noop_visit_type!();

    fn visit_jsx_element(&mut self, n: &JSXElement) {
        self.has_jsx = true;
    }

    fn visit_jsx_fragment(&mut self, n: &JSXFragment) {
        self.has_jsx = true;
    }

    fn visit_stmt(&mut self, n: &Stmt) {
        if !self.has_jsx {
            n.visit_children_with(self);
        }
    }
    fn visit_expr(&mut self, n: &Expr) {
        if !self.has_jsx {
            n.visit_children_with(self);
        }
    }
}

fn has_jsx<T: VisitWith<HasJsxStruct>>(n: &T) -> bool {
    let mut _struct = HasJsxStruct { has_jsx: false };

    n.visit_children_with(&mut _struct);
    _struct.has_jsx
}

fn _prepend_statement_with_body(body: &mut Option<BlockStmt>, stmts: &mut Vec<Stmt>) {
    match body {
        Some(body) => {
            let mut items = Vec::with_capacity(stmts.len() + body.stmts.len());
            let mut _is_append = false;

            for item in &mut body.stmts.drain(..) {
                if !_is_append && !is_script_top(&item) {
                    _is_append = true;
                    items.append(stmts);
                }

                items.push(item)
            }

            if !_is_append {
                _is_append = true;
                items.append(stmts);
            }
            body.stmts = items;
            stmts.clear()
        }
        _ => {}
    }
    stmts.clear()
}

fn _find_decl_ident_from_pattern(pat: &Pat) -> Vec<Ident> {
    let mut idents = Vec::new();
    match pat {
        Pat::Ident(ident) => idents.push(clone_ident(&ident.id)),
        Pat::Rest(rest) => {
            _find_decl_ident_from_pattern0(rest.arg.as_ref(), &mut idents);
        }
        Pat::Array(array) => {
            _find_decl_ident_from_array_pattern0(array, &mut idents);
        }
        Pat::Object(object) => {
            _find_decl_ident_from_object_pattern0(object, &mut idents);
        }
        Pat::Assign(assign) => {
            _find_decl_ident_from_pattern0(assign.left.as_ref(), &mut idents);
        }
        Pat::Invalid(_) => {}
        Pat::Expr(_) => {}
    };

    idents
}

fn _find_decl_ident_from_pattern0(pat: &Pat, idents: &mut Vec<Ident>) {
    match pat {
        Pat::Ident(ident) => idents.push(clone_ident(&ident.id)),
        Pat::Rest(rest) => {
            _find_decl_ident_from_pattern0(rest.arg.as_ref(), idents);
        }
        Pat::Array(array) => {
            _find_decl_ident_from_array_pattern0(array, idents);
        }
        Pat::Object(object) => {
            _find_decl_ident_from_object_pattern0(object, idents);
        }
        Pat::Assign(assign) => {
            _find_decl_ident_from_pattern0(assign.left.as_ref(), idents);
        }
        Pat::Invalid(_) => {}
        Pat::Expr(_) => {}
    };
}

fn _find_decl_ident_from_array_pattern0(pat: &ArrayPat, idents: &mut Vec<Ident>) {
    for elem in pat.elems.iter() {
        if let Some(elem) = elem {
            _find_decl_ident_from_pattern0(elem, idents);
        }
    }
}

fn _find_decl_ident_from_object_pattern0(pat: &ObjectPat, idents: &mut Vec<Ident>) {
    for elem in pat.props.iter() {
        match elem {
            ObjectPatProp::KeyValue(key_value) => {
                _find_decl_ident_from_pattern0(key_value.value.as_ref(), idents);
            }
            ObjectPatProp::Rest(rest) => {
                _find_decl_ident_from_pattern0(rest.arg.as_ref(), idents);
            }
            ObjectPatProp::Assign(assign) => idents.push(clone_ident(&assign.key)),
        }
    }
}

fn _find_decl_ident_from_decl0(
    decl: &Decl,
    idents: &mut Vec<Ident>,
    fn_scope_idents: &mut Vec<Ident>,
) {
    match decl {
        Decl::Class(decl) => {
            idents.push(clone_ident(&decl.ident));
        }
        Decl::Fn(decl) => {
            idents.push(clone_ident(&decl.ident));
        }
        Decl::Var(decls) => {
            let idents = if let VarDeclKind::Var = decls.kind {
                fn_scope_idents
            } else {
                idents
            };
            for decl in decls.decls.iter() {
                _find_decl_ident_from_pattern0(&decl.name, idents);
            }
        }
        Decl::TsEnum(decl) => {
            idents.push(clone_ident(&decl.id));
        }
        Decl::TsModule(decl) => {
            if let TsModuleName::Ident(ident) = &decl.id {
                idents.push(clone_ident(ident));
            }
        }
        _ => {}
    }
}

fn _find_decl_ident_from_stmt0(
    stmt: &Stmt,
    idents: &mut Vec<Ident>,
    fn_scope_idents: &mut Vec<Ident>,
) {
    match stmt {
        Stmt::Decl(decl) => {
            _find_decl_ident_from_decl0(decl, idents, fn_scope_idents);
        }
        _ => {}
    }
}

fn _find_decl_ident_from_module_item0(
    item: &ModuleItem,
    idents: &mut Vec<Ident>,
    fn_scope_idents: &mut Vec<Ident>,
) {
    match item {
        ModuleItem::ModuleDecl(decl) => match decl {
            ModuleDecl::TsImportEquals(decl) => {
                fn_scope_idents.push(clone_ident(&decl.id));
            }
            ModuleDecl::Import(decl) => {
                for spec in decl.specifiers.iter() {
                    match spec {
                        ImportSpecifier::Named(spec) => {
                            fn_scope_idents.push(clone_ident(&spec.local));
                        }
                        ImportSpecifier::Default(spec) => {
                            fn_scope_idents.push(clone_ident(&spec.local));
                        }
                        ImportSpecifier::Namespace(spec) => {
                            fn_scope_idents.push(clone_ident(&spec.local));
                        }
                        _ => {}
                    }
                }
            }
            ModuleDecl::ExportDecl(decl) => {
                _find_decl_ident_from_decl0(&decl.decl, idents, fn_scope_idents);
            }
            _ => {}
        },
        ModuleItem::Stmt(stmt) => {
            _find_decl_ident_from_stmt0(stmt, idents, fn_scope_idents);
        }
        _ => {}
    }
}

fn _find_decl_ident_from_stmts(stmts: &Vec<Stmt>) -> (Vec<Ident>, Vec<Ident>) {
    let mut idents = Vec::new();
    let mut fn_scope_idents = Vec::new();

    for stmt in stmts.iter() {
        _find_decl_ident_from_stmt0(stmt, &mut idents, &mut fn_scope_idents);
    }

    (idents, fn_scope_idents)
}

fn _find_decl_ident_from_module_items(items: &Vec<ModuleItem>) -> (Vec<Ident>, Vec<Ident>) {
    let mut idents = Vec::new();
    let mut fn_scope_idents = Vec::new();

    for item in items.iter() {
        _find_decl_ident_from_module_item0(item, &mut idents, &mut fn_scope_idents);
    }

    (idents, fn_scope_idents)
}

fn _generate_var_decl_stmts(
    new_vars: &mut LinkedHashMap<String, (VarDeclKind, Ident, Option<Box<Expr>>)>,
) -> Vec<Stmt> {
    let mut stmts = Vec::with_capacity(new_vars.len());

    for (_, (kind, ident, expr)) in new_vars.drain() {
        stmts.push(Stmt::Decl(Decl::Var(VarDecl {
            span: DUMMY_SP,
            kind,
            declare: false,
            decls: vec![VarDeclarator {
                span: DUMMY_SP,
                name: Pat::Ident(BindingIdent {
                    id: ident,
                    type_ann: None,
                }),
                init: expr,
                definite: false,
            }],
        })))
    }

    stmts
}
