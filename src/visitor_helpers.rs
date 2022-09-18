use std::{
    collections::{HashMap, HashSet},
    hash::{Hash, Hasher},
};

use linked_hash_map::LinkedHashMap;
use swc_core::{
    common::DUMMY_SP,
    ecma::{
        ast::{
            self, ArrayPat, ArrowExpr, AssignPat, BigInt, BindingIdent, BlockStmt, BlockStmtOrExpr,
            Bool, Callee, ClassMethod, ComputedPropName, Constructor, Decl, Expr, FnDecl, FnExpr,
            ForInStmt, ForOfStmt, ForStmt, GetterProp, Ident, ImportSpecifier, JSXElement,
            JSXFragment, JSXMemberExpr, JSXObject, JSXText, KeyValueProp, Lit, MemberExpr,
            MemberProp, MethodProp, Module, ModuleDecl, ModuleItem, Null, Number, ObjectPat,
            ObjectPatProp, Param, ParamOrTsParamProp, Pat, PrivateMethod, Prop, PropName,
            ReturnStmt, Script, SetterProp, SpreadElement, Stmt, Str, TsModuleDecl, TsModuleName,
            TsNamespaceBody, TsParamPropParam, UnaryExpr, UnaryOp, VarDecl, VarDeclKind,
            VarDeclOrExpr, VarDeclOrPat, VarDeclarator,
        },
        atoms::js_word,
        utils::private_ident,
        visit::{noop_visit_type, Visit, VisitWith},
    },
};

pub enum JsxTag {
    Fragment(Expr),
    String(String),
    Expr(Expr),
}

pub enum PropsItemMapItem {
    Normal(Expr),
    Accessor((Option<Box<Prop>>, Option<Box<Prop>>)),
}

pub enum PropsItem {
    Map(LinkedHashMap<ObjectKey, PropsItemMapItem>),
    Spread(SpreadElement),
}

pub enum ObjectKey {
    Str(String),
    Expr(Expr),
}

pub enum VarDeclRecord {
    Func((HashSet<Ident>, HashMap<String, Ident>)),
    Block((HashSet<Ident>, HashMap<String, Ident>)),
}

impl VarDeclRecord {
    pub(crate) fn new(is_function_scope: bool) -> VarDeclRecord {
        if is_function_scope {
            VarDeclRecord::Func((HashSet::new(), HashMap::new()))
        } else {
            VarDeclRecord::Block((HashSet::new(), HashMap::new()))
        }
    }
    pub(crate) fn insert(&mut self, ident: Ident) -> bool {
        let (set, map) = match self {
            VarDeclRecord::Func(item) => item,
            VarDeclRecord::Block(item) => item,
        };

        map.insert(ast_ident_to_string(&ident), clone_ident(&ident));
        set.insert(ident)
    }
    pub(crate) fn contains(&self, ident: &Ident) -> bool {
        let (set, _) = match self {
            VarDeclRecord::Func(item) => item,
            VarDeclRecord::Block(item) => item,
        };

        set.contains(ident)
    }
    pub(crate) fn get_ident(&self, name: &str) -> Option<&Ident> {
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
    pub(crate) fn to_prop_name(&self) -> PropName {
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
    pub(crate) fn into_expr(self) -> Expr {
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

pub(crate) trait DrainValues<V> {
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

pub(crate) fn prepend_var_decls_into_stmts<
    Iter: Iterator<Item = (VarDeclKind, Ident, Option<Box<Expr>>)>,
>(
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

pub(crate) fn prepend_var_decls_into_module_items<
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

pub(crate) fn prepend_var_decls_into_block_stmt<
    Iter: Iterator<Item = (VarDeclKind, Ident, Option<Box<Expr>>)>,
>(
    block: &mut BlockStmt,
    var_decls: Iter,
) {
    prepend_var_decls_into_stmts(&mut block.stmts, var_decls);
}

pub(crate) fn prepend_var_decls_into_option_block_stmt<
    Iter: Iterator<Item = (VarDeclKind, Ident, Option<Box<Expr>>)>,
>(
    block: &mut Option<BlockStmt>,
    var_decls: Iter,
) {
    if let Some(block) = block {
        prepend_var_decls_into_block_stmt(block, var_decls);
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
        prepend_var_decls_into_option_block_stmt(&mut self.function.body, var_decls);
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
        prepend_var_decls_into_option_block_stmt(&mut self.function.body, var_decls);
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
        prepend_var_decls_into_option_block_stmt(&mut self.function.body, var_decls);
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
        prepend_var_decls_into_option_block_stmt(&mut self.body, var_decls);
    }
}

impl FunctionScope for GetterProp {
    fn _prepend_var_decls<Iter: Iterator<Item = (VarDeclKind, Ident, Option<Box<Expr>>)>>(
        &mut self,
        var_decls: Iter,
    ) {
        prepend_var_decls_into_option_block_stmt(&mut self.body, var_decls);
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
        prepend_var_decls_into_option_block_stmt(&mut self.body, var_decls);
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
        prepend_var_decls_into_option_block_stmt(&mut self.function.body, var_decls);
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
        prepend_var_decls_into_option_block_stmt(&mut self.function.body, var_decls);
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
                prepend_var_decls_into_block_stmt(block, var_decls);
            }
            BlockStmtOrExpr::Expr(expr) => {
                let mut block = BlockStmt {
                    span: DUMMY_SP,
                    stmts: vec![Stmt::Return(ReturnStmt {
                        span: DUMMY_SP,
                        arg: Some(expr.clone()),
                    })],
                };

                prepend_var_decls_into_block_stmt(&mut block, var_decls);

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
                prepend_var_decls_into_module_items(&mut block.body, var_decls);
            }
        }
    }
}

impl FunctionScope for Module {
    fn _prepend_var_decls<Iter: Iterator<Item = (VarDeclKind, Ident, Option<Box<Expr>>)>>(
        &mut self,
        var_decls: Iter,
    ) {
        prepend_var_decls_into_module_items(&mut self.body, var_decls);
    }
}

impl FunctionScope for Script {
    fn _prepend_var_decls<Iter: Iterator<Item = (VarDeclKind, Ident, Option<Box<Expr>>)>>(
        &mut self,
        var_decls: Iter,
    ) {
        prepend_var_decls_into_stmts(&mut self.body, var_decls);
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

pub(crate) fn unwrap_expr(mut expr: &Expr) -> &Expr {
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

pub(crate) fn unwrap_expr_move(mut expr: Expr) -> Expr {
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

pub(crate) fn is_constant_expr(expr: &Expr, constant_idents: &Option<HashSet<Ident>>) -> bool {
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

pub(crate) fn jsx_member_ref_to_member_expr(jsx_member: &JSXMemberExpr) -> MemberExpr {
    MemberExpr {
        span: DUMMY_SP,
        obj: Box::new(match &jsx_member.obj {
            JSXObject::JSXMemberExpr(expr) => Expr::Member(jsx_member_ref_to_member_expr(expr)),
            JSXObject::Ident(ident) => Expr::Ident(clone_ident(ident)),
        }),
        prop: MemberProp::Ident(jsx_member.prop.clone()),
    }
}

pub(crate) fn is_top_directive(stmt: &Stmt) -> bool {
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

pub(crate) fn is_require_call(expr: &Expr) -> bool {
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

pub(crate) fn is_module_top(module_item: &ModuleItem) -> bool {
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

pub(crate) fn is_script_top(stmt: &Stmt) -> bool {
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

pub(crate) fn is_define_component_expr(expr: &Expr) -> bool {
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

pub(crate) struct HasJsxStruct {
    has_jsx: bool,
}

impl Visit for HasJsxStruct {
    noop_visit_type!();

    fn visit_jsx_element(&mut self, _: &JSXElement) {
        self.has_jsx = true;
    }

    fn visit_jsx_fragment(&mut self, _: &JSXFragment) {
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

pub(crate) fn has_jsx<T: VisitWith<HasJsxStruct>>(n: &T) -> bool {
    let mut _struct = HasJsxStruct { has_jsx: false };

    n.visit_children_with(&mut _struct);
    _struct.has_jsx
}

pub(crate) fn prepend_statement_with_body(body: &mut Option<BlockStmt>, stmts: &mut Vec<Stmt>) {
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

pub(crate) fn find_decl_ident_from_pattern(pat: &Pat) -> Vec<Ident> {
    let mut idents = Vec::new();
    match pat {
        Pat::Ident(ident) => idents.push(clone_ident(&ident.id)),
        Pat::Rest(rest) => {
            find_decl_ident_from_pattern0(rest.arg.as_ref(), &mut idents);
        }
        Pat::Array(array) => {
            find_decl_ident_from_array_pattern0(array, &mut idents);
        }
        Pat::Object(object) => {
            find_decl_ident_from_object_pattern0(object, &mut idents);
        }
        Pat::Assign(assign) => {
            find_decl_ident_from_pattern0(assign.left.as_ref(), &mut idents);
        }
        Pat::Invalid(_) => {}
        Pat::Expr(_) => {}
    };

    idents
}

pub(crate) fn find_decl_ident_from_pattern0(pat: &Pat, idents: &mut Vec<Ident>) {
    match pat {
        Pat::Ident(ident) => idents.push(clone_ident(&ident.id)),
        Pat::Rest(rest) => {
            find_decl_ident_from_pattern0(rest.arg.as_ref(), idents);
        }
        Pat::Array(array) => {
            find_decl_ident_from_array_pattern0(array, idents);
        }
        Pat::Object(object) => {
            find_decl_ident_from_object_pattern0(object, idents);
        }
        Pat::Assign(assign) => {
            find_decl_ident_from_pattern0(assign.left.as_ref(), idents);
        }
        Pat::Invalid(_) => {}
        Pat::Expr(_) => {}
    };
}

pub(crate) fn find_decl_ident_from_array_pattern0(pat: &ArrayPat, idents: &mut Vec<Ident>) {
    for elem in pat.elems.iter() {
        if let Some(elem) = elem {
            find_decl_ident_from_pattern0(elem, idents);
        }
    }
}

pub(crate) fn find_decl_ident_from_object_pattern0(pat: &ObjectPat, idents: &mut Vec<Ident>) {
    for elem in pat.props.iter() {
        match elem {
            ObjectPatProp::KeyValue(key_value) => {
                find_decl_ident_from_pattern0(key_value.value.as_ref(), idents);
            }
            ObjectPatProp::Rest(rest) => {
                find_decl_ident_from_pattern0(rest.arg.as_ref(), idents);
            }
            ObjectPatProp::Assign(assign) => idents.push(clone_ident(&assign.key)),
        }
    }
}

pub(crate) fn find_decl_ident_from_decl0(
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
                find_decl_ident_from_pattern0(&decl.name, idents);
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

pub(crate) fn find_decl_ident_from_stmt0(
    stmt: &Stmt,
    idents: &mut Vec<Ident>,
    fn_scope_idents: &mut Vec<Ident>,
) {
    match stmt {
        Stmt::Decl(decl) => {
            find_decl_ident_from_decl0(decl, idents, fn_scope_idents);
        }
        _ => {}
    }
}

pub(crate) fn find_decl_ident_from_module_item0(
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
                find_decl_ident_from_decl0(&decl.decl, idents, fn_scope_idents);
            }
            _ => {}
        },
        ModuleItem::Stmt(stmt) => {
            find_decl_ident_from_stmt0(stmt, idents, fn_scope_idents);
        }
        _ => {}
    }
}

pub(crate) fn find_decl_ident_from_stmts(stmts: &Vec<Stmt>) -> (Vec<Ident>, Vec<Ident>) {
    let mut idents = Vec::new();
    let mut fn_scope_idents = Vec::new();

    for stmt in stmts.iter() {
        find_decl_ident_from_stmt0(stmt, &mut idents, &mut fn_scope_idents);
    }

    (idents, fn_scope_idents)
}

pub(crate) fn find_decl_ident_from_module_items(
    items: &Vec<ModuleItem>,
) -> (Vec<Ident>, Vec<Ident>) {
    let mut idents = Vec::new();
    let mut fn_scope_idents = Vec::new();

    for item in items.iter() {
        find_decl_ident_from_module_item0(item, &mut idents, &mut fn_scope_idents);
    }

    (idents, fn_scope_idents)
}

pub(crate) fn generate_var_decl_stmts(
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
