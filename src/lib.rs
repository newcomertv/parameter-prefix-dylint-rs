#![feature(rustc_private)]
#![feature(let_chains)]
#![allow(unused_extern_crates)]
#![allow(unused_imports)]

extern crate rustc_arena;
extern crate rustc_ast;
extern crate rustc_ast_pretty;
extern crate rustc_attr;
extern crate rustc_data_structures;
extern crate rustc_errors;
extern crate rustc_hir;
extern crate rustc_hir_pretty;
extern crate rustc_index;
extern crate rustc_infer;
extern crate rustc_lexer;
extern crate rustc_middle;
extern crate rustc_mir_dataflow;
extern crate rustc_parse;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_trait_selection;

use clippy_utils::diagnostics::span_lint;
use rustc_lint::{LateContext, LateLintPass, LintContext};
use rustc_session::{declare_lint_pass, declare_tool_lint};
use rustc_hir::{intravisit::FnKind, Body, Expr, ExprKind, FnDecl, HirId, PatKind};
use rustc_span::{Span, def_id::LocalDefId};
use rustc_errors::{Applicability, Diag};

dylint_linting::declare_late_lint! {
    ///     # Function Parameter Prefix Lint
    /// 
    /// ### What it does
    /// 
    /// This lint checks if all function parameters start with `p_`.
    /// This convention comes from large C/C++ codebases where it is common to use prefixes to identify the origin of a variable.
    /// 
    /// ### Why is this bad?
    /// 
    /// It can be difficult for a reviewer to understand the origin of a variable if the prefix is not used.
    /// 
    /// ### Known problems
    /// Machine fix isn't implemented.
    /// This requires finding all identifiers in the function body implying some more complex AST walk.
    /// 
    /// ### Example
    /// ```rust
    /// // example code where a warning is issued
    /// fn foo(a: i32, b: i32) {
    ///     println!("{} {}", a, b);
    /// }
    /// ```
    /// Use instead:
    /// ```rust
    /// // example code that does not raise a warning
    /// fn foo(p_a: i32, p_b: i32) {
    ///     println!("{} {}", p_a, p_b);
    /// }
    /// ```
    pub FUNCTION_PARAMETER_PREFIX,
    Warn,
    "description goes here"
}

impl<'tcx> LateLintPass<'tcx> for FunctionParameterPrefix {
    // A list of things you might check can be found here:
    // https://doc.rust-lang.org/stable/nightly-rustc/rustc_lint/trait.LateLintPass.html

    fn check_fn(
        &mut self,
        cx: &LateContext<'tcx>,
        _kind: FnKind<'tcx>,
        _decl: &'tcx FnDecl,
        body: &'tcx Body,
        _span: Span,
        _hir_id: LocalDefId,
    ) { 
        // Skip if its a closure parameter
        if let FnKind::Closure = _kind {
            return;
        }
        
        if let FnKind::Method(_sig, _ty) = _kind {
            // if method name contains "py" then skip
            if _sig.name.as_str().contains("py") { // exclude pyo3 bindings
                return;
            }
        }

        // Checks if all function parameters start with 'p_'
        for param in body.params {
            if let PatKind::Binding(_binding_mode, _hir_id, ident, _subpattern) = param.pat.kind {
                let mut renames = vec![];

                if ident.name.as_str().chars().next().unwrap().is_uppercase() 
                || ident.name.as_str().starts_with("p_")
                || ident.name.as_str().starts_with('_')
                || ident.name.as_str().starts_with("self"){
                    continue;
                }
                
                let param_name = ident.name.as_str();
                let new_name = format!("p_{}", param_name);

                renames.push((ident.span, new_name.clone()));

                // TODO: We need to parse the AST of the function body to find all the identifiers
                if let ExprKind::Block(block, _) = body.value.kind {
                    for stmt in block.stmts {
                        match stmt.kind {
                            rustc_hir::StmtKind::Let(local) => {
                                if let PatKind::Binding(_binding_mode, _hir_id, ident, _subpattern) = local.pat.kind {
                                    if ident.name.as_str() == param_name {
                                        renames.push((ident.span, new_name.clone()));
                                    }
                                }
                            }
                            rustc_hir::StmtKind::Item(..) => {}
                            _ => {}
                        }
                    }
                }
                
                cx.opt_span_lint(FUNCTION_PARAMETER_PREFIX, Some(ident.span), "this parameter can be renamed", |diag| {
                    
                    
                    diag.multipart_suggestion(
                            "prefix the parameter name with 'p_'",
                            renames.iter().map(|(span, new_name)| {
                                (*span, new_name.clone())
                            }).collect::<Vec<_>>(),
                            Applicability::MachineApplicable, // This indicates the suggestion can be automatically applied
                        );
                });
            } 
        }
    }
}

#[test]
fn ui() {
    dylint_testing::ui_test(
        env!("CARGO_PKG_NAME"),
        &std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("ui"),
    );
}
