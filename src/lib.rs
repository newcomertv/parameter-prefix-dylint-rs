#![feature(rustc_private)]
#![feature(let_chains)]
#![allow(unused_extern_crates)]
#![allow(unused_imports)]
#![allow(clippy::collapsible_match)]

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
use rustc_errors::{Applicability, Diag};
use rustc_hir::{intravisit::FnKind, Body, Expr, ExprKind, FnDecl, HirId, LetExpr, PatKind};
use rustc_lint::{LateContext, LateLintPass, LintContext};
use rustc_session::{declare_lint_pass, declare_tool_lint};
use rustc_span::{def_id::LocalDefId, Span};

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
    "Prefix function parameters with 'p_'"
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
            if _sig.name.as_str().contains("py") {
                // exclude pyo3 bindings
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
                    || ident.name.as_str().starts_with("self")
                {
                    continue;
                }

                let param_name = ident.name.as_str();
                let new_name = format!("p_{}", param_name);

                renames.push((ident.span, new_name.clone()));

                let found_spans = pattern_match_for_ident_in_expr(cx, body.value, param_name);
                for span in found_spans {
                    renames.push((span, new_name.clone()));
                }

                cx.opt_span_lint(
                    FUNCTION_PARAMETER_PREFIX,
                    Some(ident.span),
                    "this parameter can be renamed",
                    |diag| {
                        diag.multipart_suggestion(
                            "prefix the parameter name with 'p_'",
                            renames
                                .iter()
                                .map(|(span, new_name)| (*span, new_name.clone()))
                                .collect::<Vec<_>>(),
                            Applicability::MachineApplicable, // This indicates the suggestion can be automatically applied
                        );
                    },
                );
            }
        }
    }
}

#[allow(unused_doc_comments)]
fn pattern_match_for_ident_in_expr<'tcx>(
    _cx: &LateContext<'tcx>,
    expr: &'tcx Expr,
    ident: &str,
) -> Vec<Span> {
    match expr.kind {
        /// Allow anonymous constants from an inline `const` block
        ExprKind::ConstBlock(_) => {
            // do nothing, it's const
            vec![]
        }
        /// An array (e.g., `[a, b, c, d]`).
        ExprKind::Array(l_expr_arr) => {
            let mut spans = vec![];
            for l_expr in l_expr_arr {
                spans.append(&mut pattern_match_for_ident_in_expr(_cx, l_expr, ident));
            }
            spans
        }
        /// A function call.
        ///
        /// The first field resolves to the function itself (usually an `ExprKind::Path`),
        /// and the second field is the list of arguments.
        /// This also represents calling the constructor of
        /// tuple-like ADTs such as tuple structs and enum variants.
        ExprKind::Call(_function_name, l_params_list) => {
            let mut spans = vec![];
            for l_param in l_params_list {
                spans.append(&mut pattern_match_for_ident_in_expr(_cx, l_param, ident));
            }
            spans
        }
        /// A method call (e.g., `x.foo::<'static, Bar, Baz>(a, b, c, d)`).
        ///
        /// The `PathSegment` represents the method name and its generic arguments
        /// (within the angle brackets).
        /// The `&Expr` is the expression that evaluates
        /// to the object on which the method is being called on (the receiver),
        /// and the `&[Expr]` is the rest of the arguments.
        /// Thus, `x.foo::<Bar, Baz>(a, b, c, d)` is represented as
        /// `ExprKind::MethodCall(PathSegment { foo, [Bar, Baz] }, x, [a, b, c, d], span)`.
        /// The final `Span` represents the span of the function and arguments
        /// (e.g. `foo::<Bar, Baz>(a, b, c, d)` in `x.foo::<Bar, Baz>(a, b, c, d)`
        ///
        /// To resolve the called method to a `DefId`, call [`type_dependent_def_id`] with
        /// the `hir_id` of the `MethodCall` node itself.
        ///
        /// [`type_dependent_def_id`]: ../../rustc_middle/ty/struct.TypeckResults.html#method.type_dependent_def_id
        ExprKind::MethodCall(
            _angle_brackets_content,
            _method_name_probably,
            l_param_list,
            _span_of_call,
        ) => {
            let mut spans = vec![];
            for l_param in l_param_list {
                spans.append(&mut pattern_match_for_ident_in_expr(_cx, l_param, ident));
            }
            spans
        }
        /// A tuple (e.g., `(a, b, c, d)`).
        ExprKind::Tup(l_param_list) => {
            let mut spans = vec![];
            for l_param in l_param_list {
                spans.append(&mut pattern_match_for_ident_in_expr(_cx, l_param, ident));
            }
            spans
        }
        /// A binary operation (e.g., `a + b`, `a * b`).
        ExprKind::Binary(_op_kind, l_left_param, l_right_param) => {
            let mut spans = vec![];
            spans.append(&mut pattern_match_for_ident_in_expr(
                _cx,
                l_left_param,
                ident,
            ));
            spans.append(&mut pattern_match_for_ident_in_expr(
                _cx,
                l_right_param,
                ident,
            ));
            spans
        }
        /// A unary operation (e.g., `!x`, `*x`).
        ExprKind::Unary(_op_kind, l_expr) => pattern_match_for_ident_in_expr(_cx, l_expr, ident),
        /// A literal (e.g., `1`, `"foo"`).
        ExprKind::Lit(_) => {
            vec![]
        }
        /// A cast (e.g., `foo as f64`).
        ExprKind::Cast(l_expr, _type) => pattern_match_for_ident_in_expr(_cx, l_expr, ident),
        /// A type ascription (e.g., `x: Foo`). See RFC 3307.
        ExprKind::Type(l_expr, _type) => pattern_match_for_ident_in_expr(_cx, l_expr, ident),
        /// Wraps the expression in a terminating scope.
        /// This makes it semantically equivalent to `{ let _t = expr; _t }`.
        ///
        /// This construct only exists to tweak the drop order in AST lowering.
        /// An example of that is the desugaring of `for` loops.
        ExprKind::DropTemps(l_expr) => pattern_match_for_ident_in_expr(_cx, l_expr, ident),
        /// A `let $pat = $expr` expression.
        ///
        /// These are not [`LetStmt`] and only occur as expressions.
        /// The `let Some(x) = foo()` in `if let Some(x) = foo()` is an example of `Let(..)`.
        ExprKind::Let(_let_expr) => {
            // TODO: complicated
            vec![]
        }
        /// An `if` block, with an optional else block.
        ///
        /// I.e., `if <expr> { <expr> } else { <expr> }`.
        ExprKind::If(l_logical_expr, l_true_arm, l_false_arm) => {
            let mut spans = vec![];
            spans.append(&mut pattern_match_for_ident_in_expr(
                _cx,
                l_logical_expr,
                ident,
            ));
            spans.append(&mut pattern_match_for_ident_in_expr(_cx, l_true_arm, ident));
            if let Some(l_false_arm) = l_false_arm {
                spans.append(&mut pattern_match_for_ident_in_expr(_cx, l_false_arm, ident));
            }
            spans
        }
        /// A conditionless loop (can be exited with `break`, `continue`, or `return`).
        ///
        /// I.e., `'label: loop { <block> }`.
        ///
        /// The `Span` is the loop header (`for x in y`/`while let pat = expr`).
        ExprKind::Loop(l_block, _label, _loop_source, _loop_span) => {
            let mut spans = vec![];
            for statement in l_block.stmts {
                match statement.kind {
                    rustc_hir::StmtKind::Expr(l_expr) => {
                        spans.append(&mut pattern_match_for_ident_in_expr(_cx, l_expr, ident));
                    }
                    rustc_hir::StmtKind::Item(..) => {}
                    rustc_hir::StmtKind::Semi(l_expr) => {
                        spans.append(&mut pattern_match_for_ident_in_expr(_cx, l_expr, ident));
                    }
                    rustc_hir::StmtKind::Let(l_let) => {
                        if let PatKind::Binding(
                            _binding_mode,
                            _hir_id,
                            l_ident,
                            _subpattern,
                        ) = l_let.pat.kind
                        {
                            //TODO: complex
                            if l_ident.name.as_str() == ident {
                                spans.push(l_ident.span);
                            }
                        }
                    }
                }
            }
            spans
        }
        /// A `match` block, with a source that indicates whether or not it is
        /// the result of a desugaring, and if so, which kind.
        ExprKind::Match(l_expr, l_arms, _match_source) => {
            let mut spans = vec![];
            spans.append(&mut pattern_match_for_ident_in_expr(_cx, l_expr, ident));
            for arm in l_arms {
                spans.append(&mut pattern_match_for_ident_in_expr(_cx, arm.body, ident));
            }
            spans
        }
        /// A closure (e.g., `move |a, b, c| {a + b + c}`).
        ///
        /// The `Span` is the argument block `|...|`.
        ///
        /// This may also be a coroutine literal or an `async block` as indicated by the
        /// `Option<Movability>`.
        ExprKind::Closure(_closure) => {
            // TODO : complex
            vec![]
        }
        /// A block (e.g., `'label: { ... }`).
        ExprKind::Block(l_block, _label) => {
            let mut spans = vec![];
            for statement in l_block.stmts {
                match statement.kind {
                    rustc_hir::StmtKind::Expr(l_expr) => {
                        spans.append(&mut pattern_match_for_ident_in_expr(_cx, l_expr, ident));
                    }
                    rustc_hir::StmtKind::Item(..) => {}
                    rustc_hir::StmtKind::Semi(l_expr) => {
                        spans.append(&mut pattern_match_for_ident_in_expr(_cx, l_expr, ident));
                    }
                    rustc_hir::StmtKind::Let(l_let) => {
                        if let PatKind::Binding(
                            _binding_mode,
                            _hir_id,
                            l_ident,
                            _subpattern,
                        ) = l_let.pat.kind
                        {
                            // TODO: complex
                            if l_ident.name.as_str() == ident {
                                spans.push(l_ident.span);
                            }
                        }
                    }
                }
            }
            spans
        }

        /// An assignment (e.g., `a = foo()`).
        ExprKind::Assign(l_left_assign, l_right_assing, _assign_span) => {
            let mut spans = vec![];
            spans.append(&mut pattern_match_for_ident_in_expr(_cx, l_left_assign, ident));
            spans.append(&mut pattern_match_for_ident_in_expr(_cx, l_right_assing, ident));
            spans
        }
        /// An assignment with an operator.
        ///
        /// E.g., `a += 1`.
        ExprKind::AssignOp(_bin_op, l_left_expr, l_right_expr) => {
            let mut spans = vec![];
            spans.append(&mut pattern_match_for_ident_in_expr(_cx, l_left_expr, ident));
            spans.append(&mut pattern_match_for_ident_in_expr(_cx, l_right_expr, ident));
            spans
        }
        /// Access of a named (e.g., `obj.foo`) or unnamed (e.g., `obj.0`) struct or tuple field.
        ExprKind::Field(l_expr, _ident) => {
            pattern_match_for_ident_in_expr(_cx, l_expr, ident)
        },
        /// An indexing operation (`foo[2]`).
        /// Similar to [`ExprKind::MethodCall`], the final `Span` represents the span of the brackets
        /// and index.
        ExprKind::Index(l_left_expr, l_right_expr, _index_span) => {
            let mut spans = vec![];
            spans.append(&mut pattern_match_for_ident_in_expr(_cx, l_left_expr, ident));
            spans.append(&mut pattern_match_for_ident_in_expr(_cx, l_right_expr, ident));
            spans
        },

        /// Path to a definition, possibly containing lifetime or type parameters.
        ExprKind::Path(_q_path) => {
            // do nothing
            vec![]
        },

        /// A referencing operation (i.e., `&a` or `&mut a`).
        ExprKind::AddrOf(_borrow_kind, _mutability, l_expr) => {
            pattern_match_for_ident_in_expr(_cx, l_expr, ident)
        },
        /// A `break`, with an optional label to break.
        ExprKind::Break(_destination, l_expr_opt) => {
            if let Some(l_expr) = l_expr_opt {
                pattern_match_for_ident_in_expr(_cx, l_expr, ident)
            } else {
                vec![]
            }
        },
        /// A `continue`, with an optional label.
        ExprKind::Continue(_destination) => {
            vec![]
        },
        /// A `return`, with an optional value to be returned.
        ExprKind::Ret(l_expr_opt) => {
            if let Some(l_expr) = l_expr_opt {
                pattern_match_for_ident_in_expr(_cx, l_expr, ident)
            } else {
                vec![]
            }
        },
        /// A `become`, with the value to be returned.
        ExprKind::Become(l_expr) => {
            pattern_match_for_ident_in_expr(_cx, l_expr, ident)
        },

        /// Inline assembly (from `asm!`), with its outputs and inputs.
        ExprKind::InlineAsm(_inline_asm) => {
            // do nothing
            vec![]
        },

        /// Field offset (`offset_of!`)
        ExprKind::OffsetOf(_ty, _field) => {
            // do nothing
            vec![]
        },

        /// A struct or struct-like variant literal expression.
        ///
        /// E.g., `Foo {x: 1, y: 2}`, or `Foo {x: 1, .. base}`,
        /// where `base` is the `Option<Expr>`.
        ExprKind::Struct(_q_path, l_expr_fields, l_expr_opt) => {
            let mut spans = vec![];
            for field in l_expr_fields {
                spans.extend(pattern_match_for_ident_in_expr(_cx, field.expr, ident));
            }
            
            if let Some(l_expr) = l_expr_opt {
                spans.extend(pattern_match_for_ident_in_expr(_cx, l_expr, ident))
            } 
            spans
        },

        /// An array literal constructed from one repeated element.
        ///
        /// E.g., `[1; 5]`. The first expression is the element
        /// to be repeated; the second is the number of times to repeat it.
        ExprKind::Repeat(l_expr, _array_len) => {
            pattern_match_for_ident_in_expr(_cx, l_expr, ident)
        },

        /// A suspension point for coroutines (i.e., `yield <expr>`).
        ExprKind::Yield(l_expr, _yield_source) => {
            pattern_match_for_ident_in_expr(_cx, l_expr, ident)
        },

        /// A placeholder for an expression that wasn't syntactically well formed in some way.
        ExprKind::Err(_error_guaranteed) => {
            // do nothing
            vec![]
        },
    }
}

#[test]
fn ui() {
    dylint_testing::ui_test(
        env!("CARGO_PKG_NAME"),
        &std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("ui"),
    );
}
