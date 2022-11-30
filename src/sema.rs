use std::{collections::HashMap, mem};

use crate::ast::{
    BinaryOpKind, Expression, ExpressionKind, NodeId, Statement, StatementKind, UnaryOpKind,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Type(TypeKind);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TypeKind {
    Void,
    Ptr,
    Int,
    Bool,
    Err,
}

impl Type {
    pub const VOID: Self = Self::from_kind(TypeKind::Void);
    pub const PTR: Self = Self::from_kind(TypeKind::Ptr);
    pub const INT: Self = Self::from_kind(TypeKind::Int);
    pub const BOOL: Self = Self::from_kind(TypeKind::Bool);
    pub const ERR: Self = Self::from_kind(TypeKind::Err);
    pub const fn from_kind(kind: TypeKind) -> Self {
        Type(kind)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ExpressionCategory {
    LValue,
    RValue,
    BValue,
    Err,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ExprSema {
    pub cat: ExpressionCategory,
    pub ty: Type,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BValLabels {
    pub taken: NodeId,
    pub not_taken: NodeId,
}

#[derive(Debug)]
pub struct Sema {
    name_to_local: HashMap<String, NodeId>,
    //
    pub id_gen: NodeId,
    pub errs: Vec<(NodeId, String)>,
    //
    pub local_defs: HashMap<NodeId, (Type, String)>,
    pub use_to_def: HashMap<NodeId, NodeId>,
    pub expr: HashMap<NodeId, ExprSema>,
    pub bval_labels: HashMap<NodeId, BValLabels>,
    pub ctl_transfer: HashMap<NodeId, NodeId>,
}

impl ExprSema {
    fn new(cat: ExpressionCategory, ty: Type) -> Self {
        Self { cat, ty }
    }
}

impl BValLabels {
    pub fn new(taken: NodeId, not_taken: NodeId) -> Self {
        Self { taken, not_taken }
    }
}

impl Sema {
    fn report(&mut self, id: NodeId, desc: &str) {
        self.errs.push((id, desc.to_string()));
    }
    pub fn analyze(next_id: NodeId, program: &mut Vec<Statement>) -> Self {
        let mut this = Self {
            name_to_local: HashMap::new(),
            id_gen: next_id,
            errs: Vec::new(),
            local_defs: HashMap::new(),
            use_to_def: HashMap::new(),
            expr: HashMap::new(),
            bval_labels: HashMap::new(),
            ctl_transfer: HashMap::new(),
        };
        let exit = Statement {
            id: this.id_gen.next(),
            kind: StatementKind::Exit,
        };
        let exit_id = exit.id;
        program.push(exit);
        this.analyze_block(program, exit_id);
        this
    }
    fn analyze_block(&mut self, stmts: &mut [Statement], succ: NodeId) {
        for i in 1..stmts.len() {
            let succ = stmts[i].id;
            self.analyze_stmt(&mut stmts[i - 1], succ);
        }
        if let Some(stmt) = stmts.last_mut() {
            self.analyze_stmt(stmt, succ);
        }
    }
    fn analyze_stmt(&mut self, stmt: &mut Statement, succ: NodeId) {
        match &mut stmt.kind {
            StatementKind::Placeholder => unreachable!(),
            StatementKind::Local(ty, name, init) => {
                self.analyze_expr_rval(init);
                self.local_defs.insert(stmt.id, (*ty, name.clone()));
                self.name_to_local.insert(name.clone(), stmt.id);
            }
            StatementKind::Assign(left, right) => {
                self.analyze_expr_lval(left);
                self.analyze_expr_rval(right);
            }
            StatementKind::Block(stmts) => self.analyze_block(stmts, succ),
            StatementKind::If(cond, then, els) => {
                self.analyze_expr_bval(
                    cond,
                    BValLabels::new(then.id, els.as_ref().map_or(succ, |a| a.id)),
                );
                self.analyze_stmt(then, succ);
                let then_ctl = self.insert_control(then);
                self.ctl_transfer.insert(then_ctl, succ);
                if let Some(els) = els {
                    self.analyze_stmt(els, succ);
                    let else_ctl = self.insert_control(els);
                    self.ctl_transfer.insert(else_ctl, succ);
                }
            }
            StatementKind::While(cond, body) => {
                self.analyze_expr_bval(cond, BValLabels::new(body.id, succ));
                let loop_header = cond.id;
                self.analyze_stmt(body, loop_header);
                let loop_ctl = self.insert_control(body);
                self.ctl_transfer.insert(loop_ctl, loop_header);
            }
            StatementKind::Expression(expr) => {
                self.analyze_expr_rval(expr);
            }
            StatementKind::Control(_) => unreachable!(),
            StatementKind::Exit => {}
        }
    }
    fn analyze_expr_lval(&mut self, expr: &mut Expression) -> Type {
        let sema = self.analyze_expr(expr, None);
        match sema.cat {
            ExpressionCategory::LValue => {}
            ExpressionCategory::RValue | ExpressionCategory::BValue => {
                self.report(expr.id, "expected lvalue");
            }
            ExpressionCategory::Err => {}
        }
        sema.ty
    }
    fn analyze_expr_rval(&mut self, expr: &mut Expression) -> Type {
        let sema = self.analyze_expr(expr, None);
        match sema.cat {
            ExpressionCategory::LValue => {
                let cast_expr = self.insert_cast_expr(expr, ExpressionKind::CastLValueToRValue);
                self.expr.insert(
                    cast_expr,
                    ExprSema::new(ExpressionCategory::RValue, sema.ty),
                );
            }
            ExpressionCategory::RValue => {}
            ExpressionCategory::BValue => {
                let cast_expr = self.insert_cast_expr(expr, ExpressionKind::CastBValueToRValue);
                self.expr.insert(
                    cast_expr,
                    ExprSema::new(ExpressionCategory::RValue, sema.ty),
                );
            }
            ExpressionCategory::Err => {}
        }
        sema.ty
    }
    fn analyze_expr_bval(&mut self, expr: &mut Expression, labels: BValLabels) -> Type {
        let sema = self.analyze_expr(expr, Some(labels));
        let mut insert_rvaltobval = false;
        match sema.cat {
            ExpressionCategory::LValue => {
                let cast_expr = self.insert_cast_expr(expr, ExpressionKind::CastLValueToRValue);
                self.expr.insert(
                    cast_expr,
                    ExprSema::new(ExpressionCategory::RValue, sema.ty),
                );
                insert_rvaltobval = true;
            }
            ExpressionCategory::RValue => {
                insert_rvaltobval = true;
            }
            ExpressionCategory::BValue => {}
            ExpressionCategory::Err => {}
        }
        let e = if insert_rvaltobval {
            let cast_expr = self.insert_cast_expr(expr, ExpressionKind::CastRValueToBValue);
            self.expr.insert(
                cast_expr,
                ExprSema::new(ExpressionCategory::BValue, Type::BOOL),
            );
            cast_expr
        } else {
            expr.id
        };
        self.bval_labels.insert(e, labels);
        sema.ty
    }
    fn analyze_expr(&mut self, expr: &mut Expression, labels: Option<BValLabels>) -> ExprSema {
        let sema = match &mut expr.kind {
            ExpressionKind::Placeholder => unreachable!(),
            ExpressionKind::Ident(name) => {
                if let Some(&a) = self.name_to_local.get(name) {
                    self.use_to_def.insert(expr.id, a);
                    ExprSema::new(ExpressionCategory::LValue, self.local_defs[&a].0)
                } else {
                    self.report(expr.id, "name does not exist");
                    ExprSema::new(ExpressionCategory::Err, Type::ERR)
                }
            }
            ExpressionKind::BoolLiteral(_) => ExprSema::new(ExpressionCategory::RValue, Type::BOOL),
            ExpressionKind::IntLiteral(_) => ExprSema::new(ExpressionCategory::RValue, Type::INT),
            ExpressionKind::Unary(kind, op) => match kind {
                UnaryOpKind::Neg => {
                    self.analyze_expr_rval(op);
                    ExprSema::new(ExpressionCategory::RValue, Type::INT)
                }
                UnaryOpKind::Not => {
                    let mut labels = labels
                        .unwrap_or_else(|| BValLabels::new(self.id_gen.next(), self.id_gen.next()));
                    mem::swap(&mut labels.taken, &mut labels.not_taken);
                    self.bval_labels.insert(expr.id, labels);

                    self.analyze_expr_bval(op, labels);
                    ExprSema::new(ExpressionCategory::BValue, Type::BOOL)
                }
            },
            ExpressionKind::Binary(kind, left, right) => match kind {
                BinaryOpKind::Equal | BinaryOpKind::LessThan | BinaryOpKind::GreaterThan => {
                    self.analyze_expr_rval(left);
                    self.analyze_expr_rval(right);
                    ExprSema::new(ExpressionCategory::BValue, Type::BOOL)
                }
                BinaryOpKind::Add | BinaryOpKind::Sub | BinaryOpKind::Mul => {
                    self.analyze_expr_rval(left);
                    self.analyze_expr_rval(right);
                    ExprSema::new(ExpressionCategory::RValue, Type::INT)
                }
            },
            ExpressionKind::BrAnd(cond, and_also) => {
                let labels = labels
                    .unwrap_or_else(|| BValLabels::new(self.id_gen.next(), self.id_gen.next()));
                self.bval_labels.insert(expr.id, labels);

                self.analyze_expr_bval(cond, BValLabels::new(and_also.id, labels.not_taken));
                self.analyze_expr_bval(and_also, labels);
                ExprSema::new(ExpressionCategory::BValue, Type::BOOL)
            }
            ExpressionKind::BrOr(cond, or_else) => {
                let labels = labels
                    .unwrap_or_else(|| BValLabels::new(self.id_gen.next(), self.id_gen.next()));
                self.bval_labels.insert(expr.id, labels);

                self.analyze_expr_bval(cond, BValLabels::new(labels.taken, or_else.id));
                self.analyze_expr_bval(or_else, labels);
                ExprSema::new(ExpressionCategory::BValue, Type::BOOL)
            }
            ExpressionKind::CastLValueToRValue(_)
            | ExpressionKind::CastRValueToBValue(_)
            | ExpressionKind::CastBValueToRValue(_) => unreachable!(),
        };
        self.expr.insert(expr.id, sema);
        sema
    }

    fn insert_control(&mut self, stmt: &mut Statement) -> NodeId {
        let id = self.id_gen.next();
        let outer = Statement {
            id,
            kind: StatementKind::Placeholder,
        };
        let inner = mem::replace(stmt, outer);
        stmt.kind = StatementKind::Control(Box::new(inner));
        id
    }

    fn insert_cast_expr<F>(&mut self, expr: &mut Expression, f: F) -> NodeId
    where
        F: Fn(Box<Expression>) -> ExpressionKind,
    {
        let id = self.id_gen.next();
        let outer = Expression {
            id,
            kind: ExpressionKind::Placeholder,
        };
        let inner = mem::replace(expr, outer);
        expr.kind = f(Box::new(inner));
        id
    }
}
