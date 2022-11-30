use std::{collections::HashMap, fmt::Display, mem};

use crate::{
    ast::{
        BinaryOpKind, Expression, ExpressionKind, NodeId, Statement, StatementKind, UnaryOpKind,
    },
    sema::{Sema, Type},
};

#[derive(Debug)]
pub struct Codegen<'a> {
    sema: &'a Sema,
    id_gen: NodeId,
    pub basic_blocks: HashMap<NodeId, Vec<NodeId>>,
    pub insn_list: Vec<(NodeId, String, Vec<NodeId>, Type)>,
    label_to_insn_index: HashMap<NodeId, usize>, // stmt/expr label -> insn index
    virt_id_to_label: HashMap<NodeId, NodeId>,   // insn label -> stmt/expr label
}

impl<'a> Codegen<'a> {
    pub fn new(sema: &'a Sema, next_id: NodeId, list: &Vec<Statement>) -> Self {
        let mut this = Self {
            sema,
            id_gen: next_id,
            basic_blocks: HashMap::new(),
            insn_list: Vec::new(),
            label_to_insn_index: HashMap::new(),
            virt_id_to_label: HashMap::new(),
        };
        this.emit_block(list);
        this.replace_virt_ids();
        this
    }
    fn replace_virt_ids(&mut self) {
        for i in 0..self.insn_list.len() {
            for k in 0..self.insn_list[i].2.len() {
                let op = self.insn_list[i].2[k];
                if let Some(label) = self.virt_id_to_label.get(&op) {
                    let insn_idx = self.label_to_insn_index[&label];
                    let insn_label = self.insn_list[insn_idx].0;
                    self.insn_list[i].2[k] = insn_label;
                }
            }
        }
        let bbs = mem::replace(&mut self.basic_blocks, HashMap::new());
        for (vid, mut preds) in bbs {
            let label = self.virt_id_to_label[&vid];
            let insn_idx = self.label_to_insn_index[&label];
            let insn_label = self.insn_list[insn_idx].0;
            self.basic_blocks
                .entry(insn_label)
                .or_insert_with(Vec::new)
                .append(&mut preds);
        }
    }
    fn insert_label(&mut self, id: NodeId) {
        let idx = self.insn_list.len();
        let old = self.label_to_insn_index.insert(id, idx);
        if let Some(old) = old {
            assert!(old == idx);
        }
    }
    fn alloc_virt_id(&mut self, id: NodeId) -> NodeId {
        let virt_id = self.id_gen.next();
        let old = self.virt_id_to_label.insert(virt_id, id);
        assert!(old.is_none());
        virt_id
    }
    pub fn fmt_label(&self, label: NodeId) -> String {
        if let Some((_, name)) = self.sema.local_defs.get(&label) {
            format!("{}.{:?}", name, label)
        } else {
            format!("{:?}", label)
        }
    }
    fn emit_insn(&mut self, label: NodeId, name: &str, ops: &[NodeId], ty: &Type) -> NodeId {
        self.insn_list
            .push((label, name.to_string(), ops.into(), ty.clone()));
        label
    }
    fn emit_alloca(&mut self, label: NodeId, val: NodeId, ty: &Type) -> NodeId {
        self.emit_insn(label, &format!("alloca({:?})", ty), &[val], &Type::PTR)
    }
    fn emit_store(&mut self, label: NodeId, dst: NodeId, src: NodeId) -> NodeId {
        self.emit_insn(label, "store", &[dst, src], &Type::VOID)
    }
    fn emit_load(&mut self, label: NodeId, src: NodeId, ty: &Type) -> NodeId {
        self.emit_insn(label, "load", &[src], ty)
    }
    fn emit_const<T: Display>(&mut self, label: NodeId, t: T, ty: &Type) -> NodeId {
        self.emit_insn(label, &format!("const({})", t), &[], &ty)
    }
    fn emit_unary(&mut self, label: NodeId, kind: UnaryOpKind, op: NodeId, ty: &Type) -> NodeId {
        self.emit_insn(label, &format!("un.{:?}", kind), &[op], ty)
    }
    fn emit_binary(
        &mut self,
        label: NodeId,
        kind: BinaryOpKind,
        left: NodeId,
        right: NodeId,
        ty: &Type,
    ) -> NodeId {
        self.emit_insn(label, &format!("bin.{:?}", kind), &[left, right], ty)
    }
    fn emit_br(&mut self, label: NodeId, target: NodeId) -> NodeId {
        let vid = self.alloc_virt_id(target);

        self.basic_blocks
            .entry(vid)
            .or_insert_with(Vec::new)
            .push(label);

        self.emit_insn(label, "br", &[vid], &Type::VOID)
    }
    fn emit_cmp_br(
        &mut self,
        label: NodeId,
        cmp_kind: &str,
        left: NodeId,
        right: NodeId,
        tru: NodeId,
        fals: NodeId,
    ) -> NodeId {
        let vid_true = self.alloc_virt_id(tru);
        let vid_false = self.alloc_virt_id(fals);

        self.basic_blocks
            .entry(vid_true)
            .or_insert_with(Vec::new)
            .push(label);
        self.basic_blocks
            .entry(vid_false)
            .or_insert_with(Vec::new)
            .push(label);

        self.emit_insn(
            label,
            &format!("cmp_br.{}", cmp_kind),
            &[left, right, vid_true, vid_false],
            &Type::VOID,
        )
    }
    fn emit_cond_br(&mut self, label: NodeId, cond: NodeId, tru: NodeId, fals: NodeId) -> NodeId {
        let vid_true = self.alloc_virt_id(tru);
        let vid_false = self.alloc_virt_id(fals);

        self.basic_blocks
            .entry(vid_true)
            .or_insert_with(Vec::new)
            .push(label);
        self.basic_blocks
            .entry(vid_false)
            .or_insert_with(Vec::new)
            .push(label);

        self.emit_insn(label, "cond_br", &[cond, vid_true, vid_false], &Type::VOID)
    }

    fn emit_block(&mut self, list: &Vec<Statement>) {
        for stmt in list {
            self.emit_stmt(stmt);
        }
    }
    fn emit_stmt(&mut self, stmt: &Statement) {
        self.insert_label(stmt.id);
        match &stmt.kind {
            StatementKind::Placeholder => unreachable!(),
            StatementKind::Local(_, _, init) => {
                let a = self.emit_expr(init);
                let (ty, _) = &self.sema.local_defs[&stmt.id];
                self.emit_alloca(stmt.id, a, ty);
            }
            StatementKind::Assign(left, right) => {
                let dst = self.emit_expr(left);
                let src = self.emit_expr(right);
                self.emit_store(stmt.id, dst, src);
            }
            StatementKind::Block(stmts) => {
                self.emit_block(stmts);
            }
            StatementKind::If(cond, then, els) => {
                self.emit_expr_br(cond);
                self.emit_stmt(then);
                if let Some(els) = els {
                    self.emit_stmt(els);
                }
            }
            StatementKind::While(cond, body) => {
                let id = self.id_gen.next();
                self.emit_br(id, cond.id);
                self.emit_expr_br(cond);
                self.emit_stmt(body);
            }
            StatementKind::Expression(e) => {
                self.emit_expr(e);
            }
            StatementKind::Control(sub) => {
                self.emit_stmt(sub);
                let target = self.sema.ctl_transfer[&stmt.id];
                self.emit_br(stmt.id, target);
            }
            StatementKind::Exit => {
                self.emit_insn(stmt.id, "exit", &[], &Type::VOID);
            }
        }
    }
    fn emit_expr(&mut self, expr: &Expression) -> NodeId {
        self.insert_label(expr.id);
        let es = &self.sema.expr[&expr.id];
        match &expr.kind {
            ExpressionKind::Placeholder => unreachable!(),
            ExpressionKind::Ident(_) => {
                //
                self.sema.use_to_def[&expr.id]
            }
            ExpressionKind::BoolLiteral(val) => self.emit_const(expr.id, val, &es.ty),
            ExpressionKind::IntLiteral(val) => self.emit_const(expr.id, val, &es.ty),
            ExpressionKind::Unary(kind, op) => {
                let op = self.emit_expr(op);
                self.emit_unary(expr.id, *kind, op, &es.ty)
            }
            ExpressionKind::Binary(kind, left, right) => {
                let left = self.emit_expr(left);
                let right = self.emit_expr(right);
                self.emit_binary(expr.id, *kind, left, right, &es.ty)
            }
            ExpressionKind::BrAnd(_, _) | ExpressionKind::BrOr(_, _) => {
                unreachable!()
            }
            ExpressionKind::CastLValueToRValue(op) => {
                let op = self.emit_expr(op);
                self.emit_load(expr.id, op, &es.ty)
            }
            ExpressionKind::CastRValueToBValue(_) => {
                unreachable!()
            }
            ExpressionKind::CastBValueToRValue(op) => self.emit_expr_sel(op),
        }
    }
    fn emit_expr_sel(&mut self, expr: &Expression) -> NodeId {
        self.insert_label(expr.id);
        let labels = self.sema.bval_labels[&expr.id];
        match &expr.kind {
            ExpressionKind::Unary(kind, op) => match kind {
                UnaryOpKind::Neg => unreachable!(),
                UnaryOpKind::Not => {
                    let op = self.emit_expr_sel(op);
                    self.emit_unary(expr.id, UnaryOpKind::Not, op, &Type::BOOL)
                }
            },
            ExpressionKind::Binary(kind, left, right) => match kind {
                BinaryOpKind::Equal | BinaryOpKind::LessThan | BinaryOpKind::GreaterThan => {
                    let left = self.emit_expr(left);
                    let right = self.emit_expr(right);
                    self.emit_binary(expr.id, *kind, left, right, &Type::BOOL)
                }
                BinaryOpKind::Add | BinaryOpKind::Sub | BinaryOpKind::Mul => unreachable!(),
            },
            ExpressionKind::BrAnd(cond, and_also) => {
                self.emit_cond_select(expr.id, cond, and_also, false, labels.not_taken)
            }
            ExpressionKind::BrOr(cond, or_else) => {
                self.emit_cond_select(expr.id, cond, or_else, true, labels.taken)
            }
            a => unreachable!("{:#?}", a),
        }
    }
    fn emit_expr_br(&mut self, expr: &Expression) {
        self.insert_label(expr.id);
        let labels = &self.sema.bval_labels[&expr.id];
        match &expr.kind {
            ExpressionKind::Unary(kind, op) => match kind {
                UnaryOpKind::Neg => unreachable!(),
                UnaryOpKind::Not => {
                    self.emit_expr_br(op);
                }
            },
            ExpressionKind::Binary(kind, left, right) => match kind {
                BinaryOpKind::Equal | BinaryOpKind::LessThan | BinaryOpKind::GreaterThan => {
                    let llvm_style = true;
                    if llvm_style {
                        let cond = self.emit_expr_sel(expr);
                        let id = self.id_gen.next();
                        self.emit_cond_br(id, cond, labels.taken, labels.not_taken);
                    } else {
                        let left = self.emit_expr(left);
                        let right = self.emit_expr(right);
                        match kind {
                            BinaryOpKind::Equal
                            | BinaryOpKind::LessThan
                            | BinaryOpKind::GreaterThan => {
                                self.emit_cmp_br(
                                    expr.id,
                                    &format!("{:?}", kind),
                                    left,
                                    right,
                                    labels.taken,
                                    labels.not_taken,
                                );
                            }
                            BinaryOpKind::Add | BinaryOpKind::Sub | BinaryOpKind::Mul => {
                                unreachable!()
                            }
                        }
                    }
                }
                BinaryOpKind::Add | BinaryOpKind::Sub | BinaryOpKind::Mul => unreachable!(),
            },
            ExpressionKind::BrAnd(cond, and_also) => {
                self.emit_expr_br(cond);
                self.emit_expr_br(and_also);
            }
            ExpressionKind::BrOr(cond, or_else) => {
                self.emit_expr_br(cond);
                self.emit_expr_br(or_else);
            }
            ExpressionKind::CastRValueToBValue(op) => {
                let cond = self.emit_expr(op);
                self.emit_cond_br(expr.id, cond, labels.taken, labels.not_taken);
            }
            a => unreachable!("{:#?}", a),
        }
    }
    /*
    res=<default>
    if <cond> {
        res=<other>
    }
    res
    */
    fn emit_cond_select(
        &mut self,
        label: NodeId,
        cond: &Expression,
        op: &Expression,
        default: bool,
        join: NodeId,
    ) -> NodeId {
        let id1 = self.id_gen.next();
        let id2 = self.id_gen.next();
        let id3 = self.id_gen.next();
        let id4 = self.id_gen.next();

        let default_val = self.emit_const(id1, default, &Type::BOOL);
        let res_var = self.emit_alloca(id2, default_val, &Type::BOOL);

        self.emit_expr_br(cond);
        let tmp = self.emit_expr_sel(op);
        self.emit_store(id3, res_var, tmp);
        self.emit_br(id4, join);

        self.insert_label(join);
        self.emit_load(label, res_var, &Type::BOOL)
    }
}
