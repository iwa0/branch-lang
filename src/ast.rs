use std::{fmt::Debug, num::NonZeroU32};

use crate::sema::Type;

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct NodeId(pub NonZeroU32);

impl Debug for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("%{}", self.0.get()))
    }
}

#[derive(Clone, Debug)]
pub struct Statement {
    pub id: NodeId,
    pub kind: StatementKind,
}

#[derive(Clone, Debug)]
pub struct Expression {
    pub id: NodeId,
    pub kind: ExpressionKind,
}

#[derive(Clone, Debug)]
pub enum StatementKind {
    Placeholder, // sentinel, used only during construction
    Local(Type, String, Box<Expression>),
    Assign(Box<Expression>, Box<Expression>),
    Block(Vec<Statement>),
    If(Box<Expression>, Box<Statement>, Option<Box<Statement>>),
    While(Box<Expression>, Box<Statement>),
    Expression(Box<Expression>),
    // inserted by compiler
    Control(Box<Statement>),
    Exit,
}

#[derive(Clone, Debug)]
pub enum ExpressionKind {
    Placeholder, // sentinel, used only during construction
    Ident(String),
    BoolLiteral(bool),
    IntLiteral(u32),
    Unary(UnaryOpKind, Box<Expression>),
    Binary(BinaryOpKind, Box<Expression>, Box<Expression>),
    //
    //BrNot
    BrAnd(Box<Expression>, Box<Expression>),
    BrOr(Box<Expression>, Box<Expression>),
    // inserted by compiler
    CastLValueToRValue(Box<Expression>),
    CastRValueToBValue(Box<Expression>),
    CastBValueToRValue(Box<Expression>),
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOpKind {
    Neg,
    Not,
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOpKind {
    Equal,
    //NotEqual,
    LessThan,
    //GreaterEqual,
    //LessEqual,
    GreaterThan,
    Add,
    Sub,
    Mul,
}

impl BinaryOpKind {
    /*pub fn not(self) -> Self {
        match self {
            BinaryOpKind::Equal => Self::NotEqual,
            BinaryOpKind::NotEqual => Self::Equal,
            BinaryOpKind::LessThan => Self::GreaterEqual,
            BinaryOpKind::GreaterEqual => Self::LessThan,
            BinaryOpKind::LessEqual => Self::GreaterThan,
            BinaryOpKind::GreaterThan => Self::LessEqual,
            BinaryOpKind::Add | BinaryOpKind::Sub | BinaryOpKind::Mul => panic!(),
        }
    }*/
}

impl NodeId {
    pub fn first() -> Self {
        Self(NonZeroU32::new(1).unwrap())
    }
    pub fn next(&mut self) -> Self {
        let new = self.0.checked_add(1).unwrap();
        std::mem::replace(self, Self(new))
    }
}
