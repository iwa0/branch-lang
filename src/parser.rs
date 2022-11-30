use crate::{
    ast::{
        BinaryOpKind, Expression, ExpressionKind, NodeId, Statement, StatementKind, UnaryOpKind,
    },
    lexer::{advance, eat, Token},
    sema::Type,
};

pub fn parse_block(id: &mut NodeId, tokens: &mut &[Token], delimited: bool) -> Vec<Statement> {
    let mut stmts = Vec::new();
    while !matches!(tokens[0], Token::Eof) {
        if delimited {
            if let Token::Symbol('}') = tokens[0] {
                break;
            }
        } else if tokens.is_empty() {
            break;
        }
        let stmt = parse_stmt(id, tokens);
        stmts.push(stmt);
    }
    stmts
}

fn parse_stmt(id: &mut NodeId, tokens: &mut &[Token]) -> Statement {
    let kind = match tokens[0] {
        Token::KwInt => {
            advance(tokens);
            let name = match advance(tokens) {
                Token::Ident(name) => name.clone(),
                _ => panic!("expected identifier"),
            };
            eat(tokens, '=');
            let init = parse_expr_boxed(id, tokens);
            eat(tokens, ';');
            StatementKind::Local(Type::INT, name, init)
        }
        Token::KwBool => {
            advance(tokens);
            let name = match advance(tokens) {
                Token::Ident(name) => name.clone(),
                _ => panic!("expected identifier"),
            };
            eat(tokens, '=');
            let init = parse_expr_boxed(id, tokens);
            eat(tokens, ';');
            StatementKind::Local(Type::BOOL, name, init)
        }
        Token::KwIf => {
            advance(tokens);
            let cond = parse_expr_boxed(id, tokens);
            let then = parse_stmt(id, tokens);
            let els = if let Token::KwElse = tokens[0] {
                advance(tokens);
                let els = parse_stmt(id, tokens);
                Some(els)
            } else {
                None
            };
            StatementKind::If(cond, Box::new(then), els.map(Box::new))
        }
        Token::KwWhile => {
            advance(tokens);
            let cond = parse_expr_boxed(id, tokens);
            let body = parse_stmt(id, tokens);
            StatementKind::While(cond, Box::new(body))
        }
        Token::Symbol('{') => {
            advance(tokens);
            let block = parse_block(id, tokens, true);
            eat(tokens, '}');
            StatementKind::Block(block)
        }
        _ => {
            let left = parse_expr_boxed(id, tokens);
            let kind = if let Token::Symbol('=') = tokens[0] {
                advance(tokens);
                let right = parse_expr_boxed(id, tokens);
                StatementKind::Assign(left, right)
            } else {
                StatementKind::Expression(left)
            };
            eat(tokens, ';');
            kind
        }
    };
    Statement {
        id: id.next(),
        kind,
    }
}

fn parse_expr_boxed(id: &mut NodeId, tokens: &mut &[Token]) -> Box<Expression> {
    Box::new(parse_expr(id, tokens))
}

fn parse_expr(id: &mut NodeId, tokens: &mut &[Token]) -> Expression {
    let kind = match advance(tokens) {
        Token::Ident(name) => ExpressionKind::Ident(name.clone()),
        Token::KwTrue => ExpressionKind::BoolLiteral(true),
        Token::KwFalse => ExpressionKind::BoolLiteral(false),
        &Token::Number(n) => ExpressionKind::IntLiteral(n),
        Token::Symbol('(') => {
            let unary_kind = match &tokens[0] {
                Token::Symbol('-') => Some(UnaryOpKind::Neg),
                Token::Symbol('!') => Some(UnaryOpKind::Not),
                _ => None,
            };
            let kind = if let Some(kind) = unary_kind {
                advance(tokens);
                let left = parse_expr_boxed(id, tokens);
                ExpressionKind::Unary(kind, left)
            } else {
                let left = parse_expr_boxed(id, tokens);
                enum ParseKind {
                    None,
                    Bin(BinaryOpKind),
                    LogicalOp(fn(Box<Expression>, Box<Expression>) -> ExpressionKind),
                }
                let kind = match &tokens[0] {
                    Token::Symbol('=') => ParseKind::Bin(BinaryOpKind::Equal),
                    Token::Symbol('<') => ParseKind::Bin(BinaryOpKind::LessThan),
                    Token::Symbol('>') => ParseKind::Bin(BinaryOpKind::GreaterThan),
                    Token::Symbol('+') => ParseKind::Bin(BinaryOpKind::Add),
                    Token::Symbol('-') => ParseKind::Bin(BinaryOpKind::Sub),
                    Token::Symbol('*') => ParseKind::Bin(BinaryOpKind::Mul),
                    Token::KwAnd => ParseKind::LogicalOp(ExpressionKind::BrAnd),
                    Token::KwOr => ParseKind::LogicalOp(ExpressionKind::BrOr),
                    _ => ParseKind::None,
                };
                match kind {
                    ParseKind::None => {
                        panic!("parenthesised expression must be either unary or binary op")
                    }
                    ParseKind::Bin(kind) => {
                        advance(tokens);
                        let right = parse_expr_boxed(id, tokens);
                        ExpressionKind::Binary(kind, left, right)
                    }
                    ParseKind::LogicalOp(cons_fn) => {
                        advance(tokens);
                        let right = parse_expr_boxed(id, tokens);
                        cons_fn(left, right)
                    }
                }
            };
            eat(tokens, ')');
            kind
        }
        tok => panic!("unknown expression token: {:#?}", tok),
    };
    Expression {
        id: id.next(),
        kind,
    }
}
