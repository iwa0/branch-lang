pub mod ast;
mod codegen;
mod lexer;
mod parser;
pub mod sema;

use ast::{Expression, ExpressionKind, NodeId};

use crate::{codegen::Codegen, sema::Sema};

pub fn mk_expr(id: NodeId, kind: ExpressionKind) -> Box<Expression> {
    Box::new(Expression { id, kind })
}

pub fn compile(source: &str) {
    let mut id = NodeId::first();

    let top_stmts = {
        let toks = lexer::tokenize(source);
        let mut cursor = &*toks;
        let top_stmts = parser::parse_block(&mut id, &mut cursor, false);
        assert!(matches!(cursor[0], lexer::Token::Eof));
        println!("tokens: {:?}", &toks);
        // println!("raw ast: {:#?}", &top_stmts);
        top_stmts
    };

    let mut top_stmts_cloned = top_stmts.clone();
    let sema = {
        let sema = Sema::analyze(id, &mut top_stmts_cloned);
        println!("resolved ast: {:#?}", &top_stmts_cloned);
        //println!("{:#?}", &sema);
        println!("errors: {:?}", sema.errs);
        sema
    };

    let cg = Codegen::new(&sema, sema.id_gen, &top_stmts_cloned);
    for (id, insn, ops, ty) in &cg.insn_list {
        if let Some(preds) = cg.basic_blocks.get(id) {
            // basic block boundary
            println!();
            println!("-- {:?} {:?}", id, preds);
        };
        let mut operands_dump = String::new();
        for i in 0..ops.len() {
            operands_dump.push_str(" ");
            operands_dump.push_str(&cg.fmt_label(ops[i]));
            if i < ops.len() - 1 {
                operands_dump.push_str(",");
            }
        }
        println!(
            "{} :: {}{} : {:?}",
            cg.fmt_label(*id),
            insn,
            operands_dump,
            ty
        );
    }
    //println!("{:#?}", &cg);
}

fn main() {
    let src = match 1 {
        1 => include_str!("examples/1simple.br"),
        2 => include_str!("examples/2casts.br"),
        3 => include_str!("examples/3elseif.br"),
        4 => include_str!("examples/4while.br"),
        _ => panic!(),
    };
    compile(src);
}
