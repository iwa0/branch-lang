#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Ident(String),
    Number(u32),
    Symbol(char),
    KwInt,
    KwBool,
    KwTrue,
    KwFalse,
    KwAnd,
    KwOr,
    KwIf,
    KwElse,
    KwWhile,
    Eof,
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut itr = input.chars();
    while let Some(chr) = itr.next() {
        if chr.is_ascii_alphabetic() {
            let mut ident = String::new();
            let mut chr = chr;
            loop {
                ident.push(chr);
                let orig = itr.clone();
                if let Some(a) = itr.next() {
                    if a.is_ascii_alphanumeric() {
                        chr = a;
                    } else {
                        itr = orig;
                        break;
                    }
                } else {
                    break;
                }
            }
            let tok = match &*ident {
                "int" => Token::KwInt,
                "bool" => Token::KwBool,
                "true" => Token::KwTrue,
                "false" => Token::KwFalse,
                "and" => Token::KwAnd,
                "or" => Token::KwOr,
                "if" => Token::KwIf,
                "else" => Token::KwElse,
                "while" => Token::KwWhile,
                _ => Token::Ident(ident),
            };
            tokens.push(tok);
        } else if chr.is_ascii_digit() {
            let mut num = 0;
            let mut chr = chr;
            loop {
                num = (num * 10) + chr.to_digit(10).unwrap();
                let orig = itr.clone();
                if let Some(a) = itr.next() {
                    if a.is_ascii_digit() {
                        chr = a;
                    } else {
                        itr = orig;
                        break;
                    }
                } else {
                    break;
                }
            }
            tokens.push(Token::Number(num));
        } else if chr.is_ascii_whitespace() {
        } else {
            tokens.push(Token::Symbol(chr));
        }
    }
    tokens.push(Token::Eof);
    tokens
}

pub fn advance<'a>(tokens: &mut &'a [Token]) -> &'a Token {
    let (first, rest) = tokens.split_first().unwrap();
    *tokens = rest;
    first
}

pub fn eat(tokens: &mut &[Token], chr: char) {
    let tok = advance(tokens);
    assert_eq!(*tok, Token::Symbol(chr));
}
