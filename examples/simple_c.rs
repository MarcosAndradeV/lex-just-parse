use lex_just_parse::lexer::{Lexer, Token, TokenKind, TokenSource};

const SOURCE: &str = "
    #include <stdio.h>
    int main() {
        printf(\"Hello, world\\n\");
    }
";

fn main() {
    let lex = Lexer::from_source(SOURCE).with_pre_rule(|lex| {
        let mut t = lex.next_token();
        if matches!(t.kind, TokenKind::Directive) {
            match t.source() {
                "#include" => {
                    if lex.peek().kind == TokenKind::Lt {
                        lex.next_token();
                        let mut buffer = String::new();
                        loop {
                            let t = lex.next_token();
                            if matches!(t.kind, TokenKind::Gt) {
                                break;
                            }
                                buffer.push_str(&t.source);
                        }
                        return Some(Token{
                            kind: TokenKind::CustomTag("include"),
                            source: TokenSource::from(buffer),
                            loc: t.loc,
                        })
                    }
                }
                _ => {
                    t.kind = TokenKind::CustomTag("UnsupportedDirective");
                    return Some(t);
                }
            }
        }
        None
    });
    for t in lex {
        println!("{}: {:?}", t.loc, t);
    }
}
