fn main() {
    let source = r#"
            x = 10;
            y = 10;
            return x + y;
        "#;
    let lex = cortex_lexer::Lexer::new(file!().into(), source.into());
    let mut parser = cortex_parser::Parser::new(lex);
    let ast = parser.parse();
    match ast {
        Ok(ok) => println!("{ok}"),
        Err(_) => {}
    }
}
