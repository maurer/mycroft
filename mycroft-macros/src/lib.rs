//! `mycroft-macros` exports the nightly-only procedural macro interface to the mycroft compiler.
//! To use it, invoke `mycroft_program! { "MYCROFT CODE HERE" }` in your library.
extern crate proc_macro;
use combine::Parser;
use proc_macro::TokenStream;
use syn::{parse_macro_input, LitStr};

fn compile(prog: &str) -> TokenStream {
    let prog = match mycroft::parse::program().parse(prog) {
        Ok((p, "")) => p,
        Ok((_, trail)) => panic!("Trailing code in mycroft program: {}", trail),
        Err(e) => panic!("Parse error in mycroft program: {}", e),
    };
    let ir = mycroft::ir::Program::from_ast(prog)
        .unwrap_or_else(|e| panic!("Error raising mycroft program to IR: {}", e));
    mycroft::codegen::program(&ir).into()
}

/// Transforms a mycroft program into a module by invoking the mycroft compiler.
#[proc_macro]
pub fn mycroft_program(input: TokenStream) -> TokenStream {
    let prog_str = parse_macro_input!(input as LitStr);
    compile(prog_str.value().as_str())
}

/// Transforms a mycroft program from a set of files into a module by invoking the mycroft
/// compiler.
#[proc_macro]
pub fn mycroft_files(input: TokenStream) -> TokenStream {
    use std::fs::File;
    use std::io::prelude::*;
    use syn::parse::Parser;
    use syn::punctuated::Punctuated;
    use syn::Token;
    let parser = Punctuated::<LitStr, Token![,]>::parse_terminated;
    let file_names = parser.parse(input).unwrap();
    let prog_str = file_names
        .iter()
        .map(|file_name_lit| {
            let file_name = file_name_lit.value();
            let mut fd = File::open(&file_name).expect(&format!(
                "Could not read mycroft program file: {}",
                file_name
            ));
            let mut contents = String::new();
            fd.read_to_string(&mut contents).expect(&format!(
                "Could not read mycroft program file: {}",
                file_name
            ));
            contents
        })
        .collect::<Vec<_>>()
        .join("\n");
    compile(prog_str.as_str())
}
