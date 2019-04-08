//! `mycroft-macros` exports the nightly-only procedural macro interface to the mycroft compiler.
//! To use it, invoke `mycroft_program! { "MYCROFT CODE HERE" }` in your library.
extern crate proc_macro;
use combine::Parser;
use proc_macro::TokenStream;

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

fn input_explanation<T, R>(_: R) -> T {
    panic!("Input to 'mycroft_program!' must be a mycroft program as a string literal.")
}

/// Transforms a mycroft program into a module by invoking the mycroft compiler.
#[proc_macro]
pub fn mycroft_program(input: TokenStream) -> TokenStream {
    let expr = syn::parse_expr(&input.to_string()).unwrap_or_else(input_explanation);
    let prog_str = match expr.node {
        syn::ExprKind::Lit(syn::Lit::Str(s, _)) => s,
        _ => input_explanation(()),
    };
    compile(prog_str.as_str())
}

fn input_explanation_file<T>(s: String) -> T {
    panic!("Expected file name as a string, got {}", s)
}

/// Transforms a mycroft program from a set of files into a module by invoking the mycroft
/// compiler.
#[proc_macro]
pub fn mycroft_files(input: TokenStream) -> TokenStream {
    use std::fs::File;
    use std::io::prelude::*;
    let prog_str = input
        .to_string()
        .split(',')
        .map(|file_name_str| {
            let expr = syn::parse_expr(file_name_str).unwrap_or_else(input_explanation_file);
            let file_name = match expr.node {
                syn::ExprKind::Lit(syn::Lit::Str(s, _)) => s,
                e => input_explanation_file(format!("{:?}", e)),
            };
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
