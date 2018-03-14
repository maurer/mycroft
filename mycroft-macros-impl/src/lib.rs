//! `mycroft-macros` exports the nightly-only procedural macro interface to the mycroft compiler.
//! To use it, invoke `mycroft_program! { "MYCROFT CODE HERE" }` in your library.
extern crate combine;
extern crate mycroft;
extern crate proc_macro;
#[macro_use]
extern crate proc_macro_hack;
extern crate quote;
extern crate syn;

use combine::Parser;

fn compile(prog: &str) -> String {
    let prog = match mycroft::parse::program().parse(prog) {
        Ok((p, "")) => p,
        Ok((_, trail)) => panic!("Trailing code in mycroft program: {}", trail),
        Err(e) => panic!("Parse error in mycroft program: {}", e),
    };
    let ir = mycroft::ir::Program::from_ast(prog)
        .unwrap_or_else(|e| panic!("Error raising mycroft program to IR: {}", e));
    mycroft::codegen::program(&ir).to_string()
}

fn input_explanation<T, R>(_: R) -> T {
    panic!("Input to 'mycroft_program!' must be a mycroft program as a string literal.")
}

proc_macro_item_impl! {
/// Transforms a mycroft program into a module by invoking the mycroft compiler.
pub fn mycroft_program_impl(input: &str) -> String {
    let expr = syn::parse_expr(input).unwrap_or_else(input_explanation);
    let prog_str = match expr.node {
        syn::ExprKind::Lit(syn::Lit::Str(s, _)) => s,
        _ => input_explanation(()),
    };
    compile(prog_str.as_str())
}
}

fn input_explanation_file<T>(s: String) -> T {
    panic!("Expected file name as a string, got {}", s)
}

proc_macro_item_impl! {
/// Transforms a mycroft program from a set of files into a module by invoking the mycroft
/// compiler.
pub fn mycroft_files_impl(input: &str) -> String {
    use std::io::prelude::*;
    use std::fs::File;
    let prog_str = input
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
}
