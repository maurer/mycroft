//! `mycroft-macros` exports the nightly-only procedural macro interface to the mycroft compiler.
//! To use it, invoke `mycroft_program! { "MYCROFT CODE HERE" }` in your library.
#![feature(proc_macro)]
extern crate proc_macro;
extern crate mycroft;
extern crate syn;
extern crate combine;
extern crate quote;

use proc_macro::TokenStream;
use combine::Parser;

fn input_explanation<T, R>(_: R) -> T {
    panic!("Input to 'mycroft_program!' must be a mycroft program as a string literal.")
}

#[proc_macro]
/// Transforms a mycroft program into a module by invoking the mycroft compiler.
pub fn mycroft_program(input: TokenStream) -> TokenStream {
    let expr = syn::parse_expr(&input.to_string()).unwrap_or_else(input_explanation);
    let prog_str = match expr.node {
        syn::ExprKind::Lit(syn::Lit::Str(s, _)) => s,
        _ => input_explanation(()),
    };
    let prog = match mycroft::parse::program().parse(prog_str.as_str()) {
        Ok((p, "")) => p,
        Ok((_, trail)) => panic!("Trailing code in mycroft program: {}", trail),
        Err(e) => panic!("Parse error in mycroft program: {}", e),
    };
    let ir = mycroft::ir::Program::from_ast(prog).unwrap_or_else(|e| {
        panic!("Error raising mycroft program to IR: {}", e)
    });
    mycroft::codegen::program(&ir).parse().unwrap()
}
