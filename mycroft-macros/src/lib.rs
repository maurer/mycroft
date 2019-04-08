//! `mycroft-macros` exports the procedural macro interface to the mycroft compiler.
//! To use it, invoke `mycroft_program! { "MYCROFT CODE HERE" }` in your library.
extern crate mycroft_macros_impl;
#[macro_use]
extern crate proc_macro_hack;
#[doc(hidden)]
pub use mycroft_macros_impl::*;

proc_macro_item_decl! {
    mycroft_program! => mycroft_program_impl
}

proc_macro_item_decl! {
    mycroft_files! => mycroft_files_impl
}
