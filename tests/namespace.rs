use mycroft_macros::mycroft_program;

// Compile test to make sure we accept namespaced identifiers.

mycroft_program!(
    r#"
Foo(String)
xlat_foos: Foo(out) <- Foo(_) + mod_name::xlat
"#
);

mod mod_name {
    use super::mycroft_program::*;
    pub fn xlat(_: &ModNameXlatIn) -> Vec<ModNameXlatOut> {
        Vec::new()
    }
}
