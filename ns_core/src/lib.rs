// ns_project_root/ns_core/src/lib.rs

pub mod ast;
pub mod codegen;
pub mod error;
pub mod lexer;
pub mod opcode;
pub mod parser;
pub mod value;
pub mod vm;

// pub mod compiler_pipeline; // For future logic similar to your Python ns.py

pub fn get_ns_engine_greeting() -> String {
    let test_source = r#"
        // Test 1: Loop that doesn't run
        (print "T1 Start")
        (while false 
            (print "T1 Loop Body - FAIL") // This (print) should not execute
        ) 
        (print "T1 End")                  // The 'while' itself evaluates to 'none'

        // Test 2: A while loop whose condition is initially true,
        // but its body doesn't (and cannot with current valid syntax) change the condition variable
        // to make it a simple, terminating loop.
        // The previous error was due to invalid `(static ...)` usage inside the loop.
        // This test will now become an infinite loop if `loop_cond_for_test2` remains true.
        // To avoid an infinite loop in testing, we'll make the condition false initially for Test 2,
        // similar to Test 1, just to ensure the structure doesn't cause other errors.
        // A true multi-iteration terminating while loop test requires more features (like mutable structs and set-field, or set! for locals/globals).
        (static loop_cond_for_test2 false) // Initialize to false
        (print "T2 Start")
        (while loop_cond_for_test2 // Condition is false, loop body should not execute
            (print "T2 Loop Body - SHOULD NOT EXECUTE")
            // No (static ...) here, as it's invalid syntax for an expression
        )
        (print "T2 End - after a non-running while")

        // The final value on stack for the test report
        "while tests done"
    "#;

    let mut output_report = String::new();
    output_report.push_str("ns_core engine: ns test execution report\n");
    output_report.push_str("----------------------------------------\n");
    output_report.push_str(&format!("Source Code:\n{}\n\n", test_source));

    match lexer::tokenize(test_source) {
        Ok(tokens) => {
            let mut parser = parser::Parser::new(&tokens);
            match parser.parse_program() {
                Ok(program_node) => {
                    let module_name = "test_while_mod".to_string();
                    let codegen = codegen::CodeGenerator::new(module_name.clone());

                    match codegen.generate_program(program_node) {
                        Ok((bytecode_segments, _dependencies)) => {
                            // For debugging bytecode:
                            // output_report.push_str(&format!("[Bytecode Segments: {:?}]\n", bytecode_segments));

                            let mut vm = vm::VirtualMachine::new();
                            let main_segment_key =
                                format!("module_{}_main", module_name.replace('-', "_"));

                            match vm.load_program(bytecode_segments, &main_segment_key) {
                                Ok(_) => {
                                    output_report.push_str("VM Program Loaded Successfully.\n");
                                    output_report.push_str("VM Execution Output (from (print ...) statements) will appear in the console.\n");
                                    output_report.push_str("Running VM...\n");
                                    match vm.run() {
                                        Ok(Some(result_val)) => {
                                            output_report.push_str(&format!(
                                                "VM Final Result (from stack): {}\n",
                                                result_val
                                            ));
                                        }
                                        Ok(None) => {
                                            output_report.push_str("VM Final Result (from stack): None (or stack empty after Halt)\n");
                                        }
                                        Err(vm_err) => {
                                            output_report.push_str(&format!(
                                                "VM Runtime Error: {}\n",
                                                vm_err
                                            ));
                                        }
                                    }
                                }
                                Err(load_err) => {
                                    output_report
                                        .push_str(&format!("VM Load Error: {}\n", load_err));
                                }
                            }
                        }
                        Err(codegen_err) => {
                            output_report.push_str(&format!("Codegen Error: {}\n", codegen_err));
                        }
                    }
                }
                Err(parser_err) => {
                    output_report.push_str(&format!("Parser Error: {}\n", parser_err));
                }
            }
        }
        Err(lexer_err) => {
            output_report.push_str(&format!("Lexer Error: {}\n", lexer_err));
        }
    }
    output_report.push_str("----------------------------------------\n");
    output_report
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn run_full_pipeline_test_while_loop() {
        let report = get_ns_engine_greeting();
        println!("{}", report);

        // The final expression is "while tests done"
        assert!(report.contains("VM Final Result (from stack): while tests done"));
        assert!(report.contains("VM Program Loaded Successfully."));
        assert!(!report.contains("Lexer Error:"));
        assert!(!report.contains("Parser Error:"));
        assert!(!report.contains("Codegen Error:"));
        assert!(!report.contains("VM Load Error:"));
        // Crucially, we expect NO runtime error now that the invalid (static...) is removed.
        assert!(!report.contains("VM Runtime Error:"));
        assert!(!report.contains("Err(")); // General check for Result::Err in the report
    }
}
