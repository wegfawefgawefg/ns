// ns_project_root/ns_core/src/lib.rs

pub mod ast;
pub mod codegen;
pub mod error;
pub mod lexer;
pub mod opcode;
pub mod parser;
pub mod value;
pub mod vm;

pub fn get_ns_engine_greeting() -> String {
    let test_source = r#"
        (fn add (a b) 
            (+ a b)
        )

        (fn simple_greet (name)
            (print "Hello,")
            (print name)
            (print "from ns function!")
            "done greeting" // Return value of simple_greet
        )

        (static result1 (add 10 25))
        (print result1) // Expected console: Output: 35

        (let message (simple_greet "Developer")) // Call simple_greet
        (print message) // Expected console: Output: done greeting (return value of simple_greet)

        // Test lambda
        (let my_adder (lambda (x y) (+ x y)))
        (print (my_adder 7 8)) // Expected console: Output: 15
        
        // Final value on stack for the test report
        (add result1 (my_adder 1 2)) // 35 + 3 = 38
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
                    let module_name = "test_fn_call_mod".to_string();
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
    fn run_full_pipeline_test_function_calls() {
        let report = get_ns_engine_greeting();
        println!("{}", report);

        assert!(report.contains("VM Final Result (from stack): 38")); // 35 + 3
        assert!(report.contains("VM Program Loaded Successfully."));
        assert!(!report.contains("Lexer Error:"));
        assert!(!report.contains("Parser Error:"));
        assert!(!report.contains("Codegen Error:"));
        assert!(!report.contains("VM Load Error:"));
        assert!(!report.contains("VM Runtime Error:"));
        assert!(!report.contains("Err("));
    }
}
