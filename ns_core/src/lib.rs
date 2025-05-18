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
        // Struct definition
        (struct Point (x y))

        // Function using structs
        (fn make_point (x_val y_val)
            (Point x_val y_val)
        )

        (fn move_point (p dx dy)
            (let new_x (+ (get p x) dx))
            (let new_y (+ (get p y) dy))
            (set p x new_x) // Mutate p.x
            (set p y new_y) // Mutate p.y
            p                 // Return the modified point
        )

        (static p1 (make_point 10 20))
        (print p1) // Expected: (struct Point { x: 10, y: 20 }) or similar

        (static p2 (move_point p1 5 5))
        (print p2) // Expected: (struct Point { x: 15, y: 25 })
        (print p1) // Expected: (struct Point { x: 15, y: 25 }) (p1 was mutated)

        (print (is_struct p1)) // Expected: true

        // Begin special form
        (print (begin 
                  "first_in_begin" 
                  (+ 100 200) 
                  "last_in_begin"
               )) // Expected: last_in_begin (begin evaluates to its last expression)

        // New primitives
        (print (% 10 3))   // Expected: 1
        (print (>= 5 5))   // Expected: true
        (print (>= 5 4))   // Expected: true
        (print (>= 4 5))   // Expected: false
        (print (<= 4 5))   // Expected: true
        (print (<= 5 5))   // Expected: true
        (print (<= 5 4))   // Expected: false
        (print (!= 5 4))   // Expected: true
        (print (!= 5 5))   // Expected: false

        // Test error (this should stop execution and be reported)
        // (error "This is a test error from ns code") 
        // For now, let's return a normal value to let other tests pass.
        // Later, we can have a separate test for (error).
        
        "all features test done" // Final value
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
                    let module_name = "test_parity_mod".to_string();
                    let codegen = codegen::CodeGenerator::new(module_name.clone());

                    match codegen.generate_program(program_node) {
                        Ok((bytecode_segments, _dependencies)) => {
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
    fn run_full_pipeline_test_parity_features() {
        let report = get_ns_engine_greeting();
        println!("{}", report);

        assert!(report.contains("VM Final Result (from stack): all features test done"));
        assert!(report.contains("VM Program Loaded Successfully."));
        assert!(!report.contains("Lexer Error:"));
        assert!(!report.contains("Parser Error:"));
        assert!(!report.contains("Codegen Error:"));
        assert!(!report.contains("VM Load Error:"));
        assert!(!report.contains("VM Runtime Error:")); // Expect no runtime errors for this test
        assert!(!report.contains("Err("));
    }
}
