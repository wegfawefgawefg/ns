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

/// Runs a test "ns" program through the full pipeline (lex, parse, codegen, vm)
/// and returns a string summarizing the process and the final result.
/// Note: Output from (print ...) statements within the "ns" code will go to
/// the main program's stdout, not into the string returned by this function.
pub fn get_ns_engine_greeting() -> String {
    let test_source = r#"
        (static my_list (list 1 "two" true))
        (print (is_list my_list))       // Expected console output: Output: true
        (print (first my_list))         // Expected console output: Output: 1
        (print (rest my_list))          // Expected console output: Output: ("two" true)
        
        (static none_val none)
        (print (is_none none_val))      // Expected console output: Output: true
        (print (is_list none_val))      // Expected console output: Output: true (as 'none' is a valid list for is_list)

        (print (is_number (first my_list))) // Expected console output: Output: true
        
        // The final expression's value will be the VM's result
        (cons 0 my_list) 
    "#;

    let mut output_report = String::new();
    output_report.push_str("ns_core engine: ns test execution report\n");
    output_report.push_str("----------------------------------------\n");
    output_report.push_str(&format!("Source Code:\n{}\n\n", test_source));

    match lexer::tokenize(test_source) {
        Ok(tokens) => {
            // output_report.push_str(&format!("[Lexer Tokens: {:?}]\n", tokens)); // Uncomment for verbose token logging
            let mut parser = parser::Parser::new(&tokens);
            match parser.parse_program() {
                Ok(program_node) => {
                    // output_report.push_str(&format!("[Parsed AST: {:?}]\n", program_node)); // Uncomment for verbose AST logging
                    let module_name = "test_lib_mod".to_string(); // Name for this test module context
                    let codegen = codegen::CodeGenerator::new(module_name.clone());

                    match codegen.generate_program(program_node) {
                        Ok((bytecode_segments, _dependencies)) => {
                            // Prefixed dependencies with _
                            // output_report.push_str(&format!("[Bytecode Segments: {:?}]\n", bytecode_segments)); // Uncomment for verbose bytecode
                            // output_report.push_str(&format!("[Discovered Dependencies: {:?}]\n", _dependencies)); // Use _dependencies

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

// Basic unit test for the greeting function.
// This test will print the full report from get_ns_engine_greeting.
// The actual (print ...) outputs from the ns code will also go to the test console.
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn run_full_pipeline_test() {
        let report = get_ns_engine_greeting();
        println!("{}", report); // Print the full report for test visibility

        // Check if the report indicates a successful VM run and a specific result.
        // The final expression `(cons 0 my_list)` should result in `(0 1 "two" true)`.
        // Note: The exact string representation depends on your Value::Display implementation.
        assert!(report.contains("VM Final Result (from stack): (0 1 \"two\" true)"));
        // You can add more assertions here, e.g., checking for "VM Program Loaded Successfully."
        assert!(report.contains("VM Program Loaded Successfully."));
        // Check that no major errors were reported in the summary string
        // Allow "Error:" if it's part of a success message like "No error:"
        // A more robust check might be for specific error prefixes or ensuring no "Runtime Error:", "Parser Error:", etc.
        // For now, checking for "Err(" (from Result::Err debug fmt) and specific error types is good.
        assert!(!report.contains("Lexer Error:"));
        assert!(!report.contains("Parser Error:"));
        assert!(!report.contains("Codegen Error:"));
        assert!(!report.contains("VM Load Error:"));
        assert!(!report.contains("VM Runtime Error:"));
        assert!(!report.contains("Err("));
    }
}
