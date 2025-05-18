// ns_project_root/ns_core/src/lib.rs

pub mod ast;
pub mod codegen;
pub mod error;
pub mod lexer;
pub mod opcode;
pub mod parser;
pub mod value;
pub mod vm;

use crate::opcode::BytecodeInstruction;
use crate::value::Value;
use std::collections::HashMap;

// --- Compilation and Execution Helper Functions ---

/// Compiles NotScheme source code into bytecode segments.
pub fn compile_ns_source_to_bytecode(
    source: &str,
    module_name_str: &str,
) -> Result<(HashMap<String, Vec<BytecodeInstruction>>, String), String> {
    let tokens = match lexer::tokenize(source) {
        Ok(t) => t,
        Err(e) => return Err(format!("Lexer Error: {}", e)),
    };

    let mut parser = parser::Parser::new(&tokens);
    let program_node = match parser.parse_program() {
        Ok(pn) => pn,
        Err(e) => return Err(format!("Parser Error: {}", e)),
    };

    let codegen = codegen::CodeGenerator::new(module_name_str.to_string());
    let (bytecode_segments, _dependencies) = match codegen.generate_program(program_node) {
        Ok(bs_and_deps) => bs_and_deps,
        Err(e) => return Err(format!("Codegen Error: {}", e)),
    };

    let main_segment_key = format!("module_{}_main", module_name_str.replace('-', "_"));
    Ok((bytecode_segments, main_segment_key))
}

/// Executes pre-compiled NotScheme bytecode segments.
pub fn execute_ns_bytecode(
    bytecode_segments: HashMap<String, Vec<BytecodeInstruction>>,
    main_segment_key: &str,
) -> Result<Option<Value>, String> {
    let mut vm = vm::VirtualMachine::new();
    if let Err(e) = vm.load_program(bytecode_segments, main_segment_key) {
        return Err(format!("VM Load Error: {}", e));
    }
    match vm.run() {
        Ok(result_val_opt) => Ok(result_val_opt),
        Err(vm_err) => Err(format!("VM Execution Error: {}", vm_err)),
    }
}

/// Compiles and runs NotScheme source code, returning a report string.
pub fn run_ns_source_code_and_report(
    source: &str,
    module_name_str: &str,
) -> Result<String, String> {
    let mut output_report = String::new();
    output_report.push_str(&format!(
        "ns_core engine: Test Execution Report for Module '{}'\n",
        module_name_str
    ));
    output_report.push_str("----------------------------------------\n");

    match compile_ns_source_to_bytecode(source, module_name_str) {
        Ok((bytecode_segments, main_segment_key)) => {
            output_report.push_str("VM Program Compiled & Loaded Successfully.\n");
            output_report.push_str("Running VM...\n");
            match execute_ns_bytecode(bytecode_segments, &main_segment_key) {
                Ok(Some(result_val)) => {
                    output_report
                        .push_str(&format!("VM Final Result (from stack): {}\n", result_val));
                }
                Ok(None) => {
                    output_report.push_str(
                        "VM Final Result (from stack): None (or stack empty after Halt)\n",
                    );
                }
                Err(vm_exec_err_str) => {
                    output_report.push_str(&format!(
                        "VM Execution Stopped/Errored: {}\n",
                        vm_exec_err_str
                    ));
                }
            }
        }
        Err(compile_err_str) => {
            output_report.push_str(&format!(
                "Compilation Pipeline Error: {}\n",
                compile_err_str
            ));
        }
    }

    output_report.push_str("----------------------------------------\n");
    Ok(output_report)
}

// --- Test Helper Functions (used by different test modules) ---

pub fn get_original_complex_test_report() -> String {
    let test_source = r#"
        (struct Point (x y))
        (fn make_point (x_val y_val) (Point x_val y_val))
        (fn move_point (p dx dy)
            (let ( (new_x (+ (get p x) dx))
                   (new_y (+ (get p y) dy)) ) 
                (set p x new_x) 
                (set p y new_y) 
                p 
            )
        )
        (static p1 (make_point 10 20))
        (print p1) 
        (static p2 (move_point p1 5 5)) 
        (print p2) 
        (print p1) 
        (print (is_struct p1)) 
        (print (begin "first" (+ 1 2) "last_in_begin")) 
        (print (% 10 3))   
        (print (>= 5 5))   
        (print (!= 5 5))  
        (print "About to throw an error...")
        (error "This is a deliberate test error!") 
        (print "THIS SHOULD NOT BE PRINTED")
        "This string should not be reached due to error" 
    "#;
    run_ns_source_code_and_report(test_source, "test_original_complex").unwrap_or_else(|e| e)
}

pub fn get_minimal_let_test_report() -> String {
    let test_source = r#"(let x 10 x)"#;
    run_ns_source_code_and_report(test_source, "test_minimal_let").unwrap_or_else(|e| e)
}

// --- Standard Tests ---
#[cfg(test)]
mod tests {
    use super::*;
    
    use crate::value::Value;
    
    
    use std::rc::Rc;

    fn expect_result(source: &str, module_name: &str, expected_val: Value) {
        println!("\n--- Testing: {} ---", module_name);
        match compile_ns_source_to_bytecode(source, module_name) {
            Ok((bytecode_segments, main_segment_key)) => {
                match execute_ns_bytecode(bytecode_segments, &main_segment_key) {
                    Ok(Some(actual_val)) => {
                        assert_eq!(
                            actual_val, expected_val,
                            "Test '{}' failed. Expected {:?}, got {:?}",
                            module_name, expected_val, actual_val
                        );
                        println!("Result for '{}': {} (Correct)", module_name, actual_val);
                    }
                    Ok(None) => {
                        panic!(
                            "Test '{}' VM execution resulted in None, expected {:?}",
                            module_name, expected_val
                        );
                    }
                    Err(e_str) => {
                        panic!("Test '{}' VM execution error: {}", module_name, e_str);
                    }
                }
            }
            Err(e_str) => {
                panic!(
                    "Test '{}' compilation pipeline error: {}",
                    module_name, e_str
                );
            }
        }
    }

    fn expect_vm_error(source: &str, module_name: &str, expected_error_substring: &str) {
        println!("\n--- Testing Error Case: {} ---", module_name);
        match compile_ns_source_to_bytecode(source, module_name) {
            Ok((bytecode_segments, main_segment_key)) => {
                match execute_ns_bytecode(bytecode_segments, &main_segment_key) {
                    Ok(Some(val)) => panic!(
                        "Test '{}' expected VM error containing '{}', but got value: {:?}",
                        module_name, expected_error_substring, val
                    ),
                    Ok(None) => panic!(
                        "Test '{}' expected VM error containing '{}', but got None",
                        module_name, expected_error_substring
                    ),
                    Err(e_str) => {
                        assert!(e_str.contains(expected_error_substring), "Test '{}' failed. Expected error string containing '{}', got actual error string '{}'", module_name, expected_error_substring, e_str);
                        println!("For '{}', Got Expected Error: {}", module_name, e_str);
                    }
                }
            }
            Err(e_str) => {
                panic!(
                    "Test '{}' compilation pipeline error (when expecting a VM error): {}",
                    module_name, e_str
                );
            }
        }
    }

    #[test]
    fn test_original_complex_evaluation_with_error() {
        let report = get_original_complex_test_report();
        println!("\n--- Test Report for test_original_complex_evaluation_with_error ---");
        println!("{}", report);
        println!("--- End Test Report ---\n");
        assert!(report.contains("VM Program Compiled & Loaded Successfully."));
        assert!(report.contains("VM Execution Stopped/Errored: VM Execution Error: VM Runtime Error: User Error: This is a deliberate test error!"));
        assert!(!report.contains("Lexer Error:"));
        assert!(!report.contains("Parser Error:"));
        assert!(!report.contains("Codegen Error:"));
        assert!(!report.contains("VM Load Error:"));
    }

    #[test]
    fn test_minimal_let_evaluation() {
        expect_result("(let x 10 x)", "minimal_let", Value::Number(10.0));
    }

    #[test]
    fn test_let_scoping_with_body() {
        expect_result(
            "(let x 10 (print x) x)",
            "let_print_body",
            Value::Number(10.0),
        );
        expect_result("(let x 10 (+ x 5))", "let_calc_body", Value::Number(15.0));
    }

    #[test]
    fn test_error_primitive_stops_execution() {
        let source = r#" (print "Before error") (error "Test user error") (print "After error - should not print") "#;
        expect_vm_error(
            source,
            "error_primitive_test",
            "User Error: Test user error",
        );
    }

    #[test]
    fn test_arithmetic_ops() {
        expect_result("(+ 10 5)", "add_simple", Value::Number(15.0));
        expect_result("(- 10 5)", "sub_simple", Value::Number(5.0));
        expect_result("(* 10 5)", "mul_simple", Value::Number(50.0));
        expect_result("(/ 10 5)", "div_simple", Value::Number(2.0));
        expect_result("(/ 10 4)", "div_float", Value::Number(2.5));
        expect_result("(% 10 3)", "mod_simple", Value::Number(1.0));
        expect_vm_error("(/ 10 0)", "div_by_zero", "Division by zero");
        expect_vm_error("(% 10 0)", "mod_by_zero", "Modulo by zero");
    }

    #[test]
    fn test_comparison_ops() {
        expect_result("(= 10 10)", "eq_num_true", Value::Boolean(true));
        expect_result("(= 10 5)", "eq_num_false", Value::Boolean(false));
        expect_result("(= \"a\" \"a\")", "eq_str_true", Value::Boolean(true));
        expect_result("(= \"a\" \"b\")", "eq_str_false", Value::Boolean(false));
        expect_result("(= true true)", "eq_bool_true", Value::Boolean(true));
        expect_result("(= none none)", "eq_none_true", Value::Boolean(true));
        expect_result(
            "(= (list 1 2) (list 1 2))",
            "eq_list_true",
            Value::Boolean(true),
        );
        expect_result(
            "(= (list 1 2) (list 1 3))",
            "eq_list_false",
            Value::Boolean(false),
        );
        expect_result("(> 10 5)", "gt_true", Value::Boolean(true));
        expect_result("(< 5 10)", "lt_true", Value::Boolean(true));
        expect_result("(>= 10 10)", "gte_true", Value::Boolean(true));
        expect_result("(<= 5 5)", "lte_true", Value::Boolean(true));
        expect_result("(!= 10 5)", "neq_true", Value::Boolean(true));
        expect_result("(!= 10 10)", "neq_false", Value::Boolean(false));
    }

    #[test]
    fn test_logical_not() {
        expect_result("(not false)", "not_false", Value::Boolean(true));
        expect_result("(not true)", "not_true", Value::Boolean(false));
        expect_result("(not none)", "not_none", Value::Boolean(true));
        expect_result("(not 0)", "not_zero_is_false", Value::Boolean(false));
        expect_result(
            "(not \"\")",
            "not_empty_str_is_false",
            Value::Boolean(false),
        );
        expect_result(
            "(not (list))",
            "not_empty_list_is_false",
            Value::Boolean(false),
        );
    }

    #[test]
    fn test_type_predicates() {
        expect_result("(is_none none)", "is_none_true", Value::Boolean(true));
        expect_result("(is_none false)", "is_none_false", Value::Boolean(false));
        expect_result("(is_boolean true)", "is_bool_true", Value::Boolean(true));
        expect_result("(is_number 123.0)", "is_num_true", Value::Boolean(true));
        expect_result("(is_string \"hi\")", "is_str_true", Value::Boolean(true));
        expect_result("(is_list (list 1))", "is_list_true", Value::Boolean(true));
        expect_result("(is_list none)", "is_list_none_true", Value::Boolean(true));
        expect_result(
            "(is_function (lambda (x) x))",
            "is_func_true",
            Value::Boolean(true),
        );
        expect_result(
            "(struct Foo ()) (is_struct (Foo))",
            "is_struct_true",
            Value::Boolean(true),
        );
    }

    #[test]
    fn test_list_primitives() {
        expect_result("(first (list 1 2 3))", "first_ok", Value::Number(1.0));
        expect_vm_error("(first (list))", "first_empty_err", "FIRST on empty list");
        expect_vm_error(
            "(first none)",
            "first_none_err",
            "FIRST expects list, got none",
        );
        expect_result(
            "(rest (list 1 2 3))",
            "rest_ok",
            Value::List(Rc::new(vec![Value::Number(2.0), Value::Number(3.0)])),
        );
        expect_result("(rest (list 1))", "rest_one_to_none", Value::NoneValue);
        expect_vm_error("(rest (list))", "rest_empty_err", "REST on empty list");
        expect_vm_error(
            "(rest none)",
            "rest_none_err",
            "REST expects list, got none",
        );
        expect_result(
            "(cons 0 none)",
            "cons_to_none",
            Value::List(Rc::new(vec![Value::Number(0.0)])),
        );
        expect_result(
            "(cons 1 (list 2 3))",
            "cons_to_list",
            Value::List(Rc::new(vec![
                Value::Number(1.0),
                Value::Number(2.0),
                Value::Number(3.0),
            ])),
        );
    }

    #[test]
    fn test_let_scoping_advanced() {
        expect_result(
            "(let x 10 (let y (+ x 5) (+ x y)))",
            "let_nested_sum",
            Value::Number(25.0),
        );
        expect_result(
            "(let x 10 (let x 20 x))",
            "let_shadow_inner_val",
            Value::Number(20.0),
        );
        expect_result(
            "(let x 10 (begin (let x 20 (print x)) x))",
            "let_shadow_outer_val",
            Value::Number(10.0),
        );
        expect_result(
            "(let ((a 1) (b (+ a 10))) (+ a b))",
            "let_multi_sequential_correct",
            Value::Number(12.0), // Corrected expectation
        );
    }

    #[test]
    fn test_if_special_form() {
        expect_result("(if true 1 2)", "if_true", Value::Number(1.0));
        expect_result("(if false 1 2)", "if_false", Value::Number(2.0));
        expect_result("(if 0 1 2)", "if_zero_truthy", Value::Number(1.0));
        expect_result("(if \"\" 1 2)", "if_empty_str_truthy", Value::Number(1.0));
        expect_result("(if none 1 2)", "if_none_falsy", Value::Number(2.0));
        expect_result(
            "(if (list) 1 2)",
            "if_empty_list_truthy",
            Value::Number(1.0),
        );
    }

    #[test]
    fn test_begin_special_form() {
        expect_result("(begin 1 2 3)", "begin_multi", Value::Number(3.0));
        expect_result("(begin 1)", "begin_single", Value::Number(1.0));
        expect_result("(begin)", "begin_empty", Value::NoneValue);
    }

    #[test]
    fn test_while_loop_simple() {
        let simple_while_source = "(while false (print \"should not print\"))";
        expect_result(simple_while_source, "while_false_loop", Value::NoneValue);
    }

    #[test]
    fn test_closures_simple() {
        // Corrected structure for scoping: ((make_adder 5)) to (make_adder 5)
        let source = r#"
            (let make_adder (lambda (x) (lambda (y) (+ x y)))
                (let add5 (make_adder 5) // Corrected: removed extra parens around (make_adder 5)
                    (add5 3)
                )
            )
        "#;
        expect_result(
            source,
            "closure_simple_add_scoped_fixed",
            Value::Number(8.0),
        );
    }
}

// --- Speed Tests Module ---
#[cfg(test)]
mod speed_tests {
    use super::*;
    use crate::value::Value;
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::rc::Rc;
    use std::time::Instant;

    fn run_notscheme_benchmark(
        test_name: &str,
        ns_source: &str,
        module_name: &str,
        expected_result_opt: Option<Value>,
    ) {
        println!("\n--- NotScheme Benchmark: {} ---", test_name);
        let compile_start = Instant::now();
        let (bytecode_segments, main_segment_key) =
            match compile_ns_source_to_bytecode(ns_source, module_name) {
                Ok(res) => res,
                Err(e) => {
                    println!("Compilation FAILED: {}", e);
                    panic!("Benchmark compilation failed for {}", test_name);
                }
            };
        let compile_duration = compile_start.elapsed();
        println!("Compilation Time: {:.3?}", compile_duration);

        let exec_start = Instant::now();
        let result = match execute_ns_bytecode(bytecode_segments, &main_segment_key) {
            Ok(res_opt) => res_opt,
            Err(e) => {
                println!("Execution FAILED: {}", e);
                panic!("Benchmark execution failed for {}", test_name);
            }
        };
        let exec_duration = exec_start.elapsed();
        println!("NotScheme Execution Time: {:.3?}", exec_duration);

        if let Some(expected_result) = expected_result_opt {
            match result {
                Some(actual_result) => {
                    assert_eq!(
                        actual_result, expected_result,
                        "Benchmark '{}' result mismatch. Expected {:?}, got {:?}",
                        test_name, expected_result, actual_result
                    );
                    println!("Result: {} (Correct)", actual_result);
                }
                None => {
                    panic!(
                        "Benchmark '{}' result was None, expected {:?}",
                        test_name, expected_result
                    );
                }
            }
        } else if let Some(actual_result) = result {
            println!("Result: {} (no expected value specified)", actual_result);
        } else {
            println!("Result: None (no expected value specified)");
        }
        println!("------------------------------------");
    }

    #[test]
    fn fibonacci_notscheme() {
        let fib_n = 20;
        let ns_fib_source = format!(
            r#"
            (fn fib (n)
                (if (<= n 1)
                    n
                    (+ (fib (- n 1)) (fib (- n 2)))
                )
            )
            (fib {})
        "#,
            fib_n
        );
        let expected_val = Value::Number(6765.0);
        run_notscheme_benchmark(
            &format!("Recursive Fibonacci({})", fib_n),
            &ns_fib_source,
            "fib_module",
            Some(expected_val),
        );
    }

    #[test]
    fn recursive_sum_notscheme() {
        let sum_n = 100;
        let ns_sum_source = format!(
            r#"
            (fn sum_recursive (n)
                (if (= n 0)
                    0
                    (+ n (sum_recursive (- n 1)))
                )
            )
            (sum_recursive {})
        "#,
            sum_n
        );
        let expected_val = Value::Number(5050.0);
        run_notscheme_benchmark(
            &format!("Recursive Sum({})", sum_n),
            &ns_sum_source,
            "sum_rec_module",
            Some(expected_val),
        );
    }

    #[test]
    fn list_creation_and_iteration_sum_notscheme() {
        let list_size = 50;
        let mut list_content_str = String::new();
        let mut expected_sum = 0.0;
        for i in 1..=list_size {
            list_content_str.push_str(&format!(" {}", i));
            expected_sum += i as f64;
        }

        // Corrected: sum_list_iter call must be within the scope of my_list
        let ns_list_sum_source = format!(
            r#"
            (fn sum_list_iter (lst current_sum)
                (if (is_none lst)
                    current_sum
                    (sum_list_iter (rest lst) (+ current_sum (first lst)))
                )
            )
            (let my_list (list{}) 
                (sum_list_iter my_list 0) 
            ) 
        "#,
            list_content_str
        );

        let expected_val = Value::Number(expected_sum);
        run_notscheme_benchmark(
            &format!("List Creation & Sum ({})", list_size),
            &ns_list_sum_source,
            "list_sum_module_scoped",
            Some(expected_val),
        );
    }

    #[test]
    fn struct_creation_notscheme() {
        let num_structs = 100;
        let ns_struct_source = format!(
            r#"
            (struct MyStruct (a b))
            (fn create_structs_loop (n count) 
                (if (<= count 0)
                    none 
                    (begin
                        (MyStruct count (+ count 10)) 
                        (create_structs_loop n (- count 1)) 
                    )
                )
            )
            (begin 
                (create_structs_loop {} {}) 
                (MyStruct 1 2) 
            )
        "#,
            num_structs, num_structs
        );

        let mut expected_fields_map = HashMap::new();
        expected_fields_map.insert(Rc::new("a".to_string()), Value::Number(1.0));
        expected_fields_map.insert(Rc::new("b".to_string()), Value::Number(2.0));

        let expected_val = Value::StructInstance(Rc::new(RefCell::new(crate::value::StructData {
            type_name: Rc::new("MyStruct".to_string()),
            fields: expected_fields_map,
        })));

        run_notscheme_benchmark(
            &format!("Struct Creation Loop ({})", num_structs),
            &ns_struct_source,
            "struct_creation_module",
            Some(expected_val),
        );
    }
}
