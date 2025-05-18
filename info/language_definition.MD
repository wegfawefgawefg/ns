# Our Language NotScheme: Syntax, Examples & Ecosystem Goals (v20 - lowercase `none`)
# File Extension: .ns

## I. Syntax Overview (S-expressions)

Our language will use S-expressions. All user-defined identifiers (variables, functions, struct names, field names, module names) should use underscore_case (e.g., `my_variable`, `calculate_sum`).

1. **Basic Structure:** (Unchanged)
2. **Atoms:**
    * **Numbers:** Integers and Floats.
    * **Strings:** Enclosed in double quotes.
    * **Symbols:** Used for variable names, function names, keywords, etc. Follow underscore_case.
    * **Booleans:** `true` and `false`.
    * **None Value/Empty List:** `none` (replaces `nil`; represents an empty list and a general null/void value).

3. **Comments:**
    * Single-line comments start with `//`.

4. **Data Structures (Literals & Constructors):**
    * **Lists:**
        * **Quoted Lists (Literal Data):** `'(<item1> <item2> ...)` creates a literal list.
            * Example: `'(1 "hello" true none)` (Updated)
        * **List Constructor (Core Primitive):** `(list <item1-expr> <item2-expr> ...)` creates a list by evaluating each item expression.
            * Example: `(list (+ 1 2) "world")` results in `(3 "world")`
    * **Structs:** (Defined by `struct` special form)

5. **Special Forms (Keywords with specific evaluation rules):**

    * **`static`**: Defines module-scoped variables: `(static <symbol> <expression>)`
    * **`fn`**: Defines named functions: `(fn <symbol> (<params...>) <body...>)`
    * **`struct`**: Defines a new struct type: `(struct <StructName> (<fields...>))`
        * Implicitly defines a constructor function named `<StructName>`.
        * Structs are mutable.
    * **`get`**: Accesses a struct field: `(get <instance> <field_name>)`
    * **`set`**: Modifies a struct field: `(set <instance> <field_name> <value>)`
    * **`lambda`**: Creates anonymous functions: `(lambda (<params...>) <body...>)`
    * **`let`**: Defines local variables.
        * Single: `(let <symbol> <expression>)`
        * Multiple sequential: `(let ((<s1> <e1>) (<s2> <e2>)...) <body...>)`
    * **`if`**: Conditional: `(if <condition> <then_expr> <else_expr>)`
    * **`begin`**: Sequences expressions: `(begin <expr1> ... <exprN>)`
    * **`while`**: Looping: `(while <condition> <body...>)` (evaluates to `none`)
    * **`use`**: Imports from modules: `(use <module_name> (<items...>))` or `(use <module_name> *)`

6. **Function Calls & Struct Instantiation:**
    * General: `(<function_expr> <args...>)`
    * Struct: `(<StructName> <field_values...>)`

## II. Modules and Imports

1. **File-based Modules:** Each `.ns` file is a module. Filename (underscore_case, no extension) is its name.
2. **Public by Default:** All top-level definitions (`static`, `fn`, `struct` type and its constructor) are public.
3. **Imports with `use`:** Imports specified items or all (`*`) items from another module.

## III. Core Primitives & Standard Library Philosophy

NotScheme aims for a minimal core language and Virtual Machine. A small set of essential primitive operations are provided directly by the VM or are intrinsically known to the compiler. More extensive "standard library" functionality is expected to be built as NotScheme libraries using these core primitives and the module system.

**A. Core Built-in Functions/Operations:**

* **Arithmetic:** `+`, `-`, `*`, `/`, `%`
* **Comparison:** `=`, `>`, `<`, `>=`, `<=`, `!=`
* **Logical:** `not`
* **Type Predicates:** `is_none` (replaces `is_nil`), `is_boolean`, `is_number`, `is_string`, `is_list`, `is_struct`, `is_function`.
* **List Primitives:** `cons`, `first`, `rest`, `list`.
* **Basic I/O:** `print`.
* **Error Handling:** `(error <message_string>)`.

**B. Examples of Functionality for User-Level Libraries:**
(e.g., `length`, `append`, `map`, `filter` in `list_utils.ns`; string utilities in `string_utils.ns`)

## IV. Example Code Snippets

(Examples should be updated to use `none` instead of `nil`, and `is_none` instead of `is_nil`)

**1. Basic Definitions, Arithmetic, and Control Flow:**

```ns
// main_basics.ns
(static pi 3.14)
(static message "Hello, NotScheme!")

(fn circle_area (radius)
  (* pi radius radius))

(fn greet (name)
  (print message)
  (print "Greetings, " name "!"))

(let result (circle_area 10)) 
(print "Area of circle with radius 10:" result)

(greet "Developer")

(if (> result 300)
    (print "That's a big circle!")
    (print "That's a normal circle."))
2. Recursive Function (Factorial):// factorial_example.ns
(fn factorial (n)
  (if (<= n 1)
      1 
      (* n (factorial (- n 1))))) 

(print "Factorial of 5 is:" (factorial 5)) 
3. Closures (Function Returning a Function):// closure_example.ns
(fn make_adder (add_value)
  (lambda (num_to_add)
    (+ num_to_add add_value)))

(let add5 (make_adder 5))
(print "add5 applied to 10:" (add5 10)) 

(print "Direct call ((make_adder 3) 7):" ((make_adder 3) 7)) 
4. Structs (Definition, Instantiation, Get/Set):// struct_example.ns
(struct Point (x_coord y_coord))

(let p1 (Point 10 20))
(print "Initial p1.x:" (get p1 x_coord)) 

(set p1 y_coord (+ (get p1 y_coord) 5)) 
(print "Modified p1.y:" (get p1 y_coord)) 

(fn describe_point (p)
  (print "Point is (" (get p x_coord) "," (get p y_coord) ")"))

(describe_point p1)
5. Lists and Looping (using while and core list primitives):// list_loop_example.ns
(let my_list (list 10 20 30 40 50))
(let sum 0) 
(let current_list my_list)

// This loop example assumes 'sum' and 'current_list' can be mutated.
// If 'let' creates immutable bindings, this would need to be structured
// using recursion or by having 'sum' and 'current_list' be fields of a mutable struct.
(while (not (is_none current_list)) // Changed from is_nil
  // (set sum (+ sum (first current_list))) // Placeholder: 'set' for locals not yet defined
  // (set current_list (rest current_list))) // Placeholder
  (print "Loop item (if set worked):" (first current_list)) // Temp print
  (set current_list (rest current_list)) // This 'set' would need to work on locals
)

(print "Sum of my_list (conceptual):" sum) 

(let new_list (cons 0 (cons 1 (cons 2 none)))) // Changed from nil
(print "Constructed list:" new_list) 
```

6. Modules (use statement): (Examples remain structurally similar, just use none if applicable)V. Tooling and Ecosystem GoalsCompiler & Build Tool (nsc & nscargo):A standalone command-line compiler, envisioned as nsc (NotScheme Compiler).A build system and package manager, conceptually nscargo.Implementation Language of Core Tools:Long-term intention to potentially rewrite core components in a systems language (Rust, C++, or C).FFI (Foreign Function Interface): This choice would influence FFI strategies.Syntax Highlighting: For popular code editors.Linting and Formatting Tools.Language Server Protocol (LSP) Support.Debugging Tools.Line-Aware and Contextual Error Reporting.VI. Future Language Design ConsiderationsOption and Result Types:Explicit types for handling optionality (Option<T>: Some(value) or NoneType (if none is the value for Option::None)) and results (Result<T, E>).Pattern Matching.Static Typing (Optional).Advanced Macro System.This version (v20) updates boolean literals, changes nil to none (and is_nil to is_none), names the compiler nsc and package manager nscargo, and discusses the intention for a systems language rewrite of core tools, FFI, and future consideration of Option/Result types. Examples have been updated for none.