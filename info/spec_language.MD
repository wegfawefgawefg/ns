# Our Language NotScheme: Syntax, Examples & Ecosystem Goals (v21 - Module Import Refinements)
# File Extension: .ns

## I. Syntax Overview (S-expressions)

Our language will use S-expressions. All user-defined identifiers (variables, functions, struct names, field names, module names) should use underscore_case (e.g., `my_variable`, `calculate_sum`).

1.  **Basic Structure:** (Unchanged)
2.  **Atoms:**
    * **Numbers:** Integers and Floats.
    * **Strings:** Enclosed in double quotes.
    * **Symbols:** Used for variable names, function names, keywords, etc. Follow underscore_case.
    * **Booleans:** `true` and `false`.
    * **None Value/Empty List:** `none` (replaces `nil`; represents an empty list and a general null/void value).

3.  **Comments:**
    * Single-line comments start with `//`.

4.  **Data Structures (Literals & Constructors):**
    * **Lists:**
        * **Quoted Lists (Literal Data):** `'(<item1> <item2> ...)` creates a literal list.
            * Example: `'(1 "hello" true none)`
        * **List Constructor (Core Primitive):** `(list <item1-expr> <item2-expr> ...)` creates a list by evaluating each item expression.
            * Example: `(list (+ 1 2) "world")` results in `(3 "world")`
    * **Structs:** (Defined by `struct` special form)

5.  **Special Forms (Keywords with specific evaluation rules):**

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
    * **`use`**: Imports symbols from other modules.
        * Syntax: `(use <ModulePath> <ImportSpecification>)`
        * `<ModulePath>`: Specifies the module to import from. It can be:
            * A single `<symbol>` (e.g., `my_module`). This refers to a file like `my_module.ns`.
            * A list of symbols representing a path (e.g., `(my_package core_utils)`). This refers to a path like `my_package/core_utils.ns`.
        * `<ImportSpecification>`: Defines which symbols to import. It can be:
            * `*`: (An atom) Imports all public symbols from the module.
            * A `<symbol>`: (An atom) Imports a single symbol by its name (e.g., `my_function`). This is the form for importing one non-aliased symbol.
            * `(as <original_symbol> <new_alias_symbol>)`: (A list) Imports a single symbol and gives it an alias (e.g., `(as internal_fn public_fn)`). This is the form for importing one aliased symbol.
            * `()`: (An empty list) Imports nothing explicitly from the module.
            * A list of **two or more** import items: `(<item_1> <item_2> ...)` where each item is either a `<symbol>` or an `(as <original_symbol> <new_alias_symbol>)` form. This is used for importing multiple symbols.
                * Example: `(my_func (as another_func an_alias) yet_another_func)`
        * *Note:* For importing a single, non-aliased symbol, the direct symbol form is used (e.g., `my_function`), not a list containing a single symbol (e.g., `(my_function)` is not the standard form for this case). Similarly for single aliased imports. Refer to the Module System Specification for exhaustive details.

6.  **Function Calls & Struct Instantiation:**
    * General: `(<function_expr> <args...>)`
    * Struct: `(<StructName> <field_values...>)`

## II. Modules and Imports

1.  **File-based Modules:** Each `.ns` file is a module. Its name is the filename (underscore_case, without the `.ns` extension). Path-like module references (e.g., `(my_lib collections)`) map to directory structures (e.g., `my_lib/collections.ns`).
2.  **Public by Default:** All top-level definitions (`static`, `fn`, `struct` type and its constructor) are public by default and thus exportable.
3.  **Imports with `use`:** The `(use <ModulePath> <ImportSpecification>)` special form allows importing symbols from other modules. `<ModulePath>` identifies the target module. `<ImportSpecification>` determines what is imported, supporting glob imports (`*`), selective import of single symbols (by name or with an alias, typically unlisted), an empty list `()` for no explicit imports, or lists of two or more symbols/aliases. (Refer to the Module System Specification for detailed syntax, semantics, and resolution rules.)

## III. Core Primitives & Standard Library Philosophy

NotScheme aims for a minimal core language and Virtual Machine. A small set of essential primitive operations are provided directly by the VM or are intrinsically known to the compiler. More extensive "standard library" functionality is expected to be built as NotScheme libraries using these core primitives and the module system.

**A. Core Built-in Functions/Operations:**

* **Arithmetic:** `+`, `-`, `*`, `/`, `%`
* **Comparison:** `=`, `>`, `<`, `>=`, `<=`, `!=`
* **Logical:** `not`
* **Type Predicates:** `is_none`, `is_boolean`, `is_number`, `is_string`, `is_list`, `is_struct`, `is_function`.
* **List Primitives:** `cons`, `first`, `rest`, `list`.
* **Basic I/O:** `print`.
* **Error Handling:** `(error <message_string>)`.

**B. Examples of Functionality for User-Level Libraries:**
(e.g., `length`, `append`, `map`, `filter` in `list_utils.ns`; string utilities in `string_utils.ns`)

## IV. Example Code Snippets

(Examples remain structurally similar, using `none` and `is_none`. Module import examples within user code would now reflect the new `use` syntax for single vs. multiple items.)

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
```

**2. Recursive Function (Factorial):**
```ns
// factorial_example.ns
(fn factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(print "Factorial of 5 is:" (factorial 5))
```

**3. Closures (Function Returning a Function):**
```ns
// closure_example.ns
(fn make_adder (add_value)
  (lambda (num_to_add)
    (+ num_to_add add_value)))

(let add5 (make_adder 5))
(print "add5 applied to 10:" (add5 10))

(print "Direct call ((make_adder 3) 7):" ((make_adder 3) 7))
```

**4. Structs (Definition, Instantiation, Get/Set):**
```ns
// struct_example.ns
(struct Point (x_coord y_coord))

(let p1 (Point 10 20))
(print "Initial p1.x:" (get p1 x_coord))

(set p1 y_coord (+ (get p1 y_coord) 5))
(print "Modified p1.y:" (get p1 y_coord))

(fn describe_point (p)
  (print "Point is (" (get p x_coord) "," (get p y_coord) ")"))

(describe_point p1)
```

**5. Lists and Looping (using while and core list primitives):**
```ns
// list_loop_example.ns
(let my_list (list 10 20 30 40 50))
(let sum 0)
(let current_list my_list)

(while (not (is_none current_list))
  // Assuming 'sum' and 'current_list' can be mutated for this example.
  // (set sum (+ sum (first current_list))) // Placeholder for direct local mutation if supported
  // (set current_list (rest current_list))) // Placeholder
  (print "Loop item:" (first current_list)) // Temp print
  (set current_list (rest current_list)) // This would need 'set' to work on locals for this exact style
)
(print "Sum of my_list (conceptual, if mutation worked):" sum)

(let new_list (cons 0 (cons 1 (cons 2 none))))
(print "Constructed list:" new_list)
```

**6. Modules (use statement examples):**
```ns
// Assume: my_math.ns contains (static pi 3.14) (fn area (r) (* pi r r))
// Assume: string_ops.ns contains (fn join (s1 s2) ...) (fn len (s) ...)

// Import a single item
(use my_math pi)
(print "PI from my_math:" pi)

// Import a single aliased item
(use my_math (as area circle_area))
(print "Area calculated by my_math:" (circle_area 10))

// Import multiple items (two or more)
(use string_ops (join len))
(print "Length of 'hello':" (len "hello"))

// Import all items
(use my_math *)
(print "PI (via glob):" pi)
(print "Area (via glob):" (area 5))

// Import from a nested module path
// Assume: company/analytics/core.ns contains (fn process_data (d) ...)
(use (company analytics core) process_data)
(process_data some_data_variable)

// Import nothing explicitly (perhaps for module initialization side effects if any)
(use some_initializer_module ())
```

## V. Tooling and Ecosystem Goals

* **Compiler & Build Tool (`nsc` & `nscargo`):** Standalone compiler and a build system/package manager.
* **Implementation Language of Core Tools:** Potential future rewrite in a systems language (Rust, C++, C) for performance and FFI.
* **FFI (Foreign Function Interface):** Design influenced by core tool language choice.
* **Syntax Highlighting, Linting, Formatting, LSP, Debugging Tools.**
* **Line-Aware and Contextual Error Reporting.**

## VI. Future Language Design Considerations

* **Option and Result Types:** For explicit optionality and error handling.
* **Pattern Matching.**
* **Static Typing (Optional).**
* **Advanced Macro System.**
