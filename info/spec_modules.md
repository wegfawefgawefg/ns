# NotScheme Module System Specification (v3 - Normalized Import Syntax)

> *Pure S-expression import system with no extra punctuation.*

## 1. Overview

The NotScheme module system facilitates code organization and reuse by allowing symbols defined in one file (module) to be used in another. It introduces two primary compile-time special forms:

| Form                               | Purpose                                                                      |
| ---------------------------------- | ---------------------------------------------------------------------------- |
| `(use <ModulePath> <ImportSpec>)`  | Brings specified symbols from another module into the current lexical scope. |
| `(use_here <ModulePath> <symbol>)` | Provides one-off, unboud access to a symbol from another module.             |

All sub-forms involved in module operations, such as `as`, are themselves S-expressions. This design maintains the syntactic uniformity of NotScheme. The symbols `use`, `use_here`, and `as` are not reserved keywords; they are ordinary symbols recognized by the compiler in operator position within these specific forms.

Each `.ns` file constitutes a module. The module's name is derived from its filename (using underscore_case, without the `.ns` extension).

## 2. Grammar

The syntax for module-related forms is as follows:

```text
ModulePath   ::= <symbol>                       // e.g., my_module
               | (<symbol> <symbol> ...)        // e.g., (my_package core utils)
                                                // Represents a path like my_package/core/utils.ns

ImportItem   ::= <symbol>
               | (as <original_symbol> <alias_symbol>)

ImportSpec   ::= * // Glob import: (use my_module *)
               | <symbol>                                // Single symbol import: (use my_module my_func)
               | (as <original_symbol> <alias_symbol>)   // Single symbol import with alias: (use my_module (as internal public_name))
               | ()                                      // Import nothing (empty list): (use my_module ())
               | (<ImportItem> <ImportItem> <ImportItem>*) // List of TWO OR MORE ImportItems.
                                                           // e.g., (use my_module (item1 item2))
                                                           // e.g., (use my_module (item1 (as item2 alias)))

use_form     ::= (use <ModulePath> <ImportSpec>)
use_here_form::= (use_here <ModulePath> <symbol>) // `use_here` always imports one symbol, unaliased.
```

**Important Note on `ImportSpec` ambiguity:**
The grammar is structured to avoid redundancy for single item imports:
* To import a single symbol `foo`: use `foo`, not `(foo)`.
* To import a single symbol `foo` as `bar`: use `(as foo bar)`, not `((as foo bar))`.
A list form for `ImportSpec` implies either zero items `()`, or two or more `ImportItem`s.

## 3. Semantics

### 3.1. `ModulePath` Resolution to File

* A simple `<symbol>` (e.g., `my_module`) maps to `my_module.ns`.
* A path `(<s1> <s2> ... <sN>)` (e.g., `(my_package core utils)`) maps to `my_package/core/utils.ns`.
* Search order:
    1.  Relative to the importing file.
    2.  `NS_PATH` Environment Variable.
    3.  Default User Registry (e.g., `$HOME/.nscargo/registry/src/*`).
* First hit wins; error if not found.

### 3.2. Compilation Rules & Flow for `use`

When the compiler encounters `(use <ModulePath> <ImportSpecification>)`:
1.  **Module Compilation:** The target module specified by `<ModulePath>` is compiled once, yielding a `ModuleId`.
2.  **Processing `<ImportSpecification>`:**
    * If `*`: All public symbols from the target module are made available in the current scope. Name clashes with existing local definitions (not other imports) might issue a warning or error depending on exact shadowing rules for glob imports. Glob imports do not support aliasing.
    * If a `<symbol>` (e.g., `my_func`): The specified public symbol `my_func` from the target module is made available in the current scope under its original name.
    * If `(as <original> <alias>)` (e.g., `(as internal_func public_func)`): The public symbol `original_func` from the target module is made available in the current scope under the name `public_func`.
    * If `()` (an empty list): No symbols are explicitly imported by this `use` statement. This can be used if a module is needed for its compilation side-effects or to re-export nothing from a facade that conditionally imports.
    * If a list `(<item1> <item2> ...)` containing **two or more** `ImportItem`s: Each `<itemN>` is processed:
        * If `<itemN>` is a `<symbol>`: The symbol is imported under its original name.
        * If `<itemN>` is `(as <original> <alias>)`: The `original` symbol is imported under the `alias` name.
    * **Error Cases for `ImportSpecification`:** Forms that are syntactically lists but do not conform to the above (e.g., a list containing a single non-aliased symbol `(my_func)`, or a list containing a single aliased symbol wrapped in an extra list `((as foo bar))`) are compile-time errors. The direct, unlisted form should be used for single items.

3.  **Symbol Mapping:** For each successfully specified import, a mapping from the local name (original or alias) to `(ModuleId, original_symbol_index)` is recorded in the current lexical scope.

### 3.3. Module Scope, Visibility, and Shadowing

(Content largely unchanged from v2: Public by default, Lexical Lookup Order, Shadowing warnings. Assumes local definitions shadow imports.)

#### 3.3.1. Module Scope and Default Visibility
* Each `.ns` file is a distinct module.
* **Public by Default:** All top-level definitions are public.

#### 3.3.2. Lexical Lookup Order for Symbols
1.  Local Bindings (`let`, params).
2.  Current File's Top-Level Definitions.
3.  Imported Names (from `use` in current or outer scopes).
4.  Error if unresolved.

#### 3.3.3. Shadowing
* Local bindings or current file top-level definitions shadow imported names, producing a **warning**.
* Inner scope imports shadow outer scope imports.

### 3.4. Mutual Recursion and Cycles

(Unchanged: Allowed for functions/structs via stubs; `static` init cycles are errors.)

### 3.5. Re-exports and Facade Modules

A `(use ...)` form at the top level of a module file automatically re-exports the imported symbols using their established local names (original or aliased).

**Example:**
```ns
// lib_internals/string_utils_impl.ns
(fn make_greeting_str (name_str) (comment "complex logic to create greeting"))
(static default_separator_char "-")

// my_string_lib.ns (Facade Module)
(use (lib_internals string_utils_impl)
    (as make_greeting_str create_greeting) // Import single aliased item
    default_separator_char                 // Import single item (becomes a list of 2+ for the use form)
)
// After the above use, my_string_lib.ns now re-exports 'create_greeting' and 'default_separator_char'.

// main_app.ns
(use my_string_lib (create_greeting default_separator_char)) // Valid: list of 2 items
(print (create_greeting "User"))
(print "Separator:" default_separator_char)

// Alternative import in main_app.ns for single items from facade:
(use my_string_lib create_greeting)
(use my_string_lib default_separator_char)
```

## 4. Name Resolution Algorithm (Conceptual)

(Unchanged from v2)

## 5. Top-Level Evaluation Order

(Unchanged from v2)

## 6. Examples of Module Usage

### 6.1. Simple Single Item Imports
```ns
// math.ns: (static pi 3.14) (fn square (x) (* x x))

// main.ns
(use math pi)
(print "PI is:" pi)

(use math (as square sq_fn))
(print "Square of 5:" (sq_fn 5))
```

### 6.2. Multi-Item Import (Two or More)
```ns
// shapes.ns: (struct Circle (...)) (fn calc_triangle_area (...)) (static default_color "red")

// main.ns
(use shapes (Circle calc_triangle_area default_color)) // List of 3 items
(let c (Circle 10))
(print "Color:" default_color)
```

### 6.3. Path Imports
```ns
// company/networking/protocols.ns: (fn http_get (url) ...)

// main.ns
(use (company networking protocols) http_get) // Single item from path
(http_get "[http://example.com](http://example.com)")
```

### 6.4. Glob Import (`*`)
```ns
// common_ops.ns: (static version 1) (fn process (x) ...) (fn validate (y) ...)

// main.ns
(use common_ops *)
(print "Version:" version)
(process some_data)
```

### 6.5. Importing Nothing Explicitly
```ns
// config_loader.ns: (static config_loaded true) (print "Config module loaded!")
// (Assume loading this module has a side effect, like initializing some global state accessible differently)

// main.ns
(use config_loader ()) // Explicitly import no symbols, perhaps for side effects.
// (print config_loaded) // This would be an error unless config_loaded was re-exported or part of core state.
```

### 6.6. Invalid Single-Item List Forms (for illustration)
```ns
// Assume utils.ns contains (fn helper_func () ...)

// (use utils (helper_func))       // INVALID: Should be (use utils helper_func)
// (use utils ((as helper_func h))) // INVALID: Should be (use utils (as helper_func h))
```

## 7. Implementation Notes (for Compiler/VM)

(Unchanged from v2)

## 8. Comparison: Avoiding Rust's Module System Pain Points

(Unchanged from v2)

## 9. Future Extensions (Not Goals for v1)

(Unchanged from v2)

This consolidated specification aims to provide a clear and robust foundation for NotScheme's module system, emphasizing both expressive power and ergonomic syntax.
