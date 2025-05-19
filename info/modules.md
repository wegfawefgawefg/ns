# NotScheme Module System Specification (v2 Consolidated)

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

The syntax for module-related forms can be described as follows (EBNF-ish, S-expression flavored):

```text
ModulePath   ::= <symbol>                       // e.g., my_module
               | (<symbol> <symbol> ...)        // e.g., (my_package core utils)
                                                // Represents a path like my_package/core/utils.ns

ImportSpec   ::= * // Imports all public symbols
               | (<ImportItem>...)                // Imports specified items

ImportItem   ::= <symbol>                       // e.g., my_function
               | (as <original_symbol> <alias_symbol>) // e.g., (as old_name new_name)

use_form     ::= (use <ModulePath> <ImportSpec>)
use_here_form::= (use_here <ModulePath> <symbol>)
```

## 3. Semantics

### 3.1. `ModulePath` Resolution to File

The compiler resolves a `<ModulePath>` to a physical `.ns` file according to these rules:

* A simple `<symbol>` (e.g., `my_module`) maps to `my_module.ns`.
* A path represented as a list of symbols `(<s1> <s2> ... <sN>)` (e.g., `(my_package core utils)`) maps to a file path by joining the symbols with directory separators, with the last symbol having the `.ns` extension. For example, `(my_package core utils)` would resolve to `my_package/core/utils.ns`. The resolution starts from a base search directory (see below).

The search for module files (or the base of a module path) follows a specific order:
1.  **Relative to the importing file:** The directory containing the current `.ns` file.
2.  **`NS_PATH` Environment Variable:** A colon-separated list of directories specified by the `NS_PATH` environment variable.
3.  **Default User Registry:** A standard location, e.g., `$HOME/.nscargo/registry/src/*`.

The first matching file found (for single symbol paths) or the first valid base directory (for multi-symbol paths) in this search order is used. If no file/path can be resolved, a compile-time error occurs.

### 3.2. Compilation Rules & Flow

1.  **Module Compilation:** When the compiler encounters a `(use ...)` or `(use_here ...)` form, it first ensures that the target module (the file pointed to by `<ModulePath>`) is compiled. Each module is compiled only once, even if imported multiple times. This process yields a unique **ModuleId** and its corresponding bytecode segment.
2.  **Selective Import with `(use ...)`:**
    * For each `<ImportItem>` in the `<ImportSpec>`:
        * The `local_name` is determined (either the `<symbol>` itself or the `<alias_symbol>` if `as` is used).
        * A mapping from this `local_name` to its origin `(ModuleId, original_symbol_index_within_module)` is recorded in the current lexical scope where the `(use ...)` form appears.
3.  **Glob Import `(use ... *)`:**
    * All public symbols from the target module are imported into the current lexical scope.
    * If a glob import introduces a name that conflicts with an already existing name in the scope (from a previous import or local definition), a compile-time error occurs.
4.  **One-Shot Access with `(use_here ...)`:**
    * The `(use_here <ModulePath> <symbol>)` form allows direct access to `<symbol>` from `<ModulePath>` without binding it to a name in the current scope.
    * The compiler emits bytecode to directly fetch and, if it's a function being called, invoke the resolved symbol. This is useful for disambiguation or for using a function without polluting the local namespace.

### 3.3. Module Scope, Visibility, and Shadowing (Rust-inspired)

#### 3.3.1. Module Scope and Default Visibility
* Each `.ns` file is a distinct module. A path like `(my_lib sub)` refers to `my_lib/sub.ns`.
* **Public by Default:** All top-level definitions within a module (`static`, `fn`, `struct` types, and their constructors) are considered public and are thus exportable by default.

#### 3.3.2. Lexical Lookup Order for Symbols
When resolving a symbol, the compiler searches in the following order:
1.  **Local Bindings:** `let`-bound variables or function parameters within the innermost current lexical scope.
2.  **Current File's Top-Level Definitions:** `static`s, `fn`s, and `struct`s defined at the top level of the current `.ns` file.
3.  **Imported Names:** Names brought into scope by `(use ...)` forms, searching from the innermost scope containing a `use` declaration outwards.
4.  **Error:** If the symbol remains unresolved after these steps, a compile-time error ("unresolved symbol") is issued.

#### 3.3.3. Shadowing
* A local binding (e.g., `let`, function parameter, or a top-level definition in the current file) shadows an imported symbol of the same name. This will produce a **warning** at compile-time.
* Imports in an inner scope can shadow imports from an outer scope.

**Corrected Shadowing Logic based on Lexical Lookup Order (Section 3.3.2):**
```notscheme
// math_constants.ns
(static pi 3.14159)
(static x_global 30)

// main.ns
(static x_global 100)

(use math_constants (pi (as x_global math_x_global)))

(fn demo_shadowing ()
  (begin
    (print "File-level static x_global:" x_global) // 100
    (print "Imported pi:" pi) // 3.14159
    (print "Imported and aliased math_x_global:" math_x_global) // 30

    (let x_global 42)
    (print "Local let x_global:" x_global) // 42

    (use (utils collections) ((as x_global util_x))) // Assuming utils/collections.ns
    // (print x_global) // Still 42
    (print "Util_x from inner import:" util_x)
  )
)
(demo_shadowing)
```

#### 3.3.4. Rationale for Model (Rust-inspired)
* Predictable Shadowing.
* Encapsulation via facade modules.
* Synergy with package management.

### 3.4. Mutual Recursion and Cycles
* Mutual recursion between functions and struct definitions across modules is allowed (via stub emission).
* Cycles in `static` initializer evaluation are a compile-time error (unless thunked).

### 3.5. Re-exports and Facade Modules
A `(use ...)` form at the top level of a module file automatically re-exports the imported symbols using their local names (which could be aliases). This allows for creating facade modules.

**Example:**
```notscheme
// crypto/hashing_impl.ns
(fn internal_sha256_func (data) (comment "implementation details for sha256"))
(static internal_version_code "v1.0.2-alpha")

// crypto/mod.ns (Facade for the crypto library)
// Import 'internal_sha256_func' and alias it to 'sha256'.
// Import 'internal_version_code' and alias it to 'version'.
// These aliased names are then re-exported from crypto/mod.ns.
(use (crypto hashing_impl) (
    (as internal_sha256_func sha256)
    (as internal_version_code version)
))
// Other imports for the facade, e.g.:
// (use (crypto symmetric) (aes_encrypt (as aes_decrypt_raw raw_aes_dec)))


// user_app.ns
(use crypto (sha256 version)) // User imports 'sha256' and 'version' from the facade 'crypto/mod.ns'
(print "Crypto library version:" version)
(let my_data "my_secret_data")
(let hash_val (sha256 my_data))
(print "Hash of" my_data ":" hash_val)
```
Clients can now use `(use crypto (sha256 version))` without needing to know about the internal structure like `crypto/hashing_impl.ns` or the original names `internal_sha256_func` and `internal_version_code`.

## 4. Name Resolution Algorithm (Conceptual)

```
resolve_symbol(symbol_name, current_scope_stack):
  // 1. Local bindings
  for frame in current_scope_stack (inner→outer):
    if symbol_name in frame.local_bindings: return frame.local_bindings[symbol_name]
  // 2. Current file's top-level
  if symbol_name in current_file.toplevel_definitions: return current_file.toplevel_definitions[symbol_name]
  // 3. Imports
  for frame in current_scope_stack (inner→outer):
    if frame_has_imports and symbol_name in frame.imported_symbols: return frame.imported_symbols[symbol_name]
  error "Unresolved symbol: " + symbol_name
```

## 5. Top-Level Evaluation Order

1.  **Parsing:** Parse all reachable modules.
2.  **Dependency Graph Construction:** Build module dependency graph.
3.  **Symbol Stub Emission:** Emit stubs for all top-level symbols.
4.  **Code Generation:** Generate bytecode for function bodies, etc.
5.  **Static Initializer Execution:** Execute `static` initializers in post-order of the dependency graph.

## 6. Examples of Module Usage

### 6.1. Simple Import
```notscheme
// math_utils.ns
(static pi 3.14159)
(fn square (x) (* x x))

// main.ns
(use math_utils (pi square))
(print "Pi:" pi)
(print "Square of 5:" (square 5))
```

### 6.2. Deep Path Import
```notscheme
// company/networking/protocols.ns
(fn http_get (url) (comment "impl for http get"))

// main.ns
(use (company networking protocols) (http_get))
(http_get "[http://example.com](http://example.com)")
```

### 6.3. Alias Import using `as`
```notscheme
// string_lib.ns
(fn concatenate (s1 s2) (comment "joins s1 and s2"))

// main.ns
(use string_lib ((as concatenate concat_strings)))
(print (concat_strings "Hello, " "World!"))
```

### 6.4. Glob Import (`*`)
```notscheme
// common_ops.ns
(static default_timeout 1000)
(fn process_item (item) (comment "processes item"))

// main.ns
(use common_ops *)
(print "Timeout:" default_timeout)
```

### 6.5. Local-Scope `use`
```notscheme
// crypto/hashing.ns
(fn sha256 (data) (comment "SHA256 impl"))

// main.ns
(fn process_sensitive_data (data)
  (begin
    (use (crypto hashing) (sha256)) // sha256 only visible here
    (let hashed_data (sha256 data))
    (print "Hashed:" hashed_data)
  )
)
// (sha256 "test") // Error here
```

### 6.6. One-Shot Access with `use_here`
```notscheme
// crypto/hashing.ns
(fn md5 (data) (comment "MD5 impl"))

// main.ns
(let user_password "password123")
(let stored_hash ((use_here (crypto hashing) md5) user_password))
(print "MD5 Hash:" stored_hash)
```

## 7. Implementation Notes (for Compiler/VM)

| Concern             | Design Suggestion                                                                 |
| ------------------- | --------------------------------------------------------------------------------- |
| **ModuleId** | `u32` index into `Vec<BytecodeSegment>`.                                          |
| **Import Stubs** | Indirection table per segment or link-time resolution.                            |
| **Closure Encoding**| `(segment_id, offset)`; VM jumps seamlessly.                                      |
| **Compilation Cache**| SHA-256 of source → `.nscache/<hash>.bc`. `nscargo` manages.                     |

## 8. Comparison: Avoiding Rust's Module System Pain Points

| Rust Quirk/Feature               | NotScheme Approach                                                              |
| -------------------------------- | ------------------------------------------------------------------------------- |
| `mod my_module;` vs. file system | One `.ns` file is one module. Filename defines module name. Path lists for hierarchy. |
| `super::` and path verbosity     | Module paths `(<symbol> ...)` are relative to search paths or current dir.      |
| Private-by-default visibility    | Public-by-default.                                                              |
| `pub use` for re-exporting       | Top-level `(use ...)` re-exports imported (and potentially aliased) names.      |
| Macro path resolution issues     | Macros (future) should use same import mechanism.                               |
| Crate root and edition churn     | Simpler, stable path rules.                                                     |

## 9. Future Extensions (Not Goals for v1)

* Package Version Resolution via `nscargo`.
* Visibility Modifiers (e.g., `(private ...)`).
* Macro-Friendly Helpers to auto-generate path lists or import items.
* Scoped Glob Imports: `(use my_module (*) :prefix "my_mod_")`.
* Conditional Imports.
* Dynamic Loading: `(require <module_name_string>)`.
* Advanced Circular Dependency Analysis.

This consolidated specification aims to provide a clear and robust foundation for NotScheme's module system.
