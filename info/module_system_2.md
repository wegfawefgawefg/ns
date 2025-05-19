# NotScheme Module System (v1 Draft)

> *Pure S-expression import system with no extra punctuation.*

## 1 Overview

The module system introduces two compile-time special forms:

| Form                               | Purpose                                                          |
| ---------------------------------- | ---------------------------------------------------------------- |
| `(use <ModulePath> <import-spec>)` | Brings symbols from another file into the current lexical scope. |
| `(use_here <ModulePath> <symbol>)` | One-off access to a symbol without any binding.                  |

All sub-forms (`from`, `as`) are themselves S-expressions, preserving the language’s uniform syntax.

---

## 2 Grammar

```text
ModulePath   ::= symbol | (from ModulePath symbol)
ImportSpec   ::= * | (ImportItem…)
ImportItem   ::= symbol | (as symbol symbol)

use-form     ::= (use ModulePath ImportSpec)
use_here     ::= (use_here ModulePath symbol)
```

---

## 3 File Resolution

* **ModulePath → File**

  * `symbol` ⇒ `symbol.ns`
  * `(from p s)` → resolve `p`, then append `/s.ns`

* **Search order**

  1. Directory of the importing file
  2. Colon-separated `NS_PATH`
  3. `$HOME/.nscargo/registry/src/*`

* **First hit wins**; error if none found.

---

## 4 Compilation Flow

1. **Encounter** `(use)` / `(use_here)` → ensure target module is compiled once, obtain **ModuleId**.
2. **Selective import** → map `local_name → (ModuleId, symbol_index)` in the current lexical frame.
3. **Glob `*`** → import every public symbol; duplicate names error.
4. **`use_here`** → compile a direct reference, no binding.

---

## 5 Module Scope & Visibility (Rust-style)

### 5.1 Module-scope

* Each `.ns` file is a node; `(from …)` walks that tree.
* **Public-by-default**: everything at top level is exported.

### 5.2 Lexical Lookup Order

1. Local `let` / function parameters
2. Current file’s top-level definitions
3. Names introduced by `(use …)` / `(use_here …)` in innermost → outermost scopes
4. **Error** if still unresolved

### 5.3 Shadowing

* Local bindings shadow imported ones, with a **warning**.
* Example:

  ```clojure
  (static x 1)

  (fn show ()
    (begin
      (use math_utils (x))   ; math_utils::x shadows outer static
      (print x)              ; prints math constant
      (let x 99)
      (print x)))            ; prints 99 (local shadows import)
  ```

### 5.4 Why Follow Rust’s Model?

* Predictable shadowing rules
* Easy encapsulation via façade files
* Works smoothly with a dependency graph built by a package manager

---

## 6 Examples

```clojure
;; simple
(use math_utils (pi sin))

;; deep path
(use (from crypto hashing) (sha256))

;; alias
(use math_utils ((as sin sine)))

;; glob
(use (from json core) *)

;; local-scope use
(fn digest_and_print (msg)
  (begin
    (use (from crypto hashing) (sha256))
    (print (sha256 msg))))

;; one-shot
(let digest ((use_here (from crypto hashing) sha256) "hello"))
```

---

## 7 Name-Resolution Algorithm

```text
resolve(S, scope-stack):
  for frame in scope-stack (inner→outer):
    if S in frame.locals:     return frame.locals[S]
    if S in frame.imports:    return frame.imports[S]
  if S in file.toplevel:      return file.toplevel[S]
  error "unresolved symbol S"
```

Imports map is per-frame; nested `(use …)` can shadow outer imports.

---

## 8 Re-exports & Facade Modules

A top-level `use` automatically re-exports its symbols:

```clojure
;; crypto/mod.ns
(use (from crypto hashing) (sha256))
```

Clients can now do `(use crypto (sha256))` without touching `crypto/hashing.ns`.

---

## 9 Top-level Evaluation Order

1. Parse all reachable modules.
2. Build the dependency graph (`use` edges).
3. Emit **stubs** for every top-level symbol to break cycles.
4. Generate code bodies (which may call stubs).
5. Run static initialisers in post-order of the dependency graph.

   * Cycles among statics → compile-time error unless delayed via a lambda.

---

## 10 Implementation Notes (VM)

| Concern          | Design                                      |
| ---------------- | ------------------------------------------- |
| ModuleId         | `u32` index into `Vec<BytecodeSegment>`     |
| Closure encoding | `(segment_id, offset)`; VM jumps seamlessly |
| Import stubs     | Indirection table per segment               |
| Cache            | SHA-256 of source → `.nscache/<hash>.bc`    |

---

## 11 Rust Pain-Points We Avoid

| Rust quirk                       | NotScheme fix                                        |
| -------------------------------- | ---------------------------------------------------- |
| `mod` vs file confusion          | One file ⇒ one module; no extra declaration          |
| `super::` verbosity              | Absolute `from` paths; no relative qualifiers        |
| Private-by-default friction      | Public-by-default eliminates a whole class of errors |
| `pub use` boilerplate            | Top-level `use` already re-exports                   |
| Macro path issues                | Macros import exactly like functions                 |
| Crate root edition splits (Rust) | No editions; path rules never change                 |

---

## 12 Future Extensions (non-goals for v1)

* Package-version resolution via `nscargo`.
* Visibility modifiers (e.g. `(private …)`) for library authors.
* Macro-friendly helpers to auto-generate `from` chains.
* Scoped glob imports: `(use mod (*) :prefix "m_")` auto-prefixes names.
* Visibility sets: `(export (only foo bar))` or `(export (except internal_helper))`.
* Dynamic loader: allow `(require module)` at runtime for plugins.
