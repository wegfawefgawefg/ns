# NotScheme Module System (v1 Draft)
> *Pure S‑expression import system with no extra punctuation.*

## 1 Overview
The module system introduces two compile‑time special forms:

| Form | Purpose |
|------|---------|
| `(use <ModulePath> <import‑spec>)` | Brings symbols from another file into the current lexical scope. |
| `(use_here <ModulePath> <symbol>)` | One‑off access to a symbol without any binding. |

All sub‑forms (`from`, `as`) are themselves S‑expressions, preserving the language’s uniform syntax.

---

## 2 Grammar (EBNF‑ish, S‑expr flavoured)

```text
ModulePath   ::= symbol | (from ModulePath symbol)
ImportSpec   ::= * | (ImportItem…)
ImportItem   ::= symbol | (as symbol symbol)

use‑form     ::= (use ModulePath ImportSpec)
use_here     ::= (use_here ModulePath symbol)
```

*No reserved keywords:* `from`, `as`, `use`, `use_here` are ordinary symbols that the compiler recognises when
they appear in operator position.

---

## 3 Semantics

### 3.1 `ModulePath` → file resolution
* `symbol` ⇒ `symbol.ns`
* `(from p s)` ⇒ resolve `p`, then append `/s.ns`
* Search order:  
  1. Directory of the importing file  
  2. Colon‑separated `NS_PATH`  
  3. `$HOME/.nscargo/registry/src/*`
* First hit wins; error if none found.

### 3.2 Compilation rules
1. On encountering `use` or `use_here`, the compiler ensures the target file is compiled exactly once,
   producing a **ModuleId** and byte‑code segment.
2. **Selective import**  
   * For each `ImportItem`, compute `local_name` (`symbol` or alias).  
   * Record mapping `local_name → (ModuleId, symbol_index)` in the current lexical scope.
3. **Glob import `*`**  
   Import all public symbols; duplicates with existing names cause a compile error.
4. **`use_here`**  
   Emits byte‑code to fetch and (optionally) call the resolved symbol directly; no binding is added.

### 3.3 Visibility & shadowing
* All top‑level definitions in a module are public by default.
* A `let`/`static`/`fn` in the current scope shadows an imported name of the same identifier, producing
  a **warning** (not an error) to help catch accidents.

### 3.4 Mutual recursion / cycles
Allowed for functions and structs.  The compiler emits stub entries before body code so
cross‑module calls link correctly.  Cycles in the evaluation of `static` initialisers are a compile‑time error
unless wrapped in a thunk.

---

## 4 Examples

### 4.1 Simple import
```clojure
(use math_utils (pi sin))
(print "sin π =" (sin pi))
```

### 4.2 Deep import with `from`
```clojure
(use (from crypto hashing) (sha256))
(sha256 "abc")
```

### 4.3 Alias import
```clojure
(use math_utils ((as sin sine)))
(print (sine 0.5))
```

### 4.4 Glob import
```clojure
(use (from json core) *)
```

### 4.5 Local‑scope `use`
```clojure
(fn digest_and_print (msg)
  (begin
    (use (from crypto hashing) (sha256))
    (print (sha256 msg))))
```

### 4.6 One‑shot use with `use_here`
```clojure
(let digest ((use_here (from crypto hashing) sha256) "hello"))
```

---

## 5 Implementation notes for the VM

| Task | Detail |
|------|--------|
| **ModuleId**           | `u32` index into `Vec<BytecodeSegment>`                       |
| **Import stubs**       | Each `use` inserts an indirection table entry.               |
| **Closure encoding**   | `(segment_id, offset)`; VM jumps across segments seamlessly. |
| **Cache**              | SHA‑256 of source → `.nscache/<hash>.bc` for incremental compile. |

---

## 6 Future extensions (non‑goals for v1)

* Package‑version resolution via `nscargo`.
* Visibility modifiers (`private`) for library authors.
* Macro‑friendly helpers to auto‑generate `from` chains.

---
