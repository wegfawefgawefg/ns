# **nscargo.ns** – Package & Dependency Specification

> S‑expression alternative to *Cargo.toml* for the NotScheme ecosystem.

---

## 1 File Location

* Each package root **must** contain a file named **`nscargo.ns`**.
* Workspaces use **`workspace.ns`** at the repository root (see §6).

---

## 2 Top‑level Sections

```clojure
(nscargo
  (package …)
  (dependencies …)
  (dev-dependencies …)        ; optional
  (features …)                ; optional
  (profile …)                 ; optional build profiles
)
```

All section names are ordinary symbols; ordering doesn’t matter.

---

## 3 `package` Section

```clojure
(package
  (name "my_lib")
  (version "0.3.1")
  (edition "2025")            ; language edition
  (description "…")           ; optional
  (license "MIT OR Apache-2.0")
  (authors ("Alice" "Bob"))   ; list literal
  (homepage "https://…")
)
```

---

## 4 `dependencies` Section

```clojure
(dependencies
  (math_utils  "1.2")                       ; version from registry
  (json_core   (version "0.8" features (serde))) 
  (my_local    (path "../my_local_lib"))    ; relative path
  (cool_git    (git "https://gitea/foo.git" rev "abc123"))
)
```

### Key forms

| Form | Meaning |
|------|---------|
| `"1.2"` | SemVer constraint (caret `^` semantics). |
| `(version "…" …)` | Extended options. |
| `(path "…")` | Local path, bypasses registry. |
| `(git url …)` | Git dependency, optional `rev` or `tag`. |
| `(features (feat1 feat2))` | Explicit feature list. |

---

## 5 `features` Section

```clojure
(features
  (default (serde))
  (serde   ())
  (fast    (math_utils/simd))     ; namespaced dep feature
)
```

* Feature values are *sets* of other features or dep‑feature pairs (`crate/feat`).

---

## 6 Workspaces

Root **`workspace.ns`**

```clojure
(workspace
  (members ("core" "cli" "benchmarks"))
  (resolver "2")     ; mirrors Cargo
)
```

Each member crate has its own `nscargo.ns` (without [package] if it only needs dependencies).

---

## 7 Compiler / Toolchain Behaviour

1. `nscargo build` →  
   * Parse workspace or single `nscargo.ns`.  
   * Resolve dependency graph against local registry cache.  
   * For git & path deps, fetch / stat timestamps for rebuild.  
2. SemVer precedence identical to Cargo.  
3. Lockfile **`nscargo.lock`** auto‑generated (pure s‑expr same schema as deps with resolved versions).

---

## 8 Profiles (optional)

```clojure
(profile
  (release (opt-level 3 debug false))
  (dev     (opt-level 0 debug true))
)
```

Unrecognised keys are forwarded to the backend compiler.

---

## 9 Advantages over *Cargo.toml*

| TOML annoyance | nscargo.ns improvement |
|----------------|------------------------|
| Duplicate key types (arrays vs tables) | Single S‑expr list semantics |
| Quoting & escaping rules | Only double‑quoted strings, rest are atoms |
| Multiple files for workspace | One `workspace.ns` |
| Manual `features = ["x"]` arrays | Plain nested lists |

---

## 10 Future Extensions

* **Patch / replace** syntax for overriding registry crates.
* **[target]**-specific deps via `(target wasm32-unknown) (…)`.
* Scriptable build steps with `(build-script "build.ns")`.
