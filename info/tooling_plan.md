# NotScheme Tooling Roadmap (v1)

**Purpose:** A phase‑ordered blueprint for delivering first‑class editor tooling (VS Code focus) for the NotScheme language.

---

## Phase Matrix (high‑level)

| Phase | Focus                       | Key Deliverables                                                                                | Depends On |
| ----: | --------------------------- | ----------------------------------------------------------------------------------------------- | ---------- |
|     0 | Baseline editor support     | VS Code extension scaffold (`yo code`); `language‑configuration.json`; minimal TextMate grammar | –          |
|     1 | LSP core + diagnostics      | `nsc‑lsp` binary (Rust + tower‑lsp); span‑aware parse + lint; VS Code client wiring             | 0          |
|     2 | Semantic tokens             | `semanticTokens/full` + `delta`; token legend (function/variable/keyword)                       | 1          |
|     3 | Hover (types & docs)        | Lightweight type inference; `textDocument/hover` with signature & doc‑comment                   | 2          |
|     4 | Completion & signature help | `completion`, `signatureHelp`; scope‑aware symbol table                                         | 3          |
|     5 | Navigation                  | `definition`, `typeDefinition`, `references`; cross‑file symbol index                           | 3          |
|     6 | Rename refactor             | `prepareRename`, `rename` returning workspace edits                                             | 5          |
|     7 | Code actions / quick fixes  | Light‑bulb fixes for lint codes (e.g. NS001)                                                    | 1, 3       |
|     8 | Formatter integration       | Stand‑alone `nscfmt`; `textDocument/formatting` shell‑out                                       | 0          |
|     9 | Inlay hints                 | `textDocument/inlayHint` for types & parameter names                                            | 3          |
|    10 | Testing & CI                | Extension tests; GitHub CI (unit, fmt, LSP smoke)                                               | all        |

---

## Phase Details

### Phase 0 — Baseline Editor Support

* Run `yo code` → *New Language Support* scaffold.
* Define language id `notscheme`, file extension `.ns`.
* Create `language‑configuration.json` (line comments `//`, bracket pairs, auto‑close).
* Write minimal `syntaxes/notscheme.tmLanguage.json` (regex rules for strings, numbers, booleans, keywords, identifiers).
* **Exit criteria:** `.ns` files highlight; comment toggle works; extension packages via `vsce package`.

### Phase 1 — LSP Core / Diagnostics

* Thread `Span { start, end }` into every AST node; emit diagnostic list with stable codes (`NS###`).
* Implement `nsc‑lsp` server using **tower‑lsp**; handle `didOpen` and `didChange` → parse + publish diagnostics.
* VS Code client starts the server over stdio.
* **Exit criteria:** live red squiggles and Problems panel entries while typing.

### Phase 2 — Semantic Tokens

* Define token legend (`function`, `variable`, `keyword`, `type`, `macro`) and modifiers (`declaration`, `mutable`, `static`).
* Walk AST to emit `(line, start, length, token_type, modifiers)`.
* Support `semanticTokens/full` and `semanticTokens/delta`.
* **Exit criteria:** distinct colours for list heads, locals, globals, keywords.

### Phase 3 — Hover (Types & Docs)

* Add lightweight Hindley–Milner‑style or best‑effort type inference to get function/variable types.
* Extract doc‑comments (`///`) adjacent to definitions.
* Implement `textDocument/hover` returning Markdown signature + first doc line.
* **Exit criteria:** hover shows type signature and doc string.

### Phase 4 — Completion & Signature Help

* Trigger completion after `(`, within `use`, after `(get`, etc.
* Rank suggestions by scope proximity and type relevance.
* Implement `signatureHelp` to display parameter list and highlight current argument index.

### Phase 5 — Navigation

* Build definition/reference index (`SymbolId → Vec<Location>`).
* Provide go‑to definition, go‑to type definition, and find references.

### Phase 6 — Rename

* Implement `prepareRename` to validate target symbol.
* Generate a `WorkspaceEdit` covering all references; ensure version consistency.

### Phase 7 — Code Actions / Quick Fixes

* Map lint codes to quick‑fixes (e.g. remove unused binding, add missing `use`).
* Support lazy edit generation with `codeAction/resolve`.

### Phase 8 — Formatter Integration

* Build `nscfmt` (rustfmt‑style pretty‑printer).
* LSP server shells out to `nscfmt`, diffs formatted text vs current buffer.
* Support `textDocument/rangeFormatting` for on‑type formatting.

### Phase 9 — Inlay Hints

* Emit inline type hints for let‑bindings, closure params, numeric literals.
* Emit parameter‑name hints at call sites.

### Phase 10 — Testing & CI

* Unit tests for parser, type checker, linter rules.
* Extension integration tests via `@vscode/test-electron`.
* CI script: `cargo test`, `cargo clippy --deny warnings`, `cargo fmt -- --check`, extension packaging.

---

## Suggested Timeline (illustrative)

| Month | Milestones             |
| ----: | ---------------------- |
|     1 | Phases 0 → 1           |
|     2 | Phases 2 → 3           |
|     3 | Phases 4 → 5           |
|     4 | Phase 6 + 7            |
|     5 | Phase 8                |
|     6 | Phases 9 → 10 & polish |

---

### Glossary

* **LSP** – Language Server Protocol (JSON‑RPC over stdio).
* **Tower** – Rust async service framework used by `tower‑lsp`.
* **Diagnostic code** – Stable error/lint identifier (e.g. `NS001`).

---

*End of roadmap v1 – feedback welcome, sir dude.*
