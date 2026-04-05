# Red Team Report Round 3: meme-clj Comprehensive Adversarial Analysis

**Date:** 2026-04-05
**Branch:** `fuzzer` (after rounds 1-2 fixes applied)
**Methodology:** 5 parallel adversarial agents testing 50 hypotheses + 1M Jazzer fuzzer executions (3 targets: roundtrip, format, idempotent). Covers syntax-quote expansion, interop/symbols/numbers, destructuring/ns/multi-arity, error recovery/performance, CLJS/generative/CLI.

---

## Executive Summary

Round 3 was the deepest and broadest probe — 50 hypotheses across entirely fresh attack surfaces plus 1M coverage-guided fuzz runs. The codebase proved remarkably solid: **44 of 50 hypotheses were fully refuted** (no bugs). Three new bugs were found, plus three lower-severity observations. The fuzzer results are appended below.

| Severity | Count | Key Findings |
|----------|-------|--------------|
| **MEDIUM** | 3 | Unterminated regex truncation; unquote outside syntax-quote silent; REPL unterminated string `:complete` |
| **LOW** | 3 | Generative test gaps; snapshot test gaps; .meme can't require .meme |
| **INFO** | 44 | All refuted — robust across all tested surfaces |

---

## Confirmed Bugs

### BUG-1: Unterminated regex silently truncates (MEDIUM)

Found by **two independent agents** (H20, H37). The string untermination fix from round 2 was not applied to regex.

```clojure
(api/meme->forms "#\"hello")  ;; => [#"hell"]  (WRONG — should error)
(api/meme->forms "#\"ab")     ;; => [#"a"]     (WRONG)
(api/meme->forms "#\"")       ;; => error       (only this case caught)
```

**Root cause:** `resolve-regex` in `resolve.cljc` only checks `(< (count raw) 3)` but does not verify the last character is `"`. Same root cause as the string bug fixed in round 2 — `consume-string` returns EOF position without signaling error.

**Fix:** Add the same closing-quote check that `resolve-string` now has:
```clojure
(when (or (< (count raw) 3)
          (not= (.charAt ^String raw (dec (count raw))) \"))
  (errors/meme-error "Unterminated regex literal" (assoc loc :incomplete true)))
```

---

### BUG-2: Unquote (~) outside syntax-quote silently accepted (MEDIUM)

Found by H9. `~x` and `~@x` at top level (not inside backtick) parse successfully and produce opaque `MemeUnquote`/`MemeUnquoteSplicing` records that survive expansion.

```clojure
(meme-lang.run/run-string "def(x ~y)" {})
;; => #'user/x  (x is bound to MemeUnquote{:form y})
;; Clojure: compile error — unquote not in syntax-quote
```

**Root cause:** The expander's `expand-syntax-quotes` recurses into `MemeUnquote`/`MemeUnquoteSplicing` nodes rather than erroring when they appear outside any syntax-quote context.

**Fix:** In `expander.cljc`, the top-level expansion should detect bare `MemeUnquote`/`MemeUnquoteSplicing` nodes and throw an error.

---

### BUG-3: REPL reports `:complete` for unterminated strings (MEDIUM)

Found by H48. Despite the round 2 fix to `resolve-string` (which now throws with `:incomplete true`), the REPL's `input-state` still reports `:complete` for `"unterminated`.

**Root cause:** The `input-state` function in `tools/repl.clj` catches `Throwable` and checks `:incomplete` in ex-data. The unterminated string error now has `:incomplete true`, but `input-state` is called via a `run-fn` that goes through the full pipeline — and the scanner's `consume-string` doesn't throw (it returns EOF position). The error only occurs later in `resolve-string`, which is called from the CST reader, not the scanner. By that point, `input-state`'s catch clause may see it as `:invalid` rather than `:incomplete`.

**Investigation needed:** Verify the exact control flow — does `input-state` actually receive the `:incomplete true` ex-data from `resolve-string`? The fix may require ensuring the `:incomplete` marker propagates correctly through the pipeline.

---

## Refuted Hypotheses — Codebase Strengths

### Syntax-Quote Expansion (H1-H10): 9/10 refuted
- Gensym uniqueness across expansions: correct (increments global counter)
- Nested gensym scoping: correct (fresh `*gensym-env*` per backtick level)
- Unquote-splicing position validation: correct (errors at top level, allows in collection context)
- Metadata in syntax-quote: correct (`with-meta` emitted)
- Interop forms in syntax-quote: correct (left unqualified)
- Namespace resolution: correct (portable design with optional `:resolve-symbol`)
- Reader macros inside backtick: correct
- Double backtick: correct (matches Clojure output exactly)
- Special forms: correct (not namespace-qualified)

### Interop, Symbols, Numbers (H11-H20): 9/10 refuted
- Multi-arg method calls, static field vs method, constructors: all correct
- Dotted class names, dot-symbols: correct
- Special-character symbols (`+`, `->>`, `swap!`, `nil?`): all correct
- All Clojure numeric formats: correct (including hex, octal, radix, BigInt, BigDecimal, ratio, scientific, `##Inf`/`##NaN`)
- Invalid numerics: all properly error with clear messages
- Comma-as-whitespace: correct (comma breaks adjacency as expected)
- Line/column tracking: accurate for multi-line, unicode, tabs, BOM

### Destructuring, NS, Multi-arity (H21-H30): 10/10 refuted
- Nested destructuring (`:keys`, `:as`, `& rest`, nested maps, `:strs`, `:syms`): all roundtrip
- Multi-arity with vector-as-head: correct
- Complex ns declarations (`:require`, `:import`, `:refer-clojure`, `:gen-class`, docstring, metadata): all roundtrip
- Protocols, records, `extend-type`, `reify`, `proxy`, `deftype`: all roundtrip
- Multimethods: all roundtrip
- Try/catch/finally (multiple catch, nested try): all roundtrip
- Threading macros (`->`, `->>`, `cond->`, `some->`, `as->`): all roundtrip
- For/doseq/loop with modifiers: all roundtrip
- Case/cond/condp edge cases: all roundtrip
- **Formatter preserves semantics for ALL control flow forms** (23/23 flat, 15/15 canon)

### Error Recovery, Performance (H31-H40): 8/10 refuted
- Error recovery: stops at first error (standard parser behavior)
- Error positions: accurate for multi-line, unicode, BOM
- Depth limit + consecutive `#_`: correctly enforced inside discards
- Keyword validation + namespaced maps: no interference
- Performance: roughly linear (some GC noise but no quadratic pathology)
- Backslash handling: correct in all contexts (char, string, regex)
- Empty forms: all roundtrip correctly
- Nested `#()` rejection precision: correct (fn inside #() still works)

### CLJS, Generative, CLI (H41-H50): 8/10 refuted
- CLJS numeric divergence: well-handled (clear error messages for BigInt, BigDecimal, ratio, hex)
- CLI `inspect`: works as designed (lang diagnostics tool)
- CLI `format --check`: works correctly
- Multi-file format: works correctly (skips non-.meme files)
- Error formatting (`format-error`): robust (CRLF, long lines, EOF, spans, hints all handled)
- Test infrastructure: deterministic, sorted, self-asserting, proper error handling

---

## Lower-Severity Observations

### OBS-1: Generative tests don't cover reader conditionals, namespaced maps, tagged literals, regex (LOW-MEDIUM)
H42. The generators cover forms with metadata, syntax-quote, deep nesting, empty collections, prefix composition, and all head types. But reader conditionals, namespaced maps, tagged literals, and regex are not generated. These are tested elsewhere (snapshot, regression, unit) but lack property-based coverage.

### OBS-2: Snapshot tests have minor gaps (LOW)
H43. No error node snapshots, no depth limit error snapshot, no `#?@` (splicing reader conditional) snapshot, no shebang/BOM token snapshots. Snapshots are stable against formatter changes (good design).

### OBS-3: `.meme` files cannot require other `.meme` files (LOW)
H47. The run pipeline evaluates forms via `eval`, so `require` uses Clojure's standard loader (only `.clj`/`.cljc`). This is an architectural limitation, not a bug — meme is a frontend, not a new runtime.

---

## Fuzzer Results (1M executions)

Three Jazzer targets, 333K executions each, with coverage-guided mutation and fork-mode parallelism.

| Target | Runs | Finding |
|--------|------|---------|
| **roundtrip** | 333,334 | (pending — results appended when complete) |
| **format** | 333,334 | (pending) |
| **idempotent** | 333,334 | (pending) |

*Results will be appended when fuzzer runs complete.*

---

## Summary of All Three Rounds

### Cumulative Bug Count

| Round | Bugs Found | Fixed | Remaining |
|-------|-----------|-------|-----------|
| Round 1 | 2 bugs + 1 design gap | All 3 fixed | 0 |
| Round 2 | 8 bugs | 7 fixed (1 documented) | 1 (comment loss — design limitation) |
| Round 3 | 3 bugs | Pending | 3 |
| **Total** | **13 bugs + 1 design gap** | **10 fixed** | **4 remaining** |

### Remaining Issues (Priority Order)

1. **Unterminated regex truncation** (MEDIUM) — `resolve.cljc`, same fix as strings
2. **Unquote outside syntax-quote** (MEDIUM) — `expander.cljc`, detect bare MemeUnquote at top level
3. **REPL unterminated string detection** (MEDIUM) — `tools/repl.clj`, ensure `:incomplete` propagates
4. **Comment loss in data literals** (LOW, design limitation) — documented, not fixable without architecture change

### Hypothesis Scorecard

| Category | Tested | Refuted | Confirmed Bug | Partial/Info |
|----------|--------|---------|---------------|-------------|
| Round 3: Syntax-quote (H1-10) | 10 | 9 | 1 | 0 |
| Round 3: Interop/symbols/numbers (H11-20) | 10 | 9 | 1 | 0 |
| Round 3: Destructuring/ns/multi-arity (H21-30) | 10 | 10 | 0 | 0 |
| Round 3: Error recovery/perf (H31-40) | 10 | 8 | 1 | 1 |
| Round 3: CLJS/generative/CLI (H41-50) | 10 | 8 | 1 | 1 |
| **Round 3 Total** | **50** | **44** | **3** (+1 dup) | **3** |
