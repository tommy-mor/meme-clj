(ns meme.alpha.pipeline
  "Explicit pipeline composition: source → scan → group → parse → expand → forms.
   Each stage is a ctx → ctx function operating on a shared context map.

   Context map contract:

   | Key          | Type           | Written by    | Read by              |
   |--------------|----------------|---------------|----------------------|
   | :source      | String         | caller        | scan, group, parse   |
   | :opts        | Map or nil     | caller        | parse, expand        |
   | :raw-tokens  | Vector         | scan          | group                |
   | :tokens      | Vector         | group         | parse                |
   | :forms       | Vector         | parse, expand | expand, caller       |

   Stages are independent functions. Compose them in any order that respects
   the read/write dependencies above. Guest languages can:
   - Replace stages (e.g. a custom parser that reads :tokens, writes :forms)
   - Add stages (e.g. a rewrite stage that reads :forms, writes :forms)
   - Skip stages (e.g. skip expand for tooling that works with AST nodes)
   - Read intermediate state (e.g. :raw-tokens for syntax highlighting)"
  (:require [clojure.string :as str]
            [meme.alpha.scan.tokenizer :as tokenizer]
            [meme.alpha.scan.grouper :as grouper]
            [meme.alpha.parse.reader :as reader]
            [meme.alpha.parse.expander :as expander]))

;; ---------------------------------------------------------------------------
;; Pipeline stages — each takes and returns a context map
;; ---------------------------------------------------------------------------

(defn strip-shebang
  "Source transform: strip a leading #! shebang line from :source, if present.
   Useful for executable .meme scripts."
  [ctx]
  (let [source (:source ctx)]
    (if (and (string? source) (str/starts-with? source "#!"))
      (let [nl (str/index-of source "\n")]
        (assoc ctx :source (if nl (subs source (inc nl)) "")))
      ctx)))

(defn scan
  "Tokenize source text into flat tokens (no structural grouping).
   Attaches leading whitespace/comments to each token as :ws."
  [ctx]
  (let [source (:source ctx)]
    (when-not (string? source)
      (throw (ex-info (str "Pipeline :source must be a string, got " (if (nil? source) "nil" (type source)))
                      {})))
    (let [tokens (tokenizer/tokenize source)]
      (assoc ctx :raw-tokens (tokenizer/attach-whitespace tokens source)))))

(defn group
  "Pass-through stage — returns tokens unchanged. Retained for pipeline symmetry."
  [ctx]
  (when-not (:raw-tokens ctx)
    (throw (ex-info "Pipeline :raw-tokens missing — run scan before group" {})))
  (when-not (string? (:source ctx))
    (throw (ex-info (str "Pipeline :source must be a string, got "
                         (if (nil? (:source ctx)) "nil" (type (:source ctx))))
                    {})))
  (assoc ctx :tokens (grouper/group-tokens (:raw-tokens ctx) (:source ctx))))

(defn parse
  "Parse grouped tokens into Clojure forms."
  [ctx]
  (when-not (:tokens ctx)
    (throw (ex-info "Pipeline :tokens missing — run group before parse" {})))
  (assoc ctx :forms (reader/read-meme-string-from-tokens
                      (:tokens ctx) (:opts ctx) (:source ctx))))

(defn expand
  "Expand syntax-quote AST nodes and unwrap MemeRaw values in :forms.
   Produces plain Clojure forms ready for eval.
   Not needed for tooling that works with AST nodes directly."
  [ctx]
  (when-not (:forms ctx)
    (throw (ex-info "Pipeline :forms missing — run parse before expand" {})))
  (assoc ctx :forms (expander/expand-forms (:forms ctx) (:opts ctx))))

;; ---------------------------------------------------------------------------
;; Pipeline composition
;; ---------------------------------------------------------------------------

(defn run
  "Run the reader pipeline: source → tokens → grouped → forms.
   Returns the context map with :source, :raw-tokens, :tokens, :forms.
   Does NOT expand syntax-quote nodes — call expand separately for eval,
   or use run-string which includes expansion.
   Unlike meme.alpha.core/meme->forms, the pipeline attaches whitespace metadata
   (:ws) to tokens via the scan stage — present on both :raw-tokens and
   :tokens (the grouper preserves :ws on pass-through tokens)."
  ([source] (run source nil))
  ([source opts]
   (-> {:source source :opts opts}
       scan
       group
       parse)))
