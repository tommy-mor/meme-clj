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
   - Read intermediate state (e.g. :raw-tokens for syntax highlighting)

   See meme.alpha.pipeline.contract for formal clojure.spec definitions of
   the context map at each stage boundary. Enable runtime validation with:
     (binding [meme.alpha.pipeline.contract/*validate* true]
       (pipeline/run source))"
  (:require [clojure.string :as str]
            [meme.alpha.scan.tokenizer :as tokenizer]
            [meme.alpha.scan.grouper :as grouper]
            [meme.alpha.parse.reader :as reader]
            [meme.alpha.parse.expander :as expander]
            [meme.alpha.pipeline.contract :as contract]))

;; ---------------------------------------------------------------------------
;; Pipeline stages — each takes and returns a context map
;; ---------------------------------------------------------------------------

(defn strip-shebang
  "Source transform: strip a leading #! shebang line from :source, if present.
   Useful for executable .meme scripts."
  [ctx]
  (contract/validate! :strip-shebang :input ctx)
  (let [source (:source ctx)
        result (if (and (string? source) (str/starts-with? source "#!"))
                 (let [nl (str/index-of source "\n")]
                   (assoc ctx :source (if nl (subs source (inc nl)) "")))
                 ctx)]
    (contract/validate! :strip-shebang :output result)
    result))

(defn scan
  "Tokenize source text into flat tokens (no structural grouping).
   Attaches leading whitespace/comments to each token as :ws."
  [ctx]
  (contract/validate! :scan :input ctx)
  (let [source (:source ctx)]
    (when-not (string? source)
      (throw (ex-info (str "Pipeline :source must be a string, got " (if (nil? source) "nil" (type source)))
                      {})))
    (let [tokens (tokenizer/tokenize source)
          result (assoc ctx :raw-tokens (tokenizer/attach-whitespace tokens source))]
      (contract/validate! :scan :output result)
      result)))

(defn group
  "Pass-through stage — returns tokens unchanged. Retained for pipeline symmetry."
  [ctx]
  (contract/validate! :group :input ctx)
  (when-not (:raw-tokens ctx)
    (throw (ex-info "Pipeline :raw-tokens missing — run scan before group" {})))
  (when-not (string? (:source ctx))
    (throw (ex-info (str "Pipeline :source must be a string, got "
                         (if (nil? (:source ctx)) "nil" (type (:source ctx))))
                    {})))
  (let [result (assoc ctx :tokens (grouper/group-tokens (:raw-tokens ctx) (:source ctx)))]
    (contract/validate! :group :output result)
    result))

(defn parse
  "Parse grouped tokens into Clojure forms."
  [ctx]
  (contract/validate! :parse :input ctx)
  (when-not (:tokens ctx)
    (throw (ex-info "Pipeline :tokens missing — run group before parse" {})))
  (let [result (assoc ctx :forms (reader/read-meme-string-from-tokens
                                   (:tokens ctx) (:opts ctx) (:source ctx)))]
    (contract/validate! :parse :output result)
    result))

(defn expand
  "Expand syntax-quote AST nodes and unwrap MemeRaw values in :forms.
   Produces plain Clojure forms ready for eval.
   Not needed for tooling that works with AST nodes directly."
  [ctx]
  (contract/validate! :expand :input ctx)
  (when-not (:forms ctx)
    (throw (ex-info "Pipeline :forms missing — run parse before expand" {})))
  (let [result (assoc ctx :forms (expander/expand-forms (:forms ctx) (:opts ctx)))]
    (contract/validate! :expand :output result)
    result))

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
