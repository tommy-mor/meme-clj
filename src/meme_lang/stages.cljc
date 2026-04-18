(ns meme-lang.stages
  "Composable pipeline stages for the lossless reader.

   Pipeline: step-parse → step-read → (optionally) step-expand-syntax-quotes

   Each stage is a ctx → ctx function operating on a shared context map:

   | Key          | Type           | Written by  | Read by          |
   |--------------|----------------|-------------|------------------|
   | :source      | String         | caller      | parse            |
   | :opts        | Map or nil     | caller      | parse, read      |
   | :cst         | Vector         | parse       | read, (tooling)  |
   | :forms       | Vector         | read        | caller, expand   |

   The table above is mirrored as machine-readable data in `stage-contracts`.
   Each stage calls `check-contract!` at entry, so miscomposed pipelines
   (e.g. calling step-read before step-parse) throw a clear pipeline-error
   with the missing key(s) and the actual ctx-keys present, instead of
   surfacing deep-inside NPEs.

   Stages are independent.  Compose in any order respecting dependencies.
   Skip step-read for tooling that works with CST directly."
  (:require [clojure.string :as str]
            [meme-lang.grammar :as grammar]
            [meme-lang.cst-reader :as cst-reader]
            [meme-lang.expander :as expander]
            [meme.tools.parser :as pratt]))

;; ---------------------------------------------------------------------------
;; Stage contracts — machine-readable required/produced ctx keys
;; ---------------------------------------------------------------------------

(def stage-contracts
  "Machine-readable pipeline contract.  Each entry is
   `{:requires #{ctx-keys} :produces #{ctx-keys}}`.

   `:requires` is enforced at runtime by `check-contract!`; `:produces`
   is documentation — if a stage fails to produce what it claims, the
   next stage's `:requires` check catches the mistake, so post-condition
   runtime checks would be redundant."
  {:step-parse                {:requires #{:source} :produces #{:cst}}
   :step-read                 {:requires #{:cst}    :produces #{:forms}}
   :step-expand-syntax-quotes {:requires #{:forms}  :produces #{:forms}}})

(defn- check-contract!
  "Throw a clear pipeline-error if `ctx` is missing any of `stage-name`'s
   required keys.  Keeps miscomposition errors near the actual mistake."
  [stage-name ctx]
  (when-let [required (get-in stage-contracts [stage-name :requires])]
    (let [missing (remove #(contains? ctx %) required)]
      (when (seq missing)
        (throw (ex-info
                 (str "Pipeline stage " stage-name " missing required ctx key(s): "
                      (str/join ", " (map pr-str missing))
                      ". Did you skip an earlier stage?")
                 {:type :meme-lang/pipeline-error
                  :stage stage-name
                  :missing (vec missing)
                  :ctx-keys (vec (keys ctx))}))))))

;; ---------------------------------------------------------------------------
;; Pipeline stages
;; ---------------------------------------------------------------------------

(defn step-parse
  "Parse source string into CST using the unified Pratt parser.
   Uses meme grammar by default, or (:grammar opts) if provided.
   Reads :source, writes :cst."
  [ctx]
  (check-contract! :step-parse ctx)
  (let [source (:source ctx)]
    (when-not (string? source)
      (throw (ex-info (str "Pipeline :source must be a string, got "
                           (if (nil? source) "nil"
                               #?(:clj (.getName (class source))
                                  :cljs (pr-str (type source)))))
                      {:type :meme-lang/pipeline-error :stage :step-parse})))
    (let [spec (or (get-in ctx [:opts :grammar]) grammar/grammar)]
      (assoc ctx :cst (pratt/parse source spec)))))

(defn step-read
  "Lower CST to Clojure forms via the CST reader.
   Reads :cst, writes :forms."
  [ctx]
  (check-contract! :step-read ctx)
  (assoc ctx :forms (cst-reader/read-forms (:cst ctx) (:opts ctx))))

(defn step-expand-syntax-quotes
  "Expand syntax-quote AST nodes, unwrap MemeRaw values, and convert
   MemeAutoKeyword records to eval-able forms in :forms.
   Produces plain Clojure forms ready for eval."
  [ctx]
  (check-contract! :step-expand-syntax-quotes ctx)
  (let [opts (cond-> (or (:opts ctx) {})
               (not (contains? (:opts ctx) :expand-auto-keywords))
               (assoc :expand-auto-keywords true))]
    (assoc ctx :forms (expander/expand-forms (:forms ctx) opts))))

(defn expand-syntax-quotes
  "Expand syntax-quote AST nodes in a seq of forms. Convenience wrapper
   around step-expand-syntax-quotes for callers that don't need the
   full pipeline context map."
  [forms opts]
  (:forms (step-expand-syntax-quotes
            {:source "" :cst [] :forms (vec forms) :opts opts})))

;; ---------------------------------------------------------------------------
;; Convenience: full pipeline
;; ---------------------------------------------------------------------------

(defn strip-shebang
  "Strip a leading #! shebang line from source."
  [source]
  (if (and (string? source) (str/starts-with? source "#!"))
    (let [lf (str/index-of source "\n")
          cr (str/index-of source "\r")
          nl (cond
               (and lf cr) (min lf cr)
               lf lf
               cr cr
               :else nil)]
      (if nl
        (let [next (inc nl)]
          ;; Skip full \r\n pair when present
          (if (and (= (.charAt ^String source nl) \return)
                   (< next (count source))
                   (= (.charAt ^String source next) \newline))
            (subs source (inc next))
            (subs source next)))
        ""))
    source))

(defn run
  "Run the full pipeline: source → CST → forms."
  ([source] (run source nil))
  ([source opts]
   (-> {:source (strip-shebang source) :opts opts}
       step-parse
       step-read)))
