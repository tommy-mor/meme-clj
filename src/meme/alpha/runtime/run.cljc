(ns meme.alpha.runtime.run
  "Run .meme files: read, eval, return last result."
  (:require [clojure.string :as str]
            [meme.alpha.pipeline :as pipeline]
            #?(:clj [meme.alpha.runtime.resolve :as resolve])))

(defn- step-strip-shebang
  "Strip a leading #! shebang line from a context's :source, if present."
  [ctx]
  (let [source (:source ctx)]
    (if (and (string? source) (str/starts-with? source "#!"))
      (let [nl (str/index-of source "\n")]
        (assoc ctx :source (if nl (subs source (inc nl)) "")))
      ctx)))

(defn- default-reader-opts
  "Build reader opts, merging caller-provided opts with platform defaults.
   On JVM/Babashka, provides a default :resolve-symbol that matches
   Clojure's syntax-quote resolution (namespace-qualifying symbols).
   Does NOT provide a default :resolve-keyword — :: keywords use the
   deferred eval path so they resolve in the file's declared namespace
   at eval time, not the caller's namespace at read time."
  [opts]
  (let [base (dissoc opts :eval)]
    #?(:clj (cond-> base
              (not (:resolve-symbol base))
              (assoc :resolve-symbol resolve/default-resolve-symbol))
       :cljs base)))

(defn run-string
  "Read meme source string, eval each form, return the last result.
   Strips shebang lines from source before parsing.
   opts:
     :eval             — eval fn (default: eval; required on CLJS)
     :resolve-keyword  — fn to resolve :: keywords at read time
     :rewrite-rules    — vector of rewrite rules (applied after expansion)
     :rewrite-max-iters — max rewrite iterations (default: 100)"
  ([s] (run-string s {}))
  ([s eval-fn-or-opts]
   (let [opts (if (map? eval-fn-or-opts) eval-fn-or-opts {:eval eval-fn-or-opts})
         eval-fn (or (:eval opts)
                     #?(:clj eval
                        :cljs (throw (ex-info "run-string requires :eval option in ClojureScript" {}))))
         reader-opts (default-reader-opts opts)
         forms (:forms (-> {:source s :opts reader-opts}
                           step-strip-shebang
                           pipeline/step-scan
                           pipeline/step-parse
                           pipeline/step-expand-syntax-quotes
                           pipeline/step-rewrite))]
     (reduce (fn [_ form] (eval-fn form)) nil forms))))

(defn run-file
  "Read and eval a .meme file. Returns the last result.
   Second arg can be an eval-fn (backward compat) or an opts map."
  ([path] (run-string (#?(:clj slurp
                          :cljs (throw (ex-info "run-file requires slurp — not available in ClojureScript" {}))) path)))
  ([path eval-fn-or-opts] (run-string (#?(:clj slurp
                                          :cljs (throw (ex-info "run-file requires slurp — not available in ClojureScript" {}))) path) eval-fn-or-opts)))
