(ns meme.alpha.convert
  "Unified convert: meme↔clj via classic, rewrite, or collapsar pipeline.

   Three pipelines:
     :classic   — recursive-descent parser + Wadler-Lindig printer (default)
     :rewrite   — tree builder + meme.alpha.rewrite rules
     :collapsar — tree builder + meme.alpha.collapsar phases"
  (:require [meme.alpha.core :as core]
            [meme.alpha.collapsar.meme :as collapsar]
            [meme.alpha.pipeline :as pipeline]
            [meme.alpha.rewrite :as rw]
            [meme.alpha.rewrite.tree :as tree]
            [meme.alpha.rewrite.rules :as rules]
            [meme.alpha.rewrite.emit :as remit]))

(def pipelines
  "Available pipeline names."
  #{:classic :rewrite :collapsar})

;; ---------------------------------------------------------------------------
;; meme → clj
;; ---------------------------------------------------------------------------

(defn meme->clj
  "Convert meme source to Clojure source using the named pipeline."
  ([src] (meme->clj src :classic))
  ([src pipeline-name]
   (case pipeline-name
     :classic   (core/meme->clj src {:read-cond :preserve})
     :rewrite   (core/forms->clj
                  (:forms (pipeline/run src {:parser tree/rewrite-parser
                                             :read-cond :preserve})))
     :collapsar (collapsar/meme->clj src {:read-cond :preserve})
     (throw (ex-info (str "Unknown pipeline: " pipeline-name
                          " — must be one of: classic, rewrite, collapsar") {})))))

;; ---------------------------------------------------------------------------
;; clj → meme
;; ---------------------------------------------------------------------------

#?(:clj
   (defn clj->meme
     "Convert Clojure source to meme source using the named pipeline.
   JVM/Babashka only."
     ([src] (clj->meme src :classic))
     ([src pipeline-name]
      (case pipeline-name
        :classic   (core/clj->meme src)
        :rewrite   (let [forms (core/clj->forms src)
                         tagged (mapv #(rw/rewrite rules/s->m-rules %) forms)
                         tagged (mapv #(rules/rewrite-inside-reader-conditionals
                                         (fn [f] (rw/rewrite rules/s->m-rules f)) %)
                                      tagged)]
                     (remit/emit-forms tagged))
        :collapsar (collapsar/clj->meme src)
        (throw (ex-info (str "Unknown pipeline: " pipeline-name
                             " — must be one of: classic, rewrite, collapsar") {}))))))
