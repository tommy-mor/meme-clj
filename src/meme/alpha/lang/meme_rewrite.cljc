(ns meme.alpha.lang.meme-rewrite
  "meme-rewrite: tree builder + rewrite rules.

   Alternative parser that builds explicit m-call/bracket/brace tagged trees,
   then applies rewrite rules to transform to S-expressions.
   Supports all commands: :run, :repl, :format, :to-clj, :to-meme."
  (:require [meme.alpha.core :as core]
            [meme.alpha.emit.formatter.canon :as fmt-canon]
            [meme.alpha.stages :as stages]
            [meme.alpha.rewrite :as rw]
            [meme.alpha.rewrite.tree :as tree]
            [meme.alpha.rewrite.rules :as rules]
            [meme.alpha.rewrite.emit :as remit]
            #?(:clj [meme.alpha.runtime.run :as run])
            #?(:clj [meme.alpha.runtime.repl :as repl])))

(def ^:private rewrite-opts {:parser tree/rewrite-parser})

(defn format-meme [source opts]
  (fmt-canon/format-forms (core/meme->forms source rewrite-opts) opts))

(defn to-clj [source]
  (core/forms->clj
   (:forms (stages/run source (merge rewrite-opts {:read-cond :preserve})))))

#?(:clj
   (defn to-meme [source]
     (let [forms (core/clj->forms source)
           tagged (mapv #(rw/rewrite rules/s->m-rules %) forms)
           tagged (mapv #(rules/rewrite-inside-reader-conditionals
                           (fn [f] (rw/rewrite rules/s->m-rules f)) %)
                        tagged)]
       (remit/emit-forms tagged))))

#?(:clj
   (defn run-source [source opts]
     (run/run-string source (merge opts rewrite-opts))))

#?(:clj
   (defn start-repl [opts]
     (repl/start (merge opts rewrite-opts))))
