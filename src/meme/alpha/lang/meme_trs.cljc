(ns meme.alpha.lang.meme-trs
  "meme-trs: token-stream term rewriting.

   Operates at the token level: tokenize → nest → rewrite → flatten → text.
   Bypasses the recursive-descent parser entirely for the meme→clj direction.
   Supports :run, :format, :convert. No :repl yet."
  (:require [meme.alpha.core :as core]
            [meme.alpha.emit.formatter.canon :as fmt-canon]
            [meme.alpha.lang.util :as util]
            [meme.alpha.rewrite :as rw]
            [meme.alpha.rewrite.rules :as rules]
            [meme.alpha.rewrite.emit :as remit]
            [meme.alpha.trs :as trs]
            #?(:clj [meme.alpha.runtime.run :as run])))

(defn format-meme [source opts]
  (fmt-canon/format-forms (core/meme->forms source) opts))

#?(:clj
   (defn clj->meme [source]
     (let [forms (core/clj->forms source)
           tagged (mapv #(rw/rewrite rules/s->m-rules %) forms)
           tagged (mapv #(rules/rewrite-inside-reader-conditionals
                           (fn [f] (rw/rewrite rules/s->m-rules f)) %)
                        tagged)]
       (remit/emit-forms tagged))))

(defn convert [source opts]
  (if (util/meme-source? source opts)
    (trs/meme->clj-text source)
    #?(:clj (clj->meme source)
       :cljs (throw (ex-info "clj→meme requires JVM" {})))))

#?(:clj
   (defn run-source [source opts]
     (let [clj-text (trs/meme->clj-text source)]
       (run/run-string clj-text opts))))
