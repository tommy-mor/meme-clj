(ns meme.alpha.lang.meme-trs
  "meme-trs: token-stream term rewriting.

   Operates at the token level: tokenize → nest → rewrite → flatten → text.
   Bypasses the recursive-descent parser entirely for the meme→clj direction.
   Supports :run, :format, :to-clj, :to-meme. No :repl yet."
  (:require [meme.alpha.core :as core]
            [meme.alpha.emit.formatter.canon :as fmt-canon]
            [meme.alpha.rewrite :as rw]
            [meme.alpha.rewrite.rules :as rules]
            [meme.alpha.rewrite.emit :as remit]
            [meme.alpha.trs :as trs]))

(defn format-meme [source opts]
  (fmt-canon/format-forms (core/meme->forms source) opts))

(defn to-clj [source]
  (trs/meme->clj-text source))

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
     (let [clj-text (trs/meme->clj-text source)
           forms (core/clj->forms clj-text)
           eval-fn (or (:eval opts) eval)]
       (reduce (fn [_ form] (eval-fn form)) nil forms))))
