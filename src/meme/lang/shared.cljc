(ns meme.lang.shared
  "Shared implementations for lang backends."
  (:require [meme.rewrite :as rw]
            [meme.rewrite.rules :as rules]
            [meme.rewrite.emit :as remit]
            #?(:clj [meme.core :as core])))

#?(:clj
   (defn clj->meme-text
     "Convert Clojure source text to meme syntax via rewrite rules.
      Shared by meme-rewrite and meme-trs langs."
     ([source]
      (let [forms (core/clj->forms source)
            tagged (mapv #(rw/rewrite rules/s->m-rules %) forms)
            tagged (mapv #(rules/rewrite-inside-reader-conditionals
                            (fn [f] (rw/rewrite rules/s->m-rules f)) %)
                         tagged)]
        (remit/emit-forms tagged)))
     ([source _opts] (clj->meme-text source))))
