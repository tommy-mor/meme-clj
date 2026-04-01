(ns meme.lang.meme-trs
  "meme-trs: token-stream term rewriting.

   Operates at the token level: tokenize → nest → rewrite → flatten → text.
   Bypasses the recursive-descent parser entirely for the meme→clj direction.
   Supports :run, :format, :to-clj, :to-meme. No :repl yet."
  (:require [meme.core :as core]
            [meme.emit.formatter.canon :as fmt-canon]
            [meme.emit.formatter.flat :as fmt-flat]
            [meme.trs :as trs]
            #?(:clj [meme.lang.shared :as shared])))

(defn format-meme [source opts]
  (let [forms (core/meme->forms source)]
    (case (:style opts)
      "flat" (fmt-flat/format-forms forms)
      "clj"  (fmt-flat/format-clj forms)
      (fmt-canon/format-forms forms opts))))

(defn to-clj
  ([source] (trs/meme->clj-text source))
  ([source _opts] (to-clj source)))

#?(:clj
   (def to-meme shared/clj->meme-text))

#?(:clj
   (defn run-source [source opts]
     (let [clj-text (trs/meme->clj-text source)
           forms (core/clj->forms clj-text)
           eval-fn (or (:eval opts) eval)]
       (reduce (fn [_ form] (eval-fn form)) nil forms))))
