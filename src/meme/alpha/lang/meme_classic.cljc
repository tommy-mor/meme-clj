(ns meme.alpha.lang.meme-classic
  "meme-classic: recursive-descent parser + Wadler-Lindig printer.

   The default lang. Supports all commands: :run, :repl, :format, :convert."
  (:require [meme.alpha.core :as core]
            [meme.alpha.emit.formatter.canon :as fmt-canon]
            [meme.alpha.lang.util :as util]))

(defn format-meme [source opts]
  (fmt-canon/format-forms (core/meme->forms source) opts))

(defn convert [source opts]
  (if (util/meme-source? source opts)
    (core/meme->clj source opts)
    #?(:clj (core/clj->meme source)
       :cljs (throw (ex-info "clj→meme requires JVM" {})))))
