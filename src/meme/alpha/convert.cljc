(ns meme.alpha.convert
  "Unified convert: meme↔clj via named langs.

   Delegates to meme.alpha.lang — each lang is a command map.
   This module provides the public API for conversion, maintaining backward
   compatibility with the legacy :classic/:rewrite/:ts-trs names."
  (:require [meme.alpha.lang :as lang]))

;; ---------------------------------------------------------------------------
;; Legacy name mapping: :classic → :meme-classic, etc.
;; ---------------------------------------------------------------------------

(def ^:private legacy-names
  {:classic :meme-classic
   :rewrite :meme-rewrite
   :ts-trs  :meme-trs})

(defn- normalize-name [lang-name]
  (get legacy-names lang-name lang-name))

;; ---------------------------------------------------------------------------
;; Public API (stable — callers don't need to change)
;; ---------------------------------------------------------------------------

(defn- all-langs []
  (let [b #?(:clj @lang/builtin :cljs lang/builtin)]
    (merge b (zipmap (keys legacy-names)
                     (map #(get b %) (vals legacy-names))))))

(def pipelines
  "Available langs by keyword. Includes both legacy and new names.
   Kept as `pipelines` for backward compatibility with existing callers."
  #?(:clj (delay (all-langs))
     :cljs (all-langs)))

(defn resolve-pipeline
  "Look up a lang by keyword. Supports both legacy names (:classic, :rewrite, :ts-trs)
   and new names (:meme-classic, :meme-rewrite, :meme-trs).
   Throws on unknown name."
  [lang-name]
  (let [p #?(:clj @pipelines :cljs pipelines)]
    (or (get p lang-name)
        (throw (ex-info (str "Unknown pipeline: " lang-name
                             " — must be one of: " (pr-str (keys p))) {})))))

(defn meme->clj
  "Convert meme source to Clojure source using the named lang."
  ([src] (meme->clj src :classic))
  ([src lang-name]
   (let [l (resolve-pipeline (normalize-name lang-name))]
     (lang/check-support! l lang-name :convert)
     ((:convert l) src {:read-cond :preserve :direction :to-clj}))))

#?(:clj
   (defn clj->meme
     "Convert Clojure source to meme source using the named lang.
   JVM/Babashka only."
     ([src] (clj->meme src :classic))
     ([src lang-name]
      (let [l (resolve-pipeline (normalize-name lang-name))]
        (lang/check-support! l lang-name :convert)
        ((:convert l) src {:direction :to-meme})))))
