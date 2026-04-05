(ns meme.loader
  "Namespace loader for registered languages.
   Intercepts clojure.core/load to search for source files with
   registered extensions on the classpath. When found, the source
   is loaded through the lang's :run function.

   Any lang registered via meme.registry with an :extension and :run
   function gets require support automatically.

   Installed implicitly by run-file and the REPL — no manual setup."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defonce ^:private original-load (atom nil))
(defonce ^:private installed? (atom false))

(defn- find-lang-resource
  "Search the classpath for a file matching any registered lang extension.
   Returns [resource run-fn] or nil."
  [path]
  (let [base (if (str/starts-with? path "/") (subs path 1) path)
        registry-fn @(requiring-resolve 'meme.registry/registered-extensions)]
    (some (fn [[ext run-fn]]
            (let [resource (io/resource (str base ext))]
              (when resource [resource run-fn])))
          (registry-fn))))

(defn- lang-load
  "Replacement for clojure.core/load that checks registered lang extensions."
  [& paths]
  (doseq [path paths]
    (if-let [[resource run-fn] (find-lang-resource path)]
      (run-fn (slurp resource) {})
      ;; No lang file found — delegate to original Clojure load
      (apply @original-load [path]))))

(defn install!
  "Install the lang-aware loader. Idempotent — safe to call multiple times.
   After this, (require 'my.ns) searches all registered lang extensions."
  []
  (when (compare-and-set! installed? false true)
    (reset! original-load @#'clojure.core/load)
    (alter-var-root #'clojure.core/load (constantly lang-load)))
  :installed)

(defn uninstall!
  "Uninstall the loader, restoring original clojure.core/load."
  []
  (when (compare-and-set! installed? true false)
    (alter-var-root #'clojure.core/load (constantly @original-load))
    (reset! original-load nil))
  :uninstalled)
