(ns meme.alpha.lang
  "Lang registry, resolution, and EDN loading.

   A lang is a map of command functions:
     :run     (fn [source opts] → result)   — run a file
     :repl    (fn [opts] → nil)             — interactive loop
     :format  (fn [source opts] → text)     — format a file
     :convert (fn [source opts] → text)     — convert a file (both directions)

   Every key is optional. A lang supports exactly the commands it has keys for.
   The CLI dispatches by looking up the command key in the lang map.

   Built-in langs are defined in meme.alpha.lang.*:
     :meme-classic (default) — recursive-descent parser + Wadler-Lindig printer
     :meme-rewrite           — tree builder + rewrite rules
     :meme-trs               — token-stream term rewriting

   User langs are EDN files:
     {:run \"core.meme\" :format :meme-classic}
   Values: string → .meme file to eval before user file (for :run)
           keyword → reference to a built-in lang's command"
  (:require [clojure.edn :as edn]
            [meme.alpha.lang.meme-classic :as meme-classic]
            [meme.alpha.lang.meme-rewrite :as meme-rewrite]
            [meme.alpha.lang.meme-trs :as meme-trs]
            #?(:clj [meme.alpha.runtime.run :as run])))

(def builtin
  {:meme-classic meme-classic/lang
   :meme-rewrite meme-rewrite/lang
   :meme-trs     meme-trs/lang})

(def default-lang :meme-classic)

;; ---------------------------------------------------------------------------
;; EDN lang loading
;; ---------------------------------------------------------------------------

#?(:clj
   (defn- resolve-command-value
     "Resolve a single command value from an EDN lang definition.
      string → for :run, wraps as: eval the .meme file, then eval user source
      keyword → look up that command from a built-in lang"
     [command value]
     (cond
       (string? value)
       (case command
         :run (fn [source opts]
                (let [prelude-src (slurp value)]
                  (run/run-string prelude-src opts)
                  (run/run-string source opts)))
         (throw (ex-info (str "String value not supported for :" (name command)
                              " — use a keyword to reference a built-in")
                         {:command command :value value})))

       (keyword? value)
       (let [base (get builtin value)]
         (when-not base
           (throw (ex-info (str "Unknown built-in lang: " value) {:value value})))
         (let [cmd-fn (get base command)]
           (when-not cmd-fn
             (throw (ex-info (str "Built-in '" (name value) "' does not support :" (name command))
                             {:lang value :command command})))
           cmd-fn))

       :else
       (throw (ex-info (str "Invalid lang value for :" (name command)
                            " — expected string or keyword, got " (type value))
                       {:command command :value value})))))

#?(:clj
   (defn load-edn
     "Load a lang from an EDN file. Returns a lang map with functions.
      Each key-value pair is resolved: strings become run-with-core functions,
      keywords reference built-in lang commands."
     [path]
     (let [edn-data (edn/read-string (slurp path))]
       (when-not (map? edn-data)
         (throw (ex-info (str "Lang EDN must be a map, got " (type edn-data))
                         {:path path})))
       (into {} (map (fn [[k v]] [k (resolve-command-value k v)]) edn-data)))))

;; ---------------------------------------------------------------------------
;; Resolution
;; ---------------------------------------------------------------------------

(defn resolve-lang
  "Resolve a lang by keyword name. Returns the lang map.
   Throws on unknown name."
  [lang-name]
  (let [name (or lang-name default-lang)]
    (or (get builtin name)
        (throw (ex-info (str "Unknown lang: " (pr-str name)
                             " — available: " (pr-str (keys builtin)))
                        {:lang name})))))

(defn supports?
  "Does the lang support the given command?"
  [lang command]
  (contains? lang command))

(defn check-support!
  "Assert that the lang supports the given command. Throws if not."
  [lang lang-name command]
  (when-not (supports? lang command)
    (throw (ex-info (str "Lang '" (if (keyword? lang-name) (name lang-name) lang-name)
                         "' does not support :"
                         (name command)
                         " — supported: " (pr-str (vec (filter keyword? (keys lang)))))
                    {:lang lang-name :command command}))))
