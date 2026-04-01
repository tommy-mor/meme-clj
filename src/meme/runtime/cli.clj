(ns meme.runtime.cli
  "Unified CLI: run, repl, to-clj, to-meme, format, inspect, version.
   Commands dispatch through lang maps."
  (:require [meme.errors :as errors]
            [meme.lang :as lang]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; File utilities
;; ---------------------------------------------------------------------------

(defn- find-files [dir pred]
  (->> (file-seq (io/file dir))
       (filter #(.isFile ^java.io.File %))
       (map str)
       (filter pred)
       sort))

(defn- expand-inputs [inputs pred]
  (mapcat (fn [path]
            (if (.isDirectory (io/file path))
              (find-files path pred)
              (if (pred path) [path] [])))
          inputs))

(defn- meme-file? [path] (str/ends-with? path ".meme"))
(defn- clj-file? [path] (boolean (re-find #"\.clj[cdsx]?$" path)))

(defn- swap-ext [path from to]
  (str/replace path (re-pattern (str "\\." from "$")) (str "." to)))

;; ---------------------------------------------------------------------------
;; Generic file processor
;; ---------------------------------------------------------------------------

(defn- process-file [transform output-fn verb stdout check path]
  (try
    (cond
      stdout (do (println (transform path)) :ok)
      check  (let [src (slurp path)
                   formatted (str (transform path) "\n")]
               (if (= src formatted)
                 :ok
                 (do (println (str "would reformat: " path)) :fail)))
      :else  (let [out (if output-fn (output-fn path) path)
                   result (transform path)]
               (spit out (str result "\n"))
               (if (= path out)
                 (println (str verb " " path))
                 (println (str path " → " out)))
               :ok))
    (catch Exception e
      (let [src (try (slurp path) (catch Exception _ nil))]
        (binding [*out* *err*]
          (println (errors/format-error e src))))
      :fail)))

(defn- process-files [{:keys [inputs pred transform output-fn stdout check verb]}]
  (let [expanded (expand-inputs inputs pred)]
    (when (empty? expanded)
      (println "No matching files found.")
      (System/exit 1))
    (let [results (doall (map #(process-file transform output-fn verb stdout check %) expanded))
          total   (count results)
          failed  (count (filter #{:fail} results))]
      (when-not stdout
        (println)
        (println (str total " file(s), " (- total failed) " " verb
                      (when (pos? failed) (str ", " failed " failed")))))
      (when (pos? failed) (System/exit 1)))))

;; ---------------------------------------------------------------------------
;; Lang resolution
;; ---------------------------------------------------------------------------

(def ^:private cli-keys
  "Keys consumed by the CLI, not passed to lang functions."
  #{:file :files :stdout :check :lang})

(defn- lang-opts [m]
  (apply dissoc m cli-keys))

(defn- get-lang
  "Resolve [name lang-map] from --lang flag, file extension, or default."
  [lang-str file]
  (cond
    (and lang-str (str/ends-with? lang-str ".edn"))
    (try [lang-str (lang/load-edn lang-str)]
         (catch Exception e
           (binding [*out* *err*]
             (println (str "Error loading lang " lang-str ": " (ex-message e))))
           (System/exit 1)))

    lang-str
    (let [lang-kw (keyword lang-str)]
      (try [lang-kw (lang/resolve-lang lang-kw)]
           (catch Exception _
             (binding [*out* *err*]
               (println (str "Unknown lang: " lang-kw))
               (println (str "Available: " (str/join ", " (map name (keys @lang/builtin))))))
             (System/exit 1))))

    file
    (if-let [[ext-name ext-lang] (lang/resolve-by-extension file)]
      [ext-name ext-lang]
      [:meme-classic (lang/resolve-lang :meme-classic)])

    :else
    [:meme-classic (lang/resolve-lang :meme-classic)]))

;; ---------------------------------------------------------------------------
;; Commands
;; ---------------------------------------------------------------------------

(defn run [{:keys [file lang] :as opts}]
  (when-not file
    (binding [*out* *err*] (println "Usage: meme run <file> [--lang name]"))
    (System/exit 1))
  (let [[lang-name l] (get-lang lang file)]
    (lang/check-support! l lang-name :run)
    (try
      ((:run l) (slurp file) (lang-opts opts))
      (catch Exception e
        (let [src (try (slurp file) (catch Exception _ nil))]
          (binding [*out* *err*] (println (errors/format-error e src))))
        (System/exit 1)))))

(defn repl [{:keys [lang] :as opts}]
  (let [[lang-name l] (get-lang lang nil)]
    (lang/check-support! l lang-name :repl)
    ((:repl l) (lang-opts opts))))

(defn to-clj [{:keys [file files stdout lang] :as opts}]
  (let [inputs (or files (when file [file]))
        [lang-name l] (get-lang lang file)]
    (when (empty? inputs)
      (println "Usage: meme to-clj <file|dir> [--lang name] [--stdout]")
      (System/exit 1))
    (lang/check-support! l lang-name :to-clj)
    (process-files
      {:inputs    inputs
       :pred      meme-file?
       :transform (fn [path] ((:to-clj l) (slurp path)))
       :output-fn (fn [path] (swap-ext path "meme" "clj"))
       :stdout    stdout
       :verb      "converted"})))

(defn to-meme [{:keys [file files stdout lang] :as opts}]
  (let [inputs (or files (when file [file]))
        [lang-name l] (get-lang lang file)]
    (when (empty? inputs)
      (println "Usage: meme to-meme <file|dir> [--lang name] [--stdout]")
      (System/exit 1))
    (lang/check-support! l lang-name :to-meme)
    (process-files
      {:inputs    inputs
       :pred      clj-file?
       :transform (fn [path] ((:to-meme l) (slurp path)))
       :output-fn (fn [path] (swap-ext path "clj" "meme"))
       :stdout    stdout
       :verb      "converted"})))

(defn format-cmd [{:keys [file files stdout check lang style] :as opts}]
  (let [inputs (or files (when file [file]))
        [lang-name l] (get-lang lang file)
        lopts (lang-opts opts)
        ;; --style clj uses :to-clj command; otherwise :format
        cmd (if (= style "clj") :to-clj :format)]
    (when (empty? inputs)
      (println "Usage: meme format <file|dir> [--style canon|flat|clj] [--stdout] [--check]")
      (System/exit 1))
    (lang/check-support! l lang-name cmd)
    (process-files
      {:inputs    inputs
       :pred      meme-file?
       :transform (fn [path]
                    (let [src (slurp path)]
                      (case style
                        "clj" ((:to-clj l) src)
                        ((cmd l) src lopts))))
       :stdout    stdout
       :check     check
       :verb      "formatted"})))

(defn inspect-cmd [{:keys [lang]}]
  (let [[lang-name l] (get-lang lang nil)]
    (println (str "Lang: " (name lang-name)))
    (println (str "  Supported commands: "
                  (str/join ", " (map name (filter keyword? (keys l))))))))

(defn version [_]
  (let [v (try (some-> (io/resource "meme/version.txt") slurp str/trim)
               (catch Exception _ nil))]
    (println (if v (str "meme " v) "meme (version unknown)"))))

(defn help [_]
  (let [l (lang/resolve-lang lang/default-lang)
        has? #(lang/supports? l %)]
    (println "meme — M-expressions for Clojure")
    (println)
    (println (str "Default lang: " (name lang/default-lang)))
    (println)
    (println "Commands:")
    (when (has? :run)     (println "  meme run <file> [--lang name]"))
    (when (has? :repl)    (println "  meme repl [--lang name]"))
    (when (has? :to-clj)  (println "  meme to-clj <file|dir> [--lang name] [--stdout]"))
    (when (has? :to-meme) (println "  meme to-meme <file|dir> [--lang name] [--stdout]"))
    (when (has? :format)  (println "  meme format <file|dir> [--style canon|flat|clj] [--stdout] [--check]"))
    (println "  meme inspect [--lang name]")
    (println "  meme version")
    (println)
    (println (str "Available langs: " (str/join ", " (map name (keys @lang/builtin)))))))

;; ---------------------------------------------------------------------------
;; bb entry point
;; ---------------------------------------------------------------------------

(defn- collect-file-args [{:keys [opts args]}]
  (let [all (into (if (:file opts) [(:file opts)] []) args)
        base (if (= 1 (count all)) {:file (first all)} {:files all})]
    (merge (dissoc opts :file) base)))

(def ^:private file+stdout+lang
  {:args->opts [:file] :spec {:stdout {:coerce :boolean} :lang {:coerce :string}}})

(defn -main [& args]
  (require 'babashka.cli)
  (let [dispatch (resolve 'babashka.cli/dispatch)]
    (dispatch
      [{:cmds ["run"]     :fn (fn [{:keys [opts]}] (run opts))
        :args->opts [:file] :spec {:lang {:coerce :string}}}
       {:cmds ["repl"]    :fn (fn [{:keys [opts]}] (repl opts))
        :spec {:lang {:coerce :string}}}
       (merge {:cmds ["to-clj"]  :fn (fn [m] (to-clj (collect-file-args m)))}
              file+stdout+lang)
       (merge {:cmds ["to-meme"] :fn (fn [m] (to-meme (collect-file-args m)))}
              file+stdout+lang)
       {:cmds ["format"]  :fn (fn [m] (format-cmd (collect-file-args m)))
        :args->opts [:file] :spec {:stdout {:coerce :boolean} :check {:coerce :boolean}
                                   :lang {:coerce :string} :style {:coerce :string}
                                   :width {:coerce :long}}}
       {:cmds ["inspect"] :fn (fn [{:keys [opts]}] (inspect-cmd opts))
        :spec {:lang {:coerce :string}}}
       {:cmds ["version"] :fn (fn [_] (version nil))}
       {:cmds []          :fn (fn [{:keys [args]}]
                                (when (seq args)
                                  (binding [*out* *err*]
                                    (println (str "Unknown command: " (first args))))
                                  (println))
                                (help nil)
                                (when (seq args) (System/exit 1)))}]
      args)))
