(ns meme.alpha.emit.printer
  "Form printer: Clojure forms → meme or Clojure text.
   Mode :meme (default) emits head(args...) call syntax.
   Mode :clj emits (head args...) S-expression syntax with reader sugar."
  (:require [clojure.string :as str]
            [meme.alpha.forms :as forms]))

;; ---------------------------------------------------------------------------
;; Forward declaration
;; ---------------------------------------------------------------------------

(declare print-form)

;; ---------------------------------------------------------------------------
;; Mode — :meme or :clj
;; ---------------------------------------------------------------------------

(def ^:dynamic *mode* :meme)


;; ---------------------------------------------------------------------------
;; Print helpers
;; ---------------------------------------------------------------------------

(defn- print-args
  "Print a sequence of forms separated by spaces."
  [forms]
  (str/join " " (map #(print-form %) forms)))

(defn- percent-param?
  "Is sym a % parameter symbol (%1, %2, %&)?"
  [sym]
  (some? (forms/percent-param-type sym)))

(defn- max-percent-n
  "Find the max numbered %N param index referenced in a form body.
   Returns max N found (0 if none). Ignores %& (rest params).
   Skips nested (fn ...) bodies — their % params are scoped to the inner fn."
  [form]
  (cond
    (symbol? form)
    (let [p (forms/percent-param-type form)]
      (if (number? p) p 0))
    (and (seq? form) (= 'fn (first form))) 0
    (seq? form) (reduce max 0 (map max-percent-n form))
    (vector? form) (reduce max 0 (map max-percent-n form))
    ;; AST node defrecords satisfy (map? x) — check before map?
    (forms/raw? form) 0
    (forms/syntax-quote? form) (max-percent-n (:form form))
    (forms/unquote? form) (max-percent-n (:form form))
    (forms/unquote-splicing? form) (max-percent-n (:form form))
    (map? form) (reduce max 0 (mapcat (fn [[k v]] [(max-percent-n k) (max-percent-n v)]) form))
    (set? form) (reduce max 0 (map max-percent-n form))
    #?@(:clj [(tagged-literal? form) (max-percent-n (.-form form))])
    :else 0))

;; ---------------------------------------------------------------------------
;; #() shorthand detection
;; ---------------------------------------------------------------------------

(defn- anon-fn-shorthand?
  "Can (fn [params] body) be printed as #(body)?
   Only when :meme/sugar tagged by reader (i.e., originally written as #())."
  [form]
  (and (:meme/sugar (meta form))
       (seq? form)
       (= 'fn (first form))
       (= 3 (count form))
       (vector? (second form))))

;; ---------------------------------------------------------------------------
;; Metadata prefix helper
;; ---------------------------------------------------------------------------

(defn- emit-meta-prefix
  "Convert a metadata map to its prefix string (^:key, ^Type, or ^{map})."
  [m]
  (cond
    (and (= 1 (count m))
         (keyword? (key (first m)))
         (true? (val (first m))))
    (str "^" (print-form (key (first m))))
    (and (= 1 (count m))
         (contains? m :tag)
         (symbol? (:tag m)))
    (str "^" (print-form (:tag m)))
    :else
    (str "^" (print-form m))))

;; ---------------------------------------------------------------------------
;; Decompose — notation decisions as layout descriptors
;;
;; Single source of truth for how a form should be rendered. Both print-form
;; (flat) and pprint (width-aware) consume these descriptors.
;;
;; Layout types:
;;   :atom    {:text "..."}                                    — leaf
;;   :prefix  {:prefix "..." :child form}                      — prefix + child
;;   :call    {:head form :args [form...]}                     — call syntax
;;   :wrap    {:open "..." :close "..." :children [...]}       — delimited seq
;;   :pairs   {:open "..." :close "..." :entries [[k v]...]}   — key-value
;;   :meta    {:prefixes ["..."] :child form}                  — metadata chain
;; ---------------------------------------------------------------------------

(defn decompose
  "Decompose a form into a layout descriptor for rendering."
  [form]
  (cond
    ;; Metadata prefix — checked first, before structural checks.
    ;; Filter out internal keys (:line/:column/:file/:ws/:meme/*).
    (and (some? form)
         #?(:clj (instance? clojure.lang.IMeta form)
            :cljs (satisfies? IMeta form))
         (some? (meta form))
         (seq (forms/strip-internal-meta (meta form))))
    (let [chain (:meme/meta-chain (meta form))
          stripped (with-meta form nil)
          prefixes (if chain
                     (mapv emit-meta-prefix (reverse chain))
                     [(emit-meta-prefix (forms/strip-internal-meta (meta form)))])]
      {:layout :meta, :prefixes prefixes, :child stripped})

    ;; Raw value wrapper — emit original source text
    (forms/raw? form) {:layout :atom, :text (:raw form)}

    ;; nil
    (nil? form) {:layout :atom, :text "nil"}

    ;; boolean
    (boolean? form) {:layout :atom, :text (str form)}

    ;; Deferred auto-resolve keywords: (clojure.core/read-string "::foo") → ::foo
    (forms/deferred-auto-keyword? form)
    {:layout :atom, :text (forms/deferred-auto-keyword-raw form)}

    ;; Empty list
    (and (seq? form) (empty? form))
    {:layout :atom, :text "()"}

    ;; Anon-fn shorthand #()
    (anon-fn-shorthand? form)
    {:layout :wrap, :open "#(", :close ")", :children [(nth form 2)]}

    ;; Sequences — check sugar then call
    (seq? form)
    (let [head (first form)]
      (cond
        ;; @deref — sugar only when :meme/sugar tagged by reader
        (and (= head 'clojure.core/deref) (:meme/sugar (meta form)))
        {:layout :prefix, :prefix "@", :child (second form)}

        ;; 'quote — sugar only when :meme/sugar tagged by reader
        (and (= head 'quote) (:meme/sugar (meta form)))
        {:layout :prefix, :prefix "'", :child (second form)}

        ;; #'var — sugar only when :meme/sugar tagged by reader
        (and (= head 'var) (:meme/sugar (meta form)))
        {:layout :prefix, :prefix "#'", :child (second form)}

        ;; Regular call
        :else
        {:layout :call, :head head, :args (vec (rest form))}))

    ;; Syntax-quote / unquote / unquote-splicing AST nodes
    ;; Must be before map? because these are defrecords (satisfy map?)
    (forms/syntax-quote? form)
    {:layout :prefix, :prefix "`", :child (:form form)}

    (forms/unquote? form)
    {:layout :prefix, :prefix "~", :child (:form form)}

    (forms/unquote-splicing? form)
    {:layout :prefix, :prefix "~@", :child (:form form)}

    ;; Reader conditional — must be before map? (CLJS MemeReaderConditional is a defrecord)
    (forms/meme-reader-conditional? form)
    (let [prefix (if (forms/rc-splicing? form) "#?@(" "#?(")]
      {:layout :pairs, :open prefix, :close ")",
       :entries (vec (partition 2 (forms/rc-form form)))})

    ;; Vector
    (vector? form)
    {:layout :wrap, :open "[", :close "]", :children (vec form)}

    ;; Map — reconstruct #:ns{} when :meme/ns metadata present
    (map? form)
    (if-let [ns-str (:meme/ns (meta form))]
      (let [strip-ns (fn [k]
                       (if (and (keyword? k)
                                (= (namespace k)
                                   (if (str/starts-with? ns-str ":") (subs ns-str 1) ns-str)))
                         (keyword (name k))
                         k))]
        {:layout :pairs, :open (str "#:" ns-str "{"), :close "}",
         :entries (mapv (fn [[k v]] [(strip-ns k) v]) form)})
      {:layout :pairs, :open "{", :close "}",
       :entries (vec form)})

    ;; Set — use :meme/order for insertion-order output when available
    (set? form)
    (let [elements (or (:meme/order (meta form)) (seq form))]
      {:layout :wrap, :open "#{", :close "}", :children (vec (or elements []))})

    ;; Symbol
    (symbol? form) {:layout :atom, :text (str form)}

    ;; Keyword
    (keyword? form)
    {:layout :atom, :text (if (namespace form)
                            (str ":" (namespace form) "/" (name form))
                            (str ":" (name form)))}

    ;; String
    (string? form) {:layout :atom, :text (pr-str form)}

    ;; Regex
    (instance? #?(:clj java.util.regex.Pattern :cljs js/RegExp) form)
    (let [raw #?(:clj (.pattern ^java.util.regex.Pattern form) :cljs (.-source form))]
      {:layout :atom,
       :text (str "#\"" (str/replace raw #"\\.|\"" (fn [m] (if (= m "\"") "\\\"" m))) "\"")})

    ;; Char (JVM/Babashka only — ClojureScript has no char type)
    #?@(:clj [(char? form)
              (let [named {(char 10) "newline" (char 13) "return" (char 9) "tab"
                           (char 32) "space" (char 8) "backspace" (char 12) "formfeed"}]
                {:layout :atom, :text (if-let [n (get named form)]
                                        (str \\ n)
                                        (str \\ form))})])

    ;; Number — preserve BigDecimal M and BigInt N suffixes, symbolic values
    #?@(:clj [(decimal? form) {:layout :atom, :text (str form "M")}
              (instance? clojure.lang.BigInt form) {:layout :atom, :text (str form "N")}
              (instance? java.math.BigInteger form) {:layout :atom, :text (str form "N")}])
    (and (number? form)
         #?(:clj (Double/isNaN (double form))
            :cljs (js/isNaN form)))
    {:layout :atom, :text "##NaN"}
    (and (number? form)
         #?(:clj (Double/isInfinite (double form))
            :cljs (and (not (js/isFinite form)) (not (js/isNaN form)))))
    {:layout :atom, :text (if (pos? (double form)) "##Inf" "##-Inf")}
    (number? form) {:layout :atom, :text (str form)}

    ;; Tagged literal (JVM only — resolved at read time in ClojureScript)
    #?@(:clj [(tagged-literal? form)
              {:layout :prefix, :prefix (str "#" (.-tag form) " "), :child (.-form form)}])

    ;; Fallback
    :else {:layout :atom, :text (pr-str form)}))

;; ---------------------------------------------------------------------------
;; Flat rendering — layout descriptor → string
;; ---------------------------------------------------------------------------

(defn- render-flat
  "Render a layout descriptor as a flat (single-line) string."
  [{:keys [layout] :as desc}]
  (case layout
    :atom (:text desc)

    :prefix (str (:prefix desc) (print-form (:child desc)))

    :call (let [head (:head desc)]
            ;; Non-callable heads in :meme mode
            (when (and (= *mode* :meme) (contains? #{nil true false} head))
              (throw (ex-info (str "Cannot print list with " (pr-str head)
                                   " as head — not representable in meme syntax")
                              {:head head})))
            (if (= *mode* :clj)
              (str "(" (print-form head) (when (seq (:args desc)) (str " " (print-args (:args desc)))) ")")
              (str (print-form head) "(" (print-args (:args desc)) ")")))

    :wrap (str (:open desc) (str/join " " (map print-form (:children desc))) (:close desc))

    :pairs (let [pair-strs (map (fn [[k v]] (str (print-form k) " " (print-form v)))
                                (:entries desc))]
             (str (:open desc) (str/join " " pair-strs) (:close desc)))

    :meta (str (str/join " " (:prefixes desc)) " " (print-form (:child desc)))))

;; ---------------------------------------------------------------------------
;; Main dispatch
;; ---------------------------------------------------------------------------

(defn print-form
  "Print a single Clojure form as meme text."
  [form]
  (render-flat (decompose form)))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn print-meme-string
  "Print Clojure forms as meme text."
  [forms]
  (str/join "\n\n" (map print-form forms)))

(defn print-clj-string
  "Print Clojure forms as Clojure text with reader sugar ('quote, @deref, #'var)."
  [forms]
  (binding [*mode* :clj]
    (str/join "\n\n" (map print-form forms))))
