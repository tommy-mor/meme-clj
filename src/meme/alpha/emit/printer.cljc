(ns meme.alpha.emit.printer
  "Wadler-Lindig pretty-printer for meme.
   Single pass: form → Doc tree → layout → string.
   Handles both flat (print-form) and width-aware (pprint) rendering."
  (:require [clojure.string :as str]
            [meme.alpha.forms :as forms]))

;; ---------------------------------------------------------------------------
;; Doc types — Lindig's strict-language variant of Wadler's algebra
;; ---------------------------------------------------------------------------

(defrecord DocText [s])
(defrecord DocLine [flat-alt])     ; nil flat-alt = hardline (always breaks)
(defrecord DocCat [a b])
(defrecord DocNest [indent doc])
(defrecord DocGroup [doc])
(defrecord DocIfBreak [break-doc flat-doc])

;; ---------------------------------------------------------------------------
;; Smart constructors
;; ---------------------------------------------------------------------------

(def doc-line  (->DocLine " "))    ; space when flat, newline+indent when broken
(def doc-line0 (->DocLine ""))     ; empty when flat, newline+indent when broken
(def doc-hardline (->DocLine nil)) ; always breaks — never flat

(defn- doc-text [s]
  (when (and s (not= s ""))
    (->DocText s)))

(defn- doc-nest [i doc]
  (when doc (->DocNest i doc)))

(defn- doc-group [doc]
  (when doc (->DocGroup doc)))

(defn- doc-if-break [break-doc flat-doc]
  (->DocIfBreak break-doc flat-doc))

(defn- doc-cat
  ([] nil)
  ([a] a)
  ([a b] (cond (nil? a) b (nil? b) a :else (->DocCat a b)))
  ([a b & more] (reduce doc-cat (doc-cat a b) more)))

;; ---------------------------------------------------------------------------
;; Portable string builder (same pattern as tokenizer)
;; ---------------------------------------------------------------------------

(defn- make-sb [] #?(:clj (StringBuilder.) :cljs #js []))

(defn- sb-append! [sb x]
  #?(:clj (.append ^StringBuilder sb ^String x) :cljs (.push sb x))
  sb)

(defn- sb-str [sb]
  #?(:clj (.toString sb) :cljs (.join sb "")))

(defn- indent-str [n]
  (apply str (repeat n \space)))

;; ---------------------------------------------------------------------------
;; Layout engine — Lindig's format algorithm
;;
;; Work-list items: [indent mode doc]
;;   indent — current indentation level (absolute)
;;   mode   — :flat or :break
;;   doc    — Doc node to process
;; ---------------------------------------------------------------------------

(defn- fits?
  "Does the first line of the work-list fit in `remaining` columns?
   Returns true at any line break (rest is on the next line)."
  [remaining work]
  (loop [remaining remaining
         work work]
    (cond
      (neg? remaining) false
      (empty? work) true
      :else
      (let [[i mode doc] (first work)
            rest-work (subvec work 1)]
        (condp instance? doc
          DocText   (recur (- remaining (count (:s doc))) rest-work)
          DocLine   (if (= mode :flat)
                      (if (nil? (:flat-alt doc))
                        false ; hardline never fits flat
                        (recur (- remaining (count (:flat-alt doc))) rest-work))
                      true) ; break mode line → always fits (rest on next line)
          DocCat    (recur remaining (into [[i mode (:a doc)] [i mode (:b doc)]] rest-work))
          DocNest   (recur remaining (into [[(+ i (:indent doc)) mode (:doc doc)]] rest-work))
          DocGroup  (recur remaining (into [[i :flat (:doc doc)]] rest-work))
          DocIfBreak (if (= mode :flat)
                       (if (:flat-doc doc)
                         (recur remaining (into [[i :flat (:flat-doc doc)]] rest-work))
                         (recur remaining rest-work))
                       (if (:break-doc doc)
                         (recur remaining (into [[i :break (:break-doc doc)]] rest-work))
                         (recur remaining rest-work)))
          ;; nil doc (from doc-cat filtering)
          (recur remaining rest-work))))))

(defn layout
  "Render a Doc tree as a string at the given page width.
   Use ##Inf for flat (single-line) rendering."
  [doc width]
  (let [sb (make-sb)]
    (loop [col 0
           work [[0 :break doc]]]
      (if (empty? work)
        (sb-str sb)
        (let [[i mode d] (first work)
              rest-work (subvec work 1)]
          (if (nil? d)
            (recur col rest-work)
            (condp instance? d
              DocText
              (let [s (:s d)]
                (sb-append! sb s)
                (recur (+ col (count s)) rest-work))

              DocLine
              (if (and (= mode :flat) (some? (:flat-alt d)))
                ;; Flat mode: emit flat-alt
                (let [alt (:flat-alt d)]
                  (when (not= alt "") (sb-append! sb alt))
                  (recur (+ col (count alt)) rest-work))
                ;; Break mode (or hardline in flat — shouldn't happen but handle safely)
                (let [indent-s (indent-str i)]
                  (sb-append! sb "\n")
                  (sb-append! sb indent-s)
                  (recur i rest-work)))

              DocCat
              (recur col (into [[i mode (:a d)] [i mode (:b d)]] rest-work))

              DocNest
              (recur col (into [[(+ i (:indent d)) mode (:doc d)]] rest-work))

              DocGroup
              (let [flat-work (into [[i :flat (:doc d)]] rest-work)]
                (if (fits? (- width col) flat-work)
                  (recur col flat-work)
                  (recur col (into [[i :break (:doc d)]] rest-work))))

              DocIfBreak
              (if (= mode :flat)
                (if (:flat-doc d)
                  (recur col (into [[i :flat (:flat-doc d)]] rest-work))
                  (recur col rest-work))
                (if (:break-doc d)
                  (recur col (into [[i :break (:break-doc d)]] rest-work))
                  (recur col rest-work)))

              ;; Unknown — skip
              (recur col rest-work))))))))

;; ---------------------------------------------------------------------------
;; Comment extraction from :ws metadata
;; ---------------------------------------------------------------------------

(defn- extract-comments
  "Extract comment lines from a :ws metadata string.
   Returns a vector of trimmed comment strings, or nil."
  [ws]
  (when ws
    (let [lines (str/split-lines ws)]
      (not-empty (mapv str/triml (filterv #(re-find #"^\s*;" %) lines))))))

(defn- form-comments
  "Get comment lines from a form's :ws metadata, or nil."
  [form]
  (when (and (some? form)
             #?(:clj  (instance? clojure.lang.IMeta form)
                :cljs (satisfies? IMeta form))
             (meta form))
    (extract-comments (:ws (meta form)))))

(defn- comment-doc
  "Build a Doc that emits comment lines followed by a hardline.
   Each comment line is on its own line."
  [comments]
  (reduce (fn [acc c]
            (doc-cat acc (doc-text c) doc-hardline))
          nil
          comments))

;; ---------------------------------------------------------------------------
;; Notation helpers
;; ---------------------------------------------------------------------------

(def ^:private head-line-args
  "How many args to keep on the first line with the head.
   nil means no special treatment (default: all args in body)."
  {'def 1, 'def- 1,
   'defn 1, 'defn- 1, 'defmacro 1, 'defmulti 1, 'defmethod 2,
   'defprotocol 1, 'defrecord 1, 'deftype 1,
   'fn 0,
   'let 0, 'loop 0, 'binding 0, 'doseq 0, 'for 0,
   'if 1, 'if-not 1, 'if-let 0, 'if-some 0,
   'when 1, 'when-not 1, 'when-let 0, 'when-some 0, 'when-first 0,
   'cond 0, 'condp 2, 'case 1, 'cond-> 1, 'cond->> 1,
   'try 0, 'catch 2, 'finally 0,
   'do 0,
   'ns 1,
   '-> 1, '->> 1, 'some-> 1, 'some->> 1, 'as-> 2,
   'deftest 1, 'testing 1, 'is 0, 'are 0})

(defn- anon-fn-shorthand?
  "Can (fn [params] body) be printed as #(body)?
   Only when :meme/sugar tagged by reader."
  [form]
  (and (:meme/sugar (meta form))
       (seq? form)
       (= 'fn (first form))
       (= 3 (count form))
       (vector? (second form))))

;; ---------------------------------------------------------------------------
;; Doc helpers for building common structures
;; ---------------------------------------------------------------------------

(defn- intersperse
  "Interleave docs with separator, returning concatenated Doc."
  [sep docs]
  (when (seq docs)
    (reduce (fn [acc d] (doc-cat acc sep d)) (first docs) (rest docs))))

;; ---------------------------------------------------------------------------
;; Form → Doc — single source of truth for notation AND formatting
;; ---------------------------------------------------------------------------

(declare to-doc to-doc-form)

(defn- emit-meta-prefix
  "Compute metadata prefix string: ^:key, ^Type, or ^{map}."
  [m mode]
  (cond
    (and (= 1 (count m))
         (keyword? (key (first m)))
         (true? (val (first m))))
    (str "^" (layout (to-doc-form (key (first m)) mode) ##Inf))
    (and (= 1 (count m))
         (contains? m :tag)
         (symbol? (:tag m)))
    (str "^" (layout (to-doc-form (:tag m) mode) ##Inf))
    :else
    (str "^" (layout (to-doc-form m mode) ##Inf))))

(defn- call-doc
  "Build Doc for a call form. Handles head-line-args and meme/clj modes."
  [head args mode]
  (let [head-doc (to-doc head mode)
        arg-docs (mapv #(to-doc % mode) args)]
    (if (= mode :clj)
      ;; Clojure mode: (head arg1 arg2)
      (if (empty? arg-docs)
        (doc-group (doc-cat (doc-text "(") head-doc (doc-text ")")))
        (doc-group
          (doc-cat
            (doc-text "(") head-doc
            (doc-nest 2 (doc-cat doc-line (intersperse doc-line arg-docs)))
            (doc-text ")"))))
      ;; Meme mode: head(arg1 arg2) with head-line-args
      (do
        ;; Non-callable heads
        (when (contains? #{nil true false} head)
          (throw (ex-info (str "Cannot print list with " (pr-str head)
                               " as head — not representable in meme syntax")
                          {:head head})))
        (let [n-head (get head-line-args head)]
          (cond
            ;; Zero args: head()
            (empty? arg-docs)
            (doc-group (doc-cat head-doc (doc-text "(")(doc-text ")")))

            ;; Head-line args: keep n args on head line, rest in body
            (and n-head (pos? n-head) (> (count arg-docs) n-head))
            (let [hl (subvec arg-docs 0 n-head)
                  body (subvec arg-docs n-head)]
              (doc-group
                (doc-cat
                  head-doc (doc-text "(")
                  (doc-nest 2
                    (doc-cat
                      (doc-group (doc-cat doc-line0 (intersperse doc-line hl)))
                      (reduce (fn [acc d] (doc-cat acc doc-line d)) nil body)))
                  (doc-text ")"))))

            ;; Default: all args in body
            :else
            (doc-group
              (doc-cat
                head-doc (doc-text "(")
                (doc-nest 2 (doc-cat doc-line0 (intersperse doc-line arg-docs)))
                (doc-text ")")))))))))

(defn- collection-doc
  "Build Doc for a delimited collection: [elems], #{elems}, #(body)."
  [open close children mode]
  (if (empty? children)
    (doc-text (str open close))
    (let [child-docs (mapv #(to-doc % mode) children)]
      (doc-group
        (doc-cat
          (doc-text open)
          (doc-nest 2 (doc-cat doc-line0 (intersperse doc-line child-docs)))
          doc-line0
          (doc-text close))))))

(defn- pairs-doc
  "Build Doc for key-value pairs: {k v ...}, #:ns{k v ...}, #?(k v ...)."
  [open close entries mode]
  (if (empty? entries)
    (doc-text (str open close))
    (let [pair-docs (mapv (fn [[k v]]
                            (doc-cat (to-doc k mode) (doc-text " ") (to-doc v mode)))
                          entries)]
      (doc-group
        (doc-cat
          (doc-text open)
          (doc-nest 2 (doc-cat doc-line0 (intersperse doc-line pair-docs)))
          doc-line0
          (doc-text close))))))

(defn- to-doc-form
  "Convert a Clojure form to a Doc tree. Handles metadata wrapping.
   mode is :meme (default) or :clj."
  [form mode]
  (cond
    ;; Metadata prefix — checked first, before structural checks.
    (and (some? form)
         #?(:clj (instance? clojure.lang.IMeta form)
            :cljs (satisfies? IMeta form))
         (some? (meta form))
         (seq (forms/strip-internal-meta (meta form))))
    (let [chain (:meme/meta-chain (meta form))
          stripped (with-meta form nil)
          prefixes (if chain
                     (mapv #(emit-meta-prefix % mode) (reverse chain))
                     [(emit-meta-prefix (forms/strip-internal-meta (meta form)) mode)])]
      (doc-cat (doc-text (str (str/join " " prefixes) " ")) (to-doc-form stripped mode)))

    ;; Raw value wrapper — emit original source text
    (forms/raw? form) (doc-text (:raw form))

    ;; nil
    (nil? form) (doc-text "nil")

    ;; boolean
    (boolean? form) (doc-text (str form))

    ;; Deferred auto-resolve keywords
    (forms/deferred-auto-keyword? form)
    (doc-text (forms/deferred-auto-keyword-raw form))

    ;; Empty list
    (and (seq? form) (empty? form))
    (doc-text "()")

    ;; Anon-fn shorthand #()
    (anon-fn-shorthand? form)
    (collection-doc "#(" ")" [(nth form 2)] mode)

    ;; Sequences — check sugar then call
    (seq? form)
    (let [head (first form)]
      (cond
        ;; @deref sugar
        (and (= head 'clojure.core/deref) (:meme/sugar (meta form)))
        (doc-cat (doc-text "@") (to-doc (second form) mode))

        ;; 'quote sugar
        (and (= head 'quote) (:meme/sugar (meta form)))
        (doc-cat (doc-text "'") (to-doc (second form) mode))

        ;; #'var sugar
        (and (= head 'var) (:meme/sugar (meta form)))
        (doc-cat (doc-text "#'") (to-doc (second form) mode))

        ;; Regular call
        :else
        (call-doc head (vec (rest form)) mode)))

    ;; Syntax-quote / unquote / unquote-splicing AST nodes
    ;; Must be before map? (defrecords satisfy map?)
    (forms/syntax-quote? form)
    (doc-cat (doc-text "`") (to-doc (:form form) mode))

    (forms/unquote? form)
    (doc-cat (doc-text "~") (to-doc (:form form) mode))

    (forms/unquote-splicing? form)
    (doc-cat (doc-text "~@") (to-doc (:form form) mode))

    ;; Reader conditional — must be before map?
    (forms/meme-reader-conditional? form)
    (let [prefix (if (forms/rc-splicing? form) "#?@(" "#?(")]
      (pairs-doc prefix ")" (vec (partition 2 (forms/rc-form form))) mode))

    ;; Vector
    (vector? form)
    (collection-doc "[" "]" (vec form) mode)

    ;; Map — reconstruct #:ns{} when :meme/ns metadata present
    (map? form)
    (if-let [ns-str (:meme/ns (meta form))]
      (let [strip-ns (fn [k]
                       (if (and (keyword? k)
                                (= (namespace k)
                                   (if (str/starts-with? ns-str ":") (subs ns-str 1) ns-str)))
                         (keyword (name k))
                         k))]
        (pairs-doc (str "#:" ns-str "{") "}" (mapv (fn [[k v]] [(strip-ns k) v]) form) mode))
      (pairs-doc "{" "}" (vec form) mode))

    ;; Set — use :meme/order for insertion-order output
    (set? form)
    (let [elements (or (:meme/order (meta form)) (seq form))]
      (collection-doc "#{" "}" (vec (or elements [])) mode))

    ;; Symbol
    (symbol? form) (doc-text (str form))

    ;; Keyword
    (keyword? form)
    (doc-text (if (namespace form)
                (str ":" (namespace form) "/" (name form))
                (str ":" (name form))))

    ;; String
    (string? form) (doc-text (pr-str form))

    ;; Regex
    (instance? #?(:clj java.util.regex.Pattern :cljs js/RegExp) form)
    (let [raw #?(:clj (.pattern ^java.util.regex.Pattern form) :cljs (.-source form))]
      (doc-text (str "#\"" (str/replace raw #"\\.|\"" (fn [m] (if (= m "\"") "\\\"" m))) "\"")))

    ;; Char (JVM only)
    #?@(:clj [(char? form)
              (let [named {(char 10) "newline" (char 13) "return" (char 9) "tab"
                           (char 32) "space" (char 8) "backspace" (char 12) "formfeed"}]
                (doc-text (if-let [n (get named form)]
                            (str \\ n)
                            (str \\ form))))])

    ;; Number — preserve BigDecimal M and BigInt N suffixes, symbolic values
    #?@(:clj [(decimal? form) (doc-text (str form "M"))
              (instance? clojure.lang.BigInt form) (doc-text (str form "N"))
              (instance? java.math.BigInteger form) (doc-text (str form "N"))])
    (and (number? form)
         #?(:clj (Double/isNaN (double form))
            :cljs (js/isNaN form)))
    (doc-text "##NaN")
    (and (number? form)
         #?(:clj (Double/isInfinite (double form))
            :cljs (and (not (js/isFinite form)) (not (js/isNaN form)))))
    (doc-text (if (pos? (double form)) "##Inf" "##-Inf"))
    (number? form) (doc-text (str form))

    ;; Tagged literal (JVM only)
    #?@(:clj [(tagged-literal? form)
              (doc-cat (doc-text (str "#" (.-tag form) " ")) (to-doc (.-form form) mode))])

    ;; Fallback
    :else (doc-text (pr-str form))))

(defn to-doc
  "Convert a Clojure form to a Doc tree, with comment attachment.
   Comments from :ws metadata are emitted only in break context (via DocIfBreak).
   mode is :meme (default) or :clj."
  ([form] (to-doc form :meme))
  ([form mode]
   (let [doc (to-doc-form form mode)
         comments (form-comments form)]
     (if comments
       (doc-if-break
         (doc-cat (comment-doc comments) doc) ; break: comments + form
         doc)                                  ; flat: just form
       doc))))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn print-form
  "Print a single Clojure form as meme text (flat, single-line)."
  [form]
  (layout (to-doc form :meme) ##Inf))

(defn print-meme-string
  "Print Clojure forms as meme text."
  [forms]
  (str/join "\n\n" (map print-form forms)))

(defn print-clj-string
  "Print Clojure forms as Clojure text with reader sugar ('quote, @deref, #'var)."
  [forms]
  (str/join "\n\n" (map #(layout (to-doc % :clj) ##Inf) forms)))
