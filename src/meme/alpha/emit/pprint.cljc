(ns meme.alpha.emit.pprint
  "Pretty-printer: Clojure forms → idiomatic multi-line meme text.
   Width-aware layout engine built on printer/decompose — notation decisions
   (sugar, namespaced maps, metadata chains) live in one place."
  (:require [meme.alpha.emit.printer :as printer]
            [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Configuration
;; ---------------------------------------------------------------------------

(def ^:private default-width 80)
(def ^:private indent-step 2)

;; ---------------------------------------------------------------------------
;; Core: width-aware recursive formatter
;; ---------------------------------------------------------------------------

(declare pp)

(defn- flat
  "Single-line representation of a form (delegates to existing printer)."
  [form]
  (printer/print-form form))

(defn- indent-str
  "String of n spaces."
  [n]
  (apply str (repeat n \space)))

;; ---------------------------------------------------------------------------
;; Call formatting — keeps leading args on head line when appropriate
;; ---------------------------------------------------------------------------

(def ^:private head-line-args
  "How many args to keep on the first line with the head.
   nil means no special treatment (default: break all to body)."
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

(defn- pp-call
  "Pretty-print a call, keeping leading args with the head when appropriate.
   head and args come from the decompose descriptor."
  [head args col width]
  (let [head-str (flat head)
        n-head-args (get head-line-args head)
        [head-args body-args]
        (if (and n-head-args (pos? n-head-args) (> (count args) n-head-args))
          [(take n-head-args args) (drop n-head-args args)]
          [nil args])

        inner-col (+ col indent-step)
        inner-indent (indent-str inner-col)]

    (if head-args
      ;; Some args stay on the head line — but only if they fit
      (let [head-args-str (str/join " " (map flat head-args))
            first-line (str head-str "(" head-args-str)]
        (if (<= (+ col (count first-line)) width)
          ;; Head-line args fit: head(name [params]\n  body)
          (let [pp-body (map #(pp % inner-col width) body-args)
                body (str/join (str "\n" inner-indent) pp-body)]
            (str first-line "\n"
                 inner-indent body ")"))
          ;; Head-line args don't fit: fall back to all-in-body
          (let [pp-args (map #(pp % inner-col width) (concat head-args body-args))
                body (str/join (str "\n" inner-indent) pp-args)]
            (str head-str "(\n"
                 inner-indent body ")"))))

      ;; All args in body
      (let [pp-args (map #(pp % inner-col width) body-args)
            body (str/join (str "\n" inner-indent) pp-args)]
        (str head-str "(\n"
             inner-indent body ")")))))

;; ---------------------------------------------------------------------------
;; Collection formatting — generic wrap and pairs
;; ---------------------------------------------------------------------------

(defn- pp-wrap
  "Pretty-print a delimited sequence (vector, set, #(), etc.).
   open/close/children come from the decompose descriptor."
  [{:keys [open close children]} col width]
  (let [inner-col (+ col indent-step)
        inner-indent (indent-str inner-col)
        outer-indent (indent-str col)
        elems (map #(pp % inner-col width) children)]
    (str open "\n"
         inner-indent (str/join (str "\n" inner-indent) elems) "\n"
         outer-indent close)))

(defn- pp-pairs
  "Pretty-print key-value pairs (map, #:ns{}, reader conditional, etc.).
   open/close/entries come from the decompose descriptor."
  [{:keys [open close entries]} col width]
  (let [inner-col (+ col indent-step)
        inner-indent (indent-str inner-col)
        outer-indent (indent-str col)
        formatted-entries
        (map (fn [[k v]]
               (let [pp-k (pp k inner-col width)
                     ;; Value column: after the key's last line + space.
                     ;; Single-line keys lack indent (it's added by the
                     ;; join), so we add inner-col. Multi-line keys have
                     ;; indentation baked into the last line by pp.
                     last-line (peek (str/split-lines pp-k))
                     multi-line? (not= last-line pp-k)
                     val-col (if multi-line?
                               (+ (count last-line) 1)
                               (+ inner-col (count last-line) 1))]
                 (str pp-k " " (pp v val-col width))))
             entries)]
    (str open "\n"
         inner-indent (str/join (str "\n" inner-indent) formatted-entries) "\n"
         outer-indent close)))

;; ---------------------------------------------------------------------------
;; Comment extraction from :ws metadata
;; ---------------------------------------------------------------------------

(defn- extract-comments
  "Extract comment lines from a :ws metadata string.
   Returns a vector of comment strings (with leading ; intact), or nil."
  [ws]
  (when ws
    (let [lines (str/split-lines ws)]
      (not-empty (filterv #(re-find #"^\s*;" %) lines)))))

(defn- form-comments
  "Get comment lines from a form's :ws metadata, or nil."
  [form]
  (when (and (some? form)
             #?(:clj  (instance? clojure.lang.IMeta form)
                :cljs (satisfies? IMeta form))
             (meta form))
    (extract-comments (:ws (meta form)))))

;; ---------------------------------------------------------------------------
;; Main dispatch — decompose then layout
;; ---------------------------------------------------------------------------

(defn- pp
  "Pretty-print a form at the given column and width.
   Uses printer/decompose for notation decisions, handles layout here."
  [form col width]
  (let [comments (form-comments form)
        indent (indent-str col)
        flat-str (flat form)
        formatted
        (if (<= (+ col (count flat-str)) width)
          ;; Fits flat — use single-line representation
          flat-str
          ;; Doesn't fit — decompose and render multi-line
          (let [{:keys [layout] :as desc} (printer/decompose form)]
            (case layout
              :atom (:text desc)

              :prefix (str (:prefix desc)
                           (pp (:child desc) (+ col (count (:prefix desc))) width))

              :call (pp-call (:head desc) (:args desc) col width)

              :wrap (pp-wrap desc col width)

              :pairs (pp-pairs desc col width)

              :meta (let [prefix-str (str/join " " (:prefixes desc))
                          prefix-len (inc (count prefix-str))]
                      (str prefix-str " "
                           (pp (:child desc) (+ col prefix-len) width))))))]
    (if comments
      ;; First comment line: no indent (caller provides it via join/concat).
      ;; Subsequent comment lines: indent to current column.
      ;; All lines: strip original whitespace — pprint re-indents.
      (let [stripped (map str/triml comments)
            indented (cons (first stripped)
                           (map #(str indent %) (rest stripped)))]
        (str (str/join "\n" indented) "\n" indent formatted))
      formatted)))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn pprint-form
  "Pretty-print a single Clojure form as meme text.
   Preserves comments from :ws metadata.
   opts: {:width 80}"
  ([form] (pprint-form form nil))
  ([form opts]
   (let [width (or (:width opts) default-width)]
     (pp form 0 width))))

(defn pprint-forms
  "Pretty-print a sequence of Clojure forms as meme text,
   separated by blank lines. Preserves comments from :ws metadata.
   opts: {:width 80}"
  ([forms] (pprint-forms forms nil))
  ([forms opts]
   (let [trailing-ws (:trailing-ws (meta forms))
         trailing-comments (when trailing-ws
                             (extract-comments trailing-ws))
         body (str/join "\n\n" (map #(pprint-form % opts) forms))]
     (if trailing-comments
       (str body "\n\n" (str/join "\n" trailing-comments))
       body))))
