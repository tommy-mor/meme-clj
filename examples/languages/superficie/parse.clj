(ns superficie.parse
  "Recursive-descent parser for superficie syntax.
   Parses .sup files into Clojure forms. No dependencies beyond Clojure.

   Plugs into meme's pipeline as a custom parser via the registry.
   Does NOT use meme's tokenizer — superficie has its own lexical rules
   (infix operators, whitespace-sensitive minus, block delimiters)."
  (:require [clojure.string :as str]))

;; ============================================================
;; Scanner
;; ============================================================

(defn- make-scanner [s]
  {:src s :pos (volatile! 0) :line (volatile! 1) :col (volatile! 1)})

(defn- seof? [sc] (>= @(:pos sc) (count (:src sc))))
(defn- speek [sc] (when-not (seof? sc) (.charAt ^String (:src sc) @(:pos sc))))
(defn- speek-str [sc n]
  (let [p @(:pos sc) end (min (+ p n) (count (:src sc)))]
    (subs (:src sc) p end)))

(defn- sadvance! [sc]
  (let [ch (speek sc)]
    (vswap! (:pos sc) inc)
    (if (= ch \newline)
      (do (vswap! (:line sc) inc) (vreset! (:col sc) 1))
      (vswap! (:col sc) inc))
    ch))

(defn- skip-ws! [sc]
  (loop []
    (when-not (seof? sc)
      (let [ch (speek sc)]
        (cond
          (#{\space \tab \, \newline \return} ch)
          (do (sadvance! sc) (recur))

          (= ch \;)
          (do (loop [] (when (and (not (seof? sc)) (not= (speek sc) \newline))
                         (sadvance! sc) (recur)))
              (recur))

          :else nil)))))

(defn- skip-sp! [sc]
  "Skip spaces and tabs only (not newlines, not commas)."
  (loop []
    (when (and (not (seof? sc)) (#{\space \tab} (speek sc)))
      (sadvance! sc) (recur))))

(defn- match-str! [sc s]
  (let [peeked (speek-str sc (count s))]
    (when (= peeked s)
      ;; Check word boundary for alphabetic tokens
      (let [end-pos (+ @(:pos sc) (count s))
            next-ch (when (< end-pos (count (:src sc)))
                      (.charAt ^String (:src sc) end-pos))]
        (when (or (nil? next-ch)
                  (not (Character/isLetterOrDigit ^char next-ch))
                  (not (Character/isLetter ^char (last s))))
          (dotimes [_ (count s)] (sadvance! sc))
          s)))))

(defn- read-while! [sc pred]
  (let [sb (StringBuilder.)]
    (loop []
      (when (and (not (seof? sc)) (pred (speek sc)))
        (.append sb (sadvance! sc))
        (recur)))
    (.toString sb)))

(defn- read-string-lit! [sc]
  (sadvance! sc) ;; skip opening "
  (let [sb (StringBuilder. "\"")]
    (loop []
      (when (seof? sc) (throw (ex-info "Unterminated string" {})))
      (let [ch (sadvance! sc)]
        (.append sb ch)
        (cond
          (= ch \") (.toString sb)
          (= ch \\) (do (.append sb (sadvance! sc)) (recur))
          :else (recur))))))

(defn- read-regex-lit! [sc]
  (sadvance! sc) ;; skip #
  (sadvance! sc) ;; skip "
  (let [sb (StringBuilder.)]
    (loop []
      (when (seof? sc) (throw (ex-info "Unterminated regex" {})))
      (let [ch (sadvance! sc)]
        (cond
          (= ch \") (re-pattern (.toString sb))
          (= ch \\) (do (.append sb ch) (.append sb (sadvance! sc)) (recur))
          :else (do (.append sb ch) (recur)))))))

;; ============================================================
;; Token predicates
;; ============================================================

(def ^:private sym-start? #(or (Character/isLetter ^char %)
                               (#{\_ \* \! \? \$ \> \< \= \& \'} %)))
(def ^:private sym-char? #(or (Character/isLetterOrDigit ^char %)
                              (#{\_ \* \+ \! \- \? \/ \$ \# \> \< \= \% \& \' \.} %)))
(def ^:private digit? #(Character/isDigit ^char %))

(def ^:private reserved-words
  #{"def" "defonce" "defn" "defn-" "defmacro" "fn"
    "if" "if-not" "if-let" "if-some"
    "when" "when-not" "when-let" "when-some" "when-first" "while"
    "let" "binding" "with-open" "with-redefs"
    "loop" "cond" "condp" "case" "do" "try" "catch" "finally"
    "for" "doseq" "dotimes"
    "end" "else" "new" "not" "throw" "recur"
    "ns" "nil" "true" "false" "and" "or" "mod" "rem" "in"
    "e/defn"})

;; ============================================================
;; Parser — recursive descent with precedence climbing
;; ============================================================

(declare parse-expr parse-primary parse-body)

(defn- read-symbol! [sc]
  (let [sb (StringBuilder.)
        _ (when (= (speek sc) \-)
            (.append sb (sadvance! sc))
            (when (and (not (seof? sc)) (= (speek sc) \>))
              (.append sb (sadvance! sc))
              (when (and (not (seof? sc)) (= (speek sc) \>))
                (.append sb (sadvance! sc)))))]
    (loop []
      (when (and (not (seof? sc)) (sym-char? (speek sc)))
        (.append sb (sadvance! sc))
        (recur)))
    (let [s (.toString sb)]
      (symbol s))))

(defn- read-number! [sc]
  (let [s (read-while! sc #(or (digit? %) (#{\. \x \X \/ \E \e \N \M \- \+ \r} %)
                                (Character/isLetter ^char %)))]
    (cond
      (re-matches #"0[xX][0-9a-fA-F]+" s) (Long/parseLong (subs s 2) 16)
      (str/ends-with? s "N") (bigint (subs s 0 (dec (count s))))
      (str/ends-with? s "M") (bigdec (subs s 0 (dec (count s))))
      (str/includes? s "/") (let [[n d] (str/split s #"/")] (/ (Long/parseLong n) (Long/parseLong d)))
      (str/includes? s ".") (Double/parseDouble s)
      :else (Long/parseLong s))))

(defn- read-keyword! [sc]
  (sadvance! sc) ;; skip :
  (let [s (read-while! sc #(or (sym-char? %) (= % \:)))]
    (if (str/includes? s "/")
      (let [[ns n] (str/split s #"/" 2)]
        (keyword ns n))
      (keyword s))))

(defn- read-char-lit! [sc]
  (sadvance! sc) ;; skip \
  (let [s (read-while! sc #(Character/isLetterOrDigit ^char %))]
    (case s
      "newline" \newline "space" \space "tab" \tab
      "backspace" \backspace "formfeed" \formfeed "return" \return
      (if (= 1 (count s)) (first s)
          (if (str/starts-with? s "u")
            (char (Integer/parseInt (subs s 1) 16))
            (first s))))))

(defn- parse-call-args [sc]
  (sadvance! sc) ;; skip (
  (skip-ws! sc)
  (if (= (speek sc) \))
    (do (sadvance! sc) [])
    (loop [args [(parse-expr sc)]]
      (skip-ws! sc)
      (if (= (speek sc) \))
        (do (sadvance! sc) args)
        (do (when (= (speek sc) \,) (sadvance! sc))
            (skip-ws! sc)
            (recur (conj args (parse-expr sc))))))))

(defn- parse-vector [sc]
  (sadvance! sc) ;; skip [
  (skip-ws! sc)
  (if (= (speek sc) \])
    (do (sadvance! sc) [])
    (loop [items [(parse-expr sc)]]
      (skip-ws! sc)
      (cond
        (= (speek sc) \]) (do (sadvance! sc) items)
        (= (speek sc) \&) (do (sadvance! sc) (skip-ws! sc)
                              (let [rest-sym (parse-primary sc)]
                                (skip-ws! sc) (sadvance! sc) ;; ]
                                (into items ['& rest-sym])))
        :else (do (when (= (speek sc) \,) (sadvance! sc))
                  (skip-ws! sc) (recur (conj items (parse-expr sc))))))))

(defn- parse-map [sc]
  (sadvance! sc) ;; skip {
  (skip-ws! sc)
  (if (= (speek sc) \})
    (do (sadvance! sc) {})
    (loop [m {}]
      (skip-ws! sc)
      (let [k (parse-expr sc)
            _ (skip-ws! sc)
            v (parse-expr sc)]
        (skip-ws! sc)
        (let [m (assoc m k v)]
          (if (= (speek sc) \})
            (do (sadvance! sc) m)
            (do (when (= (speek sc) \,) (sadvance! sc))
                (recur m))))))))

(defn- parse-set [sc]
  (sadvance! sc) ;; skip #
  (sadvance! sc) ;; skip {
  (skip-ws! sc)
  (if (= (speek sc) \})
    (do (sadvance! sc) #{})
    (loop [items #{}]
      (skip-ws! sc)
      (let [item (parse-expr sc)]
        (skip-ws! sc)
        (let [items (conj items item)]
          (if (= (speek sc) \})
            (do (sadvance! sc) items)
            (do (when (= (speek sc) \,) (sadvance! sc))
                (recur items))))))))

(defn- parse-sexp [sc]
  "Parse S-expression passthrough: (...) with Clojure inside."
  (sadvance! sc) ;; skip (
  (let [sb (StringBuilder. "(")
        depth (volatile! 1)]
    (loop []
      (when (seof? sc) (throw (ex-info "Unterminated S-expression" {})))
      (let [ch (sadvance! sc)]
        (.append sb ch)
        (cond
          (= ch \() (do (vswap! depth inc) (recur))
          (= ch \)) (do (vswap! depth dec)
                        (if (zero? @depth)
                          (read-string (.toString sb))
                          (recur)))
          (= ch \") (do (loop [] ;; skip string
                          (let [c (sadvance! sc)]
                            (.append sb c)
                            (cond (= c \") nil
                                  (= c \\) (do (.append sb (sadvance! sc)) (recur))
                                  :else (recur))))
                        (recur))
          (= ch \;) (do (loop [] ;; skip comment
                          (when (and (not (seof? sc)) (not= (speek sc) \newline))
                            (.append sb (sadvance! sc)) (recur)))
                        (recur))
          :else (recur))))))

(defn- parse-bindings [sc]
  (loop [bindings []]
    (skip-ws! sc)
    (let [target (parse-primary sc)]
      (skip-ws! sc)
      (match-str! sc ":=")
      (skip-ws! sc)
      (let [val (parse-expr sc)
            bindings (into bindings [target val])]
        (skip-sp! sc)
        (if (= (speek sc) \,)
          (do (sadvance! sc) (recur bindings))
          bindings)))))

(defn- parse-for-bindings [sc]
  (loop [bindings []]
    (skip-ws! sc)
    (let [ch (speek sc)
          peeked (speek-str sc 5)]
      (cond
        ;; :when, :while, :let modifiers
        (and (str/starts-with? peeked "when ") (not (str/starts-with? peeked "when-")))
        (do (match-str! sc "when") (skip-ws! sc)
            (recur (into bindings [:when (parse-expr sc)])))

        (str/starts-with? peeked "while")
        (do (match-str! sc "while") (skip-ws! sc)
            (recur (into bindings [:while (parse-expr sc)])))

        (str/starts-with? peeked "let ")
        (do (match-str! sc "let") (skip-ws! sc)
            (let [target (parse-primary sc)
                  _ (skip-ws! sc) _ (match-str! sc ":=") _ (skip-ws! sc)
                  val (parse-expr sc)]
              (recur (into bindings [:let [target val]]))))

        ;; Normal: target in expr
        :else
        (let [target (parse-primary sc)]
          (skip-ws! sc)
          (match-str! sc "in")
          (skip-ws! sc)
          (let [val (parse-expr sc)
                bindings (into bindings [target val])]
            (skip-sp! sc)
            (if (= (speek sc) \,)
              (do (sadvance! sc) (recur bindings))
              bindings)))))))

(defn- parse-body [sc]
  "Parse body expressions until 'end', 'else', 'catch', 'finally'."
  (loop [forms []]
    (skip-ws! sc)
    (let [peeked (speek-str sc 8)]
      (if (or (str/starts-with? peeked "end")
              (str/starts-with? peeked "else")
              (str/starts-with? peeked "catch")
              (str/starts-with? peeked "finally")
              (seof? sc))
        forms
        ;; let-statement in body?
        (if (and (str/starts-with? peeked "let ")
                 (not (str/starts-with? peeked "let-")))
          (let [_ (match-str! sc "let")
                _ (skip-ws! sc)
                target (parse-primary sc)
                _ (skip-ws! sc) _ (match-str! sc ":=") _ (skip-ws! sc)
                val (parse-expr sc)
                rest-body (parse-body sc)]
            ;; let scopes to end of block
            (conj forms (apply list 'let [target val] rest-body)))
          (recur (conj forms (parse-expr sc))))))))

(defn- parse-block [sc head-sym]
  "Parse a block: expr|bindings : body end"
  (case (str head-sym)
    ("if" "if-not")
    (let [test (parse-expr sc)
          _ (skip-ws! sc) _ (match-str! sc ":") body (parse-body sc)]
      (skip-ws! sc)
      (if (match-str! sc "else")
        (let [_ (skip-ws! sc) _ (match-str! sc ":") else-body (parse-body sc)
              _ (skip-ws! sc) _ (match-str! sc "end")]
          (apply list head-sym test (concat body else-body)))
        (do (match-str! sc "end")
            (apply list head-sym test body))))

    ("if-let" "if-some")
    (let [bindings (parse-bindings sc)
          _ (skip-ws! sc) _ (match-str! sc ":") body (parse-body sc)]
      (skip-ws! sc)
      (if (match-str! sc "else")
        (let [_ (skip-ws! sc) _ (match-str! sc ":") else-body (parse-body sc)
              _ (skip-ws! sc) _ (match-str! sc "end")]
          (apply list head-sym bindings (concat body else-body)))
        (do (match-str! sc "end")
            (apply list head-sym bindings body))))

    ("when" "when-not" "while")
    (let [test (parse-expr sc)
          _ (skip-ws! sc) _ (match-str! sc ":") body (parse-body sc)
          _ (skip-ws! sc) _ (match-str! sc "end")]
      (apply list head-sym test body))

    ("when-let" "when-some" "when-first")
    (let [bindings (parse-bindings sc)
          _ (skip-ws! sc) _ (match-str! sc ":") body (parse-body sc)
          _ (skip-ws! sc) _ (match-str! sc "end")]
      (apply list head-sym bindings body))

    ("let" "binding" "with-open" "with-redefs")
    (let [bindings (parse-bindings sc)
          _ (skip-ws! sc) _ (match-str! sc ":") body (parse-body sc)
          _ (skip-ws! sc) _ (match-str! sc "end")]
      (apply list head-sym bindings body))

    "loop"
    (do (skip-ws! sc)
        (if (= (speek sc) \:)
          (let [_ (match-str! sc ":") body (parse-body sc)
                _ (skip-ws! sc) _ (match-str! sc "end")]
            (apply list 'loop [] body))
          (let [bindings (parse-bindings sc)
                _ (skip-ws! sc) _ (match-str! sc ":") body (parse-body sc)
                _ (skip-ws! sc) _ (match-str! sc "end")]
            (apply list 'loop bindings body))))

    "cond"
    (let [_ (skip-ws! sc) _ (match-str! sc ":")
          clauses (loop [cs []]
                    (skip-ws! sc)
                    (if (or (str/starts-with? (speek-str sc 3) "end") (seof? sc))
                      cs
                      (let [test (parse-expr sc)
                            _ (skip-ws! sc) _ (match-str! sc "=>") _ (skip-ws! sc)
                            val (parse-expr sc)]
                        (recur (into cs [test val])))))
          _ (skip-ws! sc) _ (match-str! sc "end")]
      (apply list 'cond clauses))

    "case"
    (let [dispatch (parse-expr sc)
          _ (skip-ws! sc) _ (match-str! sc ":")
          clauses (loop [cs []]
                    (skip-ws! sc)
                    (let [peeked (speek-str sc 4)]
                      (cond
                        (str/starts-with? peeked "end") cs
                        (str/starts-with? peeked "else")
                        (do (match-str! sc "else") (skip-ws! sc) (match-str! sc "=>")
                            (skip-ws! sc) (conj cs (parse-expr sc)))
                        :else
                        (let [test (parse-expr sc)
                              _ (skip-ws! sc) _ (match-str! sc "=>") _ (skip-ws! sc)
                              val (parse-expr sc)]
                          (recur (into cs [test val]))))))
          _ (skip-ws! sc) _ (match-str! sc "end")]
      (apply list 'case dispatch clauses))

    ("for" "doseq" "dotimes")
    (let [bindings (parse-for-bindings sc)
          _ (skip-ws! sc) _ (match-str! sc ":") body (parse-body sc)
          _ (skip-ws! sc) _ (match-str! sc "end")]
      (apply list head-sym bindings body))

    "do"
    (let [_ (skip-ws! sc) _ (match-str! sc ":") body (parse-body sc)
          _ (skip-ws! sc) _ (match-str! sc "end")]
      (apply list 'do body))

    "try"
    (let [_ (skip-ws! sc) _ (match-str! sc ":")
          body (parse-body sc)
          catches (loop [cs []]
                    (skip-ws! sc)
                    (if (match-str! sc "catch")
                      (let [_ (skip-ws! sc) exc (parse-primary sc)
                            _ (skip-ws! sc) binding (parse-primary sc)
                            _ (skip-ws! sc) _ (match-str! sc ":") cbody (parse-body sc)]
                        (recur (conj cs (apply list 'catch exc binding cbody))))
                      cs))
          finally-form (do (skip-ws! sc)
                           (when (match-str! sc "finally")
                             (skip-ws! sc) (match-str! sc ":")
                             (let [fbody (parse-body sc)]
                               (apply list 'finally fbody))))
          _ (skip-ws! sc) _ (match-str! sc "end")]
      (apply list 'try (concat body catches (when finally-form [finally-form]))))

    "ns"
    (let [_ (skip-ws! sc) name-sym (parse-primary sc)
          decls (loop [ds []]
                  (skip-ws! sc)
                  (if (or (str/starts-with? (speek-str sc 3) "end") (seof? sc))
                    ds
                    (recur (conj ds (parse-sexp sc)))))
          _ (skip-ws! sc) _ (match-str! sc "end")]
      (apply list 'ns name-sym decls))))

(defn- parse-primary [sc]
  (skip-ws! sc)
  (when (seof? sc) (throw (ex-info "Unexpected end of input" {})))
  (let [ch (speek sc)]
    (cond
      ;; String
      (= ch \") (read-string (read-string-lit! sc))

      ;; Regex
      (and (= ch \#) (= (speek-str sc 2) "#\""))
      (read-regex-lit! sc)

      ;; Set
      (= (speek-str sc 2) "#{") (parse-set sc)

      ;; Char literal
      (and (= ch \\) (let [p (+ @(:pos sc) 1)]
                        (and (< p (count (:src sc)))
                             (Character/isLetter (.charAt ^String (:src sc) p)))))
      (read-char-lit! sc)

      ;; S-expression passthrough
      (= ch \() (parse-sexp sc)

      ;; Vector
      (= ch \[) (parse-vector sc)

      ;; Map
      (= ch \{) (parse-map sc)

      ;; Quote
      (= ch \') (do (sadvance! sc) (list 'quote (parse-primary sc)))

      ;; Var quote
      (= (speek-str sc 2) "#'") (do (sadvance! sc) (sadvance! sc)
                                     (list 'var (parse-primary sc)))

      ;; Deref
      (= ch \@) (do (sadvance! sc) (list 'deref (parse-expr sc)))

      ;; Metadata
      (= ch \^) (do (sadvance! sc)
                     (let [m (parse-primary sc) _ (skip-ws! sc) target (parse-primary sc)]
                       (let [meta-map (cond (keyword? m) {m true}
                                            (symbol? m) {:tag m}
                                            (map? m) m
                                            :else {m true})]
                         (vary-meta target merge meta-map))))

      ;; Number (including negative)
      (or (digit? ch) (and (= ch \-) (let [p (inc @(:pos sc))]
                                        (and (< p (count (:src sc)))
                                             (digit? (.charAt ^String (:src sc) p))))))
      (read-number! sc)

      ;; Keyword
      (= ch \:) (read-keyword! sc)

      ;; Symbol / reserved word / call
      (or (sym-start? ch) (= ch \-))
      (let [sym (read-symbol! sc)
            s (str sym)]
        (cond
          ;; Reserved block keywords
          (contains? #{"if" "if-not" "if-let" "if-some"
                       "when" "when-not" "when-let" "when-some" "when-first" "while"
                       "let" "binding" "with-open" "with-redefs"
                       "loop" "cond" "condp" "case" "for" "doseq" "dotimes"
                       "do" "try" "ns"} s)
          (do (skip-ws! sc) (parse-block sc sym))

          ;; Def
          (or (= s "def") (= s "defonce"))
          (let [_ (skip-ws! sc) name-sym (read-symbol! sc)
                _ (skip-ws! sc) _ (match-str! sc ":=") _ (skip-ws! sc)
                val (parse-expr sc)]
            (list (symbol s) name-sym val))

          ;; Defn
          (#{"defn" "defn-" "defmacro" "e/defn"} s)
          (let [_ (skip-ws! sc) name-sym (read-symbol! sc) _ (skip-ws! sc)]
            (if (= (speek sc) \()
              ;; Single or multi arity
              (let [params (parse-call-args sc) _ (skip-ws! sc)]
                (if (match-str! sc ":")
                  (let [body (parse-body sc) _ (skip-ws! sc) _ (match-str! sc "end")]
                    (apply list (symbol s) name-sym (vec params) body))
                  ;; Multi-arity: params was first arity's params
                  ;; Actually this means we already consumed params, expect : body end
                  (throw (ex-info (str "Expected : after params in " s) {}))))
              ;; Multi-arity without first params
              (let [arities (loop [as []]
                              (skip-ws! sc)
                              (if (= (speek sc) \()
                                (let [params (parse-call-args sc)
                                      _ (skip-ws! sc) _ (match-str! sc ":")
                                      body (parse-body sc)]
                                  (recur (conj as (apply list (vec params) body))))
                                as))
                    _ (skip-ws! sc) _ (match-str! sc "end")]
                (apply list (symbol s) name-sym arities))))

          ;; fn
          (= s "fn")
          (let [_ (skip-ws! sc)
                fn-name (when (and (not (seof? sc))
                                   (sym-start? (speek sc))
                                   (not= (speek sc) \())
                          (let [n (read-symbol! sc)] (skip-ws! sc) n))
                params (parse-call-args sc)
                _ (skip-ws! sc) _ (match-str! sc ":")
                body (parse-body sc) _ (skip-ws! sc) _ (match-str! sc "end")]
            (apply list 'fn (if fn-name
                              (cons fn-name (cons (vec params) body))
                              (cons (vec params) body))))

          ;; new
          (= s "new")
          (let [_ (skip-ws! sc) class-sym (read-symbol! sc)
                args (do (skip-ws! sc) (parse-call-args sc))]
            (apply list (symbol (str class-sym ".")) args))

          ;; not
          (= s "not") (do (skip-sp! sc) (list 'not (parse-expr sc)))

          ;; throw
          (= s "throw") (do (skip-sp! sc) (list 'throw (parse-expr sc)))

          ;; recur
          (= s "recur") (apply list 'recur (parse-call-args sc))

          ;; nil/true/false
          (= s "nil") nil
          (= s "true") true
          (= s "false") false

          ;; Call: sym(args)
          (and (not (seof? sc)) (= (speek sc) \())
          (let [args (parse-call-args sc)]
            ;; Check for postfix method/field
            (loop [result (apply list sym args)]
              (if (and (not (seof? sc)) (= (speek sc) \.))
                (let [_ (sadvance! sc)]
                  (if (= (speek sc) \-)
                    ;; Field access: .-field
                    (let [_ (sadvance! sc) field (read-symbol! sc)]
                      (recur (list (symbol (str ".-" field)) result)))
                    ;; Method call: .method(args)
                    (let [method (read-symbol! sc)]
                      (if (and (not (seof? sc)) (= (speek sc) \())
                        (let [margs (parse-call-args sc)]
                          (recur (apply list (symbol (str "." method)) result margs)))
                        (recur (list (symbol (str "." method)) result))))))
                result)))

          ;; Plain symbol — check for postfix
          :else
          (loop [result sym]
            (cond
              ;; Method call: .method(args)
              (and (not (seof? sc)) (= (speek sc) \.))
              (let [_ (sadvance! sc)]
                (if (= (speek sc) \-)
                  (let [_ (sadvance! sc) field (read-symbol! sc)]
                    (recur (list (symbol (str ".-" field)) result)))
                  (let [method (read-symbol! sc)]
                    (if (and (not (seof? sc)) (= (speek sc) \())
                      (let [margs (parse-call-args sc)]
                        (recur (apply list (symbol (str "." method)) result margs)))
                      (recur (list (symbol (str "." method)) result))))))
              :else result))))

      :else (throw (ex-info (str "Unexpected character: " ch) {:ch ch})))))

;; ============================================================
;; Infix operators — precedence climbing
;; ============================================================

(def ^:private ops
  {"or" ['or 1] "and" ['and 2]
   "=" ['= 3] "not=" ['not= 3] "<" ['< 3] ">" ['> 3] "<=" ['<= 3] ">=" ['>= 3]
   "+" ['+ 4] "-" ['- 4]
   "*" ['* 5] "/" ['/ 5] "mod" ['mod 5] "rem" ['rem 5]})

(defn- peek-infix-op [sc]
  (let [saved-pos @(:pos sc)
        _ (skip-sp! sc)
        peeked (speek-str sc 4)
        result (cond
                 (str/starts-with? peeked "|>") nil
                 (str/starts-with? peeked ".>") nil
                 (str/starts-with? peeked "not=") "not="
                 (str/starts-with? peeked "mod ") "mod"
                 (str/starts-with? peeked "rem ") "rem"
                 (str/starts-with? peeked "and ") "and"
                 (str/starts-with? peeked "or ") "or"
                 (str/starts-with? peeked "<=") "<="
                 (str/starts-with? peeked ">=") ">="
                 :else
                 (let [ch (first peeked)]
                   (when (and ch (#{\+ \- \* \/ \< \> \=} ch))
                     (when-not (and (= ch \-)
                                    (not (#{\space \tab} (.charAt ^String (:src sc) (dec @(:pos sc))))))
                       (str ch)))))]
    ;; Restore position — peek only, don't consume
    (vreset! (:pos sc) saved-pos)
    result))

(defn- parse-infix [sc min-prec]
  (let [left (parse-primary sc)]
    (loop [left left]
      (let [saved-pos @(:pos sc)
            op-str (peek-infix-op sc)]
        (if-let [[op-sym prec] (when op-str (get ops op-str))]
          (if (>= prec min-prec)
            (do (skip-sp! sc) (match-str! sc op-str) (skip-sp! sc)
                (let [right (parse-infix sc (inc prec))]
                  (recur (list op-sym left right))))
            (do (vreset! (:pos sc) saved-pos) left))
          (do (vreset! (:pos sc) saved-pos) left))))))

;; ============================================================
;; Pipes
;; ============================================================

(defn- parse-pipe [sc]
  (let [init (parse-infix sc 0)]
    (loop [steps [] pipe-sym nil]
      (let [saved @(:pos sc)]
        (skip-ws! sc)
        (let [peeked (speek-str sc 2)]
          (cond
            (= peeked "|>")
            (do (sadvance! sc) (sadvance! sc) (skip-sp! sc)
                (let [step (parse-infix sc 0)
                      step-form (if (seq? step) step (list step))]
                  (recur (conj steps step-form) '->>)))

            (= peeked ".>")
            (do (sadvance! sc) (sadvance! sc) (skip-sp! sc)
                (let [step (parse-infix sc 0)
                      step-form (if (seq? step) step (list step))]
                  (recur (conj steps step-form) '->)))

            :else
            (do (vreset! (:pos sc) saved)
                (if (seq steps)
                  (apply list pipe-sym init steps)
                  init))))))))

(defn- parse-expr [sc]
  (parse-pipe sc))

;; ============================================================
;; Top-level
;; ============================================================

(defn parse-string
  "Parse a superficie source string into a vector of Clojure forms."
  [s]
  (let [sc (make-scanner s)]
    (loop [forms []]
      (skip-ws! sc)
      (if (seof? sc)
        forms
        (recur (conj forms (parse-expr sc)))))))

(defn superficie-parser
  "Parser conforming to meme's pipeline contract:
   (fn [tokens opts source] → forms-vector).
   Ignores tokens — parses directly from source."
  [_tokens _opts source]
  (parse-string source))
