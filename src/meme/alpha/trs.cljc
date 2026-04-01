(ns meme.alpha.trs
  "Token-stream term rewriting system.

   Declarative rules that pattern-match on token subsequences and produce
   rewritten token subsequences. Rules are applied right-to-left in a
   single pass so innermost matches rewrite first.

   Pattern language (each element matches one or more tokens):
     {:type :symbol}             — match token by type
     {:type :open-paren :adj true} — match adjacent token (no :ws)
     {:pred fn}                  — match token satisfying predicate fn
     {:var :name :pred fn}       — match one token satisfying fn, bind to :name
     {:span :name}               — match balanced inner tokens until closer, bind to :name

   Replacement elements:
     {:ref :name}                — emit bound token(s)
     {:ref :name :dissoc-ws true} — emit bound token(s), strip :ws from first
     {:ref :name :ensure-ws s}   — emit bound token(s), ensure :ws on first
     {:token {...} :ws-from :name} — emit a literal token, copy :ws from bound var"
  (:require [meme.alpha.scan.tokenizer :as tokenizer]))

;; ============================================================
;; Delimiter matching
;; ============================================================

(def ^:private openers #{:open-paren :open-bracket :open-brace :open-set :open-anon-fn})
(def ^:private closers #{:close-paren :close-bracket :close-brace})

(defn- find-matching-close
  "Given a token vector and the index of an opener token, return the index
   of its matching closer. Tracks nested delimiters. Returns nil if unbalanced."
  [tokens open-idx]
  (let [n (count tokens)]
    (loop [i (inc open-idx)
           depth 1]
      (when (< i n)
        (let [t (:type (nth tokens i))]
          (cond
            (openers t) (recur (inc i) (inc depth))
            (closers t) (if (= depth 1) i (recur (inc i) (dec depth)))
            :else (recur (inc i) depth)))))))

(defn- find-matching-open
  "Given a token vector and the index of a closer token, scan backwards
   to find the matching opener. Returns the index, or nil."
  [tokens close-idx]
  (loop [i (dec close-idx)
         depth 1]
    (when (>= i 0)
      (let [t (:type (nth tokens i))]
        (cond
          (closers t) (recur (dec i) (inc depth))
          (openers t) (if (= depth 1) i (recur (dec i) (dec depth)))
          :else (recur (dec i) depth))))))

;; ============================================================
;; Pattern matching on token vectors
;; ============================================================

(defn- match-balanced-inner
  "Match tokens from i until the matching closer for the opener before i.
   Returns [close-idx tokens-between] or nil."
  [tokens i]
  (let [n (count tokens)]
    (loop [j i
           depth 1
           acc (transient [])]
      (when (< j n)
        (let [tok (nth tokens j)
              t (:type tok)]
          (cond
            (openers t)
            (recur (inc j) (inc depth) (conj! acc tok))

            (closers t)
            (if (= depth 1)
              [j (persistent! acc)]
              (recur (inc j) (dec depth) (conj! acc tok)))

            :else
            (recur (inc j) depth (conj! acc tok))))))))

(defn- match-element
  "Try to match a single pattern element at position i in tokens.
   Returns [new-index bindings-update] on success, nil on failure."
  [pat tokens i]
  (when (< i (count tokens))
    (let [tok (nth tokens i)]
      (cond
        ;; Span match: {:span :name} — match balanced inner tokens until closer
        (:span pat)
        (when-let [[close-idx inner] (match-balanced-inner tokens i)]
          [close-idx {(:span pat) inner}])

        ;; Type match with optional adjacency
        (and (:type pat) (not (:var pat)) (not (:pred pat)))
        (when (= (:type pat) (:type tok))
          (if (:adj pat)
            (when (not (contains? tok :ws))
              [(inc i) {}])
            [(inc i) {}]))

        ;; Predicate match (no binding)
        (and (:pred pat) (not (:var pat)))
        (when ((:pred pat) tok)
          [(inc i) {}])

        ;; Var match (single token, optional predicate)
        (:var pat)
        (let [pred (:pred pat)]
          (when (or (nil? pred) (pred tok))
            [(inc i) {(:var pat) [tok]}]))))))

(defn- match-pattern
  "Try to match a pattern (vector of elements) starting at position i.
   Returns {:end index :bindings {...}} on success, nil on failure."
  [pattern tokens i]
  (loop [pi 0
         ti i
         bindings {}]
    (if (>= pi (count pattern))
      {:end ti :bindings bindings}
      (let [pat (nth pattern pi)]
        (when-let [[new-ti new-binds] (match-element pat tokens ti)]
          (recur (inc pi) new-ti (merge bindings new-binds)))))))

;; ============================================================
;; Replacement emission
;; ============================================================

(defn- emit-replacement
  "Produce a token vector from a replacement template and bindings."
  [replacement bindings]
  (into []
    (mapcat
      (fn [elem]
        (cond
          ;; Reference: {:ref :name}
          (:ref elem)
          (let [toks (get bindings (:ref elem))]
            (cond
              (:dissoc-ws elem)
              (into [(dissoc (first toks) :ws)] (rest toks))

              (:ensure-ws elem)
              (if (empty? toks)
                toks
                (let [first-tok (first toks)]
                  (into [(if (contains? first-tok :ws)
                           first-tok
                           (assoc first-tok :ws (:ensure-ws elem)))]
                        (rest toks))))

              :else toks))

          ;; Literal token: {:token {...}}
          (:token elem)
          (let [tok (:token elem)]
            (if-let [ws-var (:ws-from elem)]
              (let [source-tok (first (get bindings ws-var))]
                [(if (contains? source-tok :ws)
                   (assoc tok :ws (:ws source-tok))
                   (dissoc tok :ws))])
              [tok]))

          :else []))
      replacement)))

;; ============================================================
;; Rules
;; ============================================================

(defn rule
  "Create a token-stream rewrite rule.
   pattern — vector of match elements
   replacement — vector of emit elements"
  [pattern replacement]
  {:pattern pattern :replacement replacement})

;; ============================================================
;; Rule engine: right-to-left single-pass rewriter
;; ============================================================

(defn- try-rules-at
  "Try each rule at position i. Returns {:start :end :result} on first match, nil otherwise."
  [rules tokens i]
  (some (fn [{:keys [pattern replacement]}]
          (when-let [{:keys [end bindings]} (match-pattern pattern tokens i)]
            {:start i :end end
             :result (emit-replacement replacement bindings)}))
        rules))

;; ============================================================
;; M-expression rules (declarative)
;; ============================================================

(def ^:private atom-head-types
  #{:symbol :keyword :number :string :char})

(def atom-head?
  "Predicate: is this token an atom that can be a call head?"
  #(contains? atom-head-types (:type %)))

(def m-call-rules
  "Declarative rules for M-expression -> S-expression rewriting on token streams.

   Rule: atom-head adjacent to open-paren.
     f(x y) -> (f x y)
     Pattern:  [head] [(adj)] [inner...] [)]
     Replace:  [(ws<-head)] [head(no-ws)] [inner(ensure-ws)] [)]"
  [(rule
     [{:var :head :pred atom-head?}
      {:type :open-paren :adj true}
      {:span :inner}
      {:type :close-paren}]
     [{:token {:type :open-paren :value "("} :ws-from :head}
      {:ref :head :dissoc-ws true}
      {:ref :inner :ensure-ws " "}
      {:token {:type :close-paren :value ")"}}])])

;; ============================================================
;; Group-head rewrite (delimiter-as-head, e.g. [x](body))
;; ============================================================

(defn- apply-group-head-rewrite
  "Rewrite a delimiter-group head adjacent to open-paren.
   [before] [head-group...] [(] [args...] [)] [after]
   -> [before] [(] [head-group...] [args...] [)] [after]"
  [tokens open-of-head close-of-head open-of-args close-of-args]
  (let [head-tokens (subvec tokens open-of-head (inc close-of-head))
        first-head (first head-tokens)
        head-inside (into [(dissoc first-head :ws)] (rest head-tokens))
        before (subvec tokens 0 open-of-head)
        open-tok (if (contains? first-head :ws)
                   {:type :open-paren :value "(" :ws (:ws first-head)}
                   {:type :open-paren :value "("})
        inner (subvec tokens (inc open-of-args) close-of-args)
        inner (if (seq inner)
                (let [fi (first inner)]
                  (into [(if (contains? fi :ws) fi (assoc fi :ws " "))]
                        (rest inner)))
                inner)
        close-tok (nth tokens close-of-args)
        after (subvec tokens (inc close-of-args))]
    (into [] cat [before [open-tok] head-inside inner [close-tok] after])))

;; ============================================================
;; Rewrite engine: declarative rules + group-head fallback
;; ============================================================

(defn- rewrite-with-group-heads
  "Apply m-call rules right-to-left, with special handling for delimiter-group heads.
   Declarative rules handle atom heads. Delimiter-group heads (e.g. [x](body))
   are detected structurally and rewritten by backing up to the matching opener."
  [rules tokens]
  (let [n (count tokens)]
    (if (< n 2)
      tokens
      (loop [i (- n 2)
             tokens tokens]
        (if (neg? i)
          tokens
          ;; Try declarative rules first
          (if-let [{:keys [start end result]} (try-rules-at rules tokens i)]
            (let [before (subvec tokens 0 start)
                  after (subvec tokens end)
                  rewritten (into [] cat [before result after])]
              (recur (dec start) rewritten))
            ;; Check for delimiter-group head: ] or ) followed by adjacent (
            (let [tok (nth tokens i)
                  next-tok (nth tokens (inc i))]
              (if (and (closers (:type tok))
                       (= :open-paren (:type next-tok))
                       (not (contains? next-tok :ws)))
                (let [open-of-head (find-matching-open tokens i)
                      close-of-args (when open-of-head (find-matching-close tokens (inc i)))]
                  (if close-of-args
                    (let [rewritten (apply-group-head-rewrite tokens open-of-head i (inc i) close-of-args)]
                      (recur (dec open-of-head) rewritten))
                    (recur (dec i) tokens)))
                (recur (dec i) tokens)))))))))

;; ============================================================
;; Public API
;; ============================================================

(defn rewrite-meme->sexp
  "Rewrite a meme token stream to S-expression token structure.
   Uses declarative m-call rules applied right-to-left."
  [tokens]
  (rewrite-with-group-heads m-call-rules tokens))

(defn tokens->text
  "Reconstruct source text from a token vector, preserving whitespace."
  [tokens]
  (apply str
    (mapcat (fn [tok]
              (if-let [ws (:ws tok)]
                [ws (:value tok)]
                [(:value tok)]))
            tokens)))

(defn tokenize-and-rewrite
  "Tokenize meme source and rewrite to S-expression token structure."
  [source]
  (let [tokens (tokenizer/attach-whitespace (tokenizer/tokenize source) source)]
    (rewrite-meme->sexp tokens)))

(defn meme->clj-text
  "Convert meme source to Clojure source text via token-stream rewriting."
  [source]
  (tokens->text (tokenize-and-rewrite source)))
