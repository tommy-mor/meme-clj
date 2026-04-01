(ns meme.alpha.trs
  "Token-stream term rewriting system.

   Rewrites flat token vectors directly — no intermediate tree.
   The core operation: M-expression call rewriting.

   An M-call is detected when a value token is adjacent to an open-paren
   (no :ws on the paren token). The rewrite moves the head inside:

     [head, (, ...args, )]  →  [(, head, ...args, )]

   Applied repeatedly left-to-right until no more rewrites fire."
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

;; ============================================================
;; Head detection
;; ============================================================

(def ^:private atom-head-types
  "Atom token types that can serve as a call head (single token)."
  #{:symbol :keyword :number :string :char})

(defn- atom-head?
  "Can this single token be the head of an M-expression call?"
  [tok]
  (contains? atom-head-types (:type tok)))

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

(defn- adjacent?
  "Is a token adjacent to the previous one (no whitespace between them)?"
  [tok]
  (not (contains? tok :ws)))

;; ============================================================
;; Core rewrite: M-call flattening
;; ============================================================

(defn- apply-group-m-call-rewrite
  "Apply m-call rewrite: move head tokens (head-start..head-end inclusive)
   inside the parens at (open-idx..close-idx).
   [before] [head...] [(] [args...] [)] [after]
   → [before] [(] [head...] [args...] [)] [after]"
  [tokens head-start head-end open-idx close-idx]
  (let [head-tokens (subvec tokens head-start (inc head-end))
        ;; First head token: transfer its :ws to the open-paren
        first-head (first head-tokens)
        head-tokens-inside (into [(dissoc first-head :ws)] (rest head-tokens))
        before (subvec tokens 0 head-start)
        open-tok (nth tokens open-idx)
        open-tok (if (contains? first-head :ws)
                   (assoc open-tok :ws (:ws first-head))
                   (dissoc open-tok :ws))
        inner-start (inc open-idx)
        inner-tokens (subvec tokens inner-start close-idx)
        ;; Ensure separation between last head token and first arg
        inner-tokens (if (seq inner-tokens)
                       (let [first-inner (first inner-tokens)]
                         (into [(if (contains? first-inner :ws)
                                  first-inner
                                  (assoc first-inner :ws " "))]
                               (rest inner-tokens)))
                       inner-tokens)
        close-tok (nth tokens close-idx)
        after (subvec tokens (inc close-idx))]
    (into [] cat [before
                  [open-tok]
                  head-tokens-inside
                  inner-tokens
                  [close-tok]
                  after])))

(defn- rewrite-m-calls-pass
  "Single left-to-right pass. Rewrites the LAST m-call found so that
   inner calls are rewritten before outer calls on subsequent passes.
   Returns the rewritten token vector, or the same vector if no rewrite.
   Handles both atom heads (symbol, keyword, etc.) and delimiter-group
   heads (vector-as-head: [x](body) → ([x] body))."
  [tokens]
  (let [n (count tokens)]
    (loop [i 0
           last-match nil]
      (if (>= i (- n 1))
        (if last-match
          (let [{:keys [head-start head-end open-idx close-idx]} last-match]
            (apply-group-m-call-rewrite tokens head-start head-end open-idx close-idx))
          tokens)
        (let [next-tok (nth tokens (inc i))]
          (if (and (= :open-paren (:type next-tok))
                   (adjacent? next-tok))
            ;; Adjacent open-paren — check what precedes it
            (let [tok (nth tokens i)]
              (cond
                ;; Atom head: symbol(args)
                (atom-head? tok)
                (if-let [close-idx (find-matching-close tokens (inc i))]
                  (recur (inc i) {:head-start i :head-end i
                                  :open-idx (inc i) :close-idx close-idx})
                  (recur (inc i) last-match))

                ;; Close-delimiter head: ](args) or )(args) — find matching opener
                (closers (:type tok))
                (if-let [open-of-head (find-matching-open tokens i)]
                  (if-let [close-idx (find-matching-close tokens (inc i))]
                    (recur (inc i) {:head-start open-of-head :head-end i
                                    :open-idx (inc i) :close-idx close-idx})
                    (recur (inc i) last-match))
                  (recur (inc i) last-match))

                :else (recur (inc i) last-match)))
            (recur (inc i) last-match)))))))

(defn rewrite-m-calls
  "Rewrite M-expression calls in a token vector to fixpoint.
   Each pass rewrites the rightmost (innermost) m-call found."
  [tokens]
  (loop [tokens tokens]
    (let [result (rewrite-m-calls-pass tokens)]
      (if (= result tokens)
        result
        (recur result)))))

;; ============================================================
;; Public API
;; ============================================================

(defn rewrite-meme->sexp
  "Rewrite a meme token stream to S-expression token structure.
   Moves M-expression heads inside their parens."
  [tokens]
  (rewrite-m-calls tokens))

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
