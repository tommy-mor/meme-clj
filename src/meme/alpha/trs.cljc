(ns meme.alpha.trs
  "Token-stream term rewriting system.

   Three stages:
   1. Nest: group balanced delimiters into nested vectors (cheap pre-pass)
   2. Rewrite: apply rules on nested structure, forward-matching
   3. Flatten: unnest back to flat token vector for emission

   Rules are data: a pattern that matches a window of sibling nodes,
   and a rewrite function that produces replacement nodes.

   On nested structures, patterns match forward — no backward scanning
   needed. A nested vector is just a vector in the pattern."
  (:require [meme.alpha.scan.tokenizer :as tokenizer]))

;; ============================================================
;; Delimiter classification
;; ============================================================

(def ^:private openers #{:open-paren :open-bracket :open-brace :open-set :open-anon-fn})
(def ^:private closers #{:close-paren :close-bracket :close-brace})

;; ============================================================
;; Stage 1: Nest — group balanced delimiters
;; ============================================================

(defn nest-tokens
  "Group balanced delimiters into nested vectors.
   Each delimited group becomes [opener-tok ...children closer-tok].
   Children may themselves be nested groups or atom tokens."
  [tokens]
  (loop [i 0
         stack [[]]]
    (if (>= i (count tokens))
      (peek stack)
      (let [tok (nth tokens i)
            typ (:type tok)]
        (cond
          (openers typ)
          (recur (inc i) (conj stack [tok]))

          (closers typ)
          (let [current (conj (peek stack) tok)
                parent-stack (pop stack)
                parent (peek parent-stack)]
            (recur (inc i) (conj (pop parent-stack) (conj parent current))))

          :else
          (recur (inc i) (conj (pop stack) (conj (peek stack) tok))))))))

;; ============================================================
;; Whitespace helpers for nested nodes
;; ============================================================

(defn- node-ws
  "Get the :ws of a node (atom token or first token of a nested group)."
  [node]
  (if (vector? node)
    (:ws (first node))
    (:ws node)))

(defn- strip-ws
  "Remove :ws from a node."
  [node]
  (if (vector? node)
    (into [(dissoc (first node) :ws)] (rest node))
    (dissoc node :ws)))

(defn- set-ws
  "Set :ws on a node."
  [node ws]
  (if (vector? node)
    (into [(assoc (first node) :ws ws)] (rest node))
    (assoc node :ws ws)))

(defn- ensure-ws
  "If a node has no :ws, set it to the given value."
  [node ws]
  (if (node-ws node) node (set-ws node ws)))

;; ============================================================
;; Stage 2: Rule engine on nested structures
;; ============================================================

;; A rule is {:match (fn [children i] -> match-or-nil) :rewrite (fn [match] -> nodes)}
;; A match is a map with whatever the rule needs to perform the rewrite.
;; The rule engine scans right-to-left, applies the first matching rule,
;; splices the rewrite result, and adjusts the index.

(defn rule
  "Create a rewrite rule.
   match-fn: (fn [children i] -> {:width n ...} or nil)
     Called at each position. Returns a match map with at least :width
     (number of nodes consumed) or nil if no match.
   rewrite-fn: (fn [match] -> [replacement-nodes...])
     Produces the replacement nodes to splice in."
  [match-fn rewrite-fn]
  {:match match-fn :rewrite rewrite-fn})

;; ============================================================
;; M-call rule: the one rule
;; ============================================================

(defn- paren-group?
  "Is this node a paren-delimited group?"
  [node]
  (and (vector? node) (map? (first node)) (= :open-paren (:type (first node)))))

(defn- adjacent-paren-group?
  "Is this node an adjacent paren group (no :ws on opener)?"
  [node]
  (and (paren-group? node) (nil? (node-ws node))))

(def ^:private prefix-types
  "Token types that are prefix operators — not valid call heads."
  #{:quote :deref :meta :syntax-quote :unquote :unquote-splicing
    :var-quote :discard :tagged-literal :reader-cond-start
    :namespaced-map-start})

(defn- valid-head?
  "Can this node be the head of an M-expression call?
   Any node except prefix tokens and open delimiters."
  [node]
  (if (map? node)
    (not (or (contains? prefix-types (:type node))
             (openers (:type node))))
    ;; Nested group (vector) — always a valid head
    true))

(def m-call-rule
  "M-expression call: a valid head followed by an adjacent paren-group.
   head(args...) → (head args...)
   [x](args...)  → ([x] args...)

   The head is moved inside the paren group after the opener.
   The head's :ws transfers to the opener (the group takes the head's
   position in the sibling list). A space is ensured between head and
   first arg."
  (rule
    ;; match: two adjacent siblings — valid head + adjacent paren-group
    (fn [children i]
      (when (< (inc i) (count children))
        (let [node (nth children i)
              next-node (nth children (inc i))]
          (when (and (valid-head? node) (adjacent-paren-group? next-node))
            {:width 2
             :head node
             :group next-node}))))
    ;; rewrite: move head inside paren group
    (fn [{:keys [head group]}]
      (let [opener (first group)
            closer (peek group)
            inner (subvec group 1 (dec (count group)))
            ;; Transfer head's :ws to opener
            opener (if-let [ws (node-ws head)]
                     (assoc opener :ws ws)
                     (dissoc opener :ws))
            head-inside (strip-ws head)
            ;; Ensure space between head and first arg
            inner (if (seq inner)
                    (into [(ensure-ws (first inner) " ")] (rest inner))
                    inner)]
        [(into [opener head-inside] (conj inner closer))]))))

;; ============================================================
;; Rewrite engine
;; ============================================================

(defn rewrite-level
  "Rewrite one level of nested children.
   Recurses into sub-groups first (bottom-up), then scans right-to-left
   applying rules at this level."
  [rules children]
  ;; Recurse into sub-groups
  (let [children (mapv (fn [c]
                         (if (vector? c)
                           (let [opener (first c)
                                 closer (peek c)
                                 inner (subvec c 1 (dec (count c)))
                                 rewritten (rewrite-level rules inner)]
                             (into [opener] (conj rewritten closer)))
                           c))
                       children)]
    ;; Scan right-to-left, apply first matching rule
    (loop [i (- (count children) 2)
           children children]
      (if (neg? i)
        children
        (let [match (some (fn [r] (when-let [m ((:match r) children i)]
                                    (assoc m :rule r)))
                          rules)]
          (if match
            (let [result ((:rewrite (:rule match)) match)
                  width (:width match)
                  before (subvec children 0 i)
                  after (subvec children (+ i width))
                  new-children (into [] cat [before result after])]
              (recur (dec i) new-children))
            (recur (dec i) children)))))))

;; ============================================================
;; Default rule set
;; ============================================================

(def default-rules
  "Default rule set for M-expression → S-expression rewriting."
  [m-call-rule])

;; ============================================================
;; Stage 3: Flatten — unnest back to flat token vector
;; ============================================================

(defn- flatten-nested
  "Flatten a nested token structure back to a flat token vector."
  [nodes]
  (into []
    (mapcat (fn [node]
              (if (vector? node)
                (flatten-nested node)
                [node]))
            nodes)))

;; ============================================================
;; Public API
;; ============================================================

(defn rewrite-meme->sexp
  "Rewrite a meme token stream to S-expression token structure.
   Nest → rewrite → flatten."
  ([tokens] (rewrite-meme->sexp tokens default-rules))
  ([tokens rules]
   (-> tokens nest-tokens ((->> rules (partial rewrite-level))) flatten-nested)))

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
