(ns meme.alpha.collapsar
  "Declarative rewrite phases with verified termination.

   A language's syntactic phases — parsing, desugaring, lowering — expressed
   as declarative rewrite rules. Rule sets compose with verified termination.
   Towers of languages collapse into flat pipelines.

   Self-contained: pattern matching, substitution, rewriting, head analysis,
   phases, pipelines, languages. No external dependencies beyond clojure.set.

   Three layers:
     phase    — a set of rules with a computed signature (consumed/produced heads)
     pipeline — a sequence of phases, validated: no phase raises earlier heads
     language — a pipeline + a target backend

   Head-eliminating phases (consumed ∩ produced = ∅) terminate by construction.
   Non-head-eliminating phases run with bounded iteration."
  (:require [clojure.set :as set]
            [clojure.string :as str]))

;; ============================================================
;; Pattern Language
;; ============================================================

(defn pattern-var?
  "Is x a pattern variable like ?x — matches one expression, binds to name."
  [x]
  (and (symbol? x)
       (str/starts-with? (name x) "?")
       (not (str/starts-with? (name x) "??"))
       (not= x '?)))

(defn splice-var?
  "Is x a splice variable like ??xs — matches zero or more, binds to name."
  [x]
  (and (symbol? x)
       (str/starts-with? (name x) "??")))

(defn wildcard?
  "Is x the wildcard _ — matches anything, no binding."
  [x]
  (= x '_))

(defn- var-name
  "Extract the binding name from ?x or ??xs."
  [x]
  (let [n (name x)]
    (cond
      (str/starts-with? n "??") (symbol (subs n 2))
      (str/starts-with? n "?")  (symbol (subs n 1))
      :else x)))

;; ============================================================
;; Pattern Matching
;; ============================================================

(declare match-pattern)

(defn- match-seq
  "Match a pattern sequence against an expression sequence.
   Handles splice variables (??xs) with backtracking."
  [patterns exprs bindings]
  (cond
    (and (empty? patterns) (empty? exprs))
    bindings

    (empty? patterns)
    nil

    (splice-var? (first patterns))
    (let [var (var-name (first patterns))
          rest-pats (rest patterns)]
      (loop [n 0]
        (when (<= n (count exprs))
          (let [taken (vec (take n exprs))
                remaining (drop n exprs)
                new-bindings (if (contains? bindings var)
                               (when (= (get bindings var) taken) bindings)
                               (assoc bindings var taken))]
            (or (when new-bindings
                  (match-seq rest-pats remaining new-bindings))
                (recur (inc n)))))))

    (empty? exprs)
    nil

    :else
    (when-let [new-bindings (match-pattern (first patterns) (first exprs) bindings)]
      (match-seq (rest patterns) (rest exprs) new-bindings))))

(defn match-pattern
  "Match a pattern against an expression.
   Returns a bindings map {symbol value} on success, nil on failure.
   Same variable must match the same value (consistency)."
  ([pattern expr] (match-pattern pattern expr {}))
  ([pattern expr bindings]
   (cond
     (wildcard? pattern)
     bindings

     (pattern-var? pattern)
     (let [var (var-name pattern)]
       (if (contains? bindings var)
         (when (= (get bindings var) expr) bindings)
         (assoc bindings var expr)))

     (and (sequential? pattern) (sequential? expr)
          (= (vector? pattern) (vector? expr)))
     (match-seq (vec pattern) (vec expr) bindings)

     (= pattern expr)
     bindings

     :else nil)))

;; ============================================================
;; Substitution
;; ============================================================

(defn substitute
  "Replace pattern variables in template with values from bindings.
   Splice variables (??xs) are spliced into the parent sequence."
  [template bindings]
  (cond
    (pattern-var? template)
    (get bindings (var-name template) template)

    (splice-var? template)
    (get bindings (var-name template) [])

    (sequential? template)
    (let [was-list (seq? template)
          result (reduce
                  (fn [acc item]
                    (if (splice-var? item)
                      (into acc (get bindings (var-name item) []))
                      (conj acc (substitute item bindings))))
                  []
                  template)]
      (if was-list (apply list result) result))

    (map? template)
    (into {} (map (fn [[k v]]
                    [(substitute k bindings) (substitute v bindings)])
                  template))

    :else template))

;; ============================================================
;; Rules
;; ============================================================

(defn rule
  "Create a rule: pattern => replacement. Optional guard predicate on bindings."
  ([pattern replacement]
   {:name (gensym "rule_") :pattern pattern :replacement replacement})
  ([pattern replacement guard]
   {:name (gensym "rule_") :pattern pattern :replacement replacement :guard guard})
  ([rule-name pattern replacement guard]
   {:name rule-name :pattern pattern :replacement replacement :guard guard}))

(defn named-rule
  "Create a named rule."
  ([rule-name pattern replacement]
   {:name rule-name :pattern pattern :replacement replacement})
  ([rule-name pattern replacement guard]
   {:name rule-name :pattern pattern :replacement replacement :guard guard}))

(defn apply-rule
  "Try to apply a single rule. Returns the rewritten expression, or nil."
  [rule expr]
  (when-let [bindings (match-pattern (:pattern rule) expr)]
    (if-let [guard (:guard rule)]
      (when (guard bindings) (substitute (:replacement rule) bindings))
      (substitute (:replacement rule) bindings))))

(defn apply-rules
  "Try each rule in order. Returns the first successful rewrite, or nil."
  [rules expr]
  (reduce (fn [_ rule]
            (when-let [result (apply-rule rule expr)]
              (reduced result)))
          nil rules))

;; ============================================================
;; Rewriting Strategies
;; ============================================================

(defn rewrite-once
  "One bottom-up pass: rewrite each node, innermost first.
   Returns [changed? result]."
  [rules expr]
  (if (sequential? expr)
    (let [was-list (seq? expr)
          children (if was-list (vec expr) expr)
          rewritten (mapv (fn [child] (let [[_ r] (rewrite-once rules child)] r))
                          children)
          rebuilt (if was-list (apply list rewritten) rewritten)
          result (apply-rules rules rebuilt)]
      (if result
        [true result]
        [(not= rebuilt expr) rebuilt]))
    (if-let [result (apply-rules rules expr)]
      [true result]
      [false expr])))

(defn rewrite
  "Apply rules bottom-up to fixed point, with iteration cap."
  ([rules expr] (rewrite rules expr 100))
  ([rules expr max-iters]
   (loop [expr expr i 0]
     (when (> i max-iters)
       (throw (ex-info "Rewrite did not reach fixed point"
                       {:iterations max-iters :expr expr})))
     (let [[changed? result] (rewrite-once rules expr)]
       (if changed? (recur result (inc i)) result)))))

(defn rewrite-top
  "Apply rules at top level only, to fixed point."
  ([rules expr] (rewrite-top rules expr 100))
  ([rules expr max-iters]
   (loop [expr expr i 0]
     (when (> i max-iters)
       (throw (ex-info "Rewrite-top did not reach fixed point"
                       {:iterations max-iters :expr expr})))
     (if-let [result (apply-rules rules expr)]
       (recur result (inc i))
       expr))))

;; ============================================================
;; Head Analysis
;; ============================================================

(defn pattern-head
  "Extract the consumed head symbol from a rule's pattern.
   Returns the head symbol if the pattern is a list with a literal symbol head.
   Returns nil for variable heads, non-list patterns, or empty lists."
  [pattern]
  (when (and (seq? pattern) (seq pattern))
    (let [h (first pattern)]
      (when (and (symbol? h)
                 (not (pattern-var? h))
                 (not (splice-var? h))
                 (not (wildcard? h)))
        h))))

(defn- collect-heads
  "Walk a template collecting head symbols of all list forms.
   Variable heads are skipped — opaque at definition time."
  [template]
  (cond
    (and (seq? template) (seq template))
    (let [h (first template)
          child-heads (mapcat collect-heads (rest template))]
      (if (and (symbol? h) (not (pattern-var? h)) (not (splice-var? h)))
        (cons h child-heads)
        child-heads))

    (vector? template)
    (mapcat collect-heads template)

    (map? template)
    (mapcat (fn [[k v]] (concat (collect-heads k) (collect-heads v))) template)

    :else nil))

(defn replacement-heads
  "Extract all statically-known produced head symbols from a replacement."
  [replacement]
  (set (collect-heads replacement)))

(defn- has-dynamic-heads?
  "Does this replacement contain list forms with variable heads?"
  [replacement]
  (cond
    (and (seq? replacement) (seq replacement))
    (let [h (first replacement)]
      (or (and (or (pattern-var? h) (splice-var? h)) true)
          (some has-dynamic-heads? (rest replacement))))

    (vector? replacement)
    (some has-dynamic-heads? replacement)

    :else false))

;; ============================================================
;; Rule Signature
;; ============================================================

(defn rule-signature
  "Compute the signature of a single rule.
   Returns {:consumes #{...} :produces #{...} :dynamic-produces? bool}"
  [rule]
  (let [consumed (when-let [h (pattern-head (:pattern rule))] #{h})
        produced (replacement-heads (:replacement rule))
        dynamic (has-dynamic-heads? (:replacement rule))]
    {:consumes (or consumed #{})
     :produces produced
     :dynamic-produces? dynamic}))

;; ============================================================
;; Splice & Overlap Analysis
;; ============================================================

(defn- count-splices [pattern]
  (cond
    (splice-var? pattern) 1
    (sequential? pattern) (reduce + 0 (map count-splices pattern))
    :else 0))

(defn- patterns-may-overlap?
  "Conservative: could two patterns match the same expression?"
  [p1 p2]
  (cond
    (or (pattern-var? p1) (wildcard? p1) (splice-var? p1)) true
    (or (pattern-var? p2) (wildcard? p2) (splice-var? p2)) true

    (and (seq? p1) (seq? p2))
    (let [h1 (first p1) h2 (first p2)]
      (cond
        (or (nil? h1) (nil? h2)) true
        (or (pattern-var? h1) (pattern-var? h2)) true
        :else (= h1 h2)))

    (and (vector? p1) (vector? p2)) true
    :else (= p1 p2)))

;; ============================================================
;; Phase
;; ============================================================

(defn phase-signature
  "Compute the aggregate signature of a phase (set of rules)."
  [rules]
  (reduce
   (fn [acc rule]
     (let [sig (rule-signature rule)]
       (-> acc
           (update :consumes into (:consumes sig))
           (update :produces into (:produces sig))
           (update :dynamic-produces? #(or % (:dynamic-produces? sig))))))
   {:consumes #{} :produces #{} :dynamic-produces? false}
   rules))

(defn head-eliminating?
  "Is this set of rules head-eliminating? consumed ∩ produced = ∅."
  [rules]
  (let [{:keys [consumes produces]} (phase-signature rules)]
    (empty? (set/intersection consumes produces))))

(defn- splice-warnings [rules]
  (keep
   (fn [rule]
     (let [n (count-splices (:pattern rule))]
       (when (> n 1)
         {:rule (:name rule) :type :multiple-splices :count n
          :message (str "Rule " (:name rule) " has " n
                        " splice variables — matching may be exponential")})))
   rules))

(defn- overlap-warnings [rules]
  (let [indexed (vec (map-indexed vector rules))]
    (for [[i r1] indexed
          [j r2] indexed
          :when (> j i)
          :when (patterns-may-overlap? (:pattern r1) (:pattern r2))]
      {:rules [(:name r1) (:name r2)] :type :overlapping-patterns
       :message (str "Rules " (:name r1) " and " (:name r2)
                     " have potentially overlapping patterns"
                     " — first-match ordering applies")})))

(defn make-phase
  "Create a phase with computed signature and verification.

   opts:
     :max-iters — iteration bound for non-head-eliminating phases (default 100)
     :strategy  — :bottom-up (default) or :top-only
     :measure   — optional (fn [expr] -> number), checked to decrease each pass"
  ([phase-name rules] (make-phase phase-name rules {}))
  ([phase-name rules opts]
   (let [sig (phase-signature rules)
         head-elim (empty? (set/intersection (:consumes sig) (:produces sig)))
         sw (splice-warnings rules)
         ow (overlap-warnings rules)
         warnings (vec (concat sw ow))]
     (cond-> {:name phase-name
              :rules (vec rules)
              :signature sig
              :head-eliminating? head-elim
              :strategy (or (:strategy opts) :bottom-up)}
       (not head-elim) (assoc :max-iters (or (:max-iters opts) 100))
       (:measure opts) (assoc :measure (:measure opts))
       (seq warnings)  (assoc :warnings warnings)))))

;; ============================================================
;; Phase Execution
;; ============================================================

(defn expr-size
  "Count nodes in an expression tree. Useful as a decreasing measure."
  [expr]
  (if (sequential? expr)
    (reduce + 1 (map expr-size expr))
    1))

(defn- run-rules-once [rules strategy expr]
  (case strategy
    :top-only (let [result (apply-rules rules expr)]
                (if result [true result] [false expr]))
    (rewrite-once rules expr)))

(defn run-phase
  "Execute a phase to fixed point.
   Head-eliminating phases get a high safety cap (1000).
   Non-head-eliminating phases use :max-iters (default 100).
   If :measure is set, verifies it decreases each iteration."
  [phase expr]
  (let [{:keys [rules strategy head-eliminating? max-iters measure]} phase
        cap (or max-iters (if head-eliminating? 1000 100))]
    (loop [expr expr
           i 0
           prev-m (when measure (measure expr))]
      (when (> i cap)
        (throw (ex-info (str "Phase " (:name phase)
                             " did not reach fixed point after " cap " iterations"
                             (when-not head-eliminating? " (non-head-eliminating)"))
                        {:phase (:name phase) :iterations cap :expr expr})))
      (let [[changed? result] (run-rules-once rules strategy expr)]
        (if-not changed?
          result
          (do
            (when measure
              (let [new-m (measure result)]
                (when (>= new-m prev-m)
                  (throw (ex-info (str "Phase " (:name phase)
                                       " measure did not decrease: "
                                       prev-m " → " new-m)
                                  {:phase (:name phase)
                                   :measure-before prev-m
                                   :measure-after new-m
                                   :expr result})))))
            (recur result (inc i) (when measure (measure result)))))))))

;; ============================================================
;; Procedural Phase
;; ============================================================

(defn procedural-phase
  "Create a procedural phase — an opaque transformation function.
   Invisible to head analysis. Use for semantic phases that
   can't be expressed as rewrite rules."
  [phase-name f]
  {:name phase-name
   :procedural? true
   :fn f
   :signature {:consumes #{} :produces #{} :dynamic-produces? false}
   :head-eliminating? true})

;; ============================================================
;; Pipeline
;; ============================================================

(defn- validate-pipeline
  "Check: no phase produces heads consumed by earlier phases.
   Returns violations vector (empty = valid)."
  [phases]
  (loop [i 0 earlier-consumes #{} violations []]
    (if (>= i (count phases))
      violations
      (let [phase (nth phases i)
            produced (get-in phase [:signature :produces])
            raised (set/intersection produced earlier-consumes)
            new-consumes (into earlier-consumes (get-in phase [:signature :consumes]))]
        (recur (inc i) new-consumes
               (if (seq raised)
                 (conj violations
                       {:phase (:name phase) :position i :raised-heads raised
                        :message (str "Phase " (:name phase) " at position " i
                                      " produces heads " raised
                                      " consumed by earlier phases")})
                 violations))))))

(defn valid-pipeline?
  "Does this sequence of phases form a valid pipeline?"
  [phases]
  (empty? (validate-pipeline phases)))

(defn make-pipeline
  "Create a validated pipeline. Throws if any phase raises earlier heads."
  [pipeline-name phases]
  (let [violations (validate-pipeline phases)]
    (when (seq violations)
      (throw (ex-info (str "Invalid pipeline: " (:message (first violations)))
                      {:pipeline pipeline-name :violations violations})))
    {:name pipeline-name
     :phases (vec phases)
     :signature {:consumes (reduce into #{} (map #(get-in % [:signature :consumes]) phases))
                 :produces (reduce into #{} (map #(get-in % [:signature :produces]) phases))}}))

(defn run-pipeline
  "Execute a pipeline: each phase to fixed point in sequence.
   With {:trace true}, returns {:result expr :trace [...]}."
  ([pipeline expr] (run-pipeline pipeline expr nil))
  ([pipeline expr opts]
   (let [trace? (:trace opts)]
     (loop [phases (:phases pipeline)
            expr expr
            trace (when trace? [{:phase :input :expr expr}])]
       (if (empty? phases)
         (if trace? {:result expr :trace trace} expr)
         (let [phase (first phases)
               result (if (:procedural? phase)
                        ((:fn phase) expr)
                        (run-phase phase expr))
               trace' (when trace? (conj trace {:phase (:name phase) :expr result}))]
           (recur (rest phases) result trace')))))))

;; ============================================================
;; Language
;; ============================================================

(defn make-language
  "Create a language: a named pipeline with a target backend.
   The target is a function (expr -> result) — eval, codegen, emission."
  [lang-name pipeline target]
  {:name lang-name :pipeline pipeline :target target})

(defn run-language
  "Run an expression through a language: pipeline then target.
   With {:trace true}, returns full trace."
  ([lang expr] (run-language lang expr nil))
  ([lang expr opts]
   (let [pipeline-result (run-pipeline (:pipeline lang) expr opts)
         trace? (:trace opts)
         final-expr (if trace? (:result pipeline-result) pipeline-result)]
     (if-let [target (:target lang)]
       (let [result (target final-expr)]
         (if trace?
           (assoc pipeline-result :result result :target-input final-expr)
           result))
       (if trace? pipeline-result final-expr)))))

;; ============================================================
;; Collapse — tower flattening
;; ============================================================

(defn collapse
  "Collapse a tower of pipelines into a single flat pipeline.
   Concatenates phases, validates the result. Throws if invalid."
  [name & pipelines]
  (make-pipeline name (vec (mapcat :phases pipelines))))

;; ============================================================
;; Inspection
;; ============================================================

(defn inspect-phase
  "Describe a phase: signature, verification, warnings."
  [phase]
  (cond-> {:name (:name phase)
           :rule-count (count (or (:rules phase) []))
           :consumes (get-in phase [:signature :consumes])
           :produces (get-in phase [:signature :produces])
           :head-eliminating? (:head-eliminating? phase)}
    (:procedural? phase)     (assoc :procedural? true)
    (:max-iters phase)       (assoc :max-iters (:max-iters phase))
    (:measure phase)         (assoc :has-measure? true)
    (get-in phase [:signature :dynamic-produces?])
                             (assoc :dynamic-produces? true)
    (seq (:warnings phase))  (assoc :warnings (:warnings phase))))

(defn inspect-pipeline
  "Describe a pipeline: phases, signatures, validity."
  [pipeline]
  {:name (:name pipeline)
   :phase-count (count (:phases pipeline))
   :phases (mapv inspect-phase (:phases pipeline))
   :consumes (get-in pipeline [:signature :consumes])
   :produces (get-in pipeline [:signature :produces])})

(defn rewrite-trace
  "Trace an expression through a pipeline, returning intermediate results."
  [pipeline expr]
  (:trace (run-pipeline pipeline expr {:trace true})))
