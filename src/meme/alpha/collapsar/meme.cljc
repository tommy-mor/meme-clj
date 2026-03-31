(ns meme.alpha.collapsar.meme
  "Meme ↔ Clojure as collapsar pipelines.

   Two pipelines:
     meme->clj: meme text → tokenize → tree → [m→s rules] → transform → clj text
     clj->meme: clj text → read → [s→m rules] → meme text

   The rule phases are collapsar phases with head analysis and verification.
   The procedural phases wrap tokenizer, tree builder, and emitters."
  (:require [meme.alpha.collapsar :as c]
            [meme.alpha.scan.tokenizer :as tokenizer]
            [meme.alpha.rewrite.tree :as tree]
            [meme.alpha.rewrite.rules :as rules]
            [meme.alpha.rewrite.emit :as remit]
            [meme.alpha.core :as core]
            [meme.alpha.emit.formatter.flat :as fmt-flat]
            [meme.alpha.parse.expander :as expander]))

;; ============================================================
;; Rule Phases — the rewrite rules expressed as collapsar phases
;; ============================================================

(def m->s-phase
  "Rule phase: flatten m-call and paren tags to S-expressions.
   Head-eliminating: consumes m-call/paren, produces dynamic heads only."
  (c/make-phase :m->s
    [(c/named-rule :unfold-call '(m-call ?f ??args) '(?f ??args))
     (c/named-rule :unfold-paren '(paren ??items) '(??items))]))

(def s->m-phase
  "Rule phase: tag S-expression calls as m-call nodes.
   Guard ensures only symbol/keyword-headed lists are tagged,
   and prevents re-tagging already-tagged nodes."
  (c/make-phase :s->m
    [(c/rule '(?f ??args) '(m-call ?f ??args)
             (fn [bindings]
               (let [f (get bindings 'f)]
                 (and (or (symbol? f) (keyword? f))
                      (not= f 'm-call)))))]))

;; ============================================================
;; Procedural Phases
;; ============================================================

(def tokenize+tree-phase
  "Procedural: meme source string → vector of tagged tree forms.
   Combines tokenization and tree building in one step."
  (c/procedural-phase :tokenize+tree
    (fn [source]
      (let [tokens (tokenizer/attach-whitespace (tokenizer/tokenize source) source)]
        (tree/tokens->tree tokens)))))

(defn- make-transform-structures-phase
  "Procedural: convert structural tags to Clojure data types.
   Passes opts through (supports :read-cond :preserve)."
  [opts]
  (c/procedural-phase :transform-structures
    (fn [forms]
      (mapv #(rules/transform-structures % opts) forms))))

(def transform-structures-phase
  "Default transform-structures phase (evaluate reader conditionals)."
  (make-transform-structures-phase nil))

(def expand-syntax-quotes-phase
  "Procedural: expand syntax-quote AST nodes to plain Clojure forms.
   Required for forms->clj since Clojure has no backtick form."
  (c/procedural-phase :expand-syntax-quotes
    (fn [forms]
      (expander/expand-forms forms))))

(def forms->clj-phase
  "Procedural: Clojure forms → Clojure source string."
  (c/procedural-phase :forms->clj
    (fn [forms]
      (fmt-flat/format-clj forms))))

(def rewrite-inside-rcs-phase
  "Procedural: apply S→M tagging inside ReaderConditional branch values.
   The collapsar rewrite engine doesn't descend into ReaderConditionals,
   so this step ensures calls inside #?(:clj ...) branches get tagged."
  (c/procedural-phase :rewrite-inside-rcs
    (fn [forms]
      (let [tag-fn (fn [form] (c/run-phase s->m-phase form))]
        (mapv #(rules/rewrite-inside-reader-conditionals tag-fn %) forms)))))

(def emit-meme-phase
  "Procedural: m-call tagged forms → meme source string."
  (c/procedural-phase :emit-meme
    (fn [forms]
      (remit/emit-forms forms))))

;; ============================================================
;; Per-form rule phase wrapper
;;
;; Collapsar's run-phase operates on a single expression.
;; The meme pipeline works on vectors of forms. This wrapper
;; maps run-phase over each form in the vector.
;; ============================================================

(defn- per-form-phase
  "Wrap a collapsar rule phase to operate on a vector of forms.
   Preserves the underlying phase's signature for pipeline inspection."
  [phase]
  (-> (c/procedural-phase (:name phase)
        (fn [forms]
          (mapv #(c/run-phase phase %) forms)))
      (assoc :signature (:signature phase)
             :rules (:rules phase)
             :head-eliminating? (:head-eliminating? phase))))

;; ============================================================
;; Pipelines
;; ============================================================

(defn- make-meme->clj-pipeline
  "Build meme→clj pipeline with the given opts (supports :read-cond :preserve)."
  [opts]
  (c/make-pipeline :meme->clj
    [tokenize+tree-phase
     (per-form-phase m->s-phase)
     (make-transform-structures-phase opts)
     expand-syntax-quotes-phase
     forms->clj-phase]))

(def meme->clj-pipeline
  "Default meme→clj pipeline (evaluates reader conditionals)."
  (make-meme->clj-pipeline nil))

(def clj->meme-pipeline
  "Collapsar pipeline: Clojure forms → meme source.
   Phases: s→m rules → rewrite inside RCs → emit meme."
  (c/make-pipeline :clj->meme
    [(per-form-phase s->m-phase)
     rewrite-inside-rcs-phase
     emit-meme-phase]))

;; ============================================================
;; Convenience functions — text to text
;; ============================================================

(defn meme->clj
  "Convert meme source string to Clojure source string via collapsar pipeline.
   opts supports :read-cond :preserve to preserve reader conditionals."
  ([meme-src] (meme->clj meme-src nil))
  ([meme-src opts]
   (let [pipeline (if opts (make-meme->clj-pipeline opts) meme->clj-pipeline)]
     (c/run-pipeline pipeline meme-src))))

(defn meme->clj-traced
  "Convert meme→clj with full pipeline trace."
  ([meme-src] (meme->clj-traced meme-src nil))
  ([meme-src opts]
   (let [pipeline (if opts (make-meme->clj-pipeline opts) meme->clj-pipeline)]
     (c/run-pipeline pipeline meme-src {:trace true}))))

#?(:clj
   (defn clj->meme
     "Convert Clojure source string to meme source string via collapsar pipeline.
   JVM/Babashka only — requires Clojure reader."
     [clj-src]
     (c/run-pipeline clj->meme-pipeline (core/clj->forms clj-src))))

#?(:clj
   (defn clj->meme-traced
     "Convert clj→meme with full pipeline trace."
     [clj-src]
     (c/run-pipeline clj->meme-pipeline (core/clj->forms clj-src) {:trace true})))

;; ============================================================
;; Pipeline inspection
;; ============================================================

(defn inspect
  "Inspect both pipelines: phases, signatures, verification."
  []
  {:meme->clj (c/inspect-pipeline meme->clj-pipeline)
   :clj->meme (c/inspect-pipeline clj->meme-pipeline)})
