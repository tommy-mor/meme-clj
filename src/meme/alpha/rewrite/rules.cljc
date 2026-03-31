(ns meme.alpha.rewrite.rules
  "Rewrite rule sets for S→M and M→S transformations.
   Each direction is a vector of rules for meme.alpha.rewrite/rewrite."
  (:require [meme.alpha.rewrite :as rw]))

;; ============================================================
;; S→M: Clojure forms → M-expression tagged tree
;;
;; Input:  (defn foo [x] (+ x 1))
;; Output: (m-call defn foo [x] (m-call + x 1))
;;
;; The m-call tag marks "this was a list with a callable head."
;; Vectors, maps, sets pass through unchanged.
;; ============================================================

(def s->m-rules
  "Rules that tag S-expression calls as m-call nodes.
   Apply bottom-up so inner calls are tagged before outer ones.
   List patterns only match lists (not vectors) — the engine distinguishes them."
  [(rw/rule '(?f ??args) '(m-call ?f ??args)
            (fn [bindings]
              (let [f (get bindings 'f)]
                (and (or (symbol? f) (keyword? f))
                     (not= f 'm-call)))))])

;; ============================================================
;; M→S: M-expression tagged tree → Clojure forms
;;
;; Input:  (m-call defn foo [x] (m-call + x 1))
;; Output: (defn foo [x] (+ x 1))
;; ============================================================

(def m->s-rules
  "Rules that convert m-call nodes back to S-expression lists."
  [(rw/rule '(m-call ?f ??args) '(?f ??args))])

;; ============================================================
;; Tree→S: tagged tree (from tree-builder) → Clojure forms
;;
;; Input:  (m-call defn foo (bracket x) (m-call + x 1))
;; Output: (defn foo [x] (+ x 1))
;;
;; Handles: m-call, bracket, brace, set-lit, paren, anon-fn,
;;          meme/quote, meme/deref, meme/var, meme/meta
;;
;; Note: bracket→vector, brace→map, set-lit→set cannot be expressed
;; as pure rules because substitute only produces lists. These are
;; handled by transform-structures after rule application.
;; ============================================================

(def tree->s-rules
  "Rules that flatten tagged tree nodes to Clojure forms.
   Apply bottom-up. Data structure rules (bracket, brace, set-lit) are
   handled by transform-structures — the engine can't produce non-list types."
  [;; calls
   (rw/rule '(m-call ?f ??args) '(?f ??args))
   ;; bare parens → plain list
   (rw/rule '(paren ??items) '(??items))
   ;; prefix sugar
   (rw/rule '(meme/quote ?x) '(quote ?x))
   (rw/rule '(meme/deref ?x) '(clojure.core/deref ?x))
   (rw/rule '(meme/var ?x) '(var ?x))])

(defn transform-structures
  "Walk a tree and convert structural tags to Clojure data.
   Called after tree->s-rules to handle types rules can't produce."
  [form]
  (cond
    (and (seq? form) (seq form))
    (let [head (first form)
          children (map transform-structures (rest form))]
      (case head
        bracket (vec children)
        brace (apply hash-map children)
        set-lit (set children)
        ;; default: recurse into list
        (apply list (transform-structures head) children)))

    (vector? form) (mapv transform-structures form)
    (map? form) (into {} (map (fn [[k v]] [(transform-structures k)
                                            (transform-structures v)]) form))
    (set? form) (set (map transform-structures form))
    :else form))
