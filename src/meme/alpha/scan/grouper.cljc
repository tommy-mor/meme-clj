(ns meme.alpha.scan.grouper
  "Token grouping stage: pass-through (all forms are now parsed natively
   by the recursive-descent parser). Retained for pipeline symmetry.")

;; ---------------------------------------------------------------------------
;; Group pass
;; ---------------------------------------------------------------------------

(defn group-tokens
  "Pass-through stage — returns tokens unchanged.
   All forms (reader conditionals, namespaced maps, syntax-quote) are now
   parsed natively by the recursive-descent parser. Retained for pipeline symmetry.

   source is accepted for API compatibility but unused."
  [tokens _source]
  tokens)
