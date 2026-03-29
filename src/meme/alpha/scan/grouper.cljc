(ns meme.alpha.scan.grouper
  "Token grouping stage: pass-through (all forms are now parsed natively
   by the recursive-descent parser). Retained for pipeline symmetry.")

;; ---------------------------------------------------------------------------
;; Group pass
;; ---------------------------------------------------------------------------

(defn group-tokens
  "Process a flat token vector, collapsing opaque regions into single tokens.
   Marker tokens (:reader-cond-start, :namespaced-map-start, :syntax-quote-start)
   followed by balanced delimiters are collapsed into the corresponding
   -raw composite tokens.

   source is the original source text, used for reconstructing raw values."
  [tokens source]
  (let [n (count tokens)]
    (loop [i 0 out (transient [])]
      (if (>= i n)
        (persistent! out)
        (let [tok (nth tokens i)
              typ (:type tok)]
          (case typ
            ;; Reader conditional: #?(...) or #?@(...) — pass through for native parsing
            :reader-cond-start
            (recur (inc i) (conj! out tok))

            ;; Namespaced map: #:ns{...} — pass through for native parsing
            :namespaced-map-start
            (recur (inc i) (conj! out tok))

            ;; Syntax-quote: ` — pass through as prefix (like ' for quote)
            :syntax-quote
            (recur (inc i) (conj! out tok))

            ;; All other tokens pass through unchanged
            (recur (inc i) (conj! out tok))))))))
