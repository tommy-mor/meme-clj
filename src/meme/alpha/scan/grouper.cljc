(ns meme.alpha.scan.grouper
  "Vestigial token grouping stage — identity function.
   All forms are now parsed natively by the recursive-descent parser.
   No longer part of the pipeline; retained for backward compatibility
   with test helpers that call group-tokens directly.")

(defn group-tokens
  "Identity — returns tokens unchanged.
   Retained for backward compatibility. Not part of the pipeline."
  [tokens _source]
  tokens)
