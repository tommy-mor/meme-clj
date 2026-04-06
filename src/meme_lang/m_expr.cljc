(ns meme-lang.m-expr
  "M-expression data reader: #meme/m-expr (f (args...))
   Transforms M-expression notation into S-expressions at read time.
   Rule: (head (args...)) → (head args...) when second element is a list.")

(defn m->s
  "Transform M-expression form to S-expression.
   (f (args)) → (f args...) — two-element list with head + arg list spliced.
   (f x y)    → recurse into each element otherwise.
   Vectors and maps are walked recursively. Atoms pass through unchanged."
  [form]
  (cond
    (seq? form)
    (let [[f args & more] form]
      (if (and (seq? args) (nil? more))
        (cons (m->s f) (map m->s args))
        (map m->s form)))

    (vector? form)
    (mapv m->s form)

    (map? form)
    (into {} (map (fn [[k v]] [(m->s k) (m->s v)])) form)

    (set? form)
    (into #{} (map m->s) form)

    :else form))
