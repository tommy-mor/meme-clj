(ns examples.m-expr-demo
  (:require [meme-lang.m-expr]))

;; M-expressions in a Clojure file via #meme/m-expr
;; Rule: (f (args...)) → (f args...) when exactly two elements

#meme/m-expr
(defn (fizzbuzz [n]
  (cond
    ((= (0 (mod (n 15)))) "FizzBuzz"
     (= (0 (mod (n 3))))  "Fizz"
     (= (0 (mod (n 5))))  "Buzz"
     :else                (str (n))))))

#meme/m-expr
(defn (main []
  (doseq ([i (range (1 101))]
    (println ((fizzbuzz (i))))))))

(main)
