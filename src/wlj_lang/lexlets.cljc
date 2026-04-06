(ns wlj-lang.lexlets
  "Lexlets for wlj — delegates to meme.tools.lexer for common patterns.
   Only language-specific overrides here."
  (:require [meme.tools.lexer :as lexer]))

;; Re-export common lexlets
(def digit?             lexer/digit?)
(def consume-number     lexer/consume-number)
(def consume-string     lexer/consume-string)
(def newline-consumer   lexer/newline-consumer)
(def block-comment-consumer lexer/block-comment-consumer)

;; wlj-specific: ident-start includes $
(defn ident-start? [ch]
  (or (lexer/ident-start? ch) (and ch (= ch \$))))

;; wlj-specific: consume-identifier allows trailing ? or ! for Clojure interop
(defn consume-identifier
  "Consume [a-zA-Z_$][a-zA-Z0-9_$]*[?!]?"
  [^String source ^long len ^long pos]
  (let [end (lexer/consume-identifier source len pos)]
    ;; Allow one trailing ? or ! for Clojure predicates/mutators
    (if (and (< end len)
             (let [ch (.charAt source end)] (or (= ch \?) (= ch \!))))
      (inc end)
      end)))

;; wlj-specific: comma is whitespace
(defn ws-consumer [engine]
  (lexer/ws-consumer engine (fn [ch] (or (= ch \space) (= ch \tab) (= ch \,)))))
