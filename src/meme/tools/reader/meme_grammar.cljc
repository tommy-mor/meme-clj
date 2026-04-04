(ns meme.tools.reader.meme-grammar
  "Meme language grammar spec.

   Maps characters to scanlets — the complete syntactic specification
   of M-expression syntax as data. Lexlets provide the scanning layer,
   parselets provide the compound constructs, and the parser engine
   provides generic factories."
  (:require [meme.tools.pratt.parser :as pratt]
            [meme.tools.reader.meme-lexlets :as lex]
            [meme.tools.reader.meme-parselets :as mp]))

(def grammar
  "Meme language grammar: characters → scanlets."
  {:nud
   {;; Delimiters
    \(  (lex/single-char-scanlet :open-paren
          (pratt/nud-empty-or-error :list \) :close-paren
            "Bare parentheses not allowed — every (...) needs a head"))
    \[  (lex/delimited-scanlet :vector :open-bracket \] :close-bracket)
    \{  (lex/delimited-scanlet :map :open-brace \} :close-brace)

    ;; Prefix operators
    \'  (lex/single-char-scanlet :quote (pratt/nud-prefix :quote))
    \@  (lex/single-char-scanlet :deref (pratt/nud-prefix :deref))
    \`  (lex/single-char-scanlet :syntax-quote (pratt/nud-prefix :syntax-quote))
    \^  (lex/single-char-scanlet :meta (pratt/nud-prefix-two :meta :meta :target))
    \~  mp/tilde-scanlet

    ;; Content atoms
    \\  (lex/atom-scanlet :char-literal lex/consume-char-literal)
    \"  (lex/atom-scanlet :string lex/consume-string)
    \:  (lex/atom-scanlet :keyword lex/consume-keyword)

    ;; Dispatch
    \#  mp/dispatch-scanlet}

   :nud-pred
   [[(fn [ch _e] (lex/digit? ch))                          (lex/atom-scanlet :number lex/consume-number)]
    [(fn [ch e] (mp/sign-followed-by-digit? e ch))          (lex/atom-scanlet :number lex/consume-number)]
    [(fn [ch _e] (lex/symbol-start? ch))                    (lex/atom-scanlet :symbol lex/consume-symbol)]]

   :trivia
   {\space   lex/ws-consumer
    \tab     lex/ws-consumer
    \,       lex/ws-consumer
    \newline lex/newline-consumer
    \return  lex/newline-consumer
    \;       lex/comment-consumer
    \uFEFF   lex/bom-consumer}

   :trivia-pred
   [[lex/whitespace-char? lex/ws-consumer]
    [lex/newline-char?    lex/newline-consumer]]

   :led
   [{:char \( :bp 100 :open-type :open-paren :when mp/adjacent? :fn mp/call-scanlet}]})
