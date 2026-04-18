(ns meme-lang.stages-test
  "Tests for the experimental pipeline stages, including
   step-expand-syntax-quotes and expand-forms."
  (:require [clojure.test :refer [deftest is testing]]
            [meme-lang.stages :as stages]
            [meme-lang.forms :as forms]))

;; ---------------------------------------------------------------------------
;; Full pipeline: scan → trivia → parse → read
;; ---------------------------------------------------------------------------

(deftest full-pipeline-basic
  (testing "simple call"
    (let [ctx (stages/run "+(1 2)")]
      (is (= '[(+ 1 2)] (:forms ctx)))))
  (testing "empty string"
    (let [ctx (stages/run "")]
      (is (= [] (:forms ctx)))))
  (testing "multiple forms"
    (let [ctx (stages/run "def(x 42)\nx")]
      (is (= '[(def x 42) x] (:forms ctx))))))

(deftest full-pipeline-context-keys
  (testing "context contains expected keys"
    (let [ctx (stages/run "+(1 2)")]
      (is (string? (:source ctx)))
      (is (vector? (:cst ctx)))
      (is (vector? (:forms ctx))))))

;; ---------------------------------------------------------------------------
;; step-expand-syntax-quotes
;; ---------------------------------------------------------------------------

(deftest step-expand-syntax-quotes-basic
  (testing "expands syntax-quote AST nodes"
    (let [ctx (-> (stages/run "`foo")
                  stages/step-expand-syntax-quotes)
          form (first (:forms ctx))]
      ;; `foo expands to (quote foo) — no longer a MemeSyntaxQuote record
      (is (seq? form))
      (is (= 'quote (first form)))
      (is (not (forms/syntax-quote? form)))))
  #?(:clj
     (testing "unwraps MemeRaw values"
       (let [ctx (-> (stages/run "0xFF")
                     stages/step-expand-syntax-quotes)]
         (is (= 255 (first (:forms ctx))))
         (is (not (forms/raw? (first (:forms ctx))))))))
  (testing "passes through plain forms unchanged"
    (let [ctx (-> (stages/run "+(1 2)")
                  stages/step-expand-syntax-quotes)]
      (is (= '[(+ 1 2)] (:forms ctx))))))

;; ---------------------------------------------------------------------------
;; expand-forms convenience
;; ---------------------------------------------------------------------------

(deftest expand-forms-test
  #?(:clj
     (testing "expands a vector of forms"
       (let [forms (:forms (stages/run "0xFF"))
             expanded (stages/expand-syntax-quotes forms {})]
         (is (= [255] expanded)))))
  (testing "empty forms"
    (is (= [] (stages/expand-syntax-quotes [] {})))))

;; ---------------------------------------------------------------------------
;; Pipeline with expansion — full eval path
;; ---------------------------------------------------------------------------

(deftest pipeline-with-expansion
  (testing "full pipeline + expansion produces eval-ready forms"
    (let [ctx (-> (stages/run "`map")
                  stages/step-expand-syntax-quotes)
          form (first (:forms ctx))]
      ;; After expansion, `map becomes (quote map) — plain Clojure, no AST records
      (is (seq? form))
      (is (= 'quote (first form)))
      (is (not (forms/syntax-quote? form))))))

;; ---------------------------------------------------------------------------
;; Incomplete error detection
;; ---------------------------------------------------------------------------

(deftest incomplete-errors
  (testing "unclosed paren produces :incomplete error"
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                 (stages/run "+(1 2")))
    (try (stages/run "+(1 2")
         (catch #?(:clj Exception :cljs :default) e
           (is (:incomplete (ex-data e))))))
  (testing "unclosed bracket produces :incomplete error"
    (try (stages/run "[1 2 3")
         (catch #?(:clj Exception :cljs :default) e
           (is (:incomplete (ex-data e))))))
  (testing "unclosed brace produces :incomplete error"
    (try (stages/run "{:a 1")
         (catch #?(:clj Exception :cljs :default) e
           (is (:incomplete (ex-data e))))))
  (testing "valid input does not produce :incomplete"
    (is (vector? (:forms (stages/run "+(1 2)"))))))

;; ---------------------------------------------------------------------------
;; Pipeline contract — miscomposed stages throw clear errors
;; ---------------------------------------------------------------------------

(deftest stage-contracts-are-exposed
  (testing "stage-contracts is public data, one entry per stage"
    (is (= #{:step-parse :step-read :step-expand-syntax-quotes}
           (set (keys stages/stage-contracts))))
    (is (= #{:source} (get-in stages/stage-contracts [:step-parse :requires])))
    (is (= #{:cst}    (get-in stages/stage-contracts [:step-read :requires])))
    (is (= #{:forms}  (get-in stages/stage-contracts [:step-expand-syntax-quotes :requires])))))

(deftest step-read-without-parse-throws-pipeline-error
  (testing "calling step-read without :cst in ctx fails with pipeline-error"
    (try (stages/step-read {:source "+(1 2)"})
         (is false "step-read should have thrown")
         (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) e
           (let [data (ex-data e)]
             (is (= :meme-lang/pipeline-error (:type data)))
             (is (= :step-read (:stage data)))
             (is (contains? (set (:missing data)) :cst))
             (is (re-find #"missing required ctx key" (ex-message e))))))))

(deftest step-expand-without-read-throws-pipeline-error
  (testing "calling step-expand-syntax-quotes without :forms fails with pipeline-error"
    (try (stages/step-expand-syntax-quotes {:source "x" :cst []})
         (is false "step-expand should have thrown")
         (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) e
           (let [data (ex-data e)]
             (is (= :meme-lang/pipeline-error (:type data)))
             (is (= :step-expand-syntax-quotes (:stage data)))
             (is (contains? (set (:missing data)) :forms)))))))

(deftest step-parse-without-source-throws-pipeline-error
  (testing "calling step-parse without :source fails with pipeline-error"
    (try (stages/step-parse {})
         (is false "step-parse should have thrown")
         (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) e
           (let [data (ex-data e)]
             (is (= :meme-lang/pipeline-error (:type data)))
             (is (= :step-parse (:stage data)))
             (is (contains? (set (:missing data)) :source)))))))

(deftest step-parse-with-non-string-source-still-type-checks
  (testing "type check on :source value runs after presence check"
    (try (stages/step-parse {:source 42})
         (is false "step-parse should have thrown")
         (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) e
           (let [data (ex-data e)]
             (is (= :meme-lang/pipeline-error (:type data)))
             (is (= :step-parse (:stage data)))
             (is (re-find #"must be a string" (ex-message e))))))))

;; ---------------------------------------------------------------------------
;; Shebang stripping
;; ---------------------------------------------------------------------------

(deftest shebang-stripping
  (testing "shebang line is stripped"
    (let [ctx (stages/run "#!/usr/bin/env bb\n+(1 2)")]
      (is (= '[(+ 1 2)] (:forms ctx)))))
  (testing "shebang with \\r\\n line ending"
    (let [ctx (stages/run "#!/usr/bin/env bb\r\n+(1 2)")]
      (is (= '[(+ 1 2)] (:forms ctx)))))
  (testing "shebang with bare \\r line ending"
    (let [ctx (stages/run "#!/usr/bin/env bb\r+(1 2)")]
      (is (= '[(+ 1 2)] (:forms ctx)))))
  (testing "shebang-only file with no newline"
    (let [ctx (stages/run "#!/usr/bin/env bb")]
      (is (= [] (:forms ctx)))))
  (testing "no shebang — normal parsing"
    (let [ctx (stages/run "+(1 2)")]
      (is (= '[(+ 1 2)] (:forms ctx))))))
