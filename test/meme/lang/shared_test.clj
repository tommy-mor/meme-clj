(ns meme.lang.shared-test
  (:require [clojure.test :refer [deftest is testing]]
            [meme.lang.shared :as shared]
            [meme.core :as core]))

;; ============================================================
;; clj->meme-text
;; ============================================================

(deftest clj->meme-text-basic-call
  (testing "simple function call"
    (let [result (shared/clj->meme-text "(+ 1 2)")]
      (is (string? result))
      (is (re-find #"\+" result)))))

(deftest clj->meme-text-defn
  (testing "defn converts to meme syntax"
    (let [result (shared/clj->meme-text "(defn foo [x] (+ x 1))")]
      (is (string? result))
      (is (re-find #"defn" result))
      (is (re-find #"foo" result)))))

(deftest clj->meme-text-let
  (testing "let form converts to meme syntax"
    (let [result (shared/clj->meme-text "(let [x 1] x)")]
      (is (string? result))
      (is (re-find #"let" result)))))

(deftest clj->meme-text-multiple-forms
  (testing "multiple top-level forms"
    (let [result (shared/clj->meme-text "(def a 1) (def b 2)")]
      (is (string? result))
      (is (re-find #"a" result))
      (is (re-find #"b" result)))))

;; ============================================================
;; Roundtrip: clj->meme-text then meme->clj
;; ============================================================

(deftest roundtrip-simple-call
  (testing "roundtrip preserves semantics"
    (let [original "(+ 1 2)"
          meme-text (shared/clj->meme-text original)
          clj-back (core/meme->clj meme-text)]
      (is (= original clj-back)))))

(deftest roundtrip-defn
  (testing "roundtrip defn"
    (let [original "(defn foo [x] (+ x 1))"
          meme-text (shared/clj->meme-text original)
          clj-back (core/meme->clj meme-text)]
      (is (= original clj-back)))))

(deftest roundtrip-nested
  (testing "roundtrip nested forms"
    (let [original "(if (> x 0) (inc x) (dec x))"
          meme-text (shared/clj->meme-text original)
          clj-back (core/meme->clj meme-text)]
      (is (= original clj-back)))))

(deftest clj->meme-text-two-arity
  (testing "two-arity version works the same"
    (let [one (shared/clj->meme-text "(+ 1 2)")
          two (shared/clj->meme-text "(+ 1 2)" {})]
      (is (= one two)))))
