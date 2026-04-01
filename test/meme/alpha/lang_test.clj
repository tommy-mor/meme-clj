(ns meme.alpha.lang-test
  "End-to-end tests for lang command maps and EDN loading."
  (:require [clojure.test :refer [deftest is testing]]
            [meme.alpha.lang :as lang]))

;; ============================================================
;; Built-in lang shape
;; ============================================================

(deftest builtin-langs-exist
  (testing "all built-in langs are registered"
    (is (contains? lang/builtin :meme-classic))
    (is (contains? lang/builtin :meme-rewrite))
    (is (contains? lang/builtin :meme-trs))))

(deftest meme-classic-supports-all-commands
  (let [l (:meme-classic lang/builtin)]
    (is (fn? (:run l)))
    (is (fn? (:repl l)))
    (is (fn? (:format l)))
    (is (fn? (:convert l)))))

(deftest meme-rewrite-supports-all-commands
  (let [l (:meme-rewrite lang/builtin)]
    (is (fn? (:run l)))
    (is (fn? (:repl l)))
    (is (fn? (:format l)))
    (is (fn? (:convert l)))))

(deftest meme-trs-has-no-repl
  (let [l (:meme-trs lang/builtin)]
    (is (fn? (:run l)))
    (is (nil? (:repl l)))
    (is (fn? (:format l)))
    (is (fn? (:convert l)))))

;; ============================================================
;; Lang commands work end-to-end
;; ============================================================

(deftest classic-run
  (is (= 42 ((:run (:meme-classic lang/builtin)) "+(21 21)" {}))))

(deftest classic-format
  (let [result ((:format (:meme-classic lang/builtin)) "def(x 42)" {})]
    (is (= "def(x 42)" result))))

(deftest classic-convert-to-clj
  (let [result ((:convert (:meme-classic lang/builtin)) "f(x y)" {:direction :to-clj})]
    (is (= "(f x y)" result))))

(deftest classic-convert-to-meme
  (let [result ((:convert (:meme-classic lang/builtin)) "(f x y)" {:direction :to-meme})]
    (is (= "f(x y)" result))))

(deftest rewrite-run
  (is (= 42 ((:run (:meme-rewrite lang/builtin)) "+(21 21)" {}))))

(deftest rewrite-convert-to-clj
  (let [result ((:convert (:meme-rewrite lang/builtin)) "f(x y)" {:direction :to-clj})]
    (is (= "(f x y)" result))))

(deftest trs-convert-to-clj
  (let [result ((:convert (:meme-trs lang/builtin)) "f(x y)" {:direction :to-clj})]
    (is (= "(f x y)" result))))

;; ============================================================
;; check-support!
;; ============================================================

(deftest check-support-passes
  (lang/check-support! (:meme-classic lang/builtin) :meme-classic :run)
  (is true "should not throw"))

(deftest check-support-fails
  (is (thrown-with-msg? Exception #"does not support :repl"
        (lang/check-support! (:meme-trs lang/builtin) :meme-trs :repl))))

;; ============================================================
;; EDN lang loading
;; ============================================================

(deftest load-edn-calc
  (testing "calc lang.edn loads and :run works"
    (let [l (lang/load-edn "examples/languages/calc/pipeline.edn")]
      (is (fn? (:run l)))
      (is (fn? (:format l)))
      (is (= 'x ((:run l) "simplify('+(*(1 x) 0))" {}))))))

(deftest load-edn-prefix
  (testing "prefix lang.edn loads and :run works"
    (let [l (lang/load-edn "examples/languages/prefix/pipeline.edn")]
      (is (fn? (:run l))))))

(deftest load-edn-format-delegates
  (testing ":format :meme-classic in EDN resolves to built-in format"
    (let [l (lang/load-edn "examples/languages/calc/pipeline.edn")]
      (is (= "def(x 42)" ((:format l) "def(x 42)" {}))))))

(deftest load-edn-run-evals-core-then-user
  (testing "EDN :run evals core.meme before user source"
    (spit "/tmp/test-edn-lang-core.meme" "defn(double [x] *(2 x))")
    (spit "/tmp/test-edn-lang.edn" "{:run \"/tmp/test-edn-lang-core.meme\"}")
    (let [l (lang/load-edn "/tmp/test-edn-lang.edn")]
      (is (= 84 ((:run l) "double(42)" {}))))))

;; ============================================================
;; All langs agree on convert
;; ============================================================

(deftest all-langs-agree-on-convert
  (testing "all meme langs produce same clj for basic inputs"
    (doseq [src ["f(x y)" "+(1 2)" "def(x 42)" "[1 2 3]"]]
      (let [classic ((:convert (:meme-classic lang/builtin)) src {:direction :to-clj})
            rewrite ((:convert (:meme-rewrite lang/builtin)) src {:direction :to-clj})
            trs     ((:convert (:meme-trs lang/builtin)) src {:direction :to-clj})]
        (is (= classic rewrite) (str "classic vs rewrite on: " src))
        (is (= classic trs) (str "classic vs trs on: " src))))))
