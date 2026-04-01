(ns meme.alpha.pipelines-test
  "End-to-end tests for pipeline-as-command-map and EDN pipeline loading."
  (:require [clojure.test :refer [deftest is testing]]
            [meme.alpha.pipelines :as pipelines]))

;; ============================================================
;; Built-in pipeline shape
;; ============================================================

(deftest builtin-pipelines-exist
  (testing "all built-in pipelines are registered"
    (is (contains? pipelines/builtin :meme-classic))
    (is (contains? pipelines/builtin :meme-rewrite))
    (is (contains? pipelines/builtin :meme-trs))))

(deftest meme-classic-supports-all-commands
  (let [p (:meme-classic pipelines/builtin)]
    (is (fn? (:run p)))
    (is (fn? (:repl p)))
    (is (fn? (:format p)))
    (is (fn? (:convert p)))))

(deftest meme-rewrite-supports-all-commands
  (let [p (:meme-rewrite pipelines/builtin)]
    (is (fn? (:run p)))
    (is (fn? (:repl p)))
    (is (fn? (:format p)))
    (is (fn? (:convert p)))))

(deftest meme-trs-has-no-repl
  (let [p (:meme-trs pipelines/builtin)]
    (is (fn? (:run p)))
    (is (nil? (:repl p)))
    (is (fn? (:format p)))
    (is (fn? (:convert p)))))

;; ============================================================
;; Pipeline commands work end-to-end
;; ============================================================

(deftest classic-run
  (is (= 42 ((:run (:meme-classic pipelines/builtin)) "+(21 21)" {}))))

(deftest classic-format
  (let [result ((:format (:meme-classic pipelines/builtin)) "def(x 42)" {})]
    (is (= "def(x 42)" result))))

(deftest classic-convert-to-clj
  (let [result ((:convert (:meme-classic pipelines/builtin)) "f(x y)" {:direction :to-clj})]
    (is (= "(f x y)" result))))

(deftest classic-convert-to-meme
  (let [result ((:convert (:meme-classic pipelines/builtin)) "(f x y)" {:direction :to-meme})]
    (is (= "f(x y)" result))))

(deftest rewrite-run
  (is (= 42 ((:run (:meme-rewrite pipelines/builtin)) "+(21 21)" {}))))

(deftest rewrite-convert-to-clj
  (let [result ((:convert (:meme-rewrite pipelines/builtin)) "f(x y)" {:direction :to-clj})]
    (is (= "(f x y)" result))))

(deftest trs-convert-to-clj
  (let [result ((:convert (:meme-trs pipelines/builtin)) "f(x y)" {:direction :to-clj})]
    (is (= "(f x y)" result))))

;; ============================================================
;; check-support!
;; ============================================================

(deftest check-support-passes
  (pipelines/check-support! (:meme-classic pipelines/builtin) :meme-classic :run)
  (is true "should not throw"))

(deftest check-support-fails
  (is (thrown-with-msg? Exception #"does not support :repl"
        (pipelines/check-support! (:meme-trs pipelines/builtin) :meme-trs :repl))))

;; ============================================================
;; EDN pipeline loading
;; ============================================================

(deftest load-edn-pipeline-calc
  (testing "calc pipeline.edn loads and :run works"
    (let [p (pipelines/load-edn-pipeline "examples/languages/calc/pipeline.edn")]
      (is (fn? (:run p)))
      (is (fn? (:format p)))
      (is (= 'x ((:run p) "simplify('+(*(1 x) 0))" {}))))))

(deftest load-edn-pipeline-prefix
  (testing "prefix pipeline.edn loads and :run works"
    (let [p (pipelines/load-edn-pipeline "examples/languages/prefix/pipeline.edn")]
      (is (fn? (:run p))))))

(deftest load-edn-pipeline-format-delegates
  (testing ":format :meme-classic in EDN resolves to built-in format"
    (let [p (pipelines/load-edn-pipeline "examples/languages/calc/pipeline.edn")]
      (is (= "def(x 42)" ((:format p) "def(x 42)" {}))))))

(deftest load-edn-pipeline-run-evals-core-then-user
  (testing "EDN :run evals core.meme before user source"
    (spit "/tmp/test-edn-pipeline-core.meme" "defn(double [x] *(2 x))")
    (spit "/tmp/test-edn-pipeline.edn" "{:run \"/tmp/test-edn-pipeline-core.meme\"}")
    (let [p (pipelines/load-edn-pipeline "/tmp/test-edn-pipeline.edn")]
      (is (= 84 ((:run p) "double(42)" {}))))))

;; ============================================================
;; Pipeline agreement across pipelines (via :convert)
;; ============================================================

(deftest all-pipelines-agree-on-convert
  (testing "all meme pipelines produce same clj for basic inputs"
    (doseq [src ["f(x y)" "+(1 2)" "def(x 42)" "[1 2 3]"]]
      (let [classic ((:convert (:meme-classic pipelines/builtin)) src {:direction :to-clj})
            rewrite ((:convert (:meme-rewrite pipelines/builtin)) src {:direction :to-clj})
            trs     ((:convert (:meme-trs pipelines/builtin)) src {:direction :to-clj})]
        (is (= classic rewrite) (str "classic vs rewrite on: " src))
        (is (= classic trs) (str "classic vs trs on: " src))))))
