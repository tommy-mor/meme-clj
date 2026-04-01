(ns meme.parse.reader.data-literals-test
  "Parser tests for data literal passthrough (vectors, maps, sets, keywords, numbers)."
  (:require [clojure.test :refer [deftest is]]
            [meme.core :as core]
            [meme.forms :as forms]))

(deftest parse-vector-literal
  (is (= '[[1 2 3]] (core/meme->forms "[1 2 3]"))))

(deftest parse-map-literal
  (let [result (first (core/meme->forms "{:name \"Andriy\" :age 45}"))]
    (is (map? result))
    (is (= "Andriy" (:name result)))))

(deftest parse-set-literal
  (is (= #{1 2 3} (first (core/meme->forms "#{1 2 3}")))))

(deftest parse-nested-data
  (let [result (first (core/meme->forms "{:users [{:name \"a\"} {:name \"b\"}]}"))]
    (is (vector? (:users result)))))

(deftest parse-empty-collections
  (is (= [] (first (core/meme->forms "[]"))))
  (is (= {} (first (core/meme->forms "{}"))))
  (is (= #{} (first (core/meme->forms "#{}")))))

(deftest parse-negative-number
  (is (= [-1] (core/meme->forms "-1"))))

(deftest parse-negative-float
  (is (= [-3.14] (core/meme->forms "-3.14"))))

(deftest parse-auto-resolve-keyword
  #?(:clj
     (let [form (first (core/meme->forms "::local"))]
       (is (forms/deferred-auto-keyword? form))
       (is (= "::local" (forms/deferred-auto-keyword-raw form))))
     :cljs
     (is (thrown-with-msg? js/Error #"resolve-keyword"
                           (core/meme->forms "::local")))))

(deftest parse-namespaced-keyword
  (is (= :foo/bar (first (core/meme->forms ":foo/bar")))))

;; ---------------------------------------------------------------------------
;; Extended data literal coverage
;; ---------------------------------------------------------------------------

(deftest parse-nested-maps
  (let [result (first (core/meme->forms "{:a {:b {:c 1}}}"))]
    (is (= 1 (get-in result [:a :b :c])))))

(deftest parse-numeric-keys-in-maps
  (let [result (first (core/meme->forms "{1 \"one\" 2 \"two\"}"))]
    (is (map? result))
    (is (= "one" (get result 1)))))

(deftest parse-keyword-as-function
  (let [result (first (core/meme->forms ":name({:name \"foo\"})"))]
    (is (= :name (first result)))
    (is (map? (second result)))))

(deftest parse-comma-as-whitespace
  (is (= '[[1 2 3]] (core/meme->forms "[1, 2, 3]")))
  (let [result (first (core/meme->forms "{:a 1, :b 2}"))]
    (is (= 1 (:a result)))
    (is (= 2 (:b result)))))

(deftest parse-deeply-nested-vectors
  (is (= [[[1]]] (first (core/meme->forms "[[[1]]]")))))

(deftest parse-mixed-collection-nesting
  (let [result (first (core/meme->forms "[{:a #{1 2}} {:b [3 4]}]"))]
    (is (vector? result))
    (is (= 2 (count result)))
    (is (set? (:a (first result))))
    (is (vector? (:b (second result))))))
