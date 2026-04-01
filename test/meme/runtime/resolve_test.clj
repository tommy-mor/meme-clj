(ns meme.runtime.resolve-test
  "Tests for default symbol resolution in syntax-quote expansion."
  (:require [clojure.test :refer [deftest is testing]]
            [meme.runtime.resolve :as resolve]))

(deftest already-qualified-preserved
  (testing "namespace-qualified symbols are returned unchanged"
    (is (= 'clojure.core/map (resolve/default-resolve-symbol 'clojure.core/map)))
    (is (= 'my.ns/foo (resolve/default-resolve-symbol 'my.ns/foo)))))

(deftest special-forms-unqualified
  (testing "special forms stay unqualified"
    (doseq [sym '[def if do let* fn* loop* try catch finally recur
                  quote var throw new set! monitor-enter monitor-exit
                  deftype* reify* letfn* case* clojure.core/import* & .]]
      (is (= sym (resolve/default-resolve-symbol sym))
          (str "special form " sym " should stay unqualified")))))

(deftest interop-symbols-unqualified
  (testing "interop .method symbols stay unqualified"
    (is (= '.method (resolve/default-resolve-symbol '.method)))
    (is (= '.toString (resolve/default-resolve-symbol '.toString)))))

(deftest var-resolves-to-namespace
  (testing "symbols that resolve to vars get their defining namespace"
    (is (= 'clojure.core/map (resolve/default-resolve-symbol 'map)))
    (is (= 'clojure.core/+ (resolve/default-resolve-symbol '+)))
    (is (= 'clojure.core/str (resolve/default-resolve-symbol 'str)))))

(deftest class-resolves-to-fqn
  (testing "symbols that resolve to classes get their full name"
    (is (= 'java.lang.String (resolve/default-resolve-symbol 'String)))
    (is (= 'java.lang.Exception (resolve/default-resolve-symbol 'Exception)))))

(deftest unresolved-qualifies-with-current-ns
  (testing "unknown symbols get current-ns qualification"
    (let [result (resolve/default-resolve-symbol 'totally-unknown-sym-12345)]
      (is (= (name (ns-name *ns*)) (namespace result)))
      (is (= "totally-unknown-sym-12345" (name result))))))
