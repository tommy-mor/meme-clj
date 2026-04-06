(ns meme-lang.m-expr-test
  (:require [clojure.test :refer [deftest is testing]]
            [meme-lang.m-expr :as m]))

;; ---------------------------------------------------------------------------
;; Basic transformation
;; ---------------------------------------------------------------------------

(deftest atom-passthrough
  (is (= 42 (m/m->s 42)))
  (is (= "hello" (m/m->s "hello")))
  (is (= :foo (m/m->s :foo)))
  (is (= 'x (m/m->s 'x)))
  (is (= nil (m/m->s nil)))
  (is (= true (m/m->s true))))

(deftest simple-call
  (testing "(f (x y)) → (f x y)"
    (is (= '(f x y) (m/m->s '(f (x y)))))))

(deftest zero-arg-call
  (testing "(f ()) → (f)"
    (is (= '(f) (m/m->s '(f ()))))))

(deftest single-arg-call
  (testing "(f (x)) → (f x)"
    (is (= '(f x) (m/m->s '(f (x)))))))

(deftest nested-call
  (testing "(f ((g (x)))) → (f (g x))"
    (is (= '(f (g x)) (m/m->s '(f ((g (x)))))))))

(deftest deeply-nested
  (testing "(f ((g ((h (x)))))) → (f (g (h x)))"
    (is (= '(f (g (h x))) (m/m->s '(f ((g ((h (x)))))))))))

(deftest call-with-nested-arg
  (testing "(f ((g (x)) y)) → (f (g x) y)"
    (is (= '(f (g x) y) (m/m->s '(f ((g (x)) y)))))))

;; ---------------------------------------------------------------------------
;; Non-call lists (more than 2 elements, second not a list)
;; ---------------------------------------------------------------------------

(deftest non-call-list
  (testing "(a b c) with non-list second → recurse"
    (is (= '(a b c) (m/m->s '(a b c))))))

(deftest mixed-list
  (testing "non-call with nested M-expr inside"
    (is (= '(a b (f x)) (m/m->s '(a b (f (x))))))))

;; ---------------------------------------------------------------------------
;; Vectors
;; ---------------------------------------------------------------------------

(deftest vector-passthrough
  (is (= [1 2 3] (m/m->s [1 2 3]))))

(deftest vector-with-nested-calls
  (testing "M-exprs inside vectors are transformed"
    (is (= ['(f x) '(g y)] (m/m->s ['(f (x)) '(g (y))])))))

(deftest binding-vector
  (testing "let-style binding vector"
    (is (= '[x (f 1) y (g 2)]
           (m/m->s '[x (f (1)) y (g (2))])))))

;; ---------------------------------------------------------------------------
;; Maps and sets
;; ---------------------------------------------------------------------------

(deftest map-walked
  (is (= {:key '(f x)} (m/m->s {:key '(f (x))}))))

(deftest set-walked
  (is (= #{'(f x) 'y} (m/m->s #{'(f (x)) 'y}))))

;; ---------------------------------------------------------------------------
;; Clojure forms
;; ---------------------------------------------------------------------------

(deftest defn-simple
  (testing "defn with params and body"
    (is (= '(defn greet [name] (str "Hello " name))
           (m/m->s '(defn (greet [name] (str ("Hello " name)))))))))

(deftest defn-multi-body
  (testing "defn with multiple body forms — 3+ elements, no splice"
    (is (= '(defn greet [name] (println name) name)
           (m/m->s '(defn (greet [name] (println (name)) name)))))))

(deftest let-form
  (testing "let with binding vector and body"
    (is (= '(let [x (+ 1 2)] (* x x))
           (m/m->s '(let ([x (+ (1 2))] (*(x x)))))))))

(deftest if-form
  (testing "if with condition, then, else"
    (is (= '(if (> x 0) "pos" "neg")
           (m/m->s '(if ((> (x 0)) "pos" "neg")))))))

(deftest cond-form
  (testing "cond with pairs"
    (is (= '(cond (> x 0) "pos" (< x 0) "neg" :else "zero")
           (m/m->s '(cond ((> (x 0)) "pos" (< (x 0)) "neg" :else "zero")))))))

(deftest threading
  (testing "-> threading"
    (is (= '(-> x (inc) (str))
           (m/m->s '(-> (x (inc ()) (str ()))))))))

(deftest fn-form
  (testing "anonymous function"
    (is (= '(fn [x] (* x x))
           (m/m->s '(fn ([x] (* (x x)))))))))

(deftest try-catch
  (testing "try/catch"
    (is (= '(try (risky) (catch Exception e (handle e)))
           (m/m->s '(try ((risky ()) (catch (Exception e (handle (e)))))))))))

(deftest ns-form
  (testing "ns declaration"
    (is (= '(ns my.app (:require [clojure.string :as str]))
           (m/m->s '(ns (my.app (:require ([clojure.string :as str])))))))))

;; ---------------------------------------------------------------------------
;; Edge cases
;; ---------------------------------------------------------------------------

(deftest empty-list
  (is (= '() (m/m->s '()))))

(deftest single-element-list
  (testing "(x) → (x) — no transformation"
    (is (= '(x) (m/m->s '(x))))))

(deftest nested-vectors
  (is (= [[1 2] [3 4]] (m/m->s [[1 2] [3 4]]))))

(deftest empty-vector
  (is (= [] (m/m->s []))))

(deftest empty-map
  (is (= {} (m/m->s {}))))

(deftest keyword-in-call-position
  (testing "(:key (m)) → (:key m)"
    (is (= '(:key m) (m/m->s '(:key (m)))))))

;; ---------------------------------------------------------------------------
;; Eval integration
;; ---------------------------------------------------------------------------

#?(:clj
   (deftest eval-simple-arithmetic
     (is (= 14 (eval (m/m->s '(+ (2 (* (3 4)))))))))

   )

#?(:clj
   (deftest eval-defn-and-call
     (eval (m/m->s '(defn (m-expr-test-greet [name] (str ("Hello " name))))))
     (is (= "Hello world" (eval '(m-expr-test-greet "world"))))))
