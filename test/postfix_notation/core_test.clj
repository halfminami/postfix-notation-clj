(ns postfix-notation.core-test
  (:require
   [clojure.core.match :refer [match]]
   [clojure.test :refer :all]
   [postfix-notation.core :refer :all]))

;; checking :type only
(defn tokens-equal? [tokens lst]
  (match [tokens lst]
         [([] :seq) ([] :seq)] true
         [([] :seq) _] false
         [_ ([] :seq)] false
         [([t-hd & t-tl] :seq) ([l-hd & l-tl] :seq)]
         (let [t-type (get t-hd :type)
               l-type (get l-hd :type)]
           (and (= t-type l-type)
                (tokens-equal? t-tl l-tl)))))

(defn plus  [x y]   (+ x y))
(defn minus [x y]   (- x y))
(defn max3  [x y z] (max x y z))

(def operations {"+" {:fn plus  :arity 2}
                 "-" {:fn minus :arity 2}
                 "M" {:fn max3  :arity 3}
                 "λ" {:fn +}})

(defn tokens [lst] (tokenize lst operations))

(defn evalu [tokens mode]
  (first (evaluate tokens mode)))

(deftest tokenize-test
  (is (tokens-equal? (tokens '("M" "1" "-20" "+" "-"))
                     '({:type :operation} {:type :value} {:type :value} {:type :operation} {:type :operation}))
      "numbers and known operations")
  (testing
      "unrecognized string"
    (is (thrown? Exception (tokens '("λ"))))
    (is (thrown? Exception (tokens '("∀"))))
    (is (thrown? Exception (tokens '("3.14")))
        "not integer")))

(deftest evaluate-test
  (testing "postfix notation"
    (is (= 3 (evalu (tokens '("1" "2" "+")) :postfix)))
    (is (= -1 (evalu (tokens '("1" "2" "-")) :postfix))
        "order")
    (is (= 5 (evalu (tokens '("1" "2" "3" "4" "M" "+")) :postfix))
        "different arity"))
  (testing "prefix notation"
    (is (= -1 (evalu (tokens '("-" "1" "2")) :prefix)))))
