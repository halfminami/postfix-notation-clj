(ns postfix-notation.main
  (:require [postfix-notation.core :as core]
            [clojure.string :as str]
            [clojure.pprint :as pp]))

;; postfix/prefix notation calculator
;; 
;; postfix notation can be evaluated with stack
;; prefix notation can be evaluated in a similar way
;; with the formula (and operands) reversed

;; predefined operations
(def operations [["+"      {:fn +,    :arity 2}]
                 ["-"      {:fn -,    :arity 2}]
                 ["*"      {:fn *,    :arity 2}]
                 ["/"      {:fn quot, :arity 2}]
                 ;; mod 10 operations
                 ["mod10+" {:fn #(-> %1 (+ %2) (mod 10))
                            :arity 2}]
                 ["mod10*" {:fn #(-> %1 (* %2) (mod 10))
                            :arity 2}]
                 ;; logical operations. example: 1 1 0 lor land
                 ["land"   {:fn #(if (= 1 %1) %2 %1)
                            :arity 2}]
                 ["lor"    {:fn #(if (= 1 %1) %1 %2)
                            :arity 2}]
                 ;; arity is 3. example: 1 2 1 2 + max
                 ["max"    {:fn #(max %1 %2 %3)
                            :arity 3}]])

(defn- split-every-space
  "split space delimited string

  example:
    > (split-every-space \"   1   2 + 3  -    \")
    [\"1\" \"2\" \"+\" \"3\" \"-\"]"
  [s]
  (-> s
      (str/trim)
      (str/split ,,, #" +")))

(defn- switch-from-postfix
  "modify tokens list for evaluation"
  [lst postfix?]
  (if postfix? lst (reverse lst)))

(defn calc
  "evaluate the string s
  s should not be blank"
  [s operations postfix?]
  (-> s
      (split-every-space)
      (core/tokenize ,,, operations)
      (switch-from-postfix ,,, postfix?)
      (core/evaluate ,,, '() postfix?)))

(defn -main
  "ask stdin for formula and evaluate it
  
  (command-line) args:
    pre: calculate prefix notation
    -v: print intermediate stack"
  [& args]
  (let [postfix? (not (some #(str/starts-with? % "pre") args))
        verbose? (some #(str/includes? % "-v") args)]
    (do (println "evaluate"
                 (if postfix? "postfix" "prefix")
                 "notation.")
        
        (println "all recognized operations are:" (map #(first %) operations))
        (println "example:"
                 (if postfix?
                   "1 2 + 5 -"
                   "- + 1 2 5"))
        
        (print "input formula: ") (flush)
        (let [s (read-line)]
          (if (str/blank? s)
            (println "formula is empty. exit")
            (let [[res v] (calc s (into {} operations) postfix?)]
              (do (when verbose? (pp/print-table ["stack bot->top" "token"] v))
                  (println "result:" res))))))))

