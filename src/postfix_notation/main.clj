(ns postfix-notation.main
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]
   [postfix-notation.core :as core]))

;; Postfix/prefix notation calculator
;; 
;; Postfix notation can be evaluated with stack.
;; Prefix notation can be evaluated in a similar way
;; with the formula (and operands) reversed.

;; predefined operations
(def operations [["+"      {:fn +, :arity 2}]
                 ["-"      {:fn -, :arity 2}]
                 ["*"      {:fn *, :arity 2}]
                 ["/"      {:fn quot, :arity 2}]
                 ;; mod 10 operations
                 ["mod10+" {:fn    #(-> %1 (+ %2) (mod 10))
                            :arity 2}]
                 ["mod10*" {:fn    #(-> %1 (* %2) (mod 10))
                            :arity 2}]
                 ;; logical operations. example: 1 1 0 lor land
                 ["land"   {:fn    #(if (= 1 %1) %2 %1)
                            :arity 2}]
                 ["lor"    {:fn    #(if (= 1 %1) %1 %2)
                            :arity 2}]
                 ;; arity is 3. example: 1 2 1 2 + max
                 ["max"    {:fn    #(max %1 %2 %3)
                            :arity 3}]])

(defn- split-every-space
  "Split space delimited string `s`.

  Example:
  ```
  > (split-every-space \"   1   2 + 3  -    \")
  [\"1\" \"2\" \"+\" \"3\" \"-\"]
  ```"
  [s]
  (-> s
      (str/trim)
      (str/split ,,, #" +")))

(defn calc
  "Evaluate the string `s`.
  `s` should not be blank."
  [s operations mode]
  (-> s
      (split-every-space)
      (core/tokenize ,,, operations)
      (core/evaluate ,,, mode)))

(defn -main
  "Ask input for formula and evaluate it.
  
  (command-line) `args`:
    `pre`: Calculate prefix notation.
    `-v`:  Print intermediate stack states."
  [& args]
  (let [mode (if (not (some #(str/starts-with? % "pre") args)) :postfix :prefix)
        verbose (some #(str/includes? % "-v") args)
        postfix (= mode :postfix)]
    (println "evaluate"
             (if postfix "postfix" "prefix")
             "notation.")
    
    (println "all recognized operations are:" (map #(first %) operations))
    (println "example:"
             (if postfix "1 2 + 5 -" "- + 1 2 5"))
    
    (print "input formula: ") (flush)
    (let [s (read-line)]
      (if (str/blank? s)
        (println "formula is empty. exit")
        (let [[res v] (calc s (into {} operations) mode)]
          (when verbose (pp/print-table ["stack bot->top" "token"] v))
          (println "result:" res))))))
