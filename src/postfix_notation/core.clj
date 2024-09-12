(ns postfix-notation.core
  (:require [clojure.core.match :refer [match]]))

;; for operation token
;; fn    -> some function
;; arity -> integer?
(defrecord Operation [fn arity])

;; type  -> :value | :operation
;; which -> integer? | Operation
;; str   -> the original string
(defrecord Token [type which str]
  Object
  (toString [_] str))

(defn- operation-token
  "creates Token for :operation
  if the map op contains the necessary keys
  otherwise throw"
  [op s]
  (let [{:keys [fn arity]} op]
    (if (and (not (nil? fn))
             (not (nil? arity)))
      (Token. :operation (Operation. fn arity) s)
      (throw (ex-info "operation mapping should contain :fn and :arity"
                      {:operation-mapping op})))))

(defn- try-parse-int
  "parse the string str if it is integer
  otherwise return the original string"
  [str]
  (try (Integer/parseInt str)
       (catch NumberFormatException _ str)))

(defn tokens-to-string
  "convert token list to string"
  [tokens]
  (match [tokens]
         [([] :seq)] ""
         [([head & tail] :seq)] (str (.toString head)
                                     \space
                                     (tokens-to-string tail))))

(defn- token
  "create one token from string s with operations ops
  unless the operation is not recognized then throw"
  [s ops]
  (match [(try-parse-int s)]
         [(item :guard number?)] (Token. :value item s)
         [item]
         (let [op (get ops item)]
           (if (not (nil? op))
             (operation-token op s)
             (throw (ex-info "this operation should be present in operation mapping"
                             {:operation item}))))))

;; tokenize list of string
(defn tokenize
  "tokenize list of string with operations ops

  example:
    > (tokenize '(\"1\" \"2\" \"+\") {\"+\" {:fn + :arity 2}})
    ({:type :value, :which 1, :str \"1\"}
     {:type :value, :which 2, :str \"2\"}
     {:type :operation,
      :which {:fn #function[clojure.core/+], :arity 2},
      :str \"+\"})"
  [lst ops]
  (match [lst]
         [([] :seq)] nil
         [([head & tail] :seq)] (cons (try (token head ops)
                                           (catch Exception e
                                             (throw
                                              (ex-info
                                               "operation mapping is incorrect for this formula"
                                               {:read    head
                                                :mapping ops}
                                               e))))
                                      (tokenize tail ops))))

(defn evaluate
  "evaluate the tokens list with stack
  
  if not postfix? then the tokens list should be already reversed
  
  always construct map sequence for intermediate stack of verbose mode
  the sequence is to be printed by clojure.pprint/print-table

  example:
    > (def tokens (tokenize '(\"1\" \"2\" \"-\") {\"-\" {:fn - :arity 2}}))
    > (evaluate tokens '() true)
    [-1
     ({\"stack bot->top\" (), \"token\" \"1 2 - \"}
      {\"stack bot->top\" (1), \"token\" \"2 - \"}
      {\"stack bot->top\" (1 2), \"token\" \"- \"}
      {\"stack bot->top\" (-1), \"token\" \"\"})]"
  [tokens stack postfix?]
  (let [verbose {"stack bot->top" (reverse stack) "token" (tokens-to-string tokens)}]
    (match [tokens]
           [([] :seq)]                  ; tokens end
           (match [stack]
                  [([a] :seq)] [a (list verbose)]
                  :else (throw (ex-info "the formula is incorrect (evaluated all tokens)"
                                        {:stack stack})))
           [([head & tail] :seq)]
           (match [head]
                  [{:type :value        ; is number, push it
                    :which n}]
                  (let [[res str] (evaluate tail (cons n stack) postfix?)]
                    [res (cons verbose str)])
                  [{:type :operation    ; is operation, apply and push it
                    :which {:fn f :arity arity}}]
                  (let [[left right] (split-at arity stack)]
                    (if (= arity (count left))
                      (let [[res str]
                            (evaluate tail
                                      (cons (apply f (if postfix?
                                                       (reverse left)
                                                       left))
                                            right)
                                      postfix?)]
                        [res (cons verbose str)])
                      (throw (ex-info "the formula is incorrect (exhausted numbers earlier on stack)"
                                      {:token head
                                       :stack stack}))))
                  :else (throw (ex-info "don't recognize this token"
                                        {:token head
                                         :rest  tail}))))))

