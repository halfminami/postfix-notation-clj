(ns postfix-notation.core
  (:require
   [clojure.core.match :refer [match]]))

;; For `:operation` token.
;; `fn`    -> some function
;; `arity` -> `integer?`
(defrecord Operation [fn arity])

;; `type`  -> `:value` | `:operation`
;; `which` -> `integer?` | `Operation`
;; `str`   -> the original string
(defrecord Token [type which str]
  Object
  (toString [_] str))

(defn- operation-token
  "Creates `Token` for `:operation` if the map `op` contains the necessary keys,
  otherwise throw."
  [op s]
  (match [op]
         [{:fn f :arity arity}] (Token. :operation
                                        (Operation. f arity)
                                        s)
         :else (throw (ex-info "operation mapping should contain :fn and :arity"
                               {:operation-mapping op}))))

(defn- try-parse-int
  "Parse the string `str` if it is integer
  otherwise return the original string."
  [str]
  (try (Integer/parseInt str)
       (catch NumberFormatException _ str)))

(defn tokens-to-string
  "Convert token list `tokens` to a string."
  [tokens]
  (match [tokens]
         [([] :seq)] ""
         [([head & tail] :seq)] (str (.toString head)
                                     \space
                                     (tokens-to-string tail))))

(defn- token
  "Create one `Token` from string `s` with operations `ops`
  unless the operation is not recognized then throw."
  [s ops]
  (match [(try-parse-int s)]
         [(item :guard number?)] (Token. :value item s)
         [item] (let [op (get ops item)]
                  (if (not (nil? op))
                    (operation-token op s)
                    (throw (ex-info "this operation should be present in operation mapping"
                                    {:operation item}))))))

(defn tokenize
  "Tokenize list of string `lst` with operations `ops`.

  Example:
  ```
  > (tokenize '(\"1\" \"2\" \"+\") {\"+\" {:fn + :arity 2}})
  ({:type :value, :which 1, :str \"1\"}
   {:type :value, :which 2, :str \"2\"}
   {:type :operation,
    :which {:fn #function[clojure.core/+], :arity 2},
    :str \"+\"})
  ```"
  [lst ops]
  (match [lst]
         [([] :seq)]
         nil
         
         [([head & tail] :seq)]
         (cons (try (token head ops)
                    (catch Exception e
                      (throw (ex-info "operation mapping is incorrect for this formula"
                                      {:read    head
                                       :mapping ops}
                                      e))))
               (tokenize tail ops))))

(defn evaluate
  "Evaluate the `tokens` list.

  `mode` should be either `:postfix` or `:prefix`.
  
  Always construct map sequence of intermediate stack.
  The sequence is to be printed by `clojure.pprint/print-table`.

  Example:
  ```
  > (def tokens (tokenize '(\"1\" \"2\" \"-\") {\"-\" {:fn - :arity 2}}))
  > (evaluate tokens :postfix)
  [-1
   [{\"stack bot->top\" (), \"token\" \"1\"}
    {\"stack bot->top\" (1), \"token\" \"2\"}
    {\"stack bot->top\" (1 2), \"token\" \"-\"}
    {\"stack bot->top\" (-1), \"token\" \"\"}]]
  ```"
  [tokens mode]
  (let [prefix
        (= mode :prefix)
        
        f
        (fn [[stack stack-states] token]
          (let [this-stack-state {"stack bot->top" (reverse stack)
                                  "token"          (.toString token)}]
            (match [token]
                   ;; the token is number, push it
                   [{:type :value, :which n}]
                   [(cons n stack)
                    (conj stack-states this-stack-state)]
                   ;; the token is operation, apply and push it
                   [{:type :operation, :which {:fn f :arity arity}}]
                   (let [[left right] (split-at arity stack)]
                     (if (= arity (count left))
                       [(cons (apply f (if prefix left (reverse left)))
                              right)
                        (conj stack-states this-stack-state)]
                       (throw (ex-info "the formula is incorrect (exhausted numbers earlier on stack)"
                                       {:token token
                                        :stack stack
                                        :mode  mode}))))
                   :else (throw (ex-info "don't recognize this token"
                                         {:token token
                                          :whole tokens})))))
       
        [stack stack-states]
        (reduce f
                ['() []]
                (if prefix (reverse tokens) tokens))]
    ;; all tokens are read
    (match [stack]
           [([a] :seq)] [a (conj stack-states
                                 {"stack bot->top" (reverse stack)
                                  "token"          ""})]
           :else (throw (ex-info "the formula is incorrect (evaluated all tokens)"
                                 {:stack stack
                                  :mode  mode})))))
