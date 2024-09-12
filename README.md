evaluate postfix/prefix notation in Clojure

Both postfix and prefix notation can be evaluated easily with stack.

The `postfix-notation.core` namespace provides functions for postfix/prefix notation
evaluation.

The `postfix-notation.main/-main` function shows how to use those functions.
You can run it:
```sh
lein run
```

The function collects intermediate stack besides the result for visualization.
It can be shown like so (for postfix notation `5 2 -` that is $5 - 2$):
```
$ lein run -- -v
evaluate postfix notation.
all recognized operations are: (+ - * / mod10+ mod10* land lor max)
example: 1 2 + 5 -
input formula: 5 2 -

| stack bot->top |  token |
|----------------+--------|
|             () | 5 2 -  |
|            (5) |   2 -  |
|          (5 2) |     -  |
|            (3) |        |
result: 3
```

