#scheme-evaluator
=============

Toy Scheme evaluator with REPL implemented in Haskell using Parsec

## Installation

### Requirements
You need to have [Haskell Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) installed. 

### Build 
```bash
$ cd scheme-evaluator
$ stack build
```

## Run 

```scheme 

$ stack exec scheme-evaluator-exe
Lisp>>> (+ 2 2)
4
Lisp>>> '(1 2 3 4)
(1 2 3 4)
Lisp>>> (define (counter inc) (lambda (x) (set! inc (+ x inc)) inc))
(lambda ("inc") ...)
Lisp>>> (define (map f xs) (if (eq? xs '()) xs (cons (f (car xs)) (map f (cdr xs)))))
(lambda ("f" "xs") ...)
Lisp>>> (map inc '(1 2 3 4))
(2 3 4 5)
Lisp>>>
```
 
 
