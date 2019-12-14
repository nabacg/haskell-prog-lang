hSchemeParser
=============

Toy Scheme REPL implemented in Haskell

### Installation

```bash
cabal install mtl
cabal install parsec
```



 ## Run 

Compile into executable with GHC
```bash
ghc src/schemeParser.hs -o schemeRepl.o
```

Then run 

```scheme 

scheme-evaluator cab$ ./schemeRepl.o
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
 
 
