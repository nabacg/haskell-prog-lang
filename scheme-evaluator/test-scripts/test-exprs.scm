(define my-count (counter 111))
(define (counter inc) (lambda (x) (set! inc (+ x inc)) inc))


(my-count 1)

(define (inc x) (+ x 1))
(define (map f xs) (if (eq? xs '()) xs (cons (f (car xs)) (map f (cdr xs))) ))

(map inc '(1 2 3 4))