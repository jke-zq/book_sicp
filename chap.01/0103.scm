#lang racket
;0103
(define (getLargerSum x y z) 
  (if (> x y) (if (> y z) (+ x y) (+ x z))
      (if (> z x) (+ z y) (+ y x))))
;0106/7
;;new-if is a fun and eval all parameters. note: p func vs p(parameters) are diff.
;;if is not function by programmer. It will test predicate and eval the clause.
;;double is not continue and the gap is bigger and bigger.

(define (sqrt x)
  (define (square x) (* x x))
  (define (good-enough? guess new-guess) 
    (> 0.01 (/ (abs (- guess new-guess)) guess)))
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2))
  (define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
          (else else-clause)))
  (define (sqrt-iter guess)
    (if (good-enough? guess (improve guess))
        (improve guess)
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
(define (new-if p t e)
  (cond (p t)
        (else e)))
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(define (p) (p))
