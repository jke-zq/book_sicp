#lang racket
;In general, the technique of defining an invariant quantity
;that remains unchanged from state to state is a powerful way to
;think about the design of iterative algorithms.)
;ab^n is the result, when n = 0, the result is a
;an additional state variable a, and define the state transformation
;in such a way that the product a b n is unchanged from state to state.
;At the beginning of the process a is taken to be 1, 
;and the answer is given by the value of a at the end of the process.

(define (square x)
  (* x x))
(define (even? x)
  (= (remainder x 2) 0))
(define (iter-expr a b n)
  (cond ((= n 0) a)
        ((even? n) (iter-expr a (square b) (/ n 2)))
        (else (iter-expr (* a b) b (- n 1)))))
(define (expr b n)
  (iter-expr 1 b n))
      
(define (iter-multi a b c)
  (cond ((= b 0) c)
        ((even? b) (iter-multi (+ a a) (/ b 2) c))
        (else (iter-multi a (- b 1) (+ c a)))))

(define (* a b)
  (iter-multi a b 0))