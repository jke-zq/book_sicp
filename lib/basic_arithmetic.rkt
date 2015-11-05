#lang racket
;;common lib
(provide (all-defined-out))

(define nil '())
(define (even? x)
  (= 0 (remainder x 2)))

(define (square x)
  (* x x))

(define (power a b)
  (cond ((= 0 b) 1)
        ((even? b) (square (power a (/ b 2))))
        (else (* a (power a (- b 1))))))