#lang racket
;;0207
(define (make-interval a b) (cons a b))
;;(2 3) or (3 2)
(define (upper-bound l) (max (car l) (cdr l)))
(define (lower-bound l) (min (car l) (cdr l)))
