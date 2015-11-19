#lang racket
;;common lib
(provide (all-defined-out))

;;algorithm
(define (even? x)
  (= 0 (remainder x 2)))

(define (square x)
  (* x x))

(define (power a b)
  (cond ((= 0 b) 1)
        ((even? b) (square (power a (/ b 2))))
        (else (* a (power a (- b 1))))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n x)
  (cond ((> (square x) n) n)
        ((divisor? n x) x)
        (else (find-divisor n (+ 1 x)))))

(define (divisor? n x)
  (= 0 (remainder n x)))

;;list
(define nil '())

(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (accumulate op init (cdr sequence)))))

(define (filter predicate sequences)
  (cond ((null? sequences) nil)
        ((predicate (car sequences)) 
         (cons (car sequences) 
               (filter predicate (cdr sequences))))
        (else (filter predicate (cdr sequences)))))

(define (map proc sequences)
  (if (null? sequences)
      nil
      (cons (proc (car sequences))
            (map proc (cdr sequences)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append (cdr list1) list2))))

(define (reserve sequences)
  (accumulate (lambda (first already-reserved)
                (append already-reserved (list first)))
              nil
              sequences))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ 1 low) high))))