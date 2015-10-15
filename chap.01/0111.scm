#lang racket
(define (recursive-func n)
  (if (> 3 n) 
      n
      (+ (recursive-func (- n 1)) 
         (* 2 (recursive-func (- n 2))) 
         (* 3 (recursive-func (- n 3))))))

(define (func-iter n count a b c)
  (if (> 3 n) 
      n
      (if (= count n)
          (+ c (* 2 b) (* 3 a))
          (func-iter n (+ count 1) b c (+ c (* 2 b) (* 3 a))))))

(define (iterator-func n)
  (func-iter n 3 0 1 2))