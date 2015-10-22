#lang racket
(define (cont-frac n d k)
  (if (= 1 k)
      (/ (n 1) (d 1))
      (/ (n 1) (+ (d 1) (cont-frac n d (- k 1))))))

;;iter

(define (cont-frac-iter n d k)
  (define (iter index result)
    (if (> index k)
        result
        (iter (+ 1 index) (/ (n index) (+ (d index) result)))))
  (iter 1 0))