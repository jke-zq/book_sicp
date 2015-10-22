#lang racket
(define (cont-frac n d k)
  (define (recursive index)
    (if (> index k)
        (/ (n index) (d index))
        (/ (n index) (+ (d index) (recursive (+ 1 index))))))
  (recursive 1))
;;iter

(define (cont-frac-iter n d k)
  (define (iter index result)
    (if (> index k)
        result
        (iter (+ 1 index) (/ (n index) (+ (d index) result)))))
  (iter 1 0.0))

;;q38
(define (fun-d index)
  (if (< index 3)
      index
      (let ((index (- index 2)))
        (if (= 0 (remainder index 3))
            (* 2 (+ 1 (/ index 3.0)))
            1))))
(define (cont-frac-e n d k)
  (define (recursive index)
    (if (> index k)
        (/ (n index) (d index))
        (/ (n index) (+ (d index) (recursive (+ 1 index))))))
  (recursive 1))
           
(define (e k)
  (cont-frac-e (lambda (x) 1.0) fun-d k))
;;test--(log (+ 2 (e 10000))), result = 1

;;q39
(define (tan-cf x k)
  (define (n index)
    (if (= 1 index)
        x
        (- (* x x))))
  (define (d index)
    (- (* 2.0 index) 1))
  ;;cant use cont-frac-iter
  (cont-frac n d k))
  
  