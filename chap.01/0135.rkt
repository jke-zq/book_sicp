#lang racket
(provide fixed-point)
(provide average-damp)

(define tolerance 0.00001)
(define (fixed-point f guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try guess))

;;golden ratio
(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

;;x^x fixed log
(define (fixed-log y)
  (fixed-point (average-damp (lambda (x) (/ (log y) (log x)))) 2.0))
;;;;ps:average x f(x) is better than f(x)

;;;;;
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average x y)
  (/ (+ x y) 2.0))

(define (square x)
  (* x x))

