#lang racket

(require (file "../lib/basic_arithmetic.rkt"))
;;q0204
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))
;;test
;((lambda (m) (m 2 4)) (lambda (p q) q))

;;q0205
(define (cons-product x y)
  (* (power 2 x) (power 3 y)))

(define (root-count z x)
  (define (iter z result)
    (if (= 0 (remainder z x))
        (iter (/ z x) (+ 1 result))
        result))
  (iter z 0))

(define (car-product z)
  (root-count z 2))

(define (cdr-product z)
  (root-count z 3))
;;test
#|
> (car-product (cons-product 4 5))
4
> (cdr-product (cons-product 4 5))
5
>
|#

;;q0206
;;church
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))
(define (plus first second)
  (lambda (f) (lambda (x) ((first f) ((second f) x)))))
(define (product first second)
  (lambda (f) (lambda (x) ((first (second f)) x))))

;;test
(define (f x)
  (display "*"))

(define (start-test-2-6)
  
  (display "going to display 1:")(newline)
  ((one f) 'a)(newline)
  (display "going to display 2:")(newline)
  ((two f) 'a) (newline)
  (display "going to display 1+2:")(newline)
  (((plus one two) f) 'a)
  (newline)
  (display "going to display 1+2+2")(newline)
  (((plus (plus one two) two) f) 'a)
  (newline)
  (display "going to display 3*2*2")(newline)
  (((product (product three two) two) f) 'a)
  (newline)

  (display "end.") (newline))
#|
> (start-test-2-6)
going to display 1:
*
going to display 2:
**
going to display 1+2:
***
going to display 1+2+2
*****
going to display 3*2*2
************
end.
|#
