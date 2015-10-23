#lang racket

(require "0135.rkt")
;;new-ton
(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x))
     dx)))

(define (cube x)
  (* x x x))
;;test works
;((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;;q40
(define (square x)
  (* x x))
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))
;;test
;(newtons-method (cubic a b c) 1)

;;q41
(define (double f)
  (lambda (x) (f (f x))))
(define (inc x)
  (+ 1 x))
;;q42
(define (compose f g)
  (lambda (x) (f (g x))))
;;q43
(define (repeated f times)
  (define (iter x result)
    (if (= times x)
        result
        (iter (+ x 1) (compose result f))))
  (lambda (x) ((iter 1 f) x)))
;;other solution
(define (repeat f n)
  (if (= n 1)
    f
    (lambda (x)
      (f ((repeat f (- n 1)) x)))))
;;the example below helps to understand
;;note: in lambda:(f y) is not to eval:f(y), but f
;;总把lambda里面的(f arg) 看成是f(arg),其实应该是f
(define (lambda-x f)
   (lambda (x) 
        (f ((lambda (y) 
                    (f ((lambda (z) 
                          (f z)) y))) x))))

;;q44
(define (smoothed f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))
(define (repeated-smooth f)
  (repeat (smoothed f)))
;;q45
(define (product x times)
  (if (= 0 times)
      1.0
      (* x (product x (- times 1)))))

;http://www.billthelizard.com/2010/08/sicp-145-computing-nth-roots.html
;我这里直接借用这个网址的方法,它给出了n与damp的次数关系
;maximum n: 3, 7, 15
;average damps: 1, 2, 3
;在根据它总结的公式，可以得出下面的过程：
(define (log2 n)
  (/ (log n) (log 2)))

(define (nth-root x times)
  (define (root times)
    (lambda (y) (/ x (product y (- times 1)))))
  ;;this is not average-damp(average-damp(average-damp(x)))
  ;;but average-damp(root(average-damp(root(average-damp(root(x))))))
  ;;average-damp(x)
  ;;average-damp(root(x))
  ;(fixed-point (repeated (average-damp (root times)) (floor (log2 n)) 1.0))
  (fixed-point ((repeated average-damp (floor (log2 times)))
                (root times)) times) 1.0)
;;q46
;1.e-6 == 0.000001
(define (close-enough? v1 v2) 
   (define tolerance 1.e-6) 
   (< (/ (abs (- v1 v2)) v2)  tolerance))

(define (iterative-improve close-enough? improving)
  (lambda (x) 
    (let ((newx (improving x)))
          (if (close-enough? newx x)
              newx
              ((iterative-improve close-enough? improving) newx)))))


(define (sqrt y)
  (display "0000")
  (newline)
  ((iterative-improve close-enough? (average-damp (lambda (x) (/ y x)))) 1.0))



