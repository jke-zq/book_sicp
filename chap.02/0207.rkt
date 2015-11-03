#lang racket
;;0207
(define (make-interval a b) (cons a b))
;;(2 3) or (3 2)
(define (upper-bound l) (max (car l) (cdr l)))
(define (lower-bound l) (min (car l) (cdr l)))


;;0208
(define (sub-interval x y)
  (make-interval (max 
                  (- (upper-bound x) (upper-bound y)) 
                  (- (lower-bound x) (lower-bound y)))
                 (min
                  (- (upper-bound x) (upper-bound y)) 
                  (- (lower-bound x) (lower-bound y)))))
;;0210
(define (mul-interval x y) 
   (let ((p1 (* (lower-bound x) (lower-bound y))) 
         (p2 (* (lower-bound x) (upper-bound y))) 
         (p3 (* (upper-bound x) (lower-bound y))) 
         (p4 (* (upper-bound x) (upper-bound y)))) 
     (make-interval (min p1 p2 p3 p4) 
                    (max p1 p2 p3 p4)))) 

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
(define (div-interval-with-check-zero x y)
  (if (>= 0 (* (lower-bound y) (upper-bound y)))
          (error "y spans zero" y)
          (mul-interval x
                        (make-interval (/ 1.0 (upper-bound y))
                                       (/ 1.0 (lower-bound y))))))
;;0211
(define (ben-mul-interval x y)
  (define (endpoint-sign i)
    (cond ((and
            (<= 0 (upper-bound i))
            (<= 0 (lower-bound i)))
           1)
          ((and
            (>= 0 (upper-bound i))
            (>= 0 (lower-bound i)))
           -1)
          (else 0)))
  (let ((es-x (endpoint-sign x))
        (es-y (endpoint-sign y))
        (x-lo (lower-bound x))
        (x-up (upper-bound x))
        (y-lo (lower-bound y))
        (y-up (upper-bound y)))
    (cond ((> es-x 0)
           (cond ((> es-y 0) (make-interval (* x-lo y-lo) (* x-up y-up)))
                 ((< es-y 0) (make-interval (* x-up y-lo) (* x-lo y-up)))
                 (else (make-interval (* x-up y-lo) (* x-up y-up)))))
          ((< es-x 0)
           (cond ((> es-y 0) (make-interval (* x-lo y-up) (* x-up y-lo)))
                 ((< es-y 0) (make-interval (* x-up y-up) (* x-lo y-lo)))
                 (else (make-interval (* x-lo y-up) (* x-lo y-lo)))))
          (else
           (cond ((> es-y 0) (make-interval (* x-lo y-up) (* x-up y-up)))
                 ((< es-y 0) (make-interval (* x-up y-lo) (* x-lo y-lo)))
                 (else (make-interval (min (* x-lo y-up) (* x-up y-lo))
                                      (max (* x-lo y-lo) (* x-up y-up)))))))))

;;0212
;;percentage from 0 to 1
(define (make-center-percent center percentage)
  (make-interval
   (let ((width (* center percentage)))
     (make-interval (- center width) (+ center width)))))

(define (center interval)
  (/ (+ (upper-bound interval) (lower-bound interval)) 2.0))

(define (percent interval)
  (/ (- (upper-bound interval) (center interval)) (center interval)))
;;with let
(define (percent-let interval)
  (let ((c (* 1.0 (center interval))))
    (/ (- (upper-bound interval) c) c)))

;;0213
;;误差会累加的
;;more from:http://community.schemewiki.org/?sicp-ex-2.13


;;0214-0216
;;read from:https://github.com/jke-zq/sicp/blob/master/exercises/02/2.14_2.16.md
#|wiki上称之为Dependency_problem:
The so-called dependency problem is a major obstacle to the application of interval arithmetic. 
Although interval methods can determine the range of elementary arithmetic operations and functions very accurately, 
this is not always true with more complicated functions. If an interval occurs several times in a calculation using
parameters, and each occurrence is taken independently then this can lead to an unwanted expansion of the resulting 
intervals.

...

In general, it can be shown that the exact range of values can be achieved, if each variable appears only once. 
====However, not every function can be rewritten this way.====
|#
