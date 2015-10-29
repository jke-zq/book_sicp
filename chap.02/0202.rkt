#lang racket
;;q0202
(define (make-segment startP endP)
  (cons startP endP))
(define (start-segment line)
  (car line))
(define (end-segment line)
  (cdr line))

(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

;;guly, but work
(define (midpoint-segment line)
  (make-point (/ (+ (x-point (start-segment line)) (x-point (end-segment line))) 2)
              (/ (+ (y-point (start-segment line)) (y-point (end-segment line))) 2)))


(define (print-point p)
  (display "dpoint x y coordinates")
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (test-midpoint-segment x1 y1 x2 y2)
  (let ((midpoint (midpoint-segment (make-segment (make-point x1 y1) (make-point x2 y2)))))
    (print-point midpoint)))

;;q0203--without testing
;;easy model:the rectangle needed to be aligned with the grid
(define (make-rec a b)
  (cons a b))
(define (make-rec-width rec)
  (/ (abs (- (x-point (car rec)) (x-point (cdr rec)))) 2))
(defin (make-rec-heigth rec)
  (/ (abs (- (y-point (car rec)) (y-point (cdr rec)))) 2))

(define (rec-perimeter rec)
  (* 2 ( + (make-rec-width rec) (make-rec-heigth rec))))
(define (rec-area rec)
  (* (make-rec-width rec) (make-rec-height rec)))
;; using a point with width and height to represent a rectangle
(define (new-make-rec a w d)
  (cons a (cons w d)))
(define (new-make-width rec)
  (car (cdr rec)))
(define (new-make-heigh rec)
  (cdr (cdr rec)))



