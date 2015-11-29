#lang racket

;;0246
(define (make-vect x y) (cons x y))
(define (xcor-vect vec) (car vec))
(define (ycor-vect vec) (cdr vec))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s vec)
  (make-vect (* s (xcor-vect vec))
             (* s (ycor-vect vec))))

;;0247
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (frame-origin f)
  (car f))
(define (frame-edge1 f)
  (cadr f))
(define (frame-edge2 f)
  (caddr f))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (frame-origin f)
  (car f))
(define (frame-edge1 f)
  (cadr f))
(define (frame-edge2 f)
  (cddr f))
  