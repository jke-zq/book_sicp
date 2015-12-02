#lang racket

;;0250

(define (flip-vert painter)
  (transform-painter painter 
                     (make-vect 0.0 1.0) ; new origin 
                     (make-vect 1.0 1.0) ; new end of edge1 
                     (make-vect 0.0 0.0))) ; new end of edge2
(define (flip-horiz painter)
  (transform-painter painter 
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
;;;; other ways:
;;repeat rotating 90 degree.
(define (repeated fn t) 
  (if (= t 1) 
      fn 
      (lambda (x) 
        (fn ((repeated fn (- t 1)) 
             x))))) 

(define (rotate180 painter) 
  ((repeated  rotate90 2) painter)) 

(define (rotate270 painter) 
   ((repeated rotate90 3) painter))

;;0251

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0. 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-down
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-up
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-down frame)
        (paint-up frame)))))

(define (below-2 painter1 painter2) 
   (rotate90 (beside (rotate270 painter1) (rotate270 painter2)))) 
  
;; Another way 
(define (below-2 painter1 painter2) 
  (rotate270 (beside (rotate90 painter2) (rotate90 painter1)))) 

