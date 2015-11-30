#lang racket
;;0248
(define make-segment cons) 
(define start-segment car) 
(define end-segment cdr)

;;0249
(let ((tl (make-vect 0 1)) 
     (tr (make-vect 1 1)) 
     (bl (make-vect 0 0)) 
     (br (make-vect 1 0))) 
;; a 
(segments->painter (list 
                   (make-segment bl tl) 
                   (make-segment tl tr) 
                   (make-segment tr br) 
                   (make-segment br bl))) 
;; b 
(segments->painter (list 
                   (make-segment bl tr) 
                   (make-segment br tl)))) 

(let ((l (make-vect 0 0.5)) 
     (t (make-vect 0.5 1)) 
     (r (make-vect 1 0.5)) 
     (b (make-vect 0.5 0))) 
;; c  
(segments->painter (list 
                   (make-segment l t) 
                   (make-segment t r) 
                   (make-segment r b) 
                   (make-segment b l)))) 