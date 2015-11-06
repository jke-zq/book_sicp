#lang racket

(require (file "../lib/basic_arithmetic.rkt"))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))
      

;;0221
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list-map items)
  (map square items))

;;0222
;(cons (list 1 2 3) 4)
;;'((1 2 3) . 4)
;;my way: using reverse, not append(I think it is recursive if using append. However, I dont know).

;;0223
(define (for-each proc items)
  (if (null? items)
      true;as true
      (let (); or begin key_word or cond
        (proc (car items)) 
        (for-each proc (cdr items)))))
;(for-each (lambda (x) (newline) (display x))
;          (list 57 321 88))

(define (for-each-map proc items) 
   (map proc items) 
   #t)
;; The below didn't work ... basically, I needed some kind of block 
;; structure, since if has the form (if (test) true-branch 
;; false-branch).  I needed to have true-branch execute the proc, then 
;; call the next iteration of for-each, and the only way I knew how to 
;; do that was with brackets ... but of course that doesn't work, as 
;; the interpreter tries to apply the result of the first proc call as 
;; a function to the rest. 
;; This one works. 
;; Moral: cond is better for multi-line branches. 
#|
(define (for-each proc items) 
   (cond ((not (null? items)) 
          (proc (car items)) 
          (for-each proc (cdr items)))))
|#