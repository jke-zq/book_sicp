#lang racket

(require (file "../lib/basic_arithmetic.rkt"))
;;q0224
#|
> (list 1 (list 2 (list 3 4)))
'(1 (2 (3 4)))
|#

;;q0225
;(1 3 (5 7) 9)
#|> (define x (list 1 3 (list 5 7) 9))
> x
'(1 3 (5 7) 9)
>(car (cdr (car (cdr (cdr x)))))
7
|#
#|
;((7))
> (car (car (list (list 7))))
7
|#
;(1 (2 (3 (4 (5 (6 7))))))
#|
> (define x (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
> x
'(1 (2 (3 (4 (5 (6 7))))))
>(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr x))))))))))))
7
|#
;;;;;;note: cdr will return a list (list x) [ x is (list a x)].

;;0226
#|
(define x (list 1 2 3))
(define y (list 4 5 6))
> (append x y)
'(1 2 3 4 5 6)
> (cons x y)
'((1 2 3) 4 5 6)
> (list x y)
'((1 2 3) (4 5 6))
> 
|#

;;0227
(define (deep-reverse items)
  (cond 
    ((pair? items) (append (deep-reverse (cdr items)) (list (deep-reverse (car items)))))
    (else items)))
;;;;note: (append nil (list 2 4)) vs (list nil (list 2 4)) vs (cons nil (list 2 4))
;;;;note: func-cdr returns a list.
;;;;;;;;;note: append and return result in iter-func will deal well with nil produced by cdr

(define (deep-reverse-iter items)
  (define (iter rest result)
    (cond ((null? rest) result)
          ;((pair? rest) (iter (cdr rest) (append (list (deep-reverse-iter (car rest))) result)))
          ((pair? rest) (iter (cdr rest) (cons (deep-reverse-iter (car rest)) result)))
          (else (if (null? result) rest (cons rest result)))))
          ;(else (cons rest result))));;error, if result == null, then the result will be int
  (iter items nil))

;;its alse work.
;;;;note: 
#|
> (cons 1 '())
'(1)
> (cons 1 nil)
'(1)
;but----
> (cons nil 1)
'(() . 1)
|#
(define (deep-reverse-test tree) 
   (define (iter t result) 
     (cond ((null? t) result) 
           ((not (pair? (car t))) 
            (iter (cdr t) (cons (car t) result))) 
           (else  
            (iter (cdr t) (cons (deep-reverse-test (car t)) result))))) 
   (iter tree nil)) 

;;best solution in my opinion.
(define (deep-reverse-map items)
  (if (pair? items)
      (reverse (map deep-reverse-map items))
      items))

;;0228
(define (fringe tree)
  (if (pair? tree)
      (append (fringe (car tree)) (fringe (cdr tree)))
      (if (null? tree) nil (list tree))))
(define (fringe-iter tree)
  (define (iter source dict)
    (if (null? source)
        dict
        (iter (cdr source) 
              (append dict 
                      (let ((left (car source)))
                        (if (pair? left)
                            (fringe-iter left)
                            (list left)))))));;left can be nil
  (iter tree nil))

;;0229
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (if (null? branch)
      0
      (car branch)))
(define (branch-structure branch)
  (if (null? branch)
      0
      (car (cdr branch))))

(define (left-stru mobile)
  (branch-structure (left-branch mobile)))

(define (right-stru mobile)
  (branch-structure (right-branch mobile)))

;;total-weight
(define (total-weight mobile)
  (define (iter wei result)
    (if (not (pair? wei))
        (+ wei result)
        (let ((left-wei (left-stru wei))
              (right-wei (right-stru wei)))
          (iter right-wei (iter left-wei result)))))
  (iter mobile 0))

;;test 
(define level-1-mobile (make-mobile (make-branch 2 1) 
                                     (make-branch 1 2))) 
(define level-2-mobile (make-mobile (make-branch 3 level-1-mobile) 
                                     (make-branch 9 1))) 
(define level-3-mobile (make-mobile (make-branch 4 level-2-mobile) 
                                     (make-branch 8 2))) 
;(total-weight level-3-mobile)    

;;good solution that recursive each other.
(define (branch-wei bran)
  (let ((s (branch-structure bran)))
    (if (pair? s)
        (total-weight2 s)
        s)))

(define (total-weight2 mobile)
  (+ (branch-wei (left-branch mobile))
     (branch-wei (right-branch mobile))))

;;solution one;balanced
(define (balanced? mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (= (branch-torque left) (branch-torque right))
         (balance-branch? left)
         (balance-branch? right))))
(define (balance-branch? branch)
  (let ((s (branch-structure branch)))
    (if (pair? s)
        (balanced? s)
        true)))
;;the ends of tree-leaves are true/balanced.
;;usage:(balanced? level-2-mobile) 
(define (branch-torque branch)
  (* (branch-wei branch)
     (branch-length branch)))

;;solution two;balance
;;usage:(check-mobile? level-3-mobile)
(define (check-mobile? mobile)
  (< 0 (check-mobile mobile)))

(define (check-mobile mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
        (let ((left-wei (check-balance left))
               (right-wei (check-balance right))
               (left-len (branch-length left))
               (right-len (branch-length right)))
              (if (and (< 0 (* left-wei left-len)) (= (* left-wei left-len) (* right-wei right-len)))
                  (+ left-wei right-wei)
                  -1))))
(define (check-balance branch)
  (let ((s (branch-structure branch))
        (len (branch-length branch)))
    (if (pair? s)
        (check-mobile s)
        s)))