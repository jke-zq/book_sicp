#lang racket

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

