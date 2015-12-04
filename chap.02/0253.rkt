#lang racket
(require (file "../lib/basic_arithmetic.rkt"))
;;0253
(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))
#|
'(a b c)
'((george))
'((y1 y2))
'(y1 y2)
#f
#f
'(red shoes blue socks)
|#

;;0254
#|
;;my ugly codes
(define (equal? a b)
  (if (pair? (car a))
      (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
      (and (eq? (car a) (car b)) (equal? (cdr a) (cdr b)))))
|#

(define (equal2? a b)
  (if (and (pair? a) (pair? b))
      (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
      (eq? a b)))
;;perfect
;;map and reduce
(define (equal1? list1 list2) 
  (accumulate (lambda (x y) (and x y)) #t (map eq? list1 list2))) 

;;0255
(car ''(abracadabra))
;相当于 
(car (quote (quote (abracadabra))))
;;import;;car把第一个quote后的对象作为一个list整体来看了
(cadr ''(abracadabra))
;这是返回的就是(abracadabra)了
;;more details:http://community.schemewiki.org/?sicp-ex-2.55
;;for instance:
#|
> (define b '(list 'a))
> b
'(list 'a)
> (quote (list (quote a)))
'(list 'a)
> 
|#
