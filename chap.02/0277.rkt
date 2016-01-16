#lang racket
;;0277
;;copy from:https://wizardbook.wordpress.com/2010/12/08/exercise-2-77/
#|
 1.   (magnitude '(complex rectangular 3 . 4))
 2.   --> (apply-generic 'magnitude '(complex rectangular 3 . 4))
 3.†  --> (apply (get 'magnitude '(complex)) (contents '(complex rectangular 3 . 4)))
 4.   --> (apply magnitude '(complex rectangular 3 . 4))
 5.   --> (magnitude '(rectangular 3 . 4))
 6.   --> (apply-generic 'magnitude '(rectangular 3 . 4))
 7.   --> (apply magnitude '(complex rectangular 3 . 4))
 8.‡ --> (apply (get 'magnitude '(rectangular)) (contents '(rectangular 3 . 4)))
 9.   --> (apply (lambda (z)
                   (sqrt (+ (square (real-part z))
                            (square (imag-part z))))) '(3 . 4))

10.   --> (apply (lambda (z)
                   (sqrt (+ (square (car z))
                            (square (cdr z))))) '(3 . 4))

11.   => 5
|#

;;0278
(define (attach-tag type contents)
  (if (eq? typ 'scheme-number)
      contetns
      (cons type contents)))

(define (type-tag datum)
  (cond ([pair? datum] (car datum))
        ([number? datum] 'scheme-number)
        (else (error "bad taged datum --TYPE-TAG" datum))))

(define (contents datum)
  (cond ([pair? datum] (cdr datum))
        ([number? datum] datum)
        (else (error "bad content datum --CONTENTS" datum))))

;;0279
(define (equ? x y) (apply-generic 'equ? x y))
(put 'equ? '(complex complex) (lambda (x y) (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y)))))
(put 'equ? '(rational rational) (lambda (x y) (and (= (magnitude x) (magnitude y)) (= (angle x) (angle y)))))
(put 'equ? '(schemer-number schemer-number) =)

;;0280
(define (=zero? x) (apply-generic '=zero? x))
(put '=zero? 'schemer-number (lambda (x) (= x 0)))
(put '=zero? 'complex (lambda (x) (and (= (real-part x) 0) (= (imag x) 0))))
(put '=zero? 'rational (lambda (x) (zero? (number x))))
