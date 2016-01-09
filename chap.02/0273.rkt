#lang racket
;;0273
;;;a
;;a 
;number?, same-variable? are predicates. there's nothing to dispatch/oprate. 

;; b 
(define (install-deriv-package)
  (define (sum-deriv expr var) 
    (make-sum (deriv (addend expr) var) 
              (deriv (augend expr) var))) 
  (define (addend expr) (car expr)) 
  (define (augend expr) (cadr expr)) 
  (define (make-sum x1 x2) 
    (cond ((and (number? x1) (number? x2)) (+ x1 x2)) 
          ((=number? x1 0) x2) 
          ((=number? x2 0) x1) 
          (else (list '+ x1 x2)))) 
  (define (mul-deriv expr var)
    (make-sum (make-product (multiplier expr) 
                            (deriv (multiplicand expr) var)) 
              (make-product (multiplicand expr) 
                            (deriv (multiplier expr) var)))) 
  (define (multiplier expr) (car expr)) 
  (define (multiplicand expr) (cadr expr)) 
  (define (make-product x1 x2) 
    (cond ((and (number? x1) (number? x2)) (* x1 x2)) 
          ((=number? x1 1) x2) 
          ((=number? x2 1) x2) 
          ((or (=number? x1 0) (=number? x2 0)) 0) 
          (else (list '* x1 x2)))) 
  
  (put 'deriv '+ sum-deriv) 
  (put 'deriv '* mul-deriv)

;;;c
  (define (exponentation-deriv expr var) 
    (make-product (exponent expr) 
                  (make-product
                   (make-exponentiation (base expr) 
                                        (make-sum (exponent expr) -1)) 
                   (deriv (base expr) var)))) 
  (define (exponent expr) (cadr expr)) 
  (define (base expr) (car expr))
  (define (make-exponentiation base exponent) 
    (cond ((=number? exponent 0) 1) 
          ((=number? exponent 1) base) 
          ((=number? base 1) 1) 
          (else (list '** base exponent)))) 
  (put 'deriv '** exponentiation-deriv))

;;;d 
;The only thing to do is changing the order of arguments in procedure "put".
;;for instance:(put op 'deriv item)