#lang racket

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend-list exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand-list exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand-list exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
     (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (sum? x)
     (and (pair? x) (eq? (car x) '+)))
;(define (addend s) (cadr s))
(define addend cadr)
;(define (augend s) (caddr s))
(define augend caddr)
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (product? x)
     (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))


;;0257
;solution 4
(define (make-sum-list l) 
   (if (= (length l) 2) 
       (list '+ (car l) (cadr l)) 
       (make-sum-l (car l) (make-sum-list (cdr l))))) 
 (define (make-sum-l a1 a2) 
   (cond ((=number? a1 0) a2) 
         ((=number? a2 0) a1) 
         ((and (number? a1) (number? a2)) (+ a1 a2)) 
         (else (make-sum-list (list a1 a2))))) 
  
 (define (make-product-list l) 
   (if (= (length l) 2) 
       (list '* (car l) (cadr l)) 
       (make-product-l (car l) (make-product-list (cdr l))))) 
 (define (make-product-l m1 m2) 
   (cond ((or (=number? m1 0) (=number? m2 0)) 0) 
         ((=number? m1 1) m2) 
         ((=number? m2 1) m1) 
         ((and (number? m1) (number? m2)) (* m1 m2)) 
         (else (make-product-list (list m1 m2))))) 
  
 (define (augend-l s) 
   (let ((a (cddr s))) 
     (if (= (length a) 1) 
         (car a) 
         (make-sum-list a)))) 
 (define (multiplicand-l p) 
   (let ((m (cddr p))) 
     (if (= (length m) 1) 
         (car m) 
         (make-product-list m)))) 
(define (=number? exp num)
(and (number? exp) (= exp num)))

;;0256
(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))

(define (make-exponentiation b e)
  (cond ((=number? b 1) 1)
        ((=number? e 1) base)
        ((=number? e 0) 1)
        (else
         (list '** b e))))
;;;;deal with (- n 1), n is number?
#|
(make-exponentiation (base exp)  
  (if (number? (exponent exp))  
      (- (exponent exp) 1) 
      (' (- (exponent exp) 1))))) 
In the end product, there is no need to check the exponent is number or not, 
procedure make-sum can do that.
|#
#|
((exponentiation? expr)  
 (make-product  
  (make-product  
   (exponent expr) 
   (make-exponentiation (base expr) 
                        (make-sum (exponent expr) -1)))                                                                                                 
  (deriv (base expr) var))) 
|#
;;0257
;my solution
(define (augend-list s)
  (display s)
  (if (< 3 (length s))
      (append (list (car s)) (cddr s))
      (caddr s)))
(define (multiplicand-list p)
  (if (< 3 (length p))
      (append (list (car p)) (cddr p))
      (caddr p)))
;;solution 2:accumulate is really awesome
#|
(define (augend s)    
   (accumulate make-sum 0 (cddr s))) 
  
(define (multiplicand p)  
  (accumulate make-product 1 (cddr  p))) 
|#
;;solution 3:more concise
#| 
(define (multiplicand p) 
   (if (null? (cdddr p)) 
       (caddr p) 
       (cons '* (cddr p))))
|#
;;0258

        