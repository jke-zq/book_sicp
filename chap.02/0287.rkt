#lang racket
;;0287
(define (zero-poly? p) 
    (empty-termlist? (term-list p)))
;;0288
;;;supposed there is a generic action called 'negative' to do '0 - val'
(define (negative termlist)
  (if (empty-termlist? termlist)
      the-empty-termlist
      (let ([first (first-term termlist)])
        (ajoin-term (make-term (order first)
                               (negative (coeff first)))
                    (negative (rest-terms termlist))))))
(put 'negative 'polunomial (lambda (poly) (make-polynomial (variable ply)
                                                           (negative (term-list poly)))))

(put 'sub '(polynomial polynomial) (lambda (p1, p2) (tag (add-poly p1 (negative p2)))))

;;0289
;;;supposed 'term' is as the same. but term-list is not same.
(define (first-term term-list)
  (if (empty-termlist? term-list)
      the-empty-termlist
      (make-term (- (length term-list) 1) (car term-list))))
(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (let ([exponent (order term)]
            [len (length term-list)])
        (define (iter exp term-list)
          (cond ((eq? exponent exp) (cons (coeff term) term-list))
                ((< exponent exp) (cons (car term-list) (iter (- exp 1) (cdr term-list))))
                (else (iter (+ exp 1) (cons 0 term-list)))))
        (iter (- len 1) term-list))))
;;0290
;;;solution one: find another package
;;;soluition two: distinguish the first-term and adjoin-term, but also install another package

;;0291
(define (div-terms l1 l2)  
   (if (empty-termlist? l1) 
     (list (the-empty-termlist) (the-empty-termlist)) 
     (let ((t1 (first-term l1)) 
           (t2 (first-term l2))) 
       (if (> (order t2) (order t1)) 
         (list (the-empty-termlist) l1) 
         (let ((first-term (make-term (- (order t1) (order t2)) 
                                      (div (coeff t1) (coeff t2))))) 
           (let ((rest-of-result 
                   (div-terms (sub-terms l1 
                                         (mul-term-by-all-terms first-term 
                                                                l2)) 
                              l2))) 
             (list (adjoin-term first-term (car rest-of-result)) 
                   (cadr rest-of-result)))))))) 
  
(define (div-poly p1 p2) 
  (if (same-variable? (variable p1) (variable p2)) 
      (let ((results (div-terms (term-list p1) 
                                (term-list p2)))) 
        (list (make-poly (variable p1) (car results)) (cadr results))) 
      (error "Polys not in same var -- div-poly" 
             (list p1 p2))))
