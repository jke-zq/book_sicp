#lang racket
(require (file "../lib/basic_arithmetic.rkt"))

(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (accumulate op init (cdr sequence)))))

;;0233
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate  (lambda (x y) (+ 1 y)) 0 sequence))

;;0234
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;;0235
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
                         (if (pair? x) (count-leaves x) 1))
                       t)))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((pair? tree)
         (append (enumerate-tree (car tree))
                 (enumerate-tree (cdr tree))))
        (else (list tree))))

(define (count-leaves-with-enumerate t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

(define (count-leaves-without-map t)
  (accumulate (lambda (x y)
                (cond ((null? x) 0)
                      ((not (pair? x)) (+ 1 y))
                      (else (+ (count-leaves-without-map x) y)))) 0 t))

;;0236
(define (accumulate-n op init sequences)
  (if (null? (car sequences))
      nil
      (cons (accumulate op init (map car sequences))
            (accumulate-n op init (map cdr sequences)))))
(define sequences '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

;;0237
;;this map func is same as accumulate-n abobe.
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
;;the map below is the one we defiened.
#|(define (matrix-*-vector m v)
  (map (lambda (y) (dot-product v y)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-row) (matrix-*-vector cols m-row)) m)))
|#
;;0238
;(fold-right op init (a b c) = a op (b op (c op init))
;(fold-left op init (a b c)  = (init op a) op b op c
;;commutative and associative
#|
(define (fold-right op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence))))) 
  
  
 (define (fold-left op initial sequence) 
   (define (iter result rest) 
     (if (null? rest) 
         result 
         (iter (op result (car rest)) 
               (cdr rest)))) 
   (iter initial sequence))
|#
;;0239
(define (reserve sequences)
  (flod-right (lambda (first already-reserved)
                (append already-reserved (list first)))
              nil
              sequences))
(define (reserve sequences)
  (fold-left (lambda (result first) (cons first result)) 
             nil 
             sequences))