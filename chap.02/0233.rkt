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
             