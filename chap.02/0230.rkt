#lang racket

(require (file "../lib/basic_arithmetic.rkt"))

;;0230
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))
(define (square-tree-map tree)
  (map (lambda (tree)
         (if (pair? tree)
             (square-tree-map tree)
             (square tree))) tree))
;;;;test code
(define s_tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;;0231
(define (tree-map func tree)
  (map (lambda (tree)
         (if (pair? tree)
             (tree-map func tree)
             (func tree))) tree))

;;other cods without map
(define (tree-map2 proc tree) 
   (cond ((null? tree) nil) 
         ((pair? tree)  
          (cons  
           (tree-map proc (car tree))  
           (tree-map proc (cdr tree)))) 
         (else (proc tree))))

(define (square-tree_map tree)
  (tree-map square tree))
       
;;0232
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (tree)
                            (cons (car s)  tree)) 
                          rest)))))