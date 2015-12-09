#lang racket

;;0259
(define (union-set set1 set2)
  (define (iter s1 s2 result)
    (cond ([and (null? s1) (null? s2)] result)
          ([null? s1] (if (element-of-set? (car s2) result)
                          (iter s1 (cdr s2) result)
                          (iter s1 (cdr s2) (cons (car s2) result))))
          (else (if (element-of-set? (car s1) result)
                    (iter (car s1) s2 result)
                    (iter (car s1) s2 (cons (car s1 result)))))))
  (iter set1 set2 '()))

(define (union-set2 set1 set2)
  (if (null? set1)
      set2
      (let ([e (car set1)])
        (union-set2 (cdr set1) (if (element-of-set? e set2) set2 (adjoin e set2))))))
;;0260
(define (adjoin-set x set)
  (cons x set))
(define (union-set set1 set2)
  (append set1 set2))

(define (remove-from-set x set)
  (define (remove head tail)
    (if (null? tail)
        head
        (if (equal? x (car tail))
            (append head (cdr tail))
            (remove (adjoin-set (car tail) head) (cdr tail)))))
  (remove '() set))

(define (intersection-set set1 set2)
  (cond  ([or (null? set1) (null? set2)] '())
         ([element-of-set? (car set1) set2]
          (cons (car set1) (intersection-set (cdr set1) (remove-from-set (car set1) set2))))
         (else
          (intersection-set (cdr set1) set2))))
          
       