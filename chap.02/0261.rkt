#lang racket

;;0261
(define (adjoin-set x set1)
  (define (iter pre post)
    (cond ([or (null? post) (< x (car post))] (append pre (cons x post)))
          ([= x (car post)] (append pre post))
          (else (iter (append pre (list (car post))) (cdr post)))))
  (iter '() set1))

;;recursive
(define (adjoin-set x set1)
  (cond ([or (null? set1) (< x (car set1))] (cons x set1))
        ([= x (car set1)] set1)
        (else (cons (car set1) (adjoin-set (cdr set1))))))
;;0262
(define (union-set set1 set2)
  (define (iter s1 s2 result)
    (cond ([and (null? s1) (null? s2)] result)
          ([null? s1] (append result s2))
          ([null? s2] (append result s1))
          (else (let ([a1 (car s1)]
                      [a2 (car s2)])
                     (cond ([< a1 a2] (iter (cdr s1) s2 (append result (list a1))))
                           ([> a1 a2] (iter s1 (cdr s2) (append result (list a2))))
                           (else (iter (cdr s1) (cdr s2) (append result (list a2)))))))))
  (iter set1 set2 '()))
