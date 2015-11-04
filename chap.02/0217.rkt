#lang racket

;;lenght
(define (list-len l)
  (if (null? l)
      0
      (+ 1 (list-len (cdr l)))))

(define (list-len-iter l)
  (define (iter a count)
    (if (null? a)
        count
        (iter (cdr a) (+ 1 count))))
  (iter l 0))

;;q0217
(define (list-last l)
  (if (= 1 (list-len-iter l))
      (car l)
      (list-last (cdr l))))
(define (list-last-iter items)
  (define (iter last rest)
    (if (null? rest)
        last
        (iter (car rest) (cdr rest))))
  (iter (car items) (cdr items)))
      
(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

;;q0218
(define nil '())
(define (reverse items)
  (define (iter rest result)
    (if (null? rest)
        result
        (iter (cdr rest) (cons (car rest) result))))
  (iter items nil))
;;good answer
#|
(define no-more? null?) 
(define except-first-denomination cdr) 
(define first-denomination car)
|#

;;q0220
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (no-more? items)
  (null? items))

(define (except-first-denomination items)
  (cdr items))

(define (first-denomination items)
  (car items))
;;no, nothing with the order of the coins in the list
;;======will cost more time than desending order.

;;0221
(define (same-parity . items)
  ;(display items)
  (define (iter rem left result)
    (if (null? left)
        (reverse result)
        (iter rem (cdr left) 
              (if (= rem (remainder (car left) 2))
                  (cons (car left) result)
                  result))))
  (let ((rem (remainder (car items) 2)))
    (iter rem items nil)))
;;good answer
#|
(define (same-parity n . lst) 
    (define same-parity? (if (even? n) even? odd?)) 
    (define (iter lst acc) 
      (if (null? lst) 
          acc 
          (let ((first (car lst)) 
                (rest (cdr lst))) 
            (iter rest 
                  (if (same-parity? first) 
                      (cons first acc) 
                      acc))))) 
    (cons n (reverse (iter lst null)))) 
|#