#lang racket

;test if prime? and cost time
(define (timed-prime-test n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (current-inexact-milliseconds) start-time)) #f))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))
(define (next n)
  ;(if (= 2 n) 3 (+ n 2)))
  (+ n 1))
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((= (remainder n test-divisor) 0) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (square n)
  (* n n))
(define (prime? n)
  (= n (smallest-divisor n)))
;search for 'count' primes from base 
(define (search-for-primes base count)
  (when (> count 0)
      (search-for-primes (+ base 1) (if (timed-prime-test base) (- count 1) count))))
;;expmod = base^exp / m
(define (even? n)
  (= (remainder n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (expmod (remainder (* base base) m) (/ exp 2) m))
        (else (remainder (* base (expmod (remainder base m) (- exp 1) m)) m))))
;;base^exp maybe very big big.... 