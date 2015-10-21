#lang racket
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
;;q30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (next i)
  (+ 1 i))

(define (simpson-integral f a b n)
  (define (h)
    (/ (- b a) n))
  (define (y k)
    (f (+ a (* k (h)))))
  (define (next k)
    (+ k 1))
  (define (term k)
    (cond ((and (= 1 k) (= n k)) (y k))
          ((even? k) (* 2 (y k)))
          (else (* 4 (y k)))))
  (* (/ (h) 3.0) 
      (sum term 0 next n)))
;;q31
(define (product term a next b)
  (define (product-iter a result)
    (if (> a b)
        (* 1.0 result)
        (product-iter (next a) (* result (term a)))))
  (product-iter a 1))

(define (product-recur term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recur term (next a) next b))))

(define (pro-pie a next b)
  (define (h-a a)
    (if (even? a) (+ 2 a) (+ 1 a)))
  (define (h-b a)
    (if (even? a) (+ 1 a) (+ 2 a)))
  (define (term a)
    (/ (h-a a) (h-b a)))
  (product term a next b))
;;q32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) 
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (acc-iter a result)
    (if (> a b)
        result
        (acc-iter (next a) (combiner (term a) result))))
  (acc-iter a null-value))
;;q33
#|
(define (filtered-accumulate combiner null-value term a next b filter)
  (define (filter-next a)
    (if (or (filter a) (> a b))
        a
        (filter-next (next a))))
  (define (acc-iter a result)
    (if (> a b)
        result
        (acc-iter (filter-next (next a)) (combiner (term a) result))))
  (acc-iter a null-value))
|#
(define (filtered-accumulate combiner null-value term a next b valid?)
  (define (filter-next a)
    (if (or (filter a) (> a b))
        a
        (filter-next (next a))))
  (define (acc-iter a result)
    (if (> a b)
        result
        (if (valid? a)
            (acc-iter (next a) (combiner (term a) result))
            (acc-iter (next a) result))))
  (acc-iter a null-value))

;;prime?

(define (smallest-divisor n)
  (find-divisor n 2))

(define (square n)
  (* n n))
(define (find-divisor n test-divisor)
  (cond 
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= 0 (remainder b a)))

(define (prime? n)
  (= (smallest-divisor n) n))

;;gcd
(define (gcd a b)
  (if (= 0 b)
      a
      (gcd b (remainder a b))))
;;bang bang da
(define (identify x) x)

(define (product-gcd n)
  (define (valid? a)
    (= 1 (gcd a n)))
  (filtered-accumulate * 1 identify 1 next n valid?))
 
