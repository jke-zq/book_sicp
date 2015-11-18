#lang racket

(require (file "../lib/basic_arithmetic.rkt"))

(define (flatmap proc sequences)
  (accumulate append nil (map proc sequences)))

;;test (i, j)
#|(define (test n)
  (map (lambda (x)
         (map (lambda (p)
                (cons x p))
              (enumerate-interval 1 (- x 1))))
       (enumerate-interval 1 n)))
;;result:'(() ((2 . 1)) ((3 . 1) (3 . 2)))
|#

(define (prime-sum? pair)
  (prime? (+ (car pair) (cdr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cdr pair) (+ (car pair) (cdr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (x) 
                          (map (lambda (p)
                                 (cons x p))
                               (enumerate-interval 1 (- x 1))))
                        (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (y)
                        (cons x y))
                      (permutations (remove x s))))
               s)))

;;0240
(define (unique-pairs n)
  (flatmap (lambda (x)
             (map (lambda (p)
                    (cons x p))
                  (enumerate-interval 1 (- x 1))))
           (enumerate-interval 1 n)))

;;0241
(define (ordered-triples n)
  (flatmap (lambda (x)
             (flatmap (lambda (y)
                        (map (lambda (z)
                               (list x y z))
                             (enumerate-interval 1 (- y 1))))
                      (enumerate-interval 1 (- x 1))))
           (enumerate-interval 1 n)))

(define (ordered-triples-sum n s)
  (filter (lambda (list) (= (accumulate + 0 list) s))
          (ordered-triples n)))

;;0242
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))