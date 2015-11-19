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

(define empty-board nil)

#|
(define (adjoin-position new-row k rest-of-queens)
  ;(display rest-of-queens)
  (append rest-of-queens (list (cons k new-row))))

(define (safe? k position)
  (define (check k_col left ret)
       (if (or (not ret) (null? left))
           ret
           (let ((row (car (car left)))
                 (diff (abs (- k_col (cdr (car left))))))
             (cond ((= k row) #t)
                   ((or (= 0 diff) (= (- k row) diff)) #f)
                   (else (check k_col (cdr left) #t))))))
  (let ((k_col (cdr (car (filter (lambda (x) (= k (car x))) position)))))
    (check k_col position #t)))
|#
;;more simpler solution, using index of list to present the row number
(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))

;;reverse the position to be easy to check
(define (safe? k positions)
  (define (do-check k-col offset left)
    (if (null? left)
        #t
        (let ((diff (abs (- k-col (car left)))))
          (if (or (= 0 diff) (= offset diff))
              #f
              (do-check k-col (+ 1 offset) (cdr left))))))
  (let ((rev-positions (reverse positions)))
    (do-check (car rev-positions) 1 (cdr rev-positions))))

    
;;0243
;;original
#|
q(n) = n * q(n - 1)
q(n-1) = (n - 1) * q(n - 2)
...
q(0) = 1
|#
;;interchanged
#|
q(n) = n * n * q(n-1)
q(n-1) = (n-1)*(n-1)*q(n-2)
...
q(0) = 1
|#

