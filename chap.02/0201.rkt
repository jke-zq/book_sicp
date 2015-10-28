#lang racket
(require (file "../chap.01/0129.rkt"))

(define (mark-rat n d)
  (let ((g (gcd n d)))
    (if (< 0 (* n d ))
        (cons (abs (/ n g)) (abs (/ d g)))
        (cons (* -1 (abs (/ n g))) (abs (/ d g))))))