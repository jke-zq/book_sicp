#lang racket

(define (getLargerSum x y z) 
  (if (> x y) (if (> y z) (+ x y) (+ x z))
      (if (> z x) (+ z y) (+ y x))))

