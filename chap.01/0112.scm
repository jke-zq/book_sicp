#lang racket
(define (pascal-triangle row col)
  (if (or (= row col) (= col 1) (= row 1))
      1
      (+ (pascal-triangle (- row 1) (- col 1)) (pascal-triangle (- row 1) col))))
