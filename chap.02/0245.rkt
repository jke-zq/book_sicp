#lang racket

;;0245
(define right-split (split beside below))
(define up-split (split below beside))


(define (split op1 op2)
  (lambda (pointer n)
    (if (= n 0)
      painter
      (let ((smaller ((split op1 op2) painter (- n 1))))
        (op1 paiter (op2 smaller smaller))))))

;;good codes using named internal func instead of lambda which is named by split func.
;;;;explicit internal func makes it clearer
(define (split op1 op2)
  (define (rec pointer n)
    (if (= 0 n)
        pointer
        (let ((smaller (rec pointer (- n 1))))
          (op1 pointer (op2 smaller smaller)))))
  rec)
