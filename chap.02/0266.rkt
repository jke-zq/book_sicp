#lang racket

;;0266
(define (lookup given-key tree-of-records)
  (cond ([null? tree-of-records] false)
        (else (let ([entry-key (key (entry tree-of-records))])
                (cond ([= given-key entry-key] (entry tree-of-records))
                      ([> given-key entry-key] (lookup given-key (right-branch tree-of-records)))
                      (else (lookup given-key (left-branch tree-of-records))))))))
         
       
