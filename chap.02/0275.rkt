#lang racket
;;0275
(define (make-from-mag-ang r a)
  (define (dispatch op) 
    (cond ((eq? op 'real-part) (* r (cos a))) 
          ((eq? op 'imag-part) (* r (sin a))) 
          ((eq? op 'magnitude) r) 
          ((eq? op 'angle) a) 
          (else (error "Unkown op --- MAKE-FROM-MAG-ANG" op)))) 
  dispatch) 

;;0276
;;copy from:https://wizardbook.wordpress.com/2010/12/08/exercise-2-76/
#|
For generic operations with explicit dispatch, whenever a new type is added,
procedures for every generic operation need to be written and all of the 
existing procedures need to be modified to include the dispatch on the new type. 
Whenever a new operation is written every procedure must be modified to include 
dispatch on the new operation too.

For data-directed systems, whenever a new type is added, one procedure for each 
operation must be written and installed into the operations table. Whenever a new 
operation is needed, one procedure for each data type must be written to perform 
the operation and installed into the operations table. It may be possible to shield 
those changes from the code that used the generic system too. The data-directed 
deriv procedure for example didn’t need to be modified when adding exponentiation.

Message-passing is similar to data-directed in that adding a new data type needs 
a new dispatch procedure that implements all of the operations and when a new 
operation is added each data type must alter its own dispatch procedure to dispatch 
on and perform the new operation.

There’s no correct answer to the final part of the exercise though, but it seems a 
good rule of thumb would be to use a data-directed style when lots of new operations 
are required and either message-passing or data-directed when lots of new data types are added.
|#