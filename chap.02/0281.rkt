#lang racket
;;0281
;;;a enter into an infinite loop
;;;b

;;;c
(define (apply-generic op . args)
  (define (no-method-error type1 type2 op)
    (error "no the mothod of op with two type:" (list type1 type2)))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (no-method-error type1 typ2 proc)
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "No method for these types"
                                    (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags))))))))

;;0282
(define (apply-generic op . args)
   (define (no-method-error op type-tags)
    (error "no the mothod of op with two type:" (list op type-tags))
  (define (coercion-type-to-list args type)
    (if (null? args)
        nil
        (let ((first (car args)))
          (let ((proc-coercion (get-coercion (type-tag first) type))))
            (if proc-coercion
                (cons (att-tag (type-tag first)) (proc-coercion (cdr first)) (coercion-type-to-list (cdr args) type))
                (cons first (coercion-type-to-list (cdr args) type))))))
    
  (define (apply-coercion type-tags)
    (if (null? type-tags)
        (no-method-error op type-tags)
    (let ((coercion-args (coercion-type-to-list args (car type-tags))))
      (let ((proc (get op (map type-tag coercion-args))))
        (if proc
            (apply proc (map content coercion-args))
            (apply-coercion (cdr type-tags))))))
      
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (apply-coercion type-tags)))))
     
;;;good codes, from:https://wizardbook.wordpress.com/2010/12/08/exercise-2-82/
#|
(define (apply-generic op . args)
   
  (define (all-coercable? coerce-procs)
    (not (member #f coerce-procs)))
   
  (define (coerce-args coercion-procs args)
    (map (lambda (coerce-proc arg) 
           (coerce-proc arg))
         coercion-procs
         args))
   
  ; attempt to coerce all args into a common type among the args
  (define (apply-with-coercion arg-types)
     
    ; attempt to coerce all args using each tag-type in turn
    ; it's a scoped procedure to keep the original arguments (arg-types) for error reporting
    (define (coerce-types tags)
      (if (null? tags)   ; all targets exhausted 
          (error "No method for these types - APPLY-GENERIC"
                 (list op arg-types))
          (let* ((target-type (car tags))
                 (arg-coercions (map        ; get all the coercion procedures from the target 
                                 (lambda (coerce-from) 
                                   (if (eq? coerce-from target-type)
                                       identity
                                       (get-coercion coerce-from target-type)))
                                 arg-types))) 
            (if (all-coercable? arg-coercions) 
                ; the target type is valid if all the args can be coerced 
                (apply apply-generic  
                       op 
                       (coerce-args arg-coercions args))
                ; target-type is not valid, so try the next one in the list
                (coerce-types (cdr tags))))))        ; try the next target type
     
    (coerce-types arg-types))
   
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc 
        (apply proc (map contents args))
        (apply-with-coercion type-tags))))
|#