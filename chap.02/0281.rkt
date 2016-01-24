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
     
;;0283
(define (integer->rational integer)
  (make-rational interger 1))
(define (rational->real rational)
  (define (integer->floating-point integer)
    (* integer 1.0))
  (make-real (/ (integer->floating-point (number rational))
                (denom rational))))
(define (real->complex real)
  (make-complex-form-real-imag real 0))
;;put
(put-coersion 'integer 'rational integer->rational) 
(put-coersion 'rational 'real rational->real)
(put-coersion 'real 'complex real->complex)
;;raise
#|
;;to the top type -- complex
(define (raise number)
  (define types '(number rational real complex))
  (define (try val left-types)
    (if (null? left-types)
        val
        (let ([next-type (car left-types)]
              [next-types (cdr left-types)]
              [cur-type (type-tag val)])
          (if (eq? (type-tag val) next-type)
              (try val next-types)
              (try ((get-coercion cur-type next-type) val) next-types)))))
  (try number types))
|#
#|
;;not the good codes
(define (raise number)
  (define types '(number rational real complex))
  (define (try left-types)
    (if (< (length left-types) 2)
        number
        ((get-coercion (type-tag number) (car left-types)) number)))
  (try (member (type-tag number) types)))
|#
(define (raise x)     (apply-generic 'raise x))

; in the integer package
(put 'raise  '(integer) (lambda (n) (make-rational n 1)))

; in the rational package
(define (rational->real r) (make-real (/ (numer r) (denom r)))) 
(put 'raise  '(rational) rational->real)

; in the real package
(define (real->complex r) (make-complex-from-real-imag r 0)) 
(put 'raise  '(real) real->complex)

;;0284
(define (type-lev type)
  (apply-generic 'type-level type))
(define (install-level-package) 
  (put 'level 'scheme-number 1) 
  (put 'level 'rational 2) 
  (put 'level 'real 3) 
  (put 'level 'complex 4)
  'done)
#|
;;there is a bug when type == null
(define (hightest-type types)
  (if (= (length types) 1)
      (car types)
      (let ([current-type (car types)]
            [rest-hightest-type (cdr types)])
        (if (> (type-lev current-type) (type-lev rest-hightest-type))
            current-type
            rest-hightest-type))))
|#
(define (hightest-type-lev args)
  (if (null? types)
      0
      (let ([cur-lev (type-lev (car types))]
            [rest-hightest-lev (hightest-type-lev (cdr args))])
        (if (< cur-lev rest-hightest-lev)
            rest-hightest-lev
            cur-lev))))
     
(define (raise-to val target-type-lev)
  (define (raise-iter arg)
    (let ([cur-lev (type-lev arg)])
      (cond ((= cur-lev target-type-lev) arg)
            ((< cur-lev target-type-lev) (raise-iter (raise arg)))
            (else "error, cant raise higher type to lower type" arg target-type-lev))))
  (raise-iter val))
     
(define (apply-with-raised-types args)
  (let ([target-type-lev (hightest-type-lev args)])
    (apply apply-generic
           op
           (map (lambda(arg)
                  (raise arg target-type-lev)))
           args)))
(define (apply-generic op . args)
    (let ([type-tags (map type-tag args)])
      (let ([proc (get op type-tags)])  
        (if proc
            (apply proc (map contents args))
            (apply-with-raised-types args))))
         
;;0285
(define (install-project-package) 
   ;; internal procedures 
   (define (complex->real x) 
     (make-real (real-part x))) 
   (define (real->integer x) 
     (round x)) 
   (define (rational->integer x) 
     (round (/ (car x) (cdr x)))) 
   ;; interface with system 
   (put 'project '(complex) 
        (lambda (x) (complex->real x))) 
   (put 'project '(real) 
        (lambda (x) (real->integer x))) 
   (put 'project '(rational) 
        (lambda (x) (rational->integer x))) 
   'done)
(define (project z) (appley-generic 'project z))
(define (drop z)
  ;;schemer-number's lev is 1
  (if (= (type-lev z) 1)
      z
      (let ([projected (project z)])
        (if (equ? projected (raise projected))
            (drop projected)
            z))))

(define (apply-generic op . args)
    ;;it doesn’t make sense to drop the result of all generic operations, 
    ;;for example predicates, raise, eq?
    (define (reduce-type val)
      (cond ((eq? op 'sub) (drop val))
            ((eq? op 'add) (drop val))
            (else val)))
    (let ([type-tags (map type-tag args)])
      (let ([proc (get op type-tags)])  
        (if proc
            (apply proc (map contents args))
            (apply-with-raised-types args)))))
;;0286
;;change the + - * / to add sub mul div to deal with other types, such as schemer-number\rational etc.

(define (add-complex z1 z2)
  (make-from-real-imag (add (real-part z1) (real-part z2))
                       (add (imag-part z1) (imag-part z2)))

(define (sub-complex z1 z2)
  (make-from-real-imag (sub (real-part z1) (real-part z2))
                       (sub (imag-part z1) (imag-part z2)))

(define (mul-complex z1 z2)
  (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                     (add (angle z1) (angle z2)))

(define (div-complex z1 z2)
  (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                     (sub (angle z1) (angle z2)))
;;;add into global
(define (sine x) (apply-generic 'sine x)) 
(define (cosine x) (apply-generic 'cosine x))
;;;; add into scheme-number package 
(put 'sine 'scheme-number 
       (lambda (x) (tag (sin x)))) 
(put 'cosine 'scheme-number 
     (lambda (x) (tag (cos x)))) 

;;;; add into rational package 
(put 'sine 'rational 
     (lambda (x) (tag (sin x)))) 
(put 'cosine 'rational 
     (lambda (x) (tag (cos x)))) 
;;;other operators needed in the other operators of complex type will deal with schemer-number and rational.
  

  