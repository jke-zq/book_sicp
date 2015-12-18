#lang racket

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree) 
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-do bits current-branch)
    (if (null? bits)
        '()
        (let ([next-branch (choose-branch (car bits) current-branch)])
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-do (cdr bits) tree))
              (decode-do (cdr bits) next-branch)))))
  (decode-do bits tree))
(define (choose-branch bit branch)
  (cond ([= bit 0] (left-branch branch))
        ([= bit 1] (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))
       
(define (adjoin-set x set)
  (cond ([null? set] (list x))
        ([< (weight x) (weight (car set))] (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ([pair (car pairs)])
        (adjoin-set (make-leaf (car pair)
                               (cdr pair))
                    (make-leaf-set (cdr pairs))))))
  
;;0267
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree)

;;0268
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
(define (element-of-set? x set)
  (cond ([null? set] false)
        ([equal? x (car set)] true)
        (else (element-of-set? x (cdr set)))))
#|
;;ugly codes
(define (encode-symbol letter tree)
  (if (or (null? tree) (leaf? tree)) '()
      (let ([left (left-branch tree)]
            [right (right-branch tree)])
        (cond
          ([and (null? left) (null? right)] '())
          ([and (not (null? left)) (element-of-set? letter (symbols left))] (cons 0 (encode-symbol letter left)))
          ([and (not (null? right)) (element-of-set? letter (symbols right))] (cons 1 (encode-symbol letter right)))
          (else (error "letter not in the tree ENCODE-SYMBOL" letter))))))
|#
(define (encode-symbol letter tree)
  (cond ([or (null? tree) (leaf? tree)] '())
        ([element-of-set? letter (symbols tree)]
         (let ([left (left-branch tree)]
               [right (right-branch tree)])
           (if (element-of-set? letter (symbols left))
               (cons 0 (encode-symbol letter left))
               (cons 1 (encode-symbol letter right)))))
        (else (error "letter not in the tree ENCODE-SYMBOL" letter))))

;;0269
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge leaves)
  (if (= (length leaves) 1)
      (car leaves)
      (let ([first (car leaves)]
            [second (cadr leaves)])
        (successive-merge (adjoin-set (make-code-tree first second) (cddr leaves))))))
      
