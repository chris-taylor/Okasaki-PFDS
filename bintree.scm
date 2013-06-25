#lang scheme

; signature set
; sig
;   type elem
;   type set
;   
;   val empty     :: set
;   val insert    :: elem -> set -> set
;   val is-member :: elem -> set -> bool
; end

(define empty '())

(define is-empty null?)

(define (mk-node value left right)
  (list value left right))

(define (node-val node)
  (if (null? node)
    (error "Empty node -- GET_VALUE")
    (car node)))

(define (node-left node)
  (if (null? node)
    (error "Empty node -- GET-LEFT")
    (car (cdr node))))

(define (node-right node)
  (if (null? node)
    (error "Empty node -- GET-RIGHT")
    (car (cdr (cdr node)))))

(define (is-member x node)
  (if (is-empty node)
    #f
    (let ((y (node-val node)))
      (cond
        ((< x y) (is-member x (node-left node)))
        ((> x y) (is-member x (node-right node)))
        (else #t)))))

(define (insert x node)
  (if (is-empty node)
    (mk-node x empty empty)
    (let ((y (node-val node))
          (a (node-left node))
          (b (node-right node)))
      (if (< x y)
        (mk-node y (insert x a) b)
        (mk-node y a (insert x b))))))

(define (from-list xs)
  (foldl insert empty xs))

; Exercise 2.2 -- rewrite is-member to take no more than d+1 comparisons,
; where d is the depth of the tree

(define (is-member-new x node)
  
  ; Here z is the last value for which (< x y) returned false, i.e. it is the
  ; last value for which (= x y) might have been true.
  (define (is-member-helper n z)
    (if (is-empty n)
      (= x z)
      (let ((y (node-val n)))
        (if (< x y)
          (is-member-helper (node-left n)  z)
          (is-member-helper (node-right n) y)))))

  (is-member-helper node null))
