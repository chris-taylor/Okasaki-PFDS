#lang scheme

; sig
;   type stack a
;   
;   val empty :: stack a
;   val is-empty :: stack a -> bool
; 
;   val cons :: a -> stack a -> stack a
;   val head :: stack a -> a              (raises *empty*)
;   val tail :: stack a -> stack a        (raises *empty*)
; end

(define empty null)

(define is-empty null?)

(define head car)

(define tail cdr)

(define (++ xs ys)
  (if (is-empty xs)
    ys
    (cons (head xs) (++ (tail xs) ys))))

(define (update xs i y)
  (if (is-empty xs)
    (error "SUBSCRIPT")
    (if (= i 0)
      (cons y (cdr xs))
      (cons (car xs) (update (cdr xs) (- i 1) y)))))

; Exercise 2.1 -- suffixes

(define (suffixes xs)
  (if (is-empty xs)
    (list empty)
    (cons xs (suffixes (cdr xs)))))

; or in iterative style

(define (suffixes-new xs)
  (define (iter acc x)
    (if (is-empty x)
      (cons null acc)
      (iter (cons x acc) (cdr x))))
  (reverse (iter null xs)))

; This can be represented in O(n) space as
; (define y (suffixes x))
;
;   y --> . -> . -> . -> . -> '()
;         |    |    |    |
;         v    v    v    v
;   x --> 1 -> 2 -> 3 -> 4
;   