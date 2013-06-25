#lang scheme

(define empty-heap '())

(define is-empty null?)

(define (mk-heap rank val left right)
  (list rank val left right))

(define (heap-rank heap)
  (if (is-empty heap)
    0
    (car heap)))

(define heap-val cadr)
(define heap-left caddr)
(define heap-right cadddr)

(define (makeT v a b)
  (let ((ra (heap-rank a))
        (rb (heap-rank b)))
    (if (> ra rb)
      (mk-heap (+ 1 rb) v a b)
      (mk-heap (+ 1 ra) v b a))))

(define (merge h1 h2)
  (cond
    ((is-empty h1) h2)
    ((is-empty h2) h1)
    (else
      (let ((x (heap-val h1))
            (y (heap-val h2)))
        (if (< x y)
          (makeT x (heap-left h1) (merge (heap-right h1) h2))
          (makeT y (heap-left h2) (merge h1 (heap-right h2))))))))

(define (insert x h)
  (merge (mk-heap 1 x empty-heap empty-heap) h))

(define (find-min h)
  (heap-val h))

(define (delete-min h)
  (merge (heap-left h) (heap-right h)))

(define (from-list x)
  (foldl insert empty-heap x))