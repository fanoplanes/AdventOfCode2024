#lang racket/base

(require racket/file
         racket/function
         racket/list)

(define input (map (curry regexp-match* #rx"-?[0-9]+") (file->lines "input")))

(define width 101)
(define height 103)

(define nums
  (for/list ([i input])
    (map string->number i)))

(define (move i n) ;x y vx vy
  (list (modulo (+ (list-ref i 0) (* (list-ref i 2) n)) width)
        (modulo (+ (list-ref i 1) (* (list-ref i 3) n)) height)))

(define quad-x (quotient width 2))
(define quad-y (quotient height 2))

(define moved (for/list ([i nums])
  (move i 100)))

(define removed-non-quad (for/list ([i moved]
           #:unless (or (= (list-ref i 0) quad-x) (= (list-ref i 1) quad-y)))
  i))

(define quads (for/list ([i removed-non-quad])
  (cond [(and (< (list-ref i 0) quad-x) (< (list-ref i 1) quad-y)) 0]
        [(and (> (list-ref i 0) quad-x) (< (list-ref i 1) quad-y)) 1]
        [(and (< (list-ref i 0) quad-x) (> (list-ref i 1) quad-y)) 2]
        [(and (> (list-ref i 0) quad-x) (> (list-ref i 1) quad-y)) 3])))

(define hehe (group-by (λ (x) x) quads))

(for/product ([i hehe])
  (length i))
