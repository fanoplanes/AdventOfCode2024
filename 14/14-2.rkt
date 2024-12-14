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

(define (move i n)
  (list (modulo (+ (list-ref i 0) (* (list-ref i 2) n)) width)
        (modulo (+ (list-ref i 1) (* (list-ref i 3) n)) height)))

(define (moved n)
  (for/list ([i nums])
    (move i n)))

(define (variance lst)
  (define xs (map car lst))
  (define ys (map cadr lst))
  (define av-x (/ (apply + xs) (length xs)))
  (define av-y (/ (apply + ys) (length ys)))
  (define var-x (apply + (map (λ (x) (expt (- x av-x) 2)) xs)))
  (define var-y (apply + (map (λ (y) (expt (- y av-y) 2)) ys)))
  (* var-x var-y))

(define (order n)
  (list (variance (moved n)) n))

(define (up-to-n n)
  (for/list ([i n])
    (order i)))

(argmin car (up-to-n 10000))
