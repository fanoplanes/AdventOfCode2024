#lang racket

(define input (file->lines "input"))

(define separated (map string-split input))

(define numberified
  (for/list ([i separated])
    (map string->number i)))

(define (diff line)
  (for/list ([i (sub1 (length line))])
    (- (list-ref line (+ 1 i)) (list-ref line i))))

(define (change-ok? i)
  (cond
    [(> i 3) false]
    [(< i -3) false]
    [(= i 0) false]
    [else true]))

(define (check-change line)
  (for/and ([i (diff line)])
    (change-ok? i)))

(define (check-order line)
  (cond
    [(apply > line) true]
    [(apply < line) true]
    [else false]))

(define (process line)
  (if (and (check-order line) (check-change line)) 1 0))

(for/sum ([i numberified]) (process i))
