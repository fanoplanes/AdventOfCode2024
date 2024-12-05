#lang racket

(define input (file->lines "input"))

(define (split-and-num element str)
  (map string->number (string-split element str)))

(define (split-order element)
  (split-and-num element "|"))
(define (split-data element)
  (split-and-num element ","))

(define order (map split-order (take input (index-of input ""))))
(define data (map split-data (drop input (add1 (index-of input "")))))

(define (ord<? x y)
  (cond
    [(member (list y x) order) #f]
    [else #t]))

(define (is-sorted? arr)
  (for/and ([i (in-range 0 (length arr))])
    (for/and ([j (in-range (add1 i) (length arr))])
      (ord<? (list-ref arr i) (list-ref arr j)))))

(define (get-middle-number arr)
  (list-ref arr (/ (sub1 (length arr)) 2)))

(define unsorted
  (for/list ([i data]
             #:unless (is-sorted? i))
    i))

(define unsorted-now-sorted
  (for/list ([i unsorted])
    (sort i ord<?)))

(define result (for/sum ([i unsorted-now-sorted]) (get-middle-number i)))

(pretty-print result)
