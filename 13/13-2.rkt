#lang racket/base

(require racket/file
         racket/string)

(define input (string-split (file->string "input") "\n\n"))

(define (extract str)
  (map string->number (regexp-match* #rx"[0-9]+" str)))

(define num 10000000000000)

(define (solve str)
  (define inp (extract str))
  (define determinant (- (* (list-ref inp 0) (list-ref inp 3)) (* (list-ref inp 1) (list-ref inp 2))))
  (list (/ (- (* (+ (list-ref inp 4) num) (list-ref inp 3)) (* (+ (list-ref inp 5) num) (list-ref inp 2))) determinant)
        (/ (- (* (+ (list-ref inp 5) num) (list-ref inp 0)) (* (+ (list-ref inp 4) num) (list-ref inp 1))) determinant)))

(define (ok? lst)
  (for/and ([i lst])
    (integer? i)))

(define (value lst)
  (if (ok? lst) (+ (* 3 (car lst)) (cadr lst)) 0))

(for/sum ([i input])
  (value (solve i)))
