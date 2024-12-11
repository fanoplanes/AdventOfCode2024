#lang racket/base

(require racket/file)

(define input (file->lines "input"))

(define (matches line)
  (regexp-match* #rx"mul\\(([0-9]+),([0-9]+)\\)" line #:match-select cdr))

(define (numeric line)
  (for/list ([i (matches line)])
    (map string->number i)))

(define (answer line)
  (for/sum ([i (numeric line)]) (apply * i)))

(define result (for/sum ([line input]) (answer line)))

result
