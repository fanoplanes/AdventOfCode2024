#lang racket/base

(require racket/file)

(define input (file->string "input"))

(define split (regexp-split #rx"don't\\(\\).*?(do\\(\\)|$)" input))

(define (matches line)
  (regexp-match* #rx"mul\\(([0-9]+),([0-9]+)\\)" line #:match-select cdr))

(define (numeric line)
  (for/list ([i (matches line)])
    (map string->number i)))

(define (answer line)
  (for/sum ([i (numeric line)]) (apply * i)))

(define result (for/sum ([i split]) (answer i)))

result
