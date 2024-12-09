#lang racket/base

(require racket/file
         racket/string
         racket/vector)

(define the-file (file->string "input"))

(define input (filter non-empty-string? (string-split (string-trim the-file "\n") "")))
(define nums (map string->number input))
(define vnums (list->vector nums))

(define structure
  (apply vector-append
         (for/list ([i (in-range (length nums))])
           (cond
             [(even? i) (make-vector (vector-ref vnums i) (/ i 2))]
             [(odd? i) (make-vector (vector-ref vnums i) ".")]))))

(define size (vector-length structure))

(define swapped
  (let swap ([s structure]
             [lp 0]
             [rp (sub1 size)])
    (cond
      [(= lp rp) s]
      [(not (string? (vector-ref s lp))) (swap s (add1 lp) rp)]
      [(not (number? (vector-ref s rp))) (swap s lp (sub1 rp))]
      [else
       (define val (vector-ref s rp))
       (vector-set*! s lp val rp ".")
       (swap s (add1 lp) (sub1 rp))])))

(for/sum ([i (in-range size)])
         (if (number? (vector-ref swapped i))
             (* i (vector-ref swapped i))
             0))
