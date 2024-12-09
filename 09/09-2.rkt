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
             [lrp (- size 2)]
             [rrp (sub1 size)])
    (cond
      [(<= lrp 0) s]
      [(>= lp lrp) (swap s 0 (sub1 lrp) lrp)]
      [(eq? (vector-ref s lrp) (vector-ref s rrp)) (swap s lp (sub1 lrp) rrp)]
      [(not (string? (vector-ref s lp))) (swap s (add1 lp) lrp rrp)]
      [(not (number? (vector-ref s rrp))) (swap s lp lrp (sub1 rrp))]
      [else
       (define val (vector-ref s rrp))
       (define counts (- rrp lrp))
       (cond
         [(for/and ([i counts])
            (string? (vector-ref s (+ lp i))))
          (for ([i counts])
            (vector-set*! s (+ lp i) val (add1 (+ lrp i)) "."))
          (swap s 0 (sub1 lrp) lrp)]
         [else (swap s (add1 lp) lrp rrp)])])))

(for/sum ([i (in-range size)])
         (if (number? (vector-ref swapped i))
             (* i (vector-ref swapped i))
             0))
