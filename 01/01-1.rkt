#lang racket/base

(require racket/file
         racket/string)

(define input-list (file->lines "input"))

(define split (map (λ (str) (string-split str)) input-list))

(define fst (map car split))
(define snd (map cadr split))

(define fst-numeric (map string->number fst))
(define snd-numeric (map string->number snd))

(define fst-sorted (sort fst-numeric <))
(define snd-sorted (sort snd-numeric <))

(for/sum ([i fst-sorted]
          [j snd-sorted])
  (abs (- i j)))
