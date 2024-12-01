#lang racket

(define input-list (file->lines "input"))

(define split (map (λ (str) (string-split str)) input-list))

(define fst (map car split))
(define snd (map cadr split))

(define fst-numeric (map string->number fst))
(define snd-numeric (map string->number snd))

(for/sum ([i fst-numeric])
  (* i (count (λ (num) (= i num)) snd-numeric)))
