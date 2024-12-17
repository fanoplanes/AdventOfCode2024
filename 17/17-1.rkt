#lang racket

(define input (string-split (file->string "input") "\n\n"))

(define regs (map string->number (regexp-match* #rx"[0-9]+" (car input))))
(define ops (map string->number (regexp-match* #rx"[0-9]+" (cadr input))))
(define l-o (length ops))
(define (a-o i)
  (list-ref ops i))

(let process ([r regs] [op 0] [o '()])
  (define co
    (list 0 1 2 3 (list-ref r 0) (list-ref r 1) (list-ref r 2)))
  (cond
    [(>= op l-o) (reverse o)]
    [(= (a-o op) 0) (process (list (quotient (list-ref r 0) (expt 2 (list-ref co (a-o (add1 op))))) (list-ref r 1) (list-ref r 2)) (+ op 2) o)]
    [(= (a-o op) 1) (process (list (list-ref r 0) (bitwise-xor (list-ref r 1) (a-o (add1 op))) (list-ref r 2)) (+ op 2) o)]
    [(= (a-o op) 2) (process (list (list-ref r 0) (modulo (list-ref co (a-o (add1 op))) 8) (list-ref r 2)) (+ op 2) o)]
    [(= (a-o op) 3) (if (= (list-ref co 4) 0) (process r (+ 2 op) o) (process r (a-o (add1 op)) o))]
    [(= (a-o op) 4) (process (list (list-ref r 0) (bitwise-xor (list-ref r 1) (list-ref r 2)) (list-ref r 2)) (+ op 2) o)]
    [(= (a-o op) 5) (process r (+ op 2) (cons (modulo (list-ref co (a-o (add1 op))) 8) o))]
    [(= (a-o op) 6) (process (list (list-ref r 0) (quotient (list-ref r 0) (expt 2 (list-ref co (a-o (add1 op))))) (list-ref r 2)) (+ op 2) o)]
    [(= (a-o op) 7) (process (list (list-ref r 0) (list-ref r 1) (quotient (list-ref r 0) (expt 2 (list-ref co (a-o (add1 op)))))) (+ op 2) o)]))
