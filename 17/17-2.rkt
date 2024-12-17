#lang racket

(define input (string-split (file->string "input") "\n\n"))

(define ops (map string->number (regexp-match* #rx"[0-9]+" (cadr input))))
(define l-o (length ops))
(define (a-o i)
  (list-ref ops i))

(define (do-stuff A)
  (define regs (list A 0 0))
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
    [(= (a-o op) 7) (process (list (list-ref r 0) (list-ref r 1) (quotient (list-ref r 0) (expt 2 (list-ref co (a-o (add1 op)))))) (+ op 2) o)])))

(println ops)
(+ (* (expt 8 15) 7)
             (* (expt 8 14) 2)
             (* (expt 8 13) 6)
             (* (expt 8 12) 0)
             (* (expt 8 11) 1)
             (* (expt 8 10) 1)
             (* (expt 8 9) 0)
             (* (expt 8 8) 5)
             (* (expt 8 7) 2)
             (* (expt 8 6) 2)
             (* (expt 8 5) 6)
             (* (expt 8 4) 2)
             (* (expt 8 3) 1)
             (* (expt 8 2) 6)
             (* (expt 8 1) 3)
             (* (expt 8 0) 3))

(define (make-num x pos initial)
  (+ (* (expt 8 pos) x) initial))

(define (good? num pos)
  (equal? (list-tail num pos) (list-tail ops pos)))

(define (git-gud posn ini)
  (for/list ([i 8]
             #:when (good? (do-stuff (make-num i posn ini)) posn))
    i))

;; I just did it semi-manually
;; the helper functions were useful, but I got too
;; lost in the sauce manually finding the numbers that worked
;;
;;put this into q.uiver.app lmao:
;; \[\begin{tikzcd}
;;     &&&&&&&&&& \textcolor{rgb,255:red,92;green,214;blue,92}{7} \\
;;     &&&&&&&& \textcolor{rgb,255:red,92;green,214;blue,92}{2} &&&& 4 \\
;;     &&&&&&&& \textcolor{rgb,255:red,92;green,214;blue,92}{6} &&&& 6 \\
;;     &&&&& \textcolor{rgb,255:red,92;green,214;blue,92}{0} &&&&& 6 && 6 \\
;;     &&&&& \textcolor{rgb,255:red,92;green,214;blue,92}{1} &&&&& 1 && 1 \\
;;     &&& \textcolor{rgb,255:red,92;green,214;blue,92}{1} &&& 6 &&&& 6 && 6 \\
;;     & \textcolor{rgb,255:red,92;green,214;blue,92}{0} && 2 &&& 0 &&&& 0 && 0 \\
;;     & \textcolor{rgb,255:red,92;green,214;blue,92}{5} && 0 &&& 5 &&&& 5 && 5 \\
;;     & \textcolor{rgb,255:red,92;green,214;blue,92}{2} && 3 &&& 2 &&&& 2 && 2 \\
;;     \textcolor{rgb,255:red,92;green,214;blue,92}{2} & 5 & 7 & 7 && 2 & 5 & 7 && 2 & 5 & 7 & 2 & 5 & 7 \\
;;     \textcolor{rgb,255:red,92;green,214;blue,92}{6} & 0 & 0 & 2 & 0 & 6 & 0 & 0 & 0 & 6 & 0 & 0 & 0 & 6 & 0 & 0 \\
;;     \textcolor{rgb,255:red,92;green,214;blue,92}{2} & 3 & 3 & 0 \\
;;     \textcolor{rgb,255:red,92;green,214;blue,92}{1} \\
;;     \textcolor{rgb,255:red,92;green,214;blue,92}{6} \\
;;     \textcolor{rgb,255:red,92;green,214;blue,92}{3}
;;     \arrow[color={rgb,255:red,92;green,214;blue,92}, from=1-11, to=2-9]
;;     \arrow[from=1-11, to=2-13]
;;     \arrow[color={rgb,255:red,92;green,214;blue,92}, from=2-9, to=3-9]
;;     \arrow[from=2-13, to=3-13]
;;     \arrow[color={rgb,255:red,92;green,214;blue,92}, from=3-9, to=4-6]
;;     \arrow[from=3-9, to=4-11]
;;     \arrow[from=3-13, to=4-13]
;;     \arrow[color={rgb,255:red,92;green,214;blue,92}, from=4-6, to=5-6]
;;     \arrow[from=4-11, to=5-11]
;;     \arrow[from=4-13, to=5-13]
;;     \arrow[color={rgb,255:red,92;green,214;blue,92}, from=5-6, to=6-4]
;;     \arrow[from=5-6, to=6-7]
;;     \arrow[from=5-11, to=6-11]
;;     \arrow[from=5-13, to=6-13]
;;     \arrow[color={rgb,255:red,92;green,214;blue,92}, from=6-4, to=7-2]
;;     \arrow[from=6-4, to=7-4]
;;     \arrow[from=6-7, to=7-7]
;;     \arrow[from=6-11, to=7-11]
;;     \arrow[from=6-13, to=7-13]
;;     \arrow[color={rgb,255:red,92;green,214;blue,92}, from=7-2, to=8-2]
;;     \arrow[from=7-4, to=8-4]
;;     \arrow[from=7-7, to=8-7]
;;     \arrow[from=7-11, to=8-11]
;;     \arrow[from=7-13, to=8-13]
;;     \arrow[color={rgb,255:red,92;green,214;blue,92}, from=8-2, to=9-2]
;;     \arrow[from=8-4, to=9-4]
;;     \arrow[from=8-7, to=9-7]
;;     \arrow[from=8-11, to=9-11]
;;     \arrow[from=8-13, to=9-13]
;;     \arrow[color={rgb,255:red,92;green,214;blue,92}, from=9-2, to=10-1]
;;     \arrow[from=9-2, to=10-2]
;;     \arrow[from=9-2, to=10-3]
;;     \arrow[from=9-4, to=10-4]
;;     \arrow[from=9-7, to=10-6]
;;     \arrow[from=9-7, to=10-7]
;;     \arrow[from=9-7, to=10-8]
;;     \arrow[from=9-11, to=10-10]
;;     \arrow[from=9-11, to=10-11]
;;     \arrow[from=9-11, to=10-12]
;;     \arrow[from=9-13, to=10-13]
;;     \arrow[from=9-13, to=10-14]
;;     \arrow[from=9-13, to=10-15]
;;     \arrow[color={rgb,255:red,92;green,214;blue,92}, from=10-1, to=11-1]
;;     \arrow[from=10-2, to=11-2]
;;     \arrow[from=10-3, to=11-3]
;;     \arrow[from=10-4, to=11-4]
;;     \arrow[from=10-6, to=11-5]
;;     \arrow[from=10-6, to=11-6]
;;     \arrow[from=10-7, to=11-7]
;;     \arrow[from=10-8, to=11-8]
;;     \arrow[from=10-10, to=11-9]
;;     \arrow[from=10-10, to=11-10]
;;     \arrow[from=10-11, to=11-11]
;;     \arrow[from=10-12, to=11-12]
;;     \arrow[from=10-13, to=11-13]
;;     \arrow[from=10-13, to=11-14]
;;     \arrow[from=10-14, to=11-15]
;;     \arrow[from=10-15, to=11-16]
;;     \arrow[color={rgb,255:red,92;green,214;blue,92}, from=11-1, to=12-1]
;;     \arrow[from=11-2, to=12-2]
;;     \arrow[from=11-3, to=12-3]
;;     \arrow[from=11-4, to=12-4]
;;     \arrow[color={rgb,255:red,92;green,214;blue,92}, from=12-1, to=13-1]
;;     \arrow[color={rgb,255:red,92;green,214;blue,92}, from=13-1, to=14-1]
;;     \arrow[color={rgb,255:red,92;green,214;blue,92}, from=14-1, to=15-1]
;; \end{tikzcd}\]
