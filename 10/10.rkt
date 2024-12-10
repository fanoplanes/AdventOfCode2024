#lang racket

(require algorithms)

(define (string->digits n)
  (map string->number (filter non-empty-string? (string-split n ""))))

(define input
  (vector-map (lambda (l) (list->vector (string->digits l)))
              (apply vector (file->lines "input"))))

(define height (vector-length input))
(define width (vector-length (vector-ref input 0)))

(define (access x y)
  (vector-ref (vector-ref input y) x))

(define zeros
  (for*/list ([i width]
              [j height]
              #:when (= (access i j) 0))
    (list i j)))

(define (can-be? x y i j)
  (cond
    [(or (< (+ x i) 0) (>= (+ x i) width)) #f]
    [(or (< (+ y j) 0) (>= (+ y j) height)) #f]
    [else #t]))

(define (neighbors x y)
  (for*/list ([i (in-list '(-1 0 1))]
              [j (in-list '(-1 0 1))]
              #:unless (or (and (= i 0) (= j 0)) (not (= (* i j) 0)))
              #:when (can-be? x y i j))
    (list (+ x i) (+ y j))))

(define (get-next-step x y)
  (for/list ([i (neighbors x y)]
             #:when (= (apply access i) (add1 (access x y))))
    i))

(define (go x y)
  (define next-step (get-next-step x y))
  (cond [(= (access x y) 9) (list x y)]
        [(empty? next-step) '()]
        [else (for/list ([i next-step])
                (go (car i) (cadr i)))]))


(define (part-1 i) (remove-duplicates (chunks-of (flatten (apply go i)) 2)))
(define (part-2 i) (chunks-of (flatten (apply go i)) 2))

(define part-1-answer (for/sum ([i zeros])
  (length (part-1 i))))

(define part-2-answer (for/sum ([i zeros])
  (length (part-2 i))))

(printf "Part 1: ~a\n" part-1-answer)
(printf "Part 2: ~a\n" part-2-answer)
