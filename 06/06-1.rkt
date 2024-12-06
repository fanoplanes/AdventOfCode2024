#lang racket

(require algorithms)

(define input (file->lines "input"))

(define split (map string->list input))

(define height (length split))
(define width (length (car split)))

(define (access x y)
  (list-ref (list-ref split x) y))

(define (find-char char)
  (for*/list ([i height]
              [j width]
              #:when (char=? (access i j) char))
    (list i j)))

(define start-index (car (find-char #\^)))
(define obstacles (find-char #\#))

(define dirs (list '(-1 0) '(0 1) '(1 0) '(0 -1)))

(define (next index dir)
  (zip-with + index (list-ref dirs dir)))

(define (walk index dir past)
  (define step (next index dir))
  (cond [(or (or (= (car step) height) (= (cadr step) width))
             (or (< (car step) 0) (< (cadr step) 0))) (reverse (cons index past))]
        [(member step obstacles) (walk index (modulo (add1 dir) 4) (cons index past))]
        [else (walk step dir (cons index past))]))

(length (remove-duplicates (walk start-index 0 '())))
