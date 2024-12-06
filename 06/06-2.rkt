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
(define init-obstacles (find-char #\#))

(define dirs (list '(-1 0) '(0 1) '(1 0) '(0 -1)))

(define (next index dir)
  (zip-with + index (list-ref dirs dir)))

(define (loop? index dir past)
  (if (member (list index dir) past) #t #f))

(define (walk index dir past)
  (define step (next index dir))
  (cond
    [(or (or (= (car step) height) (= (cadr step) width)) (or (< (car step) 0) (< (cadr step) 0)))
     (reverse (cons index past))]
    [(member step init-obstacles) (walk index (modulo (add1 dir) 4) (cons index past))]
    [else (walk step dir (cons index past))]))

(define p1-res (remove-duplicates (walk start-index 0 '())))

(define (walk-obst index dir past obstacles)
  (define step (next index dir))
  (if (loop? index dir past)
      1
      (cond
        [(or (or (= (car step) height) (= (cadr step) width)) (or (< (car step) 0) (< (cadr step) 0)))
         0]
        [(member step obstacles) (walk-obst index (modulo (add1 dir) 4) past obstacles)]
        [else (walk-obst step dir (cons (list index dir) past) obstacles)])))

(define chomk (chunks-of (cdr p1-res) 553)) ;magic number, yay (it's one less than the result from part 1)

(define (chunk ch)
  (for/sum ([i ch]) (walk-obst start-index 0 '() (cons i init-obstacles))))

(define fs
  (for/list ([i chomk])
    (future (lambda () (chunk i)))))
(for/sum ([i fs]) (touch i))
