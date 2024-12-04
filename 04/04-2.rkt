#lang racket

(define input (file->lines "input"))

(define array (map (lambda (line) (string->list line)) input))

(define height (length array))

(define width (length (car array)))

(define padded-up-down
  (append (list (make-list width #\.) (make-list width #\.) (make-list width #\.))
          array
          (list (make-list width #\.) (make-list width #\.) (make-list width #\.))))

(define padded
  (for/list ([i padded-up-down])
    (append '(#\. #\. #\.) i '(#\. #\. #\.))))

(define (access x y)
  (list-ref (list-ref padded (+ x 3)) (+ y 3)))

(define len 1)

(define (get-sq x y)
  (for/list ([i (in-range (- x len) (+ x len 1))])
    (for/list ([j (in-range (- y len) (+ y len 1))])
      (access i j))))

(define (could-be? x y)
  (if (char=? (list-ref (list-ref (get-sq x y) 1) 1) #\A) #t #f))

(define (check-m-d x y) ;main diagonal
  (or (and (char=? (access (- x 1) (- y 1)) #\M)
           (char=? (access (+ x 1) (+ y 1)) #\S))
      (and (char=? (access (- x 1) (- y 1)) #\S)
           (char=? (access (+ x 1) (+ y 1)) #\M))))

(define (check-s-d x y) ;secondary diagonal
  (or (and (char=? (access (+ x 1) (- y 1)) #\M)
           (char=? (access (- x 1) (+ y 1)) #\S))
      (and (char=? (access (+ x 1) (- y 1)) #\S)
           (char=? (access (- x 1) (+ y 1)) #\M))))

(define (check x y)
  (cond
    [(could-be? x y) (if (and (check-m-d x y)
                              (check-s-d x y)) 1 0)]
    [else 0]))

(define answer
  (for*/sum ([i height]
             [j width])
    (check i j)))

answer
