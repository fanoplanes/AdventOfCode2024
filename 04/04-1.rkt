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

(define len 3)

(define (get-sq x y)
  (for/list ([i (in-range (- x len) (+ x len 1))])
    (for/list ([j (in-range (- y len) (+ y len 1))])
      (access i j))))

(define (could-be? x y)
  (if (char=? (list-ref (list-ref (get-sq x y) 3) 3) #\X) #t #f))

(define (check-dir x y dx dy)
  (and (char=? (access (+ x (* 1 dx)) (+ y (* 1 dy))) #\M)
       (char=? (access (+ x (* 2 dx)) (+ y (* 2 dy))) #\A)
       (char=? (access (+ x (* 3 dx)) (+ y (* 3 dy))) #\S)))

(define (check-dir-n x y dx dy)
  (if (check-dir x y dx dy) 1 0))

(define (check x y)
  (cond
    [(could-be? x y)
     (for*/sum ([i (in-list '(-1 0 1))]
                [j (in-list '(-1 0 1))]
                #:unless (= i j 0))
       (check-dir-n x y i j))]
    [else 0]))

(define number-of-solutions
  (for*/sum ([i height]
             [j width])
    (check i j)))

number-of-solutions
