#lang racket

(require graph)

(define width 71)
(define height 71)

(define input (map (curryr string-split ",") (file->lines "input")))
(define blocks
  (for/list ([i input])
    (map string->number i)))

(define area
  (for/vector ([j height])
    (for/vector ([i width])
      #\.)))

(define (update x)
  (for ([i x])
    (vector-set! (vector-ref area (cadr (list-ref blocks i))) (car (list-ref blocks i)) #\#)))

(define (access x y)
  (cond
    [(or (< x 0) (>= x width) (< y 0) (>= y height)) #\!]
    [(vector-ref (vector-ref area y) x)]))

(define (accessl l)
  (access (car l) (cadr l)))

(match-define (list up down left right)
              (list (list 0 -1) (list 0 1) (list -1 0) (list 1 0)))

(define (add-coords a b)
  (list (+ (car a) (car b))
        (+ (cadr a) (cadr b))))

(define (adjacent x y)
  (filter
   (lambda (x) (not (empty? x)))
   (for/list ([i (in-list (list up down left right))])
     (cond
       [(char=? (accessl (add-coords (list x y) i)) #\.) (list (list x y) (add-coords (list x y) i))]
       [else '()]))))

(define (find x)
  (update x)

  (define adjacency
    (append* (for*/list ([i width]
                         [j height]
                         #:unless (char=? (access i j) #\#))
               (adjacent i j))))

  (define graph (undirected-graph adjacency))
  (let-values ([(x y) (dijkstra graph (list 0 0))])
    (hash-ref x (list 70 70))))

(find 2912)
; yes I was recreating the whole graph every time, yes it's dumb
; that's why I ended up doing a binary search manually >:3
;(for/first ([i (in-range 1024 (length blocks))]
;            #:when (= (find i) +inf.0))
;  i)
(list-ref blocks 2911)
