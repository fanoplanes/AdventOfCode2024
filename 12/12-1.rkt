#lang racket

(require threading)

(define input
  (~> "input"
      file->lines
      (map (curryr string-split "") _)
      (map (curry filter non-empty-string?) _)))

(define width (length (car input)))
(define height (length input))

(define (access x y)
  (cond
    [(or (< x 0) (>= x width)) "nope"]
    [(or (< y 0) (>= y height)) "nope"]
    [else (list-ref (list-ref input y) x)]))

(define (neighbors x y)
  (list (list (sub1 x) y)
        (list (add1 x) y)
        (list x (sub1 y))
        (list x (add1 y))))

(define (sneighbors x y)
  (set (list (sub1 x) y)
       (list (add1 x) y)
       (list x (sub1 y))
       (list x (add1 y))))

(define (good x y)
  (filter (λ (x) (not (empty? x)))
          (for/list ([i (neighbors x y)])
            (if (string=? (access x y) (access (car i) (cadr i)))
                i
                '()))))

(define settified
  (for*/set ([i width]
             [j height])
    (list i j)))

(define (get-reg elem)
  (define s (mutable-set))
  (let get-region ([e elem])
    (define neighbors (good (car e) (cadr e)))
    (set-add! s e)
    (cond
      [(empty? neighbors) s]
      [else
       (for ([i neighbors])
         (if (set-member? s i)
             s
             (get-region i)))]))
  s)

(define (get-regs st)
  (cond
    [(set-empty? st) '()]
    [else
     (define chunk (get-reg (set-first st)))
     (cons chunk (get-regs (set-subtract st chunk)))]))

(define (perimeter reg)
  (for/sum ([i reg]) (set-count (set-subtract (sneighbors (car i) (cadr i)) reg))))

(for/sum ([i (get-regs settified)]) (* (perimeter i) (set-count i)))
