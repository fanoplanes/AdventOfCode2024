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
  (list (list (sub1 x) y) (list (add1 x) y) (list x (sub1 y)) (list x (add1 y))))

(define (sneighbors x y)
  (set (list (sub1 x) y) (list (add1 x) y) (list x (sub1 y)) (list x (add1 y))))

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
             (get-region i )))]))
  s)

(define (get-regs st)
  (cond [(set-empty? st) '()]
        [else (define chunk (get-reg (set-first st)))
              (cons chunk (get-regs (set-subtract st chunk)))]))

(define regs (get-regs settified))

(define (u? elem)
  (string=? (access (car elem) (cadr elem)) (access (car elem) (sub1 (cadr elem)))))

(define (d? elem)
  (string=? (access (car elem) (cadr elem)) (access (car elem) (add1 (cadr elem)))))

(define (l? elem)
  (string=? (access (car elem) (cadr elem)) (access (sub1 (car elem)) (cadr elem))))

(define (r? elem)
  (string=? (access (car elem) (cadr elem)) (access (add1 (car elem)) (cadr elem))))

(define (corner? elem)
  (or
   (and (u? elem) (l? elem) (not (r? elem)) (not (d? elem)))
   (and (u? elem) (r? elem) (not (l? elem)) (not (d? elem)))
   (and (d? elem) (r? elem) (not (u? elem)) (not (l? elem)))
   (and (d? elem) (l? elem) (not (r? elem)) (not (u? elem)))))
(define (2corner? elem)
  (or
   (and (not (d? elem)) (not (l? elem)) (not (r? elem)) (u? elem))
   (and (not (u? elem)) (not (l? elem)) (not (r? elem)) (d? elem))
   (and (not (d? elem)) (not (u? elem)) (not (r? elem)) (l? elem))
   (and (not (d? elem)) (not (l? elem)) (not (u? elem)) (r? elem))))

(define (innie? elem)
  (or
   (and (u? elem) (r? elem)
        (equal? (string=? (access (car elem) (cadr elem)) (access (add1 (car elem)) (sub1 (cadr elem)))) #f))
   (and (d? elem) (r? elem)
        (equal? (string=? (access (car elem) (cadr elem)) (access (add1 (car elem)) (add1 (cadr elem)))) #f))
   (and (u? elem) (l? elem)
        (equal? (string=? (access (car elem) (cadr elem)) (access (sub1 (car elem)) (sub1 (cadr elem)))) #f))
   (and (d? elem) (l? elem)
        (equal? (string=? (access (car elem) (cadr elem)) (access (sub1 (car elem)) (add1 (cadr elem)))) #f))
   ))
(define (compl-innie? elem)
  (+
   (if (and (u? elem) (r? elem)
        (equal? (string=? (access (car elem) (cadr elem))
                          (access (add1 (car elem)) (sub1 (cadr elem))))
                #f)) 1 0)
   (if (and (d? elem) (r? elem)
        (equal? (string=? (access (car elem) (cadr elem))
                          (access (add1 (car elem)) (add1 (cadr elem))))
                #f)) 1 0)
   (if (and (u? elem) (l? elem)
        (equal? (string=? (access (car elem) (cadr elem))
                          (access (sub1 (car elem)) (sub1 (cadr elem))))
                #f)) 1 0)
   (if (and (d? elem) (l? elem)
        (equal? (string=? (access (car elem) (cadr elem))
                          (access (sub1 (car elem)) (add1 (cadr elem))))
                #f)) 1 0)
   ))

(define (alone? elem)
  (and (not (u? elem)) (not (d? elem)) (not (l? elem)) (not (r? elem))))

(define (sides reg)
  (for/sum ([i reg])
    (cond [(and (innie? i) (corner? i)) 2]
          [(corner? i) 1]
          [(2corner? i) 2]
          [(alone? i) 4]
          [(compl-innie? i)]
          [else 0])))


(for/sum ([i regs])
  (* (sides i) (set-count i)))
