#lang racket

(define input (map string->list (file->lines "input")))

(define (access-arr arr x y)
  (list-ref (list-ref arr x) y))

(define (access x y)
  (access-arr input y x))

(define height (length input))
(define width (length (car input)))

(define points
  (for*/list ([i width]
              [j height]
              #:unless (char=? (access i j) #\.))
    (cons (list i j) (access i j))))

(define grouped (group-by cdr points))

(define (in-bounds? pt)
  (cond
    [(< (car pt) 0) #f]
    [(>= (car pt) width) #f]
    [(< (cadr pt) 0) #f]
    [(>= (cadr pt) height) #f]
    [else #t]))

(define (antinodes pt-1 pt-2 n)
  (let* ([pt1 (car pt-1)]
         [pt2 (car pt-2)])
    (list (list (- (* (first pt1) n) (* (first pt2) (sub1 n)))
                (- (* (second pt1) n) (* (second pt2) (sub1 n))))
          (list (- (* (first pt2) n) (* (first pt1) (sub1 n)))
                (- (* (second pt2) n) (* (second pt1) (sub1 n)))))))

(define (antinodes-of-type type n)
  (filter in-bounds?
          (remove-duplicates (append* (for*/list ([i type]
                                                  [j type]
                                                  #:unless (eq? i j))
                                        (antinodes i j n))))))

(define (all-antinodes-for-n n)
  (remove-duplicates (append* (map (λ (tp) (antinodes-of-type tp n)) grouped))))

(define resonant
  (remove-duplicates (append* (for/list ([n (in-range 1 100)])
                                (all-antinodes-for-n n)))))

(length resonant)
