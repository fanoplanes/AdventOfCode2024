#lang racket

(define input (map string->list (file->lines "input")))

(define (access-arr arr x y)
  (list-ref (list-ref arr y) x))

(define (access x y)
  (access-arr input x y))

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

(define (antinodes pt-1 pt-2)
  (let* ([pt1 (car pt-1)]
         [pt2 (car pt-2)])
    (list (list (- (* (first pt1) 2) (first pt2)) (- (* (second pt1) 2) (second pt2)))
          (list (- (* (first pt2) 2) (first pt1)) (- (* (second pt2) 2) (second pt1))))))

(define (antinodes-of-type type)
  (filter in-bounds?
          (remove-duplicates (append* (for*/list ([i type]
                                                  [j type]
                                                  #:unless (eq? i j))
                                        (antinodes i j))))))

(define all-antinodes (remove-duplicates (append* (map antinodes-of-type grouped))))

(length all-antinodes)
