#lang racket

(define input (map string->list (file->lines "input")))

(define (access-arr arr x y)
  (list-ref (list-ref arr y) x))

(define (access x y)
  (access-arr input x y))

(define height (length input))
(define width (length (car input)))

;; run through the array and note down the location and symbol on each non-dot point
;; the format of the point is ((x y) . #\symbol)
(define points
  (for*/list ([i width]
              [j height]
              #:unless (char=? (access i j) #\.))
    (cons (list i j) (access i j))))

;; group the antennas by type of symbol
;; returns a list of lists of points, each sublist contains the same symbol
;; ((list of #-antennas) (list of T-antennas) ...)
(define grouped (group-by cdr points))

(define (in-bounds? pt)
  (cond
    [(< (car pt) 0) #f]
    [(>= (car pt) width) #f]
    [(< (cadr pt) 0) #f]
    [(>= (cadr pt) height) #f]
    [else #t]))

;; return the antinodes for one pair of points
(define (antinodes pt-1 pt-2)
  (let* ([pt1 (car pt-1)]
         [pt2 (car pt-2)])
    (list (list (- (* (first pt1) 2) (first pt2)) (- (* (second pt1) 2) (second pt2)))
          (list (- (* (first pt2) 2) (first pt1)) (- (* (second pt2) 2) (second pt1))))))

;; returns a list of antinodes for one kind of antenna (takes a single sublist from `grouped)
(define (antinodes-of-type type)
  (filter in-bounds?
          (remove-duplicates (append* (for*/list ([i type]
                                                  [j type]
                                                  #:unless (eq? i j))
                                        (antinodes i j))))))

;; run through all antenna types (all of `grouped), put all antinodes in a list
(define all-antinodes (remove-duplicates (append* (map antinodes-of-type grouped))))

(length all-antinodes)
