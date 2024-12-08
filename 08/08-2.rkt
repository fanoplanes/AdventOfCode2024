#lang racket

(define input (map string->list (file->lines "input")))

(define (access-arr arr x y)
  (list-ref (list-ref arr x) y))

(define (access x y)
  (access-arr input y x))

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
    ; x coordinate okay?
    [(< (car pt) 0) #f]
    [(>= (car pt) width) #f]
    ; y coordinate okay?
    [(< (cadr pt) 0) #f]
    [(>= (cadr pt) height) #f]
    [else #t]))

;; return n-th antinodes for one pair of points
(define (antinodes pt-1 pt-2 n)
  (let* ([pt1 (car pt-1)]
         [pt2 (car pt-2)])
    (list (- (* (first pt1) n) (* (first pt2) (sub1 n)))
          (- (* (second pt1) n) (* (second pt2) (sub1 n))))))

;; returns a list of n-th antinodes for one kind of antenna (takes a single sublist from `grouped)
(define (antinodes-of-type type n)
  (filter in-bounds?
          (remove-duplicates (for*/list ([i type]
                                         [j type]
                                         #:unless (eq? i j))
                               (antinodes i j n)))))

;; get n-th antinodes for all types of antennas (runs through all of `grouped`)
(define (all-antinodes-for-n n)
  (remove-duplicates (append* (map (λ (tp) (antinodes-of-type tp n)) grouped))))

;; return all antinodes for harmonics 2<=n<100 (I have no real reason for stopping at 100,
;; I just increased it until the result stopped increasing)
;; TODO: find a better way to get all harmonics
(define resonant
  (remove-duplicates (append* (for/list ([n (in-range 1 100)])
                                (all-antinodes-for-n n)))))

(length resonant)
