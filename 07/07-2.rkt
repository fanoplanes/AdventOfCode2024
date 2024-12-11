#lang racket/base

(require racket/file
         racket/list
         racket/string)

(define input (map (lambda (line) (string-split line ":")) (file->lines "input")))
(define target (map (lambda (line) (string->number (car line))) input))
(define elements (map (lambda (line) (map string->number (string-split (cadr line)))) input))

(define (apply-proc proc lst)
  (cons (apply proc (take lst 2)) (drop lst 2)))

(define (concat x y)
  (string->number (string-append (number->string x) (number->string y))))

(define (process-line line)
  (flatten (for/list ([i (in-list (list * + concat))]
                      #:final (= (length line) 1))
             (if (= (length line) 1)
                 line
                 (process-line (apply-proc i line))))))

(define (can-be? target line)
  (if (member target (process-line line)) #t #f))

(for/sum ([i target] [j elements] #:when (can-be? i j)) i)
