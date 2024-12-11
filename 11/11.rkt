#lang racket

(require memo
         threading)

(define input (~> "input"
                  (file->string)
                  (string-split)
                  (map string->number _)))

(define (digit-length num)
  (~> num
      (log 10)
      add1
      floor
      inexact->exact))

(define (split stone)
  (call-with-values (λ () (quotient/remainder
                            stone (expt 10 (quotient (digit-length stone) 2))))
                    list))

(define/memoize
 (process-stone stone blinks)
 (cond
   [(= blinks 0) 1]
   [(= stone 0) (process-stone 1 (sub1 blinks))]
   [(even? (digit-length stone)) (apply + (map (curryr process-stone (sub1 blinks)) (split stone)))]
   [else (process-stone (* stone 2024) (sub1 blinks))]))

(define (blinks n)
  (for/sum ([i input])
    (process-stone i n)))

(printf "Part 1 (25 blinks): ~a\n" (blinks 25))
(printf "Part 2 (75 blinks): ~a\n" (blinks 75))
