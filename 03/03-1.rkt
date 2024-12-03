;; LOGIC OF THE PROGRAM
;;
;; -> load the input as a list of strings (lines)
;;      stored in variable `input`
;; -> extract the arguments of mul() on each line (regex nonsense)
;;      achieved by function `matches` (matches line), where argument
;;      `line` is a string (corresponds to a single line from input)
;; -> turn the result of regex matching from strings to numbers
;;      achieved by function `numeric` (numeric line) which again takes
;;      a string (with same meaning as above)
;; -> add up the results of calling mul() within a line
;;      achieved by function `answer`
;; -> add up the results from all lines
;;      acheved by function `result`, `result` takes the variable `input`
;;      as its argument (the type is a list of strings)
;;
;; the answer is calculated like this:
;;
;;     one line is processed by composing like this:
;;         (answer (numeric (matches x)))
;;     where `x` is a dummy variable
;;
;;     then the function `result` loops over the whole input (list, really)
;;     plugging each line into that function composition and
;;     adding up the results
#lang racket

(define input (file->lines "input"))

;; preforms a regex matching on one line of the input
;; regex:
;;     matches the string mul(<number>,<number>)
;;     by default, regexp-match* returns a list of matches as follows:
;;         it returns the whole regex match and then returns the matches of the
;;         capture groups
;;     by using #:match-select cdr, I only get the cdr of that list - the list
;;     without the first element - the whole regex match
;;
;;    for example:
;;    regexp-match* #rx"([0-9]+),([0-9]+)"
;;    running on the string "45,57" would return
;;    ("45.57", "45", "57")
;;    using #:match-select cdr will discard the first whole match
;;    since I do not need the whole match, I just need to extract the arguments
;;    of the mul() function, I can safely discard them
;;
;; this function will then turn a single line into a list of lists of matches
;; so the structure will be as follows:
;;     (("x1" "y1") ("x2" "y2") ("x3" "y3") ...)
;; where each xn yn is a pair of arguments of a mul() function that exists on
;; that line
(define (matches line)
  (regexp-match* #rx"mul\\(([0-9]+),([0-9]+)\\)" line #:match-select cdr))

;; regexp-match* will parse a string and return substrings, so I need to
;; turn those strings into numbers
;; since the structure of the result of regexp-match* is a sort of list of lists,
;; I need to go "two levels deep"
;; I walk the list in a for loop, and then use a map to call string->number on
;; each element of the sublist (really, each argument of the mul() function)
;; I guess I *could* probably use a nested map or a nested for loop,
;; but this is easiest for me to think about
;;
;; also, I'm using for/list, so that the result of each loop gets appended into
;; a list, and the whole for loop returns a list of all results
(define (numeric line)
  (for/list ([i (matches line)])
    (map string->number i)))

;; here I do the exact sort of nesting of loops,
;; but in this case the "inner loop" is done with apply
;; apply takes two arguments, first is a function, second is an array
;; it's essentially a way to pass the array into the function
;; for example:
;;    + is variadic in Racket, so we could reasonably do something like
;;        (+ 1 2 3 4 5)
;;    but what if we have the array (1 2 3 4 5) bound to some variable,
;;    let's say a?
;;    really, what we need to do, is take the array (1 2 3 4 5) and sneak
;;    all the elements of the array as arguments to +
;;    that's what apply is for!
;;    (+ 1 2 3 4 5) is the same as (apply + a)
;;
;; here, I'm instead using for/sum, so that all results get accumulated as a sum
(define (answer line)
  (for/sum ([i (numeric line)]) (apply * i)))

;; all functions above run on a single line of the input
;; here, I add all the sub-results together
(define result (for/sum ([line input]) (answer line)))

result
