;; LOGIC OF THE PROGRAM
;;
;; -> load the input as one big string
;;      stored in variable `input`
;; -> remove all parts that are disabled by don't() (regex nonsense)
;;      achieved by calling regexp-split on the input
;;      stored in variable `split`
;; -> extract the arguments of mul() on each line (more regex nonsense)
;;      achieved by function `matches` (matches line), where argument
;;      `line` is a string (corresponds to a single line from input)
;; -> turn the result of regex matching from strings to numbers
;;      achieved by function `numeric` (numeric line) which again takes
;;      a string (with same meaning as above)
;; -> add up the results of calling mul() within a line
;;      achieved by function `answer`
;; -> add up the results from all lines
;;      acheved by function `result`, `result` takes the variable `split`
;;      as its argument (the type is a list of strings)
;;
;; the answer is calculated like this:
;;
;;     the file is preprocessed by `split`, resulting in a list of strings
;;
;;     one line is processed by composing like this:
;;         (answer (numeric (matches x)))
;;     where `x` is a dummy variable
;;
;;     then the function `result` loops over the whole preprocessed input
;;     (obtained by calling the regexp-split (stored in variable `split`)),
;;     plugging each line into that function composition and
;;     adding up the results
#lang racket

;; in the second part, I'm not putting the input into lines, but I'm
;; putting the whole thing into a single string
;; I was afraid of the do() and don't() instructions spanning across lines
(define input (file->string "input"))

;; first, I was removing newlines, because I was afraid the '\n' characters
;; could mess with the result; that turned out to be unfounded
;; I'm keeping the line here to trace my solving process
;;(define continuous (regexp-replace* #rx"\n" input ""))

;; here I'm using regexp-split to essentially just omit all input that's
;; between a don't() and do() isntruction
;; notice, I'm matching the stuff between don't() and do() lazily
;; (that's what the ? in .*? is for)
;; also, notice that in the last group I match do() or end-of-line
;; to deal with the possibility of having a don't towards the end of input
;; without a do() to re-enable it, which otherwise wouldn't get caught
;; and deleted
;;
;;again, this splits the input into a list of substrings, so the functions
;;below again act on a single line at a time
(define split (regexp-split #rx"don't\\(\\).*?(do\\(\\)|$)" input))

;; code after this point is almost exactly the same as in part 1

(define (matches line)
  (regexp-match* #rx"mul\\(([0-9]+),([0-9]+)\\)" line #:match-select cdr))

(define (numeric line)
  (for/list ([i (matches line)])
    (map string->number i)))

(define (answer line)
  (for/sum ([i (numeric line)]) (apply * i)))

;; here's the only deviation from part 1, I don't call the function on input,
;; but on split; that's all that changed
(define result (for/sum ([i split]) (answer i)))

result
