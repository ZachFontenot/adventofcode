#lang racket

(require algorithms)

(define input (map string->number (file->lines "input.txt")))

; This is basically a while loop with tail recursion.
; Instead of having a solution coupled to the input, I just count all increasing values
; so for part two I just alter the input list into the sums and run the same function
(define (find-increasing lines)
  (define (loop lines prev inc-count)
    (if (and (list? lines) (not (empty? lines)))
        (let ([num (car lines)])
          (if
           (< prev num)
           (loop (cdr lines) num (add1 inc-count))
           (loop (cdr lines) num inc-count)))
        inc-count))
  (loop (cdr lines) (car lines) 0))

(define (process-for-part-two lines)
  (map sum (sliding lines 3))) ; algorithms package has exactly what we need

; I basically just use racket-mode to test, so no executable output.
(define solution-one (find-increasing input))
(define solution-two (find-increasing (process-for-part-two input)))