#lang racket

(define input (map string->number (file->lines "input.txt")))

; This is basically a while loop with tail recursion.
; Instead of having a solution coupled to the input, I just count all increasing values
; so for part two I just alter the input list into the sums and run the same function
(define (find-increasing lines)
  (define (loop lines prev inc-count)
    (if (and (list? lines) (not (empty? lines)))
        (let ([num (car lines)])
          (cond
            ((< prev num) (loop (cdr lines) num (add1 inc-count)))
            ((>= prev num) (loop (cdr lines) num inc-count))
            (else (loop (cdr lines) num inc-count))))
        inc-count))
  (loop (cdr lines) (car lines) 0))

(define (process-for-part-two lines)
  (define (loop l acc-list)
    (if (>= (length l) 3)
        (loop (cdr l) (append acc-list (list (apply + (take l 3)))))
        acc-list))
  (loop lines '()))

; I basically just use racket-mode to test, so no executable output.
(define solution-one (find-increasing input))
(define solution-two (find-increasing (process-for-part-two input)))
