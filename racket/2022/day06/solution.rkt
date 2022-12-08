#lang racket

(require advent-of-code
         threading)

(define input
  (~>
   (fetch-aoc-input (find-session) 2022 6 #:cache #t)
   (string->list)
   (list->vector)))

;; index - 1
(define (windows num-to-check start end stack)
  (cond
    [(and (eqv? (check-duplicates stack) #f) (= (length stack) num-to-check)) (+ end 1)]
    [(eqv? (check-duplicates stack) #f)
     (windows num-to-check start (+ end 1) (append stack (list (vector-ref input (+ end 1)))))]
    [else (windows num-to-check (+ start 1) end (cdr stack))]))

(define solution-a (windows 4 0 0 '()))
(define solution-b (windows 14 0 0 '()))
