#lang racket

(require advent-of-code
         threading)

(define input (fetch-aoc-input (find-session) 2022 1 #:cache #t))

(define (common-parse)
  (~>> input
       (string-split _ "\n\n")
       (map (λ~>> string-split
                  (map string->number)
                  (apply +)))))

(define (solution-a)
  (~>> (common-parse)
       (apply max)))

(define (solution-b)
  (~>> (common-parse)
       (sort _ >)
       (take _ 3)
       (apply +)))
