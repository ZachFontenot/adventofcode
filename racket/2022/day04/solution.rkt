#lang racket

(require advent-of-code
         threading)

(define input
  (~>
   (fetch-aoc-input (find-session) 2022 4 #:cache #t)
   (string-split)
   (map (λ~> (string-split ",")) _)))
;; Lines!!!!

(define (range-overlaps-all? f-elf-f f-elf-s s-elf-f s-elf-s)
  (cond
    [(or (<= f-elf-f s-elf-f s-elf-s f-elf-s)
         (<= s-elf-f f-elf-f f-elf-s s-elf-s)) 1]
    [#t 0]))

(define (range-overlaps-any? f-elf-f f-elf-s s-elf-f s-elf-s)
  (cond
    [(<= (max f-elf-f s-elf-f)
         (min f-elf-s s-elf-s)) 1]
    [#t 0]))

(define (solution)
  (for/fold ([sum-a 0] [sum-b 0])
            ([line input])
    (define elves
      (~>> line
           (map (λ~> (string-split "-")))
           (map (λ~>> (map string->number)))
           (apply append)))
    (values (+ sum-a (apply range-overlaps-all? elves))
            (+ sum-b (apply range-overlaps-any? elves)))))
