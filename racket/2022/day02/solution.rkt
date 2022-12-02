#lang racket

(require advent-of-code
         threading)

(define input (fetch-aoc-input (find-session) 2022 2 #:cache #t))
;; Sum each round
;; 1 X Rock 2 Y Paper 3 Z Scissors
;; +
;; 0 loss, 3 draw, 6 win
;; (("A" "X") ("B" "Z"))...etc

(define (rules-a strs)
  (match strs
    [(list "A" "Y") 8] ;; THE POWER OF MY MIDNIGHT BRAIN
    [(list "A" "X") 4]
    [(list "A" "Z") 3]
    [(list "B" "Y") 5]
    [(list "B" "X") 1]
    [(list "B" "Z") 9]
    [(list "C" "Y") 2]
    [(list "C" "X") 7]
    [(list "C" "Z") 6]))

;; 1 Rock 2 Paper 3 Scissors
(define (rules-b strs)
  (match strs
    [(list "A" "Y") 4] ;; Draw
    [(list "A" "X") 3] ;; Loss
    [(list "A" "Z") 8] ;; Win
    [(list "B" "Y") 5]
    [(list "B" "X") 1]
    [(list "B" "Z") 9]
    [(list "C" "Y") 6]
    [(list "C" "X") 2]
    [(list "C" "Z") 7]))

(define (solution-n rules)
  (~>> input
       (string-split _ "\n")
       (map (λ (str) (string-split str " ")))
       (map (λ (strs) (rules strs)))
       (apply +)))

(define solution-a
  (solution-n rules-a))
(define solution-b
  (solution-n rules-b))
