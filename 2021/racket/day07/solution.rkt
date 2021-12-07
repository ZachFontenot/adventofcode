#lang racket

(require advent-of-code
         algorithms
         threading)

(define test "16,1,2,0,4,2,7,1,2,14")
(define test-crabs (map string->number (string-split (string-trim test) ",")))

(define crabs
  (~> (open-aoc-input (find-session) 2021 7 #:cache #t)
      port->string
      string-trim
      (string-split ",")
      (map string->number _)))

(define (highest-number lst)
  (foldl max (car lst) (cdr lst)))

(define (sum-dist crabs pos)
  (sum (map (λ (n) (abs (- n pos))) crabs)))

(define (sum-dist-part-two crabs pos)
  (sum (map (λ (n) (let ([e (abs (- n pos))])
                     (/ (* e (+ e 1)) 2))) crabs))) ; thanks math

(define (calc-fuel crabs sum-func)
  (for/fold ([sum-for-position +inf.0]
             [lowest-position -1])
            ([position (in-inclusive-range 0 (highest-number crabs))])
    (let ([current-sum (sum-func crabs position)])
      (if (> sum-for-position current-sum)
          (values current-sum position)
          (values sum-for-position lowest-position)))))

(define-values (fuel1 pos1) (calc-fuel crabs sum-dist))
(define-values (fuel2 pos2) (calc-fuel crabs sum-dist-part-two))
