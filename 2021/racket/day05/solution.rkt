;; Adapted from Jason Pollentier's (@grossvogel) Elixir solution
; because mine stunk

#lang racket

(require threading
         sugar
         megaparsack
         megaparsack/text
         data/monad
         data/applicative)

(define input (file->lines "input.txt"))

(define test
  '("0,9 -> 5,9"
    "8,0 -> 0,8"
    "9,4 -> 3,4"
    "2,2 -> 2,1"
    "7,0 -> 7,4"
    "6,4 -> 2,0"
    "0,9 -> 2,9"
    "3,4 -> 1,4"
    "0,0 -> 8,8"
    "5,5 -> 8,2"))
;; running into nested map calls, went megaparsack

(struct line (from to) #:transparent)
(struct point (x y) #:transparent)

(define points/p
  (do [first-x <- integer/p]
      (char/p #\,)
      [first-y <- integer/p]
      space/p
      (char/p #\-)
      (char/p #\>)
      space/p
      [second-x <- integer/p]
      (char/p #\,)
      [second-y <- integer/p]
      (pure (line (point first-x first-y) (point second-x second-y)))))

(define (parse-input input)
  (for/list ([line input])
    (parse-result! (parse-string points/p line))))

(define (horizontal? ln)
  (match ln
    [(line (point _ y) (point _ y)) #t]
    [_ #f]))

(define (vertical? ln)
  (match ln
    [(line (point x _) (point x _)) #t]
    [_ #f]))

(define (diagonal? ln)
  (and (not (horizontal? ln)) (not (vertical? ln))))

(define (x-diff ln)
  (match ln
    [(line (point x1 _) (point x2 _)) (- x2 x1)]
    [_ 0]))

(define (y-diff ln)
  (match ln
    [(line (point _ y1) (point _ y2)) (- y2 y1)]
    [_ 0]))

(define (points ln)
  (let* ([x (x-diff ln)]
         [y (y-diff ln)]
         [dist (max (abs x) (abs y))]
         [x-step (/ x dist)]
         [y-step (/ y dist)])
    (map (λ (distance) (point (+ (point-x (line-from ln)) (* x-step distance))
                              (+ (point-y (line-from ln)) (* y-step distance))))
         (inclusive-range 0 dist))))

(define (count-crossover point-hash)
  (for/sum ([val (hash-values point-hash)])
    (if (< 1 val)
        1
        0)))

(define (run-solve-one input)
  (~>> input
      parse-input
      (filter (λ (e) (not (diagonal? e))))
      (map points)
      flatten
      frequency-hash
      count-crossover))  

(define (run-solve-two input)
  (~>> input
       parse-input
       (map points)
       flatten
       frequency-hash
       count-crossover))
