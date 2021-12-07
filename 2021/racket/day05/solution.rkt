#lang racket

(require threading
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
      (pure (list (list first-x first-y) (list second-x second-y)))))

(define (parse-input input)
  (for/list ([line input])
    (parse-result! (parse-string points/p line))))

;; maybe wanna make a hash of points? and inc when we hit it?
;; GOING IMPERATIVE FOR THIS HASH, FUCK IT

(define h (make-hash))

(define (do-points pair)
  (let ([first-x (first (car pair))]
        [first-y (second (car pair))]
        [second-x (first  (cadr pair))]
        [second-y (second (cadr pair))])
    (if (or (= first-x second-x)
            (= first-y second-y))
        (determine first-x first-y second-x second-y)
        '())))

(define (determine x1 y1 x2 y2)
  (cond
    [(and (= x1 x2) (< y1 y2)) (do-hash-ys y1 y2 x1)]
    [(and (= x1 x2) (< y2 y1)) (do-hash-ys y2 y1 x1)]
    [(and (= y1 y2) (< x1 x2)) (do-hash-xs x1 x2 y1)]
    [else (do-hash-xs x2 x1 y1)]))

(define (do-hash-xs p1 p2 c)
  (for ([p (in-inclusive-range p1 p2)])
    (let ([point (list p c)])
      (if (hash-has-key? h point)
          (hash-update! h point add1)
          (hash-set! h point 1)))))

(define (do-hash-ys p1 p2 c)
  (for ([p (in-inclusive-range p1 p2)])
    (let ([point (list c p)])
      (if (hash-has-key? h point)
          (hash-update! h point add1)
          (hash-set! h point 1)))))

(define (count-crossover point-hash)
  (for/sum ([val (hash-values point-hash)])
    (if (< 1 val)
        1
        0)))

;; note to self can't run more than once in repl because mutable hash
; This is marginally slow, but OH WELL
(define (part-one input)
  (let ([processed (parse-input input)])
    (for ([points processed])
      (do-points points)))
  (count-crossover h))

;; Need to figure this out better.

(define (points-part-two pair)
  (let ([first-x (first (car pair))]
        [first-y (second (car pair))]
        [second-x (first  (cadr pair))]
        [second-y (second (cadr pair))])
    (cond
      [(eq? first-x second-x)
       (if (< first-y second-y)
           (do-hash-ys first-y second-y first-x)
           (do-hash-ys second-y second-x first-x))]
      [(eq? first-y second-y)
       (if (< first-x second-x)
           (do-hash-xs first-x second-x first-y)
           (do-hash-xs second-x first-x first-y))]
     [else (do-hash first-x first-y second-x second-y)])))

(define (do-hash x1 y1 x2 y2)
  (for ([x (if (<= x1 x2)
                (in-inclusive-range x1 x2)
                (in-list (reverse (range x2 (add1 x1)))))]
         [y (if (<= y1 y2)
                (in-inclusive-range y1 y2)
                (in-list (reverse (range y2 (add1 y1)))))])
    (let ([point (list x y)])
      (if (hash-has-key? h point)
          (hash-update! h point add1)
          (hash-set! h point 1)))))

(define (part-two input)
  (let ([processed (parse-input input)])
    (for ([points processed])
      (points-part-two points)))
  (count-crossover h))
