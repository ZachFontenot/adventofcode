#lang racket

(require megaparsack
         megaparsack/text
         data/monad
         data/applicative)

(define input (file->lines "input.txt"))

; Feel like megaparsack is overkill, but I assume it'll be useful for later days
(define direction-and-val/p
  (do [command <- (many/p letter/p)]
      space/p
      [num <- integer/p]
      (pure (list command num))))

; parse-result! -> '((#\chars...) value) 

(define (find-position input matcher)
  (define (loop input hoz aim depth)
    (if (null? input)
        (* hoz depth)
        (let* ([line (car input)]
               [result (parse-result! (parse-string direction-and-val/p line))]
               [direction (list->string (car result))]
               [val (cadr result)])
          (matcher input direction val hoz aim depth loop))))
    (loop input 0 0 0))
  
(define (part-one xs direction val hoz aim depth fn) ;;TOO MANY PARAMS
  (match direction 
    ["forward" (fn (rest xs) (+ hoz val) aim depth)] ;; just pass aim through
    ["up" (fn (rest xs) hoz aim (- depth val))]
    ["down" (fn (rest xs) hoz aim (+ depth val))]))

(define (part-two xs direction val hoz aim depth fn)
  (match direction
    ["forward" (fn (rest xs) (+ hoz val) aim (+ depth (* aim val)))]
    ["up" (fn (rest xs) hoz (- aim val) depth)]
    ["down" (fn (rest xs) hoz (+ aim val) depth)]))

(define (try-two input two?)
  (for/fold ([hoz 0] [aim 0] [depth 0]
             #:result (* hoz depth))
            ([line input])
    (let ([dir (car (string-split line " "))]
          [val (string->number (cadr (string-split line " ")))])
      (if two? ; I wish I could think of a better way for this
          (match dir
            ["forward" (values (+ hoz val) aim (+ depth (* aim val)))]
            ["up" (values hoz (- aim val) depth)] 
            ["down" (values hoz (+ aim val) depth)])
          (match dir
            ["forward" (values (+ hoz val) aim depth)]
            ["up" (values hoz aim (- depth val))]
            ["down" (values hoz aim (+ depth val))])))))
