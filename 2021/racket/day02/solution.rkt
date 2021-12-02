#lang racket

(require megaparsack
         megaparsack/text
         data/monad
         data/applicative)

(define input (file->lines "input.txt"))

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
               [direction (apply string (car result))]
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
