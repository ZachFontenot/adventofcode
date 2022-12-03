#lang racket

(require advent-of-code
         algorithms
         threading)

(define input
  (~>
   (fetch-aoc-input (find-session) 2022 3 #:cache #t)
   (string-split)))

(define alphabet (string->list "abcdefghijklmnopqrstuvwxyz"))
(define alphas (append alphabet (map char-upcase alphabet)))
;; using just zip gave (hash (#\a . (1)))
(define priority (make-immutable-hash (zip-with cons alphas (inclusive-range 1 52))))

(define (solution-a)
  (for/sum ([rucksacks-str input])
    (let ([len (quotient (string-length rucksacks-str) 2)])
      (~>> rucksacks-str
           string->list
           ;; capture ~>> output to put in c-w-v's generator arg
           ((λ (xs)
              (call-with-values
               (λ () (split-at xs len)) set-intersect)))
           car
           (hash-ref priority)))))

(define (solution-b)
  (for/sum ([elf-group (in-slice 3 input)])
    (~>> elf-group
         (map string->list)
         (apply set-intersect _)
         car
         (hash-ref priority))))
     
