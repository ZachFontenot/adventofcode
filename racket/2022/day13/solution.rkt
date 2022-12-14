#lang racket

(require advent-of-code
         threading)

(define tester
  "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")

(define packets
  (~>
   ;tester
   (fetch-aoc-input (find-session) 2022 13 #:cache #t)
   (string-replace "[" "(")
   (string-replace "]" ")")
   (string-replace "," " ")
   (string-split "\n")
   (filter (λ (str) (not (equal? "" str))) _)
   (map (compose read open-input-string) _)))
;; TOLD YOU I WAS TOO SLEEPY

(define (compare-pair pair1 pair2)
  (match* (pair1 pair2)
    [('() (list* _)) #t]
    [((list* _) '()) #f]
    [((list* a rst1) (list* a rst2)) (compare-pair rst1 rst2)]
    [((list* (? integer? p1) _) (list* (? integer? p2) _)) (< p1 p2)]
    [((list* (? list? rst1) _) (list* (? list? rst2) _))
     (compare-pair rst1 rst2)]
    [((list* (? list?) _) (list* (? integer? p2) rst2))
     (compare-pair pair1 (cons (list p2) rst2))]
    [((list* (? integer? p1) rst1) (list* (? list?) _))
     (compare-pair (cons (list p1) rst1) pair2)]))

(define (check-order ps)
  (for/sum  ([pair (in-slice 2 ps)]
             [idx (in-inclusive-range 1 (length ps))]
             #:when (compare-pair (car pair) (second pair)))
    idx))

(define (part-a)
  (~> packets
      (check-order)))

(define (part-b)
  (~> packets
      (append (list '((2)) '((6))))
      (sort compare-pair)
      (indexes-where (λ (ls) (member ls (list '((2)) '((6))))))
      (map add1 _)
      (apply * _)))

; 5198
; 22344
