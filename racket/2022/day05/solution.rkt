#lang racket

(require advent-of-code
         data/applicative
         data/monad
         megaparsack
         megaparsack/text
         threading)

(define test "
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(define input
  (~>
   (fetch-aoc-input (find-session) 2022 5 #:cache #t)
   ;test
   (string-split "\n\n")))
;; (car is stacks, cdr is instructions)

(define (transpose list-of-lists)
  (apply map list list-of-lists))

(define fuck-it
  (set #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(define (get-stacks set-list stack-list)
  (filter (λ (chars) (set-member? set-list (last chars))) stack-list))

(define (make-stack-hash stacks)
  (for/hash ([key (inclusive-range 1 (length stacks))])
    (values key (list-ref stacks (- key 1)))))

(define stacks
  (~>> (car input)
      (string-split _ "\n")
      (map string->list)
      transpose
      (get-stacks fuck-it)
      (map (λ~>>
            (filter (λ (char) (not (equal? #\space char))))))
      (map (λ (ls) (reverse (cdr (reverse ls)))))
      make-stack-hash))

;; move n from from-stack to to-stack
(define instruction-parser/p
  (do (string/p "move")
      space/p
      [n <- integer/p]
      space/p
      (string/p "from")
      space/p
      [from-stack <- integer/p]
      space/p
      (string/p "to")
      space/p
      [to-stack <- integer/p]
      (pure (list n from-stack to-stack))))

(define instructions
  (~>> (second input)
       (string-split _ "\n")
       (map (λ (str) (parse-result!
                      (parse-string instruction-parser/p str))))))

(define (do-instruction-a n from to)
  (if (= n 0)
      (values from to)
      (let-values ([(new-from new-to) (move-stack 1 from to)])
        (do-instruction-a (- n 1) new-from new-to))))

(define (do-instruction-b n from to)
  (move-stack n from to))

(define (move-stack n from to)  
  (match-let-values ([(vals stack) (split-at from n)])
    (values stack (append vals to))))

(define (read-instructions stacks instructions do-proc)
  (for/fold ([stacks stacks])
            ([instruction instructions])
    (match-let ([(list n from-stack-key to-stack-key) instruction])
      (let-values ([(new-from new-to)
                    (do-proc
                     n
                     (hash-ref stacks from-stack-key)
                     (hash-ref stacks to-stack-key))])
        (~> stacks
            (hash-set from-stack-key new-from)
            (hash-set to-stack-key new-to))))))

(define (solution-n do-proc)
  (let ([hash-stacks (read-instructions stacks instructions do-proc)])
    (for/list ([keys-n (hash-keys hash-stacks #t)])
      (car (hash-ref hash-stacks keys-n)))))

(define solution-a (list->string (solution-n do-instruction-a)))
(define solution-b (list->string (solution-n do-instruction-b)))
