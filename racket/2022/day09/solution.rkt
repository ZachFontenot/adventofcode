#lang racket

(require advent-of-code
         threading)

(define tester
  "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
")

(define movements
  (~>> ;tester
   (fetch-aoc-input (find-session) 2022 9 #:cache #t)
   (string-split _ "\n")
   (map string-split)
   (map (λ (command)
          (cons (first command)
                (list (string->number (second command))))))))

(struct pt (x y) #:transparent)

(define (abs-diff h t)
  (+ (abs (- (pt-x h) (pt-x t)))
     (abs (- (pt-y h) (pt-y t)))))

(define (pt-diff h t)
  (pt (- (pt-x h) (pt-x t))
      (- (pt-y h) (pt-y t))))

(define (add-pt point to-add)
  (pt (+ (pt-x point) (pt-x to-add))
      (+ (pt-y point) (pt-y to-add))))

(define (div-pt point to-div)
   (pt (quotient (pt-x point) to-div)
       (quotient (pt-y point) to-div)))

(define (move-head h dir)
  (match dir
    ["R" (add-pt h (pt 1 0))]
    ["U" (add-pt h (pt 0 1))]
    ["L" (add-pt h (pt -1 0))]
    ["D" (add-pt h (pt 0 -1))]))

(define (to-update-tail h t)
  (define diff-pt (pt-diff h t))
  (match-define (pt a b) diff-pt)
  (case (abs-diff h t)
    [(0 1) (pt 0 0)]
    [(2) (div-pt diff-pt 2)]
    [(3 4)
     (cond
       [(and (negative? a) (negative? b)) (pt -1 -1)]
       [(negative? a) (pt -1 1)]
       [(negative? b) (pt 1 -1)]
       [else (pt 1 1)])]))

(define (move-tail h t)
  (add-pt t (to-update-tail h t)))

(define (move-tails h ropes)
  (for/fold ([new-ropes (list)]
             [head h]
             #:result (reverse new-ropes))
            ([rope ropes])
    (define new-tail (move-tail head rope))
    (values (cons new-tail new-ropes) new-tail)))

(define (run)
  (for/fold ([seen (list (set (pt 0 0)) (set (pt 0 0)))]
             [h (pt 0 0)]
             [t (pt 0 0)]
             [tails (make-list 9 (pt 0 0))]
             #:result (cons (set-count (first seen)) (set-count (second seen))))
            ([move movements]
             #:do
             [(match-define (list dir amt) move)] [_ (in-range amt)])
    (define new-head (move-head h dir))
    (define new-tail (move-tail new-head t))
    (define updated-tails (move-tails new-head tails))
    (values (list
             (set-add (first seen) new-tail)
             (set-add (second seen) (last tails)))
            new-head new-tail updated-tails)))

(define (do-time) (time (run)))
