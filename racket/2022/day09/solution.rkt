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


(define (move-head h dir)
  (match dir
    ["R" (add-pt h (pt 1 0))]
    ["U" (add-pt h (pt 0 1))]
    ["L" (add-pt h (pt -1 0))]
    ["D" (add-pt h (pt 0 -1))]))

(define (to-update-tail h t)
  (match (pt-diff h t)
    [(pt 0 -2) (pt 0 -1)]
    [(pt 0 2) (pt 0 1)]
    [(pt -2 0) (pt -1 0)]
    [(pt 2 0) (pt 1 0)]
    [(pt a b) #:when (and (< 2 (abs-diff h t))
                          (< a 0) (< b 0)) (pt -1 -1)]
    [(pt a b) #:when (and (< 2 (abs-diff h t))
                          (< a 0)) (pt -1 1)]
    [(pt a b) #:when (and (< 2 (abs-diff h t))
                          (< b 0)) (pt 1 -1)]
    [(pt a b) #:when (< 2 (abs-diff h t)) (pt 1 1)]
    [_ (pt 0 0)]))

(define (move-tail h t)
  (add-pt t (to-update-tail h t)))

(define (move-tails h ropes)
  (for/fold ([new-ropes (list)]
             [head h]
             #:result (reverse new-ropes))
            ([rope ropes])
    (let ([new-tail (move-tail head rope)])
      (values (cons new-tail new-ropes) new-tail))))

(define (run-a)
  (for/fold ([seen (set (pt 0 0))]
             [h (pt 0 0)]
             [t (pt 0 0)]
             #:result (set-count seen))
            ([move movements]
             #:do
             [(match-define (list dir amt) move)] [_ (in-range amt)])
    (define updated-head (move-head h dir))
    (define updated-tail (move-tail updated-head t))

    (values (set-add seen updated-tail) updated-head updated-tail)))

(define (run-b)
  (for/fold ([seen (set (pt 0 0))]
              [h (pt 0 0)]
              [tails (make-list 9 (pt 0 0))]
              #:result (set-count seen))
             ([move movements]
              #:do
              [(match-define (list dir amt) move)] [_ (in-range amt)])
    (define updated-head (move-head h dir))
    (define updated-tails (move-tails updated-head tails))
    (values (set-add seen (last tails)) updated-head updated-tails)))

(time (run-a))
(time (run-b))
