#lang racket

(require advent-of-code
         threading)

(define (make-pt x y)
  (hash 'x x 'y y))

(struct fold (dir loc) #:transparent)
(define (make-fold dir loc)
  (fold (string->symbol dir)
        (string->number loc)))

(define plots
  (~> (open-aoc-input (find-session) 2021 13 #:cache #t)
      port->lines))
(define-values (pts-lst fds-lst) (splitf-at plots (λ~> (equal? "") not)))

(define pts
  (for/set ([p (in-list pts-lst)])
    (~> p
        (string-split ",")
        (map string->number _)
        (apply make-pt _))))

(define folds ;; empty string is first element
  (for/list ([f (in-list (rest fds-lst))])
    (~>> f
         (regexp-match #px"fold along (.)=(.*)")
         rest
         (apply make-fold))))

(define (do-fold points f)
  (let ([dir (fold-dir f)]
        [loc (fold-loc f)])
    (for/set ([p (in-set points)])
      (cond
        [(> (hash-ref p dir) loc)
         (hash-update p dir (λ (v) (- (* 2 loc) v)))]
        [else p]))))

(define (run-folds points folds)
  (for/fold ([points points])
            ([f (in-list folds)])
    (do-fold points f)))

(define (find-set-max points-after-folds)
  (for/fold ([x-max 0]
             [y-max 0])
            ([pts (in-set points-after-folds)])
    (let ([x (hash-ref pts 'x)]
          [y (hash-ref pts 'y)])
      (cond
        [(and (<= x-max x) (<= y-max y)) (values x y)]
        [(<= x-max x) (values x y-max)]
        [(<= y-max y) (values x-max y)]
        [else (values x-max y-max)]))))

; max y is 5, max x is 38
(define (print-code points-after-folds x y)
  (for/list ([y (in-range (add1 y))])
    (~>> (for/list ([x (in-range (add1 x))])
           (if (set-member? points-after-folds (hash 'x x 'y y))
               #\λ
               #\space))
         (apply string)
         println)))

(define pts-after-folds
  (run-folds pts folds))
(define-values (x-shape y-shape) (find-set-max pts-after-folds))

(print-code pts-after-folds x-shape y-shape)
#|
"λλλ  λλλλ λλλλ   λλ λ  λ λλλ  λλλλ λλλλ"
"λ  λ    λ λ       λ λ  λ λ  λ λ       λ"
"λ  λ   λ  λλλ     λ λλλλ λ  λ λλλ    λ "
"λλλ   λ   λ       λ λ  λ λλλ  λ     λ  "
"λ    λ    λ    λ  λ λ  λ λ λ  λ    λ   "
"λ    λλλλ λ     λλ  λ  λ λ  λ λ    λλλλ"
|#
