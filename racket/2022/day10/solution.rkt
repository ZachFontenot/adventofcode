#lang racket

(require advent-of-code
         threading)

(define (get-local-test) (file->string "test.in"))

(define (second-number lst)
  (match lst
    [(list a b) (string->number b)]
    [_ 'noop]))

(define cpu-instructions
  (~>>
   ;(get-local-test)
   (fetch-aoc-input (find-session) 2022 10 #:cache #t)
   (string-split _ "\n")
   (map string-split)
   (map second-number)))

(define (run)
  (define cycles (set 20 60 100 140 180 220))
  (define off-cycles (set-map cycles sub1))

  (for/fold ([sum 0]
             [cycle-count 1]
             [x-register 1]
             [message (list)]
             #:result (values sum (list->string (reverse message))))
            ([cmd cpu-instructions])

    (match cmd
      ['noop #:when (set-member? cycles cycle-count)
             (values (+ sum (* x-register cycle-count))
                     (add1 cycle-count) x-register
                     (draw-pixel 1 cycle-count x-register message))]
      ['noop (values sum (add1 cycle-count) x-register
                     (draw-pixel 1 cycle-count x-register message))]
      
      [num #:when (set-member? cycles cycle-count)
           (values (+ sum (* x-register cycle-count))
                   (+ cycle-count 2) (+ x-register num)
                    (draw-pixel 2 cycle-count x-register message))]
      [num #:when (set-member? off-cycles cycle-count)
           (values (+ sum (* x-register (add1 cycle-count)))
                   (+ cycle-count 2) (+ x-register num)
                    (draw-pixel 2 cycle-count x-register message))]
      [num (values sum (+ cycle-count 2) (+ x-register num)
                    (draw-pixel 2 cycle-count x-register message))])))

;; N is for number of cycles
(define (draw-pixel n c x-reg msg)
  (if (= n 1)
      (do-draw c x-reg msg)
      (let ([first-draw (do-draw c x-reg msg)])
        (do-draw (add1 c) x-reg first-draw))))
        
(define (do-draw c x-reg msg)
  (define c-mod (sub1 (modulo c 40)))
  (cond
    [(= c-mod -1)
     (case (abs (- x-reg 39))
       [(0 1) (append '(#\# #\newline) msg)]
       [else (append '(#\. #\newline) msg)])]
    [else
     (case (abs (- x-reg c-mod))
       [(0 1) (cons #\# msg)]
       [else (cons #\. msg)])]))

(define-values (a b) (run))
;;; X is the middle of the sprite
;;; so X = (sub1 X add1)
;;; so if cycle = any X, draw that X otherwise .
