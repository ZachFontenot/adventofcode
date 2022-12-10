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

(define (x-values)
  (for/fold ([xs (list)]
             [x-register 1]
             #:result (list->vector (reverse xs)))
            ([cmd cpu-instructions])

    (match cmd
      ['noop (values (cons x-register xs) x-register)]
      [num (values (append (list x-register x-register) xs)
                   (+ x-register num))])))

(define (draw-pixel c x-reg)
  (case (abs (- x-reg c))
    [(0 1) #\█]
    [else #\space]))

(define (get-part1 xs)
  (for/fold ([sum 0])
            ([cycle (in-range 19 (vector-length xs) 40)])
    (+ sum (* (vector-ref xs cycle) (add1 cycle)))))

(define (get-part2 xs)
  (for/fold ([msg (list)]
             #:result (list->string (reverse msg)))
            ([c (in-range (vector-length xs))])

    (define cycle (modulo c 40))
    (if (not (zero? cycle))
        (cons (draw-pixel cycle (vector-ref xs c)) msg)
        (append (list (draw-pixel cycle (vector-ref xs c)) #\newline) msg))))

(define (run-solutions)
  (define xs (time (x-values)))
  (time (get-part1 xs))
  (define part2 (time (get-part2 xs)))
  (display part2))

; Behold
; ███   ██   ██  ████ █  █ █    █  █ ████ 
; █  █ █  █ █  █ █    █ █  █    █  █ █    
; ███  █  █ █    ███  ██   █    ████ ███  
; █  █ ████ █    █    █ █  █    █  █ █    
; █  █ █  █ █  █ █    █ █  █    █  █ █    
; ███  █  █  ██  ████ █  █ ████ █  █ █    
