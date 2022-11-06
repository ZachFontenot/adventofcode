#lang racket

(require advent-of-code
         racket/hash
         sugar
         threading)

(define test "3,4,3,1,2")
(define input (map string->number (string-split (string-trim test) ",")))

(define fishes
  (~> (open-aoc-input (find-session) 2021 6 #:cache #t)
      port->string
      string-trim
      (string-split ",")
      (map string->number _)))

(define (one-day fishes)
  (map check-timer fishes))

(define (check-timer fish)
  (if (eq? 0 fish)
      (list 6 8)
      (sub1 fish)))

;; too slow for part 2
(define (part-one fishes days)
  (let run ([fs fishes]
            [ds days])
    (if (eq? ds 0)
        (length fs)
        (run (flatten (one-day fs)) (sub1 ds)))))

;; New approach is to use a hash-table for the number of each fish
;; at each timer position. The next day is made by shifting the freq down
;; to the lower timer and at 0 moving to 8 and adding 0 + 7 for 6
; thanks to jimpjorps on Racket Discord

(define (next-day fish-hash)
  (for/hash ([(timer pop) (in-hash fish-hash)])
    (values (sub1 timer) pop)))

(define (get-fish-pop days fishes)
  (for/fold ([day-state (frequency-hash fishes)]
             #:result (~> day-state ; sum all vals
                          hash-values
                          (apply + _)))
            ([day (inclusive-range 1 days)]) ; for each day
    ; Just decrement each key
    (define day-older (next-day day-state))
    ; deal with birthing
    (define new-fish
      (hash-ref day-older -1 0))
    (hash-union (hash-remove day-older -1)
                (hash 8 new-fish 6 new-fish)
                #:combine +))); combine 6 with new fish
                
(define p1 (get-fish-pop 80 fishes))
(define p2 (get-fish-pop 256 fishes))
