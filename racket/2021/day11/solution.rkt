#lang racket

(require advent-of-code
         threading)

(define (build-octo-grid input)
  (for*/hash ([(row y) (in-indexed (in-list input))]
              [(col x) (in-indexed (in-list row))])
    (values (cons x y) (cons col #f))))

(define octo-grid ; Listof Listof Numbers
  (~>> (open-aoc-input (find-session) 2021 11 #:cache #t)
      port->lines
      (map string->list)
      (map (λ (s)
             (map (λ~>> ~a string->number) s)))
      build-octo-grid))

(define test-grid
  '("5483143223"
    "2745854711"
    "5264556173"
    "6141336146"
    "6357385478"
    "4167524645"
    "2176841721"
    "6882881134"
    "4846848554"
    "5283751526"))

(define test-octos
  (~>> test-grid
       (map string->list)
       (map (λ (s)
              (map (λ~>> ~a string->number) s)))
       build-octo-grid))

(define (increase-energy octos coords)
  (for/fold ([octos octos])
            ([coord (in-list coords)]
             #:when (hash-has-key? octos coord))
    (hash-update octos coord (λ (v) (cons (add1 (car v)) (cdr v))))))

(define (increase-energies octos)
  (let ([oct-coords (hash-keys octos)])
    (increase-energy octos oct-coords)))

(define (flash octos)
  (for*/fold ([octos octos])
             ([(coord oct) (in-hash octos)]
              #:when (and (< 9 (car oct)) (false? (cdr oct)))
              [x (in-list '(-1 0 1))]
              [y (in-list '(-1 0 1))])
    (if (= 0 x y)
        (hash-update octos coord (λ (v) (cons (car v) #t)))
        (increase-energy octos (list (cons (+ x (car coord)) (+ y (cdr coord))))))))


(define (flash-round octos)
  (let ([one-octos (flash octos)])
    (if (equal? octos one-octos)
        octos
        (flash-round one-octos))))

(define (reset-flashed octos)
  (for/fold ([octos octos]
             [flashes 0])
            ([(coord oct) (in-hash octos)]
             #:when (cdr oct))
    (values (hash-set octos coord (cons 0 #f))
            (add1 flashes))))

(define (single-round octos [flashes 0])
  (~> octos
   increase-energies
   flash-round
   reset-flashed))

(define (part-one octos steps)
  (for/fold ([octos octos]
             [flashes 0]
             #:result flashes)
            ([step (in-range steps)])
    (define-values [next new-flashes]
      (single-round octos))
    (values next (+ new-flashes flashes))))

(define (part-two octos)
  (let/ec ret
    (for/fold ([octos octos])
              ([step (in-naturals 1)])
      (define-values [next-state new-flashes]
        (single-round octos))
      (if (= new-flashes 100)
          (ret step)
          next-state))))

