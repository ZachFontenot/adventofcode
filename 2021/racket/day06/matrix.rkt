;; Thanks to Ben Knoble from the Racket discord for adapting this from Julia

#lang typed/racket

(require math/matrix)

(define input : String
   (file->string "input.txt"))

(define listput : (Listof String)
  (string-split (string-trim input) ","))

;; Typed Racket, more like can't infer types on map
(define fishput : (Listof Number)
  (map (λ (n) (cast n Number)) (map string->number listput))) ;; WHAT THE FUCK

;; (define fishput : (Listof Number)
;;   (read-fish listput))

(define step : (Matrix Number)
  (matrix ([0 1 0 0 0 0 0 0 0]
           [0 0 1 0 0 0 0 0 0]
           [0 0 0 1 0 0 0 0 0]
           [0 0 0 0 1 0 0 0 0]
           [0 0 0 0 0 1 0 0 0]
           [0 0 0 0 0 0 1 0 0]
           [1 0 0 0 0 0 0 1 0]
           [0 0 0 0 0 0 0 0 1]
           [1 0 0 0 0 0 0 0 0])))


(define (make-fishes [fishes : (Listof Number)]) : (Matrix Number)
  (define num-each-fish
    (for/hash : (HashTable Number Number)
      ([group (in-list (group-by (λ (x) x) fishes))])
      (values (first group) (length group))))
  (build-matrix 9 1 (λ ([y : Index] [_ : Index])
                      (hash-ref num-each-fish y (const 0)))))

(define (simulate [fishes : (Matrix Number)] [days : Integer]) : (Matrix Number)
  (matrix* (matrix-expt step days)
           fishes))

(define (solution [fish : (Listof Number)] [days : Integer])
  (matrix-1norm (simulate (make-fishes fish) days)))

(solution fishput 80)
