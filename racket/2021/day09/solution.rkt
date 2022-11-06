#lang racket

(require advent-of-code
         threading)

(define (make-input input)
  (for/vector ([line input]
               #:unless (equal? line ""))
    (~>> line
         string->list
         (map (λ~>> ~a string->number))
         list->vector)))

(define height-map
  (let ([in (port->lines (open-aoc-input (find-session) 2021 9 #:cache #t))])
    (make-input in)))

(define test
  '("2199943210"
    "3987894921"
    "9856789892"
    "8767896789"
    "9899965678"))

(define test-heights (make-input test))

(define (get-max-rows vec)
  (vector-length vec))
(define (get-max-cols vec)
  (vector-length (vector-ref vec 0)))
(define-values (min-r min-c) (values 0 0))
(define-values (max-r max-c) (values (get-max-rows height-map) (get-max-cols height-map)))

(define (vector2d-ref vec coord)
  (match-define `(,r ,c) coord)
  (~> vec
      (vector-ref r)
      (vector-ref c)))

(define (adj coords)
  (match-define `(,r ,c) coords)
  `((,(add1 r) ,c)
    (,(sub1 r) ,c)
    (,r ,(add1 c))
    (,r ,(sub1 c))))

(define (valid-coord coord)
  (match-define `(,r ,c) coord)
  (and (>= r min-r)
       (< r max-r)
       (>= c min-c)
       (< c max-c)))

(define (find-low vec coord)
  (for*/and ([neighbor (in-list (adj coord))]
             #:when (valid-coord neighbor))
    (< (vector2d-ref vec coord) (vector2d-ref vec neighbor))))

(define (part-one input)
  (for*/sum ([r (in-range min-r max-r)]
             [c (in-range min-c max-c)]
             [coord (in-value `(,r ,c))]
             #:when (find-low input coord))
    (add1 (vector2d-ref input coord))))

;; MUTABEL IMEPRPATIVE PROGRAMINNG
(define walked (make-hash)) ;; out for a stroll

(define (walkable? vec coord) ; Have we seen it and is it passable?
  (and (< (vector2d-ref vec coord) 9)
       (not (hash-has-key? walked coord))))

(define (graph-walk vec coord)
  (cond
    [(walkable? vec coord)
     (hash-set! walked coord 'visited)
     (add1 (for/sum [(neighbor (in-list (adj coord)))
                     #:when (valid-coord neighbor)]
             (graph-walk vec neighbor)))]
    [else 0]))

(define basins
  (for*/list ([r (in-range min-r max-r)]
              [c (in-range min-c max-c)]
              [coord (in-value `(,r ,c))]
              #:when (walkable? height-map coord))
    (graph-walk height-map coord)))

(~> basins
    (sort >)
    (take 3)
    (apply * _))
