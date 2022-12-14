#lang racket

(require advent-of-code
         data/queue
         threading)

(define tester
  "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(define heightmap
  (~>>
   ;tester
   (fetch-aoc-input (find-session) 2022 12 #:cache #t)
   (string-split _ "\n")
   (map (compose (λ~>> (map char->integer)) string->list))
   (map list->vector)
   (list->vector)))

(define-values (start end)
  (values (char->integer #\S) (char->integer #\E)))

(define-values (a-ele z-ele)
  (values (char->integer #\a) (char->integer #\z)))

(define (find-char-at-index matrix char)
  (for*/fold ([idx (list)])
             ([i (vector-length matrix)]
              [j (vector-length (vector-ref matrix i))]
              #:final (= char (vector-ref (vector-ref matrix i) j)))
    (cons i j)))

(define (find-all-chars-index matrix char)
  (for*/fold ([idx (list)])
             ([i (vector-length matrix)]
              [j (vector-length (vector-ref matrix i))])
    (if (= (char->integer #\a) (vector-ref (vector-ref matrix i) j))
        (cons (cons i j) idx)
        idx)))

(define-values (start-idx end-idx)
  (values (find-char-at-index heightmap start)
          (find-char-at-index heightmap end)))

(define (can-move? x y)
  (cond
    [(= x start) (<= (- y a-ele) 1)]
    [(= y end) (<= (- z-ele x) 1)]
    [else (<= (- y x) 1)]))

(define (get-position graph pair)
  (vector-ref (vector-ref graph (car pair)) (cdr pair)))

(define (add-pair p1 p2)
  (cons (+ (car p1) (car p2)) (+ (cdr p1) (cdr p2))))

(define (get-neighbors h pos vec)
  (define-values (x-bound y-bound)
    (values (sub1 (vector-length vec))
            (sub1 (vector-length (vector-ref vec 0)))))
  
  (~>>
   '((1 . 0) (0 . 1) (-1 . 0) (0 . -1))
   (map (λ (pr) (add-pair pos pr)))
   (filter (λ (pr) (and (>= (car pr) 0) (>= (cdr pr) 0)
                       (<= (car pr) x-bound) (<= (cdr pr) y-bound))))
   (filter (λ (nbr) (can-move?
                     (get-position heightmap pos)
                     (get-position heightmap nbr))))
   (filter (λ (v) (not (hash-has-key? h v))))))

(define (bfs start)
  (define q (make-queue))
  (enqueue! q start)
  (define path-hash (hash start 0))
  (for/fold ([paths path-hash])
            ([current (in-queue q)])
    (define curr-neighbors (get-neighbors paths current heightmap))
    (define curr-distance (hash-ref paths current))
    (for/fold ([new-paths paths])
              ([spc curr-neighbors])      
      (enqueue! q spc)
      (hash-set new-paths spc (add1 curr-distance)))))

(define (part-a)
  (hash-ref (bfs start-idx) end-idx))

(define (part-b)
  (define starts (find-all-chars-index heightmap #\a))
  (~>> (for/list ([start starts])
        (define cur-start (bfs start))
        (if (hash-has-key? cur-start end-idx)
            cur-start
            0))
       (filter (λ~> (hash?)))
       (map (λ (h) (hash-ref h end-idx)))
       (apply min)))

(define (do-with-times)
  (displayln (time (part-a)))
  (displayln (time (part-b))))
