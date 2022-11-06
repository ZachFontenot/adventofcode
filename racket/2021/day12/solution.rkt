#lang racket

(require advent-of-code
         sugar
         threading)

(define (cave-hash caves-list)
  (for/fold ([cave-hash (hash)])
            ([cave-pair caves-list])
    (match-let ([(list start end) cave-pair])
      (let ([inter
             (if (hash-has-key? cave-hash start)
                 (hash-update cave-hash start (curry cons end))
                 (hash-set cave-hash start (cons end '())))])
        (if (hash-has-key? inter end)
            (hash-update inter end (curry cons start))
            (hash-set inter end (cons start '())))))))

(define cave-graph
  (~>> (open-aoc-input (find-session) 2021 12 #:cache #t)
      port->lines
      (map (λ (s) (string-split s "-")))
      cave-hash))

(define (backtrack? next visited)
  (and (equal? (string-downcase next) next)
       (member next visited)))

(define (traverse-caves
         caves [path '("start")]
         #:one-visit? [visited? #t])
  (define current (car path))
  (cond
    [(equal? current "end") (list path)]
    [else
     (~>> (for/list ([next-path (in-list (hash-ref caves current))]
                     #:when (and (not (equal? next-path "start"))
                                 (not (and (backtrack? next-path path)
                                           visited?))))
            (traverse-caves
             caves
             (cons next-path path)
             #:one-visit? (or (backtrack? next-path path)
                              visited?)))
          (apply append))]))

(define (part-one caves)
  (length (traverse-caves caves)))

(define (part-two caves)
  (length (traverse-caves caves #:one-visit? #f)))
