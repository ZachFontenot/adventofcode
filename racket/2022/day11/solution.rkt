#lang racket

(require threading)

(struct monkey (order items op test pass-tos)
  #:transparent)

;; Monkey 0
(define zero-monkey
  (monkey 0 '(93 54 69 66 71)
          (λ~> (* 3))
          7
          '(7 . 1)))

(define one-monkey
  (monkey 1 '(89 51 80 66)
          (λ~> (* 17))
          19
          '(5 . 7)))

(define two-monkey
  (monkey 2 '(90 92 63 91 96 63 64)
          add1
          13
          '(4 . 3)))

(define three-monkey
  (monkey 3 '(65 77)
          (λ~> (+ 2))
          3
          '(4 . 6)))

(define four-monkey
  (monkey 4  '(76 68 94)
          (λ (n) (* n n))
          2
          '(0 . 6)))

(define five-monkey
  (monkey 5 '(86 65 66 97 73 83)
          (λ~> (+ 8))
          11
          '(2 . 3)))

(define six-monkey
  (monkey 6 '(78)
          (λ~> (+ 6))
          17
          '(0 . 1)))

(define seven-monkey
  (monkey 7 '(89 57 59 61 87 55 55 88)
          (λ~> (+ 7))
          5
          '(2 . 5)))

(define real-monkeys 
  (hash 0 zero-monkey
        1 one-monkey
        2 two-monkey
        3 three-monkey
        4 four-monkey
        5 five-monkey
        6 six-monkey
        7 seven-monkey))

(define ez-monkey
  (monkey 0 '(79 98)
          (λ~> (* 19))
          23
          '(2 . 3)))

(define eo-monkey
  (monkey 1 '(54 65 75 74)
          (λ~> (+ 6))
          19
          '(2 . 0)))

(define et-monkey
  (monkey 2 '(79 60 97)
          (λ (n) (* n n))
          13
          '(1 . 3)))

(define eth-monkey
  (monkey 3 '(74)
          (λ~> (+ 3))
          17
          '(0 . 1)))
  

(define example-monkeys 
  (hash 0 ez-monkey
        1 eo-monkey
        2 et-monkey
        3 eth-monkey))

(define (turn monkeys counts monkey-ref mod)
  (define monkey (hash-ref monkeys monkey-ref))

  (for/fold ([monkeys monkeys]
             [inspection-counts counts])
            ([item (monkey-items monkey)])

    (define new-counts (hash-update inspection-counts monkey-ref add1 0))
    (define worry (mod ((monkey-op monkey) item)))

    (if (zero? (remainder worry (monkey-test monkey)))
        
        (values (monkey-pass monkeys worry monkey-ref (car (monkey-pass-tos monkey)))
                new-counts)
        
        (values (monkey-pass monkeys worry monkey-ref (cdr (monkey-pass-tos monkey)))
                new-counts))))

(define (monkey-pass monkeys item from-monkey-ref to-monkey-ref)
  (define from-m (hash-ref monkeys from-monkey-ref))
  (define to-m (hash-ref monkeys to-monkey-ref))

  (~> monkeys
      (hash-set from-monkey-ref
                (struct-copy monkey from-m
                             [items (cdr (monkey-items from-m))]))
      (hash-set to-monkey-ref
                (struct-copy monkey to-m
                             [items (append (monkey-items to-m) (list item))]))))

(define (m-round monkeys counts mod)
  (for/fold ([monkeys monkeys]
            [counts counts])
            ([monkey (in-range (hash-count monkeys))])
    (turn monkeys counts monkey mod)))

(define (monkey-business inspection-counts)
  (~> inspection-counts
      (hash-values)
      (sort <)
      (reverse)
      ((λ (ls) (* (first ls) (second ls))))))

(define (gameA monkeys)
  (for/fold ([monkeys monkeys]
             [counts (hash)]
             #:result (monkey-business counts))
            ([rnd (in-range 20)])
    (m-round monkeys counts (λ~> (quotient 3)))))

(define (gameB monkeys)
  (for/fold ([monkeys monkeys]
             [counts (hash)]
             #:result (monkey-business counts))
            ([rnd (in-range 10000)])
    (m-round monkeys counts
             (λ~>
              (modulo
               (apply
                lcm
                (hash-map monkeys
                          (λ (k v) (monkey-test v)))))))))
