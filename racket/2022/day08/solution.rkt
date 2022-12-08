#lang racket

(require advent-of-code
         threading)

(define tester
  "30373\n25512\n65332\n33549\n35390")

(define trees
  (~>>
   (fetch-aoc-input (find-session) 2022 8 #:cache #t)
   (string-split _ "\n")
   (map (λ~> (string-split "")))
   (map (λ~>> (filter (λ (str) (not (equal? str ""))))))
   (map (λ~>> (map (λ~> string->number))))))

(define tree-cols
  (apply map list trees))

(define (get-val x y)
  (~> trees
      (list-ref x)
      (list-ref y)))

(define (edge? x y e)
  (or (= 0 x) (= 0 y)
        (= e x) (= e y)))

(define (get-tree-lines x y)
  (let ([row (list-ref trees x)]
        [col (list-ref tree-cols y)])

    (list (reverse (take row y)) ; Left
          (drop row (add1 y))   ; Right
          (reverse (take col x)) ; Up
          (drop col (add1 x))))) ; Down

(define (is-visible? val x y max-idx tree-lines)
  (or (edge? x y max-idx)
      (for/or ([tree-line tree-lines])
        (> val (apply max tree-line)))))

(define (accum-til-increase-or-equals val trees acc)
  (cond
    [(null? trees) acc]
    [(< val (first trees)) (add1 acc)]
    [(= (first trees) val) (add1 acc)]
    [else (accum-til-increase-or-equals val (rest trees) (add1 acc))]))

(define (tree-score val tree-lines)
  (for/product ([tree-line tree-lines])
    (accum-til-increase-or-equals val tree-line 0)))

(define (look-at-trees trees-data)
  (define whole-length (length trees-data))

  (for*/fold ([visible-trees 0]
              [max-viewing 0])
             ([x-idx whole-length]
              [y-idx whole-length])

    (let ([val (get-val x-idx y-idx)] [tree-lines (get-tree-lines x-idx y-idx)])

      (match (list (is-visible? val x-idx y-idx (- whole-length 1) tree-lines)
                   (tree-score val tree-lines))
        [(list #t score) (values (add1 visible-trees) (max max-viewing score))]
        [(list #f score) (values visible-trees (max max-viewing score))]))))

(define-values (solution-a solution-b)
  (look-at-trees trees))
