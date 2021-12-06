#lang racket

(require algorithms
         data/either)

(define input (file->lines "input.txt"))

(define test 
  '("7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
    ""
    "22 13 17 11  0"
    "8  2 23  4 24"
    "21  9 14 16  7"
    "6 10  3 18  5"
    "1 12 20 15 19"
    ""
    "3 15  0  2 22"
    "9 18 13 17  5"
    "19  8  7 25 23"
    "20 11 10 24  4"
    "14 21 16 12  6"
    ""
    "14 21 17 24  4"
    "10 16 15  9 19"
    "18  8 23 26 20"
    "22 11 13  6  5"
    "2  0 12  3  7"))

(define (read-input input)
  (let* ([filtered (filter (λ (s) (not (string=? "" s))) input)]
         [bingo-draws (read-draws (first filtered))]
         [bingo-cards (read-cards (rest filtered))])
    (list bingo-draws bingo-cards)))

(define (read-draws input)
  (map string->number (string-split input ",")))

(define (read-cards filtered-input)
  (chunks-of
   (map (λ (l) (map string->number l))
        (map string-split filtered-input))
   5))

(define (check-card card)
  (any? (list (check-side card)
              (check-side (apply map list card)))))

;; Check rows for marks
;; Call on transposed card to check columns
(define (check-side rows-or-cols) ; Triangle of Doom for nested lists
  (any?
   (map
    (λ (e)
      (all? (map (λ (n) (= -1 n)) e)))
    rows-or-cols)))

(define (mark-card card draw)
  (for/list ([row card])
    (map (λ (num) (if (= num draw) -1 num)) row)))

(define (run-check draw card)
  (let ([marked (mark-card card draw)])
    (cond
      [(check-card marked) (get-sum draw marked)]
      [else marked])))

(define (get-sum draw card)
  (* draw
     (for/sum ([row card])
       (sum (map (λ (e) (if (= -1 e) 0 e)) row)))))

(define (try-run draw cards)
  (let run ([draw draw]
            [cards cards]
            [checked '()])
    (if (null? cards)
        checked
        (let ([chck (run-check draw (car cards))])
          (match chck
            [(list card ...) (run draw (cdr cards) (cons chck checked))]
            [ans ans])))))

(define (bingo cards draws)
  (if (null? draws)
      (failure "didn't find win")
      (let ([turn (try-run (car draws) cards)])
        (if (list? turn)
            (bingo turn (cdr draws))
            (success turn))))) ;; unecessary Either

(define part-one
  (let ([in (read-input input)])
    (bingo (cadr in) (car in))))
