#lang racket

(require threading)

(define input (file->lines "input.txt"))
(define greater (/ (length input) 2))

(define test '("00100"
               "11110"
               "10110"
               "10111"
               "10101"
               "01111"
               "00111"
               "11100"
               "10000"
               "11001"
               "00010"
               "01010"))

;; (define (count-bits lines which-bit)
;;   (for*/fold ([ones 0])
;;              ([line lines]))
;;   (if (char=? #\1 (string->list (list-ref line which-bit)))))
;; naughty imperative thinking

(define (transpose-bits input)
  (let ([listified (map string->list input)])
    (apply map list listified)))

(define (count-ones input)
  (foldl (λ (x acc) (if (char=? #\1 x) (add1 acc) acc)) 0 input))

(define (binary->decimal n)
  (if (zero? n)
      n
      (+ (modulo n 10) (* 2 (binary->decimal (quotient n 10))))))

(define (construct-gamma input check)
  (for/list ([line input])
    (if (< check (count-ones line))
        #\1
        #\0)))

(define (flip-bit-char gamma)
  (map (λ (x) (if (char=? #\1 x) #\0 #\1)) gamma))

(define (convert list-char)
  (binary->decimal (string->number (list->string list-char))))

(define (solve-one gamma)
  (let* ([epsilon (convert (flip-bit-char gamma))]
         [gamma-dec (convert gamma)])
    (* gamma-dec epsilon)))

(define part-one
  (~> input transpose-bits
      (construct-gamma greater)
      (solve-one)))


(define (filter-bits lines char pos)
  (filter (λ (x) (char=? char (list-ref x pos))) lines))

(define (grab-char val pivot char)
  (cond
    [(and (char=? char #\1)(<= pivot val)) #\1]
    [(and (char=? char #\1)(> pivot val)) #\0]
    [(<= pivot val) #\0]
    [(> pivot val) #\1]))

(define (find-rating input oxygen-rating?)
  (define (loop input pos)
    (if (= 1 (length input))
        (car input)
        (let* ([most-common (count-ones (list-ref (apply map list input) pos))]
               [char (grab-char most-common (- (length input) most-common ) oxygen-rating?)]
               [next (filter-bits input char pos)])
          (loop next (add1 pos)))))
  (loop input 0))

(define (solve-two input)
  (let* ([chars (map string->list input)]
         [oxygen (find-rating chars #\1)]
         [co2 (find-rating chars #\0)])
    (* (convert oxygen) (convert co2))))
