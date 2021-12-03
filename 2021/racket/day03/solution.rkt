#lang racket

(require threading)

(define input (file->lines "input.txt"))
(define greater (/ (length input) 2))

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

(define (solve gamma)
  (let* ([epsilon (convert (flip-bit-char gamma))]
         [gamma-dec (convert gamma)])
    (* gamma-dec epsilon)))

(~> input transpose-bits
    (construct-gamma greater)
    (solve))
