#lang racket

(require advent-of-code
         racket/hash
         threading)

;; Making lists of the chars
(define (split-strings lines)
  (for/list ([line lines])
    (map string-split (string-split line " | "))))

(define (make-sets lines)
  (for/list ([line lines])
    (map (λ (l) (map string->list l)) line)))

(define seven-segments
  (~> (open-aoc-input (find-session) 2021 8 #:cache #t)
      port->lines
      split-strings
      make-sets))

(define test
  (~> "test.txt"
      file->lines
      split-strings
      make-sets))

(define (match-unique? output)
  (let ([l (length output)])
    (or (equal? l 2)
        (equal? l 3)
        (equal? l 4)
        (equal? l 7))))

(define (count-uniques segments)
  (for/fold ([unique-counts 0])
            ([signal segments])
    (let ([outputs (second signal)])
      (apply + unique-counts (map (λ (cs) (if (match-unique? cs) 1 0)) outputs)))))

;; Now I actually want the letters
;; For each line, I want to map the set of letters to their corresponding
;; segments.

(define (uniques-hash inputs)
  (for/hash ([chars inputs])
    (let ([l (length chars)])
      (cond
        [(= l 2) (values 1 chars)]
        [(= l 3) (values 7 chars)]
        [(= l 4) (values 4 chars)]
        [(= l 7) (values 8 chars)]
        [else (values -1 '())]))))

(define (match-zero? input one four)
  (and (= (length input) 6)
       (subset? one input)
       (not (subset? four input))))

(define (match-six? input one)
  (and (= (length input) 6)
       (not (subset? one input))))

(define (match-nine? input four)
  (and (= (length input) 6)
       (subset? four input)))

(define (match-three? input seven four)
  (and (= (length input) 5)
       (subset? seven input)
       (not (subset? four input))))

(define (match-five? input six)
  (and (equal? 1 (length (set-symmetric-difference input six)))
       (= 5 (length input))))

(define (match-two? input six nine) ; 0 on eighth but 3 and 2 are the same chars?
  (and (equal? 3 (length (set-symmetric-difference input six)))
       (equal? 3 (length (set-symmetric-difference input nine)))
       (= 5 (length input))))
      
;; 0 = length of 6 and contains 1 or 7
(define (second-pass-hash inputs one four seven)
  (for/hash ([chars inputs])
    (cond
      [(match-zero? chars one four) (values 0 chars)]
      [(match-six? chars one) (values 6 chars)]
      [(match-nine? chars four) (values 9 chars)]
      [(match-three? chars seven four) (values 3 chars)]
      [else (values -1 '())])))

(define (final-pass-hash inputs six nine)
  (for/hash ([chars inputs])
    (cond
      [(match-two? chars six nine) (values 2 chars)]
      [(match-five? chars six) (values 5 chars)]
      [else (values -1 '())])))

(define (build-hash inputs)
  (let* ([uniques (uniques-hash inputs)]
         [one (hash-ref uniques 1)]
         [four (hash-ref uniques 4)]
         [seven (hash-ref uniques 7)]
         [second-pass (hash-union (hash-remove uniques -1)
                                  (second-pass-hash inputs one four seven))]
         [six (hash-ref second-pass 6)]
         [nine (hash-ref second-pass 9)])
    (hash-union (hash-remove second-pass -1) (final-pass-hash inputs six nine))))

(define (match-digits h d) ;; HACKY BULLSHIT
  (for/fold ([return-val 0])
            ([(k v) (in-hash h)])
    (if (set=? d v)
        (+ k return-val)
        (+ 0 return-val))))

(define (build-num xs)
  (for/fold ([ret-num 0])
            ([num xs]
             [mul '(1000 100 10 1)])
    (+ (* num mul) ret-num)))

(define (solve-part-two readings)
  (for/fold ([sum-of-displays 0])
            ([line readings])
    (let ([digit-hash (build-hash (car line))])
      (+ sum-of-displays (build-num (map (λ (cs) (match-digits digit-hash cs)) (second line)))))))
