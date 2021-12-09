#lang racket

(require advent-of-code
         racket/hash
         threading)

(struct display-digits (input output) #:transparent)

(define (split-strings lines)
  (for/list ([line lines])
    (map string-split (string-split line " | "))))

(define (make-sets lines)
  (for/list ([line lines])
    (map (λ (l) (map string->list l)) line)))

;; Making lists of the chars
(define (parse-digits lines)
  (for/list ([line lines])
    (match-define `(,input ,output) (string-split line "|"))
    (display-digits
     (map (compose (λ (s) (apply set s)) string->list) (string-split input))
     (map (compose (λ (s) (apply set s)) string->list) (string-split output)))))

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

(define (count-uniques segments)
  (for*/sum ([signal segments] ; for/fold to sum is just for/sum
             [out (in-list (display-digits-output signal))]
             #:when (or (= (set-count out) 2)
                        (= (set-count out) 3)
                        (= (set-count out) 4)
                        (= (set-count out) 7)))
    1))

;; Now I actually want the letters
;; For each line, I want to map the set of letters to their corresponding
;; segments. This is misguided because I'm doing work that I don't need to be doing
;; and the properties of the digits is more obvious to me NOW THAT I BLUNDERED THROUGH IT
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

;; Thanks Hazel for this much better solution
(define ((digit-pattern inputs) seg)
  (define (with-length len)
    (for/list ([pattern inputs]
               #:when (= (set-count pattern) len))
      pattern))

  ; set rules
  (define one (first (with-length 2)))
  (define four (first (with-length 4)))
  (define seven (first (with-length 3)))
  (define eight (first (with-length 7)))
    
  (define adg (apply set-intersect (with-length 5)))
  
  (define three (set-union adg one))

  (define nine (set-union adg four))

  (define agf (apply set-intersect (with-length 6)))

  (define f (set-subtract agf adg))

  (define e (set-subtract eight (set-union agf adg four)))

  (define two (set-union (set-subtract three f) e))

  (define c (set-subtract (set-intersect four seven) f))

  (define six (set-subtract eight c))

  (define five (set-subtract eight c e))

  (define zero (set-union (set-subtract eight adg) agf))

  (cond [(set=? seg zero) #\0]
        [(set=? seg one) #\1]
        [(set=? seg two) #\2]
        [(set=? seg three) #\3]
        [(set=? seg four) #\4]
        [(set=? seg five) #\5]
        [(set=? seg six) #\6]
        [(set=? seg seven) #\7]
        [(set=? seg eight) #\8]
        [(set=? seg nine) #\9]))

;; Not Faster but like 3 ms
(define segments
  (~> (open-aoc-input (find-session) 2021 8 #:cache #t)
      port->lines
      parse-digits))

(define (part-deux segs)
  (for/sum ([seg segs])
    (match-define (display-digits pats outs) seg)
    (string->number (apply string (map (digit-pattern pats) outs)))))
