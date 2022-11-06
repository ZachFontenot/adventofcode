#lang racket

(require advent-of-code
         threading)

(define syntax-errors
  (~> (open-aoc-input (find-session) 2021 10 #:cache #t)
      port->lines
      (map string->list _)))

(define test-errors
  (~> '("[({(<(())[]>[[{[]{<()<>>"
        "[(()[<>])]({[<{<<[]>>("
        "{([(<{}[<>[]}>{[]{[(<()>"
        "(((({<>}<{<{<>}{[]{[]{}"
        "[[<[([]))<([[{}[[()]]]"
        "[{[{({}]{}}([{[{{{}}([]"
        "{<[[]]>}<{[{[{[]{()[[[]"
        "[<(<(<(<{}))><([]([]()"
        "<{([([[(<>()){}]>(<<{{"
        "<{([{{}}[<[[[<>{}]]]>[]]")
      (map string->list _)))

;; two lists, list of chars to match, list of current openings.
;; when we pair an opening and closing bracket, we drop it from
;; the stack like structure

(define open-brackets '(#\( #\[ #\{ #\<))

(define (open-bracket? char)
  (for/or ([br open-brackets])
    (char=? char br)))

(define close-bracket '(#\) #\] #\} #\>)) ; not sure this is useful

; One thing that doesn't matter right now is unmatched pairs, so if
; at the end the 'stack' has elements still inside, then it's ok
; corruption is only mismatched

(define (match-em line)
  (let run ([line line]
            [brackets '()])
    (if (null? line)
        brackets
        (let ([char (first line)]
              [br (unless (null? brackets) (first brackets))])
          (if (open-bracket? char)
              (run (cdr line) (cons char brackets))
              (match (cons char br) 
                [(cons #\) #\() (run (cdr line) (cdr brackets))]
                [(cons #\] #\[) (run (cdr line) (cdr brackets))]
                [(cons #\} #\{) (run (cdr line) (cdr brackets))]
                [(cons #\> #\<) (run (cdr line) (cdr brackets))]
                [_ char]))))))

(define (solve-part-one input)
  (for/sum ([line input]
            #:when (char? (match-em line)))
    (match (match-em line)
      [#\) 3]
      [#\] 57]
      [#\} 1197]
      [#\> 25137])))

(define (get-score closing-brackets)
  (for/fold ([return 0])
            ([br closing-brackets])
    (+ (* 5 return)
       (match br
         [#\) 1]
         [#\] 2]
         [#\} 3]
         [#\> 4]))))

(define (get-sequence-brackets opening-brackets)
  (for/list ([br opening-brackets])
    (match br
      [#\( #\)]
      [#\[ #\]]
      [#\{ #\}]
      [#\< #\>])))

(define (get-scores input)
  (for/list ([line input]
             #:when (list? (match-em line)))
    (get-score (get-sequence-brackets (match-em line)))))

(define (solve-part-two input)
  (let ([scores (get-scores input)])
    (~> scores
        (sort <)
        (list-ref (quotient (length scores) 2)))))
