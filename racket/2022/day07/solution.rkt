#lang racket

(require advent-of-code
         data/applicative
         data/monad
         megaparsack
         megaparsack/text
         threading)

(define command/p
  (do (string/p "$")
      space/p
      (or/p (do (string/p "cd")
                space/p
                [dir <- (many/p any-char/p)]
                (pure (list "cd" (list->string dir))))
            (do (string/p "ls")
                (pure "ls")))))

(define term/p
  (or/p (do [size <- integer/p]
            space/p
            [filename <- (many/p any-char/p)]
            (pure (list size (list->string filename))))
        (do (string/p "dir")
            space/p
            [dirname <- (many/p letter/p)]
            (pure (list "dir" (list->string dirname))))))

(define tree/p
  (or/p command/p
        term/p))

(define commands
  (~>
   (fetch-aoc-input (find-session) 2022 7 #:cache #t)
   (string-split "\n")))

(define (handle-build-structure commands)
  (define (update-size h path size)
    (match path
      ['() h]
      [(list* _ fs) (update-size (hash-update h path (λ~> (+ size)) 0) fs size)]))

  (for/fold ([folders (hash)]
             [current-path '()]
             [previously-seen? #f]
             #:result folders)
            ([command commands])
    
    (match (parse-result! (parse-string tree/p command))
      ["ls" (values folders current-path (hash-has-key? folders current-path))]
      [(list "cd" "/") (values folders '("/") #f)]
      [(list "cd" "..") (values folders (rest current-path) #f)]
      [(list "cd" dirname) (values folders (cons dirname current-path) #f)]
      [(list "dir" dirname) (values folders current-path previously-seen?)]
      [(list filesize filename)
       (cond
         [previously-seen? (values folders current-path previously-seen?)]
         [else (values (update-size folders current-path filesize) current-path previously-seen?)])])))

(define (solution-a fs)
  (for/sum ([(_ size) (in-hash fs)] #:when (< size 100001)) size))

(define (solution-b fs)
  (let ([size-to-clear (- 30000000 (- 70000000 (hash-ref fs '("/"))))])
    (for/fold ([dir 70000000])
              ([(_ size) (in-hash fs)]
               #:when (< size-to-clear size))
      (min dir size))))

(define (run-parts)
  (define filesystem (handle-build-structure commands))
  (cons (solution-a filesystem) (solution-b filesystem)))

(define (do-time)
  (time (run-parts)))
