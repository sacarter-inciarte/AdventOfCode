#lang racket

(require 2htdp/batch-io)
(require test-engine/racket-tests)

; string -> int
; Determines what floor you are on from the given direction string
(check-expect (apartment-floor "(())") 0)
(check-expect (apartment-floor "()()") 0)
(check-expect (apartment-floor "(((") 3)
(check-expect (apartment-floor "(()(()(") 3)
(check-expect (apartment-floor "))(((((") 3)
(check-expect (apartment-floor "())") -1)
(check-expect (apartment-floor "))(") -1)
(check-expect (apartment-floor ")))") -3)
(check-expect (apartment-floor ")())())") -3)

(define (apartment-floor directions)
  (for/fold ([current-floor 0])
            ([c directions])
    (cond [(char=? c #\() (add1 current-floor)]
          [(char=? c #\)) (sub1 current-floor)])))

; string->int
; Determine what position within the directions string you first enter the basement in. Position is inded + 1
(check-expect (basement-position ")") 1)
(check-expect (basement-position "()())") 5)

(define (basement-position directions [i 0] [current-floor 0])
  (cond [(> i (string-length directions)) -1]
        [(= current-floor -1) i]
        [(char=? (string-ref directions i) #\() (basement-position directions (add1 i) (add1 current-floor))]
        [(char=? (string-ref directions i) #\)) (basement-position directions (add1 i) (sub1 current-floor))]))

; Opens day1.dat
(define input (read-file "day1.dat"))
(apartment-floor input) ; Part 1
(basement-position input) ; Part 2

(test)