#lang racket

(require test-engine/racket-tests)
(require 2htdp/batch-io)

; Posn

; A single two dimensional point
(define-struct posn (x y) #:transparent)

; posn posn -> posn
; Adds two posns together component wise
(define (posn-add p1 p2)
  (posn (+ (posn-x p1) (posn-x p2))
        (+ (posn-y p1) (posn-y p2))))

; Christmas

; A single christmas delievery for santa
(define-struct christmas (santa robosanta visitedhouses) #:transparent)

; void -> christmas
; Initializes a new christmas
(define (christmas-new)
  (christmas (posn 0 0) (posn 0 0) (set (posn 0 0))))

; Helper

; christmas directions -> christmas
; Moves santa to deliver presents on christmas
(check-expect (deliver-presents (christmas-new) ">") 2)
(check-expect (deliver-presents (christmas-new) "^>v<") 4)
(check-expect (deliver-presents (christmas-new) "^v^v^v^v^v") 2)

(define (deliver-presents x-mas directions)
  (cond [(= (string-length directions) 0) (set-count (christmas-visitedhouses x-mas))]
        [else (define new-posn (move-direction (christmas-santa x-mas) (string-ref directions 0)))
              (deliver-presents (struct-copy christmas x-mas [santa new-posn] [visitedhouses (set-add (christmas-visitedhouses x-mas) new-posn)])
                                (substring directions 1))]))

; christmas directions -> christmas
; Moves santa and robo santa to deliver presents on christmas
(check-expect (deliver-presents-withrobo (christmas-new) "^v") 3)
(check-expect (deliver-presents-withrobo (christmas-new) "^>v<") 3)
(check-expect (deliver-presents-withrobo (christmas-new) "^v^v^v^v^v") 11)

(define (deliver-presents-withrobo x-mas directions [move-robo #f])
  (cond [(= (string-length directions) 0) (set-count (christmas-visitedhouses x-mas))]
        [else (cond [move-robo (define new-posn (move-direction (christmas-robosanta x-mas) (string-ref directions 0)))
                               (deliver-presents-withrobo (struct-copy christmas
                                                                       x-mas
                                                                       [robosanta new-posn]
                                                                       [visitedhouses (set-add (christmas-visitedhouses x-mas) new-posn)])
                                                          (substring directions 1)
                                                          #f)]
                    [else (define new-posn (move-direction (christmas-santa x-mas) (string-ref directions 0)))
                          (deliver-presents-withrobo (struct-copy christmas
                                                                  x-mas
                                                                  [santa new-posn]
                                                                  [visitedhouses (set-add (christmas-visitedhouses x-mas) new-posn)])
                                                     (substring directions 1)
                                                     #t)])]))

; posn char -> posn
; Moves the posn based on the direction characters '<', '>', '^', 'v'.
(check-expect (move-direction (posn 0 0) #\<) (posn -1 0))
(check-expect (move-direction (posn 0 0) #\>) (posn 1 0))
(check-expect (move-direction (posn 0 0) #\^) (posn 0 1))
(check-expect (move-direction (posn 0 0) #\v) (posn 0 -1))

(define (move-direction p direction)
  (define movement-posn (cond [(char=? direction #\<) (posn -1 0)]
                              [(char=? direction #\>) (posn 1 0)]
                              [(char=? direction #\^) (posn 0 1)]
                              [(char=? direction #\v) (posn 0 -1)]))
  (posn-add p movement-posn))

; Read input
(define input (read-file "day3.dat"))

; Part 1
(deliver-presents (christmas-new) input)

; Part 2
(deliver-presents-withrobo (christmas-new) input)

(test)
