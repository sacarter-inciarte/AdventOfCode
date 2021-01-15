#lang racket

(require test-engine/racket-tests)
(require 2htdp/batch-io)

; Formats the inputs into [[length width height], ...]
(define (format-input fp)
  (for/list [(line (read-lines fp))]
    (map (lambda (x) (string->number x)) (string-split line "x"))))

; int int int -> int
; Returns the number of square feet of wrapping paper to buy which
; is the surface area of the box with dimensions length, width, and
; height plus the area of the smallest side. Result in square feet.
(check-expect (wrappingpaper-surfacearea 2 3 4) 58)
(check-expect (wrappingpaper-surfacearea 1 1 10) 43)

(define (wrappingpaper-surfacearea length width height)
  (define lw-side (* length width))
  (define wh-side (* width height))
  (define hl-side (* height length))
  (+ (* 2 lw-side) (* 2 wh-side) (* 2 hl-side) (min lw-side wh-side hl-side)))

; int int int -> int
; Returns the length of ribbon needed to wrap the package. Ribbon length is the shortest
; distance around the sides of the package + volume of the present (for the bow).
(check-expect (ribbon-length 2 3 4) 34)
(check-expect (ribbon-length 1 1 10) 14)

(define (ribbon-length length width height)
  (define lw-perimeter (+ length length width width))
  (define wh-perimeter (+ width width height height))
  (define hl-perimeter (+ height height length length))
  (define volumn (* length width height))
  (+ volumn (min lw-perimeter wh-perimeter hl-perimeter)))

; Format Input
(define inputs (format-input "day2.dat"))

; Part 1
(for/sum ([input inputs])
  (wrappingpaper-surfacearea (list-ref input 0) (list-ref input 1) (list-ref input 2)))

; Part 2
(for/sum ([input inputs])
  (ribbon-length (list-ref input 0) (list-ref input 1) (list-ref input 2)))


(test)