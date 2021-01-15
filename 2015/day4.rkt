#lang racket

(require test-engine/racket-tests)
(require file/md5)

; string -> int
; Mines for AdventCoin by finding the first md5 hash of the given secret key with at least
; five leading zeros. It then returns the integer suffix of the key.
(check-expect (mine-advent "abcdef") 609043)

(define (mine-advent key [leading-zeros 5] [i 0])
  (cond [(= (modulo i 1000) 0) (displayln i)])
  (define full-key (string-append key (number->string i)))
  (define hash (bytes->string/utf-8 (md5 full-key)))

  (if (string=? (substring hash 0 leading-zeros) (make-string leading-zeros #\0))
      i
      (mine-advent key leading-zeros (add1 i))))

;(test)

; Part 1
(mine-advent "yzbqklnj")

; Part 2
(mine-advent "yzbqklnj" 6)

