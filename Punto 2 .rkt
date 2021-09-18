#lang racket
;factorial de un número de forma recursiva
(define (! n)
  (if (<= n 1)
      1
      (* n (! (sub1 n)))
  )
)
(define (combinatoria n x)
  (/ (! n) (* (! x) (! (- n x))))
)