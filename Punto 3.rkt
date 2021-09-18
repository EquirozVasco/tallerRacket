#lang racket
(define (LateTaskNumbers list)
  (cond((empty? list) empty)
       (else(cons (first list)
                  (LateTaskNumbers (filter (lambda (dato) (not (equal? (first list) dato))) list)))


       )

   
   )

)