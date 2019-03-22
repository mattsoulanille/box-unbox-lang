#lang s-exp "box-unbox-module.rkt"

(- 4 5)
(let ([x 4] [y 5])
  (+ x y)
  )

(facet (lambda (x) #t) 1 3)
(let ([minus -])
  (minus 50 50)
  )

(define (test a b)
  (+ (* a b) (- a b)))



(define x 12)

(let ([x 5])
  
  (+ x 4)
  )
