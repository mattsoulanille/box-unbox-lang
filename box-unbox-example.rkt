#lang s-exp "box-unbox-module.rkt"


;(require rackunit)
;(- 4 5)
;(let ([x 4] [y 5])
;  (+ x y)
;  )

;(facet (lambda (x) #t) 1 3)
;(let ([minus -])
;  (minus 50 50)
;  )

;(define (test a b)
;  (+ (* a b) (- a b)))



;(define x 12)
;(print x)
;(set! x 5)
;(print x)

;(define a 4)
;(define b 5)
;(define c 6)
;(set!-values (a b c) (values 1 2 3))
;(print "vals")
;(print (list a b c))
;(let ([x 5])
;  (+ x 4)
;  )


;(check-equal? 4 4)
;(check-equal? (let ()
;                (define-values (foo bar baz) (values "foo" "bar" "baz"))
;                (string-append foo bar baz)
;                ) "foobarbaz")

;(define-values (single) "hello")
;(print single)





;(map + (list 1 2 3))

(define recur1 42)
(define recur2 recur1)
(define recur3 recur2)
(print recur3)

;(define func-test +)

;(require macro-debugger/syntax-browser)