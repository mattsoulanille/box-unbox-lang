#lang racket
(provide boxer-unboxer)
(require syntax/parse syntax/strip-context)

(define (boxer-unboxer stx)
  (syntax-parse stx
    [(~datum -) (let () (print "got a minus") #'+)]
    [(~datum +) #'-]
    [(~datum :test) #'(print "yep, working")]
    ; Recursively call this on all the syntax
    ; Without replace-context, things like #%app get lost and functions don't work
    [(any ...) (replace-context stx #`(#,@(map boxer-unboxer (syntax-e #`(any ...)))))]
    [default #'default]
  )
  )