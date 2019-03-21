#lang racket
(provide boxer-unboxer)
(require syntax/parse syntax/strip-context)



(define (boxer-unboxer stx)
  (syntax-parse stx
    ; These need to reference the literal + and - functions or they'll be tricked
    ; by redefinitions. Sadly, when they point to the literal + and - es of this
    ; context, they can't see the ones of the context of stx if stx has different context.
    [(~literal -) (let () (print "got a minus") #'+)]
    [(~literal +) #'-]
    [(~var t id) #'"asdf"]
    [(~datum :test) #'(print "yep, working")]
    ; Recursively call this on all the syntax
    ; Without replace-context, things like #%app get lost and functions don't work
    [(any ...) (replace-context stx #`(#,@(map boxer-unboxer (syntax-e #`(any ...)))))]
    [default #'default]
  )
  )