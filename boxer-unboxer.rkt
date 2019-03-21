#lang racket
(provide boxer-unboxer)
(require syntax/parse syntax/strip-context)
(require syntax/kerncase)
(define-syntax-class facet
  ;#:literals (facet)
  (pattern [(~literal facet) guard private public]
           
           ))


(define (boxer-unboxer stx)
  (syntax-parse stx
    ; These need to reference the literal + and - functions or they'll be tricked
    ; by redefinitions. Sadly, when they point to the literal + and - es of this
    ; context, they can't see the ones of the context of stx if stx has different context.
    [fac:facet #'fac.public]; same as (attribute fac.public)?
    ; #:phase -1 indicates that we want to go one level down in phase
    ; higher up means more macro-ey and lower down means more code-we're-executing-ey
    ; to put it in super impercise terms. All phase levels are relative to the code you're
    ; executing with that code being at phase level 0. Look it up for more detail.
    [(~literal - #:phase -1) #'+]
    [(~literal + #:phase -1) #'-]
    ;[(~literal +) #'-]
    ;[(~var t id) #'"asdf"]
    [(~datum :test) #'(print "yep, working")]
    ; Recursively call this on all the syntax
    ; Without replace-context, things like #%app get lost and functions don't work
    [(any ...)
     #:with recursed #`(#,@(map boxer-unboxer (syntax-e #`(any ...))))
    (replace-context stx #'recursed)]
    [default #'default]
  )
  )