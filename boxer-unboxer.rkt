#lang racket
(provide boxer-unboxer)
(require syntax/parse syntax/strip-context)
(require syntax/kerncase)


(define-syntax-class facet
  ;#:literals (facet)
  (pattern [(~literal facet #:phase -1) guard private public]
           ))


; How do I make this not produce false positives
(define (was-boxed? boxed-vars to-check)
  (if (identifier? to-check)
      (foldl (lambda (a b) (or a b)) #f (map (lambda (var) (bound-identifier=? to-check var -1)) boxed-vars))
      #f
      )
  )

(define (boxer-unboxer-helper boxed-vars stx)
  (let ([new-boxed-vars boxed-vars])
    (cons
     (syntax-parse stx
       ; These need to reference the literal + and - functions or they'll be tricked
       ; by redefinitions. Sadly, when they point to the literal + and - es of this
       ; context, they can't see the ones of the context of stx if stx has different context.
       ; Addendum: Phases fix this. See below.
       [fac:facet #'fac.public]; same as (attribute fac.public)?
       ; #:phase -1 indicates that we want to go one level down in phase
       ; higher up means more macro-ey and lower down means more code-we're-executing-ey
       ; to put it in super impercise terms. All phase levels are relative to the code you're
       ; executing with that code being at phase level 0. Look it up for more detail.
       [(~literal - #:phase -1) #'+]
       [(~literal + #:phase -1) #'-]
       ;[(~datum :test) #'(print "yep, working")]
       ; Recursively call this on all the syntax
       ; Without replace-context, things like #%app get lost and functions don't work
       [((~literal define #:phase -1) var:identifier val:expr)
        (set! new-boxed-vars (set-add boxed-vars #'var))
        (replace-context stx #'(define var (box val)))
        ]
     
       [(any ...)
        ; Todo: Make this pass the boxed-vars set correctly!
        #:with recursed #`(#,@(let ([as-list (syntax-e #'(any ...))])
                                (define (recursive-apply-box-unbox boxed-vars syntax-list)
                                  (if (equal? syntax-list null)
                                      (let ()
                                        (set! new-boxed-vars boxed-vars); Side effect!
                                        null
                                        )
                                      (let* ([res (boxer-unboxer-helper boxed-vars (car syntax-list))]
                                             [resulting-stx (car res)]
                                             [new-boxed-vars (cdr res)])
                                        (cons resulting-stx
                                              (recursive-apply-box-unbox new-boxed-vars (cdr syntax-list)))
                                        )
                                  ))
                                (recursive-apply-box-unbox boxed-vars (syntax-e stx))
                               ))
                               ;map car (map (curry boxer-unboxer-helper boxed-vars) (syntax-e #`(any ...)))))
        (replace-context stx #'recursed)]
       ; todo: Check if it's actually being used as a variable. But how?
       [single (if (was-boxed? (set->list boxed-vars) #'single)
                   #'(unbox single)
                   #'single)]  ;(let () (print #'single) #'single))]
       ;[default #'default]
       )
     new-boxed-vars)
  ))

(define (boxer-unboxer stx)
  (car (boxer-unboxer-helper (set) stx))
  )

; Questions:
; How do I detect variables?
; How do I detect if a variable is being defined vs being used?
; Why isn't bound-identifier=? working as I expect?
; Will I need a specific thing for every way a variable could be created, or
;   is there some better way to detect variable definition / binding?
; Would it be better to (expand stx) to racket/base before trying to
;   box / unbox all the variables?
; I can traverse the AST. What should the overall design structure of new Racets be?