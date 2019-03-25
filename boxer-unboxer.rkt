#lang racket
(provide boxer-unboxer)
(require syntax/parse syntax/strip-context racket/syntax)
(require syntax/kerncase)

(require (for-syntax syntax/parse))

(define (box-stx stx)
  #`(box #,stx))
(define (unbox-stx stx)
  #`(unbox #,stx))

            
(define (map-stx-list context-stx stx-procedure stx-list)
  (map stx-procedure
       (map (curry replace-context context-stx) stx-list)))

(define (box-unbox-list context-stx stx-list)
  (map-stx-list context-stx boxer-unboxer stx-list))

; Same as map but for multiple values instead of a list
(define (map-values stx)
  (syntax-parse stx
    [(func vals)
     #:with vals-lambda #'(lambda () vals)
     #:with res-list #'(call-with-values vals-lambda (lambda args (map func args)))
     #'(apply values res-list)]
    )
  )

; See https://docs.racket-lang.org/reference/syntax.html
; for a list of all racket/base syntactic forms.
(define (boxer-unboxer stx)
  (syntax-parse stx
    ; define-values
    [((~literal define-values #:phase -1) (var:identifier ...) val:expr ...)
     ; Necessary so that functions that return multiple values get their values boxed properly.
     ; NOTE: values are not the same as arguments. You can't just use a function that takes multiple
     ; arguments on a `multiple value` multiple values are stored in the same argument. It's weird.
     (replace-context stx #`(define-values (var ...) #,(map-values #'(box val ...))))]

    
    ; let-values
    [((~literal let-values #:phase -1) ([(var:identifier) val:expr] ...) body ...)
     #:with (assignment ...) (replace-context stx #`([(var) (box val)] ...))
     #:with (transformed-body ...) #`(#,@(box-unbox-list stx (syntax->list #'(body ...))))
     (replace-context stx #'(let-values (assignment ...) transformed-body ...))]

    ; set!
    [((~literal set! #:phase -1) var:identifier val:expr)
     #:with to-set-as (boxer-unboxer #'val)
     (replace-context stx #'(set-box! var to-set-as))]
    
    ; function application
    [((~literal #%app #:phase -1) func:identifier vars:identifier ...)
     (replace-context stx #`(#%app func #,@(map unbox-stx (syntax-e #'(vars ...)))))]
    
    ;[(~datum :test) #'(print "yep, working")]
    [(any ...)
     #:with recursed (map boxer-unboxer (syntax-e #`(any ...)))
     (replace-context stx #'recursed)]
    [single #'single]
    )
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