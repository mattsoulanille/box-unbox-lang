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

; TODO: Make sure it's okay to use the same context for each inner
; one as the outer one
(define (map-stx context-stx stx-procedure stx)
  (replace-context
   context-stx
   #`(#,@(map-stx-list context-stx stx-procedure (syntax->list stx)))))
  

(define (box-unbox-map-stx context-stx stx)
  (map-stx context-stx boxer-unboxer stx))

; Same as map but for multiple values instead of a list
(define (map-values stx)
  (syntax-parse stx
    [(func vals)
     #:with vals-lambda #'(lambda () vals)
     #:with res-list #'(call-with-values vals-lambda (lambda args (map func args)))
     #'(apply values res-list)]
    )
  )

(define (was-boxed var boxed-vars)
  (ormap identity (map (lambda (compareto)
                   (and (identifier? var) (free-identifier=? var compareto))
                   ) boxed-vars))
  )


; See https://docs.racket-lang.org/reference/syntax.html
; for a list of all racket/base syntactic forms.
; Whenever we box something, we set the syntax-property 'boxed to true.
; https://docs.racket-lang.org/reference/stxprops.html
(define (boxer-unboxer-helper stx boxed-vars)
  (syntax-parse stx
    ; define-values
    [((~literal define-values #:phase -1) (var:identifier ...) vals:expr)
     ; Necessary so that functions that return multiple values get their values boxed properly.
     ; NOTE: values are not the same as arguments. You can't just use a function that takes multiple
     ; arguments on a `multiple value` multiple values are stored in the same argument. It's weird.
     #:with boxed-unboxed (boxer-unboxer-helper #'vals)
     (let ([new-boxed-vars (set-union boxed-vars (list->set (syntax->list #'(var ...))))])
       (cons
        (replace-context stx #`(define-values (var ...) #,(map-values #'(box boxed-unboxed))))
        new-boxed-vars)
       
       )]

    
    ; let-values
    [((~literal let-values #:phase -1) ([(var:identifier ...) vals:expr] ...) body ...)
     #:with (transformed-vals ...) (box-unbox-map-stx stx #'(vals ...))
     #:with (boxed-vals ...) #`(#,@(map (lambda (v) (replace-context stx (map-values #`(box #,v))))
                                (syntax->list #'(transformed-vals ...))))
     #:with (transformed-body ...) (box-unbox-map-stx stx #'(body ...))
     #:with all-vars #'(var ... ...)
     (let ([new-boxed-vars (set-union boxed-vars (list->set (syntax->list #'all-vars)))])
       (cons
        (replace-context stx #'(let-values ([(var ...) boxed-vals] ...) transformed-body ...))
        new-boxed-vars)
       )]

    ; set!
    [((~literal set! #:phase -1) var:identifier val:expr)
     #:with to-set-as (boxer-unboxer-helper #'val boxed-vars)
     (replace-context stx #'(set-box! var to-set-as))]
    
    ;[(~datum :test) #'(print "yep, working")]
    [(any ...)
     #:with recursed (map boxer-unboxer-helper (syntax-e #`(any ...)))
     (replace-context stx #'recursed)]

    [var:id
     (if (was-boxed #'var boxed-vars)
         (unbox-stx #'var)
         #'var)]
    [single #'single]
  ))

(define (boxer-unboxer stx)
  (boxer-unboxer-helper stx (set))
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