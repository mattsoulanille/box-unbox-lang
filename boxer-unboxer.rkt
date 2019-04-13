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
  

(define (box-unbox-map-stx context-stx stx boxed-vars)
  (map-stx context-stx boxer-unboxer-helper stx boxed-vars))

; Same as map but for multiple values instead of a list
(define (map-values stx)
  (syntax-parse stx
    [(func vals)
     #:with vals-lambda #'(lambda () vals)
     #:with res-list #'(call-with-values vals-lambda (lambda args (map func args)))
     #'(apply values res-list)]
    )
  )

; Checks if an identifier is bound to something in a list of identifiers.
(define (identifier-in-list ident id-list)
  (define (or-func a b) (or a b))
  (if (identifier? ident)
      (foldl or-func #f (map (lambda (other) (bound-identifier=? ident other -1)) id-list))
      #f
  ))
  


; See https://docs.racket-lang.org/reference/syntax.html
; for a list of all racket/base syntactic forms.

; ~! means "if you get to here, don't consider any other possible forms
;           and fail if it doesn't end up matching"
(define (boxer-unboxer-helper stx boxed-vars)
  (define new-boxed-vars  boxed-vars)
  (cons (syntax-parse stx
    ; define-values
    [((~literal define-values #:phase -1) ~! (var:identifier ...) vals:expr)
     ; Necessary so that functions that return multiple values get their values boxed properly.
     ; NOTE: values are not the same as arguments. You can't just use a function that takes multiple
     ; arguments on a `multiple value` multiple values are stored in the same argument. It's weird.
     #:do [(set! new-boxed-vars (append new-boxed-vars (syntax->list #'(var ...))))]
     #:with boxed-unboxed (let ([boxed-result (boxer-unboxer-helper #'vals new-boxed-vars)])
                            (set! new-boxed-vars (cdr boxed-result))
                            (car boxed-result)
                            )
     ;(print #'boxed-unboxed)
     (replace-context stx #`(define-values (var ...) #,(map-values #'(box boxed-unboxed))))]

    
    ; let-values
    [((~literal let-values #:phase -1) ~! ([(var:identifier ...) vals:expr] ...) body ...)
     #:do [(set! new-boxed-vars (append new-boxed-vars (syntax->list #'(var ... ...))))]
     #:with (transformed-vals ...) (box-unbox-map-stx stx #'(vals ...) new-boxed-vars)
     #:with (boxed-vals ...) #`(#,@(map (lambda (v) (replace-context stx (map-values #`(box #,v))))
                                (syntax->list #'(transformed-vals ...))))
     #:with (transformed-body ...) (box-unbox-map-stx stx #'(body ...) new-boxed-vars)
     (replace-context stx #'(let-values ([(var ...) boxed-vals] ...) transformed-body ...))]

    ; set!
    [((~literal set! #:phase -1) ~! var:identifier val:expr)
     #:with to-set-as (boxer-unboxer-helper #'val new-boxed-vars)
     (replace-context stx #'(set-box! var to-set-as))]
    
    ; function application
    ; If it's an identifier, unbox it.
    ;[((~literal #%app #:phase -1) ~! func:identifier vars ...)
;     #:with (identifiers-unboxed ...) (map-stx stx (lambda (x)
 ;                                              (if (identifier? x)
  ;                                                 (unbox-stx x)
   ;                                                x)
;                                               )
 ;                                        #'(vars ...))
     ;(print #`(#%app func identifiers-unboxed ...))
  ;   (replace-context stx #`(#%app func identifiers-unboxed ...))]
    
    ;[(~datum :test) #'(print "yep, working")]
    [(any ...)
     #:with recursed #`(#,@(let ([stx-list (syntax->list #'(any ...))])
                                              (for/list ([stx-item stx-list])
                                              ;(print vars-list)
                                              
                                              (let* ([result (boxer-unboxer-helper stx-item new-boxed-vars)]
                                                     [new-stx (car result)])
                                                (set! new-boxed-vars (cdr result))
                                                new-stx))))

     ;(map (lambda (x) (boxer-unboxer-helper x new-boxed-vars)) (syntax-e #`(any ...)))
     (replace-context stx #'recursed)]
    [single (if (identifier-in-list #'single new-boxed-vars)
                (unbox-stx #'single)
                #'single)]
    ) new-boxed-vars)
  )

(define (boxer-unboxer stx)
  (let ([result (boxer-unboxer-helper stx (list))])
    ;(print (cdr result))
    (car result)
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